#Data cleaning and QAQC

# Prepare the environment -------------------------------------------------
library(tidyverse)
library(here) 
library(stringr)
library(zoo) 
library(openxlsx) 
library(cowplot)

#Clear the workspace
rm(list = ls())

# Read in data ------------------------------------------------------------
#Read in data files
pc_notes_readin<-read.csv(here('outputs','otolith_data_export.csv'),stringsAsFactors = F)

pc_sr_readin<-read.csv(here('data','PC_2017_2021_04_analyzed_data.csv'),
                         stringsAsFactors = F)
pc_line_readin <- read.xlsx(here::here("data", "PC_otolith_line_measurements.xlsx"),
                              sheet = "line_measurement")
  

# Prepare and join data ---------------------------------------------------

#Prepare Notes
pc_notes <- pc_notes_readin

pc_line<-pc_line_readin

#Prepare Sr isotope data
sr_chem <- pc_sr_readin %>%
  #select columns used for further analysis. We use the 5 second integrated Sr87Sr86 value but no moving average has been applied yet
  distinct(Sample_ID, Distance,totalSr, Sr87Sr86, region_name) %>% 
  #Remove the .run from the sample ID and the beginning string
  mutate (lab_id = str_sub(Sample_ID, start=15, end=-5L))%>%
  #Hard trim
  filter (Sr87Sr86>0.7 & Sr87Sr86<0.8)%>%
  filter (totalSr>0.5)%>%
  group_by(lab_id)%>%
  #Outlier rejection
  mutate(median = rollapply(Sr87Sr86, width = 20, FUN = median, partial = T)) %>%
  mutate(upper75prob = rollapply(Sr87Sr86, width = 20, FUN = quantile, partial = T, probs = 0.75, na.rm=TRUE),
         lower25prob = rollapply(Sr87Sr86, width = 20, FUN = quantile, partial = T, probs = 0.25, na.rm=TRUE),
         IQR = upper75prob - lower25prob,
         diff_flag = (Sr87Sr86 - median)/IQR,
         despiked = ifelse(abs(diff_flag) > 2, median, Sr87Sr86)) %>%
  ungroup()

#Plot full strontium isotope profiles with raw data
#Show which fish to plot)
fish_ids <- unique(sr_chem$lab_id)
fish_ids #Show list of fish that will be plotted

#Create pdf, this will take a while 
pdf("outputs/sr_iso_profiles.pdf",16, 8)
for(i in fish_ids){
  plot_chem<- sr_chem%>% filter (lab_id==i)
  p <- ggplot(data=plot_chem)+
    geom_point(aes(x=Distance, y=despiked), color="black")+
    scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))+
    scale_y_continuous(expand=c(0,0),expression(""^87*Sr*"/"^86*Sr), limits= c(0.704, 0.711), breaks = scales::pretty_breaks(n = 8))+
    theme_bw() +
    theme(legend.position="top", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(i)
  print(p)
}
dev.off()


#Transpose Sr data onto the line transect
sr_line_info <- pc_line%>%
  #Trim line transect to only ventral edge to core
  group_by(lab_id)%>%
  mutate(core_distance=case_when(tag=="core"~ transect_distance_um))%>%
  fill(core_distance, .direction="updown")%>%
  filter(transect_distance_um <=core_distance)%>%
  #Adjust for start
  filter(!tag=="start_laser")%>%
  mutate(transect_distance_um=transect_distance_um-min(transect_distance_um),
         laser_distance_um=laser_distance_um-min(laser_distance_um))%>%
  #Calculate stepsize
  group_by(lab_id)%>%
  arrange(lab_id,number)%>%
  mutate(transect_step=transect_distance_um-lag(transect_distance_um, n=1L, default=1),
         laser_step=laser_distance_um-lag(laser_distance_um, n=1L, default=1))%>%
  mutate(d_step=transect_step/laser_step)%>%
  ungroup()%>%
  distinct(lab_id, tag, number, laser_distance_um, d_step)

#Join data and calculate adjusted distances
sr_trans <- sr_chem%>%
  group_by(lab_id)%>%
  mutate(step_original=lead(Distance, n=1)-Distance)%>%
  left_join(sr_line_info, by=("lab_id"))%>%
  mutate(step_apply=case_when
         (Distance <laser_distance_um & Distance>=lag(laser_distance_um, n=1)~d_step))%>%
  drop_na(step_apply)%>%
  mutate(Distance_adjust=cumsum(step_original*step_apply))%>%
  ungroup()%>%
  drop_na(Distance_adjust)

#Spline data Sr with smoothing
sr_spline <- sr_trans %>%
  group_by(lab_id)%>%
  nest()%>%
  mutate(despiked_spline =map(data, ~mgcv::gam(despiked ~ s(Distance_adjust, k=100, bs="tp"), data=.x))) %>%
  mutate(augment_spline= map2(despiked_spline, data, ~broom::augment(.x, newdata = .y, se_fit = TRUE))) %>%
  unnest(augment_spline)%>%
  mutate (sr_spline = .fitted,
          sr_spline_se =.se.fit)%>%
  dplyr::select(lab_id, Distance_adjust, sr_spline, sr_spline_se) %>%
  mutate (sr_spline_se=sqrt((sr_spline_se^2)+(0.00005^2)))

#Join notes and sr data
pc_sr <- sr_trans %>%
  left_join (pc_notes, by="lab_id")%>%
  left_join (sr_spline, by=c("lab_id","Distance_adjust")) %>%
  #Mirror data so it starts at the core and runs to ventral edge
  group_by(lab_id)%>%
  mutate(Distance_um=max(Distance_adjust)-Distance_adjust)%>%
  #select columns to keep
  dplyr::select(lab_id, FL_mm, age, age_f, region_name, length, width, weight,
                Distance_um, tag, number, totalSr, despiked, sr_spline, sr_spline_se,
                Sex,Adipose,CWT,Hatchery,CWT_Age,CWT_Release, Date_recovered, section_alpha,survey_year)%>%
  arrange(lab_id, Distance_um)%>%
  #Create region breaks
  group_by(lab_id, number)%>%
  mutate(region_break=min(Distance_um))%>%
  ungroup()%>%
  #Create natal area
  group_by(lab_id)%>%
  mutate(region_name=case_when (region_name=="Range 1" ~"Natal"))%>%
  group_by(lab_id, region_name)%>%
  mutate(region_mean87Sr86Sr=mean(sr_spline, na.rm=T),
         region_sd87Sr86Sr=sd(sr_spline, na.rm=T))%>%
  ungroup()%>%
  arrange(lab_id, Distance_um)

#Plot full strontium isotope profiles with raw data
#Show which fish to plot)
fish_ids <- unique(pc_sr$lab_id)
fish_ids #Show list of fish that will be plotted

#Create pdf, this will take a while 
pdf("outputs/sr_iso_splined_trimmed.pdf",16, 8)
for(i in fish_ids){
  plot_chem<- pc_sr%>% filter (lab_id==i)
  p <- ggplot(data=plot_chem)+
    geom_ribbon(aes(x=Distance_um, ymin = sr_spline-(2*sr_spline_se), ymax =sr_spline+(2*sr_spline_se)),
                fill = "black", alpha=0.2)+
    geom_line(aes(x=Distance_um, y=sr_spline), color="black")+
    #Natal area
    geom_line(data=plot_chem%>%filter(region_name=="Natal"),
                                      aes(x=Distance_um, y=sr_spline), color="orange")+
    geom_vline(aes(xintercept=region_break), color="black", linetype="dashed")+
    geom_text (aes(x=region_break, y=0.710, label=tag), check_overlap = TRUE, 
               angle = 90, size=3, nudge_x = +10, color="black")+
    scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))+
    scale_y_continuous(expand=c(0,0),expression(""^87*Sr*"/"^86*Sr), limits= c(0.704, 0.711), 
                       breaks = scales::pretty_breaks(n = 8))+
    theme_bw() +
    theme(legend.position="top", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(i)
  print(p)
}
dev.off()

#Check missing samples
miss_check <- sr_chem %>%
  distinct(lab_id)%>%
  anti_join(pc_sr, by="lab_id")

#How many fish for natal classification?
natal_check <-pc_sr%>%
  filter(region_name=="Natal")%>%
  distinct(lab_id,region_mean87Sr86Sr)

#Export data
write.csv(pc_sr, "outputs/pc_sr.csv", row.names=F)

