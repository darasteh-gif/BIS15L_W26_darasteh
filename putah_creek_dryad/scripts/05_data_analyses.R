#Data analyses and plotting script
# Prepare the environment -------------------------------------------------
library(tidyverse)
library(here) #better working directory management
library(stringr) #working with strings
library(caret)# Stat model functions
library(viridis)#Color scale
library(cowplot) #GGplot extensions
library(rstatix)# pipe friendly stats
library(MASS) #Discriminant function analyses
library(randomForest) #random forest
library(moments)#Skewness 



#Clear the workspace
rm(list = ls())

#Read in data
pc_sr<-read.csv(here('outputs','pc_sr.csv'),  stringsAsFactors = F)
natal_data<-read.csv(here('outputs','natal_data.csv'),  stringsAsFactors = F)


#Read in willmes et al 2021 data
willmes_data<-read.csv(here('data','putah_willmes_etal2021.csv'),  stringsAsFactors = F)

willmes_data_join <- willmes_data %>%
  mutate(lab_id=Run_ID,
         survey_year=2016,
         age=Age_all,
         FL_mm=as.numeric(FL_cm)*10,
         section_alpha=Section)%>%
  mutate(age_f=factor(age))%>%
  dplyr::select(lab_id,survey_year,grouped_classification, 
                age, age_f, FL_mm,Sex, section_alpha)%>%
  mutate(CWT="N",
         Hatchery=NA)%>%
  #remove juveniles
  filter(age>1)


#Add natal information to the Sr profiles
pc_sr_natal <- pc_sr %>%
  left_join(natal_data, by="lab_id")%>%
  mutate(Sample_type="Adult")%>%
  ungroup()


write.csv (pc_sr_natal , "outputs/pc_sr_natal.csv", row.names=FALSE)

#Dataframe for histograms
#Lists of Hatchery and Wild places
hatchlist <- c ("CNH","FRH","MEH", "MOH", "NIH", "FRHMOH","CNHTHE", "THE", "MEHNIH")
wildlist <- c ("FEA", "MOK", "PUC", "YUB", "TUO","MER", "FEAPUC","FEAMOK","BUT","AME","FEASTA","PUCSTA","STA","MERYUB")
mixlist <- c ("FRHMOK", "MOHTUO", "MOHYUB", "AMENIH", "MEHMER","FEAFRH","FEATHE","BUTCNH")

natal_data_analysis <-pc_sr_natal%>%
  filter(region_name=="Natal")%>%
  distinct(lab_id,survey_year, grouped_classification, age, age_f, FL_mm, Sex, 
           section_alpha, CWT, Hatchery)%>%
  #Join Willmes data
  rbind(willmes_data_join)%>%
  mutate (simplified_classification = case_when (grouped_classification %in% hatchlist ~ "Hatchery-strays",
                                                 grouped_classification %in% wildlist ~"Natural-strays",
                                                 grouped_classification %in% mixlist ~"Unknown",
                                                 TRUE ~"Unclassified")) %>%
  mutate(simplified_classification_withputah=case_when(grouped_classification =="PUC"~"Putah Creek",
                                                       TRUE~simplified_classification))%>%
  mutate(simplified_classification=factor(simplified_classification,
                                          levels=c("Hatchery-strays","Natural-strays","Unknown")))%>%
  mutate(simplified_classification_withputah=factor(simplified_classification_withputah,
                                                    levels=c("Hatchery-strays","Natural-strays","Unknown","Putah Creek")))%>%
ungroup() %>% 
  mutate(age_f_rev = factor(age_f, levels = c('4','3','2')))

# Joining in CWT known-origin fish from 2017 & 2018 that were not analyzed for Sr since 
#their natal origin is already known:
#Read in data:
cwt_knownfish_readin<-read.csv(here('data','PC_cwt_knownfish_2017_2018.csv'),  stringsAsFactors = F)

cwt_knownfish <- cwt_knownfish_readin %>% 
  mutate(age_f=factor(age)) %>% 
  mutate(age_f_rev = factor(age_f, levels = c('4','3','2'))) %>% 
  mutate(simplified_classification=factor(simplified_classification,
                                          levels=c("Hatchery-strays","Natural-strays","Unknown")))%>%
  mutate(simplified_classification_withputah=factor(simplified_classification_withputah,
                                                    levels=c("Hatchery-strays","Natural-strays","Unknown","Putah Creek")))%>%
  ungroup() %>%
  mutate(CWT="Y",
         Hatchery=NA)

# CWT check vs predicted natal origins ------------------------------------
cwt_check <- pc_sr_natal %>%
  distinct(lab_id, survey_year, assignment, grouped_classification,
           age, age_f, CWT, Hatchery, CWT_Age, CWT_Release)%>%
  filter(!CWT=="n" & !CWT=="N")%>%
  mutate(Hatchery_f=case_when(Hatchery=="MOK R FISH INS" ~"MOH",
                              Hatchery=="NIMBUS FISH HATCHERY" ~"NIH",
                              Hatchery=="Feather R" ~"FRH",
                              Hatchery=="Mok R" ~"MOH",
                              Hatchery=="MERCED R FISH FACIL" ~"MEH",
                              TRUE ~Hatchery))%>%
  mutate(cwt_correct=case_when(assignment==Hatchery_f~"yes",
                         TRUE~"No"))
write.csv(cwt_check, "outputs/cwt_check.csv", row.names=F)

cwt_age_summary <- cwt_check%>%
  drop_na(CWT_Age)%>%
  #Correct age
  mutate(age_check=case_when(age==CWT_Age ~"y",
                             TRUE~"n"))


#Join cwt known data to natal_data_analysis
natal_data_analysis_withcwt <- union(natal_data_analysis, cwt_knownfish)%>%
  mutate(CWT=case_when(CWT=="N"~NA,
                       TRUE~CWT))%>%
  #Change known origin CWT fish to their CWT origin
  mutate(simplified_classification=case_when(!is.na(CWT)~"Hatchery-strays",
                                             TRUE ~simplified_classification))%>%
  mutate(simplified_classification_withputah=case_when(!is.na(CWT)~"Hatchery-strays",
                                                       TRUE ~simplified_classification_withputah))%>%
  mutate(simplified_classification=factor(simplified_classification,
                                          levels=c("Hatchery-strays","Natural-strays","Unknown")))%>%
  mutate(simplified_classification_withputah=factor(simplified_classification_withputah,
                                                    levels=c("Hatchery-strays","Natural-strays","Unknown","Putah Creek")))%>%
  group_by(survey_year, simplified_classification_withputah)%>%
  #Change the known age fish
  mutate(age=case_when(lab_id%in%c("PC_2021_048","PC_2021_052") ~2,
                       lab_id%in%c("PC_2020_047") ~4,           
                       TRUE ~age))%>%
  mutate(age_f=factor(age)) %>% 
  mutate(age_f_rev = factor(age_f, levels = c('4','3','2'))) %>% 
  mutate(n_fish_natal=n())%>%
  ungroup()%>%
  group_by(survey_year,age_f)%>%
  mutate(n_fish_ages=n())%>%
  ungroup()%>%
  #Change grouped_classification based on CWT (only important for the 2 missclassified fish in the model)
  mutate(grouped_classification=case_when(lab_id%in%c("PC_2021_066", "PC_2021_035",
                                                      "PC_2021_061") ~"FRH",
                                          lab_id%in%c("PC_2021_017", "PC_2021_038",
                                                      "PC_2021_048", "PC_2021_052",
                                                      "PC_2021_070", "PC_2021_071",
                                                      "PC_2020_009", "PC_2020_010",
                                                      "PC_2020_012", "PC_2020_027",
                                                      "PC_2020_038", "PC_2020_039",
                                                      "PC_2020_058", "PC_2020_060",
                                                      "PC_2020_047") ~"MOH",           
                                          TRUE ~grouped_classification))%>%
  #Create brood year variable
  mutate(brood_year=survey_year -age+1)


# Plotting and data analyses ----------------------------------------------------------------

#Age size plot
p_age_size_histo<-ggplot(data=natal_data_analysis_withcwt %>% drop_na(age_f) %>% drop_na(FL_mm)) +
  geom_histogram(aes(x=FL_mm, fill=age_f), color="black") +
  scale_x_continuous(name = "Fork Length (mm)", limits=c(0,NA)) +
  scale_y_continuous("Fish count")+
  scale_fill_manual(name="Age Class", values = c("#F0F9E8", "#7BCCC4", "#0868AC"))+
  # scale_fill_manual(values=c("black","grey50","grey70","grey95"), name="Age")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"),
         legend.position="right", axis.text = element_text(size = 8))
p_age_size_histo
ggsave("outputs/age_size_histo.png",p_age_size_histo,  width = 8, height = 4)


#Calculating the summary statistics for age fl data
age_size_summary_stats <- natal_data_analysis_withcwt%>% 
  group_by(age_f) %>% 
  drop_na(FL_mm)%>%
  summarize(min = min(FL_mm), max = max(FL_mm), 
            mean = mean(FL_mm), q1= quantile(FL_mm, probs = 0.25), 
            median = median(FL_mm), q3= quantile(FL_mm, probs = 0.75),
            sd = sd(FL_mm), skewness=skewness(FL_mm), kurtosis=kurtosis(FL_mm))

write.csv(age_size_summary_stats ,"outputs/age_size_summary_stats.csv", row.names=F)

#Histogram plot of natal origins (hatchery, wild, unknown)
p_histpredict_simple <- ggplot(data=natal_data_analysis_withcwt)+
  geom_bar(aes(x=simplified_classification, fill=simplified_classification), stat = "count", color="black")+
  geom_text(stat='count',aes(x=simplified_classification,label=after_stat(count)),vjust=-1)+
  scale_fill_viridis(discrete = T, name="Natal origin")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top")+
  xlab("Predicted natal origin")+
  ylab("Number of Fish")+
  facet_wrap(~survey_year, nrow=1)
p_histpredict_simple
# ggsave(plot=p_histpredict_simple, "outputs/Hist_predict_simple.png", width = 15, height = 6)

#as a stacked plot with proportions:
p_histpredict_stacked <- ggplot(data=natal_data_analysis_withcwt) + 
  geom_bar(aes(x=as.factor(survey_year), fill = simplified_classification_withputah), position="fill",color = "black", width = 0.5) +
  scale_fill_viridis(discrete = T, name = "Natal origin") +
  theme_classic() +
  scale_x_discrete(name="Year") + 
  scale_y_continuous(name="Proportion") 
p_histpredict_stacked
# ggsave(plot=p_histpredict_stacked, "outputs/Hist_predict_simple_stacked.png", width = 8, height = 5)
 
p_histpredict_simple_withputah <- ggplot(data=natal_data_analysis_withcwt)+
  geom_bar(aes(x=simplified_classification_withputah, fill=simplified_classification_withputah), 
           stat = "count", color="black")+
  geom_text(stat='count',aes(x=simplified_classification_withputah,label=after_stat(count)),vjust=-1)+
  scale_fill_viridis(discrete = T, name="Natal origin")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top")+
  scale_x_discrete("Predicted natal origin", labels=c("HS", "WS", "Unk", "PUC"))+
  scale_y_continuous("Number of Fish", limits=c(0,105))+
  facet_wrap(~survey_year, nrow=1)
p_histpredict_simple_withputah 
# ggsave(plot=p_histpredict_simple_withputah , "outputs/Hist_predict_simple_withputah.png", width = 17, height = 6)
#combining natal origin counts and proportions plots

natal_origins_combo_plot <- plot_grid(p_histpredict_simple_withputah+theme(legend.position="top"),p_histpredict_stacked +theme(legend.position="none"),
                            labels = c('A', 'B'), ncol = 1)
natal_origins_combo_plot
ggsave(plot=natal_origins_combo_plot, "outputs/natal_origins_combo_plot.png", width = 8, height = 6)

wrap_label <- function(x) str_wrap(str_replace_all(x, "_", " "), width = 10)

#Histogram plot of detailed natal origins
  p_histpredict_detail <- ggplot(data=natal_data_analysis_withcwt)+
  geom_bar(aes(x=grouped_classification), stat = "count", color="black", fill="grey50")+
  geom_text(stat='count',aes(x=grouped_classification,label=after_stat(count)),vjust=-1)+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="none")+
  scale_x_discrete("Predicted natal origin") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous("Number of Fish", limits = c(0, 185)) +
  facet_grid(~simplified_classification_withputah, labeller = as_labeller(wrap_label), scales="free", space = "free") 
p_histpredict_detail
ggsave(plot=p_histpredict_detail, "outputs/Hist_predict_detail.png", width = 9, height = 6)


#Same bar plot, but with brood years facet wrapped instead of survey years
p_histpredict_simple_withputah_broods <- ggplot(data=subset(natal_data_analysis_withcwt, !is.na(brood_year)))+
  geom_bar(aes(x=simplified_classification_withputah, fill=simplified_classification_withputah), 
           stat = "count", color="black")+
  geom_text(stat='count',aes(x=simplified_classification_withputah,label=after_stat(count)),vjust=-0.5)+
  scale_fill_viridis(discrete = T, name="Natal origin")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top")+
  scale_x_discrete("Predicted natal origin", labels=c("HS", "WS", "Unk", "PUC"))+
  scale_y_continuous("Number of Fish", limits=c(0,110))+
  facet_wrap('brood_year')
p_histpredict_simple_withputah_broods 
ggsave(plot=p_histpredict_simple_withputah_broods , "outputs/Hist_predict_simple_withputah_broods.png", width = 8, height = 6)

# Ages by year

#Histogram plot of ages by year
p_ages_hist_by_year <- ggplot(data=natal_data_analysis_withcwt %>% drop_na(age_f))+
  geom_bar(aes(x=age_f, fill=age_f), stat = "count", color="black")+
  geom_text(stat='count',aes(x=age_f,label=after_stat(count)),vjust=-1)+
  scale_fill_manual(name="Age Class", values = c("#F0F9E8", "#7BCCC4", "#0868AC"))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top")+
  xlab("Age Class")+
  scale_y_continuous("Number of Fish", limits=c(0,80))+
  facet_wrap(~survey_year, nrow=1)
p_ages_hist_by_year
# ggsave(plot=p_ages_hist_by_year, "outputs/p_ages_hist_by_year.png", width = 15, height = 6)

#as a stacked plot with proportions:
p_ages_stacked <- ggplot(data=natal_data_analysis_withcwt %>% drop_na(age_f)) + 
  geom_bar(aes(x=as.factor(survey_year), fill = age_f_rev), position="fill",color = "black", width = 0.5) +
  scale_fill_manual(name="Age Class", values = c("#0868AC", "#7BCCC4", "#F0F9E8"))+
  theme_classic() +
  scale_x_discrete(name="Year") + 
  scale_y_continuous(name="Proportion") 
p_ages_stacked
# ggsave(plot=p_ages_stacked, "outputs/p_ages_stacked.png", width = 8, height = 5)

#combining age hist and proportions plots
ages_combo_plot <- plot_grid(p_ages_hist_by_year+theme(legend.position="top"),p_ages_stacked +theme(legend.position="none"),
                                      labels = c('A', 'B'), ncol = 1)
ages_combo_plot
ggsave(plot=ages_combo_plot, "outputs/ages_combo_plot.png", width = 8, height = 6)

#Ages by natal origin classification
age_origin_hist <- ggplot(data=natal_data_analysis_withcwt %>% drop_na(age_f))+
  geom_bar(aes(x=age_f, fill=age_f), stat = "count", color="black")+
  scale_fill_manual(name="Age Class", values = c("#F0F9E8", "#7BCCC4", "#0868AC"))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top")+
  xlab("Age")+
  ylab("Number of Fish")+
  facet_wrap(~simplified_classification_withputah, nrow=1)
age_origin_hist
ggsave(plot=age_origin_hist, "outputs/Age_origin_hist.png", width = 15, height = 6)

#Ages by natal origin classification and sex
age_origin_hist_sex <- ggplot(data=natal_data_analysis_withcwt %>% drop_na(age_f))+
  geom_bar(aes(x=age_f, fill=Sex), stat = "count", color="black")+
  scale_fill_manual(name="Sex", values = c("purple","orange","grey"))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top")+
  xlab("Age")+
  ylab("Number of Fish")+
  facet_wrap(~simplified_classification_withputah, nrow=1)
age_origin_hist_sex
ggsave(plot=age_origin_hist_sex, "outputs/Age_origin_hist_sex.png", width = 8, height = 4)

#Boxplot that links Fl and age classes over the study years:
fl_age_boxplot <- ggplot (data=natal_data_analysis_withcwt %>%drop_na(age_f))+
  geom_boxplot (aes(x=survey_year, y=FL_mm, fill=survey_year, group=survey_year)) +
  geom_point(aes(x=survey_year, y=FL_mm, fill=survey_year, group=survey_year), alpha=0.1) +
  facet_wrap (~age_f, nrow=1) +
  scale_fill_viridis() +
  theme_bw() +
  theme(legend.position="none") 
fl_age_boxplot  
# ggsave(plot=fl_age_boxplot, "outputs/fl_age_boxplot.png", width=17, height=6)

#Generate separate FL histograms for each study year:

fl_hist_by_year <- ggplot(data=natal_data_analysis_withcwt, aes(x=FL_mm)) +
  geom_histogram(binwidth=50, color="black", fill="gray") +
  facet_wrap('survey_year') +
  scale_x_continuous(name = "Fork Length (mm)", limits=c(0,NA)) +
  scale_y_continuous(name = "Number of Fish") +
  scale_fill_viridis() +
  theme_bw()
fl_hist_by_year
# ggsave(plot=fl_hist_by_year, "outputs/fl_hist_by_year.png", width=8, height=6)


# Has the proportion of natal origins changed with time? ------------------
#Natal origins by year
natal_stat_tests <-natal_data_analysis_withcwt %>%
  group_by(survey_year,simplified_classification_withputah, .drop = FALSE)%>%
  summarise(fish_count=n())%>%
  group_by(survey_year)%>%
  mutate(year_count=sum(fish_count))%>%
  mutate(natal_prop=fish_count/year_count)%>%
  ungroup()


simple_natal_proportions_lineplot <- ggplot(data=natal_stat_tests, 
                                            aes(x=survey_year, y=natal_prop, 
                                                group=simplified_classification_withputah)) +
  geom_line(aes(color=simplified_classification_withputah)) +
  geom_point(aes(color=simplified_classification_withputah)) +
  scale_color_viridis(discrete = T, name="Natal origin") +
  scale_x_continuous(name = "Survey Year") +
  scale_y_continuous(name = "Proportion of Fish") +
  theme_bw()

simple_natal_proportions_lineplot
# ggsave(plot=simple_natal_proportions_lineplot, "outputs/simple_natal_proportions_lineplot.png", width=8, height=6)

# Homogeneity of in natal proportions among years
natal_stat_tests_model<-natal_stat_tests %>%
  dplyr::select(survey_year, simplified_classification_withputah, fish_count)%>% 
  filter(!simplified_classification_withputah == "Unknown") %>%
  
  pivot_wider(names_from="survey_year", values_from="fish_count")%>%
  column_to_rownames(var="simplified_classification_withputah")%>%
  ungroup()

natal_chi<- chisq.test(natal_stat_tests_model, sim=TRUE,  B=20000)
natal_chi

#Posthoc pairwise comparison
#https://rpkgs.datanovia.com/rstatix/reference/chisq_test.html
natal_chi_pwc <-pairwise_chisq_gof_test(natal_stat_tests_model, p.adjust.method = "holm")
natal_chi_pwc

write.csv(natal_chi_pwc ,"outputs/natal_chi_pwc.csv", row.names=F)



# Creating hydrograph line plots of Putah Creek flow data before and after Accord/functional flow regime implementation

putah_flow_data<-read.csv(here('data','Flow_From_dam_78-17.csv'),  stringsAsFactors = F)

as.factor(putah_flow_data$month)
putah_july_to_october <- subset.data.frame(putah_flow_data, month == c('7', '8', '9', '10'))


hydro_full_year_line_plot <- ggplot(data = putah_flow_data, aes(x = day_of_year, y = released_cms, group = year, color=accord_status)) +
  geom_line()+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="right")+
  scale_color_manual(name="Time period", values=c("orange","grey"), labels=c("Post-accord", "Pre-accord"))+
  xlab("Day of Year")+
  ylab("Release CMS")
hydro_full_year_line_plot
# ggsave(plot=hydro_full_year_line_plot, "outputs/hydro_full_year_line_plot.png", width=8, height=6)

hydro_summer_line_plot <- ggplot(data = putah_july_to_october, aes(x = day_of_year, y = released_cms, group = year, color=accord_status)) +
  geom_line() +
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="right")+
  scale_color_manual(name="Time period", values=c("orange","grey"), labels=c("Post-accord", "Pre-accord"))+
  xlab("Day of Year")+
  ylab("Release CMS")
hydro_summer_line_plot
# ggsave(plot=hydro_summer_line_plot, "outputs/hydro_summer_line_plot.png", width=8, height=6)

#combining age hist and proportions plots
hydro_combo_plot <- plot_grid(hydro_full_year_line_plot,hydro_summer_line_plot,
                             labels = c('A', 'B'), ncol = 1)
hydro_combo_plot
ggsave(plot=hydro_combo_plot, "outputs/hydro_combo_plot.png", width = 6, height = 6)

