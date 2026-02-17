#Trace Element data reduction

# Prepare the environment -------------------------------------------------
library(tidyverse)
library(here) #better working directory management
library(openxlsx) # read in excel files xlsx
library(stringr) #working with strings
library(caret)# Stat model functions
library(viridis)#Color scale
library(cowplot) #GGplot extensions


#Clear the workspace
rm(list = ls())
set.seed(12042023)

# Read in data ------------------------------------------------------------
#Iolite timeseries data
paths<-list.files(path = here("data","TE","timeseries_data"),full.names = TRUE, recursive = TRUE)
filenames<- list.files(path = here("data","TE","timeseries_data"), recursive = TRUE)
iolite_readin <-data_frame(filename = filenames) %>% 
  mutate(file_contents = map(paths, ~read_csv (., skip = 1)))%>%
  unnest(cols = c(file_contents))

#Laser runnotes
te_notes <- read.xlsx(here::here("data", "TE","putah_creek_trace_elements_run_notes_20230328.xlsx"),
                            sheet = "sample_notes")


# Clean data --------------------------------------------------------------
TE_data <- iolite_readin%>%
  mutate(Join_ID=str_sub(filename,1,-5))%>%
  #Join runnotes
  left_join(te_notes, by="Join_ID")%>%
  #Calculate time interval and check 0.97
  mutate(timedif=`Elapsed Time` - lag(`Elapsed Time`))%>%
  drop_na(Fish_ID)%>%
  #Calculate distance
  group_by(Fish_ID)%>%
  mutate(Distance=row_number()*0.97*run_speed_ums)%>%
  #Determine natal region
  mutate(Region=case_when(Distance >=Natal_start & Distance <=Natal_stop ~"Natal",
                          TRUE~"Profile"))%>%
  #Select columns to keep
  dplyr::select(Fish_ID,Distance, Natal_start, Natal_stop, Origin, Lifestage, Region, Mg24_ppm, Ca44_ppm, Mn55_ppm, Zn66_ppm, Sr88_ppm, Ba137_ppm)%>%
 #Calculate ratios to Ca44 based on the ppm values
  mutate (Mg_Ca =Mg24_ppm/Ca44_ppm) %>%
  mutate (Mn_Ca =Mn55_ppm/Ca44_ppm) %>%
  mutate (Zn_Ca =Zn66_ppm/Ca44_ppm) %>%
  mutate (Sr_Ca =Sr88_ppm/Ca44_ppm) %>%
  mutate (Ba_Ca =Ba137_ppm/Ca44_ppm) %>%
  # Change to long format
  pivot_longer(names_to="Element", values_to="Value", 8:18)%>%
  #Remove CWT fish
  filter(!Fish_ID=="PC_2021_066")

#Plot with outliers for each measured element and ratio. Each fish on its own page. 
#Filter which elements to plot
element_plotter <-TE_data %>%
  filter(Element%in% c("Mg_Ca", "Mn_Ca", "Zn_Ca","Sr_Ca","Ba_Ca"))

fish_plotter <- unique(element_plotter$Fish_ID)
fish_plotter #Show list of fish that will be plotted

#Create panel plot for each fish
pdf("outputs/PC_TE_withoutliers.pdf",16, 8)
for(i in fish_plotter){
  plot_outlier<- element_plotter %>% filter (Fish_ID==i)
  p <- ggplot(data=plot_outlier)+
  geom_point(aes(x=Distance,y=Value, color=Element))+
  geom_smooth(aes(x=Distance,y=Value, color=Element), method="loess", span = 0.1)+
  geom_vline(aes(xintercept=Natal_start))+
  geom_vline(aes(xintercept=Natal_stop))+
  facet_wrap(~Element, scales = "free")+
  theme_bw() + ggtitle(i)
  print(p)
}
dev.off()

#Plot All fish All Elements
p_allfish_allele <- ggplot(data=element_plotter)+
  geom_smooth(aes(x=Distance,y=Value, color=Fish_ID))+
  theme_bw() + 
  theme(legend.position = "none")+
  facet_wrap(~Element, scales = "free")
p_allfish_allele
# ggsave(plot=p_allfish_allele, "outputs/allfish_allele.png", width = 10, height = 10)


# Mean natal values -------------------------------------------------------
TE_natal <-element_plotter%>%
  group_by(Fish_ID, Element, Region)%>%
  mutate(Value_mean=mean(Value, na.rm=T))%>%
  filter(Region=="Natal")%>%
  ungroup()%>%
  distinct(Fish_ID, Origin, Lifestage,Element, Value_mean)%>%
  mutate(Element_label=case_when(Element=="Ba_Ca"~"Ba/Ca",
                                 Element=="Mg_Ca"~"Mg/Ca",
                                 Element=="Mn_Ca"~"Mn/Ca",
                                 Element=="Sr_Ca"~"Sr/Ca",
                                 Element=="Zn_Ca"~"Zn/Ca"))

#Plot All fish All Elements by origin
p_allfish_org <- ggplot(data=TE_natal)+
  geom_boxplot(aes(x=Origin,y=Value_mean, fill=Origin), width=0.5)+
  geom_point(aes(x=Origin,y=Value_mean), alpha=0.5)+
  scale_fill_manual(values=c("#2D708EFF","#FDE725FF", "#20A387FF"))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="none",
         legend.key = element_rect(fill = NA, color = NA))+
  ylab("Mean Natal Element/Ca")+
  facet_wrap(~Element_label, scales = "free")
p_allfish_org
ggsave(plot=p_allfish_org, "outputs/allfish_org.png", width = 8, height = 4)


#Sr vs Ba plot
TE_natal_srba <-TE_natal %>%
  dplyr::select(-Element_label)%>%
  filter(Element %in% c("Ba_Ca","Sr_Ca"))%>%
  pivot_wider(names_from=Element, values_from=Value_mean)
  
#Plot All fish All Elements by origin
p_allfish_srba <- ggplot(data=TE_natal_srba)+
geom_point(aes(y=Ba_Ca,x=Sr_Ca, fill=Origin, shape=Origin), size=2)+
  scale_fill_manual(values=c("#2D708EFF","#FDE725FF", "#20A387FF"))+
scale_shape_manual(values=c(21,22,23))+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="right",
         legend.key = element_rect(fill = NA, color = NA))+
xlab("Sr/Ca")+
ylab("Ba/Ca")
p_allfish_srba
ggsave(plot=p_allfish_srba, "outputs/p_allfish_srba.png", width = 4, height = 3)  

# Build model to assign unknown fish --------------------------------------
TE_model <- TE_natal %>%
  dplyr::select(-Element_label)%>%
  pivot_wider(names_from="Element", values_from=("Value_mean"))%>%
  #Scale data
  mutate(across(is.numeric, ~ as.numeric(scale(.))))

TE_train <- TE_model %>%
  filter(Lifestage=="Juvenile")%>%
  mutate(Origin=factor(Origin))
  
fit_te_model <- train(
  Origin~ Sr_Ca+Ba_Ca,
  data = TE_train,
  method = "lda",

)
fit_te_model

#Evaluate on train data
te_model_summary_train <- caret::confusionMatrix(predict(fit_te_model),TE_train$Origin)
te_model_summary_train
write.csv (te_model_summary_train$table, "outputs/te_model_summary_train.csv", row.names=T)

#Predict fish
TE_predict<-TE_model %>%
  filter(Lifestage=="Adult")

predict_te_natal <- data.frame(predict(fit_te_model,TE_predict, type="prob")) #remove type prob to get just the summary
predicted_te_natal<- cbind("Fish_ID"=TE_predict$Fish_ID, predict_te_natal)
write.csv (predicted_te_natal, "outputs/predicted_te_natal.csv", row.names=FALSE)

