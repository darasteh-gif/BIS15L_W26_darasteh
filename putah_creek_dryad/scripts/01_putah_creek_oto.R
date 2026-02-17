#Otolith size, width, and weight to reconstruct fish size
#14. January 2025

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(stringr)
library(here) 
library(cowplot) 
library(openxlsx) 
library(yardstick)
library(rstatix) 
library(ggpubr) 
library(FSA)


#Clear the workspace
rm(list = ls())


# Load data ---------------------------------------------------------------
#Load otolith data
otolith_readin <- read.xlsx(here::here("data", "PC_otolith_weights.xlsx"),
                            sheet = "oto_measurements")
otolith_lengths_readin<- read.xlsx(here::here("data", "PC_otolith_measurements_2017-2021.xlsx"),
                                   sheet = "lengths")

otolith_lengths <- otolith_lengths_readin%>%
  dplyr::select(lab_id, sample_id, measurement_number, length)%>%
  pivot_wider(names_from=measurement_number, values_from=length)
  

#Load metadata from the field
#Need to select which columns to keep and make sure their names are the same among years
otolith_metadata2017 <- read.xlsx(here::here("data", "Otolith_Inventory_2017-2021.xlsx"),
                                  sheet = "2017 Metadata")%>%
  mutate(FL_mm = as.numeric(FL))%>%
  dplyr::select("Fish_ID", "FL_mm", "Sex","Adipose",
                "CWT","Hatchery","CWT_Age","CWT_Release", "Date_recovered","section_alpha","survey_year") 
  
otolith_metadata2018 <- read.xlsx(here::here("data", "Otolith_Inventory_2017-2021.xlsx"),
                                  sheet = "2018 Metadata")%>%
  mutate(FL_mm = as.numeric(FL_mm))%>%
  dplyr::select("Fish_ID", "FL_mm", "Sex","Adipose",
                "CWT","Hatchery","CWT_Age","CWT_Release", "Date_recovered","section_alpha","survey_year") 


otolith_metadata2020 <- read.xlsx(here::here("data", "Otolith_Inventory_2017-2021.xlsx"),
                                  sheet = "2020 Metadata")%>%
  mutate(FL_mm = as.numeric(FL_mm))%>%
  dplyr::select("Fish_ID", "FL_mm", "Sex","Adipose",
                "CWT","Hatchery","CWT_Age","CWT_Release", "Date_recovered","section_alpha","survey_year") 

otolith_metadata2021 <- read.xlsx(here::here("data", "Otolith_Inventory_2017-2021.xlsx"),
                                  sheet = "2021 Metadata")%>%
  mutate(FL_mm = as.numeric(FL_mm))%>%
  dplyr::select("Fish_ID", "FL_mm", "Sex","Adipose",
                "CWT","Hatchery","CWT_Age","CWT_Release", "Date_recovered","section_alpha","survey_year") 

otolith_metadata <- rbind(otolith_metadata2017,otolith_metadata2018,otolith_metadata2020,otolith_metadata2021)

otolith_agescores_readin <- read.xlsx(here::here("data", "putahcreek_otolith_agescores.xlsx"), sheet="ages")

# Clean and merge data ----------------------------------------------------
otolith_data <- otolith_lengths%>%
  left_join(otolith_metadata, by=c("sample_id"="Fish_ID"))%>%
  left_join (otolith_readin , by="sample_id")%>%
  mutate(Date_recovered=as.Date(as.numeric(Date_recovered), origin = "1899-12-30"))


# Linear regression of weight and fish size -------------------------------

#XY plot: weight & fish size (with trendline)
weight_plot <- ggplot(otolith_data, aes(x=weight,y= FL_mm)) +
  geom_point() +
  #geom_text() +
  geom_smooth(method=lm, color="black")+
  scale_x_continuous("Weight (g)")+
  scale_y_continuous("Fork length (mm)")+
  theme_classic()
weight_plot
# ggsave("outputs/weight_plot.png", weight_plot,  width = 5, height = 5)

#Labeling individual points with geom_text identified the following outlier fish: 2020-024, 2020-048, and 2021-023.

#Linear model: weight & fork length
weight_model <- lm(FL_mm~ weight, data=otolith_data)
summary(weight_model)

#Basic stat tests: Normality, homogeneity of variance, quantile-quantile plot 
# Create a QQ plot of residuals
ggqqplot(residuals(weight_model)) 

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(weight_model))

#residuals versus fits plot
plot(weight_model, 1)

#Now predict the fish sizes and evaluate
putah_pred <-predict.lm(weight_model,otolith_data, interval="confidence") %>%
  cbind(otolith_data)%>%
  #Weight pred=The predicted fish length in mm based on otolith weight
  mutate(FL_weight_pred=fit,
         FL_weight_pred_lwr=lwr,
         FL_weight_pred_upr=upr)%>%
  mutate(FL_dif_weight=FL_mm-FL_weight_pred)


#Plot for evaluation of the fish size predictions
p_predict_weight <-ggplot()+
  geom_line(data=putah_pred%>%filter(!is.na(FL_mm)),
            aes(x=weight,y=FL_weight_pred),color="orange") +
  geom_point(data=putah_pred%>%filter(!is.na(FL_mm)),
            aes(x=weight,y=FL_mm),color="orange") +
  geom_segment(data=putah_pred%>%filter(!is.na(FL_mm)),
               aes(x=weight,y=FL_mm, yend=FL_weight_pred, xend=weight),
               color="orange") +
  geom_point(data=putah_pred%>%filter(is.na(FL_mm)),
             aes(x=weight,y=FL_weight_pred),color="red3",
             size=3,shape=17) +
    scale_x_continuous("Otolith weight (mg)")+
    scale_y_continuous("Fish Fork length (mm)")+
    scale_color_manual(values = c("orange", "red3"),
                       guide = guide_legend(override.aes = list(
                         linetype = rep("blank",2),
                         shape = c(16, 17),
                         size=c(2,3)
                       )),
                       labels = c("Measured FL","Predicted unknown FL"))+
    theme_classic()
p_predict_weight
# ggsave("outputs/predict_weight.png", p_predict_weight,  width = 5, height = 5)

#Export predicted FL data based on weight
write.csv(putah_pred, "outputs/putah_pred.csv",row.names=F)

# Linear regression of length measurements x weight -------------------------

#XY plot otolith length x otolith width 
length_width_plot <- ggplot(otolith_data, aes(x=length,y= width)) +
  geom_point() +
  #geom_text() +
  geom_smooth(method=lm, color="black")+
  theme_classic() +
  xlab("Otolith length (mm)") + 
  ylab("Otolith width (mm)")
length_width_plot
# ggsave("outputs/length_width_plot.png", length_width_plot,  width = 5, height = 5)

#outliers labeled with geom_text: 2017_211A, 2017_169A, 2020-070

#Linear model: otolith length x otolith width
lengths_model <- lm(width ~ length, data=otolith_data)
summary(lengths_model)
#Multiple R-squared=0.5549, Adjusted R-squared=0.5529

#Basic stat tests: Normality, homogeneity of variance, quantile-quantile plot 
# Create a QQ plot of residuals
ggqqplot(residuals(lengths_model)) 

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(lengths_model))

#residuals versus fits plot
plot(lengths_model, 1)


# Otolith measurements vs fish measurements ----------------------------------

#XY plot of otolith length x fish length
length_plot <- ggplot(otolith_data, aes(x=length,y= FL_mm)) +
  geom_point() +
  #geom_text() +
  geom_smooth(method=lm, color="black") +
  theme_classic() +
  xlab("Otolith length (mm)") +
  ylab("Fork length (mm)")
length_plot
# ggsave("outputs/length_plot.png", length_plot,  width = 5, height = 5)

lengthxfl_model <- lm(FL_mm ~ length, data=otolith_data)
summary(lengthxfl_model)
#Multiple R-squared = 0.5525, adjusted R-squared = 0.5503

width_plot <- ggplot(otolith_data, aes(x=width,y= FL_mm)) +
  geom_point() +
  # geom_text() +
  geom_smooth(method=lm, color="black")+
  theme_classic() +
  xlab("Otolith width (mm)") +
  ylab("Fork length (mm)")
width_plot
# ggsave("outputs/width_plot.png", width_plot,  width = 5, height = 5)

widthxfl_model <- lm(FL_mm ~ width, data=otolith_data)
summary(widthxfl_model)
#Multiple R-squared = 0.4374, adjusted R-squared = 0.4348


#just double-checking the min and max values for all these measurements to confirm they're reasonable:
summary(otolith_data$length)
#Min = 4.901, Mean = 8.722, Max = 11.688

summary(otolith_data$width)
#Min = 3.457, Mean = 4.500, Max = 6.060

summary(otolith_data$FL_mm)
#Min = 452, Mean = 756, Max = 1030


# Assessing Otolith age distribution -----------------------------------------
otolith_agescores <- otolith_agescores_readin %>%
  drop_na(final_age)%>%
  mutate(age = as.numeric(final_age)) %>%
  mutate(age_f=factor(age))

age_plot <- ggplot(data=otolith_agescores) +
  geom_bar(aes(x=age_f), stat="count") +
  theme_bw() +
  facet_wrap(~Year)+
  xlab("Age") +
  ylab("Number of fish")
age_plot
# ggsave("outputs/age_plot.png", age_plot,  width = 5, height = 5)

## Age Bias plot Reader1 vs Reader2
ab.tA2 <- ageBias(gw_age_est_1~ke_age_est_1, data=otolith_agescores,
                  ref.lab=" ke_age",nref.lab="gw_age")
plot(ab.tA2)

## Age precision
ap.A <- agePrecision(gw_age_est_1~ke_age_est_1, data=otolith_agescores)
summary(ap.A,what="difference")
summary(ap.A,what="precision")



#Final dataframe for export for further analyses
#Include the predicted FL based on otolith weight
putah_pred_join<- putah_pred %>%
  dplyr::select(lab_id, FL_weight_pred)

otolith_data_export <- otolith_data %>%
  left_join(otolith_agescores, by=c("lab_id"="sample_id"))%>%
  left_join(putah_pred_join, by="lab_id")%>%
  mutate(FL_mm=case_when (is.na(FL_mm)~FL_weight_pred,
                          TRUE~FL_mm))%>%
  dplyr::select(lab_id,length, width,weight, FL_mm, age, age_f,
                Sex,Adipose,CWT,Hatchery,CWT_Age,CWT_Release, Date_recovered, section_alpha, survey_year)

write.csv(otolith_data_export, "outputs/otolith_data_export.csv",row.names=F)
