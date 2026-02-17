#Natal origins script

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

#Clear the workspace
rm(list = ls())

set.seed(18012025)
# Read in data ------------------------------------------------------------
#Read in data file from data
wateriso_readin <- read.csv(here('data','waterisoscape.csv'), header=TRUE, sep=",", dec=".", stringsAsFactors=FALSE)

#Read in Sr isotope otolith data file
pc_sr<-read.csv(here('outputs','pc_sr.csv'),  stringsAsFactors = F)

#Read in TE data natal predictions
natal_te_data<-read.csv(here('outputs','predicted_te_natal.csv'),  stringsAsFactors = F)

# Isoscape ----------------------------------------------------------------
#Clean water isoscape data
wateriso <- wateriso_readin %>% 
  #If you change this remember to also change the 1:17 at the grouped classification stage
  #Remove spawning sites in the SAC river
  filter(!Site=="SAC")%>%
  mutate(Site=as.factor(Site))%>%
  mutate(Site_Group=as.factor(Site_Group))


#Create the waterisoscape boxplot 
#This is the raw data
p_isobox <- ggplot(data=wateriso)+
  geom_boxplot(aes(x=reorder(Site_Group,Ordering,fun=min), y=Sr87Sr86_rf, fill=Type), outlier.shape=NA)+
  geom_point(aes (x=reorder(Site_Group,Ordering,fun=min), y=Sr87Sr86_rf,group=Site, fill=Type), color="black",
             position = position_jitterdodge(jitter.width = 0.2), alpha=0.2)+
  scale_fill_manual(values=c("#AF8DC3","#7FBF7B"), name="")+
  scale_y_continuous(breaks = seq(0.704, 0.710, 0.001), name=expression(""^87*Sr*"/"^86*Sr))+
  xlab("Central Valley Rivers and Hatcheries")+
  geom_hline(yintercept = 0.70918, linetype="dotted", color="black")+  #Mean Ocean   
  annotate("text",x =2, y = 0.70938, label = "Pacific Ocean")+
  # annotate("text",x =2, y = 0.70765, label = "SFE 0.5 psu")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="top",
         legend.key = element_rect(fill = NA, color = NA))
p_isobox
ggsave(plot=p_isobox, "outputs/waterisoscape.png", width = 7, height = 5)


#Split the data into training and test
trainIndex <- createDataPartition(wateriso$Site_Group, p = .75, 
                                  list = FALSE, 
                                  times = 1)
wateriso_train <-wateriso[ trainIndex,]
wateriso_test  <-wateriso[-trainIndex,]


#Resampled training dataset. 
wateriso_train_boot <-  wateriso_train%>%
  group_by(Site_Group)%>% 
  dplyr::sample_n(size=25, replace=TRUE) %>%
  mutate (Sr87Sr86_rf_mean=mean(Sr87Sr86_rf, na.rm=TRUE))%>%
  ungroup()%>%
  data.frame()%>%
  mutate(Site_order= fct_reorder(Site_Group, Ordering_boot))

#Classification model
fit_bag <- train(
  Site_Group~ Sr87Sr86_rf,
  data = wateriso_train_boot,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  ntree=500
)
fit_bag

#Evaluate CART on test data
predict_test<- predict(fit_bag, wateriso_test)
caret::confusionMatrix(predict_test,wateriso_test$Site_Group)
CART_summary_test <-caret::confusionMatrix(predict_test,wateriso_test$Site_Group)
write.csv (CART_summary_test$table, "outputs/CART_summary_test_updated.csv", row.names=T)

#Predict fish
natal_oto<-pc_sr %>%
  distinct(lab_id, region_name, Sr87Sr86_rf=region_mean87Sr86Sr) %>%
  filter(region_name=="Natal")

predict_natal <- data.frame(predict(fit_bag,natal_oto, type="prob")) #remove type prob to get just the summary
predicted_dataframe <- cbind("lab_id"=natal_oto$lab_id, predict_natal)
write.csv (predicted_dataframe, "outputs/natal_predicted_probs.csv", row.names=FALSE)


## Set your cutoff here
ap_cut = 0.85
predicted_dataframe_rf_summary_1_alt <-predicted_dataframe %>%
  mutate_if(is.factor,as.character)%>% #change factors to chars so the numeric command doesnt give the factor level
  mutate_at(2:17,as.numeric)%>%
  gather(Classification_initial, Assgined_Probability, 2:17)%>%
  group_by(lab_id) %>%
  mutate (Assgined_Probability=as.numeric(Assgined_Probability))%>%
  #First assignment
  mutate (ap_counter=max(Assgined_Probability)) %>%
  mutate (assignment= case_when (Assgined_Probability==max(Assgined_Probability)~Classification_initial))%>%
  mutate (Assgined_Probability= case_when(Assgined_Probability==max(Assgined_Probability)~0,
                                          TRUE~Assgined_Probability))%>%
  #Second assignment if below ap_cut
  mutate (assignment= case_when (ap_counter  <ap_cut & Assgined_Probability==max(Assgined_Probability)~Classification_initial,
                                 TRUE~as.character(assignment)))%>%
  mutate (ap_counter=case_when(ap_counter <ap_cut~(ap_counter+max(Assgined_Probability)),
                               TRUE~ap_counter))%>%
  mutate (Assgined_Probability= case_when(is.na(assignment)~Assgined_Probability,
                                          TRUE~0))%>%
  #Third assignment if still below ap_cut
  mutate (assignment= case_when (ap_counter  <ap_cut & Assgined_Probability==max(Assgined_Probability)~Classification_initial,
                                 TRUE~as.character(assignment)))%>%
  mutate (ap_counter=case_when(ap_counter <ap_cut~(ap_counter+max(Assgined_Probability)),
                               TRUE~ap_counter))%>%
  mutate (Assgined_Probability= case_when(is.na(assignment)~Assgined_Probability,
                                          TRUE~0))%>%
  #Create name
  na.omit()%>%
  mutate (grouped_classification = paste0(assignment, collapse = ""))


#Data frame for further analyses and plotting
#Bring in the TE data by natal origin
TE_PUC <-natal_te_data %>%
  mutate(assignment=case_when(PUC>ap_cut ~ "PUC",
                              TRUE ~"Unknown"))%>%
  filter(assignment=="PUC")
  
natal_data <- predicted_dataframe_rf_summary_1_alt %>%
  distinct(lab_id, .keep_all=TRUE)%>%
  mutate(assignment=case_when(lab_id %in% TE_PUC$Fish_ID ~"PUC",
                              TRUE~assignment))%>%
  #Assign the PUC fish based on TE data and switch the original ones to FEA if found below ap threshold
  mutate(grouped_classification=case_when(lab_id %in% TE_PUC$Fish_ID ~"PUC",
                              TRUE~grouped_classification))%>%
  mutate(grouped_classification=case_when(grouped_classification=="FEAPUC"~"FEA",
            TRUE~grouped_classification))%>%
  group_by(assignment)%>%
  mutate(n_assignment=n())%>%
  ungroup()%>%
  group_by(grouped_classification)%>%
  mutate(n_grouped_classification=n())%>%
  ungroup()%>%
  distinct(lab_id, assignment, grouped_classification, n_assignment, n_grouped_classification)

write.csv (natal_data, "outputs/natal_data.csv", row.names=FALSE)


# QAQCPlots -------------------------------------------------------------------

#Histogram plot of natal origins
p_histpredict <- ggplot(data=natal_data)+
  geom_bar(aes(x=reorder(grouped_classification,-n_grouped_classification),
               fill=reorder(grouped_classification,-n_grouped_classification)), stat = "count", color="black")+
  scale_fill_viridis(discrete = T, name="Natal origin")+
  theme_classic()+
  theme (panel.background = element_rect(colour = "black"), legend.position="right")+
  xlab("Predicted natal origin")+
  ylab("Number of Fish")
p_histpredict
ggsave(plot=p_histpredict, "outputs/Hist_predict_QAQC.png", width = 14, height = 6)
