#setwd("~/Documents/ML-UYork/ML1000/Asignement2")
#setwd ("c:/Users/sarpr/Desktop/Machine Learning/Assignments/Assignment2")
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(mice)
library(RColorBrewer)
library(caret)


########################################### Reading files


demographic   = read.csv("Data/Raw/demographic.csv", header = TRUE, na.strings = c("NA","","#NA"))
diet          = read.csv("Data/Raw/diet.csv", header = TRUE, na.strings = c("NA","","#NA"))
examination   = read.csv("Data/Raw/examination.csv", header = TRUE, na.strings = c("NA","","#NA"))
labs          = read.csv("Data/Raw/labs.csv", header = TRUE, na.strings = c("NA","","#NA"))
medications   = read.csv("Data/Raw/medications.csv", header = TRUE, na.strings = c("NA","","#NA"))
questionnaire = read.csv("Data/Raw/questionnaire.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))

############################################## Merging & Combining files

data_List = list(demographic,examination,diet,labs,questionnaire,medications)
Data_joined = join_all(data_List) 

#dir.create("Data/Raw_Joined")
#write.csv(Data_joined,file = "Data/Raw_Joined/Data_joined.csv")



########################################## Stats on each of the datasets######################

################## demographic_MS : MS stand for missing data ###############################

demographic_MS <- demographic %>% summarise_all(~(sum(is.na(.))/n()))
demographic_MS <- gather(demographic_MS, key = "variables", value = "percent_missing")
demographic_MS <- demographic_MS[demographic_MS$percent_missing > 0.0, ] 
ggplot(demographic_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Demographic Missing Data By Columns")


##################   diet_MS : MS stand for missing data###################################


diet_MS <- diet %>% summarise_all(~(sum(is.na(.))/n()))
diet_MS <- gather(diet_MS, key = "variables", value = "percent_missing")
diet_MS <- diet_MS[diet_MS$percent_missing > 0.0, ] 
ggplot(diet_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Diet Missing Data By Columns")


################## examination_MS : MS stand for missing data#############################

examination_MS <- examination %>% summarise_all(~(sum(is.na(.))/n()))
examination_MS <- gather(examination_MS, key = "variables", value = "percent_missing")
examination_MS <- examination_MS[examination_MS$percent_missing > 0.0, ] 
ggplot(examination_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Examination Missing Data By Columns")



################## medications_MS : MS stand for missing data ##########################

medications_MS <- medications %>% summarise_all(~(sum(is.na(.))/n()))
medications_MS <- gather(medications_MS, key = "variables", value = "percent_missing")
medications_MS <- medications_MS[medications_MS$percent_missing > 0.0, ] 
ggplot(medications_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Medications Missing Data By Columns")





##############################labs_MS : MS stand for missing data ####################

sapply(labs, function(x) sum(is.na(x)))

labs_MS <- labs %>% summarise_all(~(sum(is.na(.))/n()))
labs_MS <- gather(labs_MS, key = "variables", value = "percent_missing")
labs_MS <- labs_MS[labs_MS$percent_missing > 0.0, ] 
ggplot(labs_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle(" Labs Missing Data By Columns")





################## questionnaire_MS : MS stand for missing data ##################

questionnaire_MS <- questionnaire %>% summarise_all(~(sum(is.na(.))/n()))
questionnaire_MS <- gather(questionnaire_MS, key = "variables", value = "percent_missing")
questionnaire_MS <- questionnaire_MS[questionnaire_MS$percent_missing > 0.0, ] 
ggplot(questionnaire_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Questionnaire Missing Data By Columns")


################## Data_processed_MS : MS stand for missing data ##################

Data_processed_MS <- Data_processed %>% summarise_all(~(sum(is.na(.))/n()))
Data_processed_MS <- gather(Data_processed_MS, key = "variables", value = "percent_missing")
Data_processed_MS <- Data_processed_MS[Data_processed_MS$percent_missing > 0.0, ] 
ggplot(Data_processed_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  ggtitle("Combined Data Missing Data By Columns")

#############################################################################################
################################### Data Exploration#########################################
#############################################################################################
#############################################################################################

nrow(Data_joined)
ncol(Data_joined)
str(Data_joined)
Data_joined = cbind(Data_joined, Diabetes = ifelse(
  Data_joined$LBXGH >= 5.7,
  "Yes", "No" ))
summary(Data_joined$Diabetes)
Data_joined = cbind(Data_joined, Target = ifelse(
  Data_joined$Diabetes == "Yes",
  1, 0 ))
summary(Data_joined$Target)
str(Data_joined$Target)
#dir.create("Data/Raw_Joined")
#write.csv(Data_joined,file = "Data/Raw_Joined/Data_joined.csv")

########################DIABETES VS GENDER############################## 

Data_processed<- Data_joined
attach(Data_processed)
freq_tbl=table(Diabetes)
head(freq_tbl)
prop.table(freq_tbl)

freq_xtab=xtabs(~RIAGENDR+Target)
head(freq_xtab)
Data_processed$RIAGENDR <- with(Data_processed, ifelse(as.integer(RIAGENDR)== 1, 'M', 
                                                       ifelse(as.integer(RIAGENDR)==2,'F',
                                                              RIAGENDR)))


##########################BAR PLOTs FOR DIABETES VS GENDER#####################

attach(Data_processed)
## GENDER w.r.t. our Target Variable
freq_xtab=xtabs(~RIAGENDR+Target)
head(freq_xtab)
prop.table(freq_xtab)
barplot(freq_xtab,
        legend = rownames(freq_xtab),
        ylab = "Number", xlab = "Target Variable",
        col = brewer.pal(3, name = "Dark2"),
        main = "Difference in Target Variable w.r.t Gender ")
barplot(prop.table(freq_xtab),
        legend = rownames(freq_xtab),
        ylab = "Percent", xlab = "Target Variable",
        col = brewer.pal(3, name = "Dark2"),
        main = "Difference in Target Variable w.r.t Gender ")

####################################Demographics#############################################

nrow(demographic)
ncol(demographic)
summary(demographic)
str(demographic)
demographic_indexed <- demographic
colnames(demographic_indexed) <- with(Dictionary,
                                      Dictionary$Variable.Description[match(colnames(demographic),
                                                                            Dictionary$Variable.Name,
                                                                            nomatch = Dictionary$Variable.Name
                                      )])
Demogramphic_Col_Labes <- cbind(c(colnames(demographic)), 
                                c(colnames(demographic_indexed)))
#dir.create("Data/Labels")
#write.csv(Demogramphic_Col_Labes,file = "Data/Labels/Demogramphic_Col_Labes.csv")

if (length(nearZeroVar(Data_processed, freqCut = 100/4, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  Data_processed <- Data_processed[, -nearZeroVar(Data_processed, freqCut = 100/4, uniqueCut = 10, saveMetrics = FALSE,
                                                  names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
}

summarise(Data_processed$RIAGENDR, (count(is.na(.))/n()))




############################################## Diet####################################

nrow(diet)
ncol(diet)
summary(diet)
str(diet)

diet_indexed <- diet
colnames(diet_indexed) <- with(Dictionary,
                               Dictionary$Variable.Description[match(colnames(diet),
                                                                     Dictionary$Variable.Name,
                                                                     nomatch = Dictionary$Variable.Name)
                                                               ])
diet_Col_Labes <- cbind(c(colnames(diet)), 
                        c(colnames(diet_indexed)))
#r.create("Data/Labels")
#write.csv(diet_Col_Labes,file = "Data/Labels/diet_Col_Labes.csv")




########################################### Examination###############################

nrow(examination)
ncol(examination)
summary(examination)
str(examination)

examination_indexed <- examination
colnames(examination_indexed) <- with(Dictionary,
                                      Dictionary$Variable.Description[match(colnames(examination),
                                                                            Dictionary$Variable.Name,
                                                                            nomatch = Dictionary$Variable.Name)
                                                                      ])
examination_Col_Labes <- cbind(c(colnames(examination)), 
                               c(colnames(examination_indexed)))
#dir.create("Data/Labels")
write.csv(examination_Col_Labes,file = "Data/Labels/examination_Col_Labes.csv")



############################################ Labs###############################################

nrow(examination)
ncol(examination)
summary(examination)
str(examination)



############################################### Medications######################################

nrow(medications)
ncol(medications)
summary(medications)
str(medications)




#################################### Questionnaire#############################################


nrow(questionnaire)
ncol(questionnaire)
summary(questionnaire)
str(questionnaire)

questionnaire_indexed <- questionnaire
colnames(questionnaire_indexed) <- with(Dictionary,
                                        Dictionary$Variable.Description[match(colnames(questionnaire),
                                                                              Dictionary$Variable.Name,
                                                                              nomatch = Dictionary$Variable.Name)
                                                                        ])
questionnaire_Col_Labes <- cbind(c(colnames(questionnaire)), 
                                 c(colnames(questionnaire_indexed)))
#r.create("Data/Labels")
write.csv(questionnaire_Col_Labes,file = "Data/Labels/questionnaire_Col_Labes.csv")








#Define the predictors
#Train the model
#Evaluate the model


