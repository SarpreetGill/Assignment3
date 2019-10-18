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


########################################### Reading files ###########################


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





############################## labs_MS : MS stand for missing data ####################



labs_MS <- labs %>% summarise_all(~(sum(is.na(.))/n()))
labs_MS <- gather(labs_MS, key = "variables", value = "percent_missing")
labs_MS <- labs_MS[labs_MS$percent_missing > 0.0, ] 
ggplot(labs_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle(" Labs Missing Data By Columns")

sapply(labs, function(x) sum(is.na(x)))



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


#############################################################################################
#############################################################################################
########################  DIABETES VS GENDER  ############################################### 
#############################################################################################
#############################################################################################

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

################################################################################
######################    MISSING VALUES     ##############################
################################################################################


####################################Demographics#############################################

nrow(demographic)
ncol(demographic)
summary(demographic)
str(demographic)

##################################### ZV/NZV feature remove#########################

demographic_major <- demographic

if (length(nearZeroVar(demographic_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  demographic_major <- demographic_major[, -nearZeroVar(demographic_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                 names = FALSE, foreach = FALSE, allowParallel = TRUE)] 

                                                                                      }
#Check the data for missing values.


sapply(demographic_major, function(x) sum(is.na(x)))

NumColm <- c(4,5,9,27:31,38:44)
CategColm <- c(1:3,6:8,10:26,32:37)
WorkingColm <- c(NumColm, CategColm)
demographic_selected = subset(demographic_major,select=WorkingColm )
demographic_selected[, CategColm] <- sapply(demographic_selected[, CategColm], as.numeric)

#Look the dataset structure.
str(demographic_selected)
sapply(demographic_selected, function(x) sum(is.na(x)))



#==========================  IMPUTATION( MICE package)   =======================
#Precisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#==============================================================================

init = mice(demographic_selected, maxit=0)
meth = init$method
predM = init$predictorMatrix

##remove the variable as a predictor but still will be imputed. Just for illustration purposes,

predM[, c(CategColm)]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""



#++++++++++++++++++++++++++++++++++

##Now let specify the methods for imputing the missing values.
## we impute only the Numerical Variable

meth[c(NumColm)]="pmm"

#meth[c("property_type_name")]="norm"
#meth[c("loan_type_name")]="logreg"
#meth[c("loan_purpose_name")]="polyreg"

set.seed(103)
imputed = mice(demographic_selected, method=meth, predictorMatrix=predM, m=5)

#Create a dataset after imputation.
demographic_imputed<- complete(imputed)

#Check for missings in the imputed dataset.
sapply(demographic_imputed, function(x) sum(is.na(x)))



############################################## Diet####################################

nrow(diet)
ncol(diet)
summary(diet)
str(diet)


########################################### Examination###############################

nrow(examination)
ncol(examination)
summary(examination)
str(examination)




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






#Define the predictors
#Train the model
#Evaluate the model


