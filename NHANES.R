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

library(randomForest)
library(psych)
library(factoextra)


# Reading files

########################################### Reading files ###########################


demographic   = read.csv("Data/Raw/demographic.csv", header = TRUE, na.strings = c("NA","","#NA"))
diet          = read.csv("Data/Raw/diet.csv", header = TRUE, na.strings = c("NA","","#NA"))
examination   = read.csv("Data/Raw/examination.csv", header = TRUE, na.strings = c("NA","","#NA"))
labs          = read.csv("Data/Raw/labs.csv", header = TRUE, na.strings = c("NA","","#NA"))
medications   = read.csv("Data/Raw/medications.csv", header = TRUE, na.strings = c("NA","","#NA"))
questionnaire = read.csv("Data/Raw/questionnaire.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))

############################################## Merging & Combining files###################

data_List = list(demographic,examination,diet,labs,questionnaire,medications)
Data_joined = join_all(data_List) 

#dir.create("Data/Raw_Joined")
#write.csv(Data_joined,file = "Data/Raw_Joined/Data_joined.csv")


########################################## Stats on each of the datasets ######################

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


library(VIM) 
aggr(demographic,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)

##################   diet_MS : MS stand for missing data      ####################
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

sapply(questionnaire, function(x) sum(is.na(x)))
questionnaire_MS <- questionnaire %>% summarise_all(~(sum(is.na(.))/n()))

questionnaire_MS <- gather(questionnaire_MS, key = "variables", value = "percent_missing")

questionnaire_MS_less25 <- questionnaire_MS[questionnaire_MS$percent_missing < 0.25 , ] 

questionnaire_MS_less50 <- questionnaire_MS[(questionnaire_MS$percent_missing > 0.25 & questionnaire_MS$percent_missing < 0.5) , ]


questionnaire_MS_less75 <- questionnaire_MS[(questionnaire_MS$percent_missing > 0.50 & questionnaire_MS$percent_missing < 0.75) , ]

ggplot(questionnaire_MS_less25, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.5, alpha = 0.8)+
  xlab('variables')+ theme(axis.text.y  = element_text(size=8))+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Questionnaire Missing Data By Columns")
  ggtitle("Questionnaire Missing Data By Columns (< 0.25)")
  

ggplot(questionnaire_MS_less50, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.5, alpha = 0.8)+
  xlab('variables')+ theme(axis.text.y  = element_text(size=8))+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Questionnaire Missing Data By Columns (< 0.50)")

questionnaire_MS_less25

questionnaire_MS_less50

questionnaire_MS_less75


################################################################################
########################  diabete and demography   #############################
################################################################################

demographic_MS <- demographic %>% summarise_all(~(sum(is.na(.))/n()))
demographic_MS <- gather(demographic_MS, key = "variables", value = "percent_missing")
demographic_MS_less25 <- demographic_MS[demographic_MS$percent_missing < 0.25, ]

demographic_MS_less25

list_demo_MS_less25 <- list(demographic_MS_less25$variables)
list_demo_MS_less25

# list_demo_MS_less25 = c( "SEQN",     "SDDSRVYR", "RIDSTATR", "RIAGENDR", "RIDAGEYR", 
#"RIDRETH1", "RIDRETH3",
#"RIDEXMON", "DMDBORN4", "DMDCITZN", "SIALANG",  "SIAPROXY", "SIAINTRP", "FIALANG", 
#"FIAPROXY", "FIAINTRP", "DMDHHSIZ", "DMDFMSIZ", "DMDHHSZA", "DMDHHSZB", "DMDHHSZE",
#"DMDHRGND", "DMDHRAGE", "DMDHRBR4", "DMDHREDU", "DMDHRMAR", "WTINT2YR", "WTMEC2YR",
#"SDMVPSU",  "SDMVSTRA", "INDHHIN2", "INDFMIN2", "INDFMPIR")


#Among the 32 Column with less than 25% of missing value we have selected the 
# 8 interesting columns (4, 5, 8, 13, 14, 30, 39,46)


 
demo_subset_8_list = c("RIAGENDR", "RIDAGEYR", "RIDRETH3", "DMDBORN4",
                        "DMDCITZN", "DMDFMSIZ", "DMDHRMAR", "INDFMIN2")


demo_subset_8  <- demographic%>% 
  select("SEQN",demo_subset_8_list)

head(demo_subset_8)


demo_subset_8<- demo_subset_8 %>% 
  rename("ID"                 =         "SEQN",  
         "Gender"             =     "RIAGENDR",  
         "Age"                =     "RIDAGEYR",  
         "Race"               =     "RIDRETH3",  
         "Country_of_birth"   =     "DMDBORN4",  
         "Citizenship_status" =     "DMDCITZN",   
         "Family_members"     =     "DMDFMSIZ",  
         "Marital_status"     =     "DMDHRMAR",  
         "Family_income"      =     "INDFMIN2"  )
    
sapply(demo_subset_8, function(x) sum(is.na(x)))

str(demo_subset_8)

# Data splitting & imputation


## conversion to factors
demo_subset_8$Gender <- as.factor(demo_subset_8$Gender)
demo_subset_8$Race  <- as.factor(demo_subset_8$Race )
demo_subset_8$Country_of_birth <- as.factor(demo_subset_8$Country_of_birth)
demo_subset_8$Citizenship_status <- as.factor(demo_subset_8$Citizenship_status)
demo_subset_8$Marital_status <- as.factor(demo_subset_8$Marital_status)
#demo_subset_8$Family_income <- as.factor(demo_subset_8$Family_income)

str(demo_subset_8)


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================
require(mice)
init = mice(demo_subset_8, maxit=0)
meth = init$method
predM = init$predictorMatrix

##remove the variable as a predictor but still will be imputed. Just for illustration purposes,
##I select the "ID" variable to not be included as predictor during imputation.
predM[, c("ID")]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""
meth[c("ID")]=""
meth[c("Gender")]=""
meth[c("Age")]=""
meth[c("Race")]=""
meth[c("Country_of_birth")]=""
meth[c("Family_members")]=""
#++++++++++++++++++++++++++++++++++

##Now let specify the methods for imputing the missing values.
## we impute only the Numerical Variable
meth[c("Citizenship_status")]="cart"
meth[c("Marital_status")]="cart"
meth[c("Family_income")]="rf" # require randomforest 

set.seed(145)
imputed = mice(demo_subset_8, method=meth, predictorMatrix=predM, m=5)

#Create a dataset after imputation.
demo_subset_8_imputed<- complete(imputed)

#Check for missings in the imputed dataset.
sapply(demo_subset_8_imputed, function(x) sum(is.na(x)))

write.csv(demo_subset_8_imputed , "Data/Working/demographic_major_imputed.csv")



# histogramme

multi.hist(demo_subset_8_imputed[,sapply(demo_subset_8_imputed, is.numeric)])

#Most of the variables  have right skewed distributions.

demo_subset_8_labaled = mutate(demo_subset_8_imputed, Gender= ifelse(
  demo_subset_8_imputed$Gender == "1" , "Male", "Female" ))


require(dplyr)# because Race is a factor of level 6
demo_subset_8_labaled <- demo_subset_8_labaled %>%
  mutate(Race = recode(Race, "1" = "Mexican_American",
                       "2" = "Other_Hispanic",
                       "3" = "White",
                       "4" = "Black",
                       "6" = "Asian",
                       "7" = "multiracial"))

#demo_subset_8_processed <- demo_subset_8_processed %>% 
 # mutate(Race = replace(Race, Race == 1, "Mexican_American")) %>%
 # mutate(Race = replace(Race, Race == 2, "Other_Hispanic")) %>%
#  mutate(Race = replace(Race, Race == 3, "White")) %>%
#  mutate(Race = replace(Race, Race == 4, "Black")) %>%
#  mutate(Race = replace(Race, Race == 6, "Asian")) %>%
#  mutate(Race = replace(Race, Race == 7, "multiracial")) 
 

demo_subset_8_labaled <- demo_subset_8_labaled %>%
  mutate(Country_of_birth  = recode(Country_of_birth , "1" = "US",
                       "2" = "Others",
                       "77" = "Refused",
                       "99" = "Uknown"))

demo_subset_8_labaled <- demo_subset_8_labaled %>%
  mutate(Citizenship_status = recode(Citizenship_status, "1" = "US",
                       "2" = "Other",
                       "7" = "Refused",
                       "9" = "Unknown"))

demo_subset_8_labaled <- demo_subset_8_labaled %>%
  mutate(Marital_status = recode(Marital_status, "1" = "Married",
                       "2" = "Widowed",
                       "3" = "Divorced",
                       "4" = "Separated",
                       "5" = "Never_married",
                       "6" = "partner",
                       "77" = "Refused",
                       "99" = "Unknown"))



demo_subset_8_labaled <- demo_subset_8_labaled %>%
  mutate(Family_income = recode(Family_income, "1" = 	"$0 - $4999",
                                "2" =	"$5000 - $9999",
                                "3" =	"$10000 - $14999",
                                "4" =	"$15000 - $19999",		
                                "5" =	"$20000 - $24999",		
                                "6" =	"$25000 - $34999",		
                                "7" =	"$35000 - $44999",	
                                "8" =	"$45000 - $54999",		
                                "9" =	"$55000 - $64999",		
                                "10" = 	"$65000 - $74999",		
                                "12" =	"$20000 and Over",	
                                "13" =	"Under $20000",	
                                "14" =	"$75000 - $99999",	
                                "15" = "$100000 and Over",	
                                "77" =	"Refused",
                                "99" =	"Unknown"	))

demo_subset_8_labaled$Family_income <- as.factor(demo_subset_8_labaled$Family_income)
demo_subset_8_imputed$Family_income <- as.factor(demo_subset_8_imputed$Family_income)


write.csv(demo_subset_8_imputed , "Data/Working/demographic_imputed.csv")


##########################  Gender #############
Gender  <- demo_subset_8_labaled %>%
  group_by(Gender) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

head(Gender)

ggplot(Gender, aes(Gender)) + geom_bar(fill = "blue") + theme_bw() +
  xlab("Gender") + ylab("Count") + 
  labs(title = "Bar Chart of Gender") + theme_gray()


#Pie plot
ggplot(Gender, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Gender,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
  ggtitle("Pie plot of Gender")+
  scale_fill_grey(start = 0.8, end = 0.2,"Gender") + theme_void()




##########################  Country_of_birth #############
Country_of_birth  <- demo_subset_8_labaled %>%
  group_by(Country_of_birth) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


#Pie plot
ggplot(Country_of_birth, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Country_of_birth,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
  ggtitle("Pie plot of Country of birth")+
  scale_fill_grey(start = 0.8, end = 0.2,"Country_of_birth") + theme_void()



########################  Marital_status  #######################

Marital_status  <- demo_subset_8_labaled %>%
  group_by(Marital_status) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


#Pie plot
ggplot(Marital_status, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Marital_status ,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
  ggtitle("Pie plot of Marital status")+
  scale_fill_grey(start = 0.8, end = 0.2,"Marital_status") + theme_void()



########################  Race  #######################

Race  <- demo_subset_8_labaled %>%
  group_by(Race) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


#Pie plot
ggplot(Race, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Race ,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
  ggtitle("Pie plot of Race")+
  scale_fill_grey(start = 0.8, end = 0.2,"Races") + theme_void()

#https://en.wikipedia.org/wiki/Race_and_ethnicity_in_the_United_States
#White 	72.4%
#Hispanic and Latino Americans (of any race) 	16.3%
#Black or African American 	12.6%
#Asian 	4.8%
#Native Americans and Alaska Natives 	0.9%
#Native Hawaiians and Other Pacific Islanders 	0.2%
#Two or more races 	2.9%
#Other 	6.2%


################################################################################
########################  diabete and medication   #############################
################################################################################



################################################################################
########################    diabete and diet       #############################
################################################################################


############################################## Diet####################################

nrow(diet)
ncol(diet)
summary(diet)
str(diet)
# IMPUTE diet csv with MICE.  
#Choose 1 day worth of vitamin recorded data.        
 one_day_diet  <- c("SEQN","DR1TNUMF","DR1TKCAL","DR1TPROT","DR1TCARB","DR1TSUGR","DR1TFIBE","DR1TTFAT","DR1TSFAT","DR1TMFAT","DR1TPFAT","DR1TCHOL","DR1TATOC","DR1TATOA","DR1TRET","DR1TVARA","DR1TACAR","DR1TBCAR","DR1TCRYP","DR1TLYCO","DR1TLZ","DR1TVB1","DR1TVB2","DR1TNIAC","DR1TVB6","DR1TFOLA","DR1TFA","DR1TFF","DR1TFDFE","DR1TCHL","DR1TVB12","DR1TB12A","DR1TVC","DR1TVD","DR1TVK","DR1TCALC","DR1TPHOS","DR1TMAGN","DR1TIRON","DR1TZINC","DR1TCOPP","DR1TSODI","DR1TPOTA","DR1TSELE","DR1TCAFF","DR1TTHEO","DR1TALCO","DR1TMOIS","DR1TS040","DR1TS060","DR1TS080","DR1TS100","DR1TS120","DR1TS140","DR1TS160","DR1TS180","DR1TM161","DR1TM181","DR1TM201","DR1TM221","DR1TP182","DR1TP183","DR1TP184","DR1TP204","DR1TP205","DR1TP225","DR1TP226","DR1.300","DR1.320Z")
diet_subset = subset(diet,select=one_day_diet )
str(diet$DR1TACAR)
sapply(diet_subset, function(x) sum(is.na(x)))
str(diet_subset)

imputed_diet_subset <- mice(diet_subset, m=5, maxit= 50, method = 'pmm', seed=501)
imputed_diet_subset_complete <- mice::complete(imputed_diet_subset, 2)
imputed_diet_subset$method

str(imputed_diet_subset_complete)
sapply(imputed_diet_subset_complete, function(x) sum(is.na(x)))
write.csv(imputed_diet_subset_complete, "diet_subset_processed.csv")


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

################################################################################
#############  diabete and symptoms (questionaire)   ###########################
################################################################################



################################################################################
########################  diabete and demography   #############################
################################################################################


#Define the predictors
#Train the model
#Evaluate the model


