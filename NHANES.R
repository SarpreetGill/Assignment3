#setwd("~/Documents/ML-UYork/ML1000/Asignement2")
#setwd ("c:/Users/sarpr/Desktop/Machine Learning/Assignments/Assignment2")
#setwd("E:\\_Git\\ML1000\\Assignment2")
library(plyr)
library(dplyr)
library(tidyr)
#library(tidyverse)
library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(mice)
library(scales)
library(randomForest)
library(psych)
library(factoextra)
library(AMR)


# Reading files
#=======
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

############################################## Merging & Combining files###################

data_List = list(demographic,examination,diet,labs,questionnaire,medications)
Data_joined = join_all(data_List) 

#dir.create("Data/Raw_Joined")
#write.csv(Data_joined,file = "Data/Raw_Joined/Data_joined.csv")


########################################## Stats on each of the datasets ######################

################## demographic_MS : MS stand for missing data ###############################

str(demographic)
demographic_MS <- demographic %>% summarise_all(~(sum(is.na(.))/n()))
demographic_MS <- gather(demographic_MS, key = "variables", value = "percent_missing")
demographic_MS <- demographic_MS[demographic_MS$percent_missing > 0.0, ] 
demographic_MS_plot<- ggplot(demographic_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Demographic Missing Data By Columns")

ggsave(plot = demographic_MS_plot,width = 8, height =4, dpi = 300, 
       filename = "Figures/demographic_MS_plot.png")

demographic_MS_plot

#<<<<<<< HEAD
#library(VIM) 
#aggr(demographic,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)

##################   diet_MS : MS stand for missing data      ####################
#=======
##################   diet_MS : MS stand for missing data###################################


#>>>>>>> ffc62e6b80ea63248a4d851391542e470f9e7ed9
diet_MS <- diet %>% summarise_all(~(sum(is.na(.))/n()))
diet_MS <- gather(diet_MS, key = "variables", value = "percent_missing")
diet_MS <- diet_MS[diet_MS$percent_missing > 0.0, ] 
diet_MS_plot <- ggplot(diet_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Diet Missing Data By Columns")

ggsave(plot = diet_MS_plot, width = 8, height = 4, dpi = 300, 
       filename = "Figures/diet_MS_plot.png")

diet_MS_plot

################## examination_MS : MS stand for missing data#############################

examination_MS <- examination %>% summarise_all(~(sum(is.na(.))/n()))
examination_MS <- gather(examination_MS, key = "variables", value = "percent_missing")
examination_MS <- examination_MS[examination_MS$percent_missing > 0.0, ] 
examination_MS_plot <- ggplot(examination_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Examination Missing Data By Columns")

ggsave(plot = examination_MS_plot, width = 8, height = 4, dpi = 300, 
       filename = "Figures/examination_MS_plot.png")

examination_MS_plot


################## medications_MS : MS stand for missing data ##########################

medications_MS <- medications %>% summarise_all(~(sum(is.na(.))/n()))
medications_MS <- gather(medications_MS, key = "variables", value = "percent_missing")
medications_MS <- medications_MS[medications_MS$percent_missing > 0.0, ] 
medications_MS_plot <- ggplot(medications_MS, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.3, alpha = 0.8)+
  xlab('variables')+
  coord_flip()+ 
  #theme_fivethirtyeight() +
  ggtitle("Medications Missing Data By Columns")

ggsave(plot = medications_MS_plot, width = 8, height = 3,  dpi = 300, 
       filename = "Figures/medications_MS_plot.png")

medications_MS_plot


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

#sapply(labs, function(x) sum(is.na(x)))



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
#<<<<<<< HEAD
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
########################           demographics    #############################
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

#write.csv(demo_subset_8_imputed,file = "Data/Working/demo_subset_8_imputed.csv")


# histogramme

multi.hist(demo_subset_8_imputed[,sapply(demo_subset_8_imputed, is.numeric)])

#Most of the variables  have right skewed distributions.

demo_subset_8_labeled = mutate(demo_subset_8_imputed, Gender= ifelse(
  demo_subset_8_imputed$Gender == "1" , "Male", "Female" ))


require(dplyr)# because Race is a factor of level 6
demo_subset_8_labeled <- demo_subset_8_labeled %>%
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
 

demo_subset_8_labeled <- demo_subset_8_labeled %>%
  mutate(Country_of_birth  = recode(Country_of_birth , "1" = "US",
                       "2" = "Others",
                       "77" = "Refused",
                       "99" = "Uknown"))

demo_subset_8_labeled <- demo_subset_8_labeled %>%
  mutate(Citizenship_status = recode(Citizenship_status, "1" = "US",
                       "2" = "Other",
                       "7" = "Refused",
                       "9" = "Unknown"))

demo_subset_8_labeled <- demo_subset_8_labeled %>%
  mutate(Marital_status = recode(Marital_status, "1" = "Married",
                       "2" = "Widowed",
                       "3" = "Divorced",
                       "4" = "Separated",
                       "5" = "Never_married",
                       "6" = "partner",
                       "77" = "Refused",
                       "99" = "Unknown"))

demo_subset_8_labeled <- demo_subset_8_labeled %>%
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

demo_subset_8_labeled$Family_income <- as.factor(demo_subset_8_labeled$Family_income)
demo_subset_8_imputed$Family_income <- as.factor(demo_subset_8_imputed$Family_income)


write.csv(demo_subset_8_labeled,file = "Data/Working/demo_subset_8_labeled.csv")

##########################  Gender #############
Gender  <- demo_subset_8_labeled %>%
  group_by(Gender) %>%
  summarize(count=n()) %>%
  arrange(desc(count))%>%
  mutate(pct = count / sum(count),
         pctlabel = paste0(round(pct*100), "%"),
         lab.ypos = 100*cumsum(pct) - 0.5  *100*pct) 


head(Gender)


require(scales)
ggplot(Gender, aes(x = reorder(Gender, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black") +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Gender", y = "percantage", title  = "Bar Chart of Gender")  

#brewer.pal( 2, name = "Dark2")

Gender_plot <- ggplot(Gender, aes(x = "", y =  round(100*pct, 1), fill = reorder(Gender,count))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = pctlabel), color = "black")+
  coord_polar("y", start = 0) +
  ggtitle("Pie plot of Gender")+
  scale_fill_grey(start = 0.8, end = 0.2,"Gender") + theme_void()

Gender_plot

#ggsave(plot = Gender_plot, width = 3, height = 3, dpi = 300, 
  #     filename = "Figures/Gender_plot.png")

ggsave(plot = Gender_plot,dpi = 300, 
     filename = "Figures/Gender_plot.png")
##########################  Country_of_birth #############
Country_of_birth  <- demo_subset_8_labeled %>%
  group_by(Country_of_birth) %>%
  summarize(count=n()) %>%
  arrange(desc(count))%>%
  mutate(pct = count / sum(count),
         pctlabel = paste0(round(pct*100), "%"),
         lab.ypos = 100*cumsum(pct) - 0.5  *100*pct) 


#Bar plot

require(scales)
Birth_plot <- ggplot(Country_of_birth, aes(x = reorder(Country_of_birth, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black") +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Country of birth", y = "percantage", title  = "Bar Chart of Country of birth")  


Birth_plot

#Pie plot
#Birth_plot <-ggplot(Country_of_birth, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Country_of_birth,count))) +
#  geom_bar(width = 1, stat = "identity", color = "white") +
#  coord_polar("y", start = 0)+
#  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
#  ggtitle("Pie plot of Country of birth")+
#  scale_fill_grey(start = 0.8, end = 0.2,"Country_of_birth") + theme_void()

ggsave(plot = Birth_plot, dpi = 300, 
       filename = "Figures/Birth_plot.png")


########################  Marital_status  #######################

Marital_status  <- demo_subset_8_labeled %>%
  group_by(Marital_status) %>%
  summarize(count=n()) %>%
  arrange(desc(count))%>%
  mutate(pct = count / sum(count),
         pctlabel = paste0(round(pct*100), "%"),
         lab.ypos = 100*cumsum(pct) - 0.5  *100*pct) 

#Bar plot

require(scales)
Marital_plot <- ggplot(Marital_status, aes(x = reorder(Marital_status, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black") +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Marital status", y = "percantage", title  = "Bar Chart of Marital status in US ")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Pie plot
#Marital_plot <- ggplot(Marital_status, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Marital_status ,count))) +
#  geom_bar(width = 1, stat = "identity", color = "white") +
#  coord_polar("y", start = 0)+
#  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
#  ggtitle("Pie plot of Marital status")+
#  scale_fill_grey(start = 0.8, end = 0.2,"Marital_status") + theme_void()



ggsave(plot = Marital_plot, dpi = 300, 
       filename = "Figures/Marital_plot.png")


Marital_plot

########################  Race  #######################

Race  <- demo_subset_8_labeled %>%
  group_by(Race) %>%
  summarize(count=n()) %>%
  arrange(desc(count))%>%
  mutate(pct = count / sum(count),
         pctlabel = paste0(round(pct*100), "%"),
         lab.ypos = 100*cumsum(pct) - 0.5  *100*pct) 




#Bar plot

require(scales)
Race_plot <- ggplot(Race, aes(x = reorder(Race, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black") +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Race", y = "percantage", title  = "Bar Chart of Race in US ")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Race_plot

#Pie plot
#Race_plot <- ggplot(Race, aes(x = "", y =  round(100*count/sum(count), 1), fill =  reorder(Race ,count))) +
#  geom_bar(width = 1, stat = "identity", color = "white") +
#  coord_polar("y", start = 0)+
#  geom_text(aes(y = cumsum(100*count/sum(count)) - 0.5*(100*count/sum(count)), label = paste(round(count/sum(count)*100),"%")), color = "black")+
#  ggtitle("Pie plot of Race")+
#  scale_fill_grey(start = 0.8, end = 0.2,"Races") + theme_void()

ggsave(plot = Race_plot, dpi = 300, 
       filename = "Figures/Race_plot.png")




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

# Import libraries
require(AMR)
require(mice)
require(randomForest)

# Import the raw dataset
examination = read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))

# Select relevant columns
examination_subset = subset(
  examination,
  select=c('SEQN', 'PEASCTM1', 'BPAARM', 'BPXSY2', 'BPXDI2', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXLEG',
           'BMXWAIST', 'MGD130', 'MGDCGSZ')
  )

# Rename columns to meaningful names
examination_renamed = rename(
  examination_subset,
  "ID"                  = "SEQN",
  "BP_test_time_exam"        = "PEASCTM1",
  "BP_arm_exam"              = "BPAARM",
  "BP_Systolic_exam"         = "BPXSY2",
  "BP_Diastolic_exam"        = "BPXDI2",
  "Weight_exam"              = "BMXWT",
  "Height_exam"              = "BMXHT",
  "Leg_length_exam"          = "BMXBMI",
  "Arm_length_exam"          = "BMXLEG",
  "Waist_circumference_exam" = "BMXWAIST",
  "Dominant_hand_exam"       = "MGD130",
  "Grip_strength_exam"       = "MGDCGSZ"
  )

# Check if there are no missing values:
0 == sum(sapply(examination_renamed, function(x) sum(is.na(x)))) # FALSE

# Explore the columns in the subset
summary(examination_renamed[2:12])
# Numerical: 9 - pmm or rf
# 2 Factors: 1 - logreg or cart
# 3 Factors: 1 - polyreg or cart

freq(examination_renamed$BP_arm_exam)
# BP_arm_exam has 2 factors. 1=Left, 2=Right, 8=Right (maybe 8 was a typo)
examination_renamed$BP_arm_exam[examination_renamed$BP_arm_exam == 8] = 2
examination_renamed$BP_arm_exam = as.factor(examination_renamed$BP_arm_exam)

freq(examination_renamed$Dominant_hand_exam)
# Dominant_hand_exam has 3 factors: 1=Right, 2=Left, 3=Neither
examination_renamed$Dominant_hand_exam = as.factor(examination_renamed$Dominant_hand_exam)

set.seed(125)
# Configure and run mice
examination_mice = mice(examination_renamed, m=5) # slow line
examination_mice$predictorMatrix[, 'ID']=0
examination_mice$method[c(2:12)] = 'rf'
examination_mice$method['BP_arm_exam'] = 'logreg'
examination_mice$method['Dominant_hand_exam'] = 'polyreg'

examination_imputed = complete(examination_mice)

# Check there are no missing values
0 == sum(sapply(examination_imputed, function(x) sum(is.na(x)))) # TRUE
summary(examination_imputed)

# Export the dataset
##write.csv(examination_imputed, file.choose())

# Label the data
examination_labeled = mutate(
  examination_imputed,
  BP_arm_exam = recode(BP_arm_exam,
                       "1" = "Left",
                       "2" = "Right"),
  Dominant_hand_exam = recode(Dominant_hand_exam,
                              "1"="Right",
                              "2"="Left",
                              "3"="Neither")
  )

# Export the dataset
##write.csv(examination_labeled, file.choose())



############################################ Labs###############################################

nrow(examination)
ncol(examination)
summary(examination)
str(examination)

# Add raw data file for labs
labs <- read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA"))
# many of columns have 60% or higher missing rows.  
sapply(labs, function(x) sum(is.na(x)))
str(labs)

str(labs_subset)
sapply(labs_subset, function(x) sum(is.na(x)))
imputed_labs_subset <- mice(diet_subset, m=5, maxit= 50, method = 'pmm', seed=501)
sapply(labs_subset, function(x) sum(is.na(x)))


select_columns_labs <- c("SEQN", "LBXWBCSI", "LBXRBCSI", "PHQ020", "PHQ030", "PHQ060", "LBXHA", "LBXHBC", "LBXTC" )
labs_subset = subset(labs, select=select_columns_labs)

# Relabel the columns to meaningful names.  

labs_subset <- labs_subset %>% 
  rename("ID"                =         "SEQN",  
         "White_blood_cells_labs" =     "LBXWBCSI",  
         "Red_bloods_cells_labs"  =     "LBXRBCSI",  
         "Caffeine_labs"          =     "PHQ020",  
         "Alcohol_labs"           =     "PHQ030",  
         "Supplements_labs"       =     "PHQ060",   
         "Hepatitis_a_labs"       =     "LBXHA",  
         "Hepatitis_b_labs"       =     "LBXHBC",  
         "Cholesterol_labs"       =     "LBXTC"  )

#LBXWBCSI  and LBXRBCSI are both numerical values  
#PHQ020 is a factor. 1=Yes, 2=No , NA= Not provided
#PHQ030 is a factor. 1=Yes, 2=No , NA= Not provided
#PHQ060 is a factor. 1=Yes, 2=No, NA= Not provided
#LBXHA is a factor. 1=Positive, 2=Negative, 3=Indeterminate, NA=Not tested
#LBXHBC is a factor. 1=Positive, 2=Negative NA=Not tested
#LBXTC is numerical.

sapply(labs_subset, function(x) sum(is.na(x)))
str(labs_subset)

labs_subset$Caffeine_labs <- as.factor(labs_subset$Caffeine_labs)
labs_subset$Alcohol_labs <- as.factor(labs_subset$Alcohol_labs)
labs_subset$Supplements_labs <- as.factor(labs_subset$Supplements_labs)
labs_subset$Hepatitis_a_labs <- as.factor(labs_subset$Hepatitis_a_labs)
labs_subset$Hepatitis_b_labs <- as.factor(labs_subset$Hepatitis_b_labs)
str(labs_subset)

summary(labs_subset)
require(mice)
install.packages("randomForest")

init = mice(labs_subset, maxit=0)
meth = init$method
predM = init$predictorMatrix
# select the "ID" variable to not be included as predictor during imputation.
predM[, c("ID")]=0

# Cart usage: https://stefvanbuuren.name/fimd/sec-cart.html
# https://en.wikipedia.org/wiki/Random_forest 

#Skip a variable from imputation, this variable will still be used for prediction
meth[c("ID")]=""

#Now let specify the methods for imputing the missing values.

meth[c("White_blood_cells_labs")]="rf"
meth[c("Red_bloods_cells_labs" )]="rf"
meth[c("Caffeine_labs")]="cart"
meth[c("Alcohol_labs")]="cart"
meth[c("Supplements_labs")]="cart"
meth[c("Hepatitis_a_labs"  )] ="cart"
meth[c("Hepatitis_b_labs" )] ="cart"
meth[c("Cholesterol_labs")]="rf"

set.seed(145)
imputed = mice(labs_subset, method=meth, predictorMatrix=predM, m=5)
#Create a dataset after imputation.
labs_subset_imputed<- complete(imputed)
sapply(labs_subset_imputed, function(x) sum(is.na(x)))

write.csv(labs_subset_imputed,file = "labs_subset_imputed.csv")

# Label the data for factor/categorical values from imputed dataset.
labs_subset_labelled <- labs_subset_imputed

labs_subset_labelled = labs_subset_labelled %>% 
  mutate(Caffeine_labs= recode(Caffeine_labs, "1" = "Yes",
                              "2" = "No", 
                              "NA" = "Not Tested"))


labs_subset_labelled = labs_subset_labelled %>% 
  mutate(Alcohol_labs= recode(Alcohol_labs, "1" = "Yes",
                               "2" = "No", 
                               "NA" = "Not Tested"))

labs_subset_labelled = labs_subset_labelled %>% 
  mutate(Supplements_labs= recode(Supplements_labs, "1" = "Yes",
                               "2" = "No", 
                               "NA" = "Not Tested"))



labs_subset_labelled = labs_subset_labelled %>% 
  mutate(Hepatitis_a_labs= recode(Hepatitis_a_labs, "1" = "Positive",
                                  "2" = "Negative",
                                  "3" = "Indeterminate",
                                  "NA" = "Not Tested"))


labs_subset_labelled = labs_subset_labelled %>% 
  mutate(Hepatitis_b_labs= recode(Hepatitis_b_labs, "1" = "Positive",
                                  "2" = "Negative",
                                  "NA" = "Not Tested"))



str(labs_subset_labelled)
write.csv(labs_subset_labelled,file = "labs_subset_labelled.csv")

############################################### Medications######################################

nrow(medications)
ncol(medications)
summary(medications)
str(medications)


##################################### ZV/NZV feature remove #########################


medsdata_major <- medications

if (length(nearZeroVar(medsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  medsdata_major <- medsdata_major[, -nearZeroVar(medsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                  names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

#colSums(is.na(medsdata_major))
#colMeans(is.na(medsdata_major))*100
medsdata_major %>% summarise_all(~(sum(is.na(.))/n()*100))

#######################################  Removing data having greater than 32 % missing values



Null_Num_medsdata <- apply(medsdata_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_medsdata <- colnames(medsdata_major)[Null_Num_medsdata > 0.33]
medsdata68 <- select(medsdata_major, -Null_Colms_medsdata)

colSums(is.na(medsdata68))
medsdata68 %>% summarise_all(~(sum(is.na(.))/n()*100))





medsdata_indexed <- medsdata68
colnames(medsdata_indexed) <- with(Dictionary,
                                   Dictionary$Variable.Description[match(colnames(medsdata68),
                                                                         Dictionary$Variable.Name,
                                                                         nomatch = Dictionary$Variable.Name
                                   )])

medsdata_Col_Labels <- data.frame("Code"=c(colnames(medsdata68)), 
                                  "Desp"=c(colnames(medsdata_indexed)))
#dir.create("Data/Labels")
write.csv(medsdata_Col_Labels,file = "Data/Labels/medsdata_Col_Labels.csv")




## 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"



#  Categorization of variables

Cat_meds <- c(0,2,2,2,2,1,2,2,1)
Cat_meds
medsdata_Col_Labels <- data.frame(medsdata_Col_Labels,Cat = Cat_meds)

write.csv(medsdata_Col_Labels,file = "Data/Labels/medsdata_Col_Labels.csv")



#######################################  Reading Index again


medsdata_Col_Labels   = read.csv("Data/Labels/medsdata_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

medsdata_Col_Labels[, 2] <- sapply(medsdata_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_medsdata <- medsdata_Col_Labels[medsdata_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_medsdata <- medsdata_Col_Labels[medsdata_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_medsdata <- medsdata_Col_Labels[medsdata_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_medsdata <- c(Catcolmn_Nul_medsdata, Numcolmn_medsdata, Catcolmn_medsdata)


#medsdata_selected = subset(medsdata68,select= WorkingColm )

medsdata_selected = medsdata68[ WorkingColm_medsdata ]

medsdata_selected[, Catcolmn_medsdata] <- sapply(medsdata_selected[, Catcolmn_medsdata], as.numeric)
medsdata_selected[, Catcolmn_Nul_medsdata] <- sapply(medsdata_selected[, Catcolmn_Nul_medsdata], as.factor)
medsdata_selected[, Numcolmn_medsdata] <- sapply(medsdata_selected[, Numcolmn_medsdata], as.numeric)

#Look the dataset structure.

sapply(medsdata_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================

meth_medsdata=
  init_medsdata=
  predM=
  init_medsdata = mice(medsdata_selected, maxit=0)
meth_medsdata = init_medsdata$method
predM_medsdata = init_medsdata$predictorMatrix

##remove the variable as a predictor but still will be imputed

predM_medsdata[, c("SEQN")]=0

meth_medsdata[Catcolmn_Nul_medsdata] = ""

meth_medsdata[Catcolmn_medsdata]="rf"

meth_medsdata[Numcolmn_medsdata]="pmm"

set.seed(256)
imputed_medsdata = mice(medsdata_selected, method=meth_medsdata, predictorMatrix=predM_medsdata, m=5)

#Create a dataset after imputation.

medsdata_imputed<- complete(imputed_medsdata)



#Check for missings in the imputed dataset.
sapply(medsdata_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute


write.csv(medsdata_imputed , "Data/Working/medsdata_imputed.csv")
#medsdata_imputed   = read.csv("Data/Working/medsdata_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))

###Selecting Variables:
Meds_sel_Feat <- c("SEQN", "RXDDRGID", "RXDDAYS", "RXDRSD1","RXDCOUNT")
medsdata_imputed_subset = subset(medsdata_imputed,select=Meds_sel_Feat )

#### Labeling the dataset 

meds_subset_labelled <- medsdata_imputed_subset
colnames(meds_subset_labelled) <- with(Dictionary,
                                   Dictionary$Variable.Description[match(colnames(medsdata_imputed_subset),
                                                                         Dictionary$Variable.Name,
                                                                         nomatch = Dictionary$Variable.Name
                                   )])

str(meds_subset_labelled)
write.csv(meds_subset_labelled,file = "Data/Working/meds_subset_labelled.csv")





#################################### Questionnaire#############################################


nrow(questionnaire)
ncol(questionnaire)
summary(questionnaire)
str(questionnaire)



##################################### ZV/NZV feature remove #########################


ques_data_major <- questionnaire


if (length(nearZeroVar(ques_data_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  ques_data_major <- ques_data_major[, -nearZeroVar(ques_data_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                    names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

#colSums(is.na(ques_data_major))
#colMeans(is.na(ques_data_major))*100
ques_data_major %>% summarise_all(~(sum(is.na(.))/n()*100))

#######################################  Removing data having greater than 25% missing values



Null_Num_ques_data <- apply(ques_data_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_ques_data <- colnames(ques_data_major)[Null_Num_ques_data > 0.25]
ques_data75 <- select(ques_data_major, -Null_Colms_ques_data)

colSums(is.na(ques_data75))
ques_data75 %>% summarise_all(~(sum(is.na(.))/n()*100))

 
#######################################  Removing Outliers

# Select interesting Questions

ques_Yes_No <- "SEQN" %>% c("HSQ500","HSQ510","HSQ520","DIQ010","DIQ050","DLQ010","DLQ020","DLQ040","FSD151","FSQ162","HIQ011","HIQ210","HUQ090","MCQ010","MCQ053","MCQ300B","SMQ870")
ques_Yes_No_with_SEQN <- c("HSQ500","HSQ510","HSQ520","DIQ010","DIQ050","DLQ010","DLQ020","DLQ040","FSD151","FSQ162","HIQ011","HIQ210","HUQ090","MCQ010","MCQ053","MCQ300B","SMQ870")
ques_data_with_outliers <- select(ques_data75, ques_Yes_No)
ques_data_without_outliers <- select(ques_data75, -ques_Yes_No_with_SEQN)

# Remove outliers from a column
remove_outliers_col <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
# Removes all outliers from a data set
remove_all_outliers_df <- function(df){
  # We only want the numeric columns
  df[,sapply(df, is.numeric)] <- lapply(df[,sapply(df, is.numeric)], remove_outliers_col)
  df
}

ques_data_pro_outliers <- remove_all_outliers_df(ques_data_with_outliers)

ques_data_na_process<-merge(x=ques_data_without_outliers,y=ques_data_pro_outliers,by="SEQN")


#######################################  Creating Index for firther use



ques_data_indexed <- ques_data_na_process
colnames(ques_data_indexed) <- with(Dictionary,
                                    Dictionary$Variable.Description[match(colnames(ques_data75),
                                                                          Dictionary$Variable.Name,
                                                                          nomatch = Dictionary$Variable.Name
                                    )])

ques_data_Col_Labels <- data.frame("Code"=c(colnames(ques_data75)), 
                                   "Desp"=c(colnames(ques_data_indexed)))
#dir.create("Data/Labels")
write.csv(ques_data_Col_Labels,file = "Data/Labels/ques_data_Col_Labels.csv")

#######################################  Categorization of variables


#  Categorization of variables

Cat_ques <- c(0,1,1,1,1,1,2,2,2,0,2,2,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,2,2,2,2,1,2,2,1,1,1,1,2,2,1,2,2,2,2,2,2)

ques_data_Col_Labels <- data.frame(ques_data_Col_Labels,Cat = Cat_ques)

write.csv(ques_data_Col_Labels,file = "Data/Labels/ques_data_Col_Labels.csv")




###################################################################################
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again##########################


ques_data_Col_Labels   = read.csv("Data/Labels/ques_data_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

ques_data_Col_Labels[, 2] <- sapply(ques_data_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_ques_data <- ques_data_Col_Labels[ques_data_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_ques_data <- ques_data_Col_Labels[ques_data_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_ques_data <- ques_data_Col_Labels[ques_data_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_ques_data <- c(Catcolmn_Nul_ques_data, Numcolmn_ques_data, Catcolmn_ques_data)


#ques_data_selected = subset(ques_data75,select= WorkingColm )

ques_data_selected = ques_data75[ WorkingColm_ques_data ]

ques_data_selected[, Catcolmn_ques_data] <- sapply(ques_data_selected[, Catcolmn_ques_data], as.numeric)
ques_data_selected[, Catcolmn_Nul_ques_data] <- sapply(ques_data_selected[, Catcolmn_Nul_ques_data], as.factor)
ques_data_selected[, Numcolmn_ques_data] <- sapply(ques_data_selected[, Numcolmn_ques_data], as.numeric)

#Look the dataset structure.

sapply(ques_data_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================

meth_ques_data=
  init_ques_data=
  predM=
  init_ques_data = mice(ques_data_selected, maxit=0)
meth_ques_data = init_ques_data$method
predM_ques_data = init_ques_data$predictorMatrix

##remove the variable as a predictor but still will be imputed

predM_ques_data[, c("SEQN")]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""

meth_ques_data[Catcolmn_Nul_ques_data] = ""

#++++++++++++++++++++++++++++++++++
##Now let specify the methods for imputing the missing values.

meth_ques_data[Catcolmn_ques_data]="cart"

## we impute the Numerical Variable

meth_ques_data[Numcolmn_ques_data]="pmm"


set.seed(415)
imputed_ques_data = mice(ques_data_selected, method=meth_ques_data, predictorMatrix=predM_ques_data, m=5)

#Create a dataset after imputation.

ques_data_imputed<- complete(imputed_ques_data)



#Check for missings in the imputed dataset.
sapply(ques_data_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute


write.csv(ques_data_imputed , "Data/Working/ques_data_imputed.csv")
ques_data_imputed   = read.csv("Data/Working/ques_data_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))




###Selecting Variables:

ques_sel_Feat <- c("SEQN","CBD070","CBD110","CBD120","CBD130","HSQ500","HSQ510","HSQ520","DIQ010","DIQ050","DBQ197","DBD895","DBD905","DBD910","DLQ010","DLQ020","DLQ040","FSD151","FSQ162","HIQ011","HIQ210","HOD050","HUQ010","HUQ041","HUQ051","HUQ090","IND235","MCQ010","MCQ053","MCQ300B","OHQ030","PAQ710","PAQ715","SMD460","SMQ870")

ques_data_imputed_subset = subset(ques_data_imputed,select=ques_sel_Feat )


#### Labeling the dataset 

ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(ques_Yes_No_with_SEQN = recode(ques_Yes_No_with_SEQN, "1" = "Yes",
                                        "2" = "No"))

 


demo_subset_8_labeled <- demo_subset_8_labeled %>%
  mutate(Country_of_birth  = recode(Country_of_birth , "1" = "US",
                                    "2" = "Others",
                                    "77" = "Refused",
                                    "99" = "Uknown"))

demo_subset_8_labeled <- demo_subset_8_labeled %>%
  mutate(Citizenship_status = recode(Citizenship_status, "1" = "US",
                                     "2" = "Other",
                                     "7" = "Refused",
                                     "9" = "Unknown"))

demo_subset_8_labeled <- demo_subset_8_labeled %>%
  mutate(Marital_status = recode(Marital_status, "1" = "Married",
                                 "2" = "Widowed",
                                 "3" = "Divorced",
                                 "4" = "Separated",
                                 "5" = "Never_married",
                                 "6" = "partner",
                                 "77" = "Refused",
                                 "99" = "Unknown"))

demo_subset_8_labeled <- demo_subset_8_labeled %>%
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




ques_subset_labelled <- ques_data_imputed_subset
colnames(ques_subset_labelled) <- with(Dictionary,
                                    Dictionary$Variable.Description[match(colnames(ques_data_imputed_subset),
                                                                          Dictionary$Variable.Name,
                                                                          nomatch = Dictionary$Variable.Name
                                    )])


write.csv(ques_subset_labelled,file = "Data/Working/ques_subset_labelled.csv")



################################################################################
#############  diabete and symptoms (questionaire)   ###########################
################################################################################



################################################################################
########################  diabete and demography   #############################
################################################################################


#Define the predictors



#Train the model




#Evaluate the model



