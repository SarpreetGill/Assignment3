#setwd("~/Documents/ML-UYork/ML1000/Asignement2")
#setwd ("c:/Users/sarpr/Desktop/Machine Learning/Assignments/Assignment2")
library(plyr)
library(dplyr)
library(tidyr)
#library(tidyverse)
library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(mice)
library(scales)
#<<<<<<< HEAD
library(randomForest)
library(psych)
library(factoextra)


# Reading files
#=======
library(RColorBrewer)
library(caret)

#>>>>>>> ffc62e6b80ea63248a4d851391542e470f9e7ed9

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



#aggr(demographic,only.miss=TRUE,numbers=TRUE,sortVar=TRUE)

##################   diet_MS : MS stand for missing data      ####################
#=======
##################   diet_MS : MS stand for missing data###################################



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
########################           Demographic Impute #1    ####################
######################  Choosing interesting variables manually   ##############

################################################################################


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
########################           Demographic Impute #2    ####################
######################  Keeping all available data for data engineering  #######

################################################################################


################################################################################
########################           demographics    #############################
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


#colSums(is.na(demographic_major))
#colMeans(is.na(demographic_major))*100
demographic_major %>% summarise_all(~(sum(is.na(.))/n()*100))
Null_Num <- apply(demographic_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms <- colnames(demographic_major)[Null_Num > 0.25]
demographic75 <- select(demographic_major, -Null_Colms)

colSums(is.na(demographic75))


demographic_indexed <- demographic75
colnames(demographic_indexed) <- with(Dictionary,
                                      Dictionary$Variable.Description[match(colnames(demographic75),
                                                                            Dictionary$Variable.Name,
                                                                            nomatch = Dictionary$Variable.Name
                                      )])

Demogramphic_Col_Labels <- data.frame("Code"=c(colnames(demographic75)), 
                                      "Desp"=c(colnames(demographic_indexed)))
#dir.create("Data/Labels")
#write.csv(Demogramphic_Col_Labels,file = "Data/Labels/Demogramphic_Col_Labels.csv")
Demogramphic_Col_Labels   = read.csv("Data/Labels/Demogramphic_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

Demogramphic_Col_Labels[, 2] <- sapply(Demogramphic_Col_Labels[, 2], as.character)


Catcolmn <- Demogramphic_Col_Labels[Demogramphic_Col_Labels$Cat ==2 , 2 ] 
Numcolmn <- Demogramphic_Col_Labels[Demogramphic_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul <- Demogramphic_Col_Labels[Demogramphic_Col_Labels$Cat ==0 , 2 ] 
WorkingColm <- c(Catcolmn_Nul, Numcolmn, Catcolmn)

##demographic_selected <- demographic ##Test

#demographic_selected = subset(demographic75,select= WorkingColm )

demographic_selected = demographic75[ WorkingColm ]

demographic_selected[, Catcolmn] <- sapply(demographic_selected[, Catcolmn], as.numeric)
demographic_selected[, Catcolmn_Nul] <- sapply(demographic_selected[, Catcolmn_Nul], as.factor)
demographic_selected[, Numcolmn] <- sapply(demographic_selected[, Numcolmn], as.numeric)

str(demographic_selected[, Catcolmn])

#Look the dataset structure.

sapply(demographic_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================

meth=
  init=
  predM=
  init = mice(demographic_selected, maxit=0)
meth = init$method
predM = init$predictorMatrix

##remove the variable as a predictor but still will be imputed. Just for illustration purposes,

predM[, c("SEQN")]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""

meth[Catcolmn_Nul] = ""

#++++++++++++++++++++++++++++++++++
##Now let specify the methods for imputing the missing values.

meth[Catcolmn]="cart"

## we impute the Numerical Variable

meth[Numcolmn]="rf"


set.seed(103)
imputed = mice(demographic_selected, method=meth, predictorMatrix=predM, m=5)

#Create a dataset after imputation.

demographic_imputed<- complete(imputed)


#Check for missings in the imputed dataset.
sapply(demographic_imputed, function(x) sum(is.na(x)))


#write.csv(demographic_imputed , "Data/Working/demographic_imputed.csv")
demographic_imputed   = read.csv("Data/Working/demographic_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))

demo_subset_8_imputed<- demographic_imputed



demo_subset_8_imputed<- demo_subset_8_imputed %>% 
  rename("ID"                 =         "SEQN",  
         "Gender"             =     "RIAGENDR",  
         "Age"                =     "RIDAGEYR",  
         "Race"               =     "RIDRETH3",  
         "Country_of_birth"   =     "DMDBORN4",  
         "Citizenship_status" =     "DMDCITZN",   
         "Family_members"     =     "DMDFMSIZ",  
         "Marital_status"     =     "DMDHRMAR",  
         "Family_income"      =     "INDFMIN2"  )


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


############################################## Diet####################################

nrow(diet)
ncol(diet)
summary(diet)
str(diet)

##################################### ZV/NZV feature remove #########################


diet_major <- diet

if (length(nearZeroVar(diet_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  diet_major <- diet_major[, -nearZeroVar(diet_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                          names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

#colSums(is.na(diet_major))
#colMeans(is.na(diet_major))*100
diet_major %>% summarise_all(~(sum(is.na(.))/n()*100))

#######################################  Removing data having greater than 25% missing values



Null_Num_diet <- apply(diet_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_diet <- colnames(diet_major)[Null_Num_diet > 0.25]
diet75 <- select(diet_major, -Null_Colms_diet)

colSums(is.na(diet75))
diet75 %>% summarise_all(~(sum(is.na(.))/n()*100))


#######################################  Creating Index for firther use



diet_indexed <- diet75
colnames(diet_indexed) <- with(Dictionary,
                               Dictionary$Variable.Description[match(colnames(diet75),
                                                                     Dictionary$Variable.Name,
                                                                     nomatch = Dictionary$Variable.Name
                               )])

diet_Col_Labels <- data.frame("Code"=c(colnames(diet75)), 
                              "Desp"=c(colnames(diet_indexed)))
#dir.create("Data/Labels")
write.csv(diet_Col_Labels,file = "Data/Labels/diet_Col_Labels.csv")

#######################################  Categorization of variables

############# 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again


diet_Col_Labels   = read.csv("Data/Labels/diet_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

diet_Col_Labels[, 2] <- sapply(diet_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_diet <- diet_Col_Labels[diet_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_diet <- diet_Col_Labels[diet_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_diet <- diet_Col_Labels[diet_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_diet <- c(Catcolmn_Nul_diet, Numcolmn_diet, Catcolmn_diet)


#diet_selected = subset(diet75,select= WorkingColm )

diet_selected = diet75[ WorkingColm_diet ]

diet_selected[, Catcolmn_diet] <- sapply(diet_selected[, Catcolmn_diet], as.numeric)
diet_selected[, Catcolmn_Nul_diet] <- sapply(diet_selected[, Catcolmn_Nul_diet], as.factor)
diet_selected[, Numcolmn_diet] <- sapply(diet_selected[, Numcolmn_diet], as.numeric)

#Look the dataset structure.

sapply(diet_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================

meth_diet=
  init_diet=
  predM=
  init_diet = mice(diet_selected, maxit=0)
meth_diet = init_diet$method
predM_diet = init_diet$predictorMatrix

##remove the variable as a predictor but still will be imputed

predM_diet[, c("SEQN")]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""

meth_diet[Catcolmn_Nul_diet] = ""

#++++++++++++++++++++++++++++++++++
##Now let specify the methods for imputing the missing values.

meth_diet[Catcolmn_diet]="cart"

## we impute the Numerical Variable

meth_diet[Numcolmn_diet]="pmm"


set.seed(256)
imputed_diet = mice(diet_selected, method=meth_diet, predictorMatrix=predM_diet, m=5)

#Create a dataset after imputation.

diet_imputed<- complete(imputed_diet)



#Check for missings in the imputed dataset.
sapply(diet_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute


write.csv(diet_imputed , "Data/Working/diet_imputed.csv")


#######################################  Upcoming probability to include column
########################################   names after impute or final model?




########################################### Examination###############################

nrow(examination)
ncol(examination)
summary(examination)
str(examination)



##################################### ZV/NZV feature remove #########################


exam_major <- examination

if (length(nearZeroVar(exam_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  exam_major <- exam_major[, -nearZeroVar(exam_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                          names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

#colSums(is.na(exam_major))
#colMeans(is.na(exam_major))*100
exam_major %>% summarise_all(~(sum(is.na(.))/n()*100))

#######################################  Removing data having greater than 25% missing values



Null_Num_exam <- apply(exam_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_exam <- colnames(exam_major)[Null_Num_exam > 0.25]
exam75 <- select(exam_major, -Null_Colms_exam)

colSums(is.na(exam75))
exam75 %>% summarise_all(~(sum(is.na(.))/n()*100))


#######################################  Creating Index for firther use



exam_indexed <- exam75
colnames(exam_indexed) <- with(Dictionary,
                               Dictionary$Variable.Description[match(colnames(exam75),
                                                                     Dictionary$Variable.Name,
                                                                     nomatch = Dictionary$Variable.Name
                               )])

exam_Col_Labels <- data.frame("Code"=c(colnames(exam75)), 
                              "Desp"=c(colnames(exam_indexed)))
#dir.create("Data/Labels")
write.csv(exam_Col_Labels,file = "Data/Labels/exam_Col_Labels.csv")

#######################################  Categorization of variables

############# 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again


exam_Col_Labels   = read.csv("Data/Labels/exam_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

exam_Col_Labels[, 2] <- sapply(exam_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_exam <- exam_Col_Labels[exam_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_exam <- exam_Col_Labels[exam_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_exam <- exam_Col_Labels[exam_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_exam <- c(Catcolmn_Nul_exam, Numcolmn_exam, Catcolmn_exam)


#exam_selected = subset(exam75,select= WorkingColm )

exam_selected = exam75[ WorkingColm_exam ]

exam_selected[, Catcolmn_exam] <- sapply(exam_selected[, Catcolmn_exam], as.numeric)
exam_selected[, Catcolmn_Nul_exam] <- sapply(exam_selected[, Catcolmn_Nul_exam], as.factor)
exam_selected[, Numcolmn_exam] <- sapply(exam_selected[, Numcolmn_exam], as.numeric)

#Look the dataset structure.

sapply(exam_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================
meth_exam=
  init_exam=
  predM=
  init_exam = mice(exam_selected, maxit=0)
meth_exam = init_exam$method
predM_exam = init_exam$predictorMatrix

##remove the variable as a predictor but still will be imputed

predM_exam[, c("SEQN")]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""

meth_exam[Catcolmn_Nul_exam] = ""

#++++++++++++++++++++++++++++++++++
##Now let specify the methods for imputing the missing values.

meth_exam[Catcolmn_exam]="cart"

## we impute the Numerical Variable

meth_exam[Numcolmn_exam]="pmm"


set.seed(256)
imputed_exam = mice(exam_selected, method=meth_exam, predictorMatrix=predM_exam, m=5)

#Create a dataset after imputation.

exam_imputed<- complete(imputed_exam)



#Check for missings in the imputed dataset.
sapply(exam_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute


write.csv(exam_imputed , "Data/Working/exam_imputed.csv")


#######################################  Upcoming probability to include column
########################################   names after impute or final model?






############################################ Labs###############################################

nrow(labs)
ncol(labs)
summary(labs)
str(labs)

##################################### ZV/NZV feature remove #########################


labsdata_major <- labs

if (length(nearZeroVar(labsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  labsdata_major <- labsdata_major[, -nearZeroVar(labsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                  names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

#colSums(is.na(labsdata_major))
#colMeans(is.na(labsdata_major))*100
labsdata_major %>% summarise_all(~(sum(is.na(.))/n()*100))

#######################################  Removing data having greater than 25% missing values



Null_Num_labsdata <- apply(labsdata_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_labsdata <- colnames(labsdata_major)[Null_Num_labsdata > 0.25]
labsdata75 <- select(labsdata_major, -Null_Colms_labsdata)

colSums(is.na(labsdata75))
labsdata75 %>% summarise_all(~(sum(is.na(.))/n()*100))


#######################################  Creating Index for firther use



labsdata_indexed <- labsdata75
colnames(labsdata_indexed) <- with(Dictionary,
                                   Dictionary$Variable.Description[match(colnames(labsdata75),
                                                                         Dictionary$Variable.Name,
                                                                         nomatch = Dictionary$Variable.Name
                                   )])

labsdata_Col_Labels <- data.frame("Code"=c(colnames(labsdata75)), 
                                  "Desp"=c(colnames(labsdata_indexed)))
#dir.create("Data/Labels")
write.csv(labsdata_Col_Labels,file = "Data/Labels/labsdata_Col_Labels.csv")

#######################################  Categorization of variables

############# 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again


labsdata_Col_Labels   = read.csv("Data/Labels/labsdata_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

labsdata_Col_Labels[, 2] <- sapply(labsdata_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_labsdata <- labsdata_Col_Labels[labsdata_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_labsdata <- labsdata_Col_Labels[labsdata_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_labsdata <- labsdata_Col_Labels[labsdata_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_labsdata <- c(Catcolmn_Nul_labsdata, Numcolmn_labsdata, Catcolmn_labsdata)


#labsdata_selected = subset(labsdata75,select= WorkingColm )

labsdata_selected = labsdata75[ WorkingColm_labsdata ]

labsdata_selected[, Catcolmn_labsdata] <- sapply(labsdata_selected[, Catcolmn_labsdata], as.numeric)
labsdata_selected[, Catcolmn_Nul_labsdata] <- sapply(labsdata_selected[, Catcolmn_Nul_labsdata], as.factor)
labsdata_selected[, Numcolmn_labsdata] <- sapply(labsdata_selected[, Numcolmn_labsdata], as.numeric)

#Look the dataset structure.

sapply(labsdata_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   =======================
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#==============================================================================

meth_labsdata=
  init_labsdata=
  predM=
  init_labsdata = mice(labsdata_selected, maxit=0)
meth_labsdata = init_labsdata$method
predM_labsdata = init_labsdata$predictorMatrix

##remove the variable as a predictor but still will be imputed

predM_labsdata[, c("SEQN")]=0

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""

meth_labsdata[Catcolmn_Nul_labsdata] = ""

#++++++++++++++++++++++++++++++++++
##Now let specify the methods for imputing the missing values.

meth_labsdata[Catcolmn_labsdata]="cart"

## we impute the Numerical Variable

meth_labsdata[Numcolmn_labsdata]="pmm"


set.seed(256)
imputed_labsdata = mice(labsdata_selected, method=meth_labsdata, predictorMatrix=predM_labsdata, m=5)

#Create a dataset after imputation.

labsdata_imputed<- complete(imputed_labsdata)



#Check for missings in the imputed dataset.
sapply(labsdata_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute


write.csv(labsdata_imputed , "Data/Working/labsdata_imputed.csv")


#######################################  Upcoming probability to include column
########################################   names after impute or final model?






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
Null_Colms_medsdata <- colnames(medsdata_major)[Null_Num_medsdata > 0.32]
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

#######################################  Categorization of variables

############# 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

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

##If you want to skip a variable from imputation use the code below.
##This variable will still be used for prediction.
#++++++++++++++++++++++++++++++++++
#meth[c("Variable")]=""

meth_medsdata[Catcolmn_Nul_medsdata] = ""

#++++++++++++++++++++++++++++++++++
##Now let specify the methods for imputing the missing values.

meth_medsdata[Catcolmn_medsdata]="rf"

## we impute the Numerical Variable

meth_medsdata[Numcolmn_medsdata]="pmm"


set.seed(256)
imputed_medsdata = mice(medsdata_selected, method=meth_medsdata, predictorMatrix=predM_medsdata, m=5)

#Create a dataset after imputation.

medsdata_imputed<- complete(imputed_medsdata)



#Check for missings in the imputed dataset.
sapply(medsdata_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute


write.csv(medsdata_imputed , "Data/Working/medsdata_imputed.csv")


#######################################  Upcoming probability to include column
########################################   names after impute or final model?



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


#######################################  Creating Index for firther use



ques_data_indexed <- ques_data75
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

############# 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again


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


#######################################  Upcoming probability to include column
########################################   names after impute or final model?











################################################################################
#############  diabete and symptoms (questionaire)   ###########################
################################################################################



################################################################################
########################  diabete and demography   #############################
################################################################################


#Define the predictors
#Train the model
#Evaluate the model


