lapply(c("plyr",
         "dplyr",
         "tidyr",
         "tidyverse",
         "knitr",    
         "ggplot2",  
         "mice",
         "scales",
         "randomForest",
         "psych",
         "factoextra",
         "AMR",
         "RColorBrewer",
         "caret",
         "AMR",
         "mice",
         "randomForest"
),
require, character.only=TRUE)



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




################## labs_MS : MS stand for missing data ####################

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


######################### Demographics#############################################


# ZV/NZV feature remove

demographic   = read.csv("Data/Raw/demographic.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))

demographic_major <- demographic

if (length(nearZeroVar(demographic_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  demographic_major <- demographic_major[, -nearZeroVar(demographic_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                        names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}

rm(demographic)

# Check the columns for missing values >25%

sapply(demographic_major, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))


Null_Num <- apply(demographic_major, 2, function(x) length(which(is.na(x) | x == "NA"))/length(x))
Null_Colms <- colnames(demographic_major)[Null_Num > 0.25]
demographic75 <- select(demographic_major, -Null_Colms)
rm(demographic_major)


# Create label file

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
rm(demographic_indexed)


#######################################  Categorization of variables


# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"


# Categorization 


Cat_demo <- c(0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1)
Demogramphic_Col_Labels <- data.frame(Demogramphic_Col_Labels,Cat = Cat_demo)

#write.csv(Demogramphic_Col_Labels,file = "Data/Labels/Demogramphic_Col_Labels.csv")



Demogramphic_Col_Labels   = read.csv("Data/Labels/Demogramphic_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

Demogramphic_Col_Labels[, 2] <- sapply(Demogramphic_Col_Labels[, 2], as.character)


Catcolmn <- Demogramphic_Col_Labels[Demogramphic_Col_Labels$Cat ==2 , 2 ] 
Numcolmn <- Demogramphic_Col_Labels[Demogramphic_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul <- Demogramphic_Col_Labels[Demogramphic_Col_Labels$Cat ==0 , 2 ] 
WorkingColm <- c(Catcolmn_Nul, Numcolmn, Catcolmn)
WorkingColm

##demographic_selected <- demographic ##Test

#demographic_selected = subset(demographic75,select= WorkingColm )
demographic_selected = demographic75[ Catcolmn_Nul ]


demographic_selected = demographic75[ WorkingColm ]

demographic_selected[, Catcolmn] <- sapply(demographic_selected[, Catcolmn], as.numeric)
demographic_selected[, Catcolmn_Nul] <- sapply(demographic_selected[, Catcolmn_Nul], as.numeric)
demographic_selected[, Numcolmn] <- sapply(demographic_selected[, Numcolmn], as.numeric)

str(demographic_selected[, Catcolmn])

#Look the dataset structure.

sapply(demographic_selected, function(x) sum(is.na(x)))


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
rm(Demogramphic_Col_Labels,demographic75,demographic_selected, imputed)

#Check for missings in the imputed dataset.
sapply(demographic_imputed, function(x) sum(is.na(x)))

dir.create("Data/Clean_Imputes")
#write.csv(demographic_imputed , "Data/Clean_Imputes/demographic_imputed.csv",row.names = FALSE)
demographic_imputed   = read.csv("Data/Clean_Imputes/demographic_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))


demo_subset_8_imputed <- demographic_imputed
rm(demographic_imputed)
# histogramme

multi.hist(demo_subset_8_imputed[,sapply(demo_subset_8_imputed, is.numeric)])

#Most of the variables  have right skewed distributions.



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

dir.create("Data/Labeled_Imputed")
#write.csv(demo_subset_8_labeled,file = "Data/Labeled_Imputed/demo_subset_8_labeled.csv")

############################  Gender #############
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
############################  Country_of_birth #############
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


############################  Marital_status  #######################

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
############################  Race  #######################

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

diet          = read.csv("Data/Raw/diet.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))

nrow(diet)
ncol(diet)
summary(diet)
str(diet)

##################################### ZV/NZV feature remove 


diet_major <- diet

if (length(nearZeroVar(diet_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  diet_major <- diet_major[, -nearZeroVar(diet_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                          names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}
rm(diet)

#######################################  Check the data for missing values.


sapply(diet_major, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))



#######################################  Removing data having greater than 25% missing values



Null_Num_diet <- apply(diet_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_diet <- colnames(diet_major)[Null_Num_diet > 0.25]
diet75 <- select(diet_major, -Null_Colms_diet)

colSums(is.na(diet75))
diet75 %>% summarise_all(~(sum(is.na(.))/n()*100))


#######################################  Creating Index for firther use
rm(diet_major)


diet_indexed <- diet75
colnames(diet_indexed) <- with(Dictionary,
                               Dictionary$Variable.Description[match(colnames(diet75),
                                                                     Dictionary$Variable.Name,
                                                                     nomatch = Dictionary$Variable.Name
                               )])

diet_Col_Labels <- data.frame("Code"=c(colnames(diet75)), 
                              "Desp"=c(colnames(diet_indexed)))

rm(diet_indexed)
#dir.create("Data/Labels")
#write.csv(diet_Col_Labels,file = "Data/Labels/diet_Col_Labels.csv")

#######################################  Categorization of variables


# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again



# Categorization 


Cat_diet <- c(0,1,1,2,2,2,1,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,1,1,1,1,2,2,2)
diet_Col_Labels <- data.frame(diet_Col_Labels,Cat = Cat_diet)

#write.csv(diet_Col_Labels,file = "Data/Labels/diet_Col_Labels.csv")




diet_Col_Labels   = read.csv("Data/Labels/diet_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

diet_Col_Labels[, 2] <- sapply(diet_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_diet <- diet_Col_Labels[diet_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_diet <- diet_Col_Labels[diet_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_diet <- diet_Col_Labels[diet_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_diet <- c(Catcolmn_Nul_diet, Numcolmn_diet, Catcolmn_diet)


#diet_selected = subset(diet75,select= WorkingColm )

diet_selected = diet75[ WorkingColm_diet ]

rm(diet_Col_Labels,diet75)

diet_selected[, Catcolmn_diet] <- sapply(diet_selected[, Catcolmn_diet], as.numeric)
diet_selected[, Catcolmn_Nul_diet] <- sapply(diet_selected[, Catcolmn_Nul_diet], as.numeric)
diet_selected[, Numcolmn_diet] <- sapply(diet_selected[, Numcolmn_diet], as.numeric)

#Look the dataset structure.

sapply(diet_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)


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

rm(imputed_diet)

#Check for missings in the imputed dataset.
sapply(diet_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute

#write.csv(diet_imputed , "Data/Clean_Imputes/diet_imputed.csv",row.names = FALSE)
diet_imputed   = read.csv("Data/Clean_Imputes/diet_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))


diet_labeled <- diet_imputed
rm(diet_imputed)

diet_labeled <- diet_labeled %>% 
  dplyr::rename("ID"                =         "SEQN",  
                "Carbs_diet"        =     "DR1TCARB",  
                "Sugar_diet"        =     "DR1TSUGR",  
                "Fiber_diet"        =     "DR1TFIBE",  
                "transfat_diet"     =     "DR1TTFAT",  
                "satfat_diet"       =     "DR1TSFAT",   
                "zinc_diet"         =     "DR1TZINC",  
                "copper_diet"       =     "DR1TCOPP",  
                "sodium_diet"       =     "DR1TSODI",  
                "pota_diet"         =     "DR1TPOTA", 
                "selenium_diet"     =     "DR1TSELE"  )



#write.csv(diet_labeled,file = "Data/Labeled_Imputed/diet_labeled.csv")

rm(diet, diet_labeled,diet_selected)


############################################## Examination###############################

examination   = read.csv("Data/Raw/examination.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))


nrow(examination)
ncol(examination)
summary(examination)
str(examination)



##################################### ZV/NZV feature remove 


exam_major <- examination
rm(examination)

if (length(nearZeroVar(exam_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  exam_major <- exam_major[, -nearZeroVar(exam_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                          names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

sapply(exam_major, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))

#######################################  Removing data having greater than 25% missing values



Null_Num_exam <- apply(exam_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_exam <- colnames(exam_major)[Null_Num_exam > 0.25]
exam75 <- select(exam_major, -Null_Colms_exam)

colSums(is.na(exam75))
exam75 %>% summarise_all(~(sum(is.na(.))/n()*100))

rm(exam_major)

#######################################  Creating Index for firther use



exam_indexed <- exam75
colnames(exam_indexed) <- with(Dictionary,
                               Dictionary$Variable.Description[match(colnames(exam75),
                                                                     Dictionary$Variable.Name,
                                                                     nomatch = Dictionary$Variable.Name
                               )])

exam_Col_Labels <- data.frame("Code"=c(colnames(exam75)), 
                              "Desp"=c(colnames(exam_indexed)))

#write.csv(exam_Col_Labels,file = "Data/Labels/exam_Col_Labels.csv")

#######################################  Categorization of variables


# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again

# Categorization 


Cat_exam <- c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
              2,2,2,2,2,2,2,2,2,2)
exam_Col_Labels <- data.frame(exam_Col_Labels,Cat = Cat_exam)

#write.csv(exam_Col_Labels,file = "Data/Labels/exam_Col_Labels.csv")


exam_Col_Labels   = read.csv("Data/Labels/exam_Col_Labels.csv", header = TRUE, na.strings = c("NA","","#NA"))

exam_Col_Labels[, 2] <- sapply(exam_Col_Labels[, 2], as.character)


#######################################  Preparing dataset for Impute


Catcolmn_exam <- exam_Col_Labels[exam_Col_Labels$Cat ==2 , 2 ] 
Numcolmn_exam <- exam_Col_Labels[exam_Col_Labels$Cat ==1 , 2 ] 
Catcolmn_Nul_exam <- exam_Col_Labels[exam_Col_Labels$Cat ==0 , 2 ] 
WorkingColm_exam <- c(Catcolmn_Nul_exam, Numcolmn_exam, Catcolmn_exam)

rm(exam_indexed, exam_Col_Labels)

#exam_selected = subset(exam75,select= WorkingColm )

exam_selected = exam75[ WorkingColm_exam ]

rm(exam75,examination)
exam_selected[, Catcolmn_exam] <- sapply(exam_selected[, Catcolmn_exam], as.factor)
exam_selected[, Catcolmn_Nul_exam] <- sapply(exam_selected[, Catcolmn_Nul_exam], as.numeric)
exam_selected[, Numcolmn_exam] <- sapply(exam_selected[, Numcolmn_exam], as.numeric)

#Look the dataset structure.

sapply(exam_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#
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


set.seed(311)
imputed_exam = mice(exam_selected, method=meth_exam, predictorMatrix=predM_exam, m=5)

#Create a dataset after imputation.

exam_imputed<- complete(imputed_exam)
rm(imputed_exam)
#Check for missings in the imputed dataset.
sapply(exam_imputed, function(x) sum(is.na(x)))

#######################################  Saving Impute


#write.csv(exam_imputed , "Data/Clean_Imputes/exam_imputed.csv",row.names = FALSE)
exam_imputed   = read.csv("Data/Clean_Imputes/exam_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))


exam_labeled <- exam_imputed
rm(exam_imputed)

exam_labeled = dplyr::rename(
  exam_labeled,
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

exam_labeled = mutate(
  exam_labeled,
  BP_arm_exam = recode(BP_arm_exam,
                       "1" = "Left",
                       "2" = "Right"),
  Dominant_hand_exam = recode(Dominant_hand_exam,
                              "1"="Right",
                              "2"="Left",
                              "3"="Neither")
)



write.csv(exam_labeled,file = "Data/Labeled_Imputed/exam_labeled.csv")

############################################## Labs###############################################

labs          = read.csv("Data/Raw/labs.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))

nrow(labs)
ncol(labs)
summary(labs)
str(labs)

##################################### ZV/NZV feature remove 

labsdata_major <- labs

if (length(nearZeroVar(labsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  labsdata_major <- labsdata_major[, -nearZeroVar(labsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                  names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}
rm(labs)

#######################################  Check the data for missing values.


sapply(labsdata_major, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))

#######################################  Removing data having greater than 35% missing values



Null_Num_labsdata <- apply(labsdata_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_labsdata <- colnames(labsdata_major)[Null_Num_labsdata > 0.35]
labsdata75 <- select(labsdata_major, -Null_Colms_labsdata)

summary(labsdata75$LBXGH)
rm(labsdata_major)

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
#write.csv(labsdata_Col_Labels,file = "Data/Labels/labsdata_Col_Labels.csv")

#######################################  Categorization of variables


# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"

#######################################  Reading Index again


# Categorization 


Cat_labs <- c(0,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              2,2,2,2,1,1,2,2,2,2,1,1,1,1,1)
labsdata_Col_Labels <- data.frame(labsdata_Col_Labels,Cat = Cat_labs)

#write.csv(labsdata_Col_Labels,file = "Data/Labels/labsdata_Col_Labels.csv")

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
labsdata_selected[, Catcolmn_Nul_labsdata] <- sapply(labsdata_selected[, Catcolmn_Nul_labsdata], as.numeric)
labsdata_selected[, Numcolmn_labsdata] <- sapply(labsdata_selected[, Numcolmn_labsdata], as.numeric)

rm(labsdata_indexed,labsdata_Col_Labels)
#Look the dataset structure.

sapply(labsdata_selected, function(x) sum(is.na(x)))


#==========================  IMPUTATION( MICE package)   
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)
#

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


set.seed(415)
imputed_labsdata = mice(labsdata_selected, method=meth_labsdata, predictorMatrix=predM_labsdata, m=5)

#Create a dataset after imputation.

labsdata_imputed<- complete(imputed_labsdata)

rm(imputed_labsdata)

#Check for missings in the imputed dataset.
sapply(labsdata_imputed, function(x) sum(is.na(x)))



#######################################  Saving Impute

#write.csv(labsdata_imputed , "Data/Clean_Imputes/labsdata_imputed.csv",row.names = FALSE)
labsdata_imputed   = read.csv("Data/Clean_Imputes/labsdata_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))

labs_labeled <-labsdata_imputed
rm(labsdata_imputed)


labs_labeled <- labs_labeled %>% 
  dplyr::rename("ID"                =         "SEQN",  
                "White_blood_cells_labs" =     "LBXWBCSI",  
                "Red_bloods_cells_labs"  =     "LBXRBCSI",  
                "Caffeine_labs"          =     "PHQ020",  
                "Alcohol_labs"           =     "PHQ030",  
                "Supplements_labs"       =     "PHQ060",   
                "Hepatitis_a_labs"       =     "LBXHA",  
                "Hepatitis_b_labs"       =     "LBXHBC",  
                "Cholesterol_labs"       =     "LBXTC"  )


labs_labeled = labs_labeled %>% 
  mutate(Caffeine_labs= recode(Caffeine_labs, "1" = "Yes",
                               "2" = "No", 
                               "NA" = "Not Tested"))


labs_labeled = labs_labeled %>% 
  mutate(Alcohol_labs= recode(Alcohol_labs, "1" = "Yes",
                              "2" = "No", 
                              "NA" = "Not Tested"))

labs_labeled = labs_labeled %>% 
  mutate(Supplements_labs= recode(Supplements_labs, "1" = "Yes",
                                  "2" = "No", 
                                  "NA" = "Not Tested"))



labs_labeled = labs_labeled %>% 
  mutate(Hepatitis_a_labs= recode(Hepatitis_a_labs, "1" = "Positive",
                                  "2" = "Negative",
                                  "3" = "Indeterminate",
                                  "NA" = "Not Tested"))


labs_labeled = labs_labeled %>% 
  mutate(Hepatitis_b_labs= recode(Hepatitis_b_labs, "1" = "Positive",
                                  "2" = "Negative",
                                  "NA" = "Not Tested"))



#write.csv(labs_labeled,file = "Data/Labeled_Imputed/labs_labeled.csv")




############################################## Medications######################################
medications   = read.csv("Data/Raw/medications.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))


nrow(medications)
ncol(medications)
summary(medications)
str(medications)


##################################### ZV/NZV feature remove 

medsdata_major <- medications

if (length(nearZeroVar(medsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  medsdata_major <- medsdata_major[, -nearZeroVar(medsdata_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                  names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
}
rm(medications)
#######################################  Check the data for missing values.

sapply(medsdata_major, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))

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

#write.csv(medsdata_Col_Labels,file = "Data/Labels/medsdata_Col_Labels.csv")


## 
# We have to now enter categorization of Factor/Numeric/ 'Computation not required' in the excel file generated
### Only to be done in 3rd column
## Code is 
# 0 = Factor requiring no computation
# 1 = Numeric requiring computation
# 2 = Factor requiring computation

# Please write Column name for the category as "Cat"



#  Categorization of variables

Cat_meds <- c(0,1,2,2,1,1,2,2,1)
Cat_meds
medsdata_Col_Labels <- data.frame(medsdata_Col_Labels,Cat = Cat_meds)

#write.csv(medsdata_Col_Labels,file = "Data/Labels/medsdata_Col_Labels.csv")



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

medsdata_selected[, Catcolmn_medsdata] <- sapply(medsdata_selected[, Catcolmn_medsdata], as.factor)
medsdata_selected[, Catcolmn_Nul_medsdata] <- sapply(medsdata_selected[, Catcolmn_Nul_medsdata], as.factor)
medsdata_selected[, Numcolmn_medsdata] <- sapply(medsdata_selected[, Numcolmn_medsdata], as.numeric)

#Look the dataset structure.

sapply(medsdata_selected, function(x) sum(is.na(x)))


#  IMPUTATION( MICE package) 

#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)


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

#write.csv(medsdata_imputed , "Data/Clean_Imputes/medsdata_imputed.csv",row.names = FALSE)
medsdata_imputed   = read.csv("Data/Clean_Imputes/medsdata_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))

#### Labeling the dataset 
medsdata_imputed_subset <- medsdata_imputed
meds_subset_labelled <- medsdata_imputed_subset
colnames(meds_subset_labelled) <- with(Dictionary,
                                       Dictionary$Variable.Description[match(colnames(medsdata_imputed_subset),
                                                                             Dictionary$Variable.Name,
                                                                             nomatch = Dictionary$Variable.Name
                                       )])

str(meds_subset_labelled)
write.csv(meds_subset_labelled,file = "Data/Labeled_Imputed/meds_subset_labelled.csv")


############################################## Questionnaire#############################################

questionnaire = read.csv("Data/Raw/questionnaire.csv", header = TRUE, na.strings = c("NA","","#NA"))
Dictionary    = read.csv("Data/Raw/Dictionary.csv", header = TRUE, na.strings = c("NA","","#NA"))

nrow(questionnaire)
ncol(questionnaire)
summary(questionnaire)
str(questionnaire)



##################################### ZV/NZV feature remove 


ques_data_major <- questionnaire


if (length(nearZeroVar(ques_data_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                       names = FALSE, foreach = FALSE, allowParallel = TRUE)) > 0){
  ques_data_major <- ques_data_major[, -nearZeroVar(ques_data_major, freqCut = 90/2, uniqueCut = 10, saveMetrics = FALSE,
                                                    names = FALSE, foreach = FALSE, allowParallel = TRUE)] 
  
}


#######################################  Check the data for missing values.

sapply(ques_data_major, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))

#######################################  Removing data having greater than 25% missing values



Null_Num_ques_data <- apply(ques_data_major, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "-999" ))/length(x))
Null_Colms_ques_data <- colnames(ques_data_major)[Null_Num_ques_data > 0.25]
ques_data75 <- select(ques_data_major, -Null_Colms_ques_data)

colSums(is.na(ques_data75))
sapply(ques_data75, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 25) %>% setNames(nm=c("variable", "missing"))



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
#write.csv(ques_data_Col_Labels,file = "Data/Labels/ques_data_Col_Labels.csv")

#######################################  Categorization of variables


#  Categorization of variables

Cat_ques <- c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

ques_data_Col_Labels <- data.frame(ques_data_Col_Labels,Cat = Cat_ques)

#write.csv(ques_data_Col_Labels,file = "Data/Labels/ques_data_Col_Labels.csv")




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
ques_data_selected[, Catcolmn_Nul_ques_data] <- sapply(ques_data_selected[, Catcolmn_Nul_ques_data], as.numeric)
ques_data_selected[, Numcolmn_ques_data] <- sapply(ques_data_selected[, Numcolmn_ques_data], as.numeric)

#Look the dataset structure.


#  IMPUTATION( MICE package)   
#recisely, the methods used by this package are:
#1)-PMM (Predictive Mean Matching) — For numeric variables
#2)-logreg(Logistic Regression) — For Binary Variables( with 2 levels)
#3)-polyreg(Bayesian polytomous regression) — For Factor Variables (>= 2 levels)
#4)-Proportional odds model (ordered, >= 2 levels)
#5)-cart  Classification and regression trees (any) 
#6)rf Random forest imputations (any)

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

meth_ques_data[Catcolmn_ques_data]="rf"

## we impute the Numerical Variable

meth_ques_data[Numcolmn_ques_data]="pmm"


set.seed(415)
imputed_ques_data = mice(ques_data_selected, method=meth_ques_data, predictorMatrix=predM_ques_data, m=5)

#Create a dataset after imputation.

ques_data_imputed<- complete(imputed_ques_data)



#Check for missings in the imputed dataset.
sapply(ques_data_imputed, function(x) sum(is.na(x)))


#######################################  Saving Impute


#write.csv(ques_data_imputed , "Data/Clean_Imputes/ques_data_imputed.csv",row.names = FALSE)
ques_data_imputed   = read.csv("Data/Clean_Imputes/ques_data_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))


ques_data_imputed_subset <-ques_data_imputed

#### Labeling the dataset 

# Select interesting Questions

ques_Yes_No_NO_SEQN <- c("HSQ500","HSQ510","HSQ520","DIQ010","DIQ050","DLQ010","DLQ020","DLQ040","FSD151","FSQ162","HIQ011","HIQ210","HUQ090","MCQ010","MCQ053","MCQ300B","SMQ870")


ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ][ ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ] == "1" ] <- "Yes"
ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ][ ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ] == "2" ] <- "No"
ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ][ ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ] == "7" ] <- "Refused"
ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ][ ques_data_imputed_subset[ , ques_Yes_No_NO_SEQN ] == "9" ] <- "Unknown"



ques_data_numeric1 <-c("CBD070", "CBD110","CBD120","CBD130")
ques_data_imputed_subset[ , ques_data_numeric1 ][ ques_data_imputed_subset[ , ques_data_numeric1 ] == "777777" ] <- "Refused"
ques_data_imputed_subset[ , ques_data_numeric1 ][ ques_data_imputed_subset[ , ques_data_numeric1 ] == "999999" ] <- "Unknown"


ques_data_numeric2 <-c("DBD895", "DBD905","DBD910","CBD130")
ques_data_imputed_subset[ , ques_data_numeric2 ][ ques_data_imputed_subset[ , ques_data_numeric2 ] == "0" ] <- "None"
ques_data_imputed_subset[ , ques_data_numeric2 ][ ques_data_imputed_subset[ , ques_data_numeric2 ] == "7777" ] <- "Refused"
ques_data_imputed_subset[ , ques_data_numeric2 ][ ques_data_imputed_subset[ , ques_data_numeric2 ] == "9999" ] <- "Unknown"
ques_data_imputed_subset[ , "DBD895" ][ ques_data_imputed_subset[ , "DBD895" ] == "5555" ] <- "More than 21 meals per week"


ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(HUQ010  = recode(HUQ010 ,
                          "1" =	"Excellent"	,
                          "2" =	"Very good"	,
                          "3"=	"Good"	,
                          "4"=	"Fair"	,
                          "5"	= "Poor"	,
                          "7"=	"Refused"	,
                          "9"=	"Unknown"))


ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(DBQ197  = recode(DBQ197 ,
                          "0"=	"Never",
                          "1"=	"Rarely-less than once a week",
                          "2"=	"Sometimes-once a week or more, but less than once a day",
                          "3"=	"Often-once a day or more?",
                          "4"=	"Varied",
                          "7"=	"Refused",
                          "9"=	"Unknown"))


ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(HUQ041  = recode(HUQ041 ,
                          "1"=	"Clinic or health center",	
                          "2"=	"Doctor's office or HMO",	
                          "3"=	"Hospital emergency room",	
                          "4"=	"Hospital outpatient department",	
                          "5"=	"Some other place",	
                          "6"=	"Doesn't go to one place most often",	
                          "77"=	"Refused",	
                          "99"=	"Unknown"))

ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(HUQ051  = recode(HUQ051 ,
                          "0"=	"None",	
                          "1"=	"1",
                          "2"=	"2 to 3",
                          "3"=	"4 to 5",
                          "4"=	"6 to 7",
                          "5"=	"8 to 9",
                          "6"=	"10 to 12",
                          "7"=	"13 to 15",
                          "8"=	"16 or more",
                          "77"=	"Refused",
                          "99"=	"Unknown"))

ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(IND235  = recode(IND235 ,
                          "1"=	"$0 - $399",	
                          "2"=	"$400 - $799",	
                          "3"=	"$800 - $1249",	
                          "4"=	"$1250 - $1649",	
                          "5"=	"$1650 - $2099",	
                          "6"=	"$2100 - $2899",	
                          "7"=	"$2900 - $3749",	
                          "8"=	"$3750 - $4599",	
                          "9"=	"$4600 - $5399",	
                          "10"=	"$5400 - $6249",	
                          "11"=	"$6250 - $8399",	
                          "12"=	"$8400 and over",	
                          "77"=	"Refused",	
                          "99"=	"Unknown"))


ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(OHQ030  = recode(OHQ030 ,
                          "1"=	"6 months or less",	
                          "2"=	"More than 6 months, but not more than 1 year ago",	
                          "3"=	"More than 1 year, but not more than 2 years ago",	
                          "4"=	"More than 2 years, but not more than 3 years ago",	
                          "5"=	"More than 3 years, but not more than 5 years ago",	
                          "6"=	"More than 5 years ago",	
                          "7"=	"Never have been",
                          "77"=	"Refused",	
                          "99"=	"Unknown"	))


ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(PAQ710  = recode(PAQ710 ,
                          "0"=	"Less than 1 hour",	
                          "1"=	"1 hour",	
                          "2"=	"2 hours",	
                          "3"=	"3 hours",	
                          "4"=	"4 hours",	
                          "5"=	"5 hours or more",	
                          "8"=	"{You don't/SP does not} watch TV or videos",	
                          "77"=	"Refused",	
                          "99"=	"Unknown"	))

ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(PAQ715  = recode(PAQ715 ,
                          "0"=	"Less than 1 hour",	
                          "1"=	"1 hour",	
                          "2"=	"2 hours",	
                          "3"=	"3 hours",	
                          "4"=	"4 hours",	
                          "5"=	"5 hours or more",	
                          "8"=	"{you do not/SP does not} use a computer outside of school",	
                          "77"=	"Refused",	
                          "99"=	"Unknown"	))

ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(SMD460  = recode(SMD460 ,
                          "0"=	"No one in houseold is a smoker",
                          "1"=	"1 household member is a smoker",	
                          "2"=	"2 household members are smokers",	
                          "3"=	"3 or more household members are smokers",	
                          "777"=	"Refused	5	10058	End of Section",
                          "999"=	"Unknown"))


ques_data_imputed_subset <- ques_data_imputed_subset %>%
  mutate(HOD050  = recode(HOD050 ,
                          "1"=	"1",	
                          "2"=	"2",	
                          "3"=	"3",	
                          "4"=	"4",	
                          "5"=	"5",	
                          "6"=	"6",	
                          "7"=	"7",	
                          "8"=	"8",	
                          "9"=	"9",	
                          "10"=	"10",	
                          "11"=	"11",	
                          "12"=	"12",	
                          "13"=	"13 or more",
                          "777"=	"Refused",	
                          "999"=	"Unknown"	))



ques_subset_labelled <- ques_data_imputed_subset
colnames(ques_subset_labelled) <- with(Dictionary,
                                       Dictionary$Variable.Description[match(colnames(ques_data_imputed_subset),
                                                                             Dictionary$Variable.Name,
                                                                             nomatch = Dictionary$Variable.Name
                                       )])


#write.csv(ques_subset_labelled,file = "Data/Labeled_Imputed/ques_subset_labelled.csv")




########################################### Reading Imputed (Unlabelled) files ###########################


demographic_imputed   = read.csv("Data/Clean_Imputes/demographic_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
diet_imputed   = read.csv("Data/Clean_Imputes/diet_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
exam_imputed   = read.csv("Data/Clean_Imputes/exam_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
labsdata_imputed   = read.csv("Data/Clean_Imputes/labsdata_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
medsdata_imputed   = read.csv("Data/Clean_Imputes/medsdata_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
ques_data_imputed   = read.csv("Data/Clean_Imputes/ques_data_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))




############################################## Combining Imputed & Imputing Target NA ###################################


## Combine the clean datasets (IMPORTANT REQUIRED)
#Dataset Merge & select attribute


data1   = read.csv("Data/Labeled_Imputed/demo_subset_8_labeled.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
data2   = read.csv("Data/Labeled_Imputed/diet_labeled.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
data3   = read.csv("Data/Labeled_Imputed/exam_labeled.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
data4   = read.csv("Data/Labeled_Imputed/labs_labeled.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
data5   = read.csv("Data/Labeled_Imputed/meds_subset_labelled.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
data6   = read.csv("Data/Labeled_Imputed/ques_subset_labelled.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]



data3 = dplyr::rename(data3,"ID"="SEQN")
data4 = dplyr::rename(data4, "ID"="SEQN")
#data4 = dplyr::rename(data4,"ID"="SEQN")
colnames(data5)[1] <- "ID"
colnames(data6)[1] <- "ID"

data_selected <- merge(data1, data2,by="ID")
data_selected <- merge(data_selected, data3,by="ID")
data_selected <- merge(data_selected, data4,by="ID")
data_selected <- merge(data_selected, data5,by="ID")
data_selected <- merge(data_selected, data6,by="ID")

#rm(data1,data2,data3,data4,data5,data6)

write.csv(data_selected,file = "Data/Labeled_Imputed/Data_Combined.csv")


sapply(data_selected, function(x) ((sum(is.na(x))))*.01) %>%
  stack %>% rev %>% filter(values > 0) %>% setNames(nm=c("variable", "missing"))


#Classifications 


##################### Demographics#######################################

demo_subset_8   = read.csv("Data/Working/demo_subset_8_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
target_disease_dataset   = read.csv("Data/Working/target_disease_dataset.csv", header = TRUE, na.strings = c("NA","","#NA"))[-1]
demographic_imputed   = read.csv("Data/Clean_Imputes/demographic_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
#+++++++++++++++++++++++ PCA ++++++++++++++++++++++++++++++++

library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

#+++++++++++++++++++++++++ full+++++++++++++++++++++++++++
demographic_imputed.pca <- prcomp(demographic_imputed[,c(2:31)], center = TRUE,scale = TRUE)
summary(demographic_imputed.pca)

screeplot(demographic_imputed.pca, type = "l", npcs = 20, main = "Screeplot of the first 20 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(demographic_imputed.pca$sdev^2 / sum(demographic_imputed.pca$sdev^2))
plot(cumpro[0:20], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 11, col="blue", lty=5)
abline(h = 0.7198, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC11"),
       col=c("blue"), lty=5, cex=0.6)



library("factoextra")
fviz_pca_ind(demographic_imputed.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = target_disease_dataset$HAS_DIABETES, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "HAS_DIABETES") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


#+++++++++++++++++++++ subset 8 ++++++++++++++++++++++++++++++++++++++
demo_subset_8.pca <- prcomp(demo_subset_8[,c(2:9)], center = TRUE,scale = TRUE)
summary(demo_subset_8.pca)
str(demo_subset_8.pca)
#write.csv(data_selected,file = "Data/Working/data_selected.csv")
ggbiplot(demo_subset_8.pca)

screeplot(demo_subset_8.pca, type = "l", npcs = 8, main = "Screeplot of the 8 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#We notice is that the first 4 components has an Eigenvalue >1 
#and explains almost 60% of variance, this is not great! 
#We can not effectively reduce dimensionality from 8  to 4 becuase we will lose 
# about 40% of variance!

cumpro <- cumsum(demo_subset_8.pca$sdev^2 / sum(demo_subset_8.pca$sdev^2))
plot(cumpro[0:8], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 4, col="blue", lty=5)
abline(h = 0.5934, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC4"),
       col=c("blue"), lty=5, cex=0.6)



library("factoextra")
fviz_pca_ind(demo_subset_8.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = target_disease_dataset$HAS_DIABETES, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "HAS_DIABETES") +
  ggtitle("2D PCA-plot from 8 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


fviz_pca_ind(demo_subset_8.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = target_disease_dataset$HAS_HYPERTENSION, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "HAS_HYPERTENSION") +
  ggtitle("2D PCA-plot from 8 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


fviz_pca_ind(demo_subset_8.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = target_disease_dataset$HAS_CANCER, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "HAS_CANCER") +
  ggtitle("2D PCA-plot from 8 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))




#+++++++++++++++++++++++ K-means ++++++++++++++++++++++++++++++++

#Elbow plot method.
library(purrr)
set.seed(226)
# function to calculate total intra-cluster sum of square
demo8_iss <- function(k) {
  kmeans(demo_subset_8[,2:9],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
demo8_iss_values <- map_dbl(k.values, demo8_iss)
plot(k.values, demo8_iss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

# From the above graph, we conclude that 6 is the appropriate number of clusters 
#since it seems to be appearing at the bend in the elbow plot.


# Now, let us take k = 8 as our optimal cluster –

demo8_k6<-kmeans(demo_subset_8[,2:9],6,iter.max=100,nstart=50,algorithm="Lloyd")
demo8_k6

# Visualizing the Clustering Results using the First Two Principle Components

pcclust=prcomp(demo_subset_8[,2:9],scale=TRUE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

set.seed(100)
ggplot(demo_subset_8, aes(x =Gender, y = Age)) +
  geom_point(stat = "identity", aes(color = as.factor(demo8_k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Demographics Data ", subtitle = "Using K-means Clustering")

# From the above visualization, we observe that there is a distribution of 6 clusters as follows
# both Gender have almost the same age cluster


ggplot(demo_subset_8, aes(x =Marital_status, y = Family_income)) +
  geom_point(stat = "identity", aes(color = as.factor(demo8_k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Demographics Data ", subtitle = "Using K-means Clustering")

ggplot(demo_subset_8, aes(x =Country_of_birth, y = Marital_status)) +
  geom_point(stat = "identity", aes(color = as.factor(demo8_k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Demographics Data ", subtitle = "Using K-means Clustering")



#+++++++++++++++++++++++ K-means ++++++++++++++++++++++++++++++++



########################################################################
### Code for creating target dataset for 
#DIQ010 - Doctor told you have diabetes
#https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.htm
#The next questions are about specific medical conditions. {Other than during pregnancy, {have you/has SP}/{Have you/Has SP}} ever been told by a doctor or health professional that {you have/{he/she/SP} has} diabetes or sugar diabetes?

# BPQ020 - Ever told you had high blood pressure
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPQ_H.htm
# {Have you/Has SP} ever been told by a doctor or other health professional that {you/s/he} had hypertension, also called high blood pressure?

# MCQ220 - Ever told you had cancer or malignancy
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.htm#MCQ220
# {Have you/Has SP} ever been told by a doctor or other health professional that {you/s/he} had cancer or a malignancy (ma-lig-nan-see) of any kind?

# Create the target dataset for the Supervised problem.
temp_questionnaire = read.csv("Data/Raw/questionnaire.csv", header = TRUE, na.strings = c("NA","","#NA"))
summary(temp_questionnaire)
target_columns <- c("SEQN","DIQ010","BPQ020","MCQ220")
target_disease_dataset = subset(temp_questionnaire, select=target_columns)

# Change disease indicators into factors
target_disease_dataset$MCQ220 <- as.factor(target_disease_dataset$MCQ220)
target_disease_dataset$DIQ010 <- as.factor(target_disease_dataset$DIQ010)
target_disease_dataset$BPQ020 <- as.factor(target_disease_dataset$BPQ020)

#Create new column for target values
target_disease_dataset = cbind(target_disease_dataset, HAS_DIABETES= ifelse(target_disease_dataset$DIQ010 == 1, "YES", "NO" ) )
target_disease_dataset= cbind(target_disease_dataset, HAS_HYPERTENSION= ifelse(target_disease_dataset$BPQ020 == 1, "YES", "NO" ) )
target_disease_dataset = cbind(target_disease_dataset, HAS_CANCER= ifelse(target_disease_dataset$MCQ220 == 1, "YES", "NO" ) )
summary(target_disease_dataset)

# With new target values, set "NA" to "NO"
target_disease_dataset$HAS_DIABETES[is.na(target_disease_dataset$HAS_DIABETES)] <- "NO"
target_disease_dataset$HAS_HYPERTENSION[is.na(target_disease_dataset$HAS_HYPERTENSION)] <- "NO"
target_disease_dataset$HAS_CANCER[is.na(target_disease_dataset$HAS_CANCER)] <- "NO"
summary(target_disease_dataset)

write.csv(target_disease_dataset,file = "Data/Working/target_disease_dataset.csv")








#Define the predictors




#Train the model




#Evaluate the model





#Define the predictors



#Train the model




#Evaluate the model









summary(Working_Data$LBXGH)






############################################## Marking Target IDS ###########################




summary(imp_data_imputed$LBXGH)

imp_target_Data = cbind(imp_data_imputed, Diabetes = ifelse(
  imp_data_imputed$LBXGH >= 5.7,
  "Yes", "No" ))
summary(imp_target_Data$Diabetes)
imp_target_Data = cbind(imp_target_Data, Target = ifelse(
  imp_target_Data$Diabetes == "Yes",
  1, 0 ))
summary(imp_target_Data$Target)
str(imp_target_Data$Target)



#######################################  Saving Impute

write.csv(imp_target_Data , "Data/Working/imp_target_Data.csv")
imp_target_Data   = read.csv("Data/Working/imp_target_Data.csv", header = TRUE, na.strings = c("NA","","#NA"))


################################# Extrating Target #################################

imp_target_Data   = read.csv("Data/Working/imp_target_Data.csv", header = TRUE, na.strings = c("NA","","#NA"))

Working_Data <- imp_target_Data

targetSeqn <- subset(Working_Data, select = c("SEQN","LBXGH", "Diabetes", "Target"))
write.csv(targetSeqn , "Data/Working/targetSeqn.csv")

targetSeqn   = read.csv("Data/Working/targetSeqn.csv", header = TRUE, na.strings = c("NA","","#NA"))


######################################## SELECT INTERESTING FEATURES (DEMOGRAPHIC) ######################
demographic_imputed   = read.csv("Data/Working/demographic_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
targetSeqn   = read.csv("Data/Working/targetSeqn.csv", header = TRUE, na.strings = c("NA","","#NA"))

data_List3 = list(demographic_imputed,targetSeqn)
Imputed_combined<-join_all(data_List3,by="SEQN")


write.csv(Imputed_combined , "Data/Working/Imputed_combined.csv")
sapply(Imputed_combined, function(x) sum(is.na(x)))



labsdata_imputed   = read.csv("Data/Working/labsdata_imputed.csv", header = TRUE, na.strings = c("NA","","#NA"))
targetSeqn   = read.csv("Data/Working/targetSeqn.csv", header = TRUE, na.strings = c("NA","","#NA"))

sapply(labsdata_imputed, function(x) sum(is.na(x)))


data_List4 = list(labsdata_imputed,targetSeqn)
Labs_with_Target<-join_all(data_List4,by="SEQN")

sapply(Labs_with_Target, function(x) sum(is.na(x)))
ncol(Labs_with_Target)
x = Labs_with_Target.iloc[:0,83]

which( colnames(Labs_with_Target)=="Target" )

y = Labs_with_Target.iloc[:0,-83]

labs_best <-SelectKBest(score=chi2,k=10)
labs_fit = labs_best.fit







summary(Working_Data$LBXGH)
summary(Working_Data$DID060)
summary(Working_Data$DID040)
summary(Working_Data$BPQ020)
summary(Working_Data$BPD035)
summary(Working_Data$MCQ220)










############EXAMPLE form Hashman #######################
#Clustering water treatment plants
#Example 0
library(fpc)
library(data.table)
library(ggplot2)
#load data
water_data <- read.table(file.choose(),sep = ",",header = F,na.strings = c("?"))
setDT(water_data)
head(water_data)
#lets check missing values
colSums(is.na(water_data))

#impute missing values with median
for(i in colnames(water_data)[!(colnames(water_data) %in% c("V1"))])
  set(x = water_data,i = which(is.na(water_data[[i]])), j = i, value = median(water_data[[i]], na.rm = T))


#scale the variables
scaled_wd <- scale(water_data[,-c("V1"),with=F])

#Hierarchical Clustering
d <- dist(scaled_wd,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward.D2") #clustering
plot(h_clust,labels = water_data$V1) #dendrogram

rect.hclust(h_clust,k=4)

#extract clusters
groups <- cutree(h_clust,k=4)
groups

#pca
pcmp <- princomp(scaled_wd)
pred_pc <- predict(pcmp, newdata=scaled_wd)[,1:2]

comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = water_data$V1)

ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)

#kmeans
kclust <- kmeans(scaled_wd,centers = 4,iter.max = 100)

ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = as.factor(kclust$cluster)),size=3)

tunek <- kmeansruns(scaled_wd,krange = 1:10,criterion = "ch")
tunek$bestk #3
tunekw <- kmeansruns(scaled_wd,krange = 1:10,criterion = "asw")
tunekw$bestk #4

#Clustering wines
#K-Means
#Example 1

install.packages('rattle')
library(rattle)
wine.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(wine.url, header=FALSE)
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash',
                    'Alcalinity', 'Magnesium', 'Phenols',
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue',
                    'Dilution', 'Proline')
wine$Type <- as.factor(wine$Type)

head(wine)
#In this data set we observe the composition of different wines. 
#Given a set of observations (x1,x2,.,xn), where each observation is a d-dimensional real vector, 
#k-means clustering aims to partition the n observations into (k≤n) S={S1,S2,.,Sk} so as to minimize 
#the within-cluster sum of squares (WCSS). In other words, its objective is to find::

#  argmin_S ∑i=1->k ∑xj∈Si ∥xj−μi∥^2

#where μi is the mean of points in Si. The clustering optimization problem is solved with the 
#function kmeans in R.

wine.stand <- scale(wine[-1])  # To standarize the variables

# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3

#In k.means.fit are contained all the elements of the cluster output:
attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster

# Cluster size:
k.means.fit$size

#A fundamental question is how to determine the value of the parameter k. 
#If we looks at the percentage of variance explained as a function of the number of clusters: 
#One should choose a number of clusters so that adding another cluster doesn’t give much better 
#modeling of the data. More precisely, if one plots the percentage of variance explained by the 
#clusters against the number of clusters, the first clusters will add much information 
#(explain a lot of variance), but at some point the marginal gain will drop, 
#giving an angle in the graph. The number of clusters is chosen at this point, hence the “elbow criterion”.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6) 

#Library clusters allow us to represent (with the aid of PCA) the cluster solution into 2 dimensions:

library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#In order to evaluate the clustering performance we build a confusion matrix:

table(wine[,1],k.means.fit$cluster)

#Hierarchical clustering:
#Hierarchical methods use a distance matrix as an input for the clustering algorithm. 
#The choice of an appropriate metric will influence the shape of the clusters, as some element
#may be close to one another according to one distance and farther away according to another.

d <- dist(wine.stand, method = "euclidean") # Euclidean distance matrix.
#We use the Euclidean distance as an input for the clustering algorithm 
#(Ward’s minimum variance criterion minimizes the total within-cluster variance):

H.fit <- hclust(d, method="ward.D2")

#The clustering output can be displayed in a dendrogram

plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=3, border="red") 

#The clustering performance can be evaluated with the aid of a confusion matrix as follows:

table(wine[,1],groups)

#Example 2: EUROPEAN PROTEIN CONSUMPTION
#We consider 25 European countries (n = 25 units) and their protein intakes (in percent) from 
#nine major food sources (p = 9). The data are listed below.

url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
head(food)

#We start first, clustering on just Red and White meat (p=2) and k=3 clusters.

set.seed(123456789) ## to fix the random starting clusters
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
grpMeat

## list of cluster assignments
o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

#To see a graphical representation of the clustering solution we plot cluster assignments 
#on Red and White meat on a scatter plot:

plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)

#Next, we cluster on all nine protein groups and prepare the program to create seven clusters. 
#The resulting clusters, shown in color on a scatter plot of white meat against red meat 
#(any other pair of features could be selected), actually makes lot of sense. 
#Countries in close geographic proximity tend to be clustered into the same group.

## same analysis, but now with clustering on all
## protein groups change the number of clusters to 7
set.seed(123456789)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])

library(cluster)
clusplot(food[,-1], grpProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

#Alternatively we can implement a Hierarchical approach. We use the agnes function in the package cluster.
#Argument diss=FALSE indicates that we use the dissimilarity matrix that is being calculated from raw data. 
#Argument metric=“euclidian” indicates that we use Euclidean distance. No standardization is used and the 
#link function is the “average” linkage.

foodagg=agnes(food,diss=FALSE,metric="euclidian")
plot(foodagg, main='Dendrogram') ## dendrogram


groups <- cutree(foodagg, k=4) # cut tree into 3 clusters
rect.hclust(foodagg, k=4, border="red") 


#Example 3: Customer Segmentation
#Customer segmentation is as simple as it sounds: grouping customers by their characteristics - 
#and why would you want to do that? To better serve their needs!

#Our example is to do with e-mail marketing. We use the dataset from this 
#http://www.salemmarafi.com/wp-content/uploads/2014/04/clustering-vanilla.xls

offers<-read.table(file.choose(), sep = ',', header=T, fileEncoding="UTF-8-BOM")
head(offers)

transactions<-read.table(file.choose(), sep = ',', header=T, fileEncoding="UTF-8-BOM")
head(transactions)

#Step 1: Organizing the information
#We have two data sets: one for the offers and the other for the transactions. 
#First what we need to do is create a transaction matrix. That means, we need to put the 
#offers we mailed out next to the transaction history of each customer. This is easily achieved 
#with a pivot table.

# Create transaction matrix (a pivot table like in Excel way!)
library(reshape)
pivot<-melt(transactions[1:2])
pivot<-(cast(pivot,value~CustomerLastName,fill=0,fun.aggregate=function(x) length(x)))
pivot<-cbind(offers,pivot[-1])

# write.csv(file="pivot.csv",pivot) # to save your data

cluster.data<-pivot[,8:length(pivot)]
cluster.data<-t(cluster.data)
head(cluster.data)
#In the clustering data set, rows represents costumers and columns are different wine brands/types.

#Step 2: Distances and Clusters
#We will use k=4 indicating that we will use 4 clusters. This is somewhat arbitrary, but the number 
#you pick should be representative of the number of segments you can handle as a business. 
#So 100 segments does not make sense for an e-mail marketing campaign.

#We need to calculate how far away each customer is from the cluster’s mean. To do this we could use 
#many distances/dissimilarity index, one of which is the Gower dissimilarity.

library(cluster)
D=daisy(cluster.data, metric='gower')

#After the creation of a distance matrix, we implement a Ward’s hierarchical clustering procedure:

H.fit <- hclust(D, method="ward.D2")
plot(H.fit) # display dendrogram

groups <- cutree(H.fit, k=4) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters
rect.hclust(H.fit, k=4, border="red") 


# 2D representation of the Segmentation:
clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')


#Top get the top deals we will have to do a little bit of data manipulation. First we need to combine 
#our clusters and transactions. Notably the lengths of the ‘tables’ holding transactions and clusters 
#are different. So we need a way to merge the data . so we use the merge() function and give our columns 
#sensible names:

# Merge Data

cluster.deals<-merge(transactions[1:2],groups,by.x = "CustomerLastName", by.y = "row.names")

colnames(cluster.deals)<-c("Name","Offer","Cluster")
head(cluster.deals)

#We then want to repeat the pivoting process to get Offers in rows and clusters in columns 
#counting the total number of transactions for each cluster. Once we have our pivot table we 
#will merge it with the offers data table like we did before:

# Get top deals by cluster
cluster.pivot<-melt(cluster.deals,id=c("Offer","Cluster"))
cluster.pivot<-cast(cluster.pivot,Offer~Cluster,fun.aggregate=length)
cluster.topDeals<-cbind(offers,cluster.pivot[-1])
head(cluster.topDeals)

#Example 4: Social Network Clustering Analysis
#For this analysis, we will be using a dataset representing a random sample of 30.000 U.S. 
#high school students who had profiles on a well-known Social Network in from 2006 to 2009.

#From the top 500 words appearing across all pages, 36 words were chosen to represent 
#five categories of interests, namely extracurricular activities, fashion, religion, 
#romance, and antisocial behavior. The 36 words include terms such as football, sexy, 
#kissed, bible, shopping, death, and drugs. The final dataset indicates, for each person, 
#how many times each word appeared in the person’s SNS profile.

teens <- read.csv(file.choose())
head(teens,3)

dim(teens)
#Let’s also take a quick look at the specifics of the data. The first several lines of the str() 
#output are as follows:

str(teens)

#As we had expected, the data include 30,000 teenagers with four variables indicating personal 
#characteristics and 36 words indicating interests. Note that there are some NA’s in the variable gender.

summary(teens$age)

#We can replace missing values with the median:
teens$age <- sapply(teens$age, FUN=function(x) {ifelse(is.na(x),median(teens$age, na.rm = TRUE),x)})

#We can skip all the data with missing values:
teens = na.omit(teens)

#We can predict the missing values using kNN:
install.packages("VIM")
library(VIM)
kNN(teens, variable = c("age"))

dim(teens)

#We’ll start our cluster analysis by considering only the 36 features that represent
#the number of times various interests appeared on the SNS profiles of teens. 
#For convenience, let’s make a data frame containing only these features:

interests <- teens[5:40]
#To apply z-score standardization to the interests data frame, 
#we can use the scale() function with lapply(), as follows:

interests_z <- as.data.frame(lapply(interests, scale))
#To divide teens into five clusters, we can use the following command:


teen_clusters <- kmeans(interests_z, 5)
#number of examples falling in each of the groups. 
#If the groups are too large or too small, then they are not likely to be very useful. 
#To obtain the size of the kmeans() clusters, use the teen_clusters$size component as follows:

teen_clusters$size

#For a more in-depth look at the clusters, we can examine the coordinates of the 
#cluster centroids using the teen_clusters$centers component, which is as follows for the 
#first eight features:

teen_clusters$centers

#The cluster characterization can be obtained with pie charts:

par(mfrow=c(2,2))
pie(colSums(interests[teen_clusters$cluster==1,]),cex=0.5)

pie(colSums(interests[teen_clusters$cluster==2,]),cex=0.5)

pie(colSums(interests[teen_clusters$cluster==3,]),cex=0.5)

pie(colSums(interests[teen_clusters$cluster==4,]),cex=0.5)



