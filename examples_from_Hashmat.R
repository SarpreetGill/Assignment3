################################################################################
######################## EXAMPLES FROM HASHMAT ##############################


+# Model 
  
  
  ##########testing some models
  
  .
#### this is the example from the Water treatment data .. 
## can we use for mosquito??

#Clustering water treatment plants
#Example 0
library(fpc)
library(data.table)
library(ggplot2)
#load data
water_data <- read.table("_test/water-treatment.data_.txt",sep = ",",header = F,na.strings = c("?"))
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
#k-means clustering aims to partition the n observations into (k???n) S={S1,S2,.,Sk} so as to minimize 
#the within-cluster sum of squares (WCSS). In other words, its objective is to find::

#  argmin_S ???i=1->k ???xj???Si ???xj?????i???^2

#where ??i is the mean of points in Si. The clustering optimization problem is solved with the 
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
#One should choose a number of clusters so that adding another cluster doesn't give much better 
#modeling of the data. More precisely, if one plots the percentage of variance explained by the 
#clusters against the number of clusters, the first clusters will add much information 
#(explain a lot of variance), but at some point the marginal gain will drop, 
#giving an angle in the graph. The number of clusters is chosen at this point, hence the "elbow criterion".

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
#(Ward's minimum variance criterion minimizes the total within-cluster variance):

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
#Argument metric="euclidian" indicates that we use Euclidean distance. No standardization is used and the 
#link function is the "average" linkage.

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

#We need to calculate how far away each customer is from the cluster's mean. To do this we could use 
#many distances/dissimilarity index, one of which is the Gower dissimilarity.

library(cluster)
D=daisy(cluster.data, metric='gower')

#After the creation of a distance matrix, we implement a Ward's hierarchical clustering procedure:

H.fit <- hclust(D, method="ward.D2")
plot(H.fit) # display dendrogram

groups <- cutree(H.fit, k=4) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters
rect.hclust(H.fit, k=4, border="red") 


# 2D representation of the Segmentation:
clusplot(cluster.data, groups, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Customer segments')


#Top get the top deals we will have to do a little bit of data manipulation. First we need to combine 
#our clusters and transactions. Notably the lengths of the 'tables' holding transactions and clusters 
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
#how many times each word appeared in the person's SNS profile.

teens <- read.csv(file.choose())
head(teens,3)

dim(teens)
#Let's also take a quick look at the specifics of the data. The first several lines of the str() 
#output are as follows:

str(teens)

#As we had expected, the data include 30,000 teenagers with four variables indicating personal 
#characteristics and 36 words indicating interests. Note that there are some NA's in the variable gender.

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

#We'll start our cluster analysis by considering only the 36 features that represent
#the number of times various interests appeared on the SNS profiles of teens. 
#For convenience, let's make a data frame containing only these features:

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


