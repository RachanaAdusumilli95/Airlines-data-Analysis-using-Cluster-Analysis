getwd()
setwd("D:/Rachana/ISB/Term 2/MLUL/IA 1")
dir()

input <- read.csv("EastWestAirlinesCluster.csv",header=TRUE)
dim(input) # checking the dimentions of the input file

mydata <- input[1:3999,2:12] ## excludes ID# and Headings 
normalized_data <- scale(mydata) ## to normalize the columns
set.seed(1112) #  sets the starting number used to generate a sequence of random numbers
# it ensures that you get the same result if you start with that same seed each time you 
# run the same process.

write.table(normalized_data,file = "Output Data file.csv", sep = ',') # Converting the data into CSV file

#b. Hirearchial clustering with ecucledean distance and Ward's method

d <- dist(normalized_data, method = "euclidean") # ecucledian distance,this function calculates the distance between the variables. 
fit <- hclust(d, method="ward.D2") # ward method
plot(fit) # This will display dendrogram

#cutting the plot for 3 clusters (Trial and error)

plot(fit) # display dendrogram
rect.hclust(fit, k=3, border="red")

#likewise checking for other possibilities
plot(fit) # display dendrogram
rect.hclust(fit, k=2, border="green")
rect.hclust(fit, k=4, border="blue")
rect.hclust(fit, k=5, border="yellow")

#c. comparing centeroid distance and naming the clusters

d<- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method="complete")
options(scipen=99999999)


groups <- cutree(fit, k=3) # we are considering 3 as best c=no. of cluster for EastwestAirline problem

clust.centroid = function(i, dat, groups) {
  ind = (groups == i)
  colMeans(dat[ind,])
} # this function is to compare clusters and categorize clusters into different groups 
Matrix<- sapply(unique(groups), clust.centroid, mydata, groups)# Creates a matrix by grouping the data into 3 clusters
Matrix

write.table(Matrix,file = "Output hirearchecal clustering file.csv", sep = ',')

# Output is included in the word file and excel sheet submitted

#d. randomly removing 5% of data to check stability of clusters and comparing if the dendogram looks the same. 

normalized_data_95 <- scale(mydata[sample(nrow(mydata), nrow(mydata)*.95), ])
d <- dist(normalized_data_95, method = "euclidean") 
fit <- hclust(d, method="ward.D2")
plot(fit)

# e.Clustering all passengers again using k-means clustering. 

fit <- kmeans(normalized_data, centers=3, iter.max=10)
KMEans_Matrix<- fit$centers # output included in pdf file
KMEans_Matrix
write.table(KMEans_Matrix,file = "Output Kmeans clustering file.csv", sep = ',')


## Determine number of clusters
Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(normalized_data, centers=i)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")
# Reasoning included in the word file 

#f. How do the characteristics of the clusters, obtained in Part (e), contrast 
#or validate the finding in Part c above?   

# Checking with hierarchical clustering
sapply(unique(groups), clust.centroid, mydata, groups) # output included in word file

# Checking with kmeans clustering
fit <- kmeans(mydata, centers=3, iter.max=10)
KMeans_Matrix<- t(fit$centers) # output included in pdf file
KMeans_Matrix
write.table(KMeans_Matrix,file = "Output Kmeans file.csv", sep = ',') # Output included in pdf file and excel sheet submitted

# g. Analyzing Which cluster(s) to target for offers, and what type of offers to device for
# target customers in that cluster. 

fit$size

# this value comes from k means. Output included in pdf for reference. It shows 