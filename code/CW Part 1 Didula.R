install.packages('dplyr')
install.packages('factoextra')
install.packages('ggplot2')
install.packages('readxl')
install.packages('NbClust')
install.packages("fpc")




# Load required libraries
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(NbClust)
library(fpc)






 # Define function for min-max normalization
 min_max_normalization <- function(data) {
   normalized_data <- apply(data, 2, function(x) (x - min(x)) / (max(x) - min(x)))
   return(normalized_data)
 }
 
 
 # Define function for outliers detection and removal using box plot method with visualization
 remove_outliers_boxplot <- function(data) {
   # Create a list to store outlier indices for each variable
   outlier_indices <- list()
   
   # Create a boxplot for each variable
   for (i in 1:ncol(data)) {
     boxplot_data <- boxplot(data[, i], plot = FALSE)
     
     # Calculate the interquartile range (IQR)
     iqr <- boxplot_data$stats[4] - boxplot_data$stats[2]
     
     # Calculate the lower and upper bounds for outliers detection
     lower_bound <- boxplot_data$stats[2] - 1.3 * iqr
     upper_bound <- boxplot_data$stats[4] + 1.3 * iqr
     
     # Identify outliers
     outliers <- data[, i] < lower_bound | data[, i] > upper_bound
     
     # Store outlier indices for visualization
     outlier_indices[[i]] <- which(outliers)
   }
   
   # Remove rows containing outliers from any variable
   all_outliers <- unlist(outlier_indices)
   data <- data[-all_outliers, ]
   
   # Plot boxplots after removing outliers
   par(mfrow = c(3, 4))  # Adjust layout for 12 variables
   for (i in 1:ncol(data)) {
     boxplot(data[, i], main = paste("Variable", i, "- Boxplot without Outliers"), 
             ylab = names(data)[i], ylim = range(data), col = "lightblue", 
             border = "brown", horizontal = TRUE)
   }
   
   # Return cleaned data
   return(as.data.frame(data))
 }
 






# Load the dataset
wine_data_1 <- read_excel("whitewine_v6.xlsx")
wine_data <- wine_data_1[, 1:11]

str(wine_data)

boxplot(wine_data)

# Pre-processing tasks


# Scale the data
scaled_data <- scale(wine_data)
head(scaled_data)



# Apply outlier detection and removal function
cleaned_data <- remove_outliers_boxplot(scaled_data)
sum(is.na(cleaned_data)) #checking for zero values
boxplot(cleaned_data)

dim(cleaned_data)







# Apply Min-Max normalization to the dataset
scaled_data_min_max <- min_max_normalization(cleaned_data)
head(scaled_data_min_max)

boxplot(scaled_data_min_max)
head(scaled_data_min_max)

# Determine number of cluster centers using automated tools

#NbClust
set.seed(23)
nb_clusters <- NbClust(scaled_data_min_max, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nb_clusters$All.index
nb_clusters$Best.nc
nb_clusters$Best.partition







# Elbow Method
elbow_method <- function(data, kmax) {
  wss_values <- numeric(kmax)
  for (k in 1:kmax) {
    kmeans_model <- kmeans(data, centers = k)
    wss_values[k] <- kmeans_model$tot.withinss
  }
  return(wss_values)
}

k_max <- 10 # Set the maximum number of clusters to consider
wss_values <- elbow_method(scaled_data_min_max, k_max) # Apply the elbow method
plot(1:k_max, wss_values, type = "b", pch = 19, xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WSS)", main = "Elbow Method") # Plot the elbow curve








#silhouette method
fviz_nbclust(scaled_data_min_max, kmeans, method = "silhouette")






#Gap Static Method
max_iter <- 100
fviz_nbclust(as.data.frame(scaled_data_min_max), kmeans, method = 'gap_stat', iter.max = max_iter)+labs(subtitle = "Gap static Method")












# Perform k-means clustering with the best k value
kmeans_model <- kmeans(scaled_data_min_max, centers = 2)
kmeans_model
# Assign cluster labels to the data
cluster_labels <- kmeans_model$cluster
#plot the clusters
fviz_cluster(kmeans_model,scaled_data_min_max, palette = c("#2E9FDF", "#E7B800","#FF0000"), 
             geom = "point", ggtheme = theme_bw())

# Internal evaluation metrics
wss <- kmeans_model$withinss
bss <- kmeans_model$betweenss
wss
bss

# Silhouette plot
silhouette_plot <- fviz_silhouette(silhouette(kmeans_model$cluster, dist(scaled_data_min_max)))
print(silhouette_plot)











 




# Perform PCA analysis
pca_result <- prcomp((scaled_data_min_max), center = TRUE ,scale= TRUE)
print(summary(pca_result)) # Eigenvalues/eigenvectors
cumulative_score <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100) # Cumulative score per principal components (PC)
print(cumulative_score)



# Choose PCs with cumulative score < 85%
selected_pcs <- which(cumulative_score < 85)
cat("Selected principal components:", selected_pcs, "\n")






wine_data_pca <- pca_result$x[,1:6]
boxplot(wine_data_pca)
head(wine_data_pca)
summary(wine_data_pca)
str(wine_data_pca)

 # Elbow Method
 elbow_method <- function(data, kmax) {
   wss_values <- numeric(kmax)
   for (k in 1:kmax) {
     kmeans_model <- kmeans(data, centers = k)
     wss_values[k] <- kmeans_model$tot.withinss
   }
   return(wss_values)
 }
 
 # Apply the Elbow Method
 k_max <- 10
 wss_values <- elbow_method(wine_data_pca, k_max)
 plot(1:k_max, wss_values, type = "b", pch = 19, xlab = "Number of Clusters (k)", 
      ylab = "Within-Cluster Sum of Squares (WSS)", main = "Elbow Method")



 # Determine number of cluster centers using automated tools
 
 #NbClust
 set.seed(23)
 nb_clusters <- NbClust(wine_data_pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
 nb_clusters$All.index
 nb_clusters$Best.nc
 nb_clusters$Best.partition
 
 
 
 
 
 
 
 
 
 
 
 
 #silhouette method
 fviz_nbclust(wine_data_pca, kmeans, method = "silhouette")
 
 
 
 
 
 
 #Gap Static Method
 max_iter <- 100
 fviz_nbclust(as.data.frame(wine_data_pca), kmeans, method = 'gap_stat', iter.max = max_iter)+labs(subtitle = "Gap static Method")

kmeans_pca <- kmeans(wine_data_pca, centers = 2, nstart = 25)
kmeans_pca

# Assign cluster labels to the data
cluster_labels <- kmeans_pca$cluster

# Plot the cluster plot
plot(wine_data_pca[, 1], wine_data_pca[, 2], col = cluster_labels, pch = 19, 
     main = "K-means Clustering")

#plot the clusters
fviz_cluster(kmeans_pca,wine_data_pca, palette = c("#2E9FDF", "#E7B800","#FF0000"), 
             geom = "point", ggtheme = theme_bw())





# Internal evaluation metrics
wss <- kmeans_pca$withinss
bss <- kmeans_pca$betweenss
wss
bss


# Silhouette plot
silhouette_plot <- fviz_silhouette(silhouette(kmeans_pca$cluster, dist(scaled_data_min_max)))
print(silhouette_plot)


# Compute the Calinski-Harabasz Index for different numbers of clusters
distance_matrix <- dist(scaled_data_min_max)
ch_index <- cluster.stats(distance_matrix,kmeans_pca$cluster)$ch

print(ch_index)




