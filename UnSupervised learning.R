library(tidyverse)

# Set the working directory
setwd("C:/Users/xavie/OneDrive/Desktop/MLCW/Data")

# Load the data
data <- read.csv("php0iVrYT.csv")
# Check for missing values
if (any(is.na(data))) {
  data <- na.omit(data)
}

# Preprocess the data if need
scaled_data <- scale(data)

# Set a seed for reproducibility
set.seed(123)

# Perform K-means clustering with varying numbers of clusters 
k_values <- 1:10
ssd <- numeric(length(k_values))

for (k in k_values) {
  kmeans_model <- kmeans(scaled_data, centers = k, nstart = 10)
  ssd[k] <- kmeans_model$tot.withinss
}

# Find the elbow point
diff_ss <- diff(ssd)
elbow_point <- which(diff(diff_ss) < 0)[1] + 1
if (is.na(elbow_point)) {
  # If elbow_point is NA, use a default value
  elbow_point <- 3  # Default value of k = 3
}

# Plot the sum of squared distances against the number of clusters
plot(k_values, ssd, type = "b", xlab = "Number of Clusters", ylab = "Sum of Squared Distances", main = "Elbow Method for Optimal K")
abline(v = elbow_point, col = "red", lty = 2)

# Perform K-means clustering with the optimal number of clusters
kmeans_model <- kmeans(scaled_data, centers = elbow_point, nstart = 10)

# Get cluster assignments for each data point
cluster_assignments <- kmeans_model$cluster

# Convert cluster assignments to a factor for plotting
cluster_factor <- as.factor(cluster_assignments)

# PCA
pca_result <- prcomp(scaled_data, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:2])  # Use the first two principal components for 2D visualization

# Convert cluster_assignments to a data frame with a column named "cluster"
cluster_assignments_df <- data.frame(cluster = cluster_assignments)

# Plot the results
plot(pca_data[,1], pca_data[,2], pch = 19, col = cluster_factor, main = "PCA Visualization", xlab = "Principal Component 1", ylab = "Principal Component 2")
legend("bottomleft", legend = unique(cluster_factor), col = unique(cluster_factor), pch = 19)
plot(tsne_result$Y, pch = 19, col = cluster_factor, main = "t-SNE Visualization", xlab = "t-SNE Dimension 1", ylab = "t-SNE Dimension 2")

# For PCA visualization
pca_data$cluster <- cluster_assignments  
cluster_stats_pca <- pca_data %>%
  group_by(cluster) %>%
  summarize(
    count = n(),
    avg_pc1 = mean(PC1),
    avg_pc2 = mean(PC2),
    min_pc1 = min(PC1),
    max_pc1 = max(PC1),
    min_pc2 = min(PC2),
    max_pc2 = max(PC2),
    range_pc1 = max(PC1) - min(PC1),
    range_pc2 = max(PC2) - min(PC2),
    sd_pc1 = sd(PC1),
    sd_pc2 = sd(PC2)
  )

# Print cluster statistics for PCA
print("Cluster statistics for PCA:")
print(cluster_stats_pca)

