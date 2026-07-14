############################################################
# Marketing Analytics - Customer Segmentation Analysis
# Case Study: Chestnut Ridge Retailer
############################################################

# Task 1: Read and inspect the dataset. Provide a descriptive analysis for each of the variables in the dataset.
# Setting the seed for reproducibility
set.seed(123)  # Important for reproducible research - ensures same results each time code is run

# Loading necessary libraries
library(cluster)    # For clustering algorithms
library(factoextra) # For visualization of clusters
library(NbClust)# For determining optimal number of clusters
library(flexclust)  # For segment profiling
library(ggplot2)    # For data visualization
library(dplyr)      # For data manipulation
library(tidyverse)
# Reading the retailer.csv file
retailer_data <- read.csv("C:\\Users\\parth\\OneDrive\\Desktop\\Exeter\\Optional modules\\Marketing analytics\\Assessment Brief\\retailer.csv")

# Examining the structure of the dataset
str(retailer_data)

# Summary statistics for all variables
summary(retailer_data)

# Checking for missing values
sum(is.na(retailer_data))

# Descriptive statistics for each variable
# Creating a function to calculate descriptive statistics
get_descriptive_stats <- function(x) {
  c(Mean = mean(x), 
    Median = median(x), 
    SD = sd(x), 
    Min = min(x), 
    Max = max(x),
    Q1 = quantile(x, 0.25),
    Q3 = quantile(x, 0.75))
}

# Applying the function to each numeric variable
descriptive_stats <- sapply(retailer_data[, 2:ncol(retailer_data)], get_descriptive_stats)
print(descriptive_stats)

# Visualizing distributions of store attributes
par(mfrow = c(2, 3))
for (i in 2:7) {
  hist(retailer_data[, i], main = colnames(retailer_data)[i], 
       xlab = colnames(retailer_data)[i], col = "lightblue", border = "black")
}

# Visualizing distributions of demographic variables
par(mfrow = c(1, 2))
hist(retailer_data$income, main = "Income Distribution", 
     xlab = "Income (thousands USD)", col = "lightgreen", border = "black")
hist(retailer_data$age, main = "Age Distribution", 
     xlab = "Age (years)", col = "lightgreen", border = "black")

# Task 2: Excluding respondent_id, which variable has the smallest minimum value, and which variable has the largest maximum value in the normalised data set?
# Creating a new data frame without respondent_id
store_attributes <- retailer_data[, 2:7]

# Normalizing the data using z-score standardization
normalized_data <- scale(store_attributes)
summary(normalized_data)

# Finding the variable with the smallest minimum value
min_values <- apply(normalized_data, 2, min)
min_var <- names(which.min(min_values))
cat("Variable with smallest minimum value:", min_var, "with value:", min(min_values), "\n")

# Finding the variable with the largest maximum value
max_values <- apply(normalized_data, 2, max)
max_var <- names(which.max(max_values))
cat("Variable with largest maximum value:", max_var, "with value:", max(max_values), "\n")

# Task 3: Make a new data object for clustering that includes only the store attributes variables
# Creating a new data object with only store attributes
cluster_data <- normalized_data
head(cluster_data)

# Task 4: Calculate the distance using "Euclidean" method
# Computing the distance matrix
dist_matrix <- dist(cluster_data, method = "euclidean")
summary(dist_matrix)

# Task 5: Use set.seed(123) for reproducibility
# Importance of setting the seed:
# Setting the seed ensures that the random processes in clustering algorithms
# produce the same results each time the code is run. This is crucial for:
# 1. Reproducibility: Other researchers can replicate your exact results
# 2. Consistency: Results don't change between runs, allowing for stable analysis
# 3. Debugging: Easier to identify and fix issues when results are consistent
# 4. Scientific integrity: Ensures transparency and validity of findings
# 5. Comparison: Enables fair comparison between different clustering approaches
set.seed(123)  # Setting seed for reproducible results

# Task 6: Run the hierarchical clustering algorithm using method = "ward.D2"
# Performing hierarchical clustering with Ward's method
hc_ward <- hclust(dist_matrix, method = "ward.D2")
cat("\nStructure of the hierarchical clustering result:\n")
str(hc_ward)

# Task 7: Plot the dendogram
# Plotting the dendogram
plot(hc_ward, main = "Hierarchical Clustering Dendrogram", 
     xlab = "Observations", ylab = "Height", hang = -1, cex = 0.6)
rect.hclust(hc_ward, k = 3, border = "red")  # Highlighting 3 clusters
rect.hclust(hc_ward, k = 4, border = "blue") # Highlighting 4 clusters

# Task 8: Divide the data points into 3 clusters by highlighting the clusters in the dendogram
# Cutting the dendogram to get 3 clusters
hc_clusters_3 <- cutree(hc_ward, k = 3)
table(hc_clusters_3)  # Number of observations in each cluster

# Task 9: How many observations are assigned to each cluster?
# Counting observations in each cluster for 3-cluster solution
cluster_counts_3 <- table(hc_clusters_3)
print("Number of observations in each cluster (3-cluster solution):")
print(cluster_counts_3)

# Task 10: Run the k-means clustering algorithm on the normalised data, creating 3 clusters
# Setting parameters for k-means
set.seed(123)  # Setting seed again for reproducibility
kmeans_3 <- kmeans(cluster_data, centers = 3, iter.max = 1000, nstart = 100)

# Examining the k-means clustering results
print("K-means clustering with 3 clusters:")
print(kmeans_3$size)  # Cluster sizes
print(kmeans_3$centers)  # Cluster centers

# Task 11: Proceed with a 4-cluster solution
# Cutting the dendrogram to get 4 clusters
hc_clusters_4 <- cutree(hc_ward, k = 4)
table(hc_clusters_4)  # Number of observations in each cluster

# Task 12: Run the k-means clustering algorithm on the normalised data, creating 4 clusters
# Setting parameters for k-means with 4 clusters
set.seed(123)  # Setting seed again for reproducibility
kmeans_4 <- kmeans(cluster_data, centers = 4, iter.max = 1000, nstart = 100)

# Examining the k-means clustering results
print("K-means clustering with 4 clusters:")
print(kmeans_4$size)  # Cluster sizes
print(kmeans_4$centers)  # Cluster centers

# Task 13: Which solution is better: 3-cluster or 4-cluster? Justify with NbClust function
# Using NbClust to determine the optimal number of clusters
set.seed(123)
nb <- NbClust(cluster_data, min.nc = 2, max.nc = 10, method = "ward.D2")
table(nb$Best.n[1,])

# Calculating silhouette scores for both solutions
sil_3 <- silhouette(kmeans_3$cluster, dist_matrix)
sil_4 <- silhouette(kmeans_4$cluster, dist_matrix)

# Average silhouette width for each solution
avg_sil_3 <- mean(sil_3[, 3])
avg_sil_4 <- mean(sil_4[, 3])

cat("Average silhouette width for 3 clusters:", avg_sil_3, "\n")
cat("Average silhouette width for 4 clusters:", avg_sil_4, "\n")

# Visualizing silhouette plots
par(mfrow = c(1, 2))
plot(sil_3, main = "Silhouette Plot - 3 Clusters", col = 1:3)
plot(sil_4, main = "Silhouette Plot - 4 Clusters", col = 1:4)

# Comparing within-cluster sum of squares
cat("Within-cluster sum of squares for 3 clusters:", kmeans_3$tot.withinss, "\n")
cat("Within-cluster sum of squares for 4 clusters:", kmeans_4$tot.withinss, "\n")

# Based on the analysis, determining the better solution
# Note: This conclusion will be based on the actual results from the analysis

# Task 14: Use the normalised data to calculate the means for each store attribute variable per cluster
# Assuming we proceed with the 4-cluster solution based on our analysis
# Adding cluster assignments to the original data
retailer_with_clusters <- cbind(retailer_data, cluster = kmeans_4$cluster)

# Calculating means for each store attribute by cluster
segment_profiles <- aggregate(store_attributes, by = list(Cluster = kmeans_4$cluster), mean)
print("Segment profiles (mean values for each attribute by cluster):")
print(segment_profiles)

# Using flexcluster to generate a segment profile plot
cluster_obj <- kcca(cluster_data, k = 4, family = kccaFamily("kmeans"))
plot(cluster_obj)

# Analyzing demographic characteristics by segment
demographic_by_segment <- aggregate(retailer_data[, c("income", "age")], 
                                   by = list(Cluster = kmeans_4$cluster), mean)
print("Demographic characteristics by segment:")
print(demographic_by_segment)

# Creating more detailed segment profiles
# Combining store attributes and demographics
full_profiles <- cbind(segment_profiles, demographic_by_segment[, -1])
print("Complete segment profiles:")
print(full_profiles)

# Creating meaningful segment names based on characteristics
# Note: These names will be determined based on the actual results

# Visualizing the segments
# Radar chart for segment profiles
# This visualization helps understand the distinctive characteristics of each segment
library(fmsb)

# Preparing data for radar chart
radar_data <- as.data.frame(t(segment_profiles[, -1]))
colnames(radar_data) <- paste("Segment", 1:4)

# Adding min and max for radar chart scaling
radar_data <- rbind(rep(5, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)

# Creating the radar chart
par(mfrow = c(1, 1))
radarchart(radar_data, 
           pcol = rainbow(4), 
           plwd = 2, 
           plty = 1,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, 5, 1), 
           cglwd = 0.8,
           title = "Segment Profiles - Store Attributes")
legend("topright", 
       legend = paste("Segment", 1:4), 
       col = rainbow(4), 
       lty = 1, 
       lwd = 2, 
       cex = 0.8)

# Task 15: GE Matrix Analysis for Segment Attractiveness
# Creating a function to evaluate segment attractiveness
evaluate_segment_attractiveness <- function(segment_data) {
  # Criteria for segment attractiveness:
  # 1. Size (number of customers)
  # 2. Growth potential (based on demographics)
  # 3. Profitability (based on income and preferences)
  
  # Placeholder for actual calculations
  # In a real analysis, these would be based on the data
  
  # Return attractiveness score
  return(list(
    size_score = nrow(segment_data) / 200,  # Normalized by total sample size
    growth_score = mean(segment_data$age < 30) * 0.7 + mean(segment_data$income > 25) * 0.3,
    profit_score = mean(segment_data$income) / 50  # Normalized by maximum income
  ))
}

# Evaluating each segment
segment_attractiveness <- list()
for (i in 1:4) {
  segment_data <- retailer_data[kmeans_4$cluster == i, ]
  segment_attractiveness[[i]] <- evaluate_segment_attractiveness(segment_data)
}

# Calculating overall attractiveness scores
overall_scores <- sapply(segment_attractiveness, function(x) {
  0.4 * x$size_score + 0.3 * x$growth_score + 0.3 * x$profit_score
})

# Displaying segment attractiveness scores
print("Segment Attractiveness Scores:")
for (i in 1:4) {
  cat("Segment", i, ":", overall_scores[i], "\n")
}

# Visualizing the GE matrix
# This would typically be a 3x3 grid showing segment positioning
# For simplicity, we'll create a scatter plot of two key dimensions

# Creating a data frame for the GE matrix visualization
ge_matrix <- data.frame(
  Segment = 1:4,
  Attractiveness = overall_scores,
  CompanyStrength = c(0.7, 0.5, 0.8, 0.4)  # Placeholder values
)

# Plotting the GE matrix
ggplot(ge_matrix, aes(x = CompanyStrength, y = Attractiveness, label = Segment)) +
  geom_point(aes(color = factor(Segment)), size = 10) +
  geom_text(color = "white") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  geom_vline(xintercept = c(0.33, 0.67), linetype = "dashed", color = "gray") +
  geom_hline(yintercept = c(0.33, 0.67), linetype = "dashed", color = "gray") +
  labs(title = "GE Matrix for Segment Attractiveness",
       x = "Company Strength",
       y = "Market Attractiveness") +
  theme_minimal() +
  theme(legend.position = "none")

# Saving key outputs for the report
# Segment profiles
write.csv(full_profiles, "segment_profiles.csv", row.names = FALSE)

# Cluster visualization
png("dendrogram.png", width = 800, height = 600)
plot(hc_ward, main = "Hierarchical Clustering Dendrogram", 
     xlab = "Observations", ylab = "Height", hang = -1, cex = 0.6)
rect.hclust(hc_ward, k = 4, border = "blue")
dev.off()

# Silhouette plot for chosen solution
png("silhouette_4_clusters.png", width = 800, height = 600)
plot(sil_4, main = "Silhouette Plot - 4 Clusters", col = 1:4)
dev.off()

# Radar chart for segment profiles
png("radar_chart_4_clusters.png", width = 800, height = 600)
radarchart(radar_data, 
           pcol = rainbow(4), 
           plwd = 2, 
           plty = 1,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, 5, 1), 
           cglwd = 0.8,
           title = "Segment Profiles - Store Attributes")
legend("topright", 
       legend = paste("Segment", 1:4), 
       col = rainbow(4), 
       lty = 1, 
       lwd = 2, 
       cex = 0.8)
dev.off()

# GE Matrix
png("ge_matrix.png", width = 800, height = 600)
print(ggplot(ge_matrix, aes(x = CompanyStrength, y = Attractiveness, label = Segment)) +
  geom_point(aes(color = factor(Segment)), size = 10) +
  geom_text(color = "white") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  geom_vline(xintercept = c(0.33, 0.67), linetype = "dashed", color = "gray") +
  geom_hline(yintercept = c(0.33, 0.67), linetype = "dashed", color = "gray") +
  labs(title = "GE Matrix for Segment Attractiveness",
       x = "Company Strength",
       y = "Market Attractiveness") +
  theme_minimal() +
  theme(legend.position = "none"))
dev.off()

# Final summary of findings
cat("\n\n=== FINAL SUMMARY ===\n")
cat("Number of segments identified:", 4, "\n")
cat("Segment sizes:", kmeans_4$size, "\n")
cat("Most attractive segment:", which.max(overall_scores), "\n")
cat("Recommended target segments:", paste(order(overall_scores, decreasing = TRUE)[1:2], collapse = ", "), "\n")
