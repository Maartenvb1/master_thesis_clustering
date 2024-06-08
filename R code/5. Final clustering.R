library(dplyr)
library(readxl)
library(writexl)
library(mice)
library(cluster)
library(flexclust)
library(mclust)
library(dendextend)
library(ggplot2)
library(RColorBrewer)


M        <- 50
K_final  <- 3
n_start  <- 50
iter_max <- 50
seed     <- 314
n        <- 511

# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# Read the data
data <- read_excel("Data/cleaned_data.xlsx")

# Select the data for clustering
cluster_data <- data %>%
  select(
    -Death_or_Urgent_CV_Hospitalization,
    -time_to_censoring,
    -VO2_max,
    -EE_high,
    -H2FPEF
  )

# Perform multiple imputation
imputed_data      <- mice(cluster_data, m=M, method='pmm', seed=seed, print=TRUE, maxit = 100)
complete_datasets <- lapply(1:M, function(i) scale(complete(imputed_data, action = i)))

# Define the PAM clustering function with Euclidean distance
cust_pam_eucl <- function(x) {
  return(pam(x, K_final, nstart=n_start, cluster.only=TRUE, metric="euclidean"))
}

# Define the PAM clustering function with Manhattan distance
cust_pam_manh <- function(x) {
  return(pam(x, K_final, nstart=n_start, cluster.only=TRUE, metric="manhattan"))
}

# Define the K-means clustering function
cust_kmeans <- function(x) {
  result <- kmeans(x, K_final, nstart=n_start, iter.max = iter_max)
  return(result$cluster)
}

# Define the K-medians clustering function
cust_kmedians <- function(x) {
  best_result <- NULL
  best_totDist <- Inf
  
  for (i in 1:n_start) {
    result <- kcca(x, k = K_final, family = kccaFamily("kmedians"))
    totDist <- sum(result@cldist)
    if (totDist < best_totDist) {
      best_result <- result
      best_totDist <- totDist
    }
  }
  return(clusters(best_result))
}

hclusCut <- function(x)
  return(cutree(hclust(dist(x, method="euclidean"), method = "ward.D2"), k=K_final))

# Define function for meta clustering
meta_clustering <- function(assignments) {
  co_occurrence_matrix <- matrix(0, n, n)
  diag(co_occurrence_matrix) <- M
  
  # Fill the co-occurrence matrix
  for (assignment in assignments) {
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        if (assignment[i] == assignment[j]) {
          co_occurrence_matrix[i, j] <- co_occurrence_matrix[i, j] + 1
          co_occurrence_matrix[j, i] <- co_occurrence_matrix[j, i] + 1
        }
      }
    }
  }
  
  # Perform hierarchical clustering on the co-occurrence matrix
  hc <- hclust(as.dist(M - co_occurrence_matrix), method = "average")
  
  # Cut the tree at the desired number of clusters
  consensus_clusters <- cutree(hc, k = K_final)
  
  # Initialize a vector to store ARI values
  ari_values <- numeric(M)
  
  # Calculate ARI for each clustering assignment compared to the consensus
  for (i in 1:M) {
    ari_values[i] <- adjustedRandIndex(consensus_clusters, assignments[[i]])
  }
  
  # Calculate the sum of co-occurrences within clusters
  calculate_co_occurrence_sum <- function(clusters, co_occurrence_matrix) {
    co_occurrence_sum <- 0
    unique_clusters <- unique(clusters)
    for (cluster in unique_clusters) {
      cluster_indices <- which(clusters == cluster)
      cluster_matrix <- co_occurrence_matrix[cluster_indices, cluster_indices]
      co_occurrence_sum <- co_occurrence_sum + sum(cluster_matrix) / 2
    }
    return(co_occurrence_sum)
  }
  
  co_occurrence_sum <- calculate_co_occurrence_sum(consensus_clusters, co_occurrence_matrix)
  
  return(list(co_occurrence_matrix = co_occurrence_matrix, hc = hc, consensus_clusters = consensus_clusters, ari_values = ari_values, co_occurrence_sum = co_occurrence_sum))
}

# Apply the PAM clustering function to each dataset in the list
assignments_pam_eucl <- lapply(complete_datasets, cust_pam_eucl)
results_pam_eucl     <- meta_clustering(assignments_pam_eucl)

assignments_kmeans   <- lapply(complete_datasets, cust_kmeans)
results_kmeans       <- meta_clustering(assignments_kmeans)

assignments_hc       <- lapply(complete_datasets, hclusCut)
results_hc           <- meta_clustering(assignments_hc)

assignments_pam_manh <- lapply(complete_datasets, cust_pam_manh)
results_pam_manh     <- meta_clustering(assignments_pam_manh)

assignments_kmedians <- lapply(complete_datasets, cust_kmedians)
results_kmedians     <- meta_clustering(assignments_kmedians)

adjustedRandIndex(results_pam_eucl$consensus_clusters, results_kmeans$consensus_clusters)
adjustedRandIndex(results_pam_eucl$consensus_clusters, results_pam_manh$consensus_clusters)
adjustedRandIndex(results_pam_eucl$consensus_clusters, results_kmedians$consensus_clusters)
adjustedRandIndex(results_pam_eucl$consensus_clusters, results_hc$consensus_clusters)
adjustedRandIndex(results_kmeans$consensus_clusters,   results_pam_manh$consensus_clusters)
adjustedRandIndex(results_kmeans$consensus_clusters,   results_kmedians$consensus_clusters)
adjustedRandIndex(results_kmeans$consensus_clusters,   results_hc$consensus_clusters)
adjustedRandIndex(results_pam_manh$consensus_clusters, results_kmedians$consensus_clusters)
adjustedRandIndex(results_pam_manh$consensus_clusters, results_hc$consensus_clusters)
adjustedRandIndex(results_kmedians$consensus_clusters, results_hc$consensus_clusters)

mean(results_pam_eucl$ari_values)
mean(results_kmeans$ari_values)
mean(results_pam_manh$ari_values)
mean(results_kmedians$ari_values)
mean(results_hc$ari_values)

results_pam_eucl$co_occurrence_sum
results_kmeans$co_occurrence_sum
results_pam_manh$co_occurrence_sum
results_kmedians$co_occurrence_sum
results_hc$co_occurrence_sum


### VISUALIZATION ###

# Convert to dendrogram
dend <- as.dendrogram(results_pam_eucl$hc, hang = 0.1)
dend <- set(dend, "labels", value = NA)
dend <- set(dend, "branches_lwd", 0.25)
dend <- color_branches(dend, k = K_final, col = brewer.pal(3, "Set2"), groupLabels = as.roman)
pdf("Figures/Results/MI analysis/pam_eucl_dend.pdf", width = 10, height = 8)
plot(dend, ylim = c(0, M))
dev.off()

dend <- as.dendrogram(results_kmeans$hc, hang = 0.1)
dend <- set(dend, "labels", value = NA)
dend <- set(dend, "branches_lwd", 0.25)
dend <- color_branches(dend, k = K_final, col = brewer.pal(3, "Set2"), groupLabels = as.roman)
pdf("Figures/Results/MI analysis/kmeans_dend.pdf", width = 10, height = 8)
plot(dend, ylim = c(0, M))
dev.off()

dend <- as.dendrogram(results_pam_manh$hc, hang = 0.1)
dend <- set(dend, "labels", value = NA)
dend <- set(dend, "branches_lwd", 0.25)
dend <- color_branches(dend, k = K_final, col = brewer.pal(3, "Set2"), groupLabels = as.roman)
pdf("Figures/Results/MI analysis/pam_manh_dend.pdf", width = 10, height = 8)
plot(dend, ylim = c(0, M))
dev.off()

dend <- as.dendrogram(results_kmedians$hc, hang = 0.1)
dend <- set(dend, "labels", value = NA)
dend <- set(dend, "branches_lwd", 0.25)
dend <- color_branches(dend, k = K_final, col = brewer.pal(3, "Set2"), groupLabels = as.roman)
pdf("Figures/Results/MI analysis/kmedians_dend.pdf", width = 10, height = 8)
plot(dend, ylim = c(0, M))
dev.off()

dend <- as.dendrogram(results_hc$hc, hang = 0.1)
dend <- set(dend, "labels", value = NA)
dend <- set(dend, "branches_lwd", 0.25)
dend <- color_branches(dend, k = K_final, col = brewer.pal(3, "Set2"), groupLabels = as.roman)
pdf("Figures/Results/MI analysis/hc_dend.pdf", width = 10, height = 8)
plot(dend, ylim = c(0, M))
dev.off()


custom_colors <- brewer.pal(8, "Set2")

# Create the histogram for PAM with Euclidean distance metric
ggplot(data.frame(ari_values = results_pam_eucl$ari_values), aes(x = ari_values)) +
  geom_histogram(binwidth = 0.01, fill = custom_colors[6], color = "black", alpha=0.75) +
  scale_x_continuous(limits = c(0.25, 1)) +
  scale_y_continuous(breaks = seq(2, 10, by = 2), limits = c(0, 9)) + 
  labs(title = "", x = "Ajusted Rand Index", y = "Frequency") +
  theme_classic() + theme(
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))

ggsave("Figures/Results/MI analysis/hist_pam_eucl_ari.png", width = 8, height = 6, dpi = 900)

# Create the histogram for K-means
ggplot(data.frame(ari_values = results_kmeans$ari_values), aes(x = ari_values)) +
  geom_histogram(binwidth = 0.01, fill = custom_colors[5], color = "black", alpha=0.75) +
  scale_x_continuous(limits = c(0.25, 1)) +
  scale_y_continuous(breaks = seq(2, 10, by = 2), limits = c(0, 9)) + 
  labs(title = "", x = "Ajusted Rand Index", y = "Frequency") +
  theme_classic() + theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))

ggsave("Figures/Results/MI analysis/hist_kmeans_ari.png", width = 8, height = 6, dpi = 900)

# Create the histogram for PAM with Manhattan distance metric
ggplot(data.frame(ari_values = results_pam_manh$ari_values), aes(x = ari_values)) +
  geom_histogram(binwidth = 0.01, fill = custom_colors[8], color = "black", alpha=0.75) +
  scale_x_continuous(limits = c(0.25, 1)) +
  scale_y_continuous(breaks = seq(2, 10, by = 2), limits = c(0, 9)) + 
  labs(title = "", x = "Ajusted Rand Index", y = "Frequency") +
  theme_classic() + theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))

ggsave("Figures/Results/MI analysis/hist_pam_manh_ari.png", width = 8, height = 6, dpi = 900)

# Create the histogram for K-medians
ggplot(data.frame(ari_values = results_kmedians$ari_values), aes(x = ari_values)) +
  geom_histogram(binwidth = 0.01, fill = custom_colors[7], color = "black", alpha=0.75) +
  scale_x_continuous(limits = c(0.25, 1)) +
  scale_y_continuous(breaks = seq(2, 10, by = 2), limits = c(0, 9)) + 
  labs(title = "", x = "Ajusted Rand Index", y = "Frequency") +
  theme_classic() + theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))

ggsave("Figures/Results/MI analysis/hist_kmedians_ari.png", width = 8, height = 6, dpi = 900)

# Create the histogram for hc
ggplot(data.frame(ari_values = results_hc$ari_values), aes(x = ari_values)) +
  geom_histogram(binwidth = 0.01, fill = custom_colors[4], color = "black", alpha=0.75) +
  scale_x_continuous(limits = c(0.25, 1)) +
  scale_y_continuous(breaks = seq(2, 10, by = 2), limits = c(0, 9)) + 
  labs(title = "", x = "Ajusted Rand Index", y = "Frequency") +
  theme_classic() + theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))

ggsave("Figures/Results/MI analysis/hist_hc_ari.png", width = 8, height = 6, dpi = 900)


# Choose K-means as final result and store the cluster assignments
results <- data
results$Clusters <- results_kmeans$consensus_clusters

results <- results %>%
  mutate(Clusters = case_when(
    Clusters == 1 ~ 2,
    Clusters == 2 ~ 1,
    TRUE ~ Clusters
  ))


write_xlsx(results, path = "Data/Results.xlsx")
