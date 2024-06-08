library(readxl)
library(dplyr)
library(cluster)
library(flexclust)
library(RColorBrewer)
library(ggplot2)
library(mclust)
library(dbscan)


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

# Define parameters
B        <- 500
K_max    <- 10
n_start  <- 50
iter_max <- 50
seed     <- 314
d.power  <- 2

# Set seed for reproducability
set.seed(seed)

# Select the complete cases and scale
cc_data   <- cluster_data[complete.cases(cluster_data), ]
cc_scaled <- scale(cc_data)

# Define custom PAM function that only returns the cluster assignments
cust_pam <- function(x, k, ...)
  list(cluster = pam(x,k, cluster.only=TRUE, medoids = "random", pamonce = 5, ...))

# Define hc clustering function together with cutting the tree
hclusCut <- function(x, k, d.meth = "euclidean", ...)
  list(cluster = cutree(hclust(dist(x, method=d.meth), ...), k=k))

# Define Kmedians clustering with restarts
kmedians <- function(x, k, iter.max = iter_max, nstart = n_start, ...) {
  best_result <- NULL
  best_totDist <- Inf
  
  for (i in 1:nstart) {
    result <- kcca(x, k = k, family = kccaFamily("kmedians"))
    totDist <- sum(result@cldist)
    if (totDist < best_totDist) {
      best_result <- result
      best_totDist <- totDist
    }
  }
  list(cluster = clusters(best_result))
}

# Calculate the Gap statistic for different algorithms
gs_eucl_pam      <- clusGap(cc_scaled, cust_pam, K_max, B = B, spaceH0 = "original", d.power = d.power, metric = "euclidean", nstart = n_start)
gs_eucl_kmeans   <- clusGap(cc_scaled, kmeans,   K_max, B = B, spaceH0 = "original", d.power = d.power, nstart = n_start, iter.max = iter_max)
gs_eucl_hc_ward  <- clusGap(cc_scaled, hclusCut, K_max, B = B, spaceH0 = "original", d.power = d.power, d.meth = "euclidean", method = 'ward.D2')
gs_manh_pam      <- clusGap(cc_scaled, cust_pam, K_max, B = B, spaceH0 = "original", d.power = d.power, metric = "manhattan", nstart = n_start)
gs_manh_kmedians <- clusGap(cc_scaled, kmedians, K_max, B = B, spaceH0 = "original", d.power = d.power, nstart = n_start)

# Results for PAM with Euclidean distance
maxSE(gs_eucl_pam$Tab[,"gap"], gs_eucl_pam$Tab[,"SE.sim"], method = "Tibs2001SEmax")
maxSE(gs_eucl_pam$Tab[,"gap"], gs_eucl_pam$Tab[,"SE.sim"], method = "firstSEmax")
maxSE(gs_eucl_pam$Tab[,"gap"], gs_eucl_pam$Tab[,"SE.sim"], method = "firstmax")

# Results for K-means
maxSE(gs_eucl_kmeans$Tab[,"gap"], gs_eucl_kmeans$Tab[,"SE.sim"], method = "Tibs2001SEmax")
maxSE(gs_eucl_kmeans$Tab[,"gap"], gs_eucl_kmeans$Tab[,"SE.sim"], method = "firstSEmax")
maxSE(gs_eucl_kmeans$Tab[,"gap"], gs_eucl_kmeans$Tab[,"SE.sim"], method = "firstmax")

# Results for hierarchical clustering with Euclidean distance and Ward linkage
maxSE(gs_eucl_hc_ward$Tab[,"gap"], gs_eucl_hc_ward$Tab[,"SE.sim"], method = "Tibs2001SEmax")
maxSE(gs_eucl_hc_ward$Tab[,"gap"], gs_eucl_hc_ward$Tab[,"SE.sim"], method = "firstSEmax")
maxSE(gs_eucl_hc_ward$Tab[,"gap"], gs_eucl_hc_ward$Tab[,"SE.sim"], method = "firstmax")

# Results for PAM with Manhattan distance
maxSE(gs_manh_pam$Tab[,"gap"], gs_manh_pam$Tab[,"SE.sim"], method = "Tibs2001SEmax")
maxSE(gs_manh_pam$Tab[,"gap"], gs_manh_pam$Tab[,"SE.sim"], method = "firstSEmax")
maxSE(gs_manh_pam$Tab[,"gap"], gs_manh_pam$Tab[,"SE.sim"], method = "firstmax")

# Results for K-medians
maxSE(gs_manh_kmedians$Tab[,"gap"], gs_manh_kmedians$Tab[,"SE.sim"], method = "Tibs2001SEmax")
maxSE(gs_manh_kmedians$Tab[,"gap"], gs_manh_kmedians$Tab[,"SE.sim"], method = "firstSEmax")
maxSE(gs_manh_kmedians$Tab[,"gap"], gs_manh_kmedians$Tab[,"SE.sim"], method = "firstmax")

# Chosen number of clusters
K_final = 3

# Define the clusters
clusters_eucl_pam <- cust_pam(cc_scaled, K_final, metric = "euclidean", nstart = n_start)$cluster
clusters_kmeans   <- kmeans(cc_scaled,   K_final, nstart = n_start, iter.max = iter_max)$cluster
clusters_hc_ward  <- hclusCut(cc_scaled, K_final, d.meth = "euclidean", method = 'ward.D2')$cluster
clusters_manh_pam <- cust_pam(cc_scaled, K_final, metric = "manhattan", nstart = n_start)$cluster
clusters_kmedians <- kmedians(cc_scaled, K_final, nstart = n_start, iter.max = iter_max)$cluster

# Compare the results of the different algorithms
adjustedRandIndex(clusters_eucl_pam, clusters_kmeans)
adjustedRandIndex(clusters_eucl_pam, clusters_hc_ward)
adjustedRandIndex(clusters_eucl_pam, clusters_manh_pam)
adjustedRandIndex(clusters_eucl_pam, clusters_kmedians)
adjustedRandIndex(clusters_kmeans,   clusters_hc_ward)
adjustedRandIndex(clusters_kmeans,   clusters_manh_pam)
adjustedRandIndex(clusters_kmeans,   clusters_kmedians)
adjustedRandIndex(clusters_hc_ward,  clusters_manh_pam)
adjustedRandIndex(clusters_hc_ward,  clusters_kmedians)
adjustedRandIndex(clusters_manh_pam, clusters_kmedians)


### DBSCAN ###

# Define a range of possible eps values
eps_range <- seq(0.1, 50, by = 0.25)

# Function to perform DBSCAN and collect results
perform_dbscan <- function(minPts) {
  noise_counts <- numeric(length(eps_range))
  cluster_counts <- numeric(length(eps_range))
  
  for (i in seq_along(eps_range)) {
    eps_value <- eps_range[i]
    dbscan_result <- dbscan(cc_scaled, eps = eps_value, minPts = minPts)
    noise_counts[i] <- sum(dbscan_result$cluster == 0)
    cluster_counts[i] <- max(dbscan_result$cluster)
  }
  
  results_df <- data.frame(eps = eps_range, clusters = cluster_counts, noise = noise_counts)
  return(results_df)
}

# Perform DBSCAN for different minPts values
perform_dbscan(minPts = 10)
perform_dbscan(minPts = 15)
perform_dbscan(minPts = 25)


### VISUALIZATION ###

# Prepare data for visualization
gs_eucl_pam <- data.frame("Gap" = gs_eucl_pam$Tab[,"gap"], "SE" = gs_eucl_pam$Tab[,"SE.sim"])
gs_eucl_pam <- transform(gs_eucl_pam, Upper = Gap + SE, Lower = Gap - SE, Method = "PAM", k = 1:K_max)

gs_eucl_kmeans <- data.frame("Gap" = gs_eucl_kmeans$Tab[,"gap"], "SE" = gs_eucl_kmeans$Tab[,"SE.sim"])
gs_eucl_kmeans <- transform(gs_eucl_kmeans, Upper = Gap + SE, Lower = Gap - SE, Method = "K-means", k = 1:K_max)

gs_eucl_hc_ward <- data.frame("Gap" = gs_eucl_hc_ward$Tab[,"gap"], "SE" = gs_eucl_hc_ward$Tab[,"SE.sim"])
gs_eucl_hc_ward <- transform(gs_eucl_hc_ward, Upper = Gap + SE, Lower = Gap - SE, Method = "Hierarchical - Ward", k = 1:K_max)

gs_manh_pam <- data.frame("Gap" = gs_manh_pam$Tab[,"gap"], "SE" = gs_manh_pam$Tab[,"SE.sim"])
gs_manh_pam <- transform(gs_manh_pam, Upper = Gap + SE, Lower = Gap - SE, Method = "PAM", k = 1:K_max)

gs_manh_kmedians <- data.frame("Gap" = gs_manh_kmedians$Tab[,"gap"], "SE" = gs_manh_kmedians$Tab[,"SE.sim"])
gs_manh_kmedians <- transform(gs_manh_kmedians, Upper = Gap + SE, Lower = Gap - SE, Method = "K-medians", k = 1:K_max)

# Extract colors from Set2 palette
set2_colors <- brewer.pal(8, "Set2")

# Combine the results for Euclidean
results_Euclidean <- rbind(gs_eucl_pam, gs_eucl_kmeans, gs_eucl_hc_ward)

# Plot the results for Euclidean
p1 <- ggplot(results_Euclidean, aes(x = k, y = Gap, color = Method)) +
  geom_line(linewidth = 0.5, alpha = 0.75) +
  geom_point(size = 2, alpha = 0.75) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, alpha = 0.75) + 
  labs(x = "Number of clusters (k)", y = "Gap Statistic", title = "") +
  scale_x_continuous(breaks = 1:K_max) +
  scale_color_manual(values = set2_colors[4:6]) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

p1

# Combine the results for Manhattan
results_Manhattan <- rbind(gs_manh_pam, gs_manh_kmedians)

# Plot the results for Manhattan
p2 <- ggplot(results_Manhattan, aes(x = k, y = Gap, color = Method)) +
  geom_line(linewidth = 0.5, alpha = 0.75) +
  geom_point(size = 2, alpha = 0.75) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, alpha = 0.75) + 
  labs(x = "Number of clusters (k)", y = "Gap Statistic", title = "") +
  scale_x_continuous(breaks = 1:K_max) +
  scale_color_manual(values = set2_colors[7:8]) +
  theme_classic() +
  theme(
    axis.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

p2

hclust_result <- hclust(dist(cluster_data, method = "euclidean"), method = 'ward.D2')
plot(hclust_result, labels = FALSE, main = "", xlab = "", ylab = "", sub = "")

# Save the plots
ggsave("Figures/Results/Complete case analysis/CCEuclidean.png", p1, dpi = 900, width = 9, height = 9/2)
ggsave("Figures/Results/Complete case analysis/CCManhattan.png", p2, dpi = 900, width = 9, height = 9/2)
pdf("Figures/Results/Complete case analysis/CCdend.pdf", width = 10, height = 8)
plot(hclust_result, labels = FALSE, main = "", xlab = "", ylab = "", sub = "")
dev.off()
