library(readxl)
library(writexl)
library(dplyr)
library(mice)
library(cluster)
library(ggplot2)
library(flexclust)
library(RColorBrewer)

# Define parameters used
M        <- 50
B        <- 250
K_max    <- 10
n_start  <- 50
seed     <- 314
iter_max <- 50

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

# Create several imputed datasets with multiple imputation
imputed_data  <- mice(cluster_data, m=M, method='pmm', seed=seed, print=TRUE, maxit = 100)
complete_data <- lapply(1:M, function(i) complete(imputed_data, action = i))

# Define custom PAM function that only returns the cluster assignments
cust_pam <- function(x, k, ...)
  list(cluster = pam(x, k, cluster.only=TRUE, medoids = "random", pamonce = 5, nstart = n_start, ...))

# Define hc clustering function together with cutting the tree
hclusCut <- function(x, k)
  list(cluster = cutree(hclust(dist(x, method="euclidean"), method = 'ward.D2'), k=k))

# Define Kmedians clustering with restarts
kmedians <- function(x, k) {
  best_result <- NULL
  best_totDist <- Inf
  
  for (i in 1:15) {
    result <- kcca(x, k = k, family = kccaFamily("kmedians"))
    totDist <- sum(result@cldist)
    if (totDist < best_totDist) {
      best_result <- result
      best_totDist <- totDist
    }
  }
  list(cluster = clusters(best_result))
}

# Define function to aggregate gap statistic results
aggregate_gap_stats <- function(method, metric) {
  gs <- list()
  opt_k_tibs       <- numeric(M)
  opt_k_firstmax   <- numeric(M)
  opt_k_firstSEmax <- numeric(M)
  
  for (m in 1:M) {
    print(paste("Processing started for imputation number", m, "..."))
    
    centered_data <- scale(complete_data[[m]])
    
    if (method == "pam") {
      gs[[m]] <- clusGap(centered_data, cust_pam, K_max, B, 2, spaceH0 = "original", metric = metric)
    } else if (method == "hc_ward") {
      gs[[m]] <- clusGap(centered_data, hclusCut, K_max, B, 2, spaceH0 = "original")
    } else if (method == "kmeans") {
      gs[[m]] <- clusGap(centered_data, kmeans,   K_max, 500, 2, spaceH0 = "original", nstart = n_start, iter.max = iter_max)
    } else {
      gs[[m]] <- clusGap(centered_data, kmedians, K_max, 50, 2, spaceH0 = "original")
    }
    
    write_xlsx(as.data.frame(gs[[m]]$Tab), path = paste(metric, "/", method, "_", m, ".xlsx", sep=""))

    opt_k_tibs[m]       <- maxSE(gs[[m]]$Tab[,"gap"], gs[[m]]$Tab[,"SE.sim"], method = "Tibs2001SEmax")
    opt_k_firstmax[m]   <- maxSE(gs[[m]]$Tab[,"gap"], gs[[m]]$Tab[,"SE.sim"], method = "firstmax")
    opt_k_firstSEmax[m] <- maxSE(gs[[m]]$Tab[,"gap"], gs[[m]]$Tab[,"SE.sim"], method = "firstSEmax")
  }
  
  combined_gaps <- numeric(K_max)
  combined_ses  <- numeric(K_max)
  
  for (k in 1:K_max) {
    gaps <- sapply(gs, function(x) x$Tab[k,"gap"])
    ses  <- sapply(gs, function(x) x$Tab[k,"SE.sim"])
    
    # Rubin's Rules for Gap Statistics
    Q_bar <- mean(gaps)
    U_bar <- mean(ses^2)
    B     <- var(gaps)
    TV    <- U_bar + ((1 + 1 / M) * B)
    
    combined_gaps[k] <- Q_bar
    combined_ses[k]  <- sqrt(TV)
  }

  aggr_opt_k_tibs       <- maxSE(combined_gaps, combined_ses, method = "Tibs2001SEmax")
  aggr_opt_k_firstmax   <- maxSE(combined_gaps, combined_ses, method = "firstmax")
  aggr_opt_k_firstSEmax <- maxSE(combined_gaps, combined_ses, method = "firstSEmax")
  
  return(list("Gap"                   = combined_gaps,
              "SE"                    = combined_ses, 
              "opt_k_tibs"            = opt_k_tibs,
              "opt_k_firstmax"        = opt_k_firstmax,
              "opt_k_firstSEmax"      = opt_k_firstSEmax,
              "aggr_opt_k_tibs"       = aggr_opt_k_tibs,
              "aggr_opt_k_firstmax"   = aggr_opt_k_firstmax,
              "aggr_opt_k_firstSEmax" = aggr_opt_k_firstSEmax
              ))
}

# Calculate the optimal number for each method
results_pam_eucl <- aggregate_gap_stats("pam",      "euclidean")
results_pam_manh <- aggregate_gap_stats("pam",      "manhattan")
results_kmeans   <- aggregate_gap_stats("kmeans",   "euclidean")
table(results_kmeans$opt_k_firstmax)
results_kmedians <- aggregate_gap_stats("kmedians", "manhattan")
results_hc       <- aggregate_gap_stats("hc_ward",  "euclidean")

# Display the results
results_pam_eucl
results_pam_manh
results_kmeans
results_kmedians
results_hc

table(results_pam_eucl$aggr_opt_k_tibs)
table(results_kmeans$aggr_opt_k_tibs)
table(results_hc$aggr_opt_k_tibs)
table(results_pam_manh$aggr_opt_k_tibs)
table(results_kmedians$aggr_opt_k_tibs)



### VISUALIZATION ###

# Prepare data for visualization
gs_eucl_pam <- data.frame("Gap" = results_pam_eucl$Gap, "SE" = results_pam_eucl$SE)
gs_eucl_pam <- transform(gs_eucl_pam, Upper = Gap + SE, Lower = Gap - SE, Method = "PAM", k = 1:K_max)

gs_eucl_kmeans <- data.frame("Gap" = results_kmeans$Gap, "SE" = results_kmeans$SE)
gs_eucl_kmeans <- transform(gs_eucl_kmeans, Upper = Gap + SE, Lower = Gap - SE, Method = "K-means", k = 1:K_max)

gs_eucl_hc_ward <- data.frame("Gap" = results_hc$Gap, "SE" = results_hc$SE)
gs_eucl_hc_ward <- transform(gs_eucl_hc_ward, Upper = Gap + SE, Lower = Gap - SE, Method = "Hierarchical - Ward", k = 1:K_max)

gs_manh_pam <- data.frame("Gap" = results_pam_manh$Gap, "SE" = results_pam_manh$SE)
gs_manh_pam <- transform(gs_manh_pam, Upper = Gap + SE, Lower = Gap - SE, Method = "PAM", k = 1:K_max)

gs_manh_kmedians <- data.frame("Gap" = results_kmedians$Gap, "SE" = results_kmedians$SE)
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
  scale_y_continuous(limits = c(1.45, 1.62)) +
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
  scale_y_continuous(limits = c(1.45, 1.62)) +
  scale_color_manual(values = set2_colors[7:8]) +
  theme_classic() +
  theme(
    axis.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

p2


# Save the plots
ggsave("Figures/Results/MI analysis/MIEuclidean.png", p1, dpi = 900, width = 9, height = 9/2)
ggsave("Figures/Results/MI analysis/MIManhattan.png", p2, dpi = 900, width = 9, height = 9/2)