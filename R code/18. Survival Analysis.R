library(survival)
library(survminer)
library(readxl)

# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# Read the data
data <- read_excel("Data/cleaned_data.xlsx")
results <- read_excel("Data/results.xlsx")

# Access the dynamic column from results
data$clusters <- results$Clusters

# Adjust data for each time point with censoring
data$time_30   <- with(data, pmin(time_to_censoring, 30))
data$status_30 <- with(data, ifelse(time_to_censoring > 30, 0, Death_or_Urgent_CV_Hospitalization))

data$time_60   <- with(data, pmin(time_to_censoring, 60))
data$status_60 <- with(data, ifelse(time_to_censoring > 60, 0, Death_or_Urgent_CV_Hospitalization))

data$time_90   <- with(data, pmin(time_to_censoring, 90))
data$status_90 <- with(data, ifelse(time_to_censoring > 90, 0, Death_or_Urgent_CV_Hospitalization))

data$time_182   <- with(data, pmin(time_to_censoring, 182.5))
data$status_182 <- with(data, ifelse(time_to_censoring > 182.5, 0, Death_or_Urgent_CV_Hospitalization))

data$time_365 <- with(data, pmin(time_to_censoring, 365))
data$status_365 <- with(data, ifelse(time_to_censoring > 365, 0, Death_or_Urgent_CV_Hospitalization))

data$time_730 <- with(data, pmin(time_to_censoring, 730))
data$status_730 <- with(data, ifelse(time_to_censoring > 730, 0, Death_or_Urgent_CV_Hospitalization))

# Create survival objects for each censored dataset
surv_obj_30 <- Surv(time = data$time_30, event = data$status_30)
surv_obj_60 <- Surv(time = data$time_60, event = data$status_60)
surv_obj_90 <- Surv(time = data$time_90, event = data$status_90)
surv_obj_182 <- Surv(time = data$time_182, event = data$status_182)
surv_obj_365 <- Surv(time = data$time_365, event = data$status_365)
surv_obj_730 <- Surv(time = data$time_730, event = data$status_730)

# Fit Kaplan-Meier models for each censored dataset
km_fit_30 <- survfit(surv_obj_30 ~ data$clusters)
km_fit_60 <- survfit(surv_obj_60 ~ data$clusters)
km_fit_90 <- survfit(surv_obj_90 ~ data$clusters)
km_fit_182 <- survfit(surv_obj_182 ~ data$clusters)
km_fit_365 <- survfit(surv_obj_365 ~ data$clusters)
km_fit_730 <- survfit(surv_obj_730 ~ data$clusters)

# Define the labels for the clusters with Roman numerals
cluster_labels <- c("I", "II", "III")

# Custom theme with bold axis titles and legend title
custom_theme <- theme_classic() + 
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

# Plot for km_fit_30
p30 <- ggsurvplot(km_fit_30, data = data, pval = TRUE,
                  ggtheme = custom_theme,
                  xlab = "Time (Days)", ylab = "Cumulative event", fun = "event",
                  legend = c(0.15, 0.7),
                  legend.title = "Cluster",
                  legend.labs = cluster_labels,
                  palette = "Set2")

# Plot for km_fit_60
p60 <- ggsurvplot(km_fit_60, data = data, pval = TRUE,
                  ggtheme = custom_theme,
                  xlab = "Time (Days)", ylab = "Cumulative event", fun = "event",
                  legend = c(0.15, 0.7),
                  legend.title = "Cluster",
                  legend.labs = cluster_labels,
                  palette = "Set2")

# Plot for km_fit_90
p90 <- ggsurvplot(km_fit_90, data = data, pval = TRUE,
                  ggtheme = custom_theme,
                  xlab = "Time (Days)", ylab = "Cumulative event", fun = "event",
                  legend = c(0.15, 0.7),
                  legend.title = "Cluster",
                  legend.labs = cluster_labels,
                  palette = "Set2")

# Plot for km_fit_182
p182 <- ggsurvplot(km_fit_182, data = data, pval = TRUE,
                   ggtheme = custom_theme,
                   xlab = "Time (Days)", ylab = "Cumulative event", fun = "event",
                   legend = c(0.15, 0.7),
                   legend.title = "Cluster",
                   legend.labs = cluster_labels,
                   palette = "Set2")

# Plot for km_fit_365
p365 <- ggsurvplot(km_fit_365, data = data, pval = TRUE,
                   ggtheme = custom_theme,
                   xlab = "Time (Days)", ylab = "Cumulative event", fun = "event",
                   legend = c(0.15, 0.7),
                   legend.title = "Cluster",
                   legend.labs = cluster_labels,
                   palette = "Set2")

# Plot for km_fit_730
p730 <- ggsurvplot(km_fit_730, data = data, pval = TRUE,
                   ggtheme = custom_theme,
                   xlab = "Time (Days)", ylab = "Cumulative event", fun = "event",
                   legend = c(0.15, 0.7),
                   legend.title = "Cluster",
                   legend.labs = cluster_labels,
                   palette = "Set2")

p30
p60
p90
p182
p365
p730


# Save the plots
ggsave("30days.png", p30, dpi = 900, width = 8, height = 4)
ggsave("60days.png", p60, dpi = 900, width = 8, height = 4)
ggsave("90days.png", p90, dpi = 900, width = 8, height = 4)
ggsave("182days.png", p182, dpi = 900, width = 8, height = 4)
ggsave("365days.png", p365, dpi = 900, width = 8, height = 4)
ggsave("730days.png", p730, dpi = 900, width = 8, height = 4)

# Perform log-rank tests for each time point
log_rank_test_30 <- survdiff(surv_obj_30 ~ data$clusters, data = data)
log_rank_test_60 <- survdiff(surv_obj_60 ~ data$clusters, data = data)
log_rank_test_90 <- survdiff(surv_obj_90 ~ data$clusters, data = data)
log_rank_test_182 <- survdiff(surv_obj_182 ~ data$clusters, data = data)
log_rank_test_365 <- survdiff(surv_obj_365 ~ data$clusters, data = data)
log_rank_test_730 <- survdiff(surv_obj_730 ~ data$clusters, data = data)

# Output the test results
print(log_rank_test_30)
print(log_rank_test_60)
print(log_rank_test_90)
print(log_rank_test_182)
print(log_rank_test_365)
print(log_rank_test_730)


