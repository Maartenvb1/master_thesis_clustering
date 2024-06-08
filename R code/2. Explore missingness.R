library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)


# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# File name of cleaned data
file_path <- "data/cleaned_data.xlsx"

# Read the cleaned data once
data <- read_excel(file_path)

# Extract the number of clustering variables and patients
p <- ncol(data) - 4
n <- nrow(data)

# Select only the variables used for clustering
cluster_data <- data %>%
  select(
    -Death_or_Urgent_CV_Hospitalization, 
    -time_to_censoring,
    -VO2_max,
    -EE_high
  )

# Calculate the percentage of missing values per variable
mv_count_variable <- colSums(is.na(cluster_data))
mv_perc_variable  <- round(mv_count_variable / n * 100, 2)
sort(mv_perc_variable, decreasing = TRUE)

# Number of variables with less than 25% of missing values
sum(mv_perc_variable < 25)

# Calculate the percentage of missing values per patient
mv_count_patient <- rowSums(is.na(cluster_data))
mv_perc_patient  <- mv_count_patient / p * 100
mv_perc_patient  <- cut(
  mv_perc_patient, 
  breaks = c(-Inf, 0, 10, 20, 30, 40, 50, 60, 70, 80, +Inf),
  labels = c("None", "1-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", ">80%")
)
round(table(mv_perc_patient) / n * 100, 2)

# Number of complete cases
sum(mv_count_patient == 0)

# Percentage of patients with missing values for no more than two variables
round(sum(mv_count_patient <= 2) / n * 100, 0)

# Select only the outcome variables
outcome_data <- data %>%
  select(
    Death_or_Urgent_CV_Hospitalization, 
    time_to_censoring,
    VO2_max,
    EE_high
  )

# Calculate the number of patients who died or had urgent CV Hospitalization
sum(outcome_data$Death_or_Urgent_CV_Hospitalization)

# Calculate the percentage of missing values per outcome variable
mv_count_variable <- colSums(is.na(outcome_data))
mv_perc_variable  <- round(mv_count_variable / n * 100, 2)
sort(mv_perc_variable, decreasing = TRUE)

# Calculate pairwise complete spearman correlations between all cluster variables
cor_matrix <- cor(
  x      = cluster_data,
  use    = "pairwise.complete.obs",
  method = "spearman"
  )

### VISUALIZATIONS ###

# Filter the data for patients who experienced the event
event_data <- outcome_data[outcome_data$Death_or_Urgent_CV_Hospitalization == 1, ]

# Create a histogram of time to event for these cases
ggplot(event_data, aes(x = time_to_censoring)) +
  geom_histogram(binwidth = 100, fill="Sea Green", color="Sea Green", alpha=0.5) +
  labs(x = "Time to Event (days)",
       y = "Nmber of patients") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 12), breaks = c(0, 3, 6, 9, 12))

# Give the cluster data nicer variable names
names(cluster_data) <- c("Sex", "$FEV[1,pred]", "RER", "$PAPm[rest]", ":E/E[rest]^'´'", "$SBP[high]", "$DBP[high]", "$Watt[high]", "$HR[high]", "$S[RV,high]^'´'", "$S[sept,high]^'´'", "$S[lat,high]^'´'", "$A[lat,high]^'´'", "$TAPSE[high]", "Age", "BSA", "LAVi", "$Papm[high]", "$LVEF[rest]", "$LVEF[high]", "$RVESPAR[high]", "VC", "VE/MVV", "PAPm/CO", "A-V O[2diff]*/Hb", "$H[2]*FPEF")

# Create the correlation plot
pdf("corrplot.pdf", width = 8, height = 8)
corrplot(cor_matrix, method = "ellipse", type = "lower", order = "hclust", hclust.method = 'single',
         tl.col = "black", tl.srt = 45,
         tl.cex = 0.7)
dev.off()

