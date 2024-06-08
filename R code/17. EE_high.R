library(readxl)

# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# Read the data
data <- read_excel("Data/cleaned_data.xlsx")
results <- read_excel("Data/results.xlsx")

# Access the dynamic column from results and merge it with data
data$Cluster <- as.factor(results$Clusters)

data$Cluster <- as.character(data$Cluster)  # Convert to character to allow replacement
data$Cluster[data$Cluster == "1"] <- "I"
data$Cluster[data$Cluster == "2"] <- "II"
data$Cluster[data$Cluster == "3"] <- "III"

# Add color information based on EE_high values
data$point_color <- ifelse(data$EE_high >= 15, "red", 
                           ifelse(data$EE_high >= 13, "orange", "black"))

# Define custom colors for y-axis labels
y_labels <- c(5, 10, 13, 15, 20, 25, 30)
y_colors <- ifelse(y_labels == 13, "orange", ifelse(y_labels == 15, "red", "black"))

# Create the boxplot with custom colors and without outliers
p <- ggplot(data, aes(x = Cluster, y = EE_high, fill = Cluster)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA) +
  geom_jitter(aes(color = point_color), alpha = 0.4, position = position_jitter(width = 0.2)) +  
  geom_abline(intercept = 13, slope = 0, color = "orange") +
  geom_abline(intercept = 15, slope = 0, color = "red") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(breaks = y_labels) +
  labs(title = "",
       y = expression(bold(E/E*minute[high] ('/'))),
       x = "") +
  theme_classic() +
  theme(
    legend.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    axis.text.y = element_text(colour = y_colors)
  ) + 
  scale_color_manual(values = c("black", "orange", "red"), guide = "none")

# Print the plot
print(p)

ggsave("EEhigh.png", p, dpi = 900, width = 9, height = 9/2)



# Create a binary diagnosis column
data$HFpEF_diagnosis <- ifelse(data$EE_high >= 13, "Yes", "No")

# Create a contingency table
table_data <- table(data$Cluster, data$HFpEF_diagnosis)
table_data

# Fisher's Exact Test
fishers_test <- fisher.test(table_data)

# Print the results
print(fishers_test)
