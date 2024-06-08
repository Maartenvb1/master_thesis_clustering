library(readxl)
library(ggplot2)
library(dplyr)

# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# Read the data
data    <- read_excel("Data/cleaned_data.xlsx")
results <- read_excel("Data/results.xlsx")



# Access the dynamic column from results and merge it with data
data$Cluster <- as.factor(results$Clusters)

data$Cluster <- as.character(data$Cluster)  # Convert to character to allow replacement
data$Cluster[data$Cluster == "1"] <- "I"
data$Cluster[data$Cluster == "2"] <- "II"
data$Cluster[data$Cluster == "3"] <- "III"


# Recode Sex from 0/1 to Male/Female
data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Male", "Female"))

# Calculate cluster means
cluster_means <- data %>%
  group_by(Cluster) %>%
  summarise(mean_Age = mean(Age),
            mean_BSA = mean(BSA))

# Create the ggplot
plot <- ggplot(data, aes(x = Age, y = BSA, shape = Sex, color = Cluster)) +
  geom_point(alpha = 0.5) +
  geom_point(data = cluster_means, aes(x = mean_Age, y = mean_BSA, color = Cluster), shape = 15, size = 3, stroke = 2, inherit.aes = FALSE) +
  geom_text(data = cluster_means, aes(x = mean_Age, y = mean_BSA, label = sprintf("(%0.f, %.2f)", mean_Age, mean_BSA)), vjust = 2, size = 3, inherit.aes = FALSE) +
  labs(x = "Age (years)", y = "BSA (m²)") +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(1.40, 2.60, by = 0.20)) +
  theme_classic() +
  theme(legend.position = "right",
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)  # Add a border around the plots
  )

# Print the plot
print(plot)

ggsave("Patient profile.png", plot, dpi = 900, width = 8, height = 4)

