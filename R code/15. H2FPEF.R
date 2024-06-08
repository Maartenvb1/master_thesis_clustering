library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

# Define the theme to be used in all plots
mytheme <- theme_classic() + theme(
  axis.text.x = element_text(margin = margin(t = 0, b = 0)),
  legend.position = "none",
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
  plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = 0)),
  plot.margin = margin(1, 1, 1, 1),
  axis.title = element_text(face = "bold"),
)

# Set the working directory (adjust if necessary)
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

data_grouped <- data %>%
  group_by(Cluster, H2FPEF) %>%
  summarise(count = n()) %>%
  group_by(Cluster) %>%
  mutate(relative_freq = count / sum(count))

# Define colors for clusters
colors <- c("I" = "#66c2a5", "II" = "#fc8d62", "III" = "#8da0cb")

# Plot for Cluster 1
plot1 <- ggplot(data_grouped %>% filter(Cluster == "I"), aes(x = H2FPEF, y = relative_freq, fill = factor(Cluster))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.75) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = 0:5, labels = 0:5) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), labels = scales::label_number(accuracy = 0.01), limits = c(0, 0.28)) +
  labs(title = "", x = "", y = "Relative Frequency (%)") +
  mytheme

# Plot for Cluster 2
plot2 <- ggplot(data_grouped %>% filter(Cluster == "II"), aes(x = H2FPEF, y = relative_freq, fill = factor(Cluster))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.75) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = 0:5, labels = 0:5) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), labels = scales::label_number(accuracy = 0.01), limits = c(0, 0.28)) +
  labs(title = "", x = "H2FPEF", y = "") +
  mytheme +
  theme(axis.title.x = element_text(face = "bold"))

# Plot for Cluster 3
plot3 <- ggplot(data_grouped %>% filter(Cluster == "III"), aes(x = H2FPEF, y = relative_freq, fill = factor(Cluster))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.75) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = 0:5, labels = 0:5) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25), labels = scales::label_number(accuracy = 0.01), limits = c(0, 0.28)) +
  labs(title = "", x = "", y = "") +
  mytheme 

# Add a common legend to the combined plot
legend <- get_legend(
  ggplot(data, aes(x = H2FPEF, fill = Cluster)) +
    geom_bar(alpha = 0.75, color = "black") +
    scale_fill_brewer(palette = "Set2") +
    theme_classic() +
    theme(
      legend.title = element_text(face = "bold"),
    )
)

# Combine plots
combined_plot <- plot_grid(
  plot1, plot2, plot3, ncol = 3, align = "v"
)

# Add the common legend to the combined plot
final_plot <- plot_grid(
  combined_plot, legend, ncol = 2, rel_widths = c(4, 0.5))

# Print the final combined plot
print(final_plot)

ggsave("H2FPEF.png", final_plot, dpi = 900, width = 8, height = 4)
