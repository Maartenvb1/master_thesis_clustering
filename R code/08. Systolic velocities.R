library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

# Define the theme to be used in all plots
mytheme <- theme_classic() + theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x = element_text(margin = margin(t = 0, b = 0)),
  legend.position = "none",
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
  plot.title = element_text(hjust = 0.5, size = 8, margin = margin(b = 0)),
  plot.margin = margin(1, 1, 1, 1)
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
# Function to generate individual plots with common settings
generate_plot <- function(data, x_var, title, x_breaks, x_limits = NULL) {
  ggplot(data, aes(y = Cluster, x = !!sym(x_var), fill = Cluster)) +
    geom_boxplot(width = 0.6, outlier.shape = 16, alpha = 0.75) +
    stat_summary(fun = mean, geom = "point", shape = 16, size = 2) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "", y = "") +
    scale_x_continuous(breaks = x_breaks, limits = x_limits) +
    ggtitle(parse(text = title)) +
    mytheme
}

# Generate individual plots
plot_PAPm_rest <- generate_plot(data, "Alat_high", "bold(A * \"'\"['lat,high'] * ' (cm/s)')", c(0, 5, 10, 15, 20, 25, 30), c(0, 30.5))
plot_PAPm_high <- generate_plot(data, "Slat_high", "bold(S * \"'\"['lat,high'] * ' (cm/s)')", c(0, 5, 10, 15, 20, 25, 30), c(0, 30.5))
plot_PAPmCO    <- generate_plot(data, "Ssept_high", "bold(S * \"'\"['sept,high'] * ' (cm/s)')", c(0, 5, 10, 15, 20, 25, 30), c(0, 30.5))
plot_VC        <- generate_plot(data, "SRV_high", "bold(S * \"'\"['RV,high'] * ' (cm/s)')",      c(0, 5, 10, 15, 20, 25, 30), c(0, 30.5))

# Combine the plots using cowplot
combined_plot <- plot_grid(plot_PAPm_rest, plot_PAPm_high, plot_PAPmCO, plot_VC, ncol = 1, align = "v")

# Add a common legend to the combined plot
legend <- get_legend(
  ggplot(data, aes(y = Cluster, x = VC, fill = Cluster)) +
    geom_boxplot(alpha = 0.75) +
    scale_fill_brewer(palette = "Set2") +
    theme_classic() +
    theme(
      legend.title = element_text(face = "bold"),
    )
)

# Combine the plots with the legend
final_plot <- plot_grid(combined_plot, legend, rel_widths = c(4, 0.5), rel_heights = 4)

# Display the final plot
print(final_plot)

ggsave("Systolic velocities.png", final_plot, dpi = 900, width = 9, height = 9/2)

