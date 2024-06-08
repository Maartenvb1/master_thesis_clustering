library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

mytheme <- theme(
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

# Generate individual plots with violin plots
plot_PAPm_rest <- ggplot(data, aes(y = Cluster, x = pred_FEV1, fill = Cluster)) +
  geom_boxplot(width = 0.6, outlier.shape = 16, alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(x = "", y = "", fill = "Cluster") +
  scale_x_continuous(breaks = seq(0, 150, by = 25)) +
  mytheme +
  ggtitle(parse(text = "bold(FEV['1,pred']*' (%)')"))

plot_PAPm_high <- ggplot(data, aes(y = Cluster, x = RER, fill = Cluster)) +
  geom_boxplot(width = 0.6, outlier.shape = 16, alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(x = "", y = "", fill = "Cluster") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.1)) +
  mytheme +
  ggtitle(parse(text = "bold('RER (/)')"))


plot_PAPmCO <- ggplot(data, aes(y = Cluster, x = VEMVV, fill = Cluster)) +
  geom_boxplot(width = 0.6, outlier.shape = 16, alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(x = "", y = "", fill = "Cluster") +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, by = 0.5)) +
  mytheme +
  ggtitle(parse(text = "bold('VE/MVV (/)')"))


plot_VC <- ggplot(data, aes(y = Cluster, x = OE, fill = Cluster)) +
  geom_boxplot(width = 0.6, outlier.shape = 16, alpha = 0.75) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(x = "", y = "", fill = "Cluster") +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, by = 0.5)) +
  mytheme +
  ggtitle(expression(bold(A-V~O[2*Diff]/Hb~(mL~O[2]~"/"~g~Hb))))


# Combine the plots using cowplot
combined_plot <- plot_grid(plot_PAPm_rest, plot_PAPm_high, plot_PAPmCO, plot_VC, ncol = 1, align = "v")

# Add a common legend to the combined plot
legend <- get_legend(
  ggplot(data, aes(y = Cluster, x = VC, fill = Cluster)) +
    geom_boxplot(width = 0.6, outlier.shape = 16, alpha = 0.75) +
    scale_fill_brewer(palette = "Set2") +
    theme_classic() +
    labs(x = "", y = "", fill = "Cluster") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(margin = margin(t = 0, b = 0)),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(hjust = 0.5, size = 8),
      legend.title = element_text(face = "bold"),
    )
)

# Combine the plots with the legend
final_plot <- plot_grid(combined_plot, legend, rel_widths = c(4, 0.5), rel_heights = 4)

# Display the final plot
print(final_plot)

ggsave("Respiratory variables.png", final_plot, dpi = 900, width = 9, height = 4.5)
