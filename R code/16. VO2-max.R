library(readxl)
library(ggplot2)

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

p <- ggplot(data, aes(x = Cluster, y = VO2_max, fill = Cluster)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA) +
  geom_jitter(alpha = 0.4, position = position_jitter(width = 0.2)) +  
  scale_fill_brewer(palette = "Set2") +
  labs(title = "",
       x = "",
       y = expression(bold(VO[2*","*max] *' (mL/kg/min)'))) +
  theme_classic()+
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
  )

p

ggsave("VO2max.png", p, dpi = 900, width = 9, height = 9/2)


# Conduct ANOVA
vo2_anova <- aov(log(VO2_max) ~ Cluster, data = data)
summary(vo2_anova)

# Post-hoc test
TukeyHSD(vo2_anova)

# Check normality
shapiro.test(log(data$VO2_max[data$Cluster == 1]))
shapiro.test(log(data$VO2_max[data$Cluster == 2]))
shapiro.test(log(data$VO2_max[data$Cluster == 3]))


# Check homogeneity of variances
bartlett.test(log(VO2_max) ~ Cluster, data = data)
