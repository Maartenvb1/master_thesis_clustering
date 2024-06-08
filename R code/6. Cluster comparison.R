library(readxl)
library(dplyr)
library(car)

# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# Read the data
data <- read_excel("Data/results.xlsx")

# Assign clusters
data$Cluster <- as.factor(data$Clusters)

# N
table(data$Cluster)
sum(table(data$Cluster))

# Age
mean(data$Age)
sd(data$Age)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

summary(aov(Age ~ Cluster, data = data))
kruskal.test(Age ~ Cluster, data = data)

# Sex
mean(data$Sex)
sum(data$Sex)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_sex = mean(Sex, na.rm = TRUE),
    sum_sex = sum(Sex, na.rm = TRUE)
    )

contingency_table <- table(data$Sex, data$Cluster)
print(contingency_table)

chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

# BSA
mean(data$BSA)
sd(data$BSA)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_BSA = mean(BSA, na.rm = TRUE),
    sd_BSA = sd(BSA, na.rm = TRUE)
  )

summary(aov(BSA ~ Cluster, data = data))
kruskal.test(BSA ~ Cluster, data = data)

# SRV_high
mean(data$SRV_high, na.rm = TRUE)
sd(data$SRV_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_SRV_high = mean(SRV_high, na.rm = TRUE),
    sd_SRV_high = sd(SRV_high, na.rm = TRUE)
  )

summary(aov(SRV_high ~ Cluster, data = data))
kruskal.test(SRV_high ~ Cluster, data = data)

# Ssept_high
mean(data$Ssept_high, na.rm = TRUE)
sd(data$Ssept_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_SRV_high = mean(Ssept_high, na.rm = TRUE),
    sd_SRV_high = sd(Ssept_high, na.rm = TRUE)
  )

summary(aov(Ssept_high ~ Cluster, data = data))
kruskal.test(Ssept_high ~ Cluster, data = data)

# Slat_high
mean(data$Slat_high, na.rm = TRUE)
sd(data$Slat_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_SRV_high = mean(Slat_high, na.rm = TRUE),
    sd_SRV_high = sd(Slat_high, na.rm = TRUE)
  )

summary(aov(Slat_high ~ Cluster, data = data))
kruskal.test(Slat_high ~ Cluster, data = data)

# Alat_high
mean(data$Alat_high, na.rm = TRUE)
sd(data$Alat_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_SRV_high = mean(Alat_high, na.rm = TRUE),
    sd_SRV_high = sd(Alat_high, na.rm = TRUE)
  )

summary(aov(Alat_high ~ Cluster, data = data))
kruskal.test(Alat_high ~ Cluster, data = data)

# FEV1
mean(data$pred_FEV1, na.rm = TRUE)
sd(data$pred_FEV1, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_pred_FEV1 = mean(pred_FEV1, na.rm = TRUE),
    sd_pred_FEV1 = sd(pred_FEV1, na.rm = TRUE),
  )

summary(aov(pred_FEV1 ~ Cluster, data = data))
kruskal.test(pred_FEV1 ~ Cluster, data = data)

# RER
mean(data$RER, na.rm = TRUE)
sd(data$RER, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_RER = mean(RER, na.rm = TRUE),
    sd_RER = sd(RER, na.rm = TRUE)
  )

summary(aov(RER ~ Cluster, data = data))
kruskal.test(RER ~ Cluster, data = data)

# VEMVV
mean(data$VEMVV, na.rm = TRUE)
sd(data$VEMVV, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_VEMVV = mean(VEMVV, na.rm = TRUE),
    sd_VEMVV = sd(VEMVV, na.rm = TRUE)
  )

# Check normality
shapiro.test(data$VEMVV[data$Cluster == 1])
shapiro.test(data$VEMVV[data$Cluster == 2])
shapiro.test(data$VEMVV[data$Cluster == 3])

# Check homogeneity of variances
leveneTest(VEMVV ~ Cluster, data = data)

kruskal.test(VEMVV ~ Cluster, data = data)

# OE
mean(data$OE, na.rm = TRUE)
sd(data$OE, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_OE = mean(OE, na.rm = TRUE),
    sd_OE = sd(OE, na.rm = TRUE)
  )

summary(aov(OE ~ Cluster, data = data))
kruskal.test(OE ~ Cluster, data = data)

# PAPm_rest
mean(data$PAPm_rest, na.rm = TRUE)
sd(data$PAPm_rest, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_PAPm_rest = mean(PAPm_rest, na.rm = TRUE),
    sd_PAPm_rest = sd(PAPm_rest, na.rm = TRUE)
  )

summary(aov(PAPm_rest ~ Cluster, data = data))
kruskal.test(PAPm_rest ~ Cluster, data = data)

# PAPm_high
mean(data$PAPm_high, na.rm = TRUE)
sd(data$PAPm_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_PAPm_high = mean(PAPm_high, na.rm = TRUE),
    sd_PAPm_high = sd(PAPm_high, na.rm = TRUE)
  )

# Check normality
shapiro.test(data$PAPm_high[data$Cluster == 1])
shapiro.test(data$PAPm_high[data$Cluster == 2])
shapiro.test(data$PAPm_high[data$Cluster == 3])

# Check homogeneity of variances
leveneTest(PAPm_high ~ Cluster, data = data)

kruskal.test(PAPm_high ~ Cluster, data = data)

# PAPmCO
mean(data$PAPmCO, na.rm = TRUE)
sd(data$PAPmCO, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_PAPmCO = mean(PAPmCO, na.rm = TRUE),
    sd_PAPmCO = sd(PAPmCO, na.rm = TRUE)
  )

summary(aov(PAPmCO ~ Cluster, data = data))
kruskal.test(PAPmCO ~ Cluster, data = data)

# VC
mean(data$VC, na.rm = TRUE)
sd(data$VC, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_VC = mean(VC, na.rm = TRUE),
    sd_VC = sd(VC, na.rm = TRUE)
  )

summary(aov(VC ~ Cluster, data = data))
kruskal.test(VC ~ Cluster, data = data)

# DBP
mean(data$BPd_high, na.rm = TRUE)
sd(data$BPd_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_BPd_high = mean(BPd_high, na.rm = TRUE),
    sd_BPd_high = sd(BPd_high, na.rm = TRUE)
  )

# Check normality
shapiro.test(data$BPd_high[data$Cluster == 1])
shapiro.test(data$BPd_high[data$Cluster == 2])
shapiro.test(data$BPd_high[data$Cluster == 3])

# Check homogeneity of variances
leveneTest(BPd_high ~ Cluster, data = data)

kruskal.test(BPd_high ~ Cluster, data = data)

# SBP
mean(data$BPs_high, na.rm = TRUE)
sd(data$BPs_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_BPs_high = mean(BPs_high, na.rm = TRUE),
    sd_BPs_high = sd(BPs_high, na.rm = TRUE)
  )

# Check normality
shapiro.test(data$BPs_high[data$Cluster == 1])
shapiro.test(data$BPs_high[data$Cluster == 2])
shapiro.test(data$BPs_high[data$Cluster == 3])

# Check homogeneity of variances
leveneTest(BPs_high ~ Cluster, data = data)

summary(aov(BPs_high ~ Cluster, data = data))

# LAVi
mean(data$LAVi, na.rm = TRUE)
sd(data$LAVi, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_LAVi = mean(LAVi, na.rm = TRUE),
    sd_LAVi = sd(LAVi, na.rm = TRUE)
  )

summary(aov(LAVi ~ Cluster, data = data))
kruskal.test(LAVi ~ Cluster, data = data)

# LVEF_rest
mean(data$LVEF_rest, na.rm = TRUE)
sd(data$LVEF_rest, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_LVEF_rest = mean(LVEF_rest, na.rm = TRUE),
    sd_LVEF_rest = sd(LVEF_rest, na.rm = TRUE)
  )

# Check normality
shapiro.test(data$LVEF_rest[data$Cluster == 1])
shapiro.test(data$LVEF_rest[data$Cluster == 2])
shapiro.test(data$LVEF_rest[data$Cluster == 3])

# Check homogeneity of variances
leveneTest(LVEF_rest ~ Cluster, data = data)

kruskal.test(LVEF_rest ~ Cluster, data = data)

# LVEF_high
mean(data$LVEF_high, na.rm = TRUE)
sd(data$LVEF_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_LVEF_high = mean(LVEF_high, na.rm = TRUE),
    sd_LVEF_high = sd(LVEF_high, na.rm = TRUE)
  )

# Check normality
shapiro.test(data$LVEF_high[data$Cluster == 1])
shapiro.test(data$LVEF_high[data$Cluster == 2])
shapiro.test(data$LVEF_high[data$Cluster == 3])

# Check homogeneity of variances
leveneTest(LVEF_high ~ Cluster, data = data)

summary(aov(LVEF_high ~ Cluster, data = data))

# EE_rest
mean(data$EE_rest, na.rm = TRUE)
sd(data$EE_rest, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_EE_rest = mean(EE_rest, na.rm = TRUE),
    sd_EE_rest = sd(EE_rest, na.rm = TRUE)
  )

summary(aov(EE_rest ~ Cluster, data = data))
kruskal.test(EE_rest ~ Cluster, data = data)

# TAPSE_high
mean(data$TAPSE_high, na.rm = TRUE)
sd(data$TAPSE_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_TAPSE_high = mean(TAPSE_high, na.rm = TRUE),
    sd_TAPSE_high = sd(TAPSE_high, na.rm = TRUE)
  )

summary(aov(TAPSE_high ~ Cluster, data = data))
kruskal.test(TAPSE_high ~ Cluster, data = data)

# RVESPAR_high
mean(data$RVESPAR_high, na.rm = TRUE)
sd(data$RVESPAR_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_RVESPAR_high = mean(RVESPAR_high, na.rm = TRUE),
    sd_RVESPAR_high = sd(RVESPAR_high, na.rm = TRUE)
  )

summary(aov(RVESPAR_high ~ Cluster, data = data))
kruskal.test(RVESPAR_high ~ Cluster, data = data)

# Watt_high
mean(data$Watt_high, na.rm = TRUE)
sd(data$Watt_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_Watt_high = mean(Watt_high, na.rm = TRUE),
    sd_Watt_high = sd(Watt_high, na.rm = TRUE)
  )

summary(aov(Watt_high ~ Cluster, data = data))
kruskal.test(Watt_high ~ Cluster, data = data)

# HR_high
mean(data$HR_high, na.rm = TRUE)
sd(data$HR_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_HR_high = mean(HR_high, na.rm = TRUE),
    sd_HR_high = sd(HR_high, na.rm = TRUE)
  )

summary(aov(HR_high ~ Cluster, data = data))
kruskal.test(HR_high ~ Cluster, data = data)

# H2FPEF
mean(data$H2FPEF, na.rm = TRUE)
sd(data$H2FPEF, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_H2FPEF = mean(H2FPEF, na.rm = TRUE),
    sd_H2FPEF = sd(H2FPEF, na.rm = TRUE)
  )

summary(aov(H2FPEF ~ Cluster, data = data))
kruskal.test(H2FPEF ~ Cluster, data = data)


# EE_high
mean(data$EE_high, na.rm = TRUE)
sd(data$EE_high, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_EE_high = mean(EE_high, na.rm = TRUE),
    sd_EE_high = sd(EE_high, na.rm = TRUE)
  )

summary(aov(EE_high ~ Cluster, data = data))
kruskal.test(EE_high ~ Cluster, data = data)
TukeyHSD(aov(EE_high ~ Cluster, data = data))

# VO2max
mean(data$VO2_max, na.rm = TRUE)
sd(data$VO2_max, na.rm = TRUE)

data %>%
  group_by(Cluster) %>%
  summarise(
    mean_VO2_max = mean(VO2_max, na.rm = TRUE),
    sd_VO2_max = sd(VO2_max, na.rm = TRUE)
  )

summary(aov(VO2_max ~ Cluster, data = data))
kruskal.test(VO2_max ~ Cluster, data = data)
TukeyHSD(aov(VO2_max ~ Cluster, data = data))
