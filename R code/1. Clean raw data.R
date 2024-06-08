library(readxl)
library(writexl)
library(dplyr)

# Set the working directory
setwd("C:/Users/maart/Documents/Master of Statistics and Data Science/Year 2/Semester 2/Master Thesis")

# File name of raw data
file_path <- "Data/Datascience_Lijst_MS_FINAL_2003.xlsx"

# Read and process the raw data
data <- read_excel(file_path)

# Remove duplicates
data <- data %>%
  group_by(PatientID) %>%
  filter(time_to_censoring == max(time_to_censoring)) %>%
  ungroup() %>%
  filter(PatientID != "479259 b")

# Remove unnecessary columns
data <- data %>%
  select(-PatientID,
         -Datum,
         -Mortaliteit,
         -follow_up,
         -time_to_first_event,
         -`Datum revisie`,
         -`Datum overlijden`,
         -`Ongeplande Hospitalisatie (eerste)`,
         -`Datum opname 1`)

# Remove unwanted tokens from column names and translate Dutch to English
data <- data %>%
  rename(SRV_high   = `S' RV_high`)                      %>%
  rename(Ssept_high = `S'sept_high`)                     %>%
  rename(Slat_high  = `S'lat_high`)                      %>%
  rename(Alat_high  = `A' lat_high`)                     %>%
  rename(EE_high    = `E/E'_high2`)                      %>%
  rename(EE_rest    = `E/E'_rest`)                       %>%
  rename(VC         = `Vascular compliance`)             %>%
  rename(VEMVV      = `VE/MVV`)                          %>%
  rename(PAPmCO     = `PAPm/CO`)                         %>%
  rename(OE         = `Oxygen extraction (AVO2Diff/Hb)`) %>%
  rename(H2FPEF     = `H2FPEF score`)                    %>%
  rename(BPd_high   = BPd_High)                          %>%
  rename(RER        = RER3)                              %>%
  rename(Sex        = Geslacht)                          %>%
  rename(Age        = Leeftijd)

# Make data numeric
data <- data %>%
  mutate(EE_rest = as.numeric(EE_rest)) %>%
  mutate(EE_high = as.numeric(EE_high))
  
# Replace errors in the data by missing values
data <- data %>%
  mutate(Alat_high = if_else(Alat_high == 0,  NA_real_, Alat_high)) %>%
  mutate(EE_high   = if_else(EE_high == 0,    NA_real_, EE_high))   %>%
  mutate(LAVi      = if_else(LAVi == 0,       NA_real_, LAVi))      %>%
  mutate(LVEF_high = if_else(LVEF_high < 0,   NA_real_, LVEF_high)) %>%
  mutate(OE        = if_else(OE == 0,         NA_real_, OE))        %>%
  mutate(PAPm_rest = if_else(PAPm_rest == 2,  NA_real_, PAPm_rest)) %>%
  mutate(Slat_high = if_else(Slat_high == 77, NA_real_, Slat_high)) %>%
  mutate(VC        = if_else(VC <= 0,         NA_real_, VC))        %>%
  mutate(VEMVV     = if_else(VEMVV <= 0,      NA_real_, VEMVV))     %>%
  mutate(VO2_max   = if_else(VO2_max == 0,    NA_real_, VO2_max))

# Make sex a binary variable, 0 = Male and 1 = Female
data <- data %>%
  mutate(Sex = case_when(
    Sex == "man" ~ 0,
    Sex == "vrouw" ~ 1,
    TRUE ~ NA_real_
  ))

# Store the cleaned data in a new xlsx-file
write_xlsx(data, path = "Data/cleaned_data.xlsx")