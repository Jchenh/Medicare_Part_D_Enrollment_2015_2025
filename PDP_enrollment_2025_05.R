library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

df_pdp_2025 <- read.csv("SCC_Enrollment_PDP_Alt_2025_05.csv")
head(df_pdp_2025, 10)

df_pdp_2025$Enrolled = as.numeric(df_pdp_2025$Enrolled)

df_pdp_plan_2025 <- select(df_pdp_2025, State, Organization.Name, Contract.ID, Enrolled)

df_pdp_plan_2025_full <- df_pdp_plan_2025 %>% group_by(State, Organization.Name, Contract.ID) %>% 
  summarise(total_enrollment = sum(Enrolled, na.rm = TRUE)) %>%
  filter(State %in% c("AL", "ME", "MI", "NH", "MS", "CT", "WV", "KY", "OR", "AZ", "MT", "CA"))
View(df_pdp_plan_2025_full)

write.csv(df_pdp_plan_2025_full, "pdp_by_plan_state_2025_05.csv")