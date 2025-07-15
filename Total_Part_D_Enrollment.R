library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

#Filter the plans that offer Part D coverage

df_partd_list <- read.csv("CPSC_Contract_Info_2025_05.csv")
head(df_partd_list, 10)

df_partd_list <- df_partd_list %>% select(Contract.ID, Plan.ID, Offers.Part.D)

df_partd <- read.csv("CPSC_Enrollment_Info_2025_05.csv")

df_partd$Enrollment = as.numeric(df_partd$Enrollment)

df_partd_new <- df_partd %>% 
  filter(!is.na(Enrollment))
View(df_partd_new)

joined_partd <- df_partd_new %>% full_join(df_partd_list, by = c("Plan.ID", "Contract.Number" = "Contract.ID")) %>%
  filter(Offers.Part.D == "Yes")

View(joined_partd)

#Prepare Urban Codes 2024

df_codes <- read.csv("UrbanInfluencecodes2024.csv")

df_codes_2024 <- df_codes %>% 
  filter(Attribute == "UIC_2024")
View(df_codes_2024)

#Join Part D enrollment 2025 with Urban Codes 2024

joined_partd_all <- joined_partd %>% full_join(df_codes_2024, by = c("FIPS.State.County.Code" = "FIPS.UIC"))

joined_partd_all_enrollment <- joined_partd_all %>% group_by(Value) %>%
  summarise(total_enrollment = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment)

#Analysis

joined_partd_all_enrollment_metro <- joined_partd_all %>%  filter(Value %in% c(1, 4)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_metro)

joined_partd_all_enrollment_adj <- joined_partd_all %>%  filter(Value %in% c(2,3,5,6)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_adj)

joined_partd_all_enrollment_rural <- joined_partd_all %>%  filter(Value %in% c(7,8,9)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_rural)

#Troubleshoot

joined_partd_all_na <- joined_partd_all %>% filter(is.na(Value))
View(joined_partd_all_na)

write.csv(joined_partd_all_na, "partd_na.csv")