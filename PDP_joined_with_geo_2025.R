library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

#Prepare PDP 2025 data

df_pdp_2025 <- read.csv("SCC_Enrollment_PDP_Alt_2025_05.csv")

df_pdp_2025$Enrolled = as.numeric(df_pdp_2025$Enrolled)

df_pdp_plan_2025 <- select(df_pdp_2025, State, County, Organization.Name, Contract.ID, FIPS.Code, Enrolled)

#Prepare Urban Codes 2024 file

df_codes <- read.csv("UrbanInfluencecodes2024.csv")

df_codes_2024 <- df_codes %>% 
  filter(Attribute == "UIC_2024")

#Join PDP enrollment with Urban Codes 2024

joined_pdp <- df_pdp_plan_2025 %>% full_join(df_codes_2024, by = c("FIPS.Code" = "FIPS.UIC"))

#Analysis

joined_pdp_count <- joined_pdp %>% group_by(Value) %>%
  summarise(total=n())

View(joined_pdp_count)

joined_pdp_enrollment <- joined_pdp %>% group_by(Value) %>%
  summarise(total_enrollment = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment)

joined_pdp_enrollment_metro <- joined_pdp %>%  filter(Value %in% c(1, 4)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment_metro)

joined_pdp_enrollment_adj <- joined_pdp %>%  filter(Value %in% c(2,3,5,6)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment_adj)

joined_pdp_enrollment_rural <- joined_pdp %>%  filter(Value %in% c(7,8,9)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment_rural)

#Troubleshoot

joined_pdp_na <- joined_pdp %>% filter(is.na(Value))
View(joined_pdp_na)

joined_pdp_FL <- joined_pdp %>% filter(State.x =="FL", Value %in% c(7,8,9)) 
View(joined_pdp_FL)
