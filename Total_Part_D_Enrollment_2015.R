library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

#Filter the plans that offer Part D coverage

df_partd_list_2015 <- read.csv("CPSC_Contract_Info_2015_05.csv")
head(df_partd_list_2015, 10)

df_partd_list_2015 <- df_partd_list_2015 %>% select(Contract.ID, Plan.ID, Offers.Part.D)

df_partd_2015 <- read.csv("CPSC_Enrollment_Info_2015_05.csv")

df_partd_2015$Enrollment = as.numeric(df_partd_2015$Enrollment)

df_partd_new_2015 <- df_partd_2015 %>% 
  filter(!is.na(Enrollment))
View(df_partd_new_2015)

joined_partd_2015 <- df_partd_new_2015 %>% full_join(df_partd_list_2015, by = c("Plan.ID", "Contract.Number" = "Contract.ID")) %>%
  filter(Offers.Part.D == "Yes")

View(joined_partd_2015)

#Prepare Urban Codes 2013

df_codes_2015 <- read.csv("Codes2013.csv")

#Join Part D enrollment 2015 with Urban Codes 2013

joined_partd_all_2015 <- joined_partd_2015 %>% full_join(df_codes_2015, by = c("FIPS.State.County.Code" = "FIPS"))

View(joined_partd_all_2015)

joined_partd_all_enrollment_2015 <- joined_partd_all_2015 %>% group_by(UIC_2013) %>%
  summarise(total_enrollment = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_2015)

#Analysis

joined_partd_all_enrollment_metro_2015 <- joined_partd_all_2015 %>%  filter(UIC_2013 %in% c(1, 2)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state_2015 = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_metro_2015)

joined_partd_all_enrollment_adj_2015 <- joined_partd_all_2015 %>%  filter(UIC_2013 %in% c(3,4,5,6,7,9,10)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state_adj_2015 = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_adj_2015)

joined_partd_all_enrollment_rural_2015 <- joined_partd_all_2015 %>%  filter(UIC_2013 %in% c(8,11,12)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state_rural_2015 = sum(Enrollment, na.rm = TRUE))

View(joined_partd_all_enrollment_rural_2015)

#Troubleshoot

joined_partd_all_2015_na <- joined_partd_all_2015 %>% filter(is.na(UIC_2013), !is.na(Enrollment))
View(joined_partd_all_2015_na)
