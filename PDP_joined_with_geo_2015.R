library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

df_pdp_2015 <- read.csv("SCC_Enrollment_PDP_Alt_2015_05.csv")

df_pdp_2015$Enrolled = as.numeric(df_pdp_2015$Enrolled)

df_pdp_plan_2015 <- select(df_pdp_2015, State, County, Organization.Name, Contract.ID, FIPS.Code, Enrolled)

View(df_pdp_plan_2015)

df_codes <- read.csv("Codes2013.csv")
View(df_codes)

#Join two PDP enrollment with Urban Codes 2013

joined_pdp_2015 <- df_pdp_plan_2015 %>% full_join(df_codes, by = c("FIPS.Code" = "FIPS"))

View(joined_pdp_2015)

#Analysis

joined_pdp_2015_count <- joined_pdp_2015 %>% group_by(UIC_2013) %>%
  summarise(total=n())

View(joined_pdp_2015_count)

joined_pdp_2015_enrollment <- joined_pdp_2015 %>% group_by(UIC_2013) %>%
  summarise(total_enrollment = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_2015_enrollment)

joined_pdp_enrollment_metro_2015 <- joined_pdp_2015 %>%  filter(UIC_2013 %in% c(1, 2)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state_metro_2015 = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment_metro_2015)

joined_pdp_enrollment_adj_2015 <- joined_pdp_2015 %>%  filter(UIC_2013 %in% c(3,4,5,6,7,9,10)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state_adj_2015 = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment_adj_2015)

joined_pdp_enrollment_rural_2015 <- joined_pdp_2015 %>%  filter(UIC_2013 %in% c(8,11,12)) %>%
  group_by(State.x) %>%
  summarise(total_enrollment_state_rural_2015 = sum(Enrolled, na.rm = TRUE))

View(joined_pdp_enrollment_rural_2015)

#Troubleshoot

joined_pdp_2015_na <- joined_pdp_2015 %>% filter(is.na(UIC_2013))
View(joined_pdp_2015_na)