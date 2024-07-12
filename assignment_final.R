rm(list=ls()) # clear the environment
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#-------Import necessary packages here-------------------#
library(tidyverse)
library(tidyr)
library(ggplot2)
library(janitor)
#------ Uploading PERMID --------------------------------#
PERMID <- "7185572" #Type your PERMID with the quotation marks
PERMID <- as.numeric(gsub("\\D", "", PERMID)) #Don't touch
set.seed(PERMID) #Don't touch

#Part 1: Cleaning Education Data
#1: Import the CPI data
cpi_data <- read_csv("CPI_U_minneapolis_fed.csv")[, 1:2]

#2: Import the education data
education_data <- read_csv("education_data.csv")
#Rename variables according to "rename" column in data_description
names(education_data)[names(education_data) == "UNITID"] <- "school_id"
names(education_data)[names(education_data) == "INSTNM"] <- "school_name"
names(education_data)[names(education_data) == "YEAR"] <- "year"
names(education_data)[names(education_data) == "STABBR"] <- "state_id"
names(education_data)[names(education_data) == "PREDDEG"] <- "predominant_degree"
names(education_data)[names(education_data) == "CONTROL"] <- "institution_type"
names(education_data)[names(education_data) == "CDR3"] <- "default_rate"
names(education_data)[names(education_data) == "LO_INC_DEBT_MDN"] <- "median_debt_low_income"
names(education_data)[names(education_data) == "MD_INC_DEBT_MDN"] <- "median_debt_med_income"
names(education_data)[names(education_data) == "HI_INC_DEBT_MDN"] <- "median_debt_high_income"
names(education_data)[names(education_data) == "FAMINC"] <- "avg_family_income"

#convert school names to lowercase
education_data$school_name <- tolower(education_data$school_name)

#convert 5 variables to numeric
numeric_vars <- c("median_debt_low_income", "median_debt_med_income",
                  "median_debt_high_income", "default_rate", "avg_family_income")
education_data[numeric_vars] <- lapply(education_data[numeric_vars], as.numeric)

#3: Update the column called institution_type to be equal to "public" if the school is public
education_data_clean <- education_data

education_data_clean$institution_type <- ifelse(education_data$institution_type == 1, "public", "private")
# 4
education_data_BA1 <- subset(education_data_clean, predominant_degree == 3)
# 5
education_data_BA <- inner_join(education_data_BA1, cpi_data, by = "year")
cpi_2018 <- cpi_data$CPI[cpi_data$year == 2018]

adjust_to_2018 <- function(value, year) {
  cpi_year <- cpi_data$CPI[cpi_data$year == year]
  return(value * (cpi_2018 / cpi_year))
}

education_data_BA$real_debt_low_income <- mapply(adjust_to_2018, education_data_BA$median_debt_low_income, education_data_BA$year)
education_data_BA$real_debt_med_income <- mapply(adjust_to_2018, education_data_BA$median_debt_med_income, education_data_BA$year)
education_data_BA$real_debt_high_income <- mapply(adjust_to_2018, education_data_BA$median_debt_high_income, education_data_BA$year)
education_data_BA$real_family_income <- mapply(adjust_to_2018, education_data_BA$avg_family_income, education_data_BA$year)

education_data_BA <- education_data_BA %>%
  select(-median_debt_low_income, -median_debt_med_income, -median_debt_high_income, -avg_family_income, -CPI)

education_data_BA <- education_data_BA
education_data_BA

# 1
cost_data1 <- read_csv("cost_data.csv") %>%
  select(-...1, -NPT42_PUB, -NPT44_PUB, -NPT42_PRIV, -NPT44_PRIV)
cost_data2 <- cost_data1
names(cost_data2)[names(cost_data2) == "UNITID"] <- "school_id"
names(cost_data2)[names(cost_data2) == "INSTNM"] <- "school_name"
names(cost_data2)[names(cost_data2) == "YEAR"] <- "year"
names(cost_data2)[names(cost_data2) == "NPT41_PUB"] <- "mean_cost_low_income_public"
names(cost_data2)[names(cost_data2) == "NPT43_PUB"] <- "mean_cost_med_income_public"
names(cost_data2)[names(cost_data2) == "NPT45_PUB"] <- "mean_cost_high_income_public"
names(cost_data2)[names(cost_data2) == "NPT41_PRIV"] <- "mean_cost_low_income_private"
names(cost_data2)[names(cost_data2) == "NPT43_PRIV"] <- "mean_cost_med_income_private"
names(cost_data2)[names(cost_data2) == "NPT45_PRIV"] <- "mean_cost_high_income_private"
# 2
cost_data2$school_name <- tolower(cost_data2$school_name)
numeric_vars <- c("mean_cost_low_income_public", "mean_cost_med_income_public",
                  "mean_cost_high_income_public", "mean_cost_low_income_private", "mean_cost_med_income_private", "mean_cost_high_income_private")
cost_data2[numeric_vars] <- lapply(cost_data2[numeric_vars], as.numeric)
cost_data3 <- cost_data2
# 3
cost_data3$mean_cost_low_income <- cost_data3$mean_cost_low_income_public
cost_data3$mean_cost_med_income <- cost_data3$mean_cost_med_income_public
cost_data3$mean_cost_high_income <- cost_data3$mean_cost_high_income_public
cost_data3$mean_cost_low_income[is.na(cost_data3$mean_cost_low_income)] <- cost_data3$mean_cost_low_income_private[is.na(cost_data3$mean_cost_low_income)]
cost_data3$mean_cost_med_income[is.na(cost_data3$mean_cost_med_income)] <- cost_data3$mean_cost_med_income_private[is.na(cost_data3$mean_cost_med_income)]
cost_data3$mean_cost_high_income[is.na(cost_data3$mean_cost_high_income)] <- cost_data3$mean_cost_high_income_private[is.na(cost_data3$mean_cost_high_income)]
cost_data3 <- cost_data3 %>%
  select(-mean_cost_low_income_public, -mean_cost_low_income_private, -mean_cost_med_income_public, -mean_cost_med_income_private, -mean_cost_high_income_public, -mean_cost_high_income_private)
# 4
cost_data4 <- inner_join(cost_data3, cpi_data, by = "year")
cpi_2018 <- cpi_data$CPI[cpi_data$year == 2018]
adjust_to_2018 <- function(value, year) {
  cpi_year <- cpi_data$CPI[cpi_data$year == year]
  return(value * (cpi_2018 / cpi_year))
}
cost_data4$real_cost_low_income <- mapply(adjust_to_2018, cost_data4$mean_cost_low_income, cost_data4$year)
cost_data4$real_cost_med_income <- mapply(adjust_to_2018, cost_data4$mean_cost_med_income, cost_data4$year)
cost_data4$real_cost_high_income <- mapply(adjust_to_2018, cost_data4$mean_cost_high_income, cost_data4$year)
# 5
cost_data <- cost_data4 %>%
  select(-mean_cost_low_income, -mean_cost_med_income, -mean_cost_high_income, -CPI)
cost_data <- cost_data
# Part 3
# 1
education_data_BA_cost <- left_join(education_data_BA, cost_data, by = c("year", "school_id"))
education_data_BA_cost <- select(education_data_BA_cost, -school_name.y)
# 2
debt_cost_sumstat_year <- education_data_BA_cost %>%
  select(-real_cost_low_income, -real_cost_med_income, -real_cost_high_income)
debt_cost_sumstat_year <- education_data_BA_cost |>
  group_by(year, institution_type) |>
  summarise(mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
            mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
            mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
            mean_cost_for_low_income = mean(real_cost_low_income, na.rm = TRUE),
            mean_cost_for_median_income = mean(real_cost_med_income, na.rm = TRUE),
            mean_cost_for_high_income = mean(real_cost_high_income, na.rm = TRUE),
            .groups = "drop")
#3
debt_cost_data_by_year <- debt_cost_sumstat_year %>%
  pivot_longer(cols = starts_with("mean_debt_for_"),
               names_to = "income_category",
               names_prefix = "mean_debt_for_",
               values_to = "debt") %>%
  pivot_longer(cols = starts_with("mean_cost_for_"),
               names_to = "income_category_cost",
               names_prefix = "mean_cost_for_",
               values_to = "cost") %>%
  filter(str_replace(income_category, "_income", " income") == str_replace(income_category_cost, "_income", " income")) %>%
  select(-income_category_cost) %>%
  mutate(income_category = str_replace(income_category, "_income", " income")) %>%
  arrange(year, institution_type, factor(income_category, levels = c("low income", "median income", "high income")))
head(debt_cost_data_by_year)
# 4
# a
debt_sumstat_school_type <- education_data_BA_cost %>%
  group_by(institution_type) %>%
  summarise(mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
            mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
            mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
            mean_family_income = mean(real_family_income, na.rm = TRUE))
# b
debt_sumstat_year <- education_data_BA_cost %>%
  group_by(year) %>%
  summarise(mean_debt_for_low_income = mean(real_debt_low_income, na.rm = TRUE),
            mean_debt_for_median_income = mean(real_debt_med_income, na.rm = TRUE),
            mean_debt_for_high_income = mean(real_debt_high_income, na.rm = TRUE),
            mean_family_income = mean(real_family_income, na.rm = TRUE))
# c
cost_sumstat_school_type <- education_data_BA_cost %>%
  group_by(institution_type) %>%
  summarise(mean_cost_for_low_income = mean(real_cost_low_income, na.rm = TRUE),
            mean_cost_for_median_income = mean(real_cost_med_income, na.rm = TRUE),
            mean_cost_for_high_income = mean(real_cost_high_income, na.rm = TRUE))
# d
cost_sumstat_year <- education_data_BA_cost %>%
  summarise(
    mean_cost_for_low_income = mean(real_cost_low_income, na.rm = TRUE),
    mean_cost_for_median_income = mean(real_cost_med_income, na.rm = TRUE),
    mean_cost_for_high_income = mean(real_cost_high_income, na.rm = TRUE),
    .by = year
  )
write.csv(education_data_BA_cost, "education_data_BA_cost.csv", row.names = FALSE)