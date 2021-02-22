# Purpose----------------------------------------------------------------------
# Generate a table of the count of households who own vs do not own a car,
# broken down by tenure (rent vs own), income (as a % of area median income),
# and area (PUMAs in Philadelphia).
# 
# Data source: ACS 2019 5-year PUMS microdata

# Preliminaries----------------------------------------------------------------

library(tidyverse)      # Data cleaning and arranging tools
library(tidylog)        # Quality control for ditto
library(tidycensus)     # Importing Census data via API
library(openxlsx)       # Writing to .xlsx file
library(here)           # Specifying current working directory path

# Replace the value inside the quotes with your own Census API key
# Obtainable at https://api.census.gov/data/key_signup.html
census_api_key("----")

# Get data---------------------------------------------------------------------
# Variables of interest are: PUMA, Vehicles available, Tenure, Household income
# See the data dictionary for specifications of variables:
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019.pdf
# The 11 PUMAs in the get_pums call cover the whole of Philadelphia exclusively; 
# for descriptions and maps of PUMAs, see:
# https://www.census.gov/geographies/reference-maps/2010/geo/2010-pumas.html

data_raw <- get_pums(variables = c("PUMA", "VEH", "TEN", "HINCP"),
              state = "Pennsylvania",
              puma = c("03201", "03202", "03203", "03204", "03205", "03206", 
                       "03207","03208","03209","03210","03211"),
              year = 2019,
              survey = "acs5",
              recode = TRUE,
              rep_weights = "housing"
              )

saveRDS(data_raw, here("data", "data_raw.RDS"))

# Clean data-------------------------------------------------------------------

# HINCP values of -60000 are GQ/Vacant households
#
# check <- data_raw %>% 
#   filter(HINCP == -60000)

data_selected <- data_raw %>% 
  distinct(SERIALNO, .keep_all = TRUE) %>% # we only have household-level variables
  filter(HINCP != -60000) %>%              # filtering for occupied housing units
  select(SERIALNO, WGTP, PUMA, VEH, TEN, HINCP) %>% 
  mutate(tenure = recode(TEN, "1" = "homeowner", "2" = "homeowner",
                           "3" = "renter", "4" = "renter")) %>%
  mutate(income = cut(HINCP,
                      breaks = c(-59999, 19320, 38640, 57960, 77280, Inf),
                      labels = c("AMI_20", "AMI_40", "AMI_60", "AMI_80", "AMI_80plus"),
                      ordered_result = TRUE)
        ) 
# AMI_20 means 'household income at or below 20% of Philadelphia Metro AMI'
# AMI_80plus means 'household income greater than 80% of Philadelphia Metro AMI'
# AMI and thresholds taken from PFHA's income limits table which uses the values
# published by HUD (see column 'Median Inc'): 
# https://www.phfa.org/forms/housing_management/tax_credits/rent_and_income_limits/2020_mtxr041.pdf

saveRDS(data_selected, here("data", "data_selected.RDS"))

# Tabulate data-----------------------------------------------------------------

long <- data_selected %>% 
  count(PUMA, tenure, income, VEH, wt = WGTP) # weighting by household-level weight 

saveRDS(long, here("data", "Table_1_original_long.RDS"))

# Pivot the VEH variable to columns, 
# Calculate total households per PUMA / Tenure / Income category,
# Calculate proportions for each row
full <- long %>% 
  pivot_wider(names_from = VEH, names_prefix = "vehicles_", 
              values_from = n, values_fill = 0) %>% 
  mutate(total = rowSums(select(., vehicles_0:vehicles_6))) %>% 
  mutate(across(.cols = starts_with("vehicles"), 
                .fns = ~ . / total, 
                .names = "{.col}_prop")) %>% 
  mutate(across(.cols = contains("prop"),
                .fns = ~ round(., 4)))

saveRDS(full, here("data", "Table_2_full.RDS"))

# Break out tables--------------------------------------------------------------

# Separate tables for renter and homeowner households
renter_households_by_PUMA <- full %>% 
  filter(tenure == "renter")

homeowner_households_by_PUMA <- full %>% 
  filter(tenure == "homeowner")

# Total counts/proportions for each table
renter_households_total <- long %>% 
  filter(tenure == "renter") %>% 
  group_by(tenure, income, VEH) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = VEH, names_prefix = "vehicles_", 
              values_from = n, values_fill = 0) %>% 
  mutate(total = rowSums(select(., vehicles_0:vehicles_6))) %>% 
  mutate(across(.cols = starts_with("vehicles"), 
                .fns = ~ . / total, 
                .names = "{.col}_prop")) %>% 
  mutate(across(.cols = contains("prop"),
                .fns = ~ round(., 4)))

homeowner_households_total <- long %>% 
  filter(tenure == "homeowner") %>% 
  group_by(tenure, income, VEH) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = VEH, names_prefix = "vehicles_", 
              values_from = n, values_fill = 0) %>% 
  mutate(total = rowSums(select(., vehicles_0:vehicles_6))) %>% 
  mutate(across(.cols = starts_with("vehicles"), 
                .fns = ~ . / total, 
                .names = "{.col}_prop")) %>% 
  mutate(across(.cols = contains("prop"),
                .fns = ~ round(., 4)))
  
# Documentation sheet

information <- c("These tabulations show car ownership of renter and homeowner households in Philadelphia, broken out by income level. Renter and homeowner households are broken out into separate tables. For each of those tables, you can also find the data broken out by geographical areas, called PUMAs (see below for details).",
          "Each of the tables shows the estimated count of households at each income level that own 0 vehicles, 1 vehicle, 2 vehicles, and so forth up to 6+ vehicles. Then it shows the proportion of households at each income level that owns 0 vehicles, 1 vehicle, &c.",
          "",
          "Data sources:",
          "The raw data come from the Census Bureau, specifically ACS public use microdata (PUMS), 5-year, vintage 2019",
          "For descriptions and maps of PUMAs, see:",
          "https://www.census.gov/geographies/reference-maps/2010/geo/2010-pumas.html",
          "",
          "Income levels are calculated according to percentages of the Philadelphia Metro Area Median Income. For example, AMI_20 means 'household income at or below 20% of Philadelphia Metro AMI' and AMI_80plus means 'household income greater than 80% of Philadelphia Metro AMI'.",
          "The AMI value is taken from PFHA's income limits table, which uses the values published by HUD (see column 'Median Inc'):",
          "https://www.phfa.org/forms/housing_management/tax_credits/rent_and_income_limits/2020_mtxr041.pdf",
          "For Philadelphia: 20% AMI = $19.320, 40% AMI = $38,640, 60% AMI = $57,960, 80% AMI = $77,280",
          "",
          "For the R code that generated these tables, see:",
          "https://github.com/lyriodendron/vehicles_tenure_income",
          "",
          "If you have any questions about the data or want to suggest a different tabulation, please contact me at chhykim@gmail.com"
          )

documentation <- tibble(information)

# Write to excel file----------------------------------------------------------

sheet_list <- list(`Documentation` = documentation, 
                   `Renter households by PUMA` = renter_households_by_PUMA, 
                   `Renter households (total)` = renter_households_total,
                   `Homeonwer households by PUMA` = homeowner_households_by_PUMA,
                   `Homeowner households (total)` = homeowner_households_total)

wb <- write.xlsx(sheet_list, "PHL car ownership by housing tenure and income.xlsx")
freezePane(wb, 2, firstActiveRow = 2, firstActiveCol = 4)
freezePane(wb, 3, firstActiveRow = 2, firstActiveCol = 3)
freezePane(wb, 4, firstActiveRow = 2, firstActiveCol = 4)
freezePane(wb, 5, firstActiveRow = 2, firstActiveCol = 3)
setColWidths(wb, 1, cols = 1, widths = 85)
wrap <- createStyle(wrapText = TRUE)
addStyle(wb, 1, wrap, rows = 1:17, cols = 1)
setColWidths(wb, 2, cols = 1:18, widths = "auto")
setColWidths(wb, 3, cols = 1:17, widths = "auto")
setColWidths(wb, 4, cols = 1:18, widths = "auto")
setColWidths(wb, 5, cols = 1:17, widths = "auto")
saveWorkbook(wb, "PHL car ownership by housing tenure and income.xlsx", 
             overwrite = TRUE, returnValue = TRUE)
