library(data.table)
library(readxl)
library(tidyverse)

# Set working directory
setwd("c:/cde/trade/canada")

# Load datasets
ustrade <- readxl::read_xlsx("usatrade_data.xlsx") %>%
  as.data.table() %>%
  select(state, value, code = commodity)

canada_tarrif <- readxl::read_xlsx("canada_list.xlsx") %>%
  as.data.table() %>%
  filter(subgroup == "00") %>%
  select(code, phase=stage)
# obtainer from: 
# first round: https://www.canada.ca/en/department-finance/news/2025/03/list-of-products-from-the-united-states-subject-to-25-per-cent-tariffs-effective-march-4-2025.html
#https://www.canada.ca/en/department-finance/programs/consultations/2025/notice-intent-impose-countermeasures-response-united-states-tariffs-on-canadian-goods.html


# Merge datasets and create tariff indicators with corrected variable name "tariffed"
result <- ustrade %>%
  left_join(canada_tarrif, by = "code") %>%
  mutate(tariffed = as.numeric(!is.na(phase)),
         phase1   = as.numeric(phase == 1 & !is.na(phase)),
         phase2   = as.numeric(phase == 2 & !is.na(phase))) %>%
  filter(!state %in% c("All States", "Unknown"))

options(digits = 2)

# Aggregate trade value by state and code
agg <- result %>%
  group_by(state, code) %>%
  summarise(tariffed = max(tariffed),
            phase2   = max(phase2),
            value    = sum(value, na.rm = TRUE),
            .groups  = "drop")

# Compute market share and HHI per code
res <- agg %>%
  group_by(code) %>%
  mutate(market_share = value / sum(value)) %>%
  summarise(hhi      = sum(market_share^2) * 10000,
            value    = sum(value),
            tariffed = max(tariffed),
            phase2   = max(phase2),
            .groups  = "drop")

res %>% fwrite('combined_phase_value.csv')
