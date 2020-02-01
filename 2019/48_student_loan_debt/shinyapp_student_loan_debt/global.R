# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forcats)
library(DT)

# Load data
loans <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
head(loans)

# Percentage of contribution to total loan payments
loan_payments <- loans %>% 
  filter(!is.na(rehabilitation)) %>% 
  mutate(year = year + 2000,
         month = case_when(quarter == 1 ~ 3,
                           quarter == 2 ~ 6,
                           quarter == 3 ~ 9,
                           quarter == 4 ~ 12),
         date = ceiling_date(ymd(paste(year,month,"01",sep="-")), "month") - 1) %>% 
  group_by(date) %>%
  summarize(pct_consolidation = sum(consolidation)/sum(total),
            pct_rehabilitation = sum(rehabilitation)/sum(total),
            pct_voluntary_payments = sum(voluntary_payments)/sum(total),
            pct_wage_garnishments = sum(wage_garnishments)/sum(total)) %>%
  gather(key = payment_type, value = pct_paid, -date) 