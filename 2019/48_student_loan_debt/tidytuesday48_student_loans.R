# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forcats)

# Load data
loans <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")
head(loans)
  
# View number of default recoveries per quarter
loans %>% 
  count(year, quarter) %>% 
  mutate(year_quarter = paste0(as.character(2000+year), " Q", as.character(quarter))) %>% 
  ggplot(aes(year_quarter, n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 315, hjust = 0))


# Loans repaid over time, faceted by quarter
loans %>% 
  group_by(year, quarter) %>% 
  summarize(total_per_quarter = sum(total)) %>% 
  ggplot(aes(year, total_per_quarter)) +
  geom_col() +
  facet_wrap(~quarter)
  
# Check NA values 
colSums(is.na(loans))

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

# Plot percentage of contribution to total loan payments
loan_payments %>% 
  ggplot(aes(x = date, y = pct_paid, color = payment_type)) +
  geom_line(size = 2) +
  scale_y_continuous(label = scales::percent) + 
  labs(x = "Time",
       y = "Percent of total loan payments",
       color = "Payment Type",
       title = "Contribution to Student Loan Payments",
       subtitle = "Percentage of student loan repayment type contribution at the end of each fiscal quarter (3 months)") +
  scale_color_brewer(labels = c("Consolidation", "Rehabilitation", "Voluntary Payments", "Wage Garnishments"),
                     palette = "Paired") +
  theme_minimal()

