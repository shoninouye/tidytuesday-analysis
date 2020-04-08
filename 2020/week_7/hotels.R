library(tidyverse)
library(lubridate)
library(zoo)

hotel_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

hotel_data %>% View()

hotel_data %>%
  filter(!is_canceled) %>% 
  count(reservation_status_date) %>% 
  ggplot(aes(reservation_status_date, n)) +
  geom_line()


hotel_data %>%
  filter(!is_canceled) %>% 
  mutate(year_month = as.yearmon(paste(year(reservation_status_date), month(reservation_status_date), sep = '-'),
                              '%Y-%m')) %>%
  ggplot(aes(year_month, stays_in_week_nights, group = year_month)) +
  geom_boxplot()
