# Load packages
library(tidyverse)

# Load data
tdf_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv")
head(tdf_winners)

# Plot of wins by winner's birth country
tdf_winners %>% 
  count(birth_country, sort = TRUE) %>% 
  mutate(birth_country = fct_reorder(birth_country, n)) %>% 
  ggplot(aes(x = birth_country, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "What countries were the most Tour de France winners born in?",
       x = "",
       y = "Number of Wins")
  
# Age distribution of winners
tdf_winners %>% 
  count(age, sort = TRUE) %>% 
  ggplot(aes(x = age, y = n)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(18,36,2)) +
  scale_y_continuous(breaks = seq(0,12,4)) +
  labs(title = "What is the age distrubtion of Tour de France winners?",
       x = "Age",
       y = "Number of Winners")

# Race completion time over time
tdf_winners %>% 
  ggplot(aes(x = start_date, y = time_overall)) + 
  geom_point(color = "steelblue", alpha = .8, stroke = .8) + 
  theme_minimal() +
  labs(title = "How has race completion time changed over time?",
       x = "Year",
       y = "Race Time (in hours)")

# Race distance over time
tdf_winners %>% 
  ggplot(aes(x = start_date, y = distance)) + 
  geom_point(color = "steelblue", alpha = .8, stroke = .8) + 
  theme_minimal() +
  labs(title = "How has race distance changed over time?",
       x = "Year",
       y = "Distance (in km)")

# Winner's average speed
tdf_winners %>% 
  mutate(avg_speed = distance/time_overall) %>% 
  ggplot(aes(x = start_date, y = avg_speed)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "How has the Tour de France winner's average speed changed over time?",
       x = "Year",
       y = "Average Speed (km/hr)")


# Runner up finishing time difference
tdf_winners %>% 
  ggplot(aes(x = start_date, y = time_margin)) + 
  geom_point(color = "steelblue", alpha = .8, stroke = .8) + 
  scale_y_sqrt(breaks = c(0.01, 0.1, 1, 2, 3)) + 
  expand_limits(y = 0.1) +
  theme_minimal() +
  labs(title = "How has the finishing time between the race winner and \nthe runner up changed over time?",
       x = "Year",
       y = "Hours")

# Smallest runner up finishing time difference
tdf_winners %>% 
  select(start_date, winner_name, time_margin) %>% 
  arrange(time_margin) %>%
  top_n(-5)
        