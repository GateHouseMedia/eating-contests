library(tidyverse)
library(here)
library(lubridate)

# WING BOWL

wing_bowl_raw <- read_csv("raw/wing_bowl.csv")

wing_bowl_clean <- wing_bowl_raw %>% 
  mutate(year = year(mdy(date)),
         name = str_remove(name, "\\[5]"),
         contest = "wing bowl") %>% 
  select(-date)

wing_bowl_clean %>% 
  filter(place == 1) %>% 
  mutate(name = fct_lump(name, 4)) %>% 
  ggplot(aes(year, number_wings, fill = name)) +
  geom_col() +
  expand_limits(y = 0) +
  labs(caption = "Source: wingbowl.radio.com",
       title = "The Wing Bowl, by contrast, has been dominated by different people over the years",
       subtitle = "Molly Schuyler, the current champion, ate 501 wings in 30 minutes last year",
       x = "",
       y = "Number of chicken wings eaten")

wing_bowl_winners <- wing_bowl_clean %>% 
  select(year, name, number_wings)

write.csv(wing_bowl_winners, here::here("output", "wing_bowl_winners.csv"), row.names = F)


# HOOTERS

hooters <- read_csv("raw/hooters.csv")

hooters %>% 
  filter(rank == 1) %>% 
  ggplot(aes(year, number_wings, fill = name)) +
  geom_col() 

write.csv(hooters, here::here("output", "hooters_winners.csv"), row.names = F)

# WING FESTIVAL

buffalo_wing_festival_raw <- read_csv("raw/buffalo_wing_festival.csv")

wing_festival <- buffalo_wing_festival_raw %>% 
  clean_names() %>% 
  mutate(year = year(mdy(date)),
         number_wings = as.numeric(chicken_wings_eaten),
         contest = "buffalo wing festival") %>% 
  fill(name) %>% 
  select(-x1, -chicken_wings_eaten, -date) %>% 
  mutate(rank = 1)

write.csv(wing_festival, here::here("output", "wing_festival_winners.csv"))

wing_festival %>% 
  ggplot(aes(year, number_wings, fill = name)) +
  geom_col() +
  expand_limits(y = 0)

# COMBINE THEM 

wings_combined <- bind_rows(wing_bowl_clean, wing_festival, hooters) %>% 
  mutate(time = case_when(contest == "hooters" ~ 10,
                          contest == "buffalo wing festival" ~ 12,
                          TRUE ~ 30),
         wings_per_min = number_wings/time,
         secs_per_wing = 60/wings_per_min)

wings_combined %>% 
  mutate(contest = str_replace(contest, "hooters", "Hooters Wing Championship (10 minutes)")) %>% 
  mutate(contest = str_replace(contest, "buffalo wing festival", "Buffalo Wing Festival (12 minutes)")) %>% 
  mutate(contest = str_replace(contest, "wing bowl", "Wing Bowl (30 minutes)")) %>% 
  filter(rank == "1") %>% 
  ggplot(aes(year, wings_per_min, color = contest)) +
  geom_line() +
  geom_point() +
  expand_limits(y = 0) +
  geom_text(data = wings_highlight, label = wings_highlight$name, check_overlap = T, vjust = 1, hjust = 1) +
  labs(title = "There are three major wing-eating competitions",
       subtitle = "Over time, competitors have been eating wings faster and faster. The shorter the competition time, the faster the winners eat.",
       x = "",
       y = "Wings eaten per minute") 

wings_per_minute <- wings_combined %>% 
  mutate(contest = str_replace(contest, "hooters", "Hooters Wing Championship (10 minutes)")) %>% 
  mutate(contest = str_replace(contest, "buffalo wing festival", "Buffalo Wing Festival (12 minutes)")) %>% 
  mutate(contest = str_replace(contest, "wing bowl", "Wing Bowl (30 minutes)")) %>% 
  filter(rank == "1") %>% 
  select(year, name, contest, wings_per_min)

write.csv(wings_per_minute, here::here("output", "wings_per_minute.csv"), row.names = F)

