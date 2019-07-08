library(tidyverse)
library(here)

nathans_raw <- read_csv(here::here("raw", "nathans_scores.csv"))

nathans <- nathans_raw %>% 
  fill(year, gender) %>% 
  mutate(number = as.double(str_extract(score, "[1-9]\\d*(\\.\\d+)?$")),
         name = str_to_title(str_trim(str_extract(score, "[a-zA-Z\\s\\-\\.]*")))) %>% 
  select(-score) %>% 
  group_by(year) %>% 
  mutate(rank_by_year = dense_rank(-number)) %>% 
  group_by(year, gender) %>% 
  mutate(rank_by_year_and_gender = dense_rank(-number),
         number_competitors = n()) %>% 
  filter(!is.na(year))

nathans_winners <- nathans %>% 
  ungroup() %>% 
  filter(rank_by_year == "1") %>% 
  select(year, number, name)

write.csv(nathans_winners, here::here("output", "nathans_winners.csv"))

nathans %>% 
  filter(rank_by_year == "1") %>% 
  ggplot(aes(year, number, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "For the past 20 years, Nathan's Hot Dog contest has been dominated by two competitors",
       caption = "Source: nathansfamous.com. Note: In 2008, the tie was broken when Joey Chestnut won in overtime",
       x = "",
       y = "Number of hot dogs eaten by winner")

nathans %>% 
  filter(rank_by_year_and_gender == "1",
         gender != "mixed") %>% 
  ggplot(aes(year, number, fill = gender)) +
  geom_col() +
  facet_wrap(~ gender) +
  labs(title = "Nathan's Hot Dog contest organizers split  men and women into separate contests in 2011",
       caption = "Source: nathansfamous.com",
       x = "",
       y = "Number of hot dogs eaten by winner")
