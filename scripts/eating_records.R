library(tidyverse)
library(here)

### MLE RECORDS ###

mle_records_raw <- read_csv(here::here("raw", "MLE_original.csv"), col_names = F)

mle_records <- mle_records_raw %>% 
  separate(X1, c("food", "other"), sep = "\\*\\s") %>% 
  separate(other, c("record_amount", "other"), sep = "\n\n", extra = "merge") %>%
  separate(other, c("record_time", "name"), sep = "\n\n") %>%
  separate(record_time, c("record_time", "date"), sep = "/") %>%
  separate(record_amount, c("record_amount", "competition_name"), sep = "/") %>%
  mutate(food = str_remove(food, "\\*"),
         year = year(mdy(date))) %>% 
  mutate_all(str_trim)

mle_records %>% 
  filter(!is.na(name)) %>% 
  count(name, sort = T) %>% 
  mutate(name = fct_reorder(name, n)) %>%
  top_n(10) %>% 
  ggplot(aes(name, n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Competitive eaters with the most number of world records")

### APE RECORDS ###

ape_raw <- read_csv(here::here("raw", "APE_original.csv"), col_names = F)

ape_records <- ape_raw %>% 
  mutate(amount = paste(lead(X1)),
         place = paste(lead(X1, 2))) %>% 
  slice(seq(1, n(), by = 3)) %>% 
  separate(place, into = c("year", "venue"), sep = "\\s", extra = "merge") %>% 
  separate(X1, into = c("food", "name"), sep = "\\-", extra = "merge") %>% 
  separate(amount, into = c("amount", "time"), sep = "\\s/\\s") %>% 
  mutate(time = str_remove(time, "-"),
         name = str_remove(name, "-")) %>% 
  mutate_all(str_trim) %>% 
  select(food, record_amount = amount, competition_name = venue, record_time = time, year, name)

write.csv(ape_records, here::here("output", "ape_records_clean.csv"), row.names = F)

### BIND MLE AND APE ###

all_records <- bind_rows(mle_records, ape_records)

top_ten <- all_records %>% 
  count(name, sort = T) %>% 
  filter(n > 6)

write.csv(top_ten, here::here("output", "top_ten.csv"))

top_ten %>% 
  mutate(name = fct_reorder(name, n)) %>% 
  ggplot(aes(name, n)) + 
  geom_col() +
  coord_flip() +
  labs(title = "Top ten eaters with the most world records",
       subtitle = "According to Major League Eating and All Pro Eating records",
       x = "Number of world records",
       y = "")