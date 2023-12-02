## day 2 ## 
library(tidyverse)
library(tidyr)
data <- read.csv("day 2/data .csv", header = FALSE)

clean_data <- data %>%
  separate(V1, into = c("game", "details"), sep = ": ", extra = "merge") %>%
  mutate(details = str_replace_all(details, "[^\\w\\s]", ""), # Remove punctuation from details
         game = str_trim(game), # Trim spaces from game
         details = str_trim(details)) # Trim spaces from details

# Convert the string into a tibble and then into long format
long_format_data <- clean_data %>%
  separate_rows(details, sep = "\\s+(?=[0-9])") %>%
  separate(details, into = c("value", "colour"), sep = "\\s+", convert = TRUE) %>%
  mutate(game_number = as.integer(str_extract(game, "\\d+")))

wrong_games <- long_format_data %>% 
  filter((colour == "red" & value >= 13)|
         (colour == "blue" & value >= 15)|
         (colour == "green" & value >= 14)) %>% 
  distinct(game_number)

long_format_data %>% filter(!game_number %in% wrong_games$game_number) %>% 
  distinct(game_number) %>% 
  sum()


answer2 <- long_format_data %>% 
  group_by(game_number, colour) %>% 
  filter(value == max(value)) %>% 
  distinct(game_number,colour,value, .keep_all = TRUE) %>% 
  ungroup() %>% 
  group_by(game_number) %>% 
  mutate(products = prod(value)) %>% 
  ungroup() %>% 
  distinct(game_number, products) %>% 
  mutate(prodsum = sum(products))

         