library(tidyverse)


timeseries_tidy <- 
  read.csv("data/timeseries-tidy.csv") %>%
  as_tibble()


leading_digit <- function(x) {
  str_extract(as.character(x), "\\d")
}

timeseries_tidy %>% 
  filter(country == "CHN") %>% 
  filter(state == "") %>% 
  filter(type == "cases") %>% 
  filter(value > 0) %>% 
  mutate(ld = leading_digit(value)) %>% 
  # filter(ld == "0")
  ggplot(aes(ld)) +
  geom_bar()


timeseries_tidy %>% 
  filter(country == "IRN") %>% 
  filter(type == "cases") %>% 
  filter(value > 0) %>% 
  mutate(ld = leading_digit(value)) %>% 
  # filter(ld == "0")
  ggplot(aes(ld)) +
  geom_bar()


timeseries_tidy %>% 
  filter(country == "IRN") %>% 
  filter(type == "cases") %>% 
  filter(value > 0) %>% 
  print(n = Inf)
