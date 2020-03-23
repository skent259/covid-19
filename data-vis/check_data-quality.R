

library(tidyverse)
# library(sf)
# library(maps)
# library(plotly)
# library(here)


timeseries <- read.csv("data/timeseries-tidy.csv")


## check that all 50 states are represented
timeseries %>% 
  filter(country == "USA") %>% 
  pull(state) %>% 
  unique() %>% 
  sort() 
  
# one blank (which is USA overall)
# AS = American Samoa
# DC = District of Columbia
# GU = Guam
# MP = Northern Marianas
# PR = Pureto Rico
# VI = Virgin Islands 
# 50 states + 7 others = 57 total 

  
## check the amount of county information that is not blank for each state
county_level_info <- timeseries %>% 
  filter(country == "USA" & state != "") %>% 
  select(city, county, state, country) %>% 
  filter(county != "") %>%  # remove the state level information
  unique()

county_level_info %>% 
  janitor::tabyl(state, show_missing_levels = FALSE) %>% 
  arrange(desc(n))

# not complete information, some states are missing the county level data.  
# don't have a quick way to decide which states have complete county data, 
# but it seems like many are okay

# notable missing states: VA, NE




## CHECK: county data adds up to state data 

county_aggregate_counts <-
  timeseries %>% 
    as_tibble() %>% 
    filter(country == "USA" & state != "" & county != "") %>% 
    filter(date == "2020-03-21") %>% 
    pivot_wider(
      names_from = "type",
      values_from = "value"
    ) %>% 
    group_by(state) %>% 
    summarize(cases = sum(cases)) %>% 
    arrange(desc(cases))
 
state_counts <-  
  timeseries %>% 
    as_tibble() %>% 
    filter(country == "USA" & state != "" & county == "") %>% 
    filter(date == "2020-03-21") %>% 
    pivot_wider(
      names_from = "type",
      values_from = "value"
    ) %>% 
    select(state, cases) %>% 
    arrange(desc(cases))
  
left_join(state_counts, county_aggregate_counts, by = "state") %>%
  mutate(equal = cases.x == cases.y) %>% 
  print(n = Inf)

## CHECK: USA data (region level)

timeseries %>% 
  as_tibble() %>% 
  filter(country == "USA" & state == "" & county == "")

# only 9 rows (2 time points).  Very strange, must not have a national data scraper at this point 


