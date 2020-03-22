library(tidyverse)
theme_set(theme_minimal())
library(sf)

# timeseries <- read_csv("data/timeseries-tidy.csv")
timeseries <- read.csv("data/timeseries-tidy.csv")

wi_timeseries <- 
  timeseries %>% 
  filter(state == "WI") %>% 
  as_tibble() %>% 
  pivot_wider(names_from = "type", 
              values_from = "value")

wi_timeseries <- 
  wi_timeseries %>% 
  mutate(county2 = str_to_lower(str_remove(county, " County")))


skimr::skim(wi_timeseries)
unique(wi_timeseries$county2)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))

wi_county <- 
  maps::map("county", plot = FALSE, fill = TRUE) %>% 
  st_as_sf() %>% 
  subset(grepl("wisconsin", .$ID)) %>% 
  mutate(county = str_replace(ID, "wisconsin,", ""))


date_to_plot <- "2020-03-21"

plot_data <- 
  wi_county %>% 
  left_join(filter(wi_timeseries, date == date_to_plot),
            by = c("county" = "county2"))
  


ggplot(data = plot_data) +
  geom_sf(aes(fill = cut(cases, 4))) +
  theme_void()








