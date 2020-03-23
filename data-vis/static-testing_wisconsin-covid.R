library(tidyverse)
# theme_set(theme_minimal())
library(sf)
library(plotly)


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
  mutate(county2 = county %>% 
           str_remove(" County") %>% 
           str_remove_all("\\.") %>% 
           str_to_lower()
  )


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
  
  
  
p <- 
  ggplot(data = plot_data,
         aes(label = county.y,
             text = paste(
               "</br>", county.y,
               "</br>Cases: ", cases
             ))) +
  geom_sf(aes(fill = cases)) +
  geom_point(aes(long, lat), size = 3, alpha = 0) +
  scale_fill_distiller(trans = "log",
                       palette = "YlOrRd",
                       na.value = "#F2F2F2",
                       direction = 1,
                       breaks = c(1,4,10,40,100)
  ) + 
  theme_void()

p +
  geom_point(aes(long, lat), size = 3, color = rgb(0,0,0,0))
  theme_minimal()


ggplotly(p, tooltip = c("text"))


p <- ggplot(plot_data,
            aes(label = Category,
                text = paste(# "Category: ", category,
                  "Number of reviews: ", n,
                  "</br>Price: ", price))
) +
  geom_bar(aes(asin, avg_score), fill = "#b3b3b3", stat = "identity") +
  geom_text(stat = "identity",
            aes(x = asin, label= paste0("<b>",description,"</b>")),
            y = 0,
            color="black", size = 3.5
  ) +
  coord_flip() +
  scale_x_discrete(limits = ordering) +
  scale_y_continuous(limits = c(0,5), expand = c(0.01,0)) +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank()) +
  labs(
    x = NULL,
    y = "Avg. Rating"
  )

ggplotly(p, tooltip = c("label","text")) %>%
  style(textposition = "right")

map_data("world")








