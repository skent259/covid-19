#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(maps)
library(plotly)
library(here)
library(ggrepel)

# timeseries <- read.csv("data/timeseries-tidy.csv")
# system.time(timeseries <- read.csv("../data/timeseries-tidy.csv"))

wi_timeseries <- 
    read.csv("data/timeseries-tidy.csv") %>% 
    filter(state == "WI") %>% 
    as_tibble() %>% 
    pivot_wider(names_from = "type", 
                values_from = "value") %>% 
    # adjust county name to match maps data
    mutate(
        county2 = county %>% 
            str_remove(" County") %>% 
            str_remove_all("\\.") %>% 
            str_to_lower(),
        date = as.Date(date)
    )

wi_county <- 
    maps::map("county", plot = FALSE, fill = TRUE) %>% 
    st_as_sf() %>% 
    subset(grepl("wisconsin", .$ID)) %>% 
    mutate(county = str_replace(ID, "wisconsin,", ""))

#########################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Wisconsin COVID-19 Data"),
    p(
        "This dashboard shows the county-level COVID-19 data for Wisconsin.  The data comes from ",
        a("Corona Data Scraper.", href = "https://coronadatascraper.com/#home", target = "_blank"),
        # "The options below offer some additional insight into the data available.  ",
        "Data is current as of ", paste0(max(wi_timeseries$date), "."),
        "Bugs and feature ideas can be reported at ", 
        a("https://github.com/skent259/covid-19.", href = "https://github.com/skent259/covid-19", target = "_blank")
    ),
    
    fluidRow(
        column(width = 2,
               # h3("Info:"),
               # p(
               #     "This dashboard shows the county-level COVID-19 data for Wisconsin.  The data comes from ",
               #     a("Corona Data Scraper.", href = "https://coronadatascraper.com/#home", target = "_blank"),
               #     # "The options below offer some additional insight into the data available.  ",
               #     "Data is current as of ", max(wi_timeseries$date)
               # ),
               # hr(),
               h3("Choose the data to visualize:"),
               selectizeInput(
                   "info_to_plot", 
                   "Select from Cases, Deaths, Recovered:",
                   c("Cases" = "cases",
                     "Deaths" = "deaths",
                     "Recovered" = "recovered",
                     "Active" = "active"),
                   selected = "cases"
               ),
               sliderInput("date_to_plot",
                           "Select a date:",
                           min = as.Date("2020-03-01","%Y-%m-%d"),
                           max = as.Date(max(wi_timeseries$date), "%Y-%m-%d"),
                           value=as.Date(max(wi_timeseries$date)),
                           timeFormat="%Y-%m-%d"),
               # textOutput("info"),
               selectInput(
                   "counties_to_plot",
                   "Select counties to display:",
                   unique(wi_timeseries$county),
                   multiple = TRUE,
                   selected = c("Milwaukee County", "Dane County", "Waukesha County")
               ),
               checkboxInput("logscale", "Show Cases on a log scale", FALSE),
               br()
        ),
        column(width = 6,
               plotlyOutput("WI_map")
        ),
        column(width = 4,
               align="center",
               # h3("Add cases over time chart")
               plotOutput("WI_cases_plot",
                          height = "300px"),
               br(),
               plotOutput("WI_cases_plot_county",
                          height = "300px"),
               selectInput(
                   "WI_cases_plot_county_xaxis",
                   label = NULL,
                   c("Date", "Days Since 10 Cases"),
                   width = "200px"
               ),
               br()
        )
    )
    
    # selectizeInput(
    #     "category_name",
    #     "Select one of Amazon's categories to view",
    #     # "",
    #     category_choices,
    #     selected = "Home & Kitchen"
    # )),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$WI_map <- renderPlotly({
        
        # date_to_plot <- "2020-03-21"
        # info_to_plot <- "cases"
        date_to_plot <- input$date_to_plot
        info_to_plot <- input$info_to_plot
        highest_value_county <- wi_timeseries %>% 
            filter(county != "") %>% 
            pull(info_to_plot) %>% 
            max()
        
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
            geom_sf(aes_(fill = as.name(info_to_plot))) +
            geom_sf(aes_(fill = as.name(info_to_plot)), size = 1.05,
                    data = . %>% filter(county.y %in% input$counties_to_plot)) +
            geom_point(aes(long, lat), size = 5, alpha = 0) +
            scale_fill_distiller(trans = "log",
                                 palette = "YlOrRd",
                                 na.value = "#F2F2F2",
                                 direction = 1,
                                 breaks = 10^(0:3),
                                 limits = c(NA, highest_value_county)
            ) +
            theme_void() +
            labs(title = "Wisconsin COVID-19 Map of Cases")
        
        ggplotly(p, tooltip = c("text"),
                 width = 640, height = 640)
        
    })
    
    
    output$WI_cases_plot <- renderPlot({
        p <- 
            wi_timeseries %>%
            filter(county == "") %>%
            ggplot(aes(date, cases)) +
            geom_line(size = 1.5) +
            scale_x_date(limits = c(Sys.Date() - 14, NA), expand = c(0.06,0,0.06,2),
                         date_breaks = "2 days", date_labels = "%b %d") +
            labs(
                title = "Wisconsin Cases of COVID-19 (last 2 weeks)",
                y = "Cases",
                x = NULL
            ) +
            theme_minimal()
        
        if(input$logscale) {
            p <- p +
                scale_y_log10()
        }
        
        p
    })
    
    output$WI_cases_plot_county <- renderPlot({
        
        # counties_to_plot <- c("Dane County", "Milwaukee County", "Waukesha County")
        counties_to_plot <- input$counties_to_plot
        
        if (input$WI_cases_plot_county_xaxis == "Date") {
            p <- 
                wi_timeseries %>%
                filter(county %in% counties_to_plot) %>%
                ggplot(aes(date, cases)) +
                geom_line(aes(color = county), size = 1.5) +
                scale_x_date(limits = c(Sys.Date() - 14, NA), expand = c(0.06,0,0.06,2),
                             date_breaks = "2 days", date_labels = "%b %d") +
                labs(
                    title = "County Level Wisconsin Cases of COVID-19 (last 2 weeks)",
                    y = "Cases",
                    x = NULL
                ) +
                theme_minimal() +
                theme(legend.position = "none")
            
        } else if (input$WI_cases_plot_county_xaxis == "Days Since 10 Cases") {
            p <- 
                wi_timeseries %>% 
                filter(county %in% counties_to_plot) %>% 
                group_by(county, state) %>% 
                filter(cases >= 10) %>% 
                arrange(county, state, date) %>% 
                mutate(days_since_cases = row_number()) %>% 
                ungroup() %>% 
                ggplot(aes(days_since_cases, cases)) +
                geom_line(aes(color = county), size = 1.5) +
                scale_x_continuous(breaks = seq(1,101,by = 2), expand = c(0.06,0,0.06,2)) +
                labs(
                    title = "County Level Wisconsin Cases of COVID-19 (last 2 weeks)",
                    y = "Cases",
                    x = NULL
                ) +
                theme_minimal() +
                theme(legend.position = "none")
        }
        
        
        if(input$logscale) {
            p <- p +
                scale_y_log10() +
                geom_label_repel(aes(label = str_remove(county, " County")),
                                 data = . %>% filter(date == max(wi_timeseries$date)),
                                 nudge_x = 1.5
                )
        } else {
            p <- p + 
                geom_label_repel(aes(label = str_remove(county, " County")),
                                 data = . %>% filter(date == max(wi_timeseries$date)),
                                 nudge_x = 1.5,
                                 nudge_y = 5
                )
        }
        
        p
        
    })
    
    output$info <- renderPrint({
        event_data("plotly_click")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
