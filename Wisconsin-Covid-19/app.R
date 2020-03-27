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
    read.csv("data/timeseries-tidy_wi_manual.csv") %>% 
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
        date = as.Date(date),
        cases_per_cap = cases / population * 100000 
    )

wi_county <- 
    maps::map("county", plot = FALSE, fill = TRUE) %>% 
    st_as_sf() %>% 
    subset(grepl("wisconsin", .$ID)) %>% 
    mutate(county = str_replace(ID, "wisconsin,", ""))

info_to_plot_options = c(
    "Cases" = "cases",
    "Cases per 100K people" = "cases_per_cap"
    # "Deaths" = "deaths",
    # "Recovered" = "recovered",
    # "Active" = "active"
)

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
               h3("Choose the data to visualize:"),
               selectizeInput(
                   "info_to_plot", 
                   "Select from Cases or Cases per 100K people:",
                   info_to_plot_options,
                   selected = "cases_per_cap"
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    wi_ts <- reactive({
        ## create variable `value` based on info_to_plot
        ## create text to layer into plot_ly hover visuals
        info_to_plot <- input$info_to_plot
        info_to_plot_name <- names(info_to_plot_options[which(info_to_plot_options == info_to_plot)])
        
        wi_ts <- 
            wi_timeseries %>% 
            mutate(value = !!as.name(info_to_plot),
                   value = round(value,1),
                   info_to_plot = info_to_plot,
                   text = paste0("</br>", county,
                                 "</br>Cases: ", cases,
                                 if_else(info_to_plot == "cases",
                                         "",
                                         paste0("</br>", info_to_plot_name, ": ", value))))
    })
    
    info_to_plot_name <- reactive({
        info_to_plot <- input$info_to_plot
        info_to_plot_name <- names(info_to_plot_options[which(info_to_plot_options == info_to_plot)])
    })
    
    output$WI_map <- renderPlotly({
        
        date_to_plot <- input$date_to_plot
        info_to_plot <- input$info_to_plot
        info_to_plot_name <- info_to_plot_name()
        wi_ts <- wi_ts()
        
        plot_data <- 
            wi_county %>% 
            left_join(filter(wi_ts, date == date_to_plot),
                      by = c("county" = "county2")) %>% 
            mutate(value = ifelse(value==0, NA, value))
        
        suppressWarnings({
            plot_data %>% 
                plot_ly(split = ~county.y, 
                        color = ~log(value+1),
                        colors = "YlOrRd", 
                        span = I(1),
                        stroke = I("gray50"),
                        alpha = 1,
                        text = ~text, 
                        hoverinfo = "text",
                        hoveron = "fills",
                        width = 640,
                        height = 640) %>%
                layout(showlegend = FALSE,
                       colorscale = list(type = "log"),
                       title = paste0("Wisconsin COVID-19 Map of ", info_to_plot_name)) %>%
                colorbar(title = info_to_plot_name,
                         tickvals = log(10^(0:3)+1),
                         ticktext = 10^(0:3),
                         len = 0.60)
        })
    })
    
    
    output$WI_cases_plot <- renderPlot({
        wi_ts <- wi_ts()
        
        p <- 
            wi_ts %>%
            filter(county == "") %>%
            ggplot(aes(date, value)) +
            geom_line(size = 1.5) +
            scale_x_date(limits = c(Sys.Date() - 14, NA), expand = c(0.06,0,0.06,2),
                         date_breaks = "2 days", date_labels = "%b %d") +
            labs(
                title = "Wisconsin Cases of COVID-19 (last 2 weeks)",
                y = info_to_plot_name(),
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
                wi_ts() %>%
                filter(county != "") %>% 
                # filter(county %in% counties_to_plot) %>%
                ggplot(aes(date, value, text = text)) +
                geom_line(aes(date, value, group = county),
                          color = "grey75",
                          alpha = 0.5) +
                geom_line(aes(date, value, color = county, group = county),
                          size = 1.5,
                          data = . %>% filter(county %in% counties_to_plot)) +
                scale_x_date(limits = c(Sys.Date() - 14, NA),
                             expand = c(0.06,0,0.06,2),
                             date_breaks = "2 days", date_labels = "%b %d") +
                labs(
                    title = "County Level Wisconsin Cases of COVID-19 (last 2 weeks)",
                    y = info_to_plot_name(),
                    x = NULL
                ) +
                theme_minimal() +
                theme(legend.position = "none")
            
        } else if (input$WI_cases_plot_county_xaxis == "Days Since 10 Cases") {
            p <- 
                wi_ts() %>% 
                filter(county != "") %>% 
                # filter(county %in% counties_to_plot) %>% 
                group_by(county, state) %>% 
                filter(cases >= 10) %>% 
                arrange(county, state, date) %>% 
                mutate(days_since_cases = row_number()) %>% 
                ungroup() %>% 
                ggplot(aes(days_since_cases, value)) +
                geom_line(aes(group = county),
                          color = "grey75",
                          alpha = 0.5) +
                geom_line(aes(color = county, group = county),
                          size = 1.5,
                          data = . %>% filter(county %in% counties_to_plot)) +
                # geom_line(aes(color = county), size = 1.5) +
                scale_x_continuous(breaks = seq(1,101,by = 2), expand = c(0.06,0,0.06,2)) +
                labs(
                    title = "County Level Wisconsin Cases of COVID-19 (last 2 weeks)",
                    y = info_to_plot_name(),
                    x = NULL
                ) +
                theme_minimal() +
                theme(legend.position = "none")
        }
        
        
        if(input$logscale) {
            p <- p +
            scale_y_log10() +
            geom_label_repel(aes(label = str_remove(county, " County")),
                             data = . %>%
                                 filter(!is.na(value), county %in% counties_to_plot) %>%
                                 group_by(county) %>%
                                 filter(date == max(date)),
                                 nudge_x = 1.5)

            # ggplotly(p, tooltip = "text") %>% 
            #     layout(yaxis = list(type = "log"))
        } else {
            # ggplotly(p, tooltip = "text")
            p <- p +
            geom_label_repel(aes(label = str_remove(county, " County")),
                             data = . %>%
                                 filter(!is.na(value), county %in% counties_to_plot) %>%
                                 group_by(county) %>%
                                 filter(date == max(date)),nudge_x = 1.5,
                             nudge_y = 5)
        }
        
        p
        # ggplotly(p, tooltip = "text")
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
