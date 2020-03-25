## Testing a doubling time calculation:
library(tidyverse)

weight_date <- function(data) {
  # Add weights skewed toward most recent dates.
  # requires a `date`` variable
  skew <- 20
  sweight <- as.numeric(unique(data$date))
  mweight <- min(sweight)
  eweight <- as.numeric(sweight - mweight)
  sweight <- max(eweight)
  eweight <- max(exp(skew * eweight / sweight))
  
  # Add weights to states
  data %>%
    mutate(Weight = as.numeric(date - mweight) / sweight) %>%
    mutate(Weight = exp(skew * Weight) / eweight)
}

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
  ) %>% 
  mutate(date = as.POSIXct(as.character(date), format="%Y-%m-%d")) %>% 
  weight_date()


wi_cty_timeseries <-
  wi_timeseries %>% 
  filter(county != "" & city == "")

# lag_join <- 
#   wi_cty_timeseries %>% 
#   select(county, state, country, date, cases) %>% 
#   mutate(date_l1 = )
# 
# 
# wi_cty_timeseries %>% 
#   left_join(lag_join, by = c("county", "state", ))
 


wi_cty_at_date <- function(start, end) {
  wi_cty_timeseries %>% 
    filter(date >= start & date <= end)
}
 

# wi_cty_at_date("2020-03-20", "2020-03-21") %>% 
#   select(county, date, cases) %>% 
#   print(n = Inf)


dates <- as.Date("2020-03-23") - (0:14) # last 14 days

tbls <- list()
for (i in 1:length(dates)) {
  
  date <- dates[i]
  # TODO: add the weighting from Byandell app
  fit <- glm(cases ~ date * county,
             weight = Weight,
             data = wi_cty_at_date(start = date - 14, end = date), 
             family = "poisson")
  
  coefs <- coef(fit)
  coefs <- coefs[str_detect(names(coefs), "date")]
  coefs[-1] <- coefs[-1] + coefs[1]
  names(coefs) <- str_remove(names(coefs), "date:county")
  
  doubling <- log(2) / coefs / 86400
  round(doubling, 3)
  
  tbls[[i]] <- 
    enframe(doubling) %>% 
    filter(name != "date") %>% 
    # filter(name == "Dane County")
    mutate(date = date) 
}


doubling_times <- do.call(rbind, tbls)

p <- 
  doubling_times %>% filter(name == "Dane County") %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  labs(
    x = NULL,
    y = "Doubling Time (higher = better)",
    title = "Estimated doubling times for Dane County",
    subtitle = "Based on 14-day previous cases count"
  )

p
ggsave(here::here("data-vis/estimated-doubling-times-for-dane-county.png"), p)

# Counties = {
#   form <- formula(Count ~ Date * County)
# },
# States = {
#   form <- formula(Count ~ Date * State)
# },
# Countries = {
#   form <- formula(Count ~ Date * Region)
# })
# fit <- glm(form, cases_reactive(),
#            weight = Weight, family = "poisson")
# coefs <- coef(fit)
# coefs <- coefs[str_detect(names(coefs), "Date")]
# coefs[-1] <- coefs[-1] + coefs[1]
# names(coefs) <- units_reactive()
# } else {
#   form <- formula(Count ~ Date)
#   fit <- glm(form, cases_reactive(),
#              weight = Weight, family = "poisson")
#   coefs <- coef(fit)
#   coefs <- coefs[str_detect(names(coefs), "Date")]
#   names(coefs) <- units_reactive()
# }
# doubling <- log(2) / coefs / 86400
