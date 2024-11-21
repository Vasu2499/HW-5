library(tidyverse)
library(nycflights13)
library(kableExtra)
library(knitr)
flights_data <- head(flights)
departure_delay_summary <- flights %>%
  group_by(dest) %>%
  summarise("Mean Delay" = mean(dep_delay, na.rm = TRUE),
            "Median Delay" = median(dep_delay, na.rm = TRUE),
            "Flights" = n()) %>%
  filter("Flights" >= 10) %>% 
  arrange(desc("Mean Delay")) %>%  
  left_join(airports, by = c("dest" = "faa")) %>%
  select(name, "Mean Delay", "Median Delay", "Flights")

kable(departure_delay_summary, caption = "Departure Delays", digits = 1, align = 'c')
arrival_delay_summary <- flights %>%
  group_by(dest) %>%
  summarise(mean_arr_delay = mean(arr_delay, na.rm = TRUE),
            median_arr_delay = median(arr_delay, na.rm = TRUE),
            num_flights = n()) %>%
  filter(num_flights >= 10) %>%
  arrange(desc(mean_arr_delay)) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(name, mean_arr_delay, median_arr_delay, num_flights)

## Display the tibble
kable(arrival_delay_summary,align='c') 
flights_speed <- flights %>%
  mutate(speed_mph = distance / (air_time / 60))  

## Find the aircraft with the fastest average speed

fastest_aircraft <- flights_speed %>%
  group_by(tailnum) %>%
  summarise(avg_speed = mean(speed_mph, na.rm = TRUE), num_flights = n()) %>%
  arrange(desc(avg_speed)) %>%
  slice(1)

## Join with the planes dataset to get the model

fastest_model <- fastest_aircraft %>%
  left_join(planes, by = "tailnum") %>%
  select(model, avg_speed, num_flights)

## Display the fastest model

fastest_model