library(tidyverse)

# Source: European Centre for Disease Prevention and Control https://www.ecdc.europa.eu/en
raw <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# Transformations and filters
prep <- raw %>%
  mutate(Date = lubridate::dmy(dateRep),
         `Cases per Mio` = cases / popData2019 * 1e6)  %>%
  filter(Date >= "2020-03-01",
         countriesAndTerritories %in% c("Switzerland", "United_States_of_America", "Germany", "Sweden"))

# Plot
ggplot(prep, aes(x = Date, y = `Cases per Mio`)) +
  geom_line() +
  geom_smooth(span = 0.3) +
  facet_wrap(~ countriesAndTerritories)
