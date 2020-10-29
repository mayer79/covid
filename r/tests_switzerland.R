library(tidyverse)
library(readxl)
library(lubridate)

# Fetch data from the Federal Office of Public Health BAG
data_url <- "https://www.bag.admin.ch/dam/bag/de/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-labortests.xlsx.download.xlsx/Dashboard_3_COVID19_labtests_positivity.xlsx"
temp <- tempfile(fileext = ".xlsx")
download.file(data_url, destfile = temp, mode = 'wb')
raw <- read_excel(temp, sheet = 1)
# file.remove(temp)

# Aggregate to full weeks (starting on Sundays)
prep <- raw %>%
  filter(Datum >= '2020-03-01',
         Datum <= '2020-10-24') %>%
  mutate(week = floor_date(Datum, unit = "week", week_start = 7)) %>%
  group_by(week, Outcome_tests) %>%
  summarize(Number_of_tests = sum(Number_of_tests),
            .groups = "drop")

# Plot
ggplot(prep, aes(x = week, y = Number_of_tests, group = Outcome_tests, fill = Outcome_tests)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of tests per week",
       x = "Date",
       title = "Covid-19 Tests per Week in Switzerland",
       caption = paste("Data status:", as.Date(raw$Replikation_dt[1]))) +
  theme_light(base_size = 15) +
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank(), legend.background = ) +
  scale_fill_viridis_d(direction = -1, begin = 0.2, end = 0.8) +
  scale_y_continuous(label = function(z) format(z, scientific = FALSE, big.mark = "'"))

# Save
ggsave("tests_Switzerland.png")



