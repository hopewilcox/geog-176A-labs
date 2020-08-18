library(tidyverse)
library(knitr)
library(readxl)
home = read_csv("C:/Users/hopew/Desktop/github176/geog-176A-labs/data/landdata-states.csv")

library(zoo)
library(readxl)

# read in data from URL
covid19 = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')


# read in population data
pop_est <- read_excel("data/PopulationEstimates.xls",
                      skip = 2)
pop_est = pop_est %>%
  select(fips="FIPStxt", "State", "Area_Name", pop2019="POP_ESTIMATE_2019")

state_ca= "California"

# join pop data to covid_ca data
pop_joined_covid = inner_join(pop_est, covid_ca, by="fips") %>%
  group_by(Area_Name)




# filter data to california and add new column of daily new cases
pop_joined_covid %>%
  mutate(daily_new_cases = cases-lag(cases)) %>%
  ungroup()


# table of 5 counties with most cases
top5cumulative = covid_ca %>%
  slice_max(cases, n=5) %>%
  select(county, cases)

knitr::kable(top5cumulative,
             caption = "Most Cumulative Cases California Counties",
             col.names = c("County", "Cumulative Cases"))
