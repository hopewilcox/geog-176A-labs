library(tidyverse)
library(knitr)
library(readxl)
home = read_csv("C:/Users/hopew/Desktop/github176/geog-176A-labs/data/landdata-states.csv")

install.packages("zoo")
library(readxl)


# read in data from URL
covid19 = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

# filter data to california and add new column of daily new cases
covid_ca = covid19 %>%
  filter(state=="California") %>%
  group_by(county) %>%
  mutate(newcases = cases - lag(cases)) %>%
  ungroup()


# table of 5 counties with most cases
top5cumulative= covid_ca %>%
  filter(date==max(date)) %>%
  slice_max(cases, n=5) %>%
  select(county, cases)

knitr::kable(top5cumulative,
             caption = "Most Cumulative Cases California Counties",
             col.names = c("County", "Cumulative Cases"))

# table of 5 counties with most new cases
top5_newcases = covid_ca %>%
  filter(date==max(date)) %>%
  slice_max(newcases, n=5) %>%
  select(county, newcases)

knitr::kable(top5_newcases,
             caption = "Most New Cases California Counties",
             col.names = c("County", "New Cases"))

# read in population data
pop_est = read_excel("data/PopulationEstimates.xls",
                     skip=2)
pop_est = pop_est %>%
  select(fips="FIPStxt", state="State", "Area_Name", pop2019="POP_ESTIMATE_2019")

# join covid data and population data
pop_joined_covid = inner_join(pop_est, covid_ca, by="fips")


cases_percapita = pop_joined_covid %>%
  filter(date==max(date)-13) %>%
  mutate(most_percapita = (sum(cases))/pop2019) %>%
  mutate(new_percapita = ((sum(cases-lag(cases)))/pop2019))





# table of most cases per capita
most_cumulative_percapita = cases_percapita %>%
  slice_max(most_percapita, n=5) %>%
  select(county, most_percapita)

knitr::kable(most_cumulative_percapita,
             caption = "Most Cumulative Cases Per Capita California Counties",
             col.names = c("County", "Cumulative Cases per Capita"))

last14days = pop_joined_covid %>%
  filter(date>max(date)-14) %>%
  group_by(county, pop2019) %>%
  summarise(newcases=sum(newcases)) %>%
  ungroup() %>%
  mutate(case_per100k = newcases/(pop2019/100000)) %>%
  filter(case_per100k<=100)
knitr::kable(last14days,
             caption="Counties with Most New Cases")



# Question 2

fourstates = covid19 %>%
  group_by(state,date) %>%
  summarise(cases=sum(cases)) %>%
  ungroup() %>%
  filter(state %in% c("California","New York","Louisiana","Florida")) %>%
  group_by(date) %>%
  mutate(newcases=cases-lag(cases),
         roll7=newcases, 7, fill=NA, allign="right") %>%
  ggplot(aes(x=date))+
  geom_col(aes(y=newcases), col=NA, fill="666666")





state.of.interest1 = "Florida"
florida_plot=covid19 %>%
  filter(state == state.of.interest1) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newcases = cases - lag(cases),
         roll7 = rollmean(newcases, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = newcases), col = NA, fill = "969696") +
  geom_line(aes(y = roll7), col = "black", size = 1) +
  ggthemes::theme_economist() +
  labs(title = paste("New Reported cases by day in", state.of.interest1)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = 'bold'))

state.of.interest2 = "California"
california_plot=covid19 %>%
  filter(state == state.of.interest2) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newcases = cases - lag(cases),
         roll7 = rollmean(newcases, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = newcases), col = NA, fill = "82291") +
  geom_line(aes(y = roll7), col = "darkgreen", size = 1) +
  ggthemes::theme_economist() +
  labs(title = paste("New Reported cases by day in", state.of.interest2)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = 'bold'))
california_plot

state.of.interest3 = "New York"
ny_plot=covid19 %>%
  filter(state == state.of.interest3) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newcases = cases - lag(cases),
         roll7 = rollmean(newcases, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = newcases), col = NA, fill = "829044") +
  geom_line(aes(y = roll7), col = "darkblue", size = 1) +
  ggthemes::theme_economist() +
  labs(title = paste("New Reported cases by day in", state.of.interest3)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = 'bold'))
ny_plot

state.of.interest4 = "Louisiana"
louisiana_plot=covid19 %>%
  filter(state == state.of.interest4) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newcases = cases - lag(cases),
         roll7 = rollmean(newcases, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = newcases), col = NA, fill = "342114") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) +
  ggthemes::theme_economist() +
  labs(title = paste("New Reported cases by day in", state.of.interest4)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = 'bold'))
louisiana_plot

facet_grid(california_plot, louisiana_plot, ny_plot, florida_plot)
