library(tidyverse)
library(lubridate)
library(readr)
library(ggthemes)
library(scales)
library(stringr)
library(plotly)

raw_data <- read_csv("../ERHS_Project/CDPHE_COVID19_County-Level_Open_Data_Repository.csv")
covid <- raw_data %>% 
  mutate(COUNTY = as.factor(COUNTY),
         Date = mdy(Date),
         Desc_ = as.factor(Desc_),
         FIPS = as.factor(FIPS),
         FULL_ = as.factor(FULL_),
         LABEL = as.factor(LABEL),
         Metric = as.factor(Metric),
         ObjectId = as.factor(ObjectId),
         POP = as.factor(POP),
         Rate = as.numeric(Rate),
         Value = as.numeric(Value)) %>% 
  rename(county = COUNTY,
         date = Date,
         descriptor = Desc_,
         fips = FIPS,
         full = FULL_,
         label = LABEL,
         metric = Metric,
         objectid = ObjectId,
         population = POP,
         rate = Rate,
         value = Value) %>% 
  select(label, fips, population, descriptor, metric, value, rate, date, objectid) %>% 
  mutate(response = coalesce(value, rate)) %>% 
  select(label, fips, population, metric, response, date)

covid

#### per day data

state_new_cases <- covid %>% 
  filter(metric %in% c("Cases")) %>% 
  group_by(date) %>% 
  summarize(state_cumulative_total_perday = sum(response)) %>% 
  ungroup() %>% 
  mutate(cases_perday = state_cumulative_total_perday - lag(state_cumulative_total_perday)) %>% 
  select(date, cases_perday)
View(state_new_cases)

state_new_deaths <- covid %>% 
  filter(metric == "Deaths") %>% 
  group_by(date) %>% 
  summarize(state_cumulative_total_perday = sum(response)) %>% 
  ungroup() %>% 
  mutate(deaths_perday = state_cumulative_total_perday - lag(state_cumulative_total_perday)) %>% 
  mutate(deaths_perday = str_replace(state_new_deaths$deaths_perday, pattern = "[-]", "0")) %>%
  select(date, deaths_perday)
View(state_new_deaths)

state_new_full <- left_join(state_new_cases, state_new_deaths, by = "date")
head(state_new_full)

perday_cases <- state_new_full %>%
  select(date, cases_perday) %>% 
  ggplot(aes(x = date, y = cases_perday)) +
  geom_line()
perday_cases

perday_deaths <- state_new_full %>%
  select(date, deaths_perday) %>% 
  ggplot(aes(x = date, y = deaths_perday)) +
  geom_col()
perday_deaths

caseColor <- "#69b3a2"
deathColor <- "#8d61cf"

perday_plot <- state_new_full %>% 
  ggplot(aes(x = date)) +
  geom_col(aes(y = cases_perday), size = 1, fill = caseColor) +
  geom_line(aes(y = deaths_perday), size = 0.7, color = deathColor) +
  labs(x = "", y = "") +
  scale_y_continuous(name = "Cases", labels = scales::comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ggtitle("Colorado New Cases and Deaths", 
          subtitle = "per day") +
  theme_few()
  
perday_plot

ggplotly(perday_plot)




