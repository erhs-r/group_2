library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(patchwork)
library(hrbrthemes)
library(plotly)

# Data cleaning

raw_data <- read.csv("../ERHS_Project/CDPHE_COVID19_County-Level_Open_Data_Repository.csv")
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

state_cumulative <- covid %>% 
  filter(metric %in% c("Cases","Deaths", "Rate Per 100,000")) %>% 
  group_by(metric, date) %>% 
  summarize(state_cumulative_total_perday = sum(response)) %>% 
  ungroup()
state_cumulative


caseColor <- rgb(0.2, 0.6, 0.9, 1)
deathColor <- "#69b3a2"

deaths <- state_cumulative %>% 
  filter(metric == "Deaths")
cases <- state_cumulative %>% 
  filter(metric == "Cases")

## Idea 1

p1 <- ggplot() +
  
  geom_area(data = cases, aes(x = date, y= state_cumulative_total_perday*0.02), fill=caseColor) + 
  geom_line(data = deaths, aes(x = date, y= state_cumulative_total_perday), size=1, color=deathColor) +
  
  scale_y_continuous(sec.axis = sec_axis(~.*50,name = "Cases", labels = scales::comma),
    name="Deaths", labels = scales::comma) + 

  theme_ipsum() +
  
  theme(axis.title.y = element_text(color = deathColor, size=13),
    axis.title.y.right = element_text(color = caseColor, size=13)) +
  
  ggtitle("Time Series with Cases and Deaths") # change titles (cumulative)

p1

## Idea 2

p2 <- cases_area <- state_cumulative %>% 
  filter(metric == "Cases") %>% 
  ggplot(aes(x = date, y = state_cumulative_total_perday)) +
  geom_area(fill=caseColor) +
  theme_ipsum() +
  theme(axis.title.y.right = element_text(color = caseColor, size=13))

p3 <- deaths_bar <- state_cumulative %>% 
  filter(metric == "Deaths") %>% 
  ggplot(aes(x = date, y = state_cumulative_total_perday)) +
  geom_area(fill=deathColor) +
  theme_ipsum() +
  theme(axis.title.y = element_text(color = deathColor, size=13))

p4 <- ggplotly(p2)
p5 <- ggplotly(p3)

## Idea 3

p6 <- ggplot() +
  
  geom_col(data = cases, aes(x = date, y= state_cumulative_total_perday), color=caseColor) + 
  geom_line(data = deaths, aes(x = date, y= state_cumulative_total_perday*50), color=deathColor) +
  
  scale_y_continuous(name = "Cases", sec.axis = sec_axis(~.*0.02, name="Deaths")) + 
  
  theme_ipsum() +
  
  theme(axis.title.y = element_text(color = caseColor, size=13),
    axis.title.y.right = element_text(color = deathColor, size=13)) +
  
  ggtitle("Time Series with Cases and Deaths")

ggplotly(p6)











