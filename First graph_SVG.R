library(tidyverse)
library(lubridate)
library(readr)
raw_data <- read_csv("../group_2/CDPHE_COVID19_County-Level_Open_Data_Repository.csv")
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

```{r state_cumulative_data}
state_cumulative <- covid %>% 
  filter(metric %in% c("Cases","Deaths", "Rate Per 100,000")) %>% 
  group_by(metric, date) %>% 
  summarize(state_cumulative_total_perday = sum(response)) %>% 
  ungroup()
state_cumulative
```
```{r time_series with cases}
deaths_bar <- state_cumulative %>% 
  filter(metric == "Deaths") %>% 
  ggplot(aes(x = date, y = state_cumulative_total_perday
  ))+
  geom_line()

cases_line <- state_cumulative %>% 
  filter(metric == "Cases") %>% 
  ggplot(aes(x = date, y = state_cumulative_total_perday
  ))+
  geom_line()
deaths_bar
cases_line