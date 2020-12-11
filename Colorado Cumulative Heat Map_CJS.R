library(tidyverse)
library(lubridate)
library(readr)
library(ggthemes)
library(scales)
library(stringr)
library(plotly)
library(sf)
library(leaflet)
library(tigris)

# Import Data
raw_data <- read.csv("../CDPHE_COVID19_County-Level_Open_Data_Repository.csv")
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

#Clean Data
tests <- covid %>% 
  filter(metric == "Cases") %>% 
  mutate(fips = str_pad(fips, 2, side = "left", pad = "0"),
         fips = str_pad(fips, 3, side = "left", pad = "0"),
         fips = paste0("08", fips)) %>% 
  select(fips,label, population, date, response) %>% 
  mutate(fips = as.factor(fips),
         response = as.numeric(response),
         population = as.character(population),
         label = as.character(label))
tests <- tests[with(tests, order(fips,date)),]
tests <- tests[cumsum(table(tests$fips)),]

##Geographic Data
colorado_counties <- counties(state = "CO", cb = TRUE, class = "sf")
class(colorado_counties)
class(colorado_counties$geometry)
st_bbox(co_counties$geometry)

##Merge Geographic Data with Raw Data
co_county_events <- colorado_counties %>% 
  mutate(fips = paste(STATEFP, COUNTYFP, sep = "")) %>% 
  full_join(tests, by = "fips") %>% 
  mutate(n = response) %>% 
  mutate(n = ifelse(!is.na(n),n,0))

#Create pallet for Leaflet
bins <- c(0, 500, 5000, 15000, 25000, 50000, Inf)
pal <- colorBin("Set2", domain = co_county_events, bins = bins)

##PLot data
leaflet() %>% 
  addTiles() %>% 
#  fitBounds(-102.03,37,-109.03, 41) %>% 
#  fitBounds(-102.03,42,-109.03, 46) %>% 
  addPolygons(data = co_county_events,
              popup = ~ paste('<p>', "County:", label,'<p></p>',
                              '<p>', "Population:",population,'<p></p>',
                              '<p>', "Cases:", n),
              color = ~pal(n),
              weight = 1,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 2,
                color = "#667",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>% 
  addLegend(data = co_county_events,
            title = "Cumulative number Cases",
            pal = pal, values = ~ n) %>% 
  addLayersControl(baseGroups = c("base map"),
                   overlayGroups = c("Cases","Deaths"))

