library(tidyverse)
library(dplyr)
library(zoo)
library(imputeTS)

#generate tibble with study period dates
Study_Period <- tibble(date=seq(as.Date('2016-12-01'), as.Date('2018-11-30'), by='day'))

#generate tibble with VR4G locations
Locations <- tibble(location=c("Ballina","Batemans", "Bondi", "Byron", "Coffs",
                             "Crescent", "Evans", "Forster", "Hawks", "Kiama", 
                             "Kingscliff", "Merimbula","Mollymook","Old", 
                             "Port", "Redhead","South","Sussex","Yamba"))

# create table with 'timebin', 'location', 'date' and 'temperature' column 
timebin<- Locations %>%
split(.$location) %>%
  map(function(x) { tibble(timebin = seq(0, 23), location=x$location) }) %>%
  bind_rows()

timebin_location_date <- Study_Period %>%
  rowid_to_column('Row') %>%
  split(.$Row) %>%
  map(function(x) { timebin %>% mutate(date=x$date) }) %>%
  bind_rows()

#load temperature csv file including VR4G and Hobo data
temperature <- read.csv(file.choose())

#rename col names
colnames(temperature)[colnames(temperature)=="Location"] <- "location"
colnames(temperature)[colnames(temperature)=="Temp"] <- "temperature"
colnames(temperature)[colnames(temperature)=="Date"] <- "date"
colnames(temperature)[colnames(temperature)=="Bins"] <- "timebin"

temperature2<-mutate(temperature, date= as.Date(date, format= "%Y-%m-%d")) %>% 
  group_by(location,date,timebin) %>% 
  summarise(temperature=mean(temperature))

#merge by date, location and timebin
Temperature_final <- timebin_location_date %>%
  left_join(temperature2,timebin_location_date,
            by= c('date' = 'date','location' = 'location', 'timebin' = 'timebin'))

#Tidyverse form which always returns the data frame and treats each location separately
inter_tidy <- function (x) { x %>% mutate(inter_temp = na.interpolation(x$temperature))}
Temp_final2 <- Temperature_final %>%
  split(.$location) %>%
  map(inter_tidy)%>%
  bind_rows()

#rename col names
colnames(Temp_final2)[colnames(Temp_final2)=="location"] <- "Location"
colnames(Temp_final2)[colnames(Temp_final2)=="temperature"] <- "Temperature"
colnames(Temp_final2)[colnames(Temp_final2)=="date"] <- "Date"
colnames(Temp_final2)[colnames(Temp_final2)=="timebin"] <- "Timebin"

