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

#I am confused why Temperature_final contains 333 559 rows. When I multiply 24 timebins 
# with 19 locations and 730 days I get 332 880. Any idea what might be going on here?

# I am trying to add a column with interpolated temperature values. However,
# the below code runs it through the entire column, not considering the different locations
Temperature_final$inter_temp<-na.interpolation(Temperature_final$temperature)

#I have hence tried to use the following but I am not sure I understand all functions in purrr
# I get an error message saying "Error in bind_rows_(x, .id) :
#Argument 4 must be length 17520, not 17524 "

inter<- function (x) {x$inter_temp<-na.interpolation(x$temperature)}

Temp_final <- Temperature_final %>%
  split(.$location) %>%
  map(inter)%>%
  bind_rows()
  



