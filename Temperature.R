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


# this interpolation is not doing it separately
Temperature_final$inter_temp<-na.interpolation(Temperature_final$temperature)


inter<- function (x) {x$inter_temp<-na.interpolation(x$temperature)}

# this interpolation seems to be correct but the returned data is not in the right format
Temp_final <- Temperature_final %>%
  split(.$location) %>%
  map(inter)%>%
  bind_rows()
  
# Interpolate Bondi on its own
bondi <- Temperature_final %>% filter(location == 'Bondi') %>%
  mutate(inter2 = na.interpolation(temperature))

# Is Bondi on own the same as Bondi with rest
sum(bondi$inter_temp-bondi$inter2)
# No it isn't

# this checks that the interpolation 
sum(bondi$inter2 - Temp_final$Bondi)
# Yes it is

# Ok so interpolation is working for Temp_final - but location time etc have gone ...


inter_tidy <- function (x) { x %>% mutate(inter_temp = na.interpolation(x$temperature))}
Temp_final2 <- Temperature_final %>%
  split(.$location) %>%
  map(inter_tidy)%>%
  bind_rows()

## Haha that looks better - the problem is that the function inter was only returning one column
## So I changed it to a tidyverse form which always returns the data frame
## The function below would also do that same

inter2 <- function (x) {
  x$inter_temp<-na.interpolation(x$temperature)
  return(x)
}

