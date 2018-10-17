library(tidyverse)
DateSequence <-seq(as.Date ("2016-11-30"), as.Date("2018-11-30"), by= "day")

# Add any dead sharks here
Dead_Sharks <- tribble(
  ~ID, ~Ended,
  '23', as.Date('2017-06-20'), 
  '131', as.Date('2017-12-20')
  )

#function to create a dataframe with every day for the tagging period for a particular shark
gen <- function(x) { tibble(Date=seq(x$Date, x$Ended, by="day"), Shark=x$ID) }

#load data, remove rows without date, remove duplicate Ballina shark which was a recapture, split data
# on individual sharks,
Tagging_Dates <- read_csv(file.choose(), 
                              # force ID to be character because it is an ID not an integer
                              col_types = cols(
  Date = col_date(format = ""),
  ID = col_character(),
  TagLoc = col_character()
)) %>% 
  filter(!is.na(Date)) %>% 
  filter(!(ID==397 & TagLoc == 'Ballina')) %>%
  
  # Join on dead sharked to set any early end dates
  # left_join and coalesce is a common way of adding information for a few rows 
  left_join(Dead_Sharks, by='ID') %>%
  mutate(Ended = coalesce(Ended, as.Date('2018-11-30')))

#  apply gen function to every shark. Combination of split and map achieves 
# the looping (see library purr and Hadley's book)
All_Tagged_Sharks <- Tagging_Dates %>%
  split(.$ID) %>%
  map(gen) %>%
  
  # joins it all into one dataframe
  bind_rows()

# Instead of generating a tibble with study period dates load one from a file with 
# moon phase as well

Study_Period <- tibble(Date=seq(as.Date('2016-11-30'), as.Date('2018-11-30'), by='day'),
                       Phase=NA
                       )


# Instead of generating Stations load from file with tide and temperature

Stations <- tibble(Station=c("Ballina","Batemans", "Bondi", "Byron", "Coffs",
              "Crescent", "Evans", "Forster", "Hawks", "Kiama", 
              "Kingscliff", "Merimbula","Mollymook","Old", 
              "Port", "Redhead","South","Sussex","Yamba")) %>%
  split(.$Station) %>%
  map(function(x) { tibble(Hour = seq(0, 23), Station=x$Station, Tide=NA, Temperature=NA) }) %>%
  bind_rows()

Study_Oceanography <- Study_Period %>%
  rowid_to_column('Row') %>%
  split(.$Row) %>%
  map(function(x) { Stations %>% mutate(Period=x$Period, Date=x$Date) }) %>%
  bind_rows()  

# Left join in here to add precense data - use coalesce to add 0s

Final <- Study_Oceanography %>%
  left_join(All_Tagged_Sharks, by='Date')

save(Final, file='Final.RData')
  


