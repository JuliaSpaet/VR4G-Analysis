install.packages("oce")
library("oce")
library(tidyverse)
library(lubridate)

t <- as.POSIXct("2016-11-30", tz="UTC") + seq(0, 731*24*3600, 3600)
angle<-moonAngle(t, -30.3, 153.1, useRefraction=TRUE)

angle$lambda <- NULL
angle$declination <- NULL
angle$rightAscension<-NULL
angle$altitude<-NULL
angle$azimuth<-NULL
angle$illuminatedFraction<-NULL
angle$diameter<-NULL
angle$distance<-NULL
angle$beta<-NULL
head(angle,20)

#to convert list into df (there must be a simpler method but nothing I tried worked)
write.csv(angle, file = "angle.csv")
angle<-read_csv(file.choose())

#delete X1 column
angle$X1<-NULL




#to add a column that only contains the decimal part of the phase number
angle$moonphase<-angle$phase-floor(angle$phase)

#delete phase column
angle$phase<-NULL


# Insert new column with New South Wales time 
##PROBLEM: Twice a year when time gets switched from daylight savings to standard
##time or vice versa either an additional row is added for 2am (switch to daylight savings
##time) or the row for 2am is missing (switch to standard time)
##Google says it's a bug in the package but the entry is from 2012, so I would assume 
##it would have been fixed by now?
angle$nswtime<-format(angle$time,tz="Australia/NSW",usetz="TRUE")

#Delete time column
angle$time<-NULL

#delete first 13 rows to get a 00:00 December 1st 2016, start date
angle2 <- angle[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), ]

#delete last 12 rows to get a 23:00 November 30th 2018,t end date
angle3 <- angle2[-c(17532,17531,17530,17529,17528,17527,17526,17525,17524,17523,17522,17521),]

#add column with bin sequence
angle3$timebins<-rep(0:23, times=730, each=1)

#remove time from nswtime column
angle3$nswtime<-as.Date(sub('(\\d+/\\d+/\\d+) .*','\\1',angle3$nswtime),'%Y-%m-%d') 

#Rename 'nswtime' column to 'date'
colnames(angle3)[colnames(angle3)=="nswtime"] <- "date"

write.csv(angle3, file="moonphase.csv")


# I would create angle like this
# it does almost everything that you have done above

# you can use as.XXXX to convert all kinds of things rather than have to save and load :)
# it is also better to use for converting timezones and for grabbing the date portion
# of a time
# The tidyverse select function is cleaner to use than having to set stuff to NULL
# it just lets you pull out what you want.

# I'm not sure if this makes sense - since I haven't converted the
# time into AEDT the timebins probably don't make sense.
# You will either have to have timebins in GMT and convert them as well
# or create the time bins using c(c(10:23),c(0:9))


angle <- moonAngle(t, -30.3, 153.1, useRefraction=TRUE) %>% 
  as.tibble() %>%
  mutate(moonphase=phase-floor(phase)) %>%
  filter(time > as_datetime("2016-12-01 00:00:00", tz='Australia/NSW'),
         time <= as_datetime("2018-12-01 00:00:00", tz='Australia/NSW')) %>%
  mutate(timebins=rep(0:23, times=730, each=1), date=as.Date(time)) %>%
  # OR if you want the bins based arrounf GMT - 10
  #  mutate(timebins=rep(c(c(10:23),c(0:9)), times=730, each=1), date=as.Date(time)) %>%
  select(date,timebins,moonphase)

# if the chaining confusing the same thing can be written as
angle1 <- as.tibble(moonAngle(t, -30.3, 153.1, useRefraction=TRUE))
angle2 <- mutate(angle1, moonphase=phase-floor(phase))
angle3 <- filter(angle2, time > as_datetime("2016-12-01 00:00:00", tz='Australia/NSW'),
                         time <= as_datetime("2018-12-01 00:00:00", tz='Australia/NSW'))
angle4 <- mutate(angle3, timebins=rep(0:23, times=730, each=1),
                         date=as.Date(time))
angle <- select(angle4, date, timebins, moonphase)

