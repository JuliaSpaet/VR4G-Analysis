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
