## Anthony Palavi 
## Econ 294, Assignment 4
## Due Date: February 19th, 2016

# clear env: rm(list = ls())
# 0. 
fn<- "Anthony" 
ln<- "Palavi"
sid<-"- Student ID# w1238813"
print(paste(fn, ln, sid))

# 1 
options(  ) 
flights <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv",stringsAsFactors=F, header=T)
planes <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",stringsAsFactors=F, header=T)
weather <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv",stringsAsFactors=F, header=T)
airports <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv",stringsAsFactors=F, header=T)

# view data structure, etc.: http://www.statmethods.net/input/contents.html
#?View
#?read.csv
#list(dest)
#View(flights,dest)
#?as.Date()

# 2
## Convert any date column from type char to type date
flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)

# 3 
# all flights that went to the city of San Francisco or Oakland CA
# ?nrow
library(dplyr)
######   Notes: dplyr provides the %>% operator. x %>% f(y) turns into f(x, y) 
######   so you can use it to rewrite multiple operations that you can read left-to-right, 
######   top-to-bottom
flights.2a <- subset(flights, dest=="SFO"| dest=="OAK")
nrow(flights.2a)
# 3508 flights destined for SFO or OAK
flights.2b <- subset(flights, dep_delay>=60)
nrow(flights.2b)
# 10474 flights delayed an hour or more 
flights.2c <- subset(flights, arr_delay>=dep_delay*2)
nrow(flights.2c)
nrow(flights.2c)

# 4
?dplyr::select
require(dplyr)
# method 1
select(flights,dep_delay,arr_delay)
# method 2
flights.Yes.delayA<-flights%>%dplyr::select(dep_delay,arr_delay)
flights.Yes.delayA
# method 3
flights.Yes.delayB<-select(flights,ends_with("delay"))
flights.Yes.delayB
# method 4 execution 
flights.Yes.delayD <- flights %>% select(dep_delay,arr_delay)
nrow(flights.Yes.delayD)

# 5
# (a)
top_5_delay <- flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)
print(top_5_delay)
## or, alternatively we could do this: top_5<- head(sort(flights$dep_delay, decreasing=TRUE), 5)
# print(top_5)

# (b) 
min_5 <-flights%>%mutate(
  min_catch_up=(dep_delay-arr_delay))%>%arrange(desc(min_catch_up))%>%head(5)
print(min_5)

# 6 
# Where the existing time variable is travel time in minutes, find speed in mph.
# Create delta, the amount of time made-up or lost in the flight 
# (e.g. a delta of 60 means the flight made up 60 minutes between departure and arrival).

# (a) 
# using mutate to create "mph" array/column in flights 
flights<-mutate(flights, mph=dist/(time/60))
# using mutate to create delta
flights<-mutate(flights, delta=(dep_delay-arr_delay))
# ordering flights according to mph
flights <- flights[order(-flights$mph) , ]
head(flights$mph,n=5)

# (b)
# ?abs
# ?order
# creating top five flights that made up the most time 
flights <- flights[order(flights$delta) , ]
# get top five in order 
top_five<-head(abs(flights$delta),n=5)
# print this bad-bwoy out! 
print(top_five)
# boom! 

# (c) 
# caluculating the flights that lost most time 
flights<-mutate(flights, most_loss=(arr_delay-dep_delay))
# creating variable that has the order of most loss time in flight
flights.6c<-flights[order(flights$most_loss),]
# not sure what I'm doing here... maybe it's useless or repetative
most_loss_flights <- head(flights.6c$most_loss,n=5)
# print it out 
print(most_loss_flights)

# 7 
# The group_by function allows you to find 
# a set of statistics within each group you define (and ungroup() undos group_by()).
install.packages("dplyr")
library(dplyr)
# grouping by 'carrier' to make data more easy to manage 
group_carrier <- group_by(flights,carrier)
# getting specific, group characteristics for flight groups
flights.7a <- group_carrier%>%
  summarise(
    cancelled_flights=sum(cancelled, na.rm=T),
    total_flights=n(),
    percent_cancelled=((cancelled_flights/total_flights)*100),
    min_delta=min(delta,na.rm=T),
    max_delta=max(delta,na.rm=T),
    median_delta=median(delta,na.rm=T),
    mean_delta=mean(delta,na.rm=T),
    first_quart=quantile(delta,0.25,na.rm=T),
    third_quart=quantile(delta,0.75,na.rm=T),
    ninety_quan=quantile(delta,0.90,na.rm=T)
  )

flights.7a <- arrange(flights.7a, desc(percent_cancelled))
print(flights.7a)
summary(flights.7a)

print("creates a day_delay variable and summarizes a grouped set of flights that 
      were not missing from dep_delay, according to date, where the mean of dep_delay 
      is also presented and n> 10")

# Rewrite it using the magrettr's %>% operator
day_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n=n()
  )

day_delay  # to check 

# 8
arrange(day_delay,date)
# difference between today and yesterday's average delay
# ?dplyr::lag
# n =  the number of positions to lead or lag by; i.e., 1
delay_full<-day_delay %>%
  mutate(delay_today=delay,delay_yesterday=lag(delay,1),delay_inc=delay_today-delay_yesterday)
delay_full<-arrange(delay_full,-delay_inc)
head(delay.8a,n=5)

# 9

dest_delay<-dplyr::filter(flights,!is.na(dep_delay))%>%
  group_by(dest)%>%
  summarize(
    arr_delay=mean(arr_delay),
    n=n()
  )

airports<-select(airports,
                 dest = iata, name = airport , city,state, lat, long)

df.9a<-left_join(dest_delay, airports, by=c("dest"="dest"))
df.9a<-arrange(df.9a,-arr_delay)
head(df.9a,n=5)

df.9b<-inner_join(dest_delay,airports,by=c("dest"="dest"))
nrow(df.9a)
nrow(df.9b)
print("No, the number of observations for the left_join and inner_join function results differ by 2.")

df.9c<-right_join(dest_delay,airports,by=c("dest"="dest"))
nrow(df.9c)
print("There are 3378 observations. Yes, there are NAs in the arr_delay column. This could be because each airport per city has a different number of observations")

df.9d<-full_join(dest_delay,airports,by=c("dest"="dest"))
nrow(df.9d)
print("There are 3378 observations. Also, there are NAs in arr_delay because each airport has a different number of observations")


# 10
hourly_delay<- filter(flights,!is.na(dep_delay))%>% group_by(date,hour)%>%
  summarise(
    delay = mean(dep_delay),
    n = n(),
    n>10)

weather %>% tbl_df
df.10<- hourly_delay%>% 
  full_join(weather)
##join by date and hour##

df.10_worstweather<-df.10%>%group_by(conditions)%>%summarise(a=max(delay,na.rm=T))
summary(df.10_worstweather)

# 11

require(tidyr)
require(dplyr)
# creating data frame
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
# mapping time!
df.map <- df %>% 
  gather(demo,value,
         ...= -treatment) %>%rename(subject=demo)%>%select(subject,treatment,value)
df.map[1,"subject"]<- 1
df.map[2,"subject"]<- 1
df.map[3,"subject"]<- 2
df.map[4,"subject"]<- 2

# (b)
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df
df.map_b <- df %>% rename(1 = subject1,2 = subject2)
spread(
  key = subject,
  value=value
)

# no time to do last part, it's 11:45 pm! 


