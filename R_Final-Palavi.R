rm(list = ls())

---
  title: "R-Final-Anthony-Palavi"
output: html_document
---
  ### Anthony Palavi
  ### R Lab, Final Exam 
  ### Due Date: midnight, 03-16-2016
  
  ```{r setup, include=FALSE}
# Install necessary packages 
install.packages("nycflights13",repos="http://cran.rstudio.com/") 
install.packages(contrib.url)
install.packages("dplyr")

# Access necessary libraries 
library(nycflights13)
library(dplyr)
library(ggplot2)  # ggplot: Sounds like an Iggy Azalea song, but I digress 
# copying or creating our table of nycflights13 using built in sqlite fcn
repos="http://cran.rstudio.com/"
?tbl()
flights_sqlite <- tbl(nycflights13_sqlite(), "flights")
# Show me the money 
flights_sqlite
# Show me the money

# Part A 

# doing some further HOUSEKEEPING, whatever that means 
BobMarley_db <- src_sqlite("my_db.sqlite3", create = T)
# now that the house is clean, we will begin to analyse the datasesas
# show relationship between flights and weather 
# to accomplish this, we join'em like siamese twins 
ReggaeMusic<-left_join(
  tbl(BobMarley_db, "flights"),
  tbl(BobMarley_db, "weather"),
  by = c("year","month","day","hour"))%>%
  collect() %>%
  mutate(canceled = is.na(arr_time))
ReggaeMusic   # here we are viewing our siamese twins
# summary dis biotch
summary(ReggaeMusic)
ReggaeMusic$month<-as.factor(ReggaeMusic$month)

# show me the money 
ReggaeMusic$month
# so after removing everything that is NOT NA, i.e., all on-time flights, we now have all canceled flights in one data frame 
weather_flights <- flights_sqlite %>% left_join(weather, by = "origin", copy = TRUE)
weather_flights <- as.data.frame(ReggaeMusic)
BobDylan <- ggplot(weather_flights, aes(x= pressure,y=dep_delay)) + geom_point()
BobDylan


knitr::opts_chunk$set(echo = TRUE)
```

## Analysis of the Relationship Between Atmopshereic Pressure and Departure Delays

Our results from the BobDylan plot, which shows the correlation between departure delays and Pressue, shows that there is high variance between pressure values and dep delay. This means that there is no significant correlation between atmospheric pressure build up and dep_delays.   

```{echo=TRUE}
# Part B 

# grouping year and mean dep_delay 
Elvis <- group_by(flights_sqlite, year) %>% summarise(count = mean(dep_delay))
Elvis  <- group_by(flights_sqlite, month) %>% summarise(count = mean(dep_delay))
Elvis  <- as.data.frame(Elvis )

ElvisPresley  <- ggplot(Elvis , aes(x= factor(month),y=count)) + geom_bar(stat="identity", fill = "indianred")
ElvisPresley



```

## Month to Delay Relationship 

Our histogram output shows that there were significant delays for months 6 , 7 and 12. The delays in months 6 and 7 could be due to the high season for traveling during summer months; perhps delays were due to complications from high traffic at airport air terminals and landing strips. The delay assiciated with month 12 could be because of the volume of travellers flying during the holidays.     



```{echo=TRUE}
# Part C

# grouping destination locations with the mean values of departure delays 
BillClinton <- group_by(flights_sqlite, dest) %>% summarise(count = mean(dep_delay))
BillClinton <- as.data.frame(BillClinton)

Monica_Lewinski <- ggplot(BillClinton, aes(x= dest,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))

# show me the money 
Monica_Lewinski 

```

## Destination to Mean(Dep_Delay) Relationship 

Our new histogram, properly named "Monica Lewinsky," shows negative values for both the LEX and PSP destinations. This means that flight arrival times for these destinations were early at an averaged-amount of time. 

```{echo=TRUE}
# Part D

# here we need to join by tail number to identify characteristics of the plane 
ChrisRock <- flights_sqlite %>% left_join(planes, by = "tailnum", copy = TRUE)

Dave_Chapelle <- group_by(ChrisRock, manufacturer) %>% summarise(count = mean(dep_delay))
Dave_Chapelle <- as.data.frame(Dave_Chapelle)

Wayans_Brothers <- ggplot(Dave_Chapelle, aes(x= manufacturer,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))

# show me the funny
Wayans_Brothers

```

## Manufacturer and Departure Delay Relationships 

In the above histogram we can see that the following two manufactureres have negative mean values: AVIONS MARCEL DASSAULT and JOHN G HESS. We can see from this result that these planes are early an averaged-amount of the time. This early correlation result is especially apparent with the AVIONS MARCEL DASSAULT maufacturer. 

```{echo=TRUE}
# Part D continued 

Dave_Chapelle <- group_by(ChrisRock, engine) %>% summarise(count = mean(dep_delay))
Dave_Chapelle <- as.data.frame(Dave_Chapelle)
ScaryMovie <- ggplot(Dave_Chapelle, aes(x= engine,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
ScaryMovie 

```

## Plane Engine and Departure Delay Relationships 

Here we see that the highest or most frequent departure delays are found with planes that have a '4 Cycle' engine.  

```{echo=TRUE}
# Part D continued 

Dave_Chapelle <- group_by(ChrisRock , engines) %>% summarise(count = mean(dep_delay))
Dave_Chapelle <- as.data.frame(Dave_Chapelle)
something <- ggplot(Dave_Chapelle, aes(x= engines,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
something

```

## Plane Engine and Departure Delay Relationships 

Our histo above shows that places with 3 engines are on-time more often then planes with planes with 1,2, or 4 engines. This could be due to a host of factors (e.g., weight) that we have not accounted for. 



