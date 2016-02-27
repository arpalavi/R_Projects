# Anthony Palavi
# 2/26/2016
# Assignment #5, Econ 294

# rm(list = ls())
cat("Name: Anthony Palavi\n      SID:1238813\n      email: arp@ucsc.edu")

################### PROBLEM #1
# Part A
require(dplyr)
library(ggplot2)
install.packages("ggplot2")
data("diamonds")
View(diamonds)
help(diamonds)
# diamonds$x
# ggplot(diamonds, aes(x=carat, y=price)) + geom_point()  # not the graph we want, but a start
diamonds<-diamonds%>%mutate(volume = x*y*z)%>%tbl_df()
d<-ggplot(diamonds,aes(x=volume,y=price))
d + geom_point(aes(color=clarity,size=carat),alpha = 1.0)+scale_x_log10()+scale_y_log10()


# Part B
?geom_histogram
# creating histogram 
Q <- ggplot(diamonds, aes(carat,..density..))
# Included the legend to show the different values for each color 
Q + geom_histogram(aes(fill = clarity),na.rm=T,show.legend=T,bins=25)+ facet_grid(cut ~ .)

# Part C
?geom_violin
?geom_jitter
# create violin variable wit cut and price in the aes command
violin <- ggplot(diamonds, aes(cut,price))
# plot the violin data subset, alpha = 0.01 because this is the closet fit I could match to your plot, visually
violin + geom_violin()+ geom_jitter(alpha=0.01)


################### PROBLEM #2
# Not sure where this number whent, maybe it was removed from homework 

################### PROBLEM #3

# Part A
library(foreign)
# download dataset onto RStudio 
org_data<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

##use summarise to get the data I need.##
data_organized <- org %>% dplyr::group_by(year,month)%>%
  dplyr::summarise(
    rw_quantile1st = quantile(rw, .1, na.rm = T),
    rw_quantile9st = quantile(rw, .9, na.rm = T),
    rw_quantile1 = quantile(rw, .25, na.rm = T),
    rw_quantile3 = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())
data_organized<-data_organized %>% mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))
brand_new <- ggplot(data_organized, aes(x=date, y=Median.RW))
brand_new + geom_ribbon(
  aes(ymin=rw_quantile1, ymax=rw_quantile3),alpha=0.4) + geom_ribbon(
    aes(ymin=rw_quantile1st, ymax=rw_quantile9st),alpha=0.3) + geom_line(
      aes(y=Median.RW))+lims(y=c(0,50))

# Part B
data_organized2 <- org %>% dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
    Median.RW = median(rw, na.rm = T),
    count = n())

data_organized2<-data_organized2 %>% mutate(date=paste(year,month,"01", sep="-"),
                                date=as.Date(date,format="%Y-%m-%d"))

enough <- ggplot(data_organized2, aes(x=date, y=Median.RW,group=educ))
enough + geom_line(aes(color=educ))



 
