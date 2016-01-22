# R_Projects

# ###############
###### Problem ZeR0 ####
# ###############

AnthonyPalaviAssignment2 <- list(
  firstName = "Anthony",
  lastName  = "Palavi",
  email     = "arp@ucsc.edu",
  studentID = 1238813
)

# ###############
###### Problem oNe ####
# ###############

# Now, load the data. I will call this data set "pearls"
pearls <- get(  
  load(
    file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
  )
)

# Counting observations 
AnthonyPalaviAssignment2$s1a <- nrow(pearls)
# Counting columns 
AnthonyPalaviAssignment2$s1b <- ncol(pearls)
# Header names 
AnthonyPalaviAssignment2$s1c <- names(pearls)
# Summary of prices 
AnthonyPalaviAssignment2$s1d <- summary(pearls$price)


#Here we will print the summary stats
AnthonyPalaviAssignment2$s1d<-summary(pearls$price)



# Print results from above
print(AnthonyPalaviAssignment2$s1a)  
print(AnthonyPalaviAssignment2$s1b)
print(AnthonyPalaviAssignment2$s1c)
print(AnthonyPalaviAssignment2$s1d)


# ###############
###### Problem 2 ####
# ###############

NHIS<-read.table("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",header = TRUE)
# Counting observations 
AnthonyPalaviAssignment2$s2a<-nrow(NHIS)
# Counting columns 
AnthonyPalaviAssignment2$s2b<-ncol(NHIS)
# Calling column names 
AnthonyPalaviAssignment2$s2c <- colnames(NHIS)
# Mean weight
AnthonyPalaviAssignment2$s2d <- mean(NHIS$weight)
# Median weight 
AnthonyPalaviAssignment2$s2e <- median(NHIS$weight)


# ?print
# Print results from above
print(AnthonyPalaviAssignment2$s2a)  
print(AnthonyPalaviAssignment2$s2b)
print(AnthonyPalaviAssignment2$s2c)
print(AnthonyPalaviAssignment2$s2d)
print(AnthonyPalaviAssignment2$s2e)

#Print headers
AnthonyPalaviAssignment2$s2d<-mean(NHIS$weight)
print("The mean weight is 266.")
AnthonyPalaviAssignment2$s2e <- median(NHIS$weight)
print("The median weight is 175.")

#Create and print histogram 
hist(NHIS$weight)

#Create and print table
table(NHIS$weight)

# locating and removing weights between 996 and # 999 pounds
# ?ifelse
NHIS$weightadj<-ifelse(NHIS$weight>=996&NHIS$weight<=999,NA,NHIS$weight)
AnthonyPalaviAssignment2$s2
AnthonyPalaviAssignment2$s2f<-mean(NHIS$weightadj,na.rm=TRUE)
print("The adjusted mean is 174.")
AnthonyPalaviAssignment2$s2g<-median(NHIS$weightadj,na.rm=TRUE)
colnames(NHIS$weight)
print("The adjusted median is 170.")
# Call and Print summary for adjusted weight for WOmen
AnthonyPalaviAssignment2$s2h<-summary(NHIS$weightadj,NHIS$SEX==2)
print(AnthonyPalaviAssignment2$s2h)

# Call and Print summary for adjusted weight for men
AnthonyPalaviAssignment2$s2i<-summary(NHIS$weightadj,NHIS$SEX==1)
print(AnthonyPalaviAssignment2$s2i)
print(YuanWuAssignment2$s2i)

# ###############
###### Problem Thr33 ####
# ###############

# Extracting values from a vector, data frame, and list 
vec<-c(letters,LETTERS)



