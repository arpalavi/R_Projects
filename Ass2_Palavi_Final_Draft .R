# R_Projects

# ###############
###### Problem 0 ####
# ###############

AnthonyPalaviAssignment2 <- list(
  firstName = "Anthony",
  lastName  = "Palavi",
  email     = "arp@ucsc.edu",
  studentID = 1238813
)

# ###############
###### Problem 1 ####
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

# ?print
# Print results from above and the Summary Table in "s1d"
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
?hist
hist(NHIS$weight, xlab = "Weight (lbs)", main = "Histogram of Weight/Frequency, Without Constraints")

#Create and print table
table(NHIS$weight)

# locating and removing weights between 996 and # 999 pounds
# Help for ifelse = ?ifelse
NHIS$newweight<-ifelse(NHIS$weight>=996&NHIS$weight<=999,NA,NHIS$weight)
# generate table for newweight 
table(NHIS$newweight)
# generate new histogram with new weight
hist(NHIS$newweight, xlab = "Weight (lbs)", main = "Histogram of Weight/Frequency with Constraint: 996 - 999 lbs")
# Lets get the new weight 
AnthonyPalaviAssignment2$s2f<-mean(NHIS$newweight,na.rm=TRUE)
# New median
AnthonyPalaviAssignment2$s2g<-median(NHIS$newweight,na.rm=TRUE)


# printing the results from code above 
# New Mean
print(AnthonyPalaviAssignment2$s2f)
# New Median 
print(AnthonyPalaviAssignment2$s2g)

# If the SEX column indicates men with 1 and woman with 2, then: 
NHIS.male <- subset(NHIS,(SEX==1))
NHIS.female <- subset(NHIS,(SEX==2))
# generate summary table 
AnthonyPalaviAssignment2$s2h <- summary(NHIS.male$weight)
AnthonyPalaviAssignment2$s2i <- summary(NHIS.female$weight)
#print the results 
print(AnthonyPalaviAssignment2$s2h)
print(AnthonyPalaviAssignment2$s2i)


# ###############
###### Problem 3 ####
# ###############

# Extracting values from a vector, data frame, and list 
vec<-c(letters,LETTERS)
# Print the even indexed characters.
AnthonyPalaviAssignment2$s3a <- vec[seq(2,52,2)]
print(AnthonyPalaviAssignment2$s3a)
#Print ant for Anthony
AnthonyPalaviAssignment2$s3b<-paste(vec[c(1,14,20)],collapse="")
print(AnthonyPalaviAssignment2$s3b)

arr<-array(c(letters,LETTERS),dim=c(3,3,3))
# extract the first column from the second matrix 
arr2<-arr[, , 2]
print(arr2)
?array
AnthonyPalaviAssignment2$s3c<-arr[1:3,1,2]
print(AnthonyPalaviAssignment2$s3c)
# extract the middle values from each of the three matrices
AnthonyPalaviAssignment2$s3d<-arr[2,2,1:3]
print(AnthonyPalaviAssignment2$s3d) 
# Last ant, first 3 letters 
AnthonyPalaviAssignment2$s3e<-paste(arr[1,1,1],arr[2,2,2],arr[2,1,3],sep="")
print(AnthonyPalaviAssignment2$s3e)
 
 
# ###############
###### Problem 4  ####
# ###############

library(foreign)
alan<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
sort(unique(alan$year))
sort(unique(alan$month))
sort(unique(alan$educ))
#Sorting data.
AnthonyPalaviAssignment2$s4<-aggregate(alan$rw,list(year=alan$year,month=alan$month,educ=alan$educ),mean,na.rm=TRUE)
print(AnthonyPalaviAssignment2$s4)
#Print the aggregated list with mean real wage for each year, month and education level.



