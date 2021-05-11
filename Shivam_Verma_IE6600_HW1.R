# Homework 1
# Shivam Verma
# Section 03 Spring 2020

# Problem 1-----------------------------------

library(dplyr)
library(tidyr)

att <- read.csv("attendance.csv")
new1 <- paste(att$lastname, att$firstname, sep = ",")
data <- data.frame(new1)

a <- data %>% 
  group_by(new1) %>% 
  summarise(count1 = n()) %>% 
  arrange(count1)


ros <- read.csv("roster.csv")

merg <- merge(ros, a , by.x = "names" , by.y =  "new1" , all.x = TRUE)

merg[which(is.na(merg$count1)), 2] = 0

final <- separate(data = merg, col = names, into = c("lname", "fname"), sep = ",")

final_attendance <- final[,c(2,1,3)]



# Problem 2-----------------------------------

b <- read.csv("bostoncrime.csv")


d <- b[,c(3,5)]

e <- d[which(d$OFFENSE_CODE_GROUP != "" & d$DISTRICT != ""),] 

library(dplyr)
s <- e %>% 
  group_by(OFFENSE_CODE_GROUP,DISTRICT) %>% 
  summarise(k = n())

library(reshape)

mat_final <- cast(s, `OFFENSE_CODE_GROUP`~ DISTRICT)



# Problem 3-----------------------------------

# Part(a)--------------------------------

wine <- read.csv("wine_data.csv")
v <-as.data.frame(wine$variety)
Freq_count <- v %>% 
  group_by(`wine$variety`) %>% 
  summarise( k = n()) %>% 
  arrange(desc(k)) %>% 
  top_n(10)


# Part(b)--------------------------------

wine <- read.csv("wine_data.csv")
w <- wine[,c(2,5)]
avg <- w %>%
  group_by(country) %>%
  summarise( average = mean(points)) %>%
  arrange(desc(average))

# Part(c)---------------------------------

wine <- read.csv("wine_data.csv")
pro1 <- wine[,c(6,7)]
pro <- pro1[which(pro1$province != ""),]
province_highest_average_price<- pro %>%
  group_by(province) %>%
  summarise( average = mean(price,na.rm = TRUE)) %>% 
  top_n(1)


# Part(d)----------------------------------

wine <- read.csv("wine_data.csv")
p1 <- wine[ which(wine$country == "US"),]
p11 = p1[,c(7,6)]
pro1 <- wine[,c(6,7)]
pro <- pro1[which(pro1$province != ""),]
p <- p11[which(pro$province != ""),]
US_province_highest_average_price <- p %>%
  group_by(province) %>%
  summarise( average= mean(price,na.rm = TRUE)) %>% 
  top_n(1)


# Part(e)------------------------------------

library(stringr)
library(compareDF)
wine <- read.csv("wine_data.csv")
years_20 <- data.frame(wine$designation)
years_20 <- na.omit(years_20)
sum_of_years <- sum(str_count(years_20$wine.designation, "20 Years"))



