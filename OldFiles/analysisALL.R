## Code for analysis of Track Data
## input is clean data from Dropbox/2021Spring/Research/TrackData/trackX00clean.csv
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(xtable)

setwd("/Users/monniemcgee/Dropbox/2021Spring/Research/TrackData/Data")

# 200 Data
track200m = read.csv("./track200clean.csv",stringsAsFactors = TRUE)

# Convert to tibbles
track200m = as_tibble(track200m[,-1])

#Create gender variable for simplicity
# track200m = track200m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))
#  Double check with previous Gender variable
# table(track200m$Gender2)
# table(track200m$Gender)
# table(track200m$Gender,track200m$Gender2)

# A check revealed that there are 11 'Events' listed as 'Mens Varsity Girls 200 Meters'. 
# All should be classified as Mens. Similarly for "Womens Varisty Boys...". 
# track200m[which((track200m$Gender == "Mens") & (track200m$Gender2 == "F")),]
# track200m$Event[which((track200m$Gender == "Mens") & (track200m$Gender2 == "F"))]
# Correct the errors
err <- grepl('Mens Varsity Girls',track200m$Event)
err2 <- grepl('Womens Varsity Boys',track200m$Event)
track200m$Event[err=='TRUE'] <- 'Mens Varsity 200 Meters'
track200m$Event[err2=='TRUE'] <- 'Womens Varsity 200 Meters'
# Now create simple Gender variable. Don't need it, but glad I did it. 
# Wouldn't have found the typos in the events otherwise!
# track200m = track200m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))
write.csv(track200m, file="track200mLong.csv")
# Investigate left_join using mtcars data
# left_join(mtcars2,mtcars,by=c('cyl','vs'))

# For each combination of Name, school, Gender, and Grade find the number of events 
# in which an athelete participated and the best time for that athlete in that grade
bestTimes200 = track200m %>%
  group_by(Name,School,Gender, Grade) %>% 
  summarise(NumRaces=n(),BestTime=min(Time)) %>% 
  filter(NumRaces >= 4)
# Merge with original data to have minimum times for each grade included in the data frame
# track200 = left_join(bestTimes200,track200m,by=c('Name','School','Gender'))
# nrow(track200)
## June 23: Thought for later - look at the times as a function of the number of events in a year 
## Requires all times from each year. Not sure how to do this yet. 
## Idea - plot of best time vs. grade with size of circle representing how many events in that grade

# Find the athletes that competed all four years of high school
# This data set include both boys and girls
fourYears200 = track200m %>%
  group_by(Name,School,Gender, Grade)%>% summarise(BestTime=min(Time))
fourYears200 <- fourYears200 %>% spread(Grade, BestTime)
fourYears200 <- fourYears200[complete.cases(fourYears200),]
nrow(fourYears200)


# Find grade in which best of the best times occurs for boys and girls
# This is the minumum time. How do we find the variable at which the minimum time occurs?

# One way. Not satisfactory because have to extract columns, convert to matrix 
# and then add the result back to the data frame.
# Put data into a matrix
# Use max.col(-mat, ties.method="first") to find minimum (the maximum of the negative values)

# Simple example from https://www.datasciencemadesimple.com/row-wise-minimum-row-minimum-in-dataframe-r-2/
# df1 = data.frame( Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
#                   Mathematics1_score=c(62,47,55,74,32,77,86),
#                   Mathematics2_score=c(45,78,44,89,66,49,72),
#                   Science_score=c(56,52,45,88,33,90,47))
# df1 %>% rowwise() %>% 
#  mutate( Min_score = min(c(Mathematics1_score,Mathematics2_score,Science_score)))

# Another way. Involves changing variable names
# test <- head(fourYears200,30)
# names(test) <- c("Name","School","Gender","G9", "G10","G11","G12","PR")
# test %>% rowwise() %>% mutate(BestGrade = which.min(c(G9,G10,G11,G12)))

# By far the easiest way!
fourYears200$PR <- apply(fourYears200[,4:7],1,min)
fourYears200$PRGrade <- apply(fourYears200[,4:7],1,which.min)
fourYears200 <- fourYears200 %>% mutate(
  BestYear = case_when(
    PRGrade == 4 ~ 12,
    PRGrade == 3 ~ 11,
    PRGrade == 2 ~ 10,
    PRGrade == 1 ~ 9)
)
# Frequency tables to show which year was the best year overall and for both genders
table(fourYears200$BestYear)
freq200 <- table(fourYears200$BestYear, fourYears200$Gender)
xtable(freq200)

# Gather the data into long format for later use.
# Get 4 year data in long format
fourYears200long <- fourYears200 %>%
  pivot_longer(!c(Name, School, Gender, PRGrade, PR, BestYear),
               names_to = "Grade", values_to="BestTime")

# 400 Data
track400m = read.csv("./track400clean.csv",stringsAsFactors = TRUE)

# Convert to tibbles
track400m = as_tibble(track400m[,-1])

#Create gender variable for simplicity
# track400m = track400m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))
#  Double check with previous Gender variable
# table(track400m$Gender2)
# table(track400m$Gender)
# table(track400m$Gender,track400m$Gender2)

# A check revealed that there are 11 'Events' listed as 'Mens Varsity Girls 400 Meters'. 
# All should be classified as Mens. Similarly for "Womens Varisty Boys...". 
# track400m[which((track400m$Gender == "Mens") & (track400m$Gender2 == "F")),]
# track400m$Event[which((track400m$Gender == "Mens") & (track400m$Gender2 == "F"))]
# Correct the errors
err <- grepl('Mens Varsity Girls',track400m$Event)
err2 <- grepl('Womens Varsity Boys',track400m$Event)
track400m$Event[err=='TRUE'] <- 'Mens Varsity 400 Meters'
track400m$Event[err2=='TRUE'] <- 'Womens Varsity 400 Meters'
# Now create simple Gender variable. Don't need it, but glad I did it. 
# Wouldn't have found the typos in the events otherwise!
# track400m = track400m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))

# Investigate left_join using mtcars data
# left_join(mtcars2,mtcars,by=c('cyl','vs'))

# For each combination of Name, school, Gender, and Grade find the number of events 
# in which an athelete participated and the best time for that athlete in that grade
bestTimes400 = track400m %>%
  group_by(Name,School,Gender, Grade) %>% 
  summarise(NumRaces=n(),BestTime=min(Time)) %>% 
  filter(NumRaces >= 4)
# Merge with original data to have minimum times for each grade included in the data frame
# track400 = left_join(bestTimes400,track400m,by=c('Name','School','Gender'))
# nrow(track400)
## June 23: Thought for later - look at the times as a function of the number of events in a year 
## Requires all times from each year. Not sure how to do this yet. 
## Idea - plot of best time vs. grade with size of circle representing how many events in that grade

# Find the athletes that competed all four years of high school
# This data set include both boys and girls
fourYears400 = track400m %>%
  group_by(Name,School,Gender, Grade)%>% summarise(BestTime=min(Time))
fourYears400 <- fourYears400 %>% spread(Grade, BestTime)
fourYears400 <- fourYears400[complete.cases(fourYears400),]
nrow(fourYears400)

### Start here next time - find grade in which best of the best times occurs for boys and girls
# This is the minumum time. How do we find the variable at which the minimum time occurs?

# One way. Not satisfactory because have to extract columns, convert to matrix 
# and then add the result back to the data frame.
# Put data into a matrix
# Use max.col(-mat, ties.method="first") to find minimum (the maximum of the negative values)

# Simple example from https://www.datasciencemadesimple.com/row-wise-minimum-row-minimum-in-dataframe-r-2/
# df1 = data.frame( Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
#                   Mathematics1_score=c(62,47,55,74,32,77,86),
#                   Mathematics2_score=c(45,78,44,89,66,49,72),
#                   Science_score=c(56,52,45,88,33,90,47))
# df1 %>% rowwise() %>% 
#  mutate( Min_score = min(c(Mathematics1_score,Mathematics2_score,Science_score)))

# Another way. Involves changing variable names
# test <- head(fourYears400,30)
# names(test) <- c("Name","School","Gender","G9", "G10","G11","G12","PR")
# test %>% rowwise() %>% mutate(BestGrade = which.min(c(G9,G10,G11,G12)))

# By far the easiest way!
fourYears400$PR <- apply(fourYears400[,4:7],1,min)
fourYears400$PRGrade <- apply(fourYears400[,4:7],1,which.min)
fourYears400 <- fourYears400 %>% mutate(
  BestYear = case_when(
    PRGrade == 4 ~ 12,
    PRGrade == 3 ~ 11,
    PRGrade == 2 ~ 10,
    PRGrade == 1 ~ 9)
)
# Frequency tables to show which year was the best year overall and for both genders
table(fourYears400$BestYear)
freq400 <- table(fourYears400$BestYear, fourYears400$Gender)
xtable(freq400)

# Gather the data into long format for later use.
fourYears400long <- fourYears400 %>%
  pivot_longer(!c(Name, School, Gender, PRGrade, PR, BestYear),
               names_to = "Grade", values_to="BestTime")

# 800 Data
track800m = read.csv("./track800clean.csv",stringsAsFactors = TRUE)

# Convert to tibbles
track800m = as_tibble(track800m[,-1])

#Create gender variable for simplicity
# track800m = track800m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))
#  Double check with previous Gender variable
# table(track800m$Gender2)
# table(track800m$Gender)
# table(track800m$Gender,track800m$Gender2)

# A check revealed that there are 11 'Events' listed as 'Mens Varsity Girls 800 Meters'. 
# All should be classified as Mens. Similarly for "Womens Varisty Boys...". 
# track800m[which((track800m$Gender == "Mens") & (track800m$Gender2 == "F")),]
# track800m$Event[which((track800m$Gender == "Mens") & (track800m$Gender2 == "F"))]
# Correct the errors
err <- grepl('Mens Varsity Girls',track800m$Event)
err2 <- grepl('Womens Varsity Boys',track800m$Event)
track800m$Event[err=='TRUE'] <- 'Mens Varsity 800 Meters'
track800m$Event[err2=='TRUE'] <- 'Womens Varsity 800 Meters'
# Now create simple Gender variable. Don't need it, but glad I did it. 
# Wouldn't have found the typos in the events otherwise!
# track800m = track800m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))

# Investigate left_join using mtcars data
# left_join(mtcars2,mtcars,by=c('cyl','vs'))

# For each combination of Name, school, Gender, and Grade find the number of events 
# in which an athelete participated and the best time for that athlete in that grade
bestTimes800 = track800m %>%
  group_by(Name,School,Gender, Grade) %>% 
  summarise(NumRaces=n(),BestTime=min(Time)) %>% 
  filter(NumRaces >= 4)
# Merge with original data to have minimum times for each grade included in the data frame
# track800 = left_join(bestTimes800,track800m,by=c('Name','School','Gender'))
# nrow(track800)
## June 23: Thought for later - look at the times as a function of the number of events in a year 
## Requires all times from each year. Not sure how to do this yet. 
## Idea - plot of best time vs. grade with size of circle representing how many events in that grade

# Find the athletes that competed all four years of high school
# This data set include both boys and girls
fourYears800 = track800m %>%
  group_by(Name,School,Gender, Grade)%>% summarise(BestTime=min(Time))
fourYears800 <- fourYears800 %>% spread(Grade, BestTime)
fourYears800 <- fourYears800[complete.cases(fourYears800),]
nrow(fourYears800)
# Gather the data into long format for later use.
# fourYears800long <- fourYears800 %>% pivot_longer(!c(Name, School, Gender), names_to = "Grade", values_to="BestTime")
### Start here next time - find grade in which best of the best times occurs for boys and girls
# This is the minumum time. How do we find the variable at which the minimum time occurs?

# One way. Not satisfactory because have to extract columns, convert to matrix 
# and then add the result back to the data frame.
# Put data into a matrix
# Use max.col(-mat, ties.method="first") to find minimum (the maximum of the negative values)

# Simple example from https://www.datasciencemadesimple.com/row-wise-minimum-row-minimum-in-dataframe-r-2/
# df1 = data.frame( Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
#                   Mathematics1_score=c(62,47,55,74,32,77,86),
#                   Mathematics2_score=c(45,78,44,89,66,49,72),
#                   Science_score=c(56,52,45,88,33,90,47))
# df1 %>% rowwise() %>% 
#  mutate( Min_score = min(c(Mathematics1_score,Mathematics2_score,Science_score)))

# Another way. Involves changing variable names
# test <- head(fourYears800,30)
# names(test) <- c("Name","School","Gender","G9", "G10","G11","G12","PR")
# test %>% rowwise() %>% mutate(BestGrade = which.min(c(G9,G10,G11,G12)))

# By far the easiest way!
fourYears800$PR <- apply(fourYears800[,4:7],1,min)
fourYears800$PRGrade <- apply(fourYears800[,4:7],1,which.min)
fourYears800 <- fourYears800 %>% mutate(
  BestYear = case_when(
    PRGrade == 4 ~ 12,
    PRGrade == 3 ~ 11,
    PRGrade == 2 ~ 10,
    PRGrade == 1 ~ 9)
)
# Frequency tables to show which year was the best year overall and for both genders
table(fourYears800$BestYear)
freq800 <- table(fourYears800$BestYear, fourYears800$Gender)
xtable(freq800)

# Gather the data into long format for later use.
fourYears800long <- fourYears800 %>%
  pivot_longer(!c(Name, School, Gender, PRGrade, PR, BestYear),
               names_to = "Grade", values_to="BestTime")

# 1600 Data
track1600m = read.csv("./track1600clean.csv",stringsAsFactors = TRUE)

# Convert to tibbles
track1600m = as_tibble(track1600m[,-1])

#Create gender variable for simplicity
# track1600m = track1600m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))
#  Double check with previous Gender variable
# table(track1600m$Gender2)
# table(track1600m$Gender)
# table(track1600m$Gender,track1600m$Gender2)

# A check revealed that there are 11 'Events' listed as 'Mens Varsity Girls 1600 Meters'. 
# All should be classified as Mens. Similarly for "Womens Varisty Boys...". 
# track1600m[which((track1600m$Gender == "Mens") & (track1600m$Gender2 == "F")),]
# track1600m$Event[which((track1600m$Gender == "Mens") & (track1600m$Gender2 == "F"))]
# Correct the errors
err <- grepl('Mens Varsity Girls',track1600m$Event)
err2 <- grepl('Womens Varsity Boys',track1600m$Event)
track1600m$Event[err=='TRUE'] <- 'Mens Varsity 1600 Meters'
track1600m$Event[err2=='TRUE'] <- 'Womens Varsity 1600 Meters'
# Now create simple Gender variable. Don't need it, but glad I did it. 
# Wouldn't have found the typos in the events otherwise!
# track1600m = track1600m%>%
#  mutate(Gender2 = case_when(
#    grepl('Wom',Event)==TRUE ~ 'F',
#    grepl('Gir',Event)==TRUE ~ 'F',
#    TRUE ~ 'M'
#  ))

# Investigate left_join using mtcars data
# left_join(mtcars2,mtcars,by=c('cyl','vs'))

# For each combination of Name, school, Gender, and Grade find the number of events 
# in which an athelete participated and the best time for that athlete in that grade
bestTimes1600 = track1600m %>%
  group_by(Name,School,Gender, Grade) %>% 
  summarise(NumRaces=n(),BestTime=min(Time)) %>% 
  filter(NumRaces >= 4)
# Merge with original data to have minimum times for each grade included in the data frame
# track1600 = left_join(bestTimes1600,track1600m,by=c('Name','School','Gender'))
# nrow(track1600)
## June 23: Thought for later - look at the times as a function of the number of events in a year 
## Requires all times from each year. Not sure how to do this yet. 
## Idea - plot of best time vs. grade with size of circle representing how many events in that grade

# Find the athletes that competed all four years of high school
# This data set include both boys and girls
fourYears1600 = track1600m %>%
  group_by(Name,School,Gender, Grade)%>% summarise(BestTime=min(Time))
fourYears1600 <- fourYears1600 %>% spread(Grade, BestTime)
fourYears1600 <- fourYears1600[complete.cases(fourYears1600),]
nrow(fourYears1600)

### Find grade in which best of the best times occurs for boys and girls
# This is the minumum time. How do we find the variable at which the minimum time occurs?

# One way. Not satisfactory because have to extract columns, convert to matrix 
# and then add the result back to the data frame.
# Put data into a matrix
# Use max.col(-mat, ties.method="first") to find minimum (the maximum of the negative values)

# Simple example from https://www.datasciencemadesimple.com/row-wise-minimum-row-minimum-in-dataframe-r-2/
# df1 = data.frame( Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
#                   Mathematics1_score=c(62,47,55,74,32,77,86),
#                   Mathematics2_score=c(45,78,44,89,66,49,72),
#                   Science_score=c(56,52,45,88,33,90,47))
# df1 %>% rowwise() %>% 
#  mutate( Min_score = min(c(Mathematics1_score,Mathematics2_score,Science_score)))

# Another way. Involves changing variable names
# test <- head(fourYears1600,30)
# names(test) <- c("Name","School","Gender","G9", "G10","G11","G12","PR")
# test %>% rowwise() %>% mutate(BestGrade = which.min(c(G9,G10,G11,G12)))

# By far the easiest way!
fourYears1600$PR <- apply(fourYears1600[,4:7],1,min)
fourYears1600$PRGrade <- apply(fourYears1600[,4:7],1,which.min)
fourYears1600 <- fourYears1600 %>% mutate(
  BestYear = case_when(
    PRGrade == 4 ~ 12,
    PRGrade == 3 ~ 11,
    PRGrade == 2 ~ 10,
    PRGrade == 1 ~ 9)
)
# Frequency tables to show which year was the best year overall and for both genders
# Frequency tables to show which year was the best year overall and for both genders
table(fourYears1600$BestYear)
freq1600 <- table(fourYears1600$BestYear, fourYears1600$Gender)
xtable(freq1600)

# Gather the data into long format for later use.
fourYears1600long <- fourYears1600 %>%
  pivot_longer(!c(Name, School, Gender, PRGrade, PR, BestYear),
               names_to = "Grade", values_to="BestTime")


# Gather all races to one dataset
fourYears200long$Event = "200 meters"
fourYears400long$Event = "400 meters"
fourYears800long$Event = "800 meters"
fourYears1600long$Event = "1600 meters"

fourYearslong = rbind(fourYears200long,fourYears400long,fourYears800long,fourYears1600long)
# Mixed model with random intercept with unstructured covariance (can't get other cov with lmer)
library(lme4)
mixeda = lmer(BestTime ~ Gender*Grade + Gender*Event + (1 | Name), data = fourYearslong)
summary(mixeda)
confint(mixeda)

#Stratified analysis by gender
girlsTimes = fourYearslong%>%filter(Gender=="F")
boysTimes = fourYearslong%>%filter(Gender=="M")
mixed_girl = lmer(BestTime ~ Grade + Event + (1 | Name), data = girlsTimes)
mixed_boy = lmer(BestTime ~ Grade + Event + (1 | Name), data = boysTimes)
