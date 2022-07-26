# Mixed model analysis for track data April 4, 2022
# input is fourYearsX00long, where X = 2, 4, 8, or 16

setwd("/Users/monniemcgee/Dropbox/2022Spring/Research/TrackPaperTAS2022/Data")
fourYears200long <- read.csv(file="fourYears200long.csv", header=T)
fourYears200long <- fourYears200long[,-1]
fourYears400long <- read.csv(file="fourYears400long.csv", header=T)
fourYears400long <- fourYears400long[,-1]
fourYears800long <- read.csv(file="fourYears800long.csv", header=T)
fourYears800long <- fourYears800long[,-1]
fourYears1600long <- read.csv(file="fourYears1600long.csv", header=T)
fourYears1600long <- fourYears1600long[,-1]

# Change Grade and Gender to factors
fourYears200long$Gender <- factor(fourYears200long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears200long$Grade <- factor(fourYears200long$Grade,levels=c(9, 10, 11, 12),labels=c("Nineth","Tenth","Eleventh","Twelfth"))
fourYears400long$Grade <- factor(fourYears400long$Grade,levels=c(9, 10, 11, 12),labels=c("Nineth","Tenth","Eleventh","Twelfth"))
fourYears400long$Gender <- factor(fourYears400long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears800long$Gender <- factor(fourYears800long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears800long$Grade <- factor(fourYears800long$Grade,levels=c(9, 10, 11, 12),labels=c("Nineth","Tenth","Eleventh","Twelfth"))
fourYears1600long$Gender <- factor(fourYears1600long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears1600long$Grade <- factor(fourYears1600long$Grade,levels=c(9, 10, 11, 12),labels=c("Nineth","Tenth","Eleventh","Twelfth"))

# Log Transformed data - added June 8, 2022
# logs as percent differences
# The percentage change in Y at period t is defined as (Y_t-Y_(t-1)/Y_(t-1), 
# which is approximately equal to LN(Y_t) - LN(Y_(t-1) for small changes.
# So, LN(BestTime in Grade 10) - LN(BestTime in Grade 9) is approximately 
# equal to (Time in Grade 10 - Time in Grade 9)/Time in Grade 9.
fourYears200long <- fourYears200long %>% mutate(LogBestTime = log(BestTime))
ggplot(fourYears200long, aes(LogBestTime)) + 
  geom_histogram() + facet_grid(Grade~Gender)
ggplot(fourYears200long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=25) + facet_grid(Gender~.)

fourYears400long <- fourYears400long %>% mutate(LogBestTime = log(BestTime))
ggplot(fourYears400long, aes(LogBestTime)) + 
  geom_histogram() + facet_grid(Grade~Gender)
ggplot(fourYears400long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=25) + facet_grid(Gender~.)

fourYears800long <- fourYears800long %>% mutate(LogBestTime = log(BestTime))
ggplot(fourYears800long, aes(LogBestTime)) + 
  geom_histogram() + facet_grid(Grade~Gender)
ggplot(fourYears800long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=15) + facet_grid(Gender~.)

fourYears1600long <- fourYears1600long %>% mutate(LogBestTime = log(BestTime))
ggplot(fourYears1600long, aes(LogBestTime)) + 
  geom_histogram() + facet_grid(Grade~Gender)
ggplot(fourYears1600long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=30) + facet_grid(Gender~.)

# Mixed model with random intercept with 
# unstructured covariance (can't get other cov with lmer)
# Best to do separate models for each event

library(lme4)
# Analysis stratified by Event
mixed200 = lmer(BestTime ~ Grade + Gender + Grade*Gender + (1 | Name), data = fourYears200long)
mixed400 = lmer(BestTime ~ Grade + Gender + Grade*Gender + (1 | Name), data = fourYears400long)
mixed800 = lmer(BestTime ~ Grade + Gender + Grade*Gender + (1 | Name), data = fourYears800long)
mixed1600 = lmer(BestTime ~ Grade + Gender + Grade*Gender + (1 | Name), data = fourYears1600long)
summary(mixed200)
confint(mixed200)
summary(mixed400)
confint(mixed400)
summary(mixed800)
confint(mixed800)
summary(mixed1600)
confint(mixed1600)



# Analysis stratified by Event and Gender
girls200Times = fourYearslong%>%filter(Gender=="Womens" & Event=="200 meters")
girls400Times = fourYearslong%>%filter(Gender=="Womens" & Event=="400 meters")
girls800Times = fourYearslong%>%filter(Gender=="Womens" & Event=="800 meters")
girls1600Times = fourYearslong%>%filter(Gender=="Womens" & Event=="1600 meters")
boys200Times = fourYearslong%>%filter(Gender=="Mens" & Event=="200 meters")
boys400Times = fourYearslong%>%filter(Gender=="Mens" & Event=="400 meters")
boys800Times = fourYearslong%>%filter(Gender=="Mens" & Event=="800 meters")
boys1600Times = fourYearslong%>%filter(Gender=="Mens" & Event=="1600 meters")
mixed_girl200 = lmer(BestTime ~ Grade + (1 | Name), data = girls200Times)
mixed_boy200 = lmer(BestTime ~ Grade + (1 | Name), data = boys200Times)
mixed_girl400 = lmer(BestTime ~ Grade + (1 | Name), data = girls400Times)
mixed_boy400 = lmer(BestTime ~ Grade + (1 | Name), data = boys400Times)
mixed_girl800 = lmer(BestTime ~ Grade + (1 | Name), data = girls800Times)
mixed_boy800 = lmer(BestTime ~ Grade + (1 | Name), data = boys800Times)
mixed_girl1600 = lmer(BestTime ~ Grade + (1 | Name), data = girls1600Times)
mixed_boy1600 = lmer(BestTime ~ Grade + (1 | Name), data = boys1600Times)
summary(mixed_girl200)
confint(mixed_girl200)
summary(mixed_boy200)
confint(mixed_boy200)
summary(mixed_girl400)
confint(mixed_girl400)
summary(mixed_boy400)
confint(mixed_boy400)
summary(mixed_girl800)
confint(mixed_girl800)
summary(mixed_boy800)
confint(mixed_boy800)
summary(mixed_girl1600)
confint(mixed_girl1600)
summary(mixed_boy1600)
confint(mixed_boy1600)

# Gather all races to one dataset
#fourYears200long$Event = "200 meters"
#fourYears400long$Event = "400 meters"
#fourYears800long$Event = "800 meters"
#fourYears1600long$Event = "1600 meters"

#fourYearslong = rbind(fourYears200long,fourYears400long,fourYears800long,fourYears1600long)
#write.csv(fourYearslong, file="fourYearsLongAll.csv")

#Stratified analysis by gender
girlsTimes = fourYearslong %>% filter(Gender=="Womens")
boysTimes = fourYearslong %>% filter(Gender=="Mens")
mixed_girl = lmer(BestTime ~ Grade + Event + (1 | Name), data = girlsTimes)
mixed_boy = lmer(BestTime ~ Grade + Event + (1 | Name), data = boysTimes)
summary(mixed_girl)
confint(mixed_girl)
summary(mixed_boy)
confint(mixed_boy)
