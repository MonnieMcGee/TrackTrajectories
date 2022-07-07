# Analysis of track data using natural logs as percent changes
# The code below creates error bar plots
# Created June 8, 2022
# Last update June 23, 2022

# libraries used
library(tidyverse)

# enter the data
#setwd("/Users/Monnie/Dropbox/2022Spring/Research/TrackPaperTAS2022/Data")
setwd("/Users/monniemcgee/Dropbox/2022Spring/Research/TrackPaperTAS2022/Data")
fourYears200long <- read.csv(file="fourYears200long.csv", header=T)
fourYears200long <- fourYears200long[,-1]
fourYears400long <- read.csv(file="fourYears400long.csv", header=T)
fourYears400long <- fourYears400long[,-1]
fourYears800long <- read.csv(file="fourYears800long.csv", header=T)
fourYears800long <- fourYears800long[,-1]
fourYears1600long <- read.csv(file="fourYears1600long.csv", header=T)
fourYears1600long <- fourYears1600long[,-1]

# Gender as factor
fourYears200long$Gender <- factor(fourYears200long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears400long$Gender <- factor(fourYears400long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears800long$Gender <- factor(fourYears800long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))
fourYears1600long$Gender <- factor(fourYears1600long$Gender,levels=c("Womens","Mens"),labels=c("Girls","Boys"))

# Add a column for Log Transformed best times 
fourYears200long <- fourYears200long %>% 
  mutate(LogBestTime = log(BestTime)) 
fourYears400long <- fourYears400long %>% 
  mutate(LogBestTime = log(BestTime)) 
fourYears800long <- fourYears800long %>% 
  mutate(LogBestTime = log(BestTime)) 
fourYears1600long <- fourYears1600long %>% 
  mutate(LogBestTime = log(BestTime)) 

# Subset by Gender
fourYears200longB <- subset(fourYears200long, Gender=="Boys")
fourYears200longG <- subset(fourYears200long, Gender=="Girls")
fourYears400longB <- subset(fourYears400long, Gender=="Boys")
fourYears400longG <- subset(fourYears400long, Gender=="Girls")
fourYears800longB <- subset(fourYears800long, Gender=="Boys")
fourYears800longG <- subset(fourYears800long, Gender=="Girls")
fourYears1600longB <- subset(fourYears1600long, Gender=="Boys")
fourYears1600longG <- subset(fourYears1600long, Gender=="Girls")

# logs as percent differences
# The percentage change in Y at period t is defined as (Y_t-Y_(t-1)/Y_(t-1), 
# which is approximately equal to LN(Y_t) - LN(Y_(t-1) for small changes.
# For example, LN(BestTime in Grade 10) - LN(BestTime in Grade 9) is approximately 
# equal to (Time in Grade 10 - Time in Grade 9)/Time in Grade 9.
# Lines on plots are different grades, panels are gender
ggplot(fourYears200long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=25) + facet_grid(Gender~.) + 
  ggtitle("Log of Best Time for 200 meter by Grade") +
  theme(legend.position="top", legend.title=element_blank())
ggplot(fourYears400long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=25) + facet_grid(Gender~.) +
  ggtitle("Log of Best Time for 400 meter by Grade") +
  theme(legend.position="top", legend.title=element_blank())
ggplot(fourYears800long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=15) + facet_grid(Gender~.) +
  ggtitle("Log of Best Time for 800 meter by Grade") +
  theme(legend.position="top", legend.title=element_blank())
ggplot(fourYears1600long, aes(LogBestTime, after_stat(density), colour = Grade)) +
  geom_freqpoly(bins=30) + facet_grid(Gender~.) +
  ggtitle("Log of Best Time for 1600 meter by Grade") +
  theme(legend.position="top", legend.title=element_blank())

# Differences in logs $\approx$ percent changes
# need differences by gender and event
# Calculate differences in logs
# Need wide data - row for each athlete with column for best time in each grade
# Boy - this was a pain. Took me over an hour to get it right.

# Percentage changes for Boys
fourYears200wideB <- fourYears200longB %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
            values_from = LogBestTime) %>% select(-row)
fourYears200wideB <- fourYears200wideB %>% mutate(diff4year = `12`-`9`,
                                    diffSrJr = `12`-`11`,
                                    diffJrSo = `11`-`10`,
                                    diffSoFr = `10`- `9`)
fourYears400wideB <- fourYears400longB %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears400wideB <- fourYears400wideB %>% mutate(diff4year = `12`-`9`,
                                                  diffSrJr = `12`-`11`,
                                                  diffJrSo = `11`-`10`,
                                                  diffSoFr = `10`- `9`)
fourYears800wideB <- fourYears800longB %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears800wideB <- fourYears800wideB %>% mutate(diff4year = `12`-`9`,
                                                  diffSrJr = `12`-`11`,
                                                  diffJrSo = `11`-`10`,
                                                  diffSoFr = `10`- `9`)
fourYears1600wideB <- fourYears1600longB %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears1600wideB <- fourYears1600wideB %>% mutate(diff4year = `12`-`9`,
                                                    diffSrJr = `12`-`11`,
                                                    diffJrSo = `11`-`10`,
                                                    diffSoFr = `10`- `9`)

# Percentage changes for Girls
fourYears200wideG <- fourYears200longG %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears200wideG <- fourYears200wideG %>% mutate(diff4year = `12`-`9`,
                                                  diffSrJr = `12`-`11`,
                                                  diffJrSo = `11`-`10`,
                                                  diffSoFr = `10`- `9`)
fourYears400wideG <- fourYears400longG %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears400wideG <- fourYears400wideG %>% mutate(diff4year = `12`-`9`,
                                                  diffSrJr = `12`-`11`,
                                                  diffJrSo = `11`-`10`,
                                                  diffSoFr = `10`- `9`)
fourYears800wideG <- fourYears800longG %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears800wideG <- fourYears800wideG %>% mutate(diff4year = `12`-`9`,
                                                  diffSrJr = `12`-`11`,
                                                  diffJrSo = `11`-`10`,
                                                  diffSoFr = `10`- `9`)
fourYears1600wideG <- fourYears1600longG %>% select(Name, Grade, LogBestTime) %>%
  group_by(Grade) %>% mutate(row = row_number()) %>%
  pivot_wider(names_from=Grade, 
              values_from = LogBestTime) %>% select(-row)
fourYears1600wideG <- fourYears1600wideG %>% mutate(diff4year = `12`-`9`,
                                                    diffSrJr = `12`-`11`,
                                                    diffJrSo = `11`-`10`,
                                                    diffSoFr = `10`- `9`)

## Summary statistics
summary(fourYears200wideB)
summary(fourYears200wideG)
summary(fourYears400wideB)
summary(fourYears400wideG)
summary(fourYears800wideB)
summary(fourYears800wideG)
summary(fourYears1600wideB)
summary(fourYears1600wideG)

# Put differences into data frame
gdiff200 <- fourYears200wideG %>% select(Name, diff4year:diffSoFr)
bdiff200 <- fourYears200wideB %>% select(Name, diff4year:diffSoFr)
diff200 = bind_rows(gdiff200, bdiff200, .id= "Gender")
diff200 <- diff200 %>%
  mutate(Gender = case_when(Gender == 1 ~ 'Girls', Gender == 2 ~ 'Boys'))
gdiff400 <- fourYears400wideG %>% select(Name, diff4year:diffSoFr)
bdiff400 <- fourYears400wideB %>% select(Name, diff4year:diffSoFr)
diff400 = bind_rows(gdiff400, bdiff400, .id= "Gender")
diff400 <- diff400 %>%
  mutate(Gender = case_when(Gender == 1 ~ 'Girls', Gender == 2 ~ 'Boys'))
gdiff800 <- fourYears800wideG %>% select(Name, diff4year:diffSoFr)
bdiff800 <- fourYears800wideB %>% select(Name, diff4year:diffSoFr)
diff800 = bind_rows(gdiff800, bdiff800, .id= "Gender")
diff800 <- diff800 %>%
  mutate(Gender = case_when(Gender == 1 ~ 'Girls', Gender == 2 ~ 'Boys'))
gdiff1600 <- fourYears1600wideG %>% select(Name, diff4year:diffSoFr)
bdiff1600 <- fourYears1600wideB %>% select(Name, diff4year:diffSoFr)
diff1600 = bind_rows(gdiff1600, bdiff1600, .id= "Gender")
diff1600 <- diff1600 %>%
  mutate(Gender = case_when(Gender == 1 ~ 'Girls', Gender == 2 ~ 'Boys'))

## Data frame of summary statistics
# Advanced pivoting: https://dcl-wrangle.stanford.edu/pivot-advanced.html
# Stack overflow: https://stackoverflow.com/questions/59253987/parallel-pivot-longer-of-two-sets-of-columns
sum200 <- diff200 %>%
  group_by(Gender) %>%
  summarise(SoFr_sd=sd(diffSoFr), JrSo_sd=sd(diffJrSo), SrJr_sd=sd(diffSrJr),All_sd=sd(diff4year),
            SoFr_mean=mean(diffSoFr), JrSo_mean=mean(diffJrSo), SrJr_mean=mean(diffSrJr),All_mean=mean(diff4year))
sum200
sum200long <- sum200 %>% 
  pivot_longer(
    cols = !Gender,
    names_to = c("Year", ".value"),
    names_sep = "_",
  )
sum400 <- diff400 %>%
  group_by(Gender) %>%
  summarise(SoFr_sd=sd(diffSoFr), JrSo_sd=sd(diffJrSo), SrJr_sd=sd(diffSrJr),All_sd=sd(diff4year),
            SoFr_mean=mean(diffSoFr), JrSo_mean=mean(diffJrSo), SrJr_mean=mean(diffSrJr),All_mean=mean(diff4year))
sum400
sum400long <- sum400 %>% 
  pivot_longer(
    cols = !Gender,
    names_to = c("Year", ".value"),
    names_sep = "_",
  )

sum800 <- diff800 %>%
  group_by(Gender) %>%
  summarise(SoFr_sd=sd(diffSoFr), JrSo_sd=sd(diffJrSo), SrJr_sd=sd(diffSrJr),All_sd=sd(diff4year),
            SoFr_mean=mean(diffSoFr), JrSo_mean=mean(diffJrSo), SrJr_mean=mean(diffSrJr),All_mean=mean(diff4year))
sum800
sum800long <- sum800 %>% 
  pivot_longer(
    cols = !Gender,
    names_to = c("Year", ".value"),
    names_sep = "_",
  )

sum1600 <- diff1600 %>%
  group_by(Gender) %>%
  summarise(SoFr_sd=sd(diffSoFr), JrSo_sd=sd(diffJrSo), SrJr_sd=sd(diffSrJr),All_sd=sd(diff4year),
            SoFr_mean=mean(diffSoFr), JrSo_mean=mean(diffJrSo), SrJr_mean=mean(diffSrJr),All_mean=mean(diff4year))
sum1600
sum1600long <- sum1600 %>% 
  pivot_longer(
    cols = !Gender,
    names_to = c("Year", ".value"),
    names_sep = "_",
  )

allSummary1 = bind_rows(sum200,sum1600, .id= "Event")
allSummary2 <- bind_rows(sum400, sum800, .id="Event")
allSummary1 <- allSummary1 %>%
  mutate(Event = case_when(Event == 1 ~ '200m', 
                           Event == 2 ~ '1600m'))
allSummary2 <- allSummary2 %>%
  mutate(Event = case_when(Event == 1 ~ '400m', 
                           Event == 2 ~ '800m'))
allSummary <- bind_rows(allSummary1, allSummary2)
xtable(allSummary1)
xtable(allSummary2)

# Horizontal error bars with mean points
# Change the color by groups
pd <- position_dodge(0.2) # move them .05 to the left and right
# Help from: http://www.cookbook-r.com/Graphs/
# Color blind friendly palette
mypal <- c("#0072B2", "#CC79A7")
ggplot(sum200long, aes(x = Year, y = mean, colour=Gender)) +
  theme_minimal() + 
  ggtitle("Percent Change by Year and Gender for 200m") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) + 
  geom_point(position=pd, size=3) +
  scale_colour_manual(values=mypal) +
  scale_x_discrete(limits = c("SoFr", "JrSo", "SrJr","All"), 
                   labels=c("9-10","10-11","11-12","Overall")) +
  theme(legend.position="topleft", legend.title=element_blank()) +
  labs(y="Percent Change", x=NULL)

ggplot(sum400long, aes(x = Year, y = mean, colour=Gender)) +
  theme_minimal() + 
  ggtitle("Percent Change by Year and Gender for 400m") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) + 
  geom_point(position=pd, size=3) +
  scale_colour_manual(values=mypal) +
  scale_x_discrete(limits = c("SoFr", "JrSo", "SrJr","All"), 
                   labels=c("9-10","10-11","11-12","Overall")) +
  theme(legend.position="topleft", legend.title=element_blank()) +
  labs(y="Percent Change", x=NULL)

ggplot(sum800long, aes(x = Year, y = mean, colour=Gender)) +
  theme_minimal() + 
  ggtitle("Percent Change by Year and Gender for 800m") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) + 
  geom_point(position=pd, size=3) +
  scale_colour_manual(values=mypal) +
  scale_x_discrete(limits = c("SoFr", "JrSo", "SrJr","All"), 
                   labels=c("9-10","10-11","11-12","Overall")) +
  theme(legend.position="topleft", legend.title=element_blank()) +
  labs(y="Percent Change", x=NULL)

ggplot(sum1600long, aes(x = Year, y = mean, colour=Gender)) +
  theme_minimal() + 
  ggtitle("Percent Change by Year and Gender for 1600m") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) + 
  geom_point(position=pd, size=3) +
  scale_colour_manual(values=mypal) +
  scale_x_discrete(limits = c("SoFr", "JrSo", "SrJr","All"), 
                   labels=c("9-10","10-11","11-12","Overall")) +
  theme(legend.position="topleft", legend.title=element_blank()) +
  labs(y="Percent Change", x=NULL)

# Start here - delete column for 4 year differences
# Histograms of differences
# gfg_plot < - ggplot(gfg, aes(x, y)) +
#  geom_point() + facet_grid(.~group)

# gfg$group < - factor(gfg$group, levels=c("E", "B", "A", "C", "D"))
# gfg_plot < - ggplot(gfg, aes(x, y)) + geom_point() +
#   facet_grid(.~group)
# labels <- c(Female = "Women", Male = "Men")
# sp + facet_grid(. ~ sex, labeller=labeller(sex = labels))


names(diff200) <- c("Gender", "Name", "4year", "SrJr", "JrSo", "SoFr")
labels <- c(SoFr = "10th - 9th", JrSo="11th - 10th", SrJr = "12th - 11th")
slopes200 = diff200 %>% 
  pivot_longer(
    cols = !c(Gender,Name),
    names_to = "Grade",
    values_to = "Slope",
  )
slopes200a <- slopes200 %>% filter(Grade != "4year")
slopes200a$Grade <- factor(slopes200a$Grade, levels=c("SoFr", "JrSo", "SrJr"))
gmeans <- slopes200a %>% group_by(Gender, Grade) %>% summarise(mean = mean(Slope,na.rm=T)*100)
gmeans$mean <- sprintf("Mean Percent Change = %2.1f", gmeans$mean)
ggplot(data=slopes200a,aes(x=Slope, fill=Gender)) + 
  scale_fill_manual(values=mypal) +
  geom_histogram(bins=50) + 
  labs(title="Differences in Times for 200m by Gender and Grade", x="Difference in Mean Times", y='Count') + 
  facet_grid(Grade~Gender, labeller=labeller(Grade = labels)) + theme(legend.position = 'none') +
  geom_text(x = -.15, y = 30, aes(label = mean), data = gmeans, size=3)

names(diff400) <- c("Gender", "Name", "4year", "SrJr", "JrSo", "SoFr")
slopes400 = diff400 %>% 
  pivot_longer(
    cols = !c(Gender,Name),
    names_to = "Grade",
    values_to = "Slope",
  )
slopes400a <- slopes400 %>% filter(Grade != "4year")
slopes400a$Grade <- factor(slopes400a$Grade, levels=c("SoFr", "JrSo", "SrJr"))
gmeans <- slopes400a %>% group_by(Gender, Grade) %>% summarise(mean = mean(Slope,na.rm=T)*100)
gmeans$mean <- sprintf("Mean Percent Change = %2.1f", gmeans$mean)
ggplot(data=slopes400a,aes(x=Slope, fill=Gender)) + 
  scale_fill_manual(values=mypal) +
  geom_histogram(bins=50) + 
  labs(title="Differences in Times for 400m by Gender and Grade", x="Difference in Mean Times", y='Count') + 
  facet_grid(Grade~Gender, labeller=labeller(Grade = labels)) + theme(legend.position = 'none') +
  geom_text(x = .15, y = 22, aes(label = mean), data = gmeans, size=3)

names(diff800) <- c("Gender", "Name", "4year", "SrJr", "JrSo", "SoFr")
slopes800 = diff800 %>% 
  pivot_longer(
    cols = !c(Gender,Name),
    names_to = "Grade",
    values_to = "Slope",
  )
slopes800a <- slopes800 %>% filter(Grade != "4year")
slopes800a$Grade <- factor(slopes800a$Grade, levels=c("SoFr", "JrSo", "SrJr"))
gmeans <- slopes800a %>% group_by(Gender, Grade) %>% summarise(mean = mean(Slope,na.rm=T)*100)
gmeans$mean <- sprintf("Mean Percent Change = %2.1f", gmeans$mean)
ggplot(data=slopes800a,aes(x=Slope, fill=Gender)) + 
  scale_fill_manual(values=mypal) +
  geom_histogram(bins=30) + 
  labs(title="Differences in Times for 800m by Gender and Grade", x="Difference in Mean Times", y='Count') + 
  facet_grid(Grade~Gender, labeller=labeller(Grade = labels)) + theme(legend.position = 'none') +
  geom_text(x = -.08, y = 12, aes(label = mean), data = gmeans, size=3)

names(diff1600) <- c("Gender", "Name", "4year", "SrJr", "JrSo", "SoFr")
slopes1600 = diff1600 %>% 
  pivot_longer(
    cols = !c(Gender,Name),
    names_to = "Grade",
    values_to = "Slope",
  )
slopes1600a <- slopes1600 %>% filter(Grade != "4year")
slopes1600a$Grade <- factor(slopes1600a$Grade, levels=c("SoFr", "JrSo", "SrJr"))
gmeans <- slopes1600a %>% group_by(Gender, Grade) %>% summarise(mean = mean(Slope,na.rm=T)*100)
gmeans$mean <- sprintf("Mean Percent Change = %2.1f", gmeans$mean)
ggplot(data=slopes1600a,aes(x=Slope, fill=Gender)) + 
  scale_fill_manual(values=mypal) +
  geom_histogram(bins=50) + 
  labs(title="Differences in Times for 1600m by Gender and Grade", x="Difference in Mean Times", y='Count') + 
  facet_grid(Grade~Gender, labeller=labeller(Grade = labels)) + theme(legend.position = 'none') +
  geom_text(x = -.15, y = 50, aes(label = mean), data = gmeans, size=3)

## Boxplots
slopes200$Grade <- factor(slopes200$Grade, levels=c("SoFr", "JrSo", "SrJr", "4year"))
ggplot(data=slopes200)+geom_boxplot(aes(x=Grade,y=Slope,fill=Gender)) +
  scale_fill_manual(values=mypal, labels=c("Boys","Girls")) +
  scale_x_discrete(labels=c("SoFr" = "10th - 9th", "JrSo" = "11th - 10th", "SrJr" = "12th - 11th",
                            "4year" = "Overall")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Change in Time (log scale)") + 
  ggtitle("Difference in Time for Consecutive Grades (200m)")

slopes400$Grade <- factor(slopes400$Grade, levels=c("SoFr", "JrSo", "SrJr", "4year"))
ggplot(data=slopes400)+geom_boxplot(aes(x=Grade,y=Slope,fill=Gender)) +
  scale_fill_manual(values=mypal, labels=c("Boys","Girls")) +
  scale_x_discrete(labels=c("SoFr" = "10th - 9th", "JrSo" = "11th - 10th", "SrJr" = "12th - 11th",
                            "4year" = "Overall")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Change in Time (log scale)") + 
  ggtitle("Difference in Time for Consecutive Grades (400m)")

slopes800$Grade <- factor(slopes800$Grade, levels=c("SoFr", "JrSo", "SrJr", "4year"))
ggplot(data=slopes800)+geom_boxplot(aes(x=Grade,y=Slope,fill=Gender)) +
  scale_fill_manual(values=mypal, labels=c("Boys","Girls")) +
  scale_x_discrete(labels=c("SoFr" = "10th - 9th", "JrSo" = "11th - 10th", "SrJr" = "12th - 11th",
                            "4year" = "Overall")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Change in Time (log scale)") + 
  ggtitle("Difference in Time for Consecutive Grades (800m)")

slopes1600$Grade <- factor(slopes1600$Grade, levels=c("SoFr", "JrSo", "SrJr", "4year"))
ggplot(data=slopes1600)+geom_boxplot(aes(x=Grade,y=Slope,fill=Gender)) +
  scale_fill_manual(values=mypal, labels=c("Boys","Girls")) +
  scale_x_discrete(labels=c("SoFr" = "10th - 9th", "JrSo" = "11th - 10th", "SrJr" = "12th - 11th",
                            "4year" = "Overall")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Change in Time (log scale)") + 
  ggtitle("Difference in Time for Consecutive Grades (1600m)")

