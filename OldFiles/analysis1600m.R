## Code for analysis of 1600m Track Data
## input is clean data from AppropriatePathName/track1600clean.csv
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(xtable)

setwd("/Users/monniemcgee/Dropbox/2021Spring/Research/TrackData/Data")
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

png("AllTraj1600_font.png")
ggplot(data=fourYears1600long) +
  geom_line(size=1,show.legend=FALSE,aes(x=as.numeric(Grade),y=BestTime,color=Name)) + 
  scale_color_grey(start=0.8,end=0.2) + scale_y_reverse(limits=c(440,240)) + 
  facet_wrap(~Gender,nrow=2, strip.position = "left") + theme_bw() +
  labs(x="Grade",y="Best Time (in seconds)",title="Best Times in 1600m by Grade")+theme(plot.title=element_text(size=15),axis.title=element_text(size=12))
# Saved as "AllTraj1600.png" in Track Trajectories directory.
dev.off()

################################# Analysis for Boys ########################################
boy1600m = fourYears1600 %>%
  filter(Gender=='Mens')
kb <- nrow(boy1600m)
write.csv(boy1600m,file="boy1600m_cc.csv")

#Take sample of 20
set.seed(4321)
indB = sample(1:kb,20) # randomly sample 30 rows from boy1600m
boysamp16 = boy1600m[indB,] # select those rows to form a new data frame
boysamp16long = boysamp16 %>% pivot_longer(!c(Name, School, Gender, PRGrade, PR, BestYear), names_to = "Grade", values_to = "Time")

# Plot times for sample in spaghetti plot
ggplot(data=boysamp16long) + 
  geom_line(size=1,show.legend=FALSE,aes(x=as.numeric(Grade),y=Time,color=Name)) + 
  scale_y_reverse()+labs(x="Grade",y="Best Time (in seconds)",title="Boys' Best Times in 1600m by Grade")+theme_bw()


################################# Analysis for Girls ########################################
girl1600m = fourYears1600 %>%
  filter(Gender=='Womens')
kg <- nrow(girl1600m)
write.csv(girl1600m,file="girl1600m_cc.csv")

#Take sample of 20
set.seed(4321)
indG = sample(1:kg,20)
girlsamp16 = girl1600m[indG,]
girlsamp16long = girlsamp16 %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")

# Plot times for sample in spaghetti plot
ggplot(data=girlsamp16long) + 
  geom_line(size=1,show.legend=FALSE,aes(x=as.numeric(Grade),y=Time,color=Name)) + 
  scale_y_reverse()+labs(x="Grade",y="Best Time (in seconds)",title="Girls' Best Times in 1600m by Grade")+theme_bw()


#Take differences of 12th grade time minus 9th grade time
# Differences are Y - X, which means that if Y > X, the time is slower in grade Y. 
# If Y < X, then the time is faster in grade Y.
gdiff16 = girl1600m %>% mutate(Range4 = `12`-`9`,
                             Range3 = `12`-`11`,
                             Range2 = `11`-`10`,
                             Range1 = `10`-`9`)
gdiff16$Gender = 'Girls'
xtable(summary(gdiff16[,c(4:7,11:14)])) # get 5 number summary of girls data

#Take differences of 12th grade time minus 9th grade time
bdiff16 = boy1600m %>%mutate(Range4 = `12`-`9`,
                           Range3 = `12`-`11`,
                           Range2 = `11`-`10`,
                           Range1 = `10`-`9`)
bdiff16$Gender = 'Boys'
xtable(summary(bdiff16[,c(4:7,11:14)])) # get 5 number summary of boys data

# Combine Girls and Boys data
diff1600 = bind_rows(gdiff16,bdiff16, .id= "Gender")
diff1600 <- diff1600 %>%mutate(Gender = case_when(Gender == 1 ~ 'Girls',
                   Gender == 2 ~ 'Boys'))
# differences <- differences[,-15]
boxplot(Range4~Gender,data=diff1600)
ggplot(data=diff1600)+geom_boxplot(aes(x=Gender,y=Range4,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 12th grade - 9th grade",fill="Gender")
ggplot(data=diff1600)+geom_boxplot(aes(x=Gender,y=Range3,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 12th grade - 11th grade",fill="Gender")
ggplot(data=diff1600)+geom_boxplot(aes(x=Gender,y=Range2,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 11th grade - 10th grade",fill="Gender")
ggplot(data=diff1600)+geom_boxplot(aes(x=Gender,y=Range1,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 10th grade - 9th grade",fill="Gender")

# How do I get these boxplots onto one axis?
# Data need to be in long format
# I need Gender, Range4 - Range1, PR, and PRGrade
delt1600 <- diff1600 %>% select(c(Name, Gender,PR:Range1))
delt1600Long <- delt1600 %>% pivot_longer(Range4:Range1, names_to = "GradeRange", values_to = "Difference")
png("DiffTimeGrade1600_font.png")
ggplot(data=delt1600Long)+geom_boxplot(aes(x=GradeRange,y=Difference,fill=Gender)) +
scale_fill_manual(values=brewer.pal(3,"PuBu"), labels = c("Boys", "Girls")) +
scale_x_discrete(labels = c("10 to 9", "11 to 10", "12 to 11","12 to 9")) +
theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) +
ylab("Difference in Time between Grades (in seconds)") +
ggtitle("Difference in Time for Consecutive Grades (1600m)")+theme(plot.title=element_text(size=15),axis.title=element_text(size=12))
dev.off()
ggplot(data=delt1600Long)+geom_boxplot(aes(x=Gender,y=Difference,fill=GradeRange)) +
  scale_fill_manual(values=brewer.pal(4,"PuBu"), labels = c("10 to 9", "11 to 10", "12 to 11","12 to 9")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Difference in Time between Grades (in seconds)") + 
  ggtitle("Difference in Time for Consecutive Grades (1600m)")+theme(plot.title=element_text(size=15),axis.title=element_text(size=12))

# Create bar graph to show which grade was peak performance for 1600m by percentage
# Start here - variables and data sets need changing
peak1600 = delt1600 %>% group_by(Gender, BestYear)%>%
  summarise(Count=n())%>%
  mutate(Percent=Count/sum(Count))

ggplot(peak1600)+geom_bar(aes(x=BestYear,fill=Gender,y=Percent),position='dodge',stat='identity') +
  labs(x=" ",y="Percent Obtaining Personal Best",fill="Gender",title="1600m Personal Best Times per Grade by Gender") +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  scale_fill_manual(values=brewer.pal(4,"PiYG"))

################# Loess Curves
## Use full 4 year data here
# Plot loess curve for men and women and all events
# Example: polls_2008 %>% ggplot(aes(day, margin)) + geom_point() + 
# geom_smooth(method = "loess", span = 0.15, method.args = list(degree=1))
# Obtain panels for each level of a categorical variable
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
# Use vars() to supply faceting variables:
# p + facet_wrap(vars(class))

## Code below not working. How do I plot multiple loess curves, one for each athlete, on each panel?
 g1 = ggplot(data=fourYears1600,aes(x=BestYear, y=PR)) + scale_y_reverse() + geom_point() + 
  facet_wrap(vars(Gender)) + labs(x="Grade",y="Time",title="Time for the 1600m (in seconds)")  + 
  theme_bw()+theme(plot.title=element_text(size=15),axis.title=element_text(size=12))
png("Loess1600_font.png")
 g1 + geom_smooth(method = "loess", method.args = list(degree=1)) 
dev.off()

## Boostrap for slopes
nsims = 1000
boy1600m = track1600m %>% filter(Gender=="Mens")
nb <- nrow(boy1600m)
girl1600m = track1600m %>% filter(Gender=="Womens")
ng <- nrow(girl1600m)

# Sampling distribution of the slope for consecutive grades 
### Junior/Senior ###
# Male
boySlope1600 = numeric(nsims)
boySlope1600a = numeric(nsims)
boySlope1600b = numeric(nsims)
for(i in 1:nsims){
  ind = sample(1:nb,nb,replace=TRUE)
  samp = boy1600m[ind,]
  sample.boy = boy1600m %>%
    filter(Name %in% samp$Name)
  grade12.sample = sample.boy %>% filter(Grade==12)
  grade12.sample.mean = mean(grade12.sample$Time)
  grade11.sample = sample.boy%>%filter(Grade==11)
  grade11.sample.mean = mean(grade11.sample$Time)
  grade10.sample = sample.boy%>%filter(Grade==10)
  grade10.sample.mean = mean(grade10.sample$Time)
  grade09.sample = sample.boy%>%filter(Grade==9)
  grade09.sample.mean = mean(grade09.sample$Time)
  boySlope1600[i] =  grade12.sample.mean - grade11.sample.mean
  boySlope1600a[i] =  grade11.sample.mean - grade10.sample.mean
  boySlope1600b[i] =  grade10.sample.mean - grade09.sample.mean
}

#Female
girlSlope1600 = numeric(nsims)
girlSlope1600a = numeric(nsims)
girlSlope1600b = numeric(nsims)
for(i in 1:nsims){
  ind = sample(1:ng,ng,replace=TRUE)
  samp = girl1600m[ind,]
  sample.girl = girl1600m %>%
    filter(Name %in% samp$Name)
  grade12.sample = sample.girl %>% filter(Grade==12)
  grade12.sample.mean = mean(grade12.sample$Time)
  grade11.sample = sample.girl %>% filter(Grade==11)
  grade11.sample.mean = mean(grade11.sample$Time)
  grade10.sample = sample.girl%>%filter(Grade==10)
  grade10.sample.mean = mean(grade10.sample$Time)
  grade09.sample = sample.girl%>%filter(Grade==9)
  grade09.sample.mean = mean(grade09.sample$Time)
  girlSlope1600[i] =  grade12.sample.mean - grade11.sample.mean
  girlSlope1600a[i] =  grade11.sample.mean - grade10.sample.mean
  girlSlope1600b[i] =  grade10.sample.mean - grade09.sample.mean
}
# If mean slope is negative, then grade 12 is faster than grade 11.
slopes1600 = data.frame("Slope"=c(boySlope1600,boySlope1600a,boySlope1600b,girlSlope1600,girlSlope1600a,girlSlope1600b),
                       "Gender"=c(rep("M",nsims*3),rep("F",nsims*3)),"Grade"=c(rep(c(12,11,10),each=1000),rep(c(12,11,10),each=1000)))
gmeans <- slopes1600 %>% group_by(Gender, Grade) %>% summarise(mean = mean(Slope,na.rm=T))
gmeans$mean <- sprintf("Mean Slope = %.2f", gmeans$mean)
gmeans
png("DiffMeanTimes1600_font.png")
ggplot(data=slopes1600,aes(x=Slope)) + 
  geom_histogram(bins=100) + 
  labs(title="Differences in Mean Times for 1600m by Gender and Grade", x="Difference in Mean Times", y='Count') + 
  facet_grid(Grade~Gender) + theme(legend.position = 'none',plot.title=element_text(size=15),axis.title=element_text(size=12)) +
  geom_text(x = -8, y = 250, aes(label = mean), data = gmeans)
dev.off()
#geom_vline(linetype='longdash', data=subset(slopes200,slopes200$Gender=='F'),aes(xintercept=mean(Slope),color="red")) + 
#geom_vline(linetype='longdash',data=subset(slopes200, slopes200$Gender=='M'),aes(xintercept=mean(Slope),color="red")) + 

##Take mean of all slopes

# Gender Grade mean               
# 1 F         10 Mean Slope = -5.01 
# 2 F         11 Mean Slope = -6.04 
# 3 F         12 Mean Slope = -9.17 
# 4 M         10 Mean Slope = -12.35
# 5 M         11 Mean Slope = -8.42 
# 6 M         12 Mean Slope = -6.80 

# Mixed model with random intercept
library(lme4)
mixed1600a = lmer(BestTime ~ Gender + (1 | Name), data = fourYears1600long)
mixed1600b = lmer(BestTime ~ Grade + Gender + (1 | Name), data = fourYears1600long)
summary(mixed1600a)
summary(mixed1600b)
confint(mixed1600a)
confint(mixed1600b)
# Further analyses
install.packages("merTools")
library(merTools)
plotREsim(REsim(mixed1600b))  # plot the interval estimates
