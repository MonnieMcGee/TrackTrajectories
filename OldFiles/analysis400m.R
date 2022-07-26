## Code for analysis of Track Data
## input is clean data from Dropbox/2021Spring/Research/TrackData/trackX00clean.csv
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(xtable)

setwd("/Users/monniemcgee/Dropbox/2021Spring/Research/TrackData/Data")
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

png("AllTraj400_font.png")
ggplot(data=fourYears400long) +
  geom_line(size=1,show.legend=FALSE,aes(x=as.numeric(Grade),y=BestTime,color=Name)) + 
  scale_color_grey(start=0.8,end=0.2) + scale_y_reverse(limits=c(80,45)) + 
  facet_wrap(~Gender,nrow=2, strip.position = "left") + theme_bw() +
  labs(x="Grade",y="Best Time (in seconds)",title="Best Times in 400m by Grade")
# Saved as "AllTraj400.png" in Track Trajectories directory.
dev.off()

################################# Analysis for Boys ########################################
boy400m = fourYears400 %>%
  filter(Gender=='Mens')
kb <- nrow(boy400m)
write.csv(boy400m,file="boy400m_cc.csv")

#Take sample of 20
set.seed(4321)
indB = sample(1:kb,20) # randomly sample 30 rows from boy400m
boysamp4 = boy400m[indB,] # select those rows to form a new data frame
boysamp4long = boysamp4 %>% pivot_longer(!c(Name, School, Gender, PRGrade, PR, BestYear), names_to = "Grade", values_to = "Time")

# Plot times for sample in spaghetti plot
ggplot(data=boysamp4long) + 
  geom_line(size=1,show.legend=FALSE,aes(x=as.numeric(Grade),y=Time,color=Name)) + 
  scale_y_reverse()+labs(x="Grade",y="Best Time (in seconds)",title="Boys' Best Times in 400m by Grade")+theme_bw()


################################# Analysis for Girls ########################################
girl400m = fourYears400 %>%
  filter(Gender=='Womens')
kg <- nrow(girl400m)
write.csv(girl400m,file="girl400m_cc.csv")

#Take sample of 20
set.seed(4321)
indG = sample(1:kg,20)
girlsamp4 = girl400m[indG,]
girlsamp4long = girlsamp4 %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")

# Plot times for sample in spaghetti plot
ggplot(data=girlsamp4long) + 
  geom_line(size=1,show.legend=FALSE,aes(x=as.numeric(Grade),y=Time,color=Name)) + 
  scale_y_reverse()+labs(x="Grade",y="Best Time (in seconds)",title="Girls' Best Times in 400m by Grade")+theme_bw()


#Take differences of 12th grade time minus 9th grade time
# Differences are Y - X, which means that if Y > X, the time is slower in grade Y. 
# If Y < X, then the time is faster in grade Y.
gdiff400 = girl400m %>% mutate(Range4 = `12`-`9`,
                             Range3 = `12`-`11`,
                             Range2 = `11`-`10`,
                             Range1 = `10`-`9`)
gdiff400$Gender = 'Girls'
xtable(summary(gdiff400[,c(4:7,11:14)])) # get 5 number summary of girls data

#Take differences of 12th grade time minus 9th grade time
bdiff400 = boy400m %>%mutate(Range4 = `12`-`9`,
                           Range3 = `12`-`11`,
                           Range2 = `11`-`10`,
                           Range1 = `10`-`9`)
bdiff400$Gender = 'Boys'
xtable(summary(bdiff400[,c(4:7,11:14)])) # get 5 number summary of boys data

# Combine Girls and Boys data
diff400 = bind_rows(gdiff400,bdiff400, .id= "Gender")
diff400 <- diff400 %>%mutate(Gender = case_when(Gender == 1 ~ 'Girls',
                   Gender == 2 ~ 'Boys'))

# differences <- differences[,-15]
boxplot(Range4~Gender,data=differences)
ggplot(data=diff400)+geom_boxplot(aes(x=Gender,y=Range4,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 12th grade - 9th grade",fill="Gender")
ggplot(data=diff400)+geom_boxplot(aes(x=Gender,y=Range3,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 12th grade - 11th grade",fill="Gender")
ggplot(data=diff400)+geom_boxplot(aes(x=Gender,y=Range2,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 11th grade - 10th grade",fill="Gender")
ggplot(data=diff400)+geom_boxplot(aes(x=Gender,y=Range1,fill=Gender))+labs(x="Gender",y="Difference",title="Differences in time from 10th grade - 9th grade",fill="Gender")

# How do I get these boxplots onto one axis?
# Data need to be in long format
# I need Gender, Range4 - Range1, PR, and PRGrade
delt400 <- diff400 %>% select(c(Name, Gender,PR:Range1))
# Titles
# ggtitle("Plot of length \n by dose") + xlab("Dose (mg)") + ylab("Teeth length")
# hide axis titles
# theme(plot.title = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
delt400Long <- delt400 %>% 
  pivot_longer(Range4:Range1, names_to = "GradeRange", values_to = "Difference")
png("DiffTimeGrade400_font.png")
ggplot(data=delt400Long)+geom_boxplot(aes(x=GradeRange,y=Difference,fill=Gender)) +
  scale_fill_manual(values=brewer.pal(3,"PiYG"), labels = c("Boys", "Girls")) +
  scale_x_discrete(labels = c("10 to 9", "11 to 10", "12 to 11","12 to 9")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Difference in Time between Grades (in seconds)") + 
  ggtitle("Difference in Time for Consecutive Grades (400m)")
dev.off()
ggplot(data=delt400Long)+geom_boxplot(aes(x=Gender,y=Difference,fill=GradeRange)) +
  scale_fill_manual(values=brewer.pal(4,"PuBu"), labels = c("10 to 9", "11 to 10", "12 to 11","12 to 9")) +
  theme_bw() + theme(legend.position="bottom",axis.title.x = element_blank()) + 
  ylab("Difference in Time between Grades (in seconds)") + 
  ggtitle("Difference in Time for Consecutive Grades (400m)")


# Create bar graph to show which grade was peak performance for 400m by percentage

peak400 = delt400 %>% group_by(Gender, BestYear)%>%
  summarise(Count=n())%>%
  mutate(Percent=Count/sum(Count))

ggplot(peak400)+geom_bar(aes(x=BestYear,fill=Gender,y=Percent),position='dodge',stat='identity') +
  labs(x=" ",y="Percent Obtaining Personal Best",fill="Gender",title="400m Personal Best Times per Grade by Gender") +
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
g1 = ggplot(data=fourYears400,aes(x=BestYear, y=PR)) + scale_y_reverse() + geom_point() + 
  facet_wrap(vars(Gender)) + labs(x="Grade",y="Time",title="Time for the 400m (in seconds)")  + 
  theme_bw()
png("Loess400_font.png")
g1 + geom_smooth(method = "loess", method.args = list(degree=1))+theme(plot.title=element_text(size=15),axis.title=element_text(size=12)) 
dev.off()

##### Notes
# Suppose that between time 1 and 2, an intervention occurred, and we wish to fit a piecewise 
# linear model rather than an overall smooth. We can do this by creating a dummy variable 
# (pre/post intervention) and its interaction with time. The only change is a slightly more 
# complex formula. The default is y ~ x. “I(x > 1)” creates a dummy (TRUE/FALSE) variable if 
# x (time in this case) is greater than 1. The “*” in the formula asks for the main effects 
# and the interaction between x and the dummy variable from x. Of course ggplot2 takes care of 
# fitting the model separately by male and plotting it for us. Now we can see that the trend 
# line ‘jumps’ after time 1, and the slope is allowed to change (although the change appears 
# minimal suggestion there is not an interaction between our hypothetical intervention and time).

# p <- ggplot(data = tolerance, aes(x = time, y = tolerance, group = id))
# p + geom_line() + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x 
# * I(x > 1), se = FALSE) + stat_summary(aes(group = 1), fun.y = mean, geom = "point",
# shape = 17, size = 3) + facet_grid(. ~ male)

## Boostrap for slopes
nsims = 1000
boy400m = track400m %>% filter(Gender=="Mens")
nb <- nrow(boy400m)
girl400m = track400m %>% filter(Gender=="Womens")
ng <- nrow(girl400m)

# Sampling distribution of the slope for consecutive grades 
### Junior/Senior ###
# Male
boySlope400 = numeric(nsims)
boySlope400a = numeric(nsims)
boySlope400b = numeric(nsims)
for(i in 1:nsims){
  ind = sample(1:nb,nb,replace=TRUE)
  samp = boy400m[ind,]
  sample.boy = boy400m %>%
    filter(Name %in% samp$Name)
  grade12.sample = sample.boy %>% filter(Grade==12)
  grade12.sample.mean = mean(grade12.sample$Time)
  grade11.sample = sample.boy%>%filter(Grade==11)
  grade11.sample.mean = mean(grade11.sample$Time)
  grade10.sample = sample.boy%>%filter(Grade==10)
  grade10.sample.mean = mean(grade10.sample$Time)
  grade09.sample = sample.boy%>%filter(Grade==9)
  grade09.sample.mean = mean(grade09.sample$Time)
  boySlope400[i] =  grade12.sample.mean - grade11.sample.mean
  boySlope400a[i] =  grade11.sample.mean - grade10.sample.mean
  boySlope400b[i] =  grade10.sample.mean - grade09.sample.mean
}

#Female
girlSlope400 = numeric(nsims)
girlSlope400a = numeric(nsims)
girlSlope400b = numeric(nsims)
for(i in 1:nsims){
  ind = sample(1:ng,ng,replace=TRUE)
  samp = girl400m[ind,]
  sample.girl = girl400m %>%
    filter(Name %in% samp$Name)
  grade12.sample = sample.girl %>% filter(Grade==12)
  grade12.sample.mean = mean(grade12.sample$Time)
  grade11.sample = sample.girl %>% filter(Grade==11)
  grade11.sample.mean = mean(grade11.sample$Time)
  grade10.sample = sample.girl%>%filter(Grade==10)
  grade10.sample.mean = mean(grade10.sample$Time)
  grade09.sample = sample.girl%>%filter(Grade==9)
  grade09.sample.mean = mean(grade09.sample$Time)
  girlSlope400[i] =  grade12.sample.mean - grade11.sample.mean
  girlSlope400a[i] =  grade11.sample.mean - grade10.sample.mean
  girlSlope400b[i] =  grade10.sample.mean - grade09.sample.mean
}
# If mean slope is negative, then grade 12 is faster than grade 11.
slopes400 = data.frame("Slope"=c(boySlope400,boySlope400a,boySlope400b,girlSlope400,girlSlope400a,girlSlope400b),
                       "Gender"=c(rep("M",nsims*3),rep("F",nsims*3)),"Grade"=c(rep(c(12,11,10),each=1000),rep(c(12,11,10),each=1000)))
gmeans <- slopes400 %>% group_by(Gender, Grade) %>% summarise(mean = mean(Slope,na.rm=T))
gmeans$mean <- sprintf("Mean Slope = %.2f", gmeans$mean)
gmeans
png("DiffMeanTimes400_font.png")
ggplot(data=slopes400,aes(x=Slope)) + 
  geom_histogram(bins=100) + 
  labs(title="Differences in Mean Times for 400m by Gender and Grade", x="Difference in Mean Times", y='Count') + 
  facet_grid(Grade~Gender) + theme(legend.position = 'none',plot.title=element_text(size=15),axis.title=element_text(size=12)) +
  geom_text(x = -1.9, y = 175, aes(label = mean), data = gmeans)
dev.off()
#geom_vline(linetype='longdash', data=subset(slopes200,slopes200$Gender=='F'),aes(xintercept=mean(Slope),color="red")) + 
#geom_vline(linetype='longdash',data=subset(slopes200, slopes200$Gender=='M'),aes(xintercept=mean(Slope),color="red")) + 

##Take mean of all slopes
# Gender Grade mean              
# 1 F         10 Mean Slope = -1.23
# 2 F         11 Mean Slope = -1.25
# 3 F         12 Mean Slope = -1.46
# 4 M         10 Mean Slope = -2.25
# 5 M         11 Mean Slope = -1.34
# 6 M         12 Mean Slope = -1.18

# Mixed model with random intercept
library(lme4)
mixed400a = lmer(BestTime ~ Gender + (1 | Name), data = fourYears400long)
mixed400b = lmer(BestTime ~ Grade + Gender + (1 | Name), data = fourYears400long)
summary(mixed400a)
summary(mixed400b)
confint(mixed400a)
confint(mixed400b)
# Further analyses
install.packages("merTools")
library(merTools)
plotREsim(REsim(mixed400b))  # plot the interval estimates
