# Creating clusters of patterns for running trajectories
# Plot them in ggplot2

################### Types of clusters
# Steady increase (linear)
# Steady decrease (linear)
# Peak at grade 9, then flat (or increase in time)
# Peak at grade 10, then flat (or increase in time)
# Peak at grade 11, then flat (or increase in time)

# Reference for curve clustering
# https://stats.stackexchange.com/questions/17772/how-to-cluster-longitudinal-variables
# Packages kml and Hmisc (curveRep function)
# http://varianceexplained.org/r/kmeans-free-lunch/

# Import data
setwd("/Users/monniemcgee/Dropbox/2021Spring/Research/TrackData/Data")
girl1600m = read.csv("./girl1600m_cc.csv",stringsAsFactors = TRUE)
boy1600m = read.csv("./boy1600m_cc.csv",stringsAsFactors = TRUE)
boy1600m <- boy1600m[,-1]
girl1600m <- girl1600m[,-1]

# Using package kml
library(kml)
library(kmlShape)
library(tidyverse)
# First, create a clusterLongData object for use in function kml.
# Assumes boy1600m and girl1600m have been created using June2021Analysis1600m.R
boy1600temp <- boy1600m %>% ungroup() %>% select("X9","X10","X11","X12")
kb <- nrow(boy1600temp)
boy1600temp <- as.data.frame(boy1600temp)
boy1600cld <- clusterLongData(traj=boy1600temp,idAll=paste("I-",1:kb,sep=""),time=9:12,varNames="Grade")

## Same drill for girls
girl1600temp <- girl1600m %>% ungroup() %>% select("X9","X10","X11","X12")
kg <- nrow(girl1600temp)
girl1600temp <- as.data.frame(girl1600temp)
girl1600cld <- clusterLongData(traj=girl1600temp,idAll=paste("I-",1:kg,sep=""),time=9:12,varNames="Grade")

# Clustering using kmlShape
girl1600clds <- cldsWide(data.frame(1:kg,girl1600temp),times=9:12)
par(ask=FALSE)
kmlShape(girl1600clds,2)
kmlShape(girl1600clds,3)
girl1600clusters <- kmlShape(girl1600clds,4)
kmlShape(girl1600clds,5) # Did not converge after max iterations
kmlShape(girl1600clds,6) # Did not converge after max iterations
# Figure out how to export results to a data frame to use with 
# ggplot2 data. Or how to modify graphics from kmlShape. I need a 
# plot with just the cluster shapes on it. 

boy1600clds <- cldsWide(data.frame(1:kb,boy1600temp),times=9:12)
par(ask=FALSE)
kmlShape(boy1600clds,2)
boy1600clusters <- kmlShape(boy1600clds,3)
# Did not converge after max iterations
# kmlShape(boy1600clds,4)
# kmlShape(boy1600clds,5)
# kmlShape(boy1600clds,6)

# Problem: If variances of clusters are different, then k-means clustering will not work.
# How can we fix this? Do we want to? Would single linkage clustering work for longitudinal data?

# Collect means from kmlShape and place into data frame for plotting with ggplot2
trajMeans1600k4boys <- boy1600clusters['trajMeans']
trajMeans1600k4girls <- girl1600clusters['trajMeans']
trajMeans1600k4girls$Gender = 'Girls'
trajMeans1600k4boys$Gender = 'Boys'
trajMeans1600k4both <- bind_rows(trajMeans1600k4boys, trajMeans1600k4girls, .id="Gender")

# Changing labels on Facets on original data
# If you don't want to change the original data, then assign to another object.
trajMeans1600k4both <- trajMeans1600k4both %>%
  # Rename 1 to Boys and 2 to Girls
  mutate(Gender = recode(Gender, "1" = "Boys", "2"="Girls"), iCenters=as.factor(iCenters))

trajMeans1600k4both %>% ggplot( aes(x=times, y=traj, group=iCenters, color=Gender)) +
  geom_line(aes(linetype=iCenters), size=2, show.legend = FALSE) + 
  scale_color_manual(values=c("#CC6666", "#9999CC")) + facet_wrap(~Gender) +
  ggtitle("Clusters of Time Trajectories for 1600 meters") + theme_bw() +
  ylab("Time (in seconds)") +
  xlab("Year of High School")+theme(plot.title=element_text(size=15),axis.title=element_text(size=12))

##### Notes from
# https://stats.idre.ucla.edu/r/faq/how-can-i-visualize-longitudinal-data-in-ggplot2/
# https://stats.idre.ucla.edu/r/faq/how-can-i-make-individual-growth-curves-in-ggplot2/
# https://plotly.com/ggplot2/facet_wrap/
# https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
# https://rpubs.com/jaw588/lab14

### Example: tolerance data
## change id and male to factor variables
# tolerance <- within(tolerance, {
# id <- factor(id)
# male <- factor(male, levels = 0:1, labels = c("female", "male"))
# })

## view the first few rows of the dataset
# head(tolerance)
# Note: data is in long form

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

# More example code using 'stat_summary' and 'fun' instead of 'geom_line' and ;fun.y'
#ggplot(DF, aes(x = DOB, y = trip.duration.hr)) +
#  geom_jitter(alpha = 1/10) +
#  stat_summary(geom = "line", fun = "mean", color = "orange", size = 1) +
#  stat_summary(geom = "line", fun = "quantile", fun.args = list(probs = .9), linetype = 2, color = "red")

# The issue with the above code is that we have 712 observations

# Plan
# Read 3 statistical practice articles from TAS
# Purpose of this article
#1 Determine whether there is a true change in performance at grade 10
#2 Show use of algorithms for clustering longitudinal data
## 2a) Use kml, kmlShape, dtwclust, and pdc
## 2b) examine consensus
#3 Develop permutation test for change of slope (as partially discussed below)

# Examine change in slope at Grade 10
names(boy1600m) <- c("Name", "School", "Gender", "9","10", "11", "12", "PR", "PRGrade", "BestYear")
boy16long = boy1600m %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
boy16long$Grade <- as.numeric(boy16long$Grade)
names(girl1600m) <- c("Name", "School", "Gender", "9","10", "11", "12", "PR", "PRGrade", "BestYear")
girl16long = girl1600m %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
girl16long$Grade <- as.numeric(girl16long$Grade)
both16long <- bind_rows(girl16long,boy16long, .id="Gender")
# Make Gender a factor and add labels for graphic purposes
both16long$Gender <- factor(both16long$Gender, labels=c("Female", "Male"))
both16long$Grade <- factor(both16long$Grade)

# Set up plots
pdf(file="LinearSmooth1600m.pdf")
cp16 <- ggplot(data = both16long, aes(x = Grade, y= Time, group = Name)) + geom_line(col="gray") + facet_grid(.~Gender) + theme_bw()
cp16 + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x * I(x >= 11), se = FALSE, col="purple", linetype="dashed") + 
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x * I(x > 10), se = FALSE, col="mistyrose") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", shape = 17, size=3, color="maroon")
#  theme_bw() + facet_grid(.~Gender)
# stat_summary(aes(group = 1), fun = mean, geom = "point", shape = 19, size=3, color="blue") + 
dev.off()
#
## Regressions - would like to have gender as part of the model.
# If grade = 11 or 12 then Grade > 10 is TRUE(1) and the equation is
# Time = b0 + b1(Grade) + b2 + b3(Grade) = b0 + (b1 + b3)(Grade) + b2
# If grade = 9 or 10 then Grade > 10 is FALSE(0) and the equation is
# Time = b0 + b1(Grade) + b2(0) + b3(Grade*0)
# So b1 + b3 represents the difference between Time at Grade <= 10 and Grade > 10
# I am trying to show that there is no need for a change point 
# at Grade 10 - or the slope 
# after grade 10 is the same as the slope before grade 10
# or that the slope after grade 10 is steeper and more positive - in other 
# words, there is an increase in speed after grade 10.

model16.lm = lm(Time~Gender*I(Grade >= 10), data=both16long)
model16a.lm = lm(Time~Gender*I(Grade >= 9), data=both16long)
model16b.lm = lm(Time~Gender*I(Grade >= 11), data=both16long)
summary(model16.lm)
summary(model16a.lm)
summary(model16b.lm)

model16full.lm = lm(Time~Gender*Grade, data=both16long)
summary(model16full.lm)

## Not sure that this is necessary. Fits a regression equation to each row.
n <- nrow(both16long)
b0 <- numeric(n)
b1 <- numeric(n)
b2 <- numeric(n)
b3 <- numeric(n)
for (i in 1:n){
  dat <- girl1600m[i,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade >= 10), data=longdat)
  model2.lm = lm(Time~Grade*I(Grade >= 11), data=longdat)
  #  model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  gint[i] = model.lm$coefficients[1]
  gb1[i] = model.lm$coefficients[2]
  gb2[i] = model.lm$coefficients[3]
  gb3[i] = model.lm$coefficients[4]
  gint2[i] = model2.lm$coefficients[1]
  gb12[i] = model2.lm$coefficients[2]
  gb22[i] = model2.lm$coefficients[3]
  gb32[i] = model2.lm$coefficients[4]
}

# Set up plots
pdf(file="LinearSmooth1600m.pdf")
cp16 <- ggplot(data = both16long, aes(x = Grade, y= Time, group = Name)) + geom_line(col="gray") + facet_grid(.~Gender) + theme_bw()
cp16 + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x * I(x >= 11), se = FALSE, col="purple", linetype="dashed") + 
stat_smooth(aes(group = 1), method = "lm", formula = y ~ x * I(x > 10), se = FALSE, col="mistyrose") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", shape = 17, size=3, color="maroon")
#  theme_bw() + facet_grid(.~Gender)
# stat_summary(aes(group = 1), fun = mean, geom = "point", shape = 19, size=3, color="blue") + 

# Develop a way to test whether the slope after the grade changes from the slope before
# Try randomly selecting a row to get bootstrap distribution for each parameter.
############# For Girls ####################
kg = nrow(girl1600m)
B = 5000
gint.bs = numeric(B)
gb1.bs = numeric(B)
gb2.bs = numeric(B)
gb3.bs = numeric(B)
for (i in 1:B){
  set.seed(147835+i)
  idx <- sample(1:kg,kg,replace=TRUE)
  # print(idx)
  dat <- girl1600m[idx,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade>10), data=longdat)
  # model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  gint.bs[i] = model.lm$coefficients[1]
  gb1.bs[i] = model.lm$coefficients[2]
  gb2.bs[i] = model.lm$coefficients[3]
  gb2.bs[i] = model.lm$coefficients[4]
}
coef.gdf.bs <- data.frame(Intercept=gint.bs, Overall=gb1.bs, Indictor=gb2.bs, Interaction=gb3.bs)
head(coef.gdf.bs)

# Go through all rows and estimate the breakpoints. 
names(girl1600m) <- c("Name", "School", "Gender", "9","10", "11", "12", "PR", "PRGrade", "BestYear")
kg = nrow(girl1600m)
gint = numeric(kg)
gb1 = numeric(kg)
gb2 = numeric(kg)
gb3 = numeric(kg)
gint2 = numeric(kg)
gb12 = numeric(kg)
gb22 = numeric(kg)
gb32 = numeric(kg)
for (i in 1:kg){
  dat <- girl1600m[i,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade >= 10), data=longdat)
  model2.lm = lm(Time~Grade*I(Grade >= 11), data=longdat)
#  model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  gint[i] = model.lm$coefficients[1]
  gb1[i] = model.lm$coefficients[2]
  gb2[i] = model.lm$coefficients[3]
  gb3[i] = model.lm$coefficients[4]
  gint2[i] = model2.lm$coefficients[1]
  gb12[i] = model2.lm$coefficients[2]
  gb22[i] = model2.lm$coefficients[3]
  gb32[i] = model2.lm$coefficients[4]
}
coef.gdf.all <- data.frame(Intercept=gint, Overall=gb1, Indictor=gb2, Interaction=gb3)
coef.gdf2.all <- data.frame(Intercept=gint2, Overall=gb12, Indictor=gb22, Interaction=gb32)
head(coef.gdf.all)
head(coef.gdf2.all)



############# For Boys ####################
kb = nrow(boy1600m)
B = 5000
bint.bs = numeric(B)
bb1.bs = numeric(B)
bb2.bs = numeric(B)
bb3.bs = numeric(B)
for (i in 1:B){
  set.seed(147835+i)
  idx <- sample(1:kb,kb,replace=TRUE)
  # print(idx)
  dat <- boy1600m[idx,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade>10), data=longdat)
  # model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  bint.bs[i] = model.lm$coefficients[1]
  bb1.bs[i] = model.lm$coefficients[2]
  bb2.bs[i] = model.lm$coefficients[3]
  bb2.bs[i] = model.lm$coefficients[4]
}
coef.bbdf.bs <- data.frame(Intercept=bint.bs, Overall=bb1.bs, Indictor=bb2.bs, Interaction=bb3.bs)
head(coef.bbdf.bs)

# Go through all rows and estimate the breakpoints. 
