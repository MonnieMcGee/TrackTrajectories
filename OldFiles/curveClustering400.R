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
boy400m = read.csv("./boy400m_cc.csv",stringsAsFactors = TRUE)
girl400m = read.csv("./girl400m_cc.csv",stringsAsFactors = TRUE)
boy400m <- boy400m[,-1]
girl400m <- girl400m[,-1]

# Using package kml
library(kml)
library(kmlShape)
# First, create a clusterLongData object for use in function kml.
# Assumes boy400m and girl400m have been created using June2021Analysis400m.R
boy400temp <- boy400m %>% ungroup() %>% select("X9","X10","X11","X12")
kb <- nrow(boy400temp)
boy400temp <- as.data.frame(boy400temp)
boy400cld <- clusterLongData(traj=boy400temp,idAll=paste("I-",1:kb,sep=""),time=9:12,varNames="Grade")

## Same drill for girls
girl400temp <- girl400m %>% ungroup() %>% select("X9","X10","X11","X12")
kg <- nrow(girl400temp)
girl400temp <- as.data.frame(girl400temp)
girl400cld <- clusterLongData(traj=girl400temp,idAll=paste("I-",1:kg,sep=""),time=9:12,varNames="Grade")

# Clustering using kmlShape
girl400clds <- cldsWide(data.frame(1:kg,girl400temp),times=9:12)
par(ask=FALSE)
kmlShape(girl400clds,2)
kmlShape(girl400clds,3)
kmlShape(girl400clds,4)
girl400clusters <- kmlShape(girl400clds,5)
kmlShape(girl400clds,6) # Did not converge after max iterations

boy400clds <- cldsWide(data.frame(1:kb,boy400temp),times=9:12)
par(ask=FALSE)
kmlShape(boy400clds,2)
kmlShape(boy400clds,3)
kmlShape(boy400clds,4)
boy400clusters <- kmlShape(boy400clds,5)
kmlShape(boy400clds,6) # Did not converge after max iterations

# Figure out how to export results to a data frame to use with 
# ggplot2 data. Or how to modify graphics from kmlShape. I need a 
# plot with just the cluster shapes on it. 

# Problem: If variances of clusters are different, then k-means clustering will not work.
# How can we fix this? Do we want to? Would single linkage clustering work for longitudinal data?
# Collect means from kmlShape and place into data frame for plotting with ggplot2
trajMeans400k5boys <- boy400clusters['trajMeans']
trajMeans400k5girls <- girl400clusters['trajMeans']
trajMeans400k5girls$Gender = 'Girls'
trajMeans400k5boys$Gender = 'Boys'
trajMeans400k5both <- bind_rows(trajMeans400k5boys, trajMeans400k5girls, .id="Gender")

trajMeans400k5both <- trajMeans400k5both %>%
  # Rename 1 to Boys and 2 to Girls
  mutate(Gender = recode(Gender, "1" = "Boys", "2"="Girls"), iCenters=as.factor(iCenters))

trajMeans400k5both %>% ggplot( aes(x=times, y=traj, group=iCenters, color=Gender)) +
  geom_line(aes(linetype=iCenters), size=2, show.legend = FALSE) + 
  scale_color_manual(values=c("#CC6666", "#9999CC")) + facet_wrap(~Gender) +
  ggtitle("Clusters of Time Trajectories for 400 meters") + theme_bw() +
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
names(boy400m) <- c("Name", "School", "Gender", "9","10", "11", "12", "PR", "PRGrade", "BestYear")
boy4long = boy400m %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
boy4long$Grade <- as.numeric(boy4long$Grade)
names(girl400m) <- c("Name", "School", "Gender", "9","10", "11", "12", "PR", "PRGrade", "BestYear")
girl4long = girl400m %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
girl4long$Grade <- as.numeric(girl4long$Grade)
both4long <- bind_rows(girl4long,boy4long, .id="Gender")
# Make Gender a factor and add labels for graphic purposes
both4long$Gender <- factor(both4long$Gender, labels=c("Female", "Male"))
both4long$Grade <- factor(both4long$Grade)

# Set up plots
pdf(file="LinearSmooth400m.pdf")
cp4 <- ggplot(data = both4long, aes(x = Grade, y= Time, group = Name)) + geom_line(col="gray") + facet_grid(.~Gender) + theme_bw()
cp4 + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x * I(x >= 11), se = FALSE, col="purple", linetype="dashed") + 
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x * I(x > 10), se = FALSE, col="mistyrose") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", shape = 17, size=3, color="maroon")
#  theme_bw() + facet_grid(.~Gender)
# stat_summary(aes(group = 1), fun = mean, geom = "point", shape = 19, size=3, color="blue") + 
dev.off()

# Examining slopes of lines to determine relationships
model4.lm = lm(Time~Gender*I(Grade >= 11), data=both4long)
model4a.lm = lm(Time~Gender*I(Grade >= 9), data=both4long)
model4b.lm = lm(Time~Gender*I(Grade >= 11), data=both4long)
summary(model4.lm)
summary(model4a.lm)
summary(model4b.lm)

model4full.lm = lm(Time~Gender*Grade, data=both4long)
summary(model4full.lm)

# Develop a way to test whether the slope after the grade changes from the slope before
# Try randomly selecting a row to get bootstrap distribution for each parameter.
############# For Girls ####################
kg = nrow(girl400m)
B = 100
gint.bs = numeric(B)
gb1.bs = numeric(B)
gb2.bs = numeric(B)
gb3.bs = numeric(B)
for (i in 1:B){
  set.seed(147835+i)
  idx <- sample(1:kg,kg,replace=TRUE)
  # print(idx)
  dat <- girl400m[idx,]
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
kg = nrow(girl400m)
gint = numeric(kg)
gb1 = numeric(kg)
gb2 = numeric(kg)
gb3 = numeric(kg)
for (i in 1:kg){
  dat <- girl400m[i,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade > 10), data=longdat)
  #  model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  gint[i] = model.lm$coefficients[1]
  gb1[i] = model.lm$coefficients[2]
  gb2[i] = model.lm$coefficients[3]
  gb3[i] = model.lm$coefficients[4]
}
coef.gdf.all <- data.frame(Intercept=int, Overall=b1, Indictor=b2, Interaction=b3)
head(coef.gdf.all)

############# For Boys ####################
kb = nrow(boy400m)
B = 100
bint.bs = numeric(B)
bb1.bs = numeric(B)
bb2.bs = numeric(B)
bb3.bs = numeric(B)
for (i in 1:B){
  set.seed(147835+i)
  idx <- sample(1:kb,kb,replace=TRUE)
  # print(idx)
  dat <- boy400m[idx,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade>10), data=longdat)
  # model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  bint.bs[i] = model.lm$coefficients[1]
  bb1.bs[i] = model.lm$coefficients[2]
  bb2.bs[i] = model.lm$coefficients[3]
  bb2.bs[i] = model.lm$coefficients[4]
}
coef.bdf.bs <- data.frame(Intercept=bint.bs, Overall=bb1.bs, Indictor=bb2.bs, Interaction=bb3.bs)
head(coef.bdf.bs)

# Go through all rows and estimate the breakpoints. 
kb = nrow(boy400m)
bint = numeric(kg)
bb1 = numeric(kg)
bb2 = numeric(kg)
bb3 = numeric(kg)
for (i in 1:kb){
  dat <- boy400m[i,]
  longdat <- dat %>% pivot_longer(!c(Name, School, Gender, PR, PRGrade, BestYear), names_to = "Grade", values_to = "Time")
  longdat$Grade <- as.numeric(longdat$Grade)
  model.lm = lm(Time~Grade*I(Grade > 10), data=longdat)
  #  model.seg = suppressWarnings(segmented(model.lm,npsi=1))
  bint[i] = model.lm$coefficients[1]
  bb1[i] = model.lm$coefficients[2]
  bb2[i] = model.lm$coefficients[3]
  bb3[i] = model.lm$coefficients[4]
}
coef.bdf.all <- data.frame(Intercept=bint, Overall=bb1, Indictor=bb2, Interaction=bb3)
head(coef.bdf.all)
