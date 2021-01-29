setwd("~/git/major-sleep")
dat <- read.csv("sleep.csv")
head(dat)

#Libraries
library(car)
library(gplots)
library(ggplot2)
library(psych)
library(arm)


##################################
#####     DATA CLEANING      #####
##################################


# Setting gender data to categorical
dat$gender <- as.factor(dat$gender)
levels(dat$gender) <- c('male', 'female')
levels(dat$gender)

# Setting major data to categorical, and changing from 4 levels to 2 levels
dat$major <- as.factor(dat$major)
levels(dat$major) <- c('humanities', 'business', 'stem', 'socscience')
levels(dat$major) <- list(tech=c('stem', 'business'), nontech=c('humanities', 'socscience'))
levels(dat$major)
plot(dat$major, main='Distribution of Major', xlab = 'Major', ylab='count') #checking

##################################
##### DESCRIPTIVE STATISTICS #####
##################################


# sample size
nrow(dat)

############# AGE ##############

# average age
mean(dat$age)

# age standard deviation
sd(dat$age)

# range age
range(dat$age)


############# GENDER ##############

# gender distribution

sum(dat$gender == 'male') # num males
sum(dat$gender == 'female') # num females

sum(dat$gender == 'male')/nrow(dat) # % males
sum(dat$gender == 'female')/nrow(dat) # % females



############# SLEEP QUALITY ##############

# creating the sleep quality scale
sleep <- with(dat, cbind(fall.asleep, awake, problem, quality, energy,
                         productivity, troubled, problem.time))
sleep <- 5 - sleep # converting to 4-0 Likert scale as in the original study
dat$SLEEP <- rowMeans(sleep)

mean(dat$SLEEP) # mean
sd(dat$SLEEP) # SD
range(dat$SLEEP) # range
alpha(sleep) # alpha reliability
describe(dat$SLEEP) 
summary(dat$SLEEP)

############# COLLEGE MAJOR ##############
summary(dat$major) # frequencies


############# SLEEP DURATION ##############

mean(dat$duration) # mean
sd(dat$duration) # sd
range(dat$duration) # range



##################################
#####  DATA VISUALIZATIONS   #####
##################################

#AGE
ggplot(dat, aes(x=age, fill = ..x..)) + geom_bar() + 
  labs(title='Distribution of Age', x='Age (years)', y='Count') +
  scale_fill_gradient(low='royalblue1', high = 'deepskyblue')

#GENDER
ggplot(dat, aes(x=gender, fill=gender)) + geom_bar()+
  labs(title='Distribution of Gender', x='Gender', y='Count')


#AGE AND GENDER
ggplot(dat, aes(x=age, fill = gender)) + geom_bar() + 
  labs(title='Distribution of Gender by Age', x='Age (years)', y='Count') +
  scale_fill_manual(values = c("dodgerblue1", "salmon1"), name="Gender")


############# SLEEP QUALITY #################
ggplot(dat, aes(x=SLEEP, fill = ..x..)) + geom_histogram(binwidth=0.5) +
  labs(title='Histogram of Sleep Quality Score', x='Sleep Quality Score', y='Count')

############## COLLEGE MAJOR ##################
ggplot(dat, aes(x=major, fill=major)) + geom_bar() + 
  labs(title='Distribution of Major', x='Major', y='Count') +
  scale_fill_manual(values = c("dodgerblue", "dodgerblue4" ))

########### SLEEP DURATION ###################
ggplot(dat, aes(x=duration, fill = ..x..)) + geom_histogram(binwidth=0.5) +
  labs(title='Histogram of Sleep Duration', x='Average Sleep Duration', y='Count')




################################################
########  LINEAR MODELS   ######################
################################################

#bivariate model to test main effect hypothesis
###### DV ~ IV1 #######
mod1 <- lm(scale(SLEEP) ~ major, data=dat)
ggplot(dat, aes(x=major, y=scale(SLEEP), fill=major)) +
  geom_boxplot() +
  guides(fill=FALSE)  +
  labs(title='Sleep Quality Score by Major', x='Major', y='Sleep Quality Score (Z-scored)') +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14))
mean(dat[dat$major=='tech',]$SLEEP) # mean for tech majors is slightly lower
mean(dat[dat$major=='nontech',]$SLEEP)

summary(mod1)

#confidence intervals
confint(mod1, 'majornontech', level=0.95)

# power
t.majornontech.mod1 <- 0.127 

cutoff1 <- qt(0.025, df = 65)

pt(cutoff1-t.majornontech.mod1, df=65, lower.tail=TRUE)



####### DV ~ IV2 ###########
mod2 <- lm(scale(SLEEP) ~ scale(duration), data=dat)
ggplot(dat, aes(x=scale(duration), y=scale(SLEEP))) +
  geom_point() + 
  geom_smooth(method='lm') +
  labs(title='Sleep Duration vs Quality', x='Duration (Z-scored)', y='Sleep Quality Score (Z-scored') +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14))


summary(mod2)
sqrt(summary(mod2)$r.squared) # r

confint(mod2, 'scale(duration)', level=0.95)


####### DV ~ IV1 + IV2 ########

mod3 <- lm(scale(SLEEP) ~ major + scale(duration), data=dat)
ggplot(dat, aes(x=scale(duration), y=scale(SLEEP), fill=major)) +
  geom_point() + 
  geom_smooth(method='lm') +
  labs(title='Sleep Quality and Duration across Majors', x='Duration (Z-scored)', y='Sleep Quality Score (Z-scored)') +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14))

summary(mod3)

#confidence intervals
confint(mod3, 'majornontech', level=0.95)
confint(mod3, 'scale(duration)', level=0.95)

# power
t.majornontech.mod3 <- -0.049
t.duration.mod3 <- 2.797

cutoff3 <- qt(0.025, df = 64)

pt(cutoff3-t.majornontech.mod3, df=64, lower.tail=TRUE)
pt(cutoff3-t.duration.mod3, df=64, lower.tail=TRUE)

######### Model 4: DV ~ IV1 * IV2
mod4 <- lm(scale(SLEEP) ~ major * scale(duration), data=dat)
ggplot(dat, aes(x=scale(duration), y=scale(SLEEP), fill=major)) +
  geom_point() + 
  geom_smooth(method='lm') +
  labs(title='Sleep Quality and Duration across Majors', x='Duration (Z-scored)', y='Sleep Quality Score (Z-scored)') +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14))

summary(mod4)

confint(mod4, 'majornontech', level=0.95)
confint(mod4, 'scale(duration)', level=0.95)
confint(mod4, 'majornontech:scale(duration)', level=0.95)


