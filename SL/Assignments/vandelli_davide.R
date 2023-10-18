# Vandelli Davide 

# Problem 4 
 

#1 
library(emdbook)
data <- emdbook::FirDBHFec_sum
# 22 NAs will be replaced with their mean. 
sum(is.na(data))
sum(is.na(data$DBH)) #they're all here

data$DBH[is.na(data$DBH)]<-mean(data$DBH,na.rm=TRUE)

sum(is.na(data$DBH))

#2

hist(data$DBH)

#3 
# pdf of a gamma is: 
# x^(alpha-1)*e^(-x/alpha) / sigma^(alpha)*Gamma(alpha)

library('bbmle')
DBH <- data$DBH


n=length(DBH)
negative_loglik <- function(a,s){ -sum(dgamma(DBH, shape=a, scale=s, log = T)) }
MLE <- mle2(negative_loglik, start=list(a=1,s=1))
summary(MLE)

#from the summary:
alpha <- 11.0189
sigma <- 0.7507
x=c(seq(0,333))
y=dgamma(x, shape=alpha, scale=sigma)

hist(data, probability = T)
lines(x,y)

#6 
plot(data$fecundity, DBH)

# Problem 5

#1 
students <- na.omit(read.csv("student_data.csv"))
sum(is.na(students))

#2 Mean, quartiles, std
summary(students)
sd(students$study_hours)
sd(students$exam_scores)

#3 Linear Model
mod <- lm(students$exam_scores ~., data=students)
summary(mod)
# This model has overall a very high R-squared, indicating that 
# there is an acceptable linear explanation for exam scores. 
# More in detail, residuals seem to be symmetric, and the t-value for study_hours
# coefficient is indicating a statistically significant effect. 

#4 Residuals
plot(mod)

# Residuals vs fitted seems to have overall linearity, but homoscedasticity 
# is to check for (the variance seems not constant). Furthermore, the residuals
# are scattered around apparently randomly, so assumption of linearity should be 
# met. 

# Normal Q-Q
# Observations follow the diagonal line, without straying too farther, meaning that
# normality is met for residuals, overall. 

# Scale-Location: as seen before, the variance is not constant. 

#5
y <- students$exam_scores
x <- students$study_hours

plot(x, y, main = "Study and scores")
abline(lm(y ~ x, data = mtcars), col = "blue")

# The model seems to be very accurate, almost every observations is touched
# by the line. 
       