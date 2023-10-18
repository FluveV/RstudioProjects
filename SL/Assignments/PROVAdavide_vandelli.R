# Vandelli Davide

# To obtain an audience estimate of a game special shirts 
# were distributed to 300 supporters on condition that they were used 
# during the game. During the game 250 fans were randomly selected and 
# 12 of them had the shirt.

# Use hypergeometric(N, K, n) distribution, where: 

#   P(X = k) = (K over k)(N-K over n-k) / (N over n)
# N is unknown. 
K <- 300
n <- 250 
k <- 12  # fans wearing shirts out of n

# Log-likelihood is: 
#     l(N) = log(N-K)!(N-n)!/((N-(K+n-k))!N!)
#  which is: 
#     l(N) = log(N-300)!(N-250)!/((N-538))!N!)
#  and by using possible simplifications:
#     l(N) = Σ(238, j=1)log(N-538+j) - Σ(250, j=1)log(N-250+j)

hypergeom.log_like = function(N){
  num=0
  for(i in 1:238){
    num=num+log(N-538+i)
  }
  den=0
  for(i in 1:250){
    den=den+log(N-250+i)
  }
  return(num-den)
}

x = seq(3000, 17000)
y = hypergeom.log_like(x)
plot(x,y, type = 'l')

# The peak is a little over 6000. 

neg.hypergeom.log_like = function(N){
  -hypergeom.log_like(N)
}

optim(6000,neg.hypergeom.log_like, method='Brent', lower = 3000, upper = 10000)
# $par should be the MLE which is 6249.502

# _______________________________________________________________

#Problem 5
# A population of women who were at least 21 years old, of 
# Pima Indian heritage and living near Phoenix, Arizona, was 
# tested for diabetes according to World Health Organization 
# criteria

#1 
library(MASS)
data <- Pima.tr2
bmi <- data$bmi
bp <- data$bp
type <- data$type

View(data)
sum(is.na(data)) # there are 114 NAs on this dataset. 

#2
sum(is.na(data$bp)) 


data$bp[is.na(data$bp)]<-mean(data$bp,na.rm=TRUE)
data$type[is.na(data$type)]<-mode(data$type,na.rm=TRUE)

#3
summary(data$bmi)

summary(data$glu)

summary(data$bp)

#4
mod <- lm(bp ~ bmi, data)
summary(mod)

#There is an association between blood pressure and BMI with coefficient 0.475;
#Under H0 for blood pressure's contribution to the model, 
# I reject the null hypothesis with more than 99% confidence. Intercept stands at 
# 56.98. 
# Some coefficients may change due to missing values in BMI index data. 
# R squared states that 0.07309 proportion of variance in the dependent variable
# that can be explained by the independent variable 

#5

library(ggplot2)

ggplot(data, aes(x = bmi, y = bp)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") 
# The data is not particularly well described by the line from the linear model. 
# It seems to be very scattered around values of blood pressure from 60 to 90. 

#6
finalmod <- lm(bp ~., data = na.omit(data))
summary(finalmod)
#This model is likely to be more accurate as now bmi has a lower coefficient, 
# which may mean that with the simpler model a lot of the contribution was due 
# to the unseen effect of other variables too. Diabetes pedigree function (ped)
# is the strongest predictor, with -2.12 coefficient, and is followed by 
# type "YES" for diabetes according to WHO criteria. 
# Adjusted R squared is now higher than the R-squared from the simple model, meaning 
# it is a better fit. 

# Unfortunately, only age is the variable that cannot be rejected with any 
# standard confidence level. The only other variable with p-value < 0.05 is 
# plasma glucose concentration. 

#____________________________________________________________________________

# Vandelli Davide
# assume that the data are i.i.d. gamma distributed. 
# Compute the MLE in α and σ using
# an appropriate R optimization function.

# pdf of a gamma is: 
# x^(alpha-1)*e^(-x/alpha) / sigma^(alpha)*Gamma(alpha)

#1
df = read.table("https://hastie.su.domains/CASI_files/DATA/diabetes.csv", 
                sep = ',', header = TRUE)
prog <- df$prog

#2
n=length(prog)
neg.log_likelihood <- function(a,s){
  -sum(dgamma(prog, shape=a, scale=s, log = T))
}
library("bbmle")
res_mle <- mle2(neg.log_likelihood, start=list(a=1,s=1))
summary(res_mle)

#3 95% confidence intervals for variables
confint(res_mle)

#4 Plot a histogram of the data and on top of it plot the estimated density
a = 3.643885  # data from the summary
s = 41.751024
x=seq(0,350)
y=dgamma(x, shape=a, scale=s)
hist(prog, probability = T)
lines(x,y)

# another problem: 

#1 
data <- read.table("https://www.dropbox.com/s/cl3710l22jywgxm/df_brain.dat?dl=1", 
                   header=TRUE)
View(data)
gender <- factor(data$Gender)
age <- factor(data$Age)
Head_size <- data$Head_size
Brain_weight <- data$Brain_weight

#2 Mean, quartiles, standard deviation for numerical variables
summary(Brain_weight)
summary(Head_size)

#3 boxplot 
boxplot(Head_size ~ gender, data = data,
        xlab="Gender", ylab="Head size", 
        names = c("Male", "Female")) 

#4 Linear model
mod <- lm(Brain_weight ~ Head_size, data = data)
summary(mod)

library(ggplot2)

ggplot(data, aes(x = Brain_weight, y = Head_size)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") 

# 5
plot(mod)

# comments: residuals vs fitted check for linearity, homoscedasticity (constant variance), and outliers. If the residuals are randomly scattered around zero with no apparent pattern, it suggests that the linear model assumptions are met.
# Q-Q plot assesses the assumption of normality of residuals. If the points on the plot closely follow the diagonal line, it indicates that the residuals are approximately normally distributed.
# scale-location plot is used to check the assumption of constant variance (homoscedasticity). Ideally, the spread of residuals should be consistent across all levels of fitted values.

# t-test
t_test_result <- t.test(Head_size ~ gender, data)
t_test_result


# The analysis was conducted to compare the head size between genders. The t-test yielded a t-value of 9.3144, calculated based on a sample size of 229.16 degrees of freedom. The resulting p-value is remarkably small, less than 2.2e-16, which indicates strong evidence against the null hypothesis.
# The alternative hypothesis is that the true difference in means between group 1 and group 2 is not equal to 0, which aligns with the expectation that the head size differs between genders.
# The 95 percent confidence interval for the mean difference in head size lies between 298.0219 and 457.9374. The sample estimates indicate that the mean head size in group 1 (presumably one gender) is approximately 3798.261, while in group 2 (the other gender) it is around 3420.282.
# Overall, the significant p-value and the calculated confidence interval suggest that there is a substantial difference in mean head size between the two gender groups, supporting the notion that gender plays a role in determining head size.

