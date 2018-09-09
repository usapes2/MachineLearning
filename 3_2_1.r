## R commands for STAT 494 (39241, 39243)
## Special Topics in Statistics, Probability and Operations Research:
## Statistical Techniques for Machine Learning and Data Mining
## Fall Semester 2016
## Version: 2016.08.22
## Reference: http://homepages.math.uic.edu/~jyang06/stat494/stat494.html


## R package: ElemStatLearn, version 2015.6.26
## Data Sets, Functions and Examples from the Book (ESL): 
## "The Elements of Statistical Learning, Data Mining, Inference, and Prediction" 
## by Trevor Hastie, Robert Tibshirani and Jerome Friedman, Second Edition, 2009

## At Cran: https://cran.r-project.org/web/packages/ElemStatLearn/index.html
## Reference: Section 3.2.1 in the ESL book

# load the package  "ElemStatLearn" 
library("ElemStatLearn")

# load the data set "prostate"
data(prostate)
str( prostate )
# 'data.frame':	97 obs. of  10 variables:
# $ lcavol : num  -0.58 -0.994 -0.511 -1.204 0.751 ...
# $ lweight: num  2.77 3.32 2.69 3.28 3.43 ...
# $ age    : int  50 58 74 58 62 50 64 58 47 63 ...
# $ lbph   : num  -1.39 -1.39 -1.39 -1.39 -1.39 ...
# $ svi    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ lcp    : num  -1.39 -1.39 -1.39 -1.39 -1.39 ...
# $ gleason: int  6 6 7 6 6 6 6 6 6 6 ...
# $ pgg45  : int  0 0 20 0 0 0 0 0 0 0 ...
# $ lpsa   : num  -0.431 -0.163 -0.163 -0.163 0.372 ...
# $ train  : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
table(prostate$train)
# FALSE  TRUE 
# 30    67

# partition the original data into training and testing datasets
train <- subset( prostate, train==TRUE )[,1:9] # w.o the last variable
test  <- subset( prostate, train==FALSE)[,1:9] # w.o the last variable
# Note: There is a typo in "prostate" help file in "ElemStatLearn" package
# In "test  <- subset( prostate, train=FALSE )[,1:9]", "=" should be "=="

# check correlations
cor( prostate[,1:8] )
# reproduce Table 3.1 on page 50
round(cor( train[,1:8] ),3)

# scatter plot, reproducing Figure 1.1 on page 3
pairs( prostate[,1:9], col="violet" )

# fit linear model on training dataset using LS method
trainst <- train
for(i in 1:8) {
  trainst[,i] <- trainst[,i] - mean(prostate[,i]);
  trainst[,i] <- trainst[,i]/sd(prostate[,i]);
}
fitls <- lm( lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=trainst )
# reproduce Table 3.2 on page 50, as well as some numbers in Table 3.3 on page 63
summary(fitls)
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  2.46493    0.08931  27.598  < 2e-16 ***
#   lcavol       0.67953    0.12663   5.366 1.47e-06 ***
#   lweight      0.26305    0.09563   2.751  0.00792 ** 
#   age         -0.14146    0.10134  -1.396  0.16806    
#   lbph         0.21015    0.10222   2.056  0.04431 *  
#   svi          0.30520    0.12360   2.469  0.01651 *  
#   lcp         -0.28849    0.15453  -1.867  0.06697 .  
#   gleason     -0.02131    0.14525  -0.147  0.88389    
#   pgg45        0.26696    0.15361   1.738  0.08755 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7123 on 58 degrees of freedom
# Multiple R-squared:  0.6944,	Adjusted R-squared:  0.6522 
# F-statistic: 16.47 on 8 and 58 DF,  p-value: 2.042e-12

## Conclusion: age, lcp, gleason, pgg45 are not significant at 5% significance level based on Z-test
## Question: Could we remove all the four predictors?
## Answer: Use partial F-test
## [1] Fit the full model and get RSSfull=0.7123^2*58=29.43
## [2] Fit the reduced model:
fitlsr <- lm( lpsa ~ lcavol+lweight+lbph+svi, data=trainst )
summary(fitlsr)
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  2.47142    0.08901  27.766  < 2e-16 ***
#   lcavol       0.59582    0.10910   5.461 8.85e-07 ***
#   lweight      0.23084    0.09456   2.441   0.0175 *  
#   lbph         0.20313    0.10215   1.988   0.0512 .  
#   svi          0.27814    0.11311   2.459   0.0167 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7275 on 62 degrees of freedom
# Multiple R-squared:  0.6592,	Adjusted R-squared:  0.6372 
# F-statistic: 29.98 on 4 and 62 DF,  p-value: 6.911e-14
##  then RSSreduced=0.7275^2*62=32.81
## [3] Test statistic: F=((32.81-29.43)/(62-58))/(29.43/58)=1.67
## [4] p-value:
1-pf(1.67, df1=4, df2=58)
## 0.1692794

## check testing errors
testst <- test
for(i in 1:8) {
  testst[,i] <- testst[,i] - mean(prosqtate[,i]);
  testst[,i] <- testst[,i]/sd(prostate[,i]);
}

# mean prediction error on testing data using mean training value
mean(trainst[,9])                             # 2.452345
# mean (absolute) prediction error
mean(abs(testst[,9]-mean(trainst[,9])))       # 0.7409334
# mean (squared) prediction error
mean((testst[,9]-mean(trainst[,9]))^2)        # 1.056733
# standard error of mean (squared) prediction error
sd((testst[,9]-mean(trainst[,9]))^2)/sqrt(30) # 0.396115

# mean prediction error based on full model, reproducing some numbers in Table 3.3 on page 63
test.fitls=predict(fitls, newdata=testst)  
# mean (absolute) prediction error
mean(abs(test[,9]-test.fitls))                # 0.5233719
# mean (squared) prediction error
mean((test[,9]-test.fitls)^2)                 # 0.521274
# standard error of mean (squared) prediction error
sd((test[,9]-test.fitls)^2)/sqrt(30)          #  0.178724


# mean prediction error based on reduced model
test.fitlsr=predict(fitlsr, newdata=testst)  
# mean (absolute) prediction error
mean(abs(test[,9]-test.fitlsr))                # 0.5139785
# mean (squared) prediction error
mean((test[,9]-test.fitlsr)^2)                 # 0.4563321
# standard error of mean (squared) prediction error
sd((test[,9]-test.fitlsr)^2)/sqrt(30)          # 0.1242902


#0.7409334  # 0.5233719 # 0.5139785