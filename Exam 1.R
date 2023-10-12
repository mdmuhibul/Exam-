#Md Muhibul islam


#Question 04 (a)
#Choose a subgroup of the sample to consider and provide summary statistics of that subgroup. Explain why this subgroup is interesting.
attach(acs2021)
summary(acs2021)
summary(acs2021$DEGFIELDD)
obj1 <- (acs2021$DEGFIELDD=="General Education")|(acs2021$DEGFIELDD=="Chemical Engineering")| (acs2021$DEGFIELDD=="Biochemical Sciences")     
summary(obj1)
#This subgroup is interesting to me because from entire dataset I got 2994 people who had studied general education, chemical engineering and biochemical sciences respectively.

#Question 04 (b)

#Here, I am going to choose variable named as EEDUC, EEDUCD, BPL & DEGFIELD2 to run the t-test. 
# Here i combined the categories as indicated in the categories below in the code: 

GroupD <- (acs2021$EDUC=="Grade 12")|(acs2021$EDUCD=="Regular high school diploma")|(acs2021$BPL=="New York"|
  (acs2021$ DEGFIELD2=="Social Sciences")
## Then i subsetted that new categories (GroupD) fron the total dataset to perfrom the t-test.
GroupD1 <-subset(acs2021, GroupD)
summary(GroupD)
t.test(GroupD)

#One Sample t-test

#data:  GroupD
#t = 775.61, df = 200066, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  0.7485323 0.7523249
#sample estimates:
#  mean of x 
#0.7504286 

GroupE <- (acs2021$EDUC=="5+ years of college")|(acs2021$EDUCD=="Bachelor's degree")|(acs2021$BPL=="China")|
  (acs2021$ DEGFIELD2=="Accounting")
GroupE1 <-subset(acs2021, GroupE)

t.test(GroupE)
#One Sample t-test
#data:  GroupE
#t = 310, df = 200066, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  0.3224348 0.3265378
#sample estimates:
#  mean of x 
#0.3244863

#After combining two groups here i am going to run two sample test. 
t.test(GroupD, GroupE)

#Welch Two Sample t-test
#data:  GroupD and GroupE
#t = 298.83, df = 397681, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.4231486 0.4287360
#sample estimates:
#  mean of x mean of y 
#0.7504286 0.3244863 

##Here, a two sample t-test was run to compare two groups named as GroupD and GroupE with respect to variable named as EDUC, EDUCD; BPL & DEIGFIELD2.
#Here, mean of GroupD and GroupE is 0.7504286 and 0.3244863 respectively.The null hypothesis H0 indicates that there is no significant
#difference in means between two group as there is 95% confidence interval Whereas alternative hypothesis indicated that two groups differ
#significantly. As p value is less than 2.2e-16 providing strong evidence against null hypothesis which suggests that means of
#two groups are indeed significantly different from each other.
# Here, I got t value is 298.83; Confidence interval is 95%. And p value is less than 0.05.


Question 04(c)

#I am going to compare K-NN with simple OLS regression. 
attach(acs2021)
summary(acs2021)
library(tidyverse)
library(caret)
library(class)

summary(acs2021$DEGFIELD)
educational_attainment <- subset(acs2021, (acs2021$DEGFIELD!= "NA") )
summary(educational_attainment)


Class1 <- fct_recode(educational_attainment$DEGFIELD, '5' = 'Business',
                    '4'='Education Administration and Teaching', '3'='Social Sciences',
                    '2'='Medical and Health Sciences and Services', '1'='Fine Arts',
                    '3'='(Other)')
summary(Class1)
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
  
data_use_prelim <- data.frame(norm_varb(as.numeric(educational_attainment$DEGFIELD)),norm_varb(as.numeric(norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}

data_use_prelim <- data.frame(norm_varb(as.numeric(educational_attainment$DEGFIELD)),norm_varb(as.numeric(educational_attainment$BPLD)))

good_obs_data_use <- complete.cases(data_use_prelim,educational_attainment$DEGFIELD)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(DEGFIELD,good_obs_data_use)

#divide the data into gtwo groups. 

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
summary(train_data)

for (indx in seq(1, 9, by= 2)) {
  pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_y == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

Question 4 (d) 
#Regression with dummy variables:

model_ols1 <- lm(cl_data_n ~ train_data$EEDUC + train_data$EEDUCD + train_data$DEGFIELD)
summary(model_ols1)
y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])

cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$EEDUC + train_data$EEDUCD + train_data$DEGFIELD)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])                    



#Question 01:

library(tidyverse)
load("C:/Fall 2023/Econometrics Exam/acs2021_ny_data.RData")
attach(acs2021)
require(haven)
summary(acs2021)

table(Educ_BAplus,wfh)

#            wfh     some wfh        no
#BAplus      6838   7218            15973
#no         2640   2230             20336

GroupA <- c(7218,6838)
GroupA1 <- subset(GroupA)
GroupB <- c(2230, 2640)
GroupB1 <- subset(GroupB)
summary(GroupA)

t.test(GroupA)
t.test(GroupB)


#Question 2:

library(tidyverse)
load("C:/Fall 2023/Econometrics Exam/acs2021_ny_data.RData")
attach(acs2021)
require(haven)
summary(acs2021)

xtabs(~ kids_vax1[(REGION == "Northeast")] + Educ_BAplus[(REGION == "Northeast")]

xtabs(~ kids_vax1[(REGION == "Midwest")] + Educ_BAplus[(REGION == "Midwest")]
xtabs(~ kids_vax1[(REGION == "South")] + Educ_BAplus[(REGION == "South")], data = Household_Pulse_data)
xtabs(~ kids_vax1[(REGION == "West")] + Educ_BAplus[REGION == "West")]              
              