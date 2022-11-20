# Assignment: Housing Assignment
# Name: Swartzwelter, Myranda
# Date: 2021-02-13


#Set Working Directory
setwd("/Users/myrandaswartzwelter/Documents/Bellvue/Term-2/DSC-520/DSC520-Stats-for-DS/Assignments")

#Read in CSV
housing_df <- read.csv("assignment_week8_9/week-6-housing.csv")

#Import useful libraries

install.packages('coefplot')

library('dplyr')
library('purrr')
library('stringr')
library('broom')
library('car')
library('ggplot2')
library('coefplot')

#Transforamtions / modifications:

#create a new column that calculates price per sqft
housing_df$price_per_sqft <-  housing_df$sale_price / housing_df$sq_ft_lot

#create new column that calculates total bathrooms
housing_df$total_bathrooms <- housing_df$bath_3qtr_count + housing_df$bath_full_count + housing_df$bath_half_count

ggplot(housing_df, aes(x=sq_ft_lot)) + geom_histogram()
ggplot(housing_df, aes(x=sale_price)) + geom_histogram()

sum(housing_df$sq_ft_lot >=1000000)
#8 properties over 1000000 sq ft

sum(housing_df$sq_ft_lot >=500000)
#27 over 500000

#There are 27 properties with more than 500000 sq ft lots that skew our graphs, lets remove them
housing_df <- housing_df[housing_df$sq_ft_lot < 500000, ]

ggplot(housing_df, aes(x=sq_ft_lot, y = sale_price)) + geom_point()
ggplot(housing_df, aes(x=log(sq_ft_lot), y = sale_price)) + geom_point()
ggplot(housing_df, aes(x=sq_ft_lot, y = log(sale_price))) + geom_point()
ggplot(housing_df, aes(x=log(sq_ft_lot), y = log(sale_price))) + geom_point()

#The plots indicate a log(sq_ft_lot) might be helpful for modeling but not certain

#Create variable that contains sale pricw and sq foot lot
sdate_sq_ft <- select(housing_df, sale_price, sq_ft_lot)

simple_lm <-  lm(sale_price ~ sq_ft_lot , data=housing_df)

#Create variable that contains sale price and additional parameters
#Selecting number of bedrooms and bathrooms, year built and year renovated and sale date as those 
#are the parameters that I believe will have a relationship with price and are different across the different sales
many_params <- select(housing_df, sale_price, sq_ft_lot, bedrooms, bath_full_count, year_built, year_renovated)

multi_lm <- lm(sale_price ~  sq_ft_lot + bedrooms + bath_full_count + year_built + year_renovated, data=housing_df)

#Summary functions
summary(simple_lm)
#R2 Value: 0.01435
#adjusted R2 value: 0.001428

summary(multi_lm)
#R2 value: 0.1461
#Adjusted R2: 0.1458

#The R2 value is higher for the model with more parameters, indicating a better fit for the model
#Including the additional parameters did help the model fit

#Get the standardized betas for the simple model
simple_lm$coef

#get the standardized betas for the more complex model
multi_lm$coef

#Lets visualize them quick
coefplot(simple_lm)
coefplot(multi_lm)

#A standardized beta coefficient  compares the strength of the effect of each individualind variable to the 
#dependent variable. The higher the coefficient, the stronger the effect.

#So, in my model I would expect the number of bedrooms and bathrooms to be the 
#strongest indicator for sale price.

#Calculate confidence intervals

confint(simple_lm, "sq_ft_lot")

confint(multi_lm, "sq_ft_lot")
confint(multi_lm, "bedrooms")
confint(multi_lm, "bath_full_count")
confint(multi_lm, "year_built")
confint(multi_lm,"year_renovated")

#compare models using ANOVA() 
anova(simple_lm, multi_lm)

#Since we see a very small P Value of <2.2e-16 we can conclude that the more complex model has a better fit

# Assessing Outliers
#Look at leverage plots and outlier tests for each model

#create dataframes to store data for each model
housing_df_simple <- housing_df
housing_df_multi <- housing_df

#get casewise statistics for simple model
housing_df_simple$standardized_residuals <- rstandard(simple_lm)
housing_df_simple$studentized_residuals <- rstudent(simple_lm)
housing_df_simple$cooks_distance <- cooks.distance(simple_lm)
housing_df_simple$dfbeta <- dfbeta(simple_lm)
housing_df_simple$dffit <- dffits(simple_lm)
housing_df_simple$leverage <- hatvalues(simple_lm)
housing_df_simple$covariance_ratios <- covratio(simple_lm)

#get casewise statistics for complex model
housing_df_multi$standardized_residuals <- rstandard(multi_lm)
housing_df_multi$studentized_residuals <- rstudent(multi_lm)
housing_df_multi$cooks_distance <- cooks.distance(multi_lm)
housing_df_multi$dfbeta <- dfbeta(multi_lm)
housing_df_multi$dffit <- dffits(multi_lm)
housing_df_multi$leverage <- hatvalues(multi_lm)
housing_df_multi$covariance_ratios <- covratio(multi_lm)

#Calculate the standardized residuals using the appropriate command, 
#specifying those that are +-2, storing the results of 
#large residuals in a variable you create.

#simple regression
housing_df_simple$large_residuals <- housing_df_simple$standardized_residuals > 2 | housing_df_simple$standardized_residuals <  -2

#complex regression
housing_df_multi$large_residuals <- housing_df_multi$standardized_residuals > 2 | housing_df_multi$standardized_residuals <  -2


#Use the appropriate function to show the sum of large residuals.
sum(housing_df_simple$large_residuals)
#There are 334 large residuals in the simple model

sum(housing_df_multi$large_residuals)
#There are 351 large residuals in the complex model

#Which specific variables have large residuals (only cases that evaluate as TRUE)?
#Variables with large residuals for simple model:
housing_df_simple[housing_df_simple$large_residuals,]

#Variables with large residuals for multi model:
housing_df_multi[housing_df_multi$large_residuals,]

#Investigate further by calculating the leverage, cooks distance, and covariance rations. 
#Comment on all cases that are problematics.

#simple model
housing_df_simple[housing_df_simple$large_residuals,c("cooks_distance","leverage","covariance_ratios")]

#multi paratmeter model
housing_df_multi[housing_df_multi$large_residuals,c("cooks_distance","leverage","covariance_ratios")]

#for either model, we would be concerned with a cooks distance greater than 1
#For both models, there are no variables with cooks distance greater than 1
housing_df_simple[housing_df_simple$large_residuals & housing_df_simple$cooks_distance > 1 ,c("cooks_distance","leverage","covariance_ratios")]
housing_df_multi[housing_df_multi$large_residuals & housing_df_multi$cooks_distance > 1 ,c("cooks_distance","leverage","covariance_ratios")]

#Leverage
#look for values 2 or 3 times the average leverage
#formula (k + 1)/n

#simple k = 1 so  (2/12856) = 0.000155 
#so for the simple model we are looking for leverages greater than 0.0004667
simple_leverage = 3*(2/12856)
housing_df_simple$large_leverage <- housing_df_simple$large_residuals & housing_df_simple$leverage > simple_leverage
sum(housing_df_simple$large_leverage)
#there are 51 variables in the simple model with a larger leverage than the average leverage

#multi k = 6 so  (6/12856) =  0.0004667
#so for the simple model we are looking for leverages greater than 0.0014
multi_leverage = 3*(6/12856)
housing_df_multi$large_leverage <- housing_df_multi$large_residuals & housing_df_multi$leverage > multi_leverage
sum(housing_df_multi$large_leverage)
#there are 65 variables in the multi parameter model with a leverage larger than the average leverage

#Covariance ratio
#use criteria CVR > 1 + [3(k+1)/n] and CVR < 1- [3(k+1)/n]

#simple
simple_cvr_comp <- 3*(1+1)/12856
simple_cvr_high <- 1 + simple_cvr_comp
simple_cvr_low <- 1 - simple_cvr_comp

housing_df_simple$deviate_cvr <- housing_df_simple$large_residuals & (housing_df_simple$covariance_ratios > simple_cvr_high | housing_df_simple$covariance_ratios < simple_cvr_low)
sum(housing_df_simple$deviate_cvr)
#There are 308 variables that deviate substantially from the boundaries of our covariance ratios

#multi
multi_cvr_comp <- 3*(5+1)/12856
multi_cvr_high <- 1 + multi_cvr_comp
multi_cvr_low  <- 1 - multi_cvr_comp

housing_df_multi$deviate_cvr <- housing_df_multi$large_residuals & (housing_df_multi$covariance_ratios > multi_cvr_high | housing_df_multi$covariance_ratios < multi_cvr_low)
sum(housing_df_multi$deviate_cvr)
#There are 283 variables that deviate substantially from the boundaries of our covariance ratios

###Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.

#simple 
dwt(simple_lm)

#The D-W statistic is 0.738 since this is less than 1,it does not meet the assumption of independence

dwt(multi_lm)

#The D-W statistic is 0.712 since this is less than 1, it does not meet the assumption of independence


###Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.

#simple
simple_vif <- vif(simple_lm)
#this contains fewer than 2 terms so cannot be calculated

#multi
vif(multi_lm)
multi_vif <- vif(multi_lm)
1/multi_vif
mean(vif(multi_lm))

#The largest VIF is not above 10
#The average VIF is not substantially greater than 1
#The tolerance for all parameters is great than 0.2 
#We can conclude there is no collinearity within our data


#Visually check the assumptions related to the residuals using the plot() and hist() functions.
plot(simple_lm)
plot(housing_df_simple$studentized_residuals)
hist(housing_df_simple$studentized_residuals)
#The residuals look fairly random and they follow a roughly normal distribution
#The first plot shows that residuals decrease in magnitude as the values increase
#The normal Q-Q plot does not look linear at high values
#The scale location plot is fitted
#The residuals and leverage plot shows what we saw in our above calculations

plot(multi_lm)
plot(housing_df_multi$studentized_residuals)
hist(housing_df_multi$studentized_residuals)
#The residuals look fairly random and they follow a roughly normal distribution
#the fitted residuals plot looks a bit clustered
#The normal Q-Q plot does not look linear at high values
#The scale location plot is clustered
#The residuals and leverage plot shows what we saw in our above calculations


#Overall, is this regression model unbiased? If an unbiased regression model, what does this tell us about the sample vs. the entire population model?
#There are some outliers and influential cases, but if were to remove these, I believe the model will be overall unbiased and can thus be used as a model for the entire population
