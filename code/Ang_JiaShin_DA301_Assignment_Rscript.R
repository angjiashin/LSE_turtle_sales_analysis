## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

getwd()
setwd(dir='/Users/shin/Desktop/lse_daca/course_3/LSE_DA301_assignment_files') 

# Install and import Tidyverse.
# install.packages('tidyverse')
library(tidyverse)
library(moments)
library(dplyr)
library(psych)

# Import the data set.
turtle_sales <- read.csv(file='/Users/shin/Desktop/lse_daca/course_3/LSE_DA301_assignment_files/sales_cleaned.csv', header=T)

# Print the data frame.
head(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 

turtle_sales_cleaned <- turtle_sales[,c('Product','Platform','NA_Sales',
                                        'EU_Sales','Global_Sales')]

# View the data frame.
as_tibble(turtle_sales_cleaned)
View(turtle_sales_cleaned)

# View the descriptive statistics.
summary(turtle_sales_cleaned)
# The turtle_sales_cleaned contains 352 observations and 5 variables.

# Saving the new data frame into a csv file.
write.csv(turtle_sales_cleaned,
          "/Users/shin/Desktop/lse_daca/course_3/LSE_DA301_assignment_files/turtle_sales_cleaned.csv")
 
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# Platform vs Global_Sales
ggplot(data=turtle_sales_cleaned, aes(x=Platform, Global_Sales)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, linewidth=1.5)


# Product vs Global_Sales
ggplot(data=turtle_sales_cleaned,aes(x=Product, Global_Sales)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, linewidth=1.5)



## 2b) Histograms
# Create histograms.
# Global sales (Bin size= 10)
ggplot(turtle_sales_cleaned, aes(x=Global_Sales)) + 
    geom_histogram(binwidth=10,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    ggtitle("Count of Global Sales (10 Millions pounds)") +
    theme(plot.title = element_text(size=10))

# North America Sales (Bin size= 10)
ggplot(turtle_sales_cleaned, aes(x=NA_Sales)) + 
  geom_histogram(binwidth=10,fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  ggtitle("Count of North America Sales (10 Millions pounds)") +
  theme(plot.title = element_text(size=10))

# Europe Sales (Bin size= 10)
ggplot(turtle_sales_cleaned, aes(x=EU_Sales)) + 
  geom_histogram(binwidth=10,fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  ggtitle("Count of Europe Sales (10 Millions pounds)") +
  theme(plot.title = element_text(size=10))

## 2c) Boxplots
# Create boxplots.

# Product Sales
ggplot(turtle_sales_cleaned, aes(x=as.factor(Product), y=Global_Sales)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Product")
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

# Based on the Histogram on sales, Majority of the global sales 
# were less than 10 million pounds. Breaking down the
# sales further into different regions, North America tend to sell more of the
# products as compared to Europe given that there are more sales numbering over 
# 10 million pounds.

# One particularly interesting trend we can see from the scatter and boxplot
# of the product sales is that, the number of sales decreases as new product
# are released across the years. (Starting from #1) It is likely that 
# the customers are playing a lot lesser platform games throughout the years.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
turtle_sales <- read.csv(file='/Users/shin/Desktop/lse_daca/course_3/LSE_DA301_assignment_files/turtle_sales_cleaned.csv', header=T)

# Check output: Determine the min, max, and mean values.

turtle_sales_1 <- sapply(turtle_sales, mean)
turtle_sales_1

turtle_sales_1 <- sapply(turtle_sales, min)
turtle_sales_1

turtle_sales_1 <- sapply(turtle_sales, max)
turtle_sales_1

# checking the data frame
view(turtle_sales)
head(turtle_sales)
# Checking for NA values
sum(is.na(turtle_sales))

# Removing redundant column X.
turtle_sales = subset(turtle_sales, select = -c(X) )

# Checking dimensions of the data frame.
dim(turtle_sales)

# View the descriptive statistics.
summary(turtle_sales)

###############################################################################

# 2. Determine the impact on sales per product_id.

# Import dplyr library.
# library(dplyr)

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sum_per_product = turtle_sales %>% group_by(Product) %>% summarise(EU_Sales=sum(EU_Sales),
                                                                   NA_Sales=sum(NA_Sales),
                                                                   Global_Sales=sum(Global_Sales))
# View the data frame.
view(sum_per_product)

# Explore the data frame.

# Determine the sum of missing values 
sum(is.na(sum_per_product))

# descriptive statistics
summary(sum_per_product)

# Create a data profile report.
DataExplorer::create_report(sum_per_product)

# export data using csv format.

write.csv(sum_per_product,
          "/Users/shin/Desktop/lse_daca/course_3/LSE_DA301_assignment_files/sum_per_product.csv")

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

# Global Sales per product
ggplot(data=sum_per_product,aes(x=Product, Global_Sales)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, linewidth=1.5)

# Europe Sales per product
ggplot(data=sum_per_product,aes(x=Product, EU_Sales)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, linewidth=1.5)

# North America Sales per product
ggplot(data=sum_per_product,aes(x=Product, NA_Sales)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, linewidth=1.5)

# Create histograms

# Global sales (Bin size= 10)
ggplot(sum_per_product, aes(x=Global_Sales)) + 
  geom_histogram(binwidth=10,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Count of Global Sales (10 Millions pounds)") +
  theme(plot.title = element_text(size=10))

# North America Sales (Bin size= 10)
ggplot(sum_per_product, aes(x=NA_Sales)) + 
  geom_histogram(binwidth=10,fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  ggtitle("Count of North America Sales (10 Millions pounds)") +
  theme(plot.title = element_text(size=10))

# Europe Sales (Bin size= 10)
ggplot(sum_per_product, aes(x=EU_Sales)) + 
  geom_histogram(binwidth=10,fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  ggtitle("Count of Europe Sales (10 Millions pounds)") +
  theme(plot.title = element_text(size=10))


# Create boxplots.


ggplot(sum_per_product, aes(x=as.factor(Product), y=Global_Sales)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Product")

ggplot(sum_per_product, aes(x=as.factor(Product), y=NA_Sales)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Product")

ggplot(sum_per_product, aes(x=as.factor(Product), y=EU_Sales)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Product")

# Boxplot per column
boxplot(sum_per_product$Product)
boxplot(sum_per_product$Global_Sales)
boxplot(sum_per_product$NA_Sales)
boxplot(sum_per_product$EU_Sales)
###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Global 
qqnorm(sum_per_product$Global_Sales,
       col='blue', 
       xlab='z Value',
       ylab='Global Sales', 
       pch = 1,
       frame = FALSE)

qqline(sum_per_product$Global_Sales,
       col = "red",
       lwd = 2)

# North America
qqnorm(sum_per_product$NA_Sales,
       col='blue',
       xlab='z Value',
       ylab='North America Sales',
       pch = 1,
       frame = FALSE)

qqline(sum_per_product$NA_Sales,
       col = "red",
       lwd = 2)

# Europe
qqnorm(sum_per_product$EU_Sales,
       col='blue',
       xlab='z Value',
       ylab='Europe Sales',
       pch = 1,
       frame = FALSE)

qqline(sum_per_product$EU_Sales,
       col = "red",
       lwd = 2)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments')
library (moments)
# Perform Shapiro-Wilk test.

shapiro.test(sum_per_product$Global_Sales)
shapiro.test(sum_per_product$NA_Sales)
shapiro.test(sum_per_product$EU_Sales)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(sum_per_product$Global_Sales) 
kurtosis(sum_per_product$Global_Sales)

skewness(sum_per_product$NA_Sales) 
kurtosis(sum_per_product$NA_Sales)

skewness(sum_per_product$EU_Sales) 
kurtosis(sum_per_product$EU_Sales)

# The four -values are all greater than 0.05, 
# indicating the data is normally distributed.

## 3d) Determine correlation
# Determine correlation.
cor(sum_per_product$Product, sum_per_product$Global_Sales) 
cor(sum_per_product$Product, sum_per_product$NA_Sales)
cor(sum_per_product$Product, sum_per_product$EU_Sales) 

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

#The Q-Q plot indicates a positive correlation. 
# However, for an alpha level of 0.05, 
# all the p values tested were less than 0.05 and 
# rejects the null hypothesis that the data are 
# from a normally distributed population. 
# This is supported by the fact that the skewness of all
# the sum of sales (global,NA,EU) were not close to zero.
# The kurtosis values (>3) also supported the fact that
# the data does not has a peak.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
sum_per_product <- read.csv(file='/Users/shin/Desktop/lse_daca/course_3/LSE_DA301_assignment_files/sum_per_product.csv',
                           header=T)

# Removing redundant column X.
sum_per_product = subset(sum_per_product, select = -c(X) )

# Determine a summary of the data frame.
summary(sum_per_product)
dim(sum_per_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

plot(sum_per_product$Product, sum_per_product$Global_Sales)

model1 <- lm(Global_Sales~Product,
             data=sum_per_product)

# Print the summary statistics.
summary(model1)

# p-value: < 2.2e-16; Adjusted R-squared:  0.3637
# F-statistic: 100.5 on 1 and 173 DF

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# View residuals on a plot.
plot(model1$residuals)


# Plot the relationship with base R graphics.
plot(sum_per_product$Product, sum_per_product$Global_Sales)
coefficients(model1)


# Add line-of-best-fit.
abline(coefficients(model1))

# Noted that the linear regression model made a very poor fit 
# as the relationship between Product and Global Sales
# is not a linear one. To try a log transformation on Global Sales
# on a 2nd linear model.

# Complete a log transformation with dplyr's mutate() function.
sum_per_product <- mutate(sum_per_product, 
              logGlobal_Sales=log(Global_Sales))

# View new object with new variable.
head(sum_per_product)


# Create a new model using logIndex.
model2 <- lm(logGlobal_Sales~Product,
             data=sum_per_product)


# View full regression table.
summary(model2)

# Adjusted R-squared: 0.5459 , p-value: < 2.2e-16
# F-statistic: 210.2 on 1 and 173 DF

# View residuals on a plot.
plot(model2$residuals)

# Plot the relationship between year and logIndex.
plot(sum_per_product$Product, sum_per_product$logGlobal_Sales)

# Add a line-of-best fit to existing plot.
abline(coefficients(model2))

# Model 2 seems to fit better than model 1 with a higher R-squared value and 
# F-statistic value.

###############################################################################

# 3. Create a multiple linear regression model

# visualise the correlation between all the variables with the psych package.
# install.packages('psych')

corPlot(sum_per_product, cex=1)

# Select only numeric columns from the original data frame.
modela = lm(Global_Sales~EU_Sales+NA_Sales, data=sum_per_product)


# Print the summary statistics.
summary(modela)



###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)

df_test <- data.frame(NA_Sales, EU_Sales)
df_test

predictTest = predict(modela, newdata=df_test,
                      interval='confidence')
predictTest

# Observed values

turtle_sales[turtle_sales$NA_Sales==34.02 &
                  turtle_sales$EU_Sales==23.80, ]
# Predicted value = 68.056548 actual value = 67.85
# Predicted value was within confidence interval.

turtle_sales[turtle_sales$NA_Sales==3.93 &
                  turtle_sales$EU_Sales==1.56, ]

# Predicted value = 7.356754 actual value = 6.04
# Predicted value was not within confidence interval.

turtle_sales[turtle_sales$NA_Sales==2.73 &
                  turtle_sales$EU_Sales==0.65, ]

# Predicted value = 4.908353 actual value = 4.32
# Predicted value was not within confidence interval.

turtle_sales[turtle_sales$NA_Sales==2.26 &
                  turtle_sales$EU_Sales==0.97, ]
# Predicted value = 4.761039 actual value = 3.53
# Predicted value was not within confidence interval.

turtle_sales[turtle_sales$NA_Sales==22.08 &
                  turtle_sales$EU_Sales==0.52, ]

# Predicted value = 26.625558 actual value = 23.21.
# Predicted value was not within confidence interval.
###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Statistics for MLR:-
# Adjusted R-squared:  0.9664 
# F-statistic:  2504 on 2 and 172 DF,  p-value: < 2.2e-16
# MLR is a stronger model than the LR model earlier 
# with a higher R-squared value.
# However, when it came to predictions,
# only 1 predicted value was within the 95% confidence interval
# of the observed value.
# A 20% chance of accurately predicting the Global Sales.
###############################################################################
###############################################################################

