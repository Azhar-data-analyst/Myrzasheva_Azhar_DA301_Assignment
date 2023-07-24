# LSE Data Analytics Online Career Accelerator
# Myrzasheva_Azhar_DA301_Assignment
# DA301: Advanced Analytics for Organisational Impact
# July 2023

# Overview: 

# This file is intended to utilise R script to explore the available 
# data, create visualisations to identify trends and extract meaningful information 
# to inform decision making for our client, Turtle Games. The client is a game 
# manufacturer that produces and sells its own products as well as acts as a 
# retailer selling products manufactured by other companies. The products range 
# includes books, video games, board games, toys, etc.

# The business objective of Turtle games: 
#                       to improve overall sales performance.

# Questions that need to be answered for the client with R:
  
#                       1. The impact that each product has on sales
#                       2. How reliable the data is (e.g. normal distribution, 
#                       skewness, or kurtosis)
#                       3. What the relationship(s) is/are (if any) between North
#                       American, European, and global sales?

###############################################################################

## 1. The impact that each product has on sales

## Objective:

## To determine what insights can be gathered from scatterplots, histograms
## and boxplots about the turtle_sales data set. The EDA is to explore the data,
## visualise the data to get initial insights. 

###############################################################################

# 1. Load and explore the data
# Install and import Tidyverse, dplyr and ggplot2
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")

library(tidyverse)
library(dplyr)
library(ggplot2)

# Set the working directory
setwd("~/Desktop/LSE CAREER ACCELERATOR_DATA ANALYSIS/21_LSE_DA301_assignment_files")

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
View(sales)
summary(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_new <- select(sales, -c('Ranking', 'Year', 'Genre', 'Publisher'))

# View the data frame.
View(sales_new)

# View the descriptive statistics.
summary(sales_new)
as_tibble(sales_new)
str(sales_new)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(x = Platform, 
      y = NA_Sales, 
      data = sales_new, 
      color = Platform,
      main = "North American Sales across different products", 
      xlab = "Platform", 
      ylab = "NA Sales (pound, millions)", 
      geom =c('point', 'jitter'))

qplot(x = Platform, 
      y = EU_Sales, 
      data = sales_new, 
      color = Platform,
      main = "European Sales across different products", 
      xlab = "Platform", 
      ylab = "EU Sales (pound, millions)", 
      geom =c('point', 'jitter'))

qplot(x = Platform, 
      y = Global_Sales, 
      data = sales_new, 
      color = Platform,
      main = "Global Sales across different products", 
      xlab = "Platform", 
      ylab = "Global Sales (pound, millions)", 
      geom =c('point', 'jitter'))

## 2b) Histograms
# Create histograms.
# A histogram for NA_Sales
qplot(NA_Sales, data=sales_new, geom='histogram', fill = I("blue"), 
      colour = I('black'), 
      main = "Histogram of North American Sales", 
      xlab = "NA Sales (million pounds)", 
      ylab = "Frequency")

qplot(EU_Sales, data=sales_new, geom='histogram', fill = I("green"), 
      colour = I('black'), 
      main = "Histogram of European Sales", 
      xlab = "EU Sales (million pounds)",
      ylab = "Frequency")


qplot(Global_Sales, data=sales_new, geom='histogram', fill = I("purple"), 
      colour = I('black'), 
      main = "Histogram of Global Sales", 
      xlab = "Global Sales (million pounds)",
      ylab = "Frequency")

## 2c) Boxplots
# Create boxplots.
qplot(Platform, NA_Sales, data=sales_new, geom='boxplot', colour = I('blue'),
      main = 'NA Sales boxplot')

qplot(Platform, EU_Sales, data=sales_new, geom='boxplot', colour = I('green'),
      main = 'EU Sales boxplot')

qplot(Platform, Global_Sales, data=sales_new, geom='boxplot', colour = I('purple'),
      main = 'Global Sales boxplot')


###############################################################################

# 3. Observations and insights

## The initial 'sales' data set was transformed to only have 5 columns 
## ('Product', 'Platform', 'NA_Sales', 'EU_Sales' and 'Global_Sales'). A new data
## set, 'sales_new' was created to analyse what impact each product has on sales. 
## With the raw data, some plots were built to explore the data and note the 
## outliers, skewness and kurtosis of the data set. 

## It is noted here that most of the products are sold up to 5 million
## pounds in North American countries with some platforms having the
## highest sales, up to 10 million pounds. These are DS, Wii, PS2, X360 and GB. 
## There are, however, visible outliers, reaching up to 20 and 30 million pounds.

## European product sales usually show constant sales of products up to
## 2,5 mln pounds, with some platforms standing out and showing the highest
## sales (from 2,5 to 5 mln). These are: DS, 3DS, GB, PS, PS2, PS3, PS4, Wii
## and X360. There are visible outliers here as well. 

## In the Global sales, the following products bring the highest revenue (up
## to 20 million pounds): DS, GB, PS2, PS3, PS4, Wii and X360.

## Overall, the histograms built for every column, demonstrate right-skewed 
## distribution of data and some extreme outliers for all three regions (NA,
## EU and Global). 

## Although the scatterplots were showing quite high numbers, the boxplots 
## demonstrate that the mean of the figures are below 2.5 mln, 5 mln and less
## than 10 mln for EU, NA and Global respectively. 

###############################################################################
###############################################################################


## 2. How reliable the data is (e.g. normal distribution, skewness, or kurtosis) 

## Objective:

## Utilising R, to explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. 


################################################################################

# 1. Load and explore the data
# View data frame created in Week 4.
View(sales_new)

# Check output: Determine the min, max, and mean values.
summary(sales_new)

# View the descriptive statistics.
as_tibble(sales_new)
glimpse(sales_new)

# The Product column has data type of an integer, although the numbers here 
# only represent the code or ID of a product. This has to be changed. 

sales_new$Product <- as.factor(sales_new$Product)
glimpse(sales_new) # To view the descriptive statistics again and check the 
                   # 'Product' data type.

print(summary(sales_new))
DataExplorer::create_report(sales_new)

# to see how many times a certain product is present in the table
table(sales_new$Product)

# To count how many products IDs are in the data set
distinct_count <- sales_new %>%
  distinct(Product) %>%
  nrow()

print(distinct_count)

# There are 175 different products listed in this data set. We will now further
# explore what's the impact on sales per product ID.

###############################################################################

# 2. Determine the impact on sales per product_id.
## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sum_per_product <- sales_new %>%
  group_by(Product) %>%
  summarise(Total_EU_Sales = sum(EU_Sales),
            Total_NA_Sales = sum(NA_Sales),
            Total_Global_Sales = sum(Global_Sales))

print(sum_per_product, n=175)

# View the data frame.
View(sum_per_product)

# Explore the data frame.
View(sum_per_product)
summary(sum_per_product)

# The 'sum_per_product' column now has 175 observations with the second
# variable showing the sum per a certain product. 
# There are, however, outliers with a sum showing more than 30 mln.pounds. 
# Identify the products that have the sales of over 30 mln pounds. These might be
# outliers.

products_high_sales <- sum_per_product %>%
  filter(Total_Global_Sales < 30) %>%
  group_by(Product) %>%
  summarise(Total_EU_Sales = sum(Total_EU_Sales),
            Total_NA_Sales = sum(Total_NA_Sales),
            Total_Global_Sales = sum(Total_Global_Sales))

print(products_high_sales)

# Products 107, 123, 515 were identified as selling more than 30 mln and these
# numbers are skewing the data. We eliminated them. 

# Now our new data set, 'products_high_sales', contains only 172 observations with 
# the sales of these products not more than 30 mln pounds. 

# Identify the highest selling products (with global sales of more than 20 mln)

highest_selling <- filter(products_high_sales, Total_Global_Sales >= 20)
highest_selling

# View and see the descriptive statistics of the 'highest_selling' data set,
# Total_Global_Sales column, EU_Sales and NA_Sales
summary(highest_selling$Total_Global_Sales)
summary(highest_selling$Total_EU_Sales)
summary(highest_selling$Total_NA_Sales)


###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales_new$Global_Sales)
qqline(sales_new$Global_Sales)

qqnorm(sales_new$NA_Sales)
qqline(sales_new$NA_Sales)

qqnorm(sales_new$EU_Sales)
qqline(sales_new$EU_Sales)

# The Q-Q plot (or quantile-quantile plot) is a tool to see if the data set
# follows a certain theoretical distribution, which ideally should be
# normal distribution. Firstly, it is evident that in all three columns, 
# the distribution is quite normal, with dots forming a good diagonal line. 
# There is also a positive skew towards the end of the plot for all three
# data sets. 

## 3b) Perform Shapiro-Wilk test
# Install and import Moments which contains the shapito.test() function
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_new$NA_Sales)
shapiro.test(sales_new$EU_Sales)
shapiro.test(sales_new$Global_Sales)

# In all three cases the p-value is very low. The p-value is a measure of evidence
# against the null-hypothesis. The low p-value indicates strong evidence against
# null-hypothesis. We have sufficient evidence that the sample data is 
# influenced by some non-random cause. 

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_new$NA_Sales)
skewness(sales_new$EU_Sales)
skewness(sales_new$Global_Sales)

# In all three cases there is a positive skew with the skewness of more than 4.
# Skewness is a measure of the asymmetry of the probability distribution of a 
# real-valued random variable about its mean. A positive skewness value indicates
# that the distribution has a tail on the right side (right-skewed, which we 
# also observed with the qqline function).

## 3d) Determine correlation
# Determine correlation.
library(ggplot2)
ggplot(data = sales_new, aes(x = EU_Sales, y = Global_Sales, color = Product)) +
  geom_point(size = 2) +
  guides(color = 'none') +
  labs(title = "Correlation between EU and Global sales")

ggplot(data = sales_new, aes(x = NA_Sales, y = Global_Sales, color = Product)) +
  geom_point(size = 2) +
  guides(color = 'none') + 
  labs(title = "Correlation between NA and Global sales")

# A scatterplot here is a good way to visually determine the correlation
# between the global sales with respect to the EU sales and NA sales. 
# It is evident here that there is a very strong positive correlation between
# the EU and global sales as well as NA and global sales. It can also be visually 
# observed that the NA sales contribute to the global sales more than EU sales. 

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.

ggplot(data = highest_selling, aes(x = reorder(Product, -Total_EU_Sales), 
                                   # Reordered the products in descending order
                                   y = Total_EU_Sales, fill = Product)) + 
  geom_bar(stat = "identity") +
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (title = "Total EU sales by 16 highest selling products",
        x = "Product ID",
        y = "Total EU Sales")


ggplot(data = highest_selling, aes(x = reorder(Product, -Total_NA_Sales), 
                                   y = Total_NA_Sales, fill = Product)) + 
  # Use the stat 'identity' function for transformation
  geom_bar(stat = "identity") +
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (title = "Total NA sales by 16 highest selling products",
        x = "Product ID",
        y = "Total NA Sales")

ggplot(data = highest_selling, aes(x = reorder(Product, -Total_Global_Sales), 
                                   y = Total_Global_Sales, fill = Product)) + 
  geom_bar(stat = "identity") +
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs (title = "Total Global sales by 16 highest selling products",
        x = "Product ID",
        y = "Total Global Sales")

###############################################################################
###############################################################################

# 5. Observations and insights

# First, the initial data, 'sales', was viewed and data types were checked. 
# 'Product' column data type was numerical and I changed it to a factor as the
# numbers are only showing the product ID. The descriptive statistics was viewed
# and the Data Explorer report was downloaded with no null values found in all
# the columns. 

# The sales for the distinct product IDs was calculated and there are 175
# products listed in the data frame. The outliers, the products with more than
# 30 mln pounds revenue, were removed. I filtered the 16 most highly sold products
# with more than 20 mln pounds revenue. Three top-selling products were identified
# in EU sales, NA and Global. These are 195, 876 and 979 for EU, 326, 254,
# 948 for North America and 254, 195 and 231 globally.

# In all three cases the p-value is very low. The low p-value indicates strong 
# evidence against null-hypothesis. We have sufficient evidence that the sample 
# data is influenced by some non-random cause. The data set is positively skewed
# in all three columns - EU sales, NA sales and global sales. 

###############################################################################
###############################################################################

## 3. What the relationship(s) is/are (if any) between North American, European
## and global sales

## Objective:

## To better understand if there is any relationship between North America,
## Europe, and global sales. To investigate any possible relationship(s) in
## the sales data by creating a simple and multiple linear regression model.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
View(sales_new)

# Determine a summary of the data frame.
summary(sales_new)
as_tibble(sales_new)

# The data set has been cleaned previously and we can start building the 
# simple linear regression model now.

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data and check the 
# relationship between the NA sales and Global sales.
cor(sales_new$NA_Sales, sales_new$Global_Sales)
cor(sales_new$EU_Sales, sales_new$Global_Sales)

# The correlation coefficient for NA Sales is equal to 0.93, which suggests that
# there is a high positive relationship between the North American and Global 
# sales. 
# The correlation coefficient between EU and global sales is 0.88.

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales_new$NA_Sales, sales_new$Global_Sales)
plot(sales_new$EU_Sales, sales_new$Global_Sales)

# There are still some outliers present, and we will eliminate those rows
# that show very high figures. These are rows with product IDs 107 and 123
# (that were identified previously)

sales_new_clean <- sales_new[-c(1, 2), ]
head(sales_new_clean)

# We will build the scatterplot again to see the relationship between the 
# NA and Global sales without the outliers.
plot(sales_new_clean$NA_Sales, sales_new_clean$Global_Sales)
plot(sales_new_clean$EU_Sales, sales_new_clean$Global_Sales) 

# The outliers could have affected the correlation coefficient, so I will
# check the correlation again. 
cor(sales_new_clean$NA_Sales, sales_new_clean$Global_Sales)
cor(sales_new_clean$EU_Sales, sales_new_clean$Global_Sales)

# As expected, the correlation has now been reduced to 0.91 between the NA
# and Global sales, and 0.85 between the EU and global sales, which are still
# very high indicators. 

###############################################################################

# Select only numeric columns from the original data frame.
model1 <- lm(Global_Sales~NA_Sales,
             data=sales_new_clean)
model2 <- lm(Global_Sales~EU_Sales,
             data=sales_new_clean)

# View the model
model1
model2

# So the Global sales would increase by 1.690 with every unit of NA Sales 
# increase. And the global sales will increase by 2.62 with every unit of
# EU sales increase. 

# What would we see if we get the summary of model1? 

summary(model1)
summary(model2)

# From the regression table, we can say that the NA Sales are very important
# for the global sales and have a high impact on them. The standard error is
# quite low so the independent variable (NA_Sales) has a high statistical
# significance (t-value = 40.63). R-squared is 83% which means that
# NA sales alone explain 83% of the variability in the global sales. 

# As for the EU sales, the R squared value is 0.72 and the t-value shows 29.702
# (lower statistical value compared to NA sales).

# Now we will look at the residuals. 

plot(model1$residuals)
plot(model2$residuals)

# In both plots, the residuals are very close to zero, and the plot gives an 
# almost horizontal line here. 

# Add a line of best fit to the existing plot
plot(sales_new_clean$NA_Sales, sales_new_clean$Global_Sales)
abline(coefficients(model1))

# Add a line of best fit with the coefficients of model1. 
plot(sales_new_clean$EU_Sales, sales_new_clean$Global_Sales)
abline(coefficients(model2))

# There is quite a strong linear relationship between NA Sales and Global sales,
# and EU sales and Global sales. 
# However, as we move on, with higher sales, we can see that the dots on the
# plot stray away from the line of best fit. 

# We will not do a log transformation as the relationships between the NA_Sales
# and Global sales, as well as EU and Global sales are already quite high. I 
# will build a prediction model now.

sales_new_clean_forecast <- data.frame(NA_Sales = 19.08)
predict(model1, sales_new_clean_forecast)

sales_new_clean_forecast <- data.frame(EU_Sales = 1.56)
predict(model2, sales_new_clean_forecast)

# If the NA Sales hit the revenue of 34.02, the predicted Global sales value
# would be 58.58 mln. If the EU sales are 23.80, then Global - 63.37 mln.
# If the NA Sales are 3.93, then the global sales forecast is 7.72 mln. 
# If the EU sales are 1.56, then the global sales would be predicted to be 5 mln.
# If the NA Sales are 19.08, then the global sales could be 33.33 mln pounds. 

###############################################################################

# 3. Create multiple linear regression model 
# Multiple linear regression model.

# View the data
summary(sales_new_clean)

# Determine the correlation between the variables
# Create a new data frame with numeric variables only

sales_mr <- select(sales_new_clean, c(NA_Sales, EU_Sales, Global_Sales))
sales_mr

# Install the psych package.
install.packages('psych')

# Import the psych package. Used for multivariate analysis
library(psych)

# Use the corPlot() function.
# Specify the data frame (sales_mr) 
corPlot(sales_mr, cex=2)

# From the plot it is visible that there is the strongest correlation
# between the NA sales and global sales with an r value of 0.91 and 
# 0.85 correlation between EU and Global sales. 

# Create a new object and 
# specify the lm function and the variables.
model_mr = lm(Global_Sales~EU_Sales+NA_Sales, data=sales_mr)

# Print the summary statistics.
summary(model_mr)

# With the adjusted R-squared here that's equal to 0.9546, we can confidently 
# say that the model is quite strong and we can now move on to predict global 
# sales based on the given NA and EU sales. 

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Build a data frame with provided values.
new_values <- data.frame(
  NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# View the data frame
new_values

# Create a new object and specify the predict function.
test_prediction = predict(model_mr, newdata=new_values,
                      interval='confidence')

# Print the object.
test_prediction

# Overall, we can see the predicted values for the new data set. 
# The predicted values take into account the confidence interval and have
# the fit values, lower range and an upper range of values. So, if the NA
# sales show the revenue of 34.02 mln pounds and EU sales are 23.80 mln,
# then the ideal Global sales revenue could be 73.35 mln pounds. However,
# the value could be anywhere between 71.74 mln and 74.97 mln pounds. 

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# There is a very high correlation between North American sales and Global sales.
# North American sales account for 91% of the global sales while EU sales are 
# usually, historically lower, but still account for 85% of the changes
# in the global sales. 

###############################################################################
###############################################################################








