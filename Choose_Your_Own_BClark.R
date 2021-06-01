# Choose Your Own Project
# Airline Passenger Satisfaction Project
# Brian Clark
# 6/1/2021

# 2. Methods and Analysis

# The comments below reflect the steps taken in performing my analysis. The numbering scheme 
# follows that of the final report; which is why we start at number 2 here. For a discussion of 
# the rationale and results pertaining to these steps, please see the Rmd and pdf files.

## 2.1 Data Initialization and Cleaning

### Load Libraries and Specify Rounding

#I begin my project by loading the following packages:

# tidyverse - makes it easy to install and load core packages in a single command
# caret - contains functions to streamline the model training process
# matrixStats - high-performing functions operating on rows and columns of matrices

# I include "!require(package)" coding because the r code is shared for grading purposes.  When 
# loading a program that someone else is going to run, it's best to make sure the packages are 
# installed.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(matrixStats)

# I report all numeric results to four significant digits as a personal preference.

options(digits = 4)

### Read Data

#### Airline Passenger Satisfaction  

# Dataset for this analysis is found on Kaggle at 

# https://www.kaggle.com/teejmahal20/airline-passenger-satisfaction/download

# Acknowledgement:

# TJ Klein
# Servo Firmware Engineer at Seagate Technology
# Minneapolis, Minnesota, United States

# The actual dataset is divided into two datasets named test.csv and train.csv. The combined 
# dataset is extremely large and cumbersome for machine learning. So, for this assignment, I use 
# just the smaller test.csv dataset and create my own test and train datasets from test.csv.

# I have downloaded the test.csv file to my github repository and made the file public. Here is 
# how I read the file.

rddata <- "https://raw.githubusercontent.com/btclark34/Create_Your_Own_BClark/main/test.csv"
rd <- read.csv(rddata, header = TRUE)

# This dataset contains an airline passenger satisfaction survey.

# Data Columns:

# Column 01: X: Column Number
# Column 02: id: Survey ID
# Column 03: Gender: Gender of the passengers (Female, Male)
# Column 04: Customer Type: The customer type (Loyal customer, disloyal customer)
# Column 05: Type of Travel: Purpose of the flight (Personal Travel, Business Travel)
# Column 06: Age: The actual age of the passengers
# Column 07: Class: Travel class (Business, Eco, Eco Plus)
# Column 08: Flight distance: The flight distance of this journey
# Column 09: Inflight wifi service: Satisfaction level of wifi service (0:Not Applicable;1-5)
# Column 10: Departure/Arrival time convenient: Satisfaction level (0:Not Applicable;1-5)
# Column 11: Ease of Online booking: Satisfaction level of online booking (0:Not Applicable;1-5)
# Column 12: Gate location: Satisfaction level of Gate location (0:Not Applicable;1-5)
# Column 13: Food and drink: Satisfaction level of Food and drink (0:Not Applicable;1-5)
# Column 14: Online boarding: Satisfaction level of online boarding (0:Not Applicable;1-5)
# Column 15: Seat comfort: Satisfaction level of Seat comfort (0:Not Applicable;1-5)
# Column 16: Inflight entertainment: Satisfaction level of entertainment (0:Not Applicable;1-5)
# Column 17: On-board service: Satisfaction level of On-board service (0:Not Applicable;1-5)
# Column 18: Leg room service: Satisfaction level of Leg room service (0:Not Applicable;1-5)
# Column 19: Baggage handling: Satisfaction level of baggage handling (0:Not Applicable;1-5)
# Column 20: Check-in service: Satisfaction level of Check-in service (0:Not Applicable;1-5)
# Column 21: Inflight service: Satisfaction level of inflight service (0:Not Applicable;1-5)
# Column 22: Cleanliness: Satisfaction level of Cleanliness (0:Not Applicable;1-5)
# Column 23: Departure Delay in Minutes: Minutes delayed when departing
# Column 24: Arrival Delay in Minutes: Minutes delayed when arriving
# Column 25: Satisfaction: Airline satisfaction level (Satisfaction, neutral or dissatisfaction)

# I would like to know, up front and before I start manipulating the data, what proportion of the 
# airline customers in the original dataset are satisfied customers.

mean(rd$satisfaction == "satisfied")

### Data Cleaning

# Now, let's get an idea of what this dataset we just read looks like.

# Here are the dimensions of the dataset.

dim(rd)

# Here is how the dataset is set up.

head(rd,3)

# The first two columns are unique identifiers and not necessary for my analysis, so I will 
# remove them.

rd <- rd[, c(3:25)]

# The column names are rather lengthy. I would like to simplify the r coding and data 
# visualization, so I am going to rename each of them to their column number.

colnames(rd) <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                  13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)

# The analysis portion of my project begins with matrix scaling which is used throughout the rest 
# of the project. Matrix scaling requires numeric data. So, I change character column values to 
# numeric values.

rd$`3`[rd$`3` == "Female"] <- 1
rd$`3`[rd$`3` == "Male"] <- 2
rd$`3` <- as.integer(rd$`3`)

rd$`4`[rd$`4` == "Loyal Customer"] <- 1
rd$`4`[rd$`4` == "disloyal Customer"] <- 2
rd$`4` <- as.integer(rd$`4`)

rd$`6`[rd$`6` == "Business travel"] <- 1
rd$`6`[rd$`6` == "Personal Travel"] <- 2
rd$`6` <- as.integer(rd$`6`)

rd$`7`[rd$`7` == "Business"] <- 1
rd$`7`[rd$`7` == "Eco Plus"] <- 2
rd$`7`[rd$`7` == "Eco"] <- 3
rd$`7` <- as.integer(rd$`7`)

# I am also changing the "y" values to numeric as a matter of convenience.

rd$`25`[rd$`25` == "neutral or dissatisfied"] <- 0
rd$`25`[rd$`25` == "satisfied"] <- 1
rd$`25` <- as.integer(rd$`25`)

# Now let's look at a summary of the new dataset.

summary(rd)

# Based on this summary, for more meaningful results, I would like to categorize age as follows:

rd$`5`[rd$`5` <= 14] <- 1                 # Ages 07-14 Children
rd$`5`[rd$`5` > 14 & rd$`5` <= 24] <- 2   # Ages 15-24 Youth
rd$`5`[rd$`5` > 24 & rd$`5` <= 64] <- 3   # Ages 25-64 Adults
rd$`5`[rd$`5` > 64] <- 4                  # Ages 65+   Seniors
rd$`5` <- as.integer(rd$`5`)

# and flight distance as follows:

rd$`8`[rd$`8` <= 700] <- 1                 # Miles <= 700         Short
rd$`8`[rd$`8` >700 & rd$`8` < 2400] <- 2   # Miles > 700 & < 2400 Medium
rd$`8`[ rd$`8` >= 2400] <- 3               # Miles >= 2400        Long
rd$`8` <- as.integer(rd$`8`)

# I also suspect that columns 23 and 24 have little to offer. Let's see.

nzv <- nearZeroVar(rd)
nzv

# As suspected, columns 21 (named 23) and 22 (named 24) have near zero variance and will 
# contribute little to our machine learning. I will remove both items.

rd <- rd[, c(1:20,23)]

# I would also like to identify zeros or "Not Applicable" on the survey responses (columns 9-22) 
# as "NA".

rd$`9`[ rd$`9` == 0] <- NA
rd$`10`[ rd$`10` == 0] <- NA
rd$`11`[ rd$`11` == 0] <- NA
rd$`12`[ rd$`12` == 0] <- NA
rd$`13`[ rd$`13` == 0] <- NA
rd$`14`[ rd$`14` == 0] <- NA
rd$`15`[ rd$`15` == 0] <- NA
rd$`16`[ rd$`16` == 0] <- NA
rd$`17`[ rd$`17` == 0] <- NA
rd$`18`[ rd$`18` == 0] <- NA
rd$`19`[ rd$`19` == 0] <- NA
rd$`20`[ rd$`20` == 0] <- NA
rd$`21`[ rd$`21` == 0] <- NA
rd$`22`[ rd$`22` == 0] <- NA

# I would like to remove the rows with NA's. This will have little impact on the overall results # because the dataset is so large.

rd <- na.omit(rd)

## 2.2 Data Exploration and Visualization

# Now let's look at the refined dataset.

# Here are its dimensions.

dim(rd)

# Overall summary of the refined dataset.

summary(rd)

# What proportion of the passengers in the refined dataset are satisfied customers?

mean(rd$`25` == 1)

# Next, I create a new dataset to aid in my exploration and modeling of the data. I begin by 
# creating a null dataset so that I can create x and y subsets within that dataset.

sat <- list()

# I create the "x" values based on the 20 columns remaining from the original rd dataset. There 
# are actually 21 columns remaining from the orignal rd; however, the last column contains the y 
# values.

# sat$x is created as a matrix as required for subsequent analysis.

sat$x <- as.matrix(rd[, c(1:20)])

# sat$y is created as a factor as required for subsequent analysis.

sat$y <- factor(rd$`25`)

# Check the dimensions of sat$x and match to the refined rd file.

dim(sat$x)

# What proportion of the passengers are satisfied customers? Does it match the original rd 
# dataset?

mean(sat$y == 1)

### Matrix Scaling

# Now we scale the matrix in order to standardize the x values.

x_centered <- sweep(sat$x, 2, colMeans(sat$x))
x_scaled <- sweep(x_centered, 2, colSds(sat$x), FUN = "/")

# Let's see the scaled matrix from a graphical perspective.

data.frame(type = sat$y, x_scaled[,1:20]) %>%
  gather(key = "Measure", value = "value", -type) %>%
  ggplot(aes(Measure, value, fill = type)) +
  geom_boxplot()

### Clustering

# Clustering is a machine learning technique in which we do not necessarily know the outcomes, 
# but, instead, are interested in discovering groups or clusters.

### Hierarchical Clustering

# Hierarchical clustering is an algorithm that allows us to define groups or clusters.

d_features <- dist(t(x_scaled))
h <-hclust(d_features)

# Let's see what the resulting groups look like in a dendogram.

plot(h, cex = 0.65, main = "", xlab = "")

# To generate actual groups, we can decide on the number of groups we want and then find the 
# minimum distance that achieves this. I base the number of groups (k) on the dendogram.

groups <- cutree(h, k=7)
split(names(groups), groups)

### Principal Component Analysis

# Principal Component Analysis (PCA) is an exploratory tool for data analysis. The idea behind 
# PCA is to reduce the number of variables of a data set, while preserving as much information as 
# possible.

pca <- prcomp(x_scaled)
summary(pca)

# Let's see the Prinicipal Components from a graphical perspective.

data.frame(type = sat$y, pca$x[,1:20]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

# Now, let's look at PCA again using a table of the Cumulative Variance plotted against the 
# Amount of Explained Variance.

cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:22], xlab = "PC #", ylab = "Amount of Explained Variance", main = "Cumulative Variance Plot")
abline(h = 0.9, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ 90%"), col=c("blue"), lty=5, cex=0.6)

## 2.3 Modeling

### Build Training and Test Sets

# Let's create the training and test datasets by splitting sat$y and x_scaled matrices into 50% 
# test set and 50% train set.

# Create test datasets.

set.seed(143, sample.kind = "Rounding")
test_index <- createDataPartition(sat$y, times = 1, p = 0.5, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- sat$y[test_index]

# How many rows and columns does test_x have?

dim(test_x)

# How many rows does test_y have?

dim(data.frame(test_y))

# Now I create the training datasets.

train_x <- x_scaled[-test_index,]
train_y <- sat$y[-test_index]

# How many rows and columns does train_x have? Does the number of columns agree with test_x?

dim(train_x)

# How many rows does train_y have?

dim(data.frame(train_y))

# Check to make sure that the training and test sets have similar proportions of satisfied 
# customers.

mean(train_y == 1)
mean(test_y == 1)

### Modeling Approach

# Now I look at various machine learning techniques using the caret package to 
# determine which technique most accurately identifies the satisfied customer.

### K-Means Clustering
# K-means clustering is a method of partitioning observations into k clusters in which each 
# observation belongs to the cluster with the nearest mean, serving as a prototype of the 
# cluster. To use the k-means clustering algorithm we have to pre-define k, the number of 
# clusters or centers. The number of clusters I use here is based on repeating the algorithm 
# until I found the one with the greatest accuracy. I only show that particular iteration here.

predict_kmeans <- function(x, k) {centers <- k$centers
distances <- sapply(1:nrow(x), function(i)
{apply(centers, 1, function(y) dist (rbind(x[i,], y)))})
max.col(-t(distances))}

set.seed(693, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, 0, 1)

# How accurate is this model at predicting satisfied customers?

mean(kmeans_preds == test_y)

# Now let's look at sensitivity and specificity

sensitivity(factor(kmeans_preds), test_y, positive = 1) # satisfied customer
specificity(factor(kmeans_preds), test_y, negative = 0) # not satisfied or neutral customer

### K-Nearest Neighbors (Knn) Model

# K-Nearest Neighbors algorithm, or Knn, is an approach to data classification that estimates how 
# likely a data point is to belong to one group or another depending on where the data points 
# nearest to it belong. Again, I ran some preliminary iterations of the model using different k 
# values until I found the best. I only show that particular iteration here.

set.seed(447, sample.kind = "Rounding")
tuning <- data.frame(k = seq(13, 15, 1))
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = tuning)

# What is the best k value?

train_knn$bestTune
ggplot(train_knn, highlight = TRUE)

# How accurate is this model at predicting satisfied customers?

train_knn$finalModel
knn_preds <- predict(train_knn,test_x)
mean(knn_preds == test_y)

# Now let's look at sensitivity and specificity

sensitivity(factor(knn_preds), test_y, positive = 1) # satisfied customer
specificity(factor(knn_preds), test_y, negative = 0) # not satisfied or neutral customer

### Random Forest Model

# Random forests are a learning method for classification and regression and they operate by 
# constructing a multitude of decision trees at training time and outputting the class that is 
# the mode of the classes or mean prediction of the individual trees. The development of this 
# model is very time consuming and the code below takes approximately 30 minutes to run. Once 
# again, I ran several iterations of this model until I found the optimum mtry value. mtry refers 
# to the number of variables randomly sampled as candidates at each split. I only show the final 
# iteration here.

set.seed(901, sample.kind = "Rounding")
tuning <- data.frame(mtry = c(6, 7, 8, 9))
train_rf <- train(train_x, train_y,
                  method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE)

# What is the optimum mtry value?

train_rf$bestTune
ggplot(train_rf)


# Now let's look at the predictive value of this model.

rf_preds <- predict(train_rf,test_x)
mean(rf_preds == test_y)
sensitivity(factor(rf_preds), test_y, positive = 1) # satisfied customer
specificity(factor(rf_preds), test_y, negative = 0) # not satisfied or neutral customer

# What is the ranking of the variables based on importance to the Random Forest model?

varImp(train_rf)

### Ensemble Model

# This ensemble model is based on the predictions from the top two best models created 
# previously. The top two models are K-Nearest Neighbors and Random Forest.

ensemble <- cbind(knn = knn_preds == 0, 
                  rf = rf_preds == 0)
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, 0, 1)

# Let's look at the accuracy:

mean(ensemble_preds == test_y)

# Now, we look at sensitivity and specificity:

sensitivity(factor(ensemble_preds), test_y, positive = 1) # satisfied customer
specificity(factor(ensemble_preds), test_y, negative = 0) # not satisfied or neutral customer

### Accuracy Comparison Table

# Finally, we present the overall accuracy results in a single table.

models <- c("K means", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y), 
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
sensitivity <- c(sensitivity(factor(kmeans_preds), test_y, positive = 1),
                 sensitivity(factor(knn_preds), test_y, positive = 1),
                 sensitivity(factor(rf_preds), test_y, positive = 1),
                 sensitivity(factor(ensemble_preds), test_y, positive = 1))
specificity <- c(specificity(factor(kmeans_preds), test_y, negative = 0),
                 specificity(factor(knn_preds), test_y, negative = 0),
                 specificity(factor(rf_preds), test_y, negative = 0),
                 specificity(factor(ensemble_preds), test_y, negative = 0))
data.frame(Model=models, Accuracy=accuracy, Sensitivity=sensitivity, Specificity=specificity)

### The End