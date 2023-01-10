## Session 4, Data Science in R, Nick's cohort
## Date not given
## Can be combined with powerpoint and excel sheets for a general overview of R.

#----Machine learning in R-----------------------------------------------------------------------------------------------------------
## Machine learning is more commonly used in python, with statistical analysis performed in R. However, R can also be used for machine learning

## There are multiple types of machine learning but the main types I will cover are supervised, unsupervised, and reinforcement learning.
# When using supervised learning, algorothm is told what the coder expects from example input (i.e. what the data represetns). The algorithm will then use this information to define other groups the coder has not defined
# Unsupervised learning provides no expectation from the coder, and the algorithm works to detect structure from unlabelled data
# Reinforcement learning is where the algorithm performs a task using feedback from the environment it is in, gaining "rewards" for doing the correct action

# Packages needed
library(ggplot2)
library(corrgram)
library(dplyr)
library(caTools)

#----Supervised ML-------------------------------------------------------------------------------------------------------------------

# Supervised, i.e. specified which "group" a certain point belongs
# Common types are linear modelling (regression with prediction) and random forrest. There are many types of supervised learning, covered efficiently here http://www.john-ros.com/Rcourse/supervised.html
# Regression for use in predictions - a common stats method but can be used for predicting as well
# Split data set into train and test
setwd("C:/Users/lby76f/Desktop/Teaching/Session 1")
all_rounders<-read.csv("All_rounders.csv", header=TRUE, row.names = 1)
# Split data into test and train (should really do this using renadom numbers)
set.seed(50)
split<-sample.split(Y=all_rounders$Ave, SplitRatio=0.7)
train<-subset(x = all_rounders, split==TRUE)
test<-subset(x = all_rounders, split==FALSE)

# Want a model that will work out likely average from other information given - corrgram shows correlation across variables, from this can see that hundreds and runs, and five-fors and wickets are heavily correlated so may want to remove from model
corrgram(all_rounders, lower.panel = panel.shade, upper.panel = panel.cor)
# Use all variables in the model
m1<-lm(Ave~., data = test)
# Show model output
m1
summary(m1)
# Save predictions of likely average from other information given in test data
pred<-predict(m1, test)

# Visualise predictions
pred
# Calculate likely average from other data defined in try. This needs to be in a dataframe and titles need to match original dataframe
try<-data.frame(Runs = 3000, HS = 150, Hundreds = 5, Wkts = 230, Ave.1 = 35.3, Five_fors = 3, Ct = 45)
predict(m1, try)

# Extract residuals
modelres<-as.data.frame(residuals(m1))
# Look at residuals - doesn't look normal, how do we deal with this, is it due to overfit of the model, would this look normal is there was more data - think about the usual responses to non-parametric data (can we transform, worth using glmm etc?)
ggplot(modelres, aes(residuals(m1))) +
  geom_histogram(fill = "blue", color="black", bins = 10) +
  theme_bw()

# Training least squares loss, measure of error. A measure of how well the model works against known data in training
Evaluationtrain<-as.data.frame(cbind(train$Ave, predict(m1,train)))
colnames(Evaluationtrain)<-c("Actual", "Predicted")
# Training error in model
msetrain<-mean((Evaluationtrain$Actual - Evaluationtrain$Predicted)^2)
rmetrain<-sqrt(msetest)

# Test Least squares loss, measure of error. A measure of how well the model works against test data
Evaluationtest<-as.data.frame(cbind(test$Ave, pred))
colnames(Evaluationtest)<-c("Actual", "Predicted")
# Testing error in model
msetest<-mean((Evaluation$Actual - Evaluation$Predicted)^2)
rmsetest<-sqrt(msetest)

# Compare the train and testing LSL shows that we effectively do better forecasting the test data from the train data than prdicting the train data. This may be to do with outliers or misfitting of models etc
# As the test data is closer to the model than training data, the model is not overfit. However, could do with a lot of improvement as doesn't fit training data particulary well (large LSL)
cbind(msetrain, msetest)

# Graphing performance of test data, its not too bad
ggplot(Evaluation, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(color="red") +
  theme_bw()

# Graphing residual testing
Evaluation$res<-Evaluation$Predicted - Evaluation$Actual
ggplot(Evaluation, aes(x = Predicted, y = res)) +
  geom_pointrange(aes(ymin = 0, ymax= res)) +
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle("residuals vs linear model predictions") +
  theme_bw()

# This is a very simple model and we haven't bothered to try and improve the fit. We could do this with model selection using AIC or LMS as you would in any linear model, can use the command "step" within lm to achieve this
# Going back to our examination of normality, might want to try and run a generalised linear model to forecast

#----Unsupervised learning---------------------------------------------------------------------------------------------------------------

# Unsupervised, i.e. not specified which "group" a certain datapoint belongs
# k-means clustering
# This was covered in the first session material as an example of what R could do

#----Hierarchical clustering-------------------------------------------------------------------------------------------------------------

# Works in a bottom up fashion to cluster points https://www.geeksforgeeks.org/hierarchical-clustering-in-data-mining/
# Similar to k means but does not require the user to specify number of clusters before running the analysis
# Need to specify a distance method i.e. how data is scaled and compared between rows of the data frame, can use variable distance matrices but most commonly used are euclidean
dis<-dist(all_rounders, method="euclidean")
clus<-hclust(dis)
plot(clus)
# Can decide how many groups we want to seperate the data into, groups then defined in cut
cut<-cutree(clus, 3)
cut

#----Principle Components Analysis (PCA)-------------------------------------------------------------------------------------------------

# Further theory reading https://royalsocietypublishing.org/doi/10.1098/rsta.2015.0202
# Dimensionality reduction tries to transform data into a 2D space (PCA1 and PCA2) and summerise properties of the whole data set
# Using our all rounders dataset
setwd("C:/Users/lby76f/Desktop/Teaching/Session 1")
all_rounders<-read.csv("All_rounders.csv", header=TRUE, row.names = 1)
all_rounders$hun<-ifelse(all_rounders$Hundreds == 0, 'No', 'Yes')
# Can only be used for independent variables so need to get rid of HS, Hundreds, Five_fors, and whether they have got a 100 or not
all_rounders2<-all_rounders[,-c(2, 4, 7, 9)]
cric_pca<-prcomp(all_rounders2, centre = TRUE, scale. = TRUE)
cric_pca
# Summary shows that PC1 covers 48% of the variance seen in the dataset, and that PC2 covers 32% etc.
summary(cric_pca)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g<-ggbiplot(cric_pca,
         obs.scale = 1,
         var.scale = 1,
         groups = all_rounders$hun,
         ellipse = TRUE) +
  theme_bw () +
  labs(title = "International all rounders",
         subtitle = "1900 - 2022",
       color = "Have they got a hundred?")
g

# This shows that Average, number caught, and Runs are positively correlated with PC1, Average.1 is positively correlated with PC2, and that number of wickets is negatively correlated with PC2
# It also shows that there is a postitive correlation between Runs, batting average, and number caught (makes sense) and a negative correlation between number of wickets and bowling average (also makes sense)
# In the future can predict classes from this dataset, for example, if we had another set of all rounders that we only had partial information for we would be able to calculate the rest of the information

#----Reinforcement learning------------------------------------------------------------------------------------------------------------
# Reinforcement learning is not covered in this example as I do not have an appropriate dataset to test on. Example datasets that will take you through the process can be found...
# https://www.oxford-man.ox.ac.uk/wp-content/uploads/2020/05/Reinforcement-Learning-in-R.pdf
# https://dataaspirant.com/reinforcement-learning-r/
