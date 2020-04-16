setwd("project")
getwd()
library(plyr) #data manipulation
library(dplyr) 
library(ggplot2) #For graph plotting
library(corrplot) #for correlation plot
library(gridExtra) #For multiple plots
library(mlbench) #For feature engineering
library(caret) #For feature engineering
library(tidyverse)
library(caTools)
library(ggplot2)
library(dplyr)
diamonds<- read.csv("~/Desktop/diamonds.csv")
summary(diamonds)
dim(diamonds) # The number of observations and variables.
rng <- range(diamonds$price)
rng
diamonds %>%
  summarise(mean = mean(price), sd = sd(price), median = median(price, max=max(price))) 
diamonds %>%
  group_by(cut) %>%
  summarise(counts = n())
diamonds %>%
  group_by(cut) %>%
  summarise(counts = n() / nrow(diamonds))
diamonds %>%
  group_by(carat) %>%
  summarise(counts = n())
#Four factor impact on price
ggplot(diamonds, aes(x=color)) + geom_bar(fill="rosybrown4") #Bar chart(color):(categorical variable) 
ggplot(diamonds, aes(x=clarity)) + geom_bar(fill="rosybrown4") #Bar chart(clarity):(categorical variable)
#Bar chart(cut):(categorical variable)
ggplot(diamonds, aes(x=cut)) + geom_bar(fill="rosybrown4")
#Histogram(price):(continuous variable)
ggplot(diamonds, aes(x=price)) +
  geom_histogram(binwidth=2000, fill="rosybrown4")
#Histogram(carat): (continuous variable)
ggplot(diamonds, aes(x=carat)) +  geom_histogram(binwidth=2, fill="rosybrown4")
#Carat :The factors have some impact on price:(Scatter plot(continuous+ continuous variable))
diamonds_filtered <- diamonds %>% filter(carat <= 2)

#Clarity :The factors have some impact on price: (box plot(categorical + continuous))
ggplot(diamonds, aes(clarity, price, fill = color)) + geom_boxplot()
#Color :The factors have some impact on price: (box plot(categorical + continuous))
ggplot(diamonds, aes(color, price, fill = color)) + geom_boxplot()
#Cut :The factors have some impact on price:(boxplot(categorical + continuous))
ggplot(diamonds, aes(cut, price, fill = color)) + geom_boxplot()




#check the values to see if I have any NAs in my data 
sum(is.na(diamonds))
which(is.na(diamonds))
#Capping outlier values at 98th percentile
# Create a new dataframe with same no of rows as dataset and 1 column
percentile_98th <- diamonds[1,]

# Seperating continuous and categorical variables

categorical_var <- diamonds[,c(2,3,4)]
continous_var <- diamonds[,-c(2,3,4)]

# Checking the 98th percent value for each column of continuous variable


for(i in 1:ncol(continous_var)){
  if(is.numeric(continous_var[,i])){
    percentile_98th[,i] <- quantile(continous_var[,i], 0.98, na.rm = T)
  }
}

# Capping values at 98th percentile columnwise
for(i in 1:ncol(continous_var)){
  if(is.numeric(continous_var[,i])){
    continous_var[which(continous_var[,i]>percentile_98th[,i]),i] <- percentile_98th[,i] 
  }
}

plot1 <- ggplot(diamonds,aes(x=price))+
  geom_histogram(color="rosybrown4",fill = "rosybrown4",binwidth=100)+
  scale_x_continuous(breaks=seq(300,19000,1000),limit=c(300,19000))+
  ggtitle('Price')
plot2 <- ggplot(diamonds,aes(x=price))+
  geom_histogram(color=' rosybrown3',fill=' rosybrown3',binwidth=0.01)+
  scale_x_log10(breaks=seq(300,19000,1000),limit=c(300,19000))+
  ggtitle('Price(log10)')
grid.arrange(plot1,plot2,ncol=2)


#linear regression
split = sample.split(diamonds$carat, SplitRatio = 0.7) 
trainingset = subset(diamonds, split == TRUE) 
testset = subset(diamonds, split == FALSE)
lm.r= lm(formula = carat ~ price, data = trainingset) 
coef(lm.r)
ypred = predict(lm.r, newdata = testset)
ggplot() + geom_point(aes(x = trainingset$price,  
                          y = trainingset$carat), colour = 'rosybrown4') +
  geom_line(aes(x = trainingset$price, 
                y = predict(lm.r, newdata = trainingset)), colour = 'rosybrown1') +
  
  ggtitle('Carat vs Price (Training set)') +
  xlab('Price') +
  ylab('Carat')

ggplot() +
  geom_point(aes(x = testset$price, y = testset$carat),
             colour = 'rosybrown4') +
  geom_line(aes(x = trainingset$price,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'rosybrown1') +
  ggtitle('Carat vs Price (Test set)') +
  xlab('Price') +
  ylab('Carat')



resid_frame <- data.frame(predicted = fitted.values(mod),
                          residuals = residuals(mod))
ggplot(resid_frame, aes(x = predicted, y = residuals)) + geom_point(alpha = 0.2, color = 'rosybrown4') + geom_smooth(color = "darkblue")


#(log regression)
ggplot(diamonds_filtered, aes(x = carat, y = price)) +
  geom_point(colour = 'rosybrown4') + geom_smooth(method="lm")

#Log regression: 
ggplot(diamonds_filtered, aes(x = log(carat), y = log(price))) +
  geom_point(colour = 'rosybrown4') + geom_smooth(method="lm")

#fit polynomial regressions :using the function(poly) to specify the maximum degree you want to fit in your model 

ggplot(diamonds_filtered, aes(x = carat, y = price)) + geom_point(colour = 'rosybrown4') +
  geom_smooth(method="lm", formula = y ~ poly(x, 4))
#Evaluation

diamonds_model <- lm(carat ~ price, data=diamonds)
diamonds$carat_pred <- predict(diamonds_model, newdata=diamonds)
error_sq <- (diamonds$carat_pred - diamonds$price)^2
( RMSE <- sqrt(mean(error_sq)) )

#Calculating R-square:

error_sq <- (diamonds$diamonds_model - diamonds$price)^2
numerator <- sum(error_sq)
delta_sq <- (mean(diamonds$price) - diamonds$price)^2
denominator = sum(delta_sq)
(R2 <- 1 - numerator/denominator)

#Visualization of final results

ggplot(diamonds_model,aes(x=carat,y=price))+
  geom_point(color="rosybrown4",fill="rosybrown4")+
  xlim(0,quantile(diamonds_model$carat,0.99))+
  ylim(0,quantile(diamonds_model$price,0.99))+
  ggtitle('Diamond price vs. carat')


                     
                          
                          