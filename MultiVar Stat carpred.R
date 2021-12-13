
install.packages("ggplot2")
install.packages("readr")
library(ggplot2)
library(readr)



my_data=read_csv("D:/R_Project_Data/CarSales.csv")
print(head(my_data))



my_data$Resale_value <- ifelse(is.na(my_data$Resale_value),
                               ave(my_data$Resale_value,FUN=function(x) mean(x,na.rm= TRUE)),
                               my_data$Resale_value)

my_data$Sales_in_thousands <- ifelse(is.na(my_data$Sales_in_thousands),
                                     ave(my_data$Sales_in_thousands,FUN=function(x) mean(x,na.rm= TRUE)),
                                     my_data$Sales_in_thousands)

my_data$Price_in_thousands <- ifelse(is.na(my_data$Price_in_thousands),
                                     ave(my_data$Price_in_thousands,FUN=function(x) mean(x,na.rm= TRUE)),
                                     my_data$Price_in_thousands)

my_data$Engine_size <- ifelse(is.na(my_data$Engine_size),
                              ave(my_data$Engine_size,FUN=function(x) mean(x,na.rm= TRUE)),
                              my_data$Engine_size)

my_data$Horsepower <- ifelse(is.na(my_data$Horsepower),
                             ave(my_data$Horsepower,FUN=function(x) mean(x,na.rm= TRUE)),
                             my_data$Horsepower)

my_data$Wheelbase <- ifelse(is.na(my_data$Wheelbase),
                            ave(my_data$Wheelbase,FUN=function(x) mean(x,na.rm= TRUE)),
                            my_data$Wheelbase)

my_data$Width <- ifelse(is.na(my_data$Width),
                        ave(my_data$Width,FUN=function(x) mean(x,na.rm= TRUE)),
                        my_data$Width)

my_data$Power_perf_factor <- ifelse(is.na(my_data$Power_perf_factor),
                                    ave(my_data$Power_perf_factor,FUN=function(x) mean(x,na.rm= TRUE)),
                                    my_data$Power_perf_factor)

my_data$Length <- ifelse(is.na(my_data$Length),
                         ave(my_data$Length,FUN=function(x) mean(x,na.rm= TRUE)),
                         my_data$Length)

my_data$Curb_weight <- ifelse(is.na(my_data$Curb_weight),
                              ave(my_data$Curb_weight,FUN=function(x) mean(x,na.rm= TRUE)),
                              my_data$Curb_weight)

my_data$Fuel_capacity <- ifelse(is.na(my_data$Fuel_capacity),
                                ave(my_data$Fuel_capacity,FUN=function(x) mean(x,na.rm= TRUE)),
                                my_data$Fuel_capacity)

my_data$Fuel_efficiency <- ifelse(is.na(my_data$Fuel_efficiency),
                                  ave(my_data$Fuel_efficiency,FUN=function(x) mean(x,na.rm= TRUE)),
                                  my_data$Fuel_efficiency)

qplot(my_data$Resale_value, xlab = 'Resale Values', ylab = 'Count',
      main = 'Histogram of Resale values of cars', bins = 10)

qplot(my_data$Engine_size, xlab = 'Engine size', ylab = 'Count',
      main = 'Histogram of Engine size of cars', bins = 10)

qplot(my_data$Horsepower, xlab = 'Horsepower', ylab = 'Count',
      main = 'Histogram of Horsepower of cars', bins = 10)

qplot(my_data$Vehicle_type, xlab = 'Vehicle Type', ylab = 'Count',
      main = 'Histogram of type of vehicle', xlabel= 'passengeg', ylable='car')


#Encoding categorical data
my_data$Vehicle_type=factor(my_data$Vehicle_type,
                            levels=c('Passenger','Car'),
                            labels=c(1,2))

#Splitting the data into training and testing set
set.seed(123)
split=caTools::sample.split(my_data$Sales_in_thousands,SplitRatio = 0.2)
training_set=subset(my_data,split==TRUE)
test_set=subset(my_data,split==FALSE)

#Fitting regression model to the training set
regressor=lm(formula=Sales_in_thousands~. ,
             data=training_set)

summary(regressor)
#we see that wheelbase higly statistically significant


#Predicting the test set
y_pred=predict(regressor1,newdata=test_set)

test_set
y_pred

