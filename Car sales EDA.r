https://www.kaggle.com/code/gavrildsouza/sales-of-cars-eda

install.packages("ggplot2")
install.packages("readr")
library(ggplot2)
library(readr)

my_data=read_csv("/kaggle/input/car-sales/Car_sales.csv")
head(my_data)

names(my_data)[1] <- "Manufacturer"
names(my_data)[2] <- "Model"
names(my_data)[3] <- "Sales_in_thousands"
names(my_data)[4] <- "Resale_value"
names(my_data)[5] <- "Vehicle_type"
names(my_data)[6] <- "Price_in_thousands"
names(my_data)[7] <- "Engine_size"
names(my_data)[8] <- "Horsepower"
names(my_data)[9] <- "Wheelbase"
names(my_data)[10] <- "Width"
names(my_data)[11] <- "Length"
names(my_data)[12] <- "Curb_weight"
names(my_data)[13] <- "Fuel_capacity"
names(my_data)[14] <- "Fuel_efficiency"
names(my_data)[15] <- "Latest_Launch"
names(my_data)[16] <- "Power_perf_factor"

head(my_data)

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

  
qplot(my_data$Engine_size, xlab = 'Engine size', ylab = 'Count',
      main = 'Histogram of Engine size of cars', bins = 10)
      
qplot(my_data$Horsepower, xlab = 'Horsepower', ylab = 'Count',
      main = 'Histogram of Horsepower of cars', bins = 10)
      
qplot(my_data$Resale_value, xlab = 'Resale_value', ylab = 'Count',
      main = 'Histogram of Resale_value of cars (in K)', bins = 10)
