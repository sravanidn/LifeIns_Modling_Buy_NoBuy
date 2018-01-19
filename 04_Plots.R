library(readr)
Sravani <- read_csv("C:/Users/srava/Desktop/Sravani_1.csv")
View(Sravani)

install.packages("tidyr")
install.packages("dplyr") #Data Manipulation
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(ggplot2)

colnames(Sravani)
data <- data.frame(Sravani)
colnames(data)

# The X, Y goes in aes, Geom_point is for scatterplot. 
attach(data)
colnames(data)[colnames(data) == "Age Bins"] <- 'AgeBins'
data$AgeBins_1 <- NA
data$AgeBins_1 <- as.factor(data$Age.Bins)
plot(data$AgeBins_1, legend.text = "AGe Bins",col= 'Blue')

#Plotting Age Bins - Segment wise
ggplot(data, aes(x=AgeBins_1))+ 
  geom_histogram(stat = "count", na.rm = TRUE, aes(fill = segment)) 
+ facet_wrap(~segment,scales = "free")

#Plotting AgeBins Vs Employment Status
agevsemp = ggplot(data, aes(x=Q10))+ 
  geom_histogram(stat = "count", na.rm = TRUE, aes(fill = segment)) 
+ facet_wrap(~segment,scales = "free")
plot(agevsemp)

#Layer with Income level on the plot above 
data$IncomeLevel <- NA
data$IncomeLevel <- as.factor(data$Q8)
agevsempvsincome = agevsemp + geom_bar(data="IncomeLevel",stat = "count")




















