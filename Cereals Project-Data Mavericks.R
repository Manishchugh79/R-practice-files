install.packages("arrange")
library(arrange)

cereal_asce<- arrange(cereal, calories)
head(cereal_asce)

cereal_desc<-arrange(cereal, desc(calories))
head(cereal_desc)

boxplot(cereal$calories, main="Box Plot of Calories", col = "red")

hist(cereal$calories, xlab="Calories", ylabs="Frequency", main="Histogram of Cereals", col="red")

install.packages("car")
library(car)
scatterplotMatrix(~calories+sugars+carbo+fat|type,
                   data=cereal,
                  main = "Calories vs Sugar, Carbo & Fat")

scatterplotMatrix(~calories+protein+vitamins+fiber|type,
                  data=cereal, 
                  main = "Calories vs Protein,Vitamin & Fiber")

install.packages("readr")
library(readr)
library(dplyr)
library(ggplot2)
install.packages(scales)
library(scales)
library(quantmod)
install.packages("quantmod")

glimpse(cereal)
par()
par(mfrow=c(2,2))
plot(cereal$protein, cereal$calories)
plot(cereal$vitamins, cereal$calories)
plot(cereal$fiber, cereal$calories)

#making line graph using ggplot
ggplot(data=cereal, mapping=aes(x=calories, y=protein))+
  geom_line()

str(cereal)

install.packages("readxl")
library(readxl)
cereal1<-read.csv("C:/Users/Manish Chugh/Desktop/ACADGILD/4. Files for Assignment_8.12.2018/cereal")
View(cereal1)
View(cereal)
