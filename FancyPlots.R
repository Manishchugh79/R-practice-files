dose<- c(20, 30, 40, 45, 60)
drugA<- c(16, 20, 27, 40, 60)
drugB<- c(15, 18, 25, 31, 40)

par(pin=c(2,3)) #plot will be 2*3
par(lwd=3, cex=1.5)
par(cex.axis=0.75, font.axis=3)
plot(dose,drugA,
     type='b', pch=23, lty=2,
     col='blue',bg='green')

install.packages("plotfunctions")
library(plotfunctions)

par(pin=c(2, 3)) #plot will be 2*3
par(lwd = 2, cex = 1.5)
par(cex.axis = 0.75, font.axis = 3)
plot(dose,drugA,
     type = 'b', pch = 23, lty = 2,
     col = 'red', bg = 'green')

par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=3)
plot(dose,drugA,
     type='b', pch=23, lty=2,
     col="red", bg='green')


par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1.5)
par(cex.axis=0.75, font.axis=3)
plot(dose,drugA,
     type='b', pch=23, lty=2,
     col="red", bg='green')

title(main='Line Diagram', col.main='red',
      sub='Drug A vs Dose', col.sub='blue',
      xlab='Dose', ylab='Drug Al',
      col.lab='green', cex.lab='0.75')


par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=3)
plot(dose,drugA,
     type='b', pch=25, lty=2,
     col="blue", bg='green')


par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1.5)
par(cex.axis=0.75, font.axis=3)
plot(dose,drugA,
     type='b', pch=15, lty=2,
     col="red", bg='green')


par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=1)
plot(dose,drugA,
     type='b', pch=25, lty=2,
     col='blue', bg='green')

par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=2)
plot(dose,drugA,
     type='b', pch=25, lty=2,
     col='blue', bg='green')

par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=1)
plot(dose,drugA,
     type='b', pch=25, lty=2,
     col='blue', bg='green')

par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=2)
plot(dose,drugA,
     type='b', pch=25, lty=2,
     col='blue', bg='green')

par(pin=c(2,3)) #plot will be 2*3
par(lwd=2, cex=1)
par(cex.axis=0.75, font.axis=3)
plot(dose,drugA,
     type='b', pch=25, lty=2,
     col='blue', bg='green')

dotchart(mtcars$mpg, labels=row.names(mtcars),
         cex=.5, col='blue',
         main='Gas Milage by Cars',
         xlab="Miles/Gallon")
#dot plot by group
myCars<-mtcars[order(mtcars$mpg),] #ordering mpg
myCars$cyl<- factor(myCars$cyl) #making cyl as factor
myCars$color[myCars$Cyl==4] <- 'red' #assigning
myCars$color[myCars$Cyl==6] <- 'blue' #assigning
myCars$color[myCars$Cyl==8] <- 'purple' #assigning
dotchart(myCars$mpg, labels=row.names(myCars),
         cex=.5, groups=myCars$cyl,
         main='Gas Milage by Cars',
         xlab="MPG", color=myCars$color)
View(myCars)

a<- table(grades$ethnicity)
a
group<-c("Native/American", "Asian", "Black", "White", "Hispanic")
pie(a, labels=group,
    main="Pie Ethnicity")

prcnt<-round(a/sum(a)*100)
grouped<-paste(group, prcnt) #add percents to
label<-paste(grouped,"%", sep="") #add % to
pie(a,labels=label,
    col=rainbow(length(label)),
    main="Pie Chart-Percentage of Ethnicity")

count<-round(a)
grouped<-paste(group, count) #add count
label<-paste(grouped, "#", sep="") # add #
pie(a,labels=label,
    col=rainbow(length(label)),
    main='Pie Chart-Counts of Ethnicity')

install.packages('plotrix')
library(plotrix)

count<-round(a)
grouped<-paste(group, count) #add count
label<-paste(grouped, "#", sep="") # add #
pie3D(a, labels=label,explode=0.1,
      main="3D Pie Chart of Ethnicity")


prcnt<-round(a/sum(a)*100)
grouped<-paste(group, prcnt) #add percents to
label<-paste(grouped,"%", sep="") #add % to
pie3D(a, labels=label,explode=0.1,
      main="3D Pie Chart of Ethnicity")


install.packages('ggplot2')
library(ggplot2)

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)+
geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = grades)+
  geom_point(mapping=aes(x=gpa,
                         y=final,
                         color=ethnicity,
                         size=ethnicity))

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,
                         y=hwy,
                         color=class,
                         size=class))

ggplot(data = grades)+
  geom_point(mapping = aes(x=gpa,
       y = final, color = ethnicity,
       size = ethnicity,
       shape = ethnicity))

ggplot(data=grades)+
  geom_smooth(mapping = aes(x=gpa,
                            y=final,
                            group=ethnicity,
                            color=ethnicity))

ggplot(data=grades)+
  geom_smooth(mapping = aes(x=gpa,
  y=final,color=ethnicity,size=ethnicity))

ggplot(data=mpg)+
geom_point(mapping=aes(x=displ,
                       y=hwy))+
  facet_wrap(~class, nrow=2)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,
                         y=hwy, color=class, size=class))

ggplot(data=mpg)+
geom_smooth(mapping=aes(x=displ,
                       y=hwy, color=class, size=class))

ggplot(data=mpg, mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth()

ggplot(data=grades, mapping=aes(x=gpa,y=final))+  
geom_point(mapping=aes(color=ethnicity))+
geom_smooth(mapping=aes(size=ethnicity))  

install.packages('car')
library(car)
library(ggplot2)

scatterplot(mpg~wt|cyl, data=mtcars,
            xlab="Weight of Car",
            ylab="Miles Per Gallon",
            main="Enhanced Scatter Plot",
            labels=row.names(mtcars))



scatterplot(final~gpa|ethnicity, data=grades,
            xlab="gpa",
            ylab="final",
            main="Enhanced Scatter Plot",
            labels=row.names(grades))
library(car)
library(carData)

scatterplotMatrix(~mpg+disp+
                     drat+wt|cyl,
                   data=mtcars,
                   main="Three Cylinder Options")

scatterplotMatrix(~final+quiz1+
                    quiz2+gpa|ethnicity,
                  data=grades,
                  main="Five Ethnic Groups")
