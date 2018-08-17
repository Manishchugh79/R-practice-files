hist(grades$gpa)

hist(grades$gpa, xlab = "gpa", ylab = "Frequency", main = "Histogram of gpa")

hist(grades$gpa, xlab = "gpa", ylab = "Frequency", main = "Histogram of gpa", col = "red")
  
x<-grades$gpa
h<-hist(x, breaks=10, col='red', xlab="GPA", ylab="Frequency", main="Histogram of gpa with Normal Curve")
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit<-yfit*diff(h$mids[1:2]*length(x))
lines(xfit, yfit, col='blue', lwd=2)

d<-density(grades$gpa)
plot(d, main="Kernel Density of GPA")
polygon(d, col='blue', border='black')

boxplot(grades$gpa)
boxplot(grades$gpa, main="Box Plot of gpa", xlab="Box Plot", ylab="gpa", col='red')

boxplot(gpa~ethnicity, data = grades, main="GPA versus Ethnicity", xlab="GPA", ylab="Ethnicity")

boxplot(gpa~ethnicity, data = grades, main="GPA versus Ethnicity", xlab="GPA", ylab="Ethnicity", col='red')

boxplot(gpa~ethnicity, data = grades, main="GPA versus Ethnicity", xlab="GPA", ylab="Ethnicity", col=heat.colors(5))

install.packages('corrplot')
library(corrplot)
View(mtcars)
cor(mtcars[,-1])

str(mtcars)

corrplot(cor(mtcars[,-1]), type='upper',
         order='hclust',
         tl.col='black', tl.srt=90)

boxplot(gpa~ethnicity, data = grades, main="GPA versus Ethnicity", xlab="GPA", ylab="Ethnicity", col=topo.colors(5))

plot(grades$gpa, grades$final)

plot(grades$gpa, grades$final, xlab="gpa", ylab="final", col='blue', main="Scatter Plot Final vs GPA")

plot(grades$gpa, grades$final, xlab='gpa', ylab='final', mean="Scatter Plot Final vs GPA", col='blue')
abline(lm(grades$final~grades$gpa), col='red')

library(car)
install.packages("car")
scatterplotMatrix(~mpg+disp|cyl,
                  data=mtcars, lwd=3, pch=13)

grades <- read.csv(C:/Users/Manish Chugh/Desktop/ACADGILD/grades.csv)
View(grades)
counts<-table(grades$ethnicity)
barplot(counts, main='Bar Plot of Ethnicity', xlab='Ethnicity', ylab='Counts', col='red')

barplot(counts, main='Bar Plot of Ethnicity', xlab='Ethnicity', horiz=T, ylab='Counts', col='red')

barplot(counts, main='Bar Plot of Ethnicity', xlab='Ethnicity', ylab='Counts', col='red', names.arg = c("Native","Asian","Black","White",'Hispanic'))

barplot(counts, main='Bar Plot of Ethnicity', xlab='Ethnicity', ylab='Counts', col= c('red', 'blue', 'green', 'yellow', 'brown'), names.arg = c("Native","Asian","Black","White",'Hispanic'))

bp<-barplot(counts, main='Bar Plot of Ethnicity', xlab='Ethnicity', ylab='Counts', col= c('red', 'blue', 'green', 'yellow', 'brown'), names.arg = c("Native","Asian","Black","White",'Hispanic'))
text(bp,0,counts, cex=1,pos=3)

prop.counts<-counts/(sum(counts))*100
bp<-barplot(prop.counts, main='Bar Plot of Ethnicity', xlab='Ethnicity', ylab='Counts', col= c('red', 'blue', 'green', 'yellow', 'brown'), names.arg = c("Native","Asian","Black","White",'Hispanic'))
text(bp,0,round(prop.counts,1), cex=1,pos=3)

counts<-table(grades$gender,grades$ethnicity)
barplot(counts, main='Distribution of Ethnicity by Gender', xlab='Ethnicity', col=c('blue','red'), legend=rownames(counts), names.arg = c('Native','Asia','Black','White','Hispanic'))

counts<-table(grades$gender,grades$ethnicity)
barplot(counts, main='Distribution of Ethnicity by Gender', xlab='Ethnicity', col=c('blue','red'), legend=c('Female','Male'), names.arg = c('Native','Asia','Black','White','Hispanic'))

counts<-table(grades$gender,grades$ethnicity)
barplot(counts, main='Distribution of Ethnicity by Gender', xlab='Ethnicity', col=c('blue','red'), legend=c('Female','Male'), names.arg = c('Native','Asia','Black','White','Hispanic'), beside=T)

vinod=c(5,20,24,45,11)
names(vinod)=c('Natve','Asian','Black','White','Hispanic')
pie(vinod)
q()
y