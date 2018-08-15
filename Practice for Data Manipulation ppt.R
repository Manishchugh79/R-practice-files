str(t)
summary(t)
t_complete=t[complete.cases(t),]
summary(t_complete)
dim(t_complete)
library(dplyr)
cs2m_mutate<- mutate(cs2m, chlst_bp = Chlstrl/BP)
head(cs2m_mutate)
cs2m_1<- cs2m
cs2m_1$chlst_bp<- cs2m_1$Chlstrl/cs2m_1$BP
View(cs2m_1)
j = m
m = cs2m
dim(j)
install.packages("reshape")
library(reshape)
j = rename(j, c(DrugR = 'Reaction', Prgnt = 'Pregnant'))
summary(j)
variable.names(j)
names(j)
names(j)[5] = 'Anxiety'
names(j)[2] = "Cholestrol"
names(j)
names(j)[1] = 'Blood Pressure'
names(j)
cs2m_asce<- cs2m
cs2m_asce<- arrange(cs2m, Age)
head(cs2m_asce)
View(cs2m_acse)
cs2m_desc<- arrange(cs2m, desc(Age))
head(cs2m_desc)
grades1<-subset(grades, select = c(quiz1, gpa, final))
head(grades1)
grades4<- select(grades, quiz1, gpa, final)
grades4
grades2<- select(grades, quiz1, gpa, final)
grades2
apply(cs2m,2,mean)
apply(cs2m_1,2,mean)
mean(cs2m)
mean(cs2m$BP)
mean(cs2m$DrugR)
mean(cs2m$BP)
apply(cs2m,1,mean)
by(mtcars[,-2], mtcars$cyl, colMeans)
mtcars$cyl: 4
mtcars$cyl: 6
mtcars$cyl: 8
tapply(cs2m$BP, cs2m$Prgnt, mean)
tapply(grades$gpa, grades$ethnicity, mean)
as.numeric(TRUE)
mtcars = as.numeric(TRUE)  
by(mtcars[,-2], mtcars$cyl, colMeans)
mtcars$cyl: 4
y = as.numeric("mtcars")
by(mtcars[,2], mtcars$cyl, colMeans)
mtcars$cyl: 4
final_60<- subset(grades, final>60)
head(final_60)
boxplot(final, col = 'red')
boxplot(y$final, main = 'Box Plot of final>60', col = 'blue')
str(data)
mtcars = as.numeric(TRUE)
cor.test(grades$gpa, grades$final)
cor.test(y$gpa, y$final)
cs2m_1<- filter(cs2m, Age>20 & Age<32)
cs2m_1
cs2m_2<- filter(cs2m, Age>20 & Age<30)
cs2m_2
cs2m_3<- subset(cs2m, Age>19 & Age<31)
cs2m_3
cs2m_2<- subset(cs2m, Age>20 & Age<30)
cs2m_2
ethnicity_whites<- subset(grades, ethnicity == 4)
head(ethnicity_whites)
boxplot(ethnicity_whites$final, main = "Box Plot of final for Whites", col = 'green')
ethnicity_HISPANICS<- subset(grades, ethnicity == 5)
head(ethnicity_HISPANICS)
boxplot(ethnicity_HISPANICS$final, main = "Box Plot of final for HISPANICS", col = 'magenta')
grades$sqrtfinal<-sqrt(grades$final)
head(grades)
hist(grades$final, col = 'blue')
hist(grades$sqrtfinal, col='pink')
grades$catgryfinal<- ifelse(grades$final<60, yes = "final<60", no = "final>60")
head(grades)
table(grades$catgryfinal)
grades$final_cat<- cut(grades$final, breaks = seq(40,75,5), labels = c("final1", "final2", "final3", "final4", "final5", "final6", "final7"))
head(grades)
table(grades$final_cat)
library(readr)
k<-read.csv("C:/Users/Manish Chugh/Desktop/ACADGILD/cs2m.csv")
str(k)
summary(k$Age)
# using within()
m=k
summary(m)
View(m)
m <- within(m,{
            agecat<- NA
            agecat[Age>=15 & Age <=25]             <-'Low'
            agecat[Age>=26 & Age <=40]             <-'Middle'
            agecat[Age>41]                         <-'High'
            })
head(m,3)  
           

row.names(mtcars)<- mtcars$x
View(mtcars)
summary(mtcars)
library(datasets)
data("mtcars")
head(mtcars)
by(mtcars[,-2], mtcars$cyl, colMeans)
mtcars$cyl: 4


summarise(cs2m, mean_age = mean(Age, na.rm = T),
          median_age = median(Age, na.rm = T))
df<- cs2m %>%
  filter(Prgnt == 1)%>%
  select(BP, Age, DrugR)%>%
  mutate(BP_Age = BP/Age)
head(df)
df
install.packages("rmarkdown")
37.77-40
sd(cs2m$Age)


grades$cateth<-grades$ethnicity
grades$cateth[grades$cateth == 1|grades$cateth == 3|grades$cateth == 5] = 1
grades$cateth[grades$cateth == 2|grades$cateth == 4] =2


sam<-sample(x=1:nrow(grades), size = 0.2*nrow(grades))
grade20<-grades[sam, ]
head(grade20)
grade20
sam


summarise(cs2m, mean_age = mean(Age, na.rm = T), 
          median_age =median(Age, na.rm = T))
summarise(cs2m_1, mean_age = mean(Age, na.rm = T),
          median_age = median(Age, na.rm = T))

grades%>% group_by(ethnicity, gender)%>%
  select(gpa, ethnicity, final, gender)%>%
    summarise(avggpa = mean(gpa),
          avgfinal = mean(final))%>%
filter(avggpa>2.25)
filter(avgfinal)
grades



mtcars%>% group_by(cyl, gear)%>%
  select(mpg, cyl, wt, gear, am)%>%
  summarise(avgmpg = mean(mpg),
            avgwt = mean(wt))%>%
  filter(avgmpg>15)
filter(avgmpg)
View("avgmpg")
mtcars
filter(avgmpg)


dim(cs2m)
str(cs2m)
w=cs2m
str(w)
w$Prgnt[w$Prgnt == 0]<- 'No'
w$Prgnt[w$Prgnt == 1]<- 'Yes'
str(w)
View(w)
w$AnxtyLH[w$AnxtyLH == 0]<- 'No'
w$AnxtyLH[w$AnxtyLH == 1]<- 'Yes'
str(w)
View(w)
w$DrugR[w$DrugR == 0]<- 'No'
w$DrugR[w$DrugR == 1]<- 'Yes'
str(w)
View(w)


# convert 0 = No, 1 = Yes in cs2m
f2<-function(x){
  for(i in 1:ncol(x)){
    if(count(unique(x[i])) == 2){
      x[i]<-ifelse(x[i] == 0, 'No', 'Yes')
    }
  }
  return(x)
}

cs2m1<-f2(cs2m)
str(cs2m1)

install.packages('tidyr')
library(tidyr)
preg<-read.csv('C:/Users/Manish Chugh/Desktop/ACADGILD/Files from 5th Aug session/preg.csv')
View(preg)
str(preg)
library(dplyr)

preg1<-preg%>% gather (treatment, n, treatmenta:treatmentb)%>%
       arrange(names, treatment)
preg1

str(preg1)
str(preg)

preg1<- preg%>% gather(treatment, n, treatmenta:treatmentb)%>%
        arrange(names, treatment)
preg1

str(preg1)

preg2<- preg%>% gather(treatment, n, treatmenta:treatmentb)%>%
        arrange(names, treatment, n)
preg2
str(preg2)

preg2$treatment[preg2$treatment == 'treatmenta']<-'a'
preg2$treatment[preg2$treatment == 'treatmentb']<-'b'
preg2
str(preg2)
