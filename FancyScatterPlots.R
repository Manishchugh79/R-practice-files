install.packages('ggplot2')
library(ggplot2)

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = grades)+
  geom_point(mapping = aes(x = gpa, 
                           y = final, 
                           color = ethnicity, 
                           size = ethnicity))

ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ,
                           y = hwy, 
                           color = class, 
                           size = class))

ggplot(data = grades)+
  geom_point(mapping = aes(x = gpa,
                           y = final,
                           color = ethnicity,
                           size = ethnicity))

ggplot(data = grades)+
  geom_point(mapping = aes(x = gpa,
       y = final, color = ethnicity,
       size = ethnicity,
       shape = ethnicity))
grades<- as.variable(TRUE)


ggplot(data = grades)+
  geom_smooth(mapping = aes(x = gpa,
                           y = final, 
                           group = ethnicity,
                           color = ethnicity))

ggplot(data = grades)+
  geom_smooth(mapping = aes(x = gpa,
  y = final, color = ethnicity,
              size = ethnicity))


ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,
                           y=hwy))+
  facet_wrap(~class, nrow = 2)

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x=displ,
                            y=hwy,
                            color=class,
                            size=class))

ggplot(data = mpg, mapping = aes(x=displ,
                                 y=hwy))+
  geom_point(mapping = aes(color = class))+
  geom_smooth()


ggplot(data = grades, mapping = aes(x = gpa,
                                    y = final))+
  geom_point(mapping = aes(color = ethnicity))+
  geom_smooth()

library(car)
install.packages('car')
"car"

scatterplot(mpg~wt|cyl, data = mtcars,
            xlab = "Weight of car",
            ylab = "Mile Per Gallon",
            main = "Enhanced Scatter Plot",
            labels = row.names(mtcars))

scatterplot(final~gpa|ethnicity,data = grades,
            xlab = "gpa",
            ylab = "final",
            main = "Enhanced Scatter Plot",
            labels = row.names(grades))

library(car)
scatterplot(mpg~wt|cyl, data = mtcars,
            xlabs = "Weight of car",
            ylabs = "Mile per Gallon",
            main = "Enhanced scatter Plot",
            labels = row.names(mtcars))

scatterplotMatrix(~mpg+disp+
                     drat+wt|cyl,
                   data = mtcars,
    main = "Three Cylinder Option")

scatterplotMatrix(~final+quiz1+
                    quiz2+gpa|ethnicity,
                  data = grades,
                  main = "Five Ethnic Groups")

