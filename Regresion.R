# Regresion Lineal Simple
y = c(58,105,88,118,117,137,157,169,149,202)
x = c(2,6,8,8,12,16,20,20,22,26)
plot(x,y)
reg = lm(y~x)
summary(reg)
yest = reg$coefficients[1]+reg$coefficients[2]*x
cor(x,y)
abline(reg)
anova(reg)
library(xtable)
xtable(summary(reg))


# Regresion Lineal Multiple
data(iris)
c = cor(iris[,1:4])
#install.packages(corrplot)
library(corrplot)
corrplot(c)
reg2 = lm(iris$Petal.Length~iris$Petal.Width)
summary(reg2)
reg3 = lm(iris$Sepal.Width~iris$Sepal.Length)
summary(reg3)
reg4 = lm(iris$Petal.Width~iris$Sepal.Length+iris$Sepal.Width+iris$Petal.Length)
summary(reg4)


