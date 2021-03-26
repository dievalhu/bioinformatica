### Regresion
### Cross Validation
#Datos
#install.packages("ISLR") 
library(ISLR)
data(Auto)
head(Auto, 3)
dim(Auto)

# Randomizando los datos 
set.seed(1)

### Training (50/50)
train <- sample(x = 1:392, 196)
plot(Auto$mpg,Auto$horsepower)

modelo <- lm(mpg~horsepower, data = Auto, subset = train)
summary(modelo)

### Predicciones Test 
predicciones <- predict(object = modelo, newdata = Auto[-train, ])
library(Metrics)
library(caret)
MSE = mse(Auto$mpg[-train],predicciones)

### Random Cross Validation: 100
library(ggplot2)
library(gridExtra)
cv_MSE <- rep(NA,100)
for (i in 1:100) {
  train <- sample(x = 1:392, 196)
  modelo <- lm(mpg ~ horsepower, data = Auto, subset = train)
  predicciones <- predict(object = modelo,newdata = Auto[-train, ])
  cv_MSE[i] <- mse(Auto$mpg[-train],predicciones)
}
p1 <- ggplot(data = data.frame(cv_MSE = cv_MSE), aes(x = 1, y = cv_MSE)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = c("firebrick3"), width = 0.1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2 <- ggplot(data = data.frame(cv_MSE = cv_MSE), aes(cv_MSE)) +
  geom_histogram(colour = "firebrick3") +
  theme_bw()
grid.arrange(p1, p2, ncol = 2)

summary(cv_MSE)
sd(cv_MSE)

### Diagrama de Dispersion
ggplot(data = Auto, aes(x = horsepower, y = mpg)) +
  geom_point(colour = c("firebrick3")) +
  geom_smooth(method = "lm", colour = "black") +
  theme_bw() +
  labs(title  =  'mpg ~ horsepower') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

### Modelo Lineal General: Polinomiales: x^1....x^10 --- poly(horsepower,2, raw = TRUE)
cv_MSE <- rep(NA,10)
set.seed(1)
train <- sample(x = 1:392, 196)

for (i in 1:10) {
  modelo <- lm(mpg ~ poly(horsepower,i), data = Auto, subset = train)
  predicciones <- predict(object = modelo, newdata = Auto[-train, ])
  cv_MSE[i] <- mse(Auto$mpg[-train],predicciones)
}
ggplot(data = data.frame(polinomio = 1:10, cv_MSE = cv_MSE),
       aes(x = polinomio, y = cv_MSE)) +
  geom_point(colour = c("firebrick3")) +
  geom_path() +
  scale_x_continuous(breaks = c(0:10)) +
  theme_bw() + 
  labs(title  =  'Test Error ~ Grado del polinomio') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

min(cv_MSE)
modelo1 <- lm(mpg ~ poly(horsepower,2,raw = TRUE), data = Auto, subset = train)
modelo2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)

### Diagrama de Dispersion (mejor modelo)
ggplot(data = Auto, aes(x = horsepower, y = mpg)) +
  geom_point(colour = c("firebrick3")) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black") +
  theme_bw() +
  labs(title  =  'mpg ~ horsepower^2') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))


