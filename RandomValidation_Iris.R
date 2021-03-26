### Iris: Clasificacion Binaria con Regresion Logistica 
### Random Validation: Una Iteración
#Tomamos las dos primeras clases
data(iris)
cant_datos = 100
d = iris[1:cant_datos,]
train = 0.51
test = 1-train
#Randomizamos con semilla
#set.seed(12)
#ran = sample(cant_datos)
#d = d[ran,]

### Training
d1 = d[1:(cant_datos*train),]
x1 = d1$Sepal.Length
x2 = d1$Sepal.Width
x3 = d1$Petal.Length
x4 = d1$Petal.Width
y = d1$Species
y = (d1$Species=='setosa')*1
reglog = glm(y~x1+x2+x3+x4, family = binomial())

### Test
d2 = d[(cant_datos*train+1):cant_datos,]
x11 = d2$Sepal.Length
x22 = d2$Sepal.Width
x33 = d2$Petal.Length
x44 = d2$Petal.Width
y1 = d2$Species
y1 = (d2$Species=='setosa')*1
yestr = exp(reglog$coefficients[1]+reglog$coefficients[2]*x11+reglog$coefficients[3]*x22+reglog$coefficients[4]*x33+reglog$coefficients[5]*x44)/(1+exp(reglog$coefficients[1]+reglog$coefficients[2]*x11+reglog$coefficients[3]*x22+reglog$coefficients[4]*x33+reglog$coefficients[5]*x44))
yestr2 = round(yestr)
rate_error1 = (y1==yestr2)*1
accuracy = sum(rate_error1)/length(y1)
