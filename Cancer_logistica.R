library(readr)
breast <- read_csv("C:/Users/Diego/Desktop/breast-cancer-wisconsin.data", 
                                    col_names = FALSE)
View(breast)

breast1 = breast[1:490,] #train
breast2 = breast[491:699,] #test


### Training (70% de los datos)
x1 = breast1$X2
x2 = breast1$X3
x3 = breast1$X4
x4 = breast1$X5
x5 = breast1$X6
#x6 = breast1$X7 # elimino toda la variable x6
x6 = breast1$X8
x7 = breast1$X9
x8 = breast1$X10
y = breast1$X11
y = ((y=="4")*1) # cambio de ceros y unos
reg = glm(y~x1+x2+x3+x4+x5+x6+x7+x8,family = binomial())
summary(reg)

# coeficientes de regresion
b0 = reg$coefficients[1]
b1 = reg$coefficients[2]
b2 = reg$coefficients[3]
b3 = reg$coefficients[4]
b4 = reg$coefficients[5]
b5 = reg$coefficients[6]
b6 = reg$coefficients[7]
b7 = reg$coefficients[8]
b8 = reg$coefficients[9]

### Test (30% de los datos)
x11 = breast2$X2
x22 = breast2$X3
x33 = breast2$X4
x44 = breast2$X5
x55 = breast2$X6
#x6 = breast1$X7 # elimino toda la variable x6
x66 = breast2$X8
x77 = breast2$X9
x88 = breast2$X10
y1 = breast2$X11
y1 = ((y1=="4")*1) # cambio de ceros y unos
yest = exp(b0+b1*x11+b2*x22+b3*x33+b4*x44+b5*x55+b6*x66+b7*x77+b8*x88)/(1+(exp(b0+b1*x11+b2*x22+b3*x33+b4*x44+b5*x55+b6*x66+b7*x77+b8*x88)))
yest1 = round(yest)
error = (yest1==y1)*1
porc_ace = sum(error)/length(error)

