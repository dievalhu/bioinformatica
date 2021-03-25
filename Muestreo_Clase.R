# Muestreo
edad = c(35, 37, 49, 40, 31, 33, 36, 44)
sample(1:10,5)
set.seed(4) # el 4 puede ser cualquier valor entero positivo
sample(edad,5) # 5 muestras del vector edad
m = sample(1:length(edad),4)
s = edad[m]
