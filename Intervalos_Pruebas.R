### Intervalos de Confianza ###

## Media y Varianza conocida
h = c(41.6,41.48,42.32,41.95,41.86,42.18,41.72,42.26,41.81,42.04)
medx = mean(h) 
des = sd(h)
n = length(h)
desp = 0.1
alfa = 0.05
alfa2 = alfa/2
z_a2 = qnorm(1-alfa2)
L = medx - z_a2*desp/sqrt(n)
U = medx + z_a2*desp/sqrt(n)
E = 0.05
n_m = (z_a2*desp/E)^2 


## Media y Varianza desconocida
t = c(9.85,	9.93,	9.75,	9.77,	9.67,	9.87,	9.67,	9.94,	9.85,	9.75,
      9.83,	9.92,	9.74,	9.99,	9.88,	9.95,	9.95,	9.93,	9.92,	9.89)
medx = mean(t) 
des = sd(t)
n = length(t)
df = n-1
t_2 = qt(0.025, df, lower.tail = FALSE)
L = medx - t_2*des/sqrt(n)
U = medx + t_2*des/sqrt(n)


### Pruebas de Hipótesis ###

## Media y Varianza conocida
# Nula: u = 40 
# Alternativa: u no igual a 40  
u = 40
medx = 41.25 
desp = 2
n = 25
alfa = 0.05
alfa2 = alfa/2
zo = (medx-u)/(desp/sqrt(n)) #Metodo valor critico
z_a2_sup = qnorm(1-alfa2)
z_a2_inf = qnorm(alfa2)
zo > z_a2_sup
-zo < z_a2_inf
#install.packages("BSDA")
library(BSDA) 
zsum.test(mean.x = 41.25, sigma.x = desp, n.x = 25, mean.y = NULL,
          sigma.y = NULL, n.y = NULL, alternative = "two.sided", mu = 40,
          conf.level = 0.95) #Documentacion: http://finzi.psych.upenn.edu/R/library/BSDA/html/zsum.test.html


## Media y Varianza desconocida
# Nula: u = 0.81 
# Alternativa: u no igual a 0.81  
datos = c(0.8411, 0.8191, 0.8182, 0.8125, 0.8750,
          0.8580, 0.8532, 0.8483, 0.8276, 0.7983,
          0.8042, 0.8730, 0.8282, 0.8359, 0.8660)
u = 0.81
medx = mean(datos) 
des = sd(datos)
n = length(datos)
df = n-1
alfa = 0.05
alfa2 = alfa/2
to = (medx-u)/(des/sqrt(n)) #Metodo valor critico
t_2_sup = qt(alfa2, df, lower.tail = FALSE)
t_2_inf = qt(alfa2, df, lower.tail = TRUE)
to > t_2_sup
-to < t_2_inf
t.test(x = datos, alternative = "two.sided", mu = 0.81, 
       conf.level  = 0.95)
