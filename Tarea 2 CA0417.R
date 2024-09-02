#-------------------------Tarea 2 CA0417---------------------------------------

datos <- c(40.2,41.0,41.1,40.1,40.4,41.3,41.6,41.9,42.1,41.5,41.3,40.8,40.3,40.9
           ,40.8)
n <- length(datos)

datos_transf <- c()
for (i in 1: (n-1)){
  datos_transf[i] <- log(datos[i+1])-log(datos[i])
  
}
datos_ordenados <- rev(datos_transf)

# a) Volatilidad calculando el estimador insesgado usual

# Media 
m <- length(datos_transf)
media <- mean(datos_transf)
estimador_insesgado <- sd(datos_ordenados)

#b) Suponiendo que la media es cero y reemplazando m−1 por m en el denominador.
estimador_insesgado_2 <- sqrt((1/m)*sum(datos_ordenados^2))

#c) Usando un estimador EWMA con ponderador histórico igual a 0.95.
lambda <- 0.95

var_EWMA <- (1 - lambda) / (1 - lambda^14) * sum(lambda^(0:(m-1)) * (datos_ordenados - media)^2) 
estimador_insesgado_EWMA <- sqrt(var_EWMA) 

# Use los tres resultados para estimar el VaR y el ES a un día y al 95 % de una posición
#conformada de 1000 acciones, compare los resultados. Suponga que los rendimientos
# siguen una distribución normal.

z <- qnorm(0.95, 0,1) #cuantil

#insesgado 1:
Var1 <- 1000*z*estimador_insesgado
ES1 <- 1000*estimador_insesgado*exp(-z^2/2)/sqrt(2*pi)*0.95

#insesgado 2:
Var2 <- 1000*z*estimador_insesgado_2
ES2 <- 1000*estimador_insesgado_2*exp(-z^2/2)/sqrt(2*pi)*0.95

#insesgado 3:
Var3 <- 1000*z*estimador_insesgado_EWMA
ES3 <- 1000*estimador_insesgado_EWMA*exp(-z^2/2)/sqrt(2*pi)*0.95


#El VaR y Es son similares en los tres casos, sobre todo usando la volatilidad 
# usual y con el estimador EWMA. En ese caso, aproximadamente el nivel de
# perdidas que se supera un 95% de las veces es 22 y las pérdidas esperadas dado que 
# superen al VaR es de 1.33. Con el estimador del inciso b),
# el VaR es de 21.6 y la ES es 1.29.

#2. Suponga que el precio de un activo al cierre es de $200. Su volatilidad se estima en
# 1.5 % diaria. Si el precio de hoy es $103, actualice el estimado de volatilidad usando:

#a) Un modelo EWMA con ponderador histórico 0.93

rt <- log(103)-log(200)

ponderador <- 0.93
nueva_volatilidad_EWMA <-sqrt(ponderador*(0.015^2)+(1-ponderador)*(rt^2))


# b) Un modelo GARCH(1, 1) con ω = 0.0000018, α = 0.03 y β = 0.95.
w <- 0.0000018
alfa <-  0.03 
beta <- 0.95

nueva_volatilidad_GARCH <- sqrt(w + alfa*(rt^2) + beta*(0.15^2)) 
