install.packages("ggplot2")
library(ggplot2)

# Vector de Frecuencias Relativas de nacimientos por días del año.
p = c(rep(96, 61), 
      rep(98, 89), 
      rep(99, 62),
      rep(100, 61), 
      rep(104, 62), 
      rep(106, 30), 
      25) # Este 25 corresponde al 29/2 que sólo se da en años bisiestos.

## Ejercicio 6-1
# Escalado del vector
length(p); 1/366; unique(p / sum(p));

# Gráfico de las probabilidad de cada día
plot(1:366, p/sum(p), type = 'p', xlab = 'Día',
     ylab = 'Probabilidad', cex = .25, pch = 19)
abline(h = 1/366, lwd = 2, col ='red')


# Versión mejorada
df <- data.frame(
  dia = 1:366,
  prob = p / sum(p)
)

ggplot(df, aes(x = dia, y = prob)) +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 1/366, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribución de probabilidades de cumpleaños",
       x = "Día del año",
       y = "Probabilidad") +
  theme_minimal()



## Ejercicio 6-2
set.seed(1237)       
N = 100000; n = 25                                  # Nro de repeticiones; n° personas en la habitación
coincidencias = coincidencias_p = numeric(N)        # Inicialización del vector "X: n° de coincidencias" para almacenar los resultados.


for (i in 1:N) # for para la simulación
{
  # n fechas de cumpleaños al azar, con probabilidad p 
  sample_p = sample(x = 1:366, size = 25, repl = TRUE, prob =  p)  # distribución no uniforme
  sample = sample(x = 1:366, size = 25, repl = TRUE) # distribución uniforme
  
  # Nro de coincidencias en i-ésima iteración
  coincidencias_p[i] = n - length(unique(sample_p))   # cantidad de fechas distintas. la resta para saber cuantas coincidencias hay.
  coincidencias[i] = n - length(unique(sample)) 
  
}
 mean mean(x_p == 0); mean(x_p) (x_p == 0); mean(x_p) # Aproximación de P{X=0} y de E(X)

# Frecuencias absolutas
table(coincidencias)
table(coincidencias_p)  

# Frecuencias relativas, aprox. P{X=x}
prop.table(table(coincidencias))  
prop.table(table(coincidencias_p))  


barplot(prop.table(table(coincidencias_p)), col = 'skyblue',
       main = 'Distribución de coincidencias con probabilidad')

barplot(prop.table(table(coincidencias)), col = 'green',
        main = 'Distribución')


# Anexo: vario el vector p con otra frecuencia relativa, a fin de probar otro caso sesgado.
 p = c(rep(9,91), rep(1,275))

 set.seed(1237)       
N = 100000; n = 25                                  
coincidencias = coincidencias_p = numeric(N)        

for (i in 1:N)
{
  sample_p = sample(x = 1:366, size = 25, repl = TRUE, prob =  p)  
  sample = sample(x = 1:366, size = 25, repl = TRUE)
  
  coincidencias_p[i] = n - length(unique(sample_p))   
  coincidencias[i] = n - length(unique(sample)) 
}

# Gráficos
barplot(prop.table(table(coincidencias_p)), col = 'skyblue',
        main = 'Distribución de coincidencias con p muy sesgado')

