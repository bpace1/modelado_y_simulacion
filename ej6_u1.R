install.packages("tidyr")
library(ggplot2)
library(tidyr)

# Vector de Frecuencias Relativas de nacimientos por días del año.
p = c(rep(96, 61), 
      rep(98, 89), 
      rep(99, 62),
      rep(100, 61), 
      rep(104, 62), 
      rep(106, 30), 
      25) # Este 25 corresponde al 29/2 que sólo se da en años bisiestos.



## Ejercicio 6-1 ##

# Cantidad de elementos en p 
length(p);

# Frecuencia Relativa de un día en caso de su distribución sea uniforme
1/366;

# Distintas probabilidades posibles para la selección de día
unique(p / sum(p));

# Gráfico de las probabilidad de cada día
plot(1:366, p/sum(p), type = 'p', xlab = 'Día',
     ylab = 'Probabilidad', cex = .25, pch = 19)
abline(h = 1/366, lwd = 2, col ='red')


# Versión mejorada del gráfico
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



## Ejercicio 6-2 ## 

#set.seed(1237)       
N = 100000; n = 25                                  # Nro de repeticiones; n° personas en la habitación
coincidencias = coincidencias_p = numeric(N)        # Inicialización del vector "X: n° de coincidencias" para almacenar los resultados.


# Bucle con las N repeticiones de simulación
for (i in 1:N) 
{
  # Muestras del dia de cumpleaños de 25 personas
  sample_p = sample(x = 1:366, size = 25, repl = TRUE, prob =  p)   # Distribución no uniforme
  sample = sample(x = 1:366, size = 25, repl = TRUE)                # Distribución uniforme
  
  # Cáculo del Nro de coincidencias para ambos casos
  coincidencias_p[i] = n - length(unique(sample_p))
  coincidencias[i] = n - length(unique(sample)) 
  
}

# Proporción de simulaciones sin coincidencias de cumpleaños
mean(coincidencias_p == 0)
mean(coincidencias == 0)


probabilidad = 1 - mean(coincidencias_ == 0)
raiz = sqrt(probabilidad * (1 - probabilidad) / N)
v = c(-1.96, 1.96)

probabilidad + v * raiz

probabilidad_p = 1 - mean(coincidencias_p == 0)
raiz_p = sqrt(probabilidad_p * (1 - probabilidad_p) / N)

probabilidad_p + v * raiz_p

# Valor esperado del número de coincidencias de cumpleaños
mean(coincidencias_p)
mean(coincidencias)

probabilidad = 1 - mean(coincidencias_p == 0)

v = c(-1.96, 1.96)

raiz = sqrt(probabilidad * (1 - probabilidad) / N)
probabilidad + 1.96 * raiz
probabilidad - 1.96 * raiz
# p +- 1.96 sqrt(p*(1-p)/N)



# Frecuencias absolutas

# Creamos un dataset para mostrar las distintas frecuencias en ambas distribuciones
df_frec <- as.data.frame(table(coincidencias))
df_frec_p <- as.data.frame(table(coincidencias_p))

colnames(df_frec) <- c("Coincidencias", "Uniforme")
colnames(df_frec_p) <- c("Coincidencias", "No_Uniforme")

frecuencias <- merge(df_frec, df_frec_p, by = "Coincidencias", all = TRUE)

frecuencias$Coincidencias <- as.integer(as.character(frecuencias$Coincidencias))
frecuencias[is.na(frecuencias)] <- 0


# Convertir a formato largo para graficar
frecuencias_largas <- pivot_longer(frecuencias,
                                   cols = c("Uniforme", "No_Uniforme"),
                                   names_to = "Distribucion",
                                   values_to = "Frecuencia")

# Grafico de barras
ggplot(frecuencias_largas, aes(x = factor(Coincidencias), y = Frecuencia, fill = Distribucion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencias de la cantidad de coincidencias de cumpleaños",
       x = "Número de coincidencias",
       y = "Frecuencia",
       fill = "Distribución") +
  scale_fill_manual(values = c("Uniforme" = "skyblue", "No_Uniforme" = "salmon")) +
  theme_minimal()



### Anexo ###

# Varío el vector p con otra Frecuencia Relativa, a fin de probar otro caso sesgado
p_sesgo = c(rep(9,91), rep(1,275))
coincidencias_sesgo = numeric(N)
p_sesgo

for (i in 1:N)
{
  sample_sesgo = sample(x = 1:366, size = 25, repl = TRUE, prob =  p_sesgo)  
  coincidencias_sesgo[i] = n - length(unique(sample_sesgo))   
}

# Frecuencias absolutas
table(coincidencias_sesgo)

# Creamos un dataset para graficar
df_sesgo <- as.data.frame(table(coincidencias_sesgo))
colnames(df_sesgo) <- c("Coincidencias", "Frecuencia")
df_sesgo$Coincidencias <- as.integer(as.character(df_sesgo$Coincidencias))
df_sesgo$Distribucion <- "Sesgada"

# Unimos con el dataset ya existente
frecuencias_largas <- rbind(frecuencias_largas, df_sesgo)



probabilidad_sesgo = 1 - mean(coincidencias_sesgo == 0)
raiz_sesgada = sqrt(probabilidad_sesgo * (1 - probabilidad_sesgo) / N)

# de forma manual
probabilidad_sesgo + 1.96 * raiz_sesgada
probabilidad_sesgo - 1.96 * raiz_sesgada

# lo mismo pero con un vector


probabilidad_sesgo + v * raiz_sesgada



# Grafico de barras
ggplot(frecuencias_largas, aes(x = factor(Coincidencias), y = Frecuencia, fill = Distribucion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencias de la cantidad de coincidencias de cumpleaños",
       x = "Número de coincidencias",
       y = "Frecuencia",
       fill = "Distribución") +
  scale_fill_manual(values = c("Uniforme" = "skyblue", "No_Uniforme" = "salmon", "Sesgada" = "seagreen")) +
  theme_minimal()
