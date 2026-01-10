#insertar etiquetas 
y=factor(x,levels=c(1,2,3,4),labels=c("Sin estudios","Estudios primarios", "Estudios secundarios", "Estudios superiores"))
head(y) 

#creacion de tablas
fi=table(y)
hi = prop.table(fi)
pi= prop.table(fi)*100
tabla = cbind(fi,hi,pi)
tabla

#grafico de barras
barplot(fi)
gb=barplot(fi, main="Nivel de estudio", col=c("yellow", "red","blue", "green"), name=c("Sin estudios", "Estudios primarios", "Estudios secundarios", "Estudios superiores"), ylim=c(0, max(fi)*1.1))
text(x=gb, y=fi, labels=fi, pos=3) #print?

# Frecuencia  relativa Percentual       
porcentaje<-c(15,35,30,20)
etiqueta<-c(paste(porcentaje,"%",sep=""))
colores<-c("green","yellow","red","blue")
pie(porcentaje,labels=etiqueta,cloCkwise=TRUE,col=c("green","yellow","red","blue"), main = "Diagrama circular del Nivel de Estudio de los trabajadores")
legend("bottomright",c("Sin estudios", "Estudios primarios", "Estudios Secundarios", "Estudios superiores"), cex=0.6,fill=colores)
#puede moverse de ubicación la leyenda
legend("topright",c("Sin estudios", "Estudios primarios", "Estudios Secundarios", "Estudios superiores"), cex=0.6,fill=colores)


#EJEMPLO 1 GUIA 2

#TABLA SENCILLa

# Frecuencia Absoluta
frec_abs <- table(Art_defectuosos)
# Frecuencia Relativa
frec_rel <- prop.table(frec_abs)
# Frecuencia Porcentual
frec_porc <- prop.table(frec_abs) * 100
# Crear un data.frame con los resultados
resultados <- data.frame(Art_defectuosos = names(frec_abs),
                         fi = as.numeric(frec_abs),
                         hi = as.numeric(frec_rel),
                         pi = as.numeric(frec_porc))
# Imprimir la tabla de resultados
print(resultados)


#TABLA ACUMULADA

tabla_frecuencias <- as.data.frame(table(Art_defectuosos)) # RECUERDA QUE ESTO ES COMO LOS DATOS ALMACENADOS
colnames(tabla_frecuencias) <- c("Art_defectuosos", "fi")

# Calcular frecuencia relativa
tabla_frecuencias$hi <- tabla_frecuencias$fi / sum(tabla_frecuencias$fi)

# Calcular frecuencia porcentual
tabla_frecuencias$pi <- tabla_frecuencias$hi * 100

# Calcular frecuencias acumuladas
tabla_frecuencias$Fi <- cumsum(tabla_frecuencias$fi)
tabla_frecuencias$Hi<- cumsum(tabla_frecuencias$hi)
tabla_frecuencias$Pi <- cumsum(tabla_frecuencias$pi)

# Mostrar tabla final
print(tabla_frecuencias)


# 2.1 CREACION DE TABLA DE BASTONES

plot(i=1:length(frec_abs),frec_abs)
bp=plot(i=1:length(frec_abs),frec_abs,type = "h", col = "pink",
        xlab = "Número de articulos defectuosos", 
        ylab = "Número de lotes",
        ylim = c(0, max(frec_abs) * 1.1),
        main = "Gráfico 1.\n Número de articulos defectuosos por lote")
text(x=bp, y=frec_abs, labels=frec_abs, pos=3)


# 3. EJEMPLO: RANGO , AMPLITUD
# Hallar los intervalos
a<-min(Datos)
b<-max(Datos)
rango<-b-a
rango

#k=1+3.3*log10(length(Datos)) si no hay n final
#abajo en consola generamos amp, rango y se redondeo

intervalos <- seq(a, a+k*amp, by = amp) #que es seq
intervalos


#variable de tipo continua: histograma poligono y ojiva

#3.1 CREACION DEL HISTOGRAMA

#Grafico: histograma
histograma<-hist(Datos, breaks = intervalos, right = FALSE, col = "skyblue", 
                 main = "Histograma de las calificaciones de los alumnos en estadística",
                 xlab = "Nota", ylab = "Número de alumnos", border = "black")

# Agregar las frecuencias sobre las barras
text(x = histograma$mids, 
     y = histograma$counts, 
     labels = histograma$counts, 
     pos = 3, 
     col = "red", 
     cex = 0.8)  # Ajusta el tamaño del texto


#CREACION DE POLIGONO FREC 

#Para graficar polígono de frecuencias, debemos hallar los puntos medios
# Extraer puntos medios y frecuencias absolutas
puntos_medios <- histograma$mids  # Centros de los intervalos (Marca de clase)
frecuencias <- histograma$counts  # Frecuencias absolutas
# Agregar puntos iniciales y finales (cero para cerrar el polígono)
puntos_medios_ext <- c(min(puntos_medios) - diff(puntos_medios)[1], puntos_medios, max(puntos_medios) + diff(puntos_medios)[1])
frecuencias_ext <- c(0, frecuencias, 0) # de 0 en 0 
# Graficar el polígono de frecuencias
plot(puntos_medios_ext, frecuencias_ext, type = "o", col = "blue", lwd = 2, pch = 16,
     main = "Polígono de Frecuencias de las calificaciones de los alumnos en estadística",
     xlab = "Nota", ylab = "Número de alumnos")
# Opcional: Agregar líneas de referencia
grid(col = "gray", lty = "dotted")


#CREACION DE OJIVA 

#Para graficarla ojiva, debemos extraer los límites
# Extraer límites superiores e inferiores
limites_superiores <- histograma$breaks[-1]  # Excluye el límite inicial
frecuencias_acumuladas <- cumsum(histograma$counts)  # Frecuencias acumuladas
# Agregar punto inicial (cero para iniciar la ojiva)
limites_superiores_ext <- c(min(histograma$breaks), limites_superiores)
frecuencias_acumuladas_ext <- c(0, frecuencias_acumuladas)
# Graficar la ojiva
plot(limites_superiores_ext, frecuencias_acumuladas_ext, type = "o", col = "darkgreen", lwd = 2, pch = 16,
     main = "Ojiva de las calificaciones de los alumnos en estadística",
     xlab = "Nota", ylab = "Número de alumnos", ylim = c(0, max(frecuencias_acumuladas_ext)))
# Opcional: Agregar líneas de referencia
grid(col = "gray", lty = "dotted")


#EJEMPLO 6
#PARTE

# Hallar los intervalos
a<-min(peso)
b<-max(peso)
rango1<-b-a
rango1

intervalos <- seq(a, a+k*amp, by = amp) #que es seq
intervalos

tabla_frecuencia <- cut(peso, breaks = intervalos, include.lowest = TRUE,right = F)
frec_abs <- table(tabla_frecuencia)
df_frecuencia <- as.data.frame(frec_abs)
names(df_frecuencia) <- c("Intervalo", "fi")
df_frecuencia$hi <- prop.table(df_frecuencia$fi)
df_frecuencia$pi <- df_frecuencia$hi * 100
df_frecuencia$Fi <- cumsum(df_frecuencia$fi)
df_frecuencia$Hi <- cumsum(df_frecuencia$hi)
df_frecuencia$Pi <- cumsum(df_frecuencia$pi)
print(df_frecuencia)

histograma<-hist(peso, breaks = intervalos, right = FALSE, col = "skyblue", 
                 main = "Histograma de las calificaciones de los alumnos en estadística",
                 xlab = "Nota", ylab = "Número de alumnos", border = "black")

# Agregar las frecuencias sobre las barras
text(x = histograma$mids, 
     y = histograma$counts, 
     labels = histograma$counts, 
     pos = 3, 
     col = "red", 
     cex = 0.8)  # Ajusta el tamaño del texto



# Extraer puntos medios y frecuencias absolutas
puntos_medios <- histograma$mids  # Centros de los intervalos
frecuencias <- histograma$counts  # Frecuencias absolutas
# Agregar puntos iniciales y finales (cero para cerrar el polígono)
puntos_medios_ext <- c(min(puntos_medios) - diff(puntos_medios)[1], puntos_medios, max(puntos_medios) + diff(puntos_medios)[1])
frecuencias_ext <- c(0, frecuencias, 0)
# Graficar el polígono de frecuencias
plot(puntos_medios_ext, frecuencias_ext, type = "o", col = "blue", lwd = 2, pch = 16,
     main = "Polígono de Frecuencias de las calificaciones de los alumnos en estadística",
     xlab = "Nota", ylab = "Número de alumnos")
# Opcional: Agregar líneas de referencia
grid(col = "gray", lty = "dotted")

#installar en package "modeest"





