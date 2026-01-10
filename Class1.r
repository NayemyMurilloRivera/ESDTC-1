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

