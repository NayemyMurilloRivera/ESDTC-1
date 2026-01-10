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
