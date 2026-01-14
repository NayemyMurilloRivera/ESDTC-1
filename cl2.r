> library(readxl)
> DatosPC2 <- read_excel("C:/Users/ALUMNO-P103/Downloads/DatosPC2.xlsx")
> View(DatosPC2)
> modelaje<-c(790.4, 800.6, 804.6, 774.7, 811.3, 796.7, 824.5, 798.1, 796.7, 805.4, 799.3, 784.4, 802.9, 801.9, 797.1, 788.1, 812.9, 810.2, 802.4, 805.3)
> modelaje
 [1] 790.4 800.6 804.6 774.7 811.3 796.7 824.5 798.1 796.7 805.4 799.3 784.4 802.9 801.9 797.1 788.1 812.9 810.2 802.4 805.3
> media<-mean(modelaje)
> media
[1] 800.375
> sangre<-c(7.32, 7.34, 7.40, 7.28, 7.29, 7.35, 7.33, 7.34, 7.28, 7.31, 7.35, 7.32, 7.33, 7.36, 7.32, 7.34, 7.31, 7.35, 7.36, 7.26, 7.39, 7.29, 7.32, 7.34, 7.30, 7.34, 7.32, 7.30, 7.33, 7.33, 7.35, 7.34, 7.33, 7.36, 7.33, 7.35, 7.31, 7.26, 7.39, 7.35)
> sangre
 [1] 7.32 7.34 7.40 7.28 7.29 7.35 7.33 7.34 7.28 7.31 7.35 7.32 7.33 7.36 7.32 7.34 7.31 7.35 7.36 7.26 7.39 7.29 7.32 7.34 7.30 7.34 7.32 7.30 7.33 7.33 7.35 7.34
[33] 7.33 7.36 7.33 7.35 7.31 7.26 7.39 7.35
> maxima<-quantile(sangre, 0.85)
> maxima
   85% 
7.3515 
> comun<-mfv(DatosPC2$REGION)
Hubo 21 avisos (use warnings() para verlos)
> mfv(DatosPC2$REGION)
[1] "costa"
> summary(DatosPC2$TALLA)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  149.0   159.0   167.0   165.9   171.0   177.0 
> mediana<-median(DatosPC2$TALLA)
> mediana
[1] 167
> varianza<-var(DatosPC2$TALLA)
> variazna
Error: objeto 'variazna' no encontrado

> varianza
[1] 48.2149
> sd(DatosPC2$TALLA)
[1] 6.943695
