#!/usr/bin/env Rscript            
#==========================================================================
# Estadística elemental de conjunto de datos, a través de funciones
# internas de R.
# 
# Universidad Internacional de Valencia (VIU)
# Grado en Ciencia de Datos e Inteligencia Artificial.
# Asignatura: 03GIAR_Tema1_S#_P##. Curso octubre 2024-25

# Versión: 0.01
# Autor  : 2024@Lacruz, E. (VIU)
# Fecha I: 22/10/2024 
# Fecha F: 22/10/2024 

#==========================================================================
# Representación gráfica de datos con R. 
# Estadística descriptiva unidimensional

# 1. Diagramas de Sectores (Pie o Tarta).

# Generamos un vector.
library(RColorBrewer)  # Una buena elección de colores.
v1<- c(3,5,3,5,2)
v1
pie(v1,clockwise = TRUE,radius = 1.0)  # radius=1 es el máximo.
# Para generar el diagrama utilizamos la función "pie".
# Podemos construir un buen diagrama.

par(mfrow = c(1, 1))    # Si queremos varios paneles usamos c(1,2)  
pie(v1, labels = v1, col = rainbow(6), cex = 1) # Otros colores.

# Definamos otro vector y veamos sus % en el diagrama. 
v2<-c(17,41,12,51,30,09)

# Definimos un nuevo vector menor que 100.
v3<-c(19,15,12,12,18,11)

# Creamos las etiquetas con la función "round".
# Al ser v3 menor que 100 las partes proporcionales son diferentes. 
# Son casos especiales. 
etiquetas <- paste0(round(100 * v3/sum(v3), 2), "%")

par(mfrow = c(1, 2))     
pie(v3, labels = etiquetas, cex = 1)
pie(v3, labels = etiquetas,col = rainbow(6), cex = 1)
par(mfrow = c(1, 1)) # Volvemos a usar un solo panel para los gráficos.

# Un ejemplo de datos que son variables discretas pero no es ideal 
# este tipo de diagramas. Accedemos a una base de datos cualquiera. 
#==========================================================================

web1 <- 'https://raw.githubusercontent.com/fhernanb/datos/master/aptos2015'
datos <- read.table(file=web1, header=TRUE)   # leemos la Base de Datos.
datos        # Muestra los datos de la Base de Datos por consola.
dim(datos)   # Dimensión de la base de datos.

# Extraemos una sola columna y todas sus filas.
c4<- datos[,4]        # Elegimos la columna 4 variable "estrato".
c4                    # Nuestra Variable aleatoria elegida, 
#                       aunque podemos elegir otra disponible.
# Generamos el diagrama de sectores. 
pie(c4, labels = c4, col = rainbow(6), cex = 1)  # Utilizando colores
# ¿ Cómo podemos representar estos datos ?
c3<-datos[,2]        # Otra Variable aleatoria, varaible "mt2".
c3
# Generamos el diagrama de sectores. 
pie(c3, labels = c3, col = rainbow(6), cex = 1)  # Utilizando colores y %
# ¿Es bueno este tipo de gráfico para estos datos ?

# Utilicemos otra perspectiva, en este caso en 3D. 
# Haciendo uso de una Base de Datos (BD) interna de R, Lock5Data.  
# Asignaremos una varaible de la BD a un objeto que llamaremos "ta" 
library(plotrix)           # Librería para pintar en 3D Sectores
library(Lock5Data)         # Librería que podemos instalar. 
data(HappyPlanetIndex)     # Medidas relacionadas con la felicidad de 143
                           # países.
attach(HappyPlanetIndex)   # busca en la base de datos al evaluar una 
# variable, por lo que se puede acceder a los objetos de la BD 
# simplemente dando sus nombres.


# Entonces, buscamos la variable "Region" y la asignamos a "ta".
ta<-table(Region)   # Con formato de la clase table.

# Creamos etiquetas con 2 decimales.
etiquetas <- paste0(round(100 * ta/sum(ta), 3), "%")
pie3D(ta,labels = etiquetas) # Pintamos las proporciones en 3D.
legend("topright", etiquetas, cex = 0.6,
       fill = rainbow(length(ta))) # Apreciar si no obstruye información.
# Podemos crear diagramas de sectores para una variable
# en 3D, por porcentajes, por colores.

# Vamos a crear otro vector asignandolo a otro objeto "vv".
vv<-c(6,8,2)
etiquetas <-c("Neutrino", "Potrón", "Electrón")
pie3D(vv,labels = etiquetas)
legend(x = "topright", legend = c("Neutrino", "Potrón", "Electrón"), 
       fill = c("blue", "red", "green"),title = "Leyenda",
       cex = 0.6)

#¿Cómo podemos generar el mismo gráfico pero colocando una leyenda?

#===============================================================================

# 2. Pictogramas. 

# Para la representación de estos gráficos haremos lo siguiente:
# 1.- Creamos un vector (por país y puntaje)
# 2.- Definimos dos vectores.
# 3.- Haremos uso de tres librerías. 
library(tidyverse)   # 
library(echarts4r)   # 
library(highcharter) # 

# Etiquetas.
pais<-c("Bélgica", " Eslovenia", "Francia ", "Italia ", "UK", 
        "Pasises Bajos ", "España ", "Dinamarca ", " Astralia", 
        " Colombia")
# Puntajes.
nbike<-c(14349.33,11993,11541.67,10851,9960.6,9808.66,7979,
         7911.6,7001.66,6796)  
ibike<-"https://i.etsystatic.com/5421899/r/il/30bb7f/814956732/il_1588xN.814956732_k71a.jpg"

# Buscamos un pictograma que nos represente la variable y definimos un 
# objeto "imgB" con la imagen. Eso si, debe ser del mismo tamaño de los 
# otros objetos "pais" y "nbike". 
imgB = c(paste0("image://", ibike),paste0("image://", ibike),
         paste0("image://", ibike),paste0("image://", ibike),
         paste0("image://", ibike),paste0("image://", ibike),
         paste0("image://", ibike),paste0("image://", ibike),
         paste0("image://", ibike),paste0("image://", ibike))

posicion<- data.frame(pais,nbike,imgB)   # Función data.frame
# Tras ejecutar la siguiente línea nos muestra lo que hemos construido. 
posicion %>%                        # %>% un operedor de concatenación.
  e_charts(pais) %>%                # usamos echarts4s. 
  e_pictorial(nbike,                # Valor e imagen proporcional.
              imgB) %>%
  e_theme("Posición") %>% 
  e_legend(F) %>% 
  e_title("Posición Tour 2021", left='center') %>%
  e_labels(show=TRUE) %>%
  #  e_x_axis(splitLine=list(show = T)) %>%    #probar eje x con este.
  e_x_axis(show= T, axisLabel = list(interval = 0, rotate = 45)) %>%
  e_y_axis(show= T, 
           min=0, max= 14600, 
           interval= 1500, 
           splitLine=list(show = T))
# Buscar en la red una imagen del tour y cambiar la que esta.

# Otro Ejemplo, en este caso sobre el valor de Bancos. 
# Sobre 6 elementos del conjunto total.
ac<-c("SAN", "BBVA", "CABK", "IAG", "TEF", "IBE")  # Etiquetas.
acg<-c(2.6895, 4.59, 3.007, 1.6925, 4.770, 10.815) # Valores.
# Imagen Santander
im1<- "https://brandemia.org/sites/default/files/inline/images/simbolo_
antes_despues.gif"
# Imagen BBVA
im2<- "https://brandemia.org/sites/default/files/inline/images/
bbva_logo_portada.jpg"
#Imgaen Caixa
im3<- "https://uh.gsstatic.es/sfAttachPlugin/getCachedContent/id/
328926/width/346/height/250 "
#Imgaen IAG
im4<- "https://www.moneycontroller.es/upload/aziende/iag-i
nternational-consolidated_20210129115358.png"
#Imgaen TEF
im5<- " https://i.blogs.es/5dd137/logo-telefonica/1366_2000.jpg"
#Imagen IBER
im6<-"https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/
Logo_Iberdrola_compacto_%282023%29.svg/92px-Logo_Iberdrola_
compacto_%282023%29.svg.png"

imT<-c(paste0("image://", im1),paste0("image://", im2),
       paste0("image://", im3),paste0("image://", im4),
       paste0("image://", im5),paste0("image://", im6))
accion<- data.frame(ac,acg,imT)   # Función data.frame
accion %>%                         # %>% un operedor de concatenación.
  e_charts(ac) %>%                  # usamos echarts4s. 
  e_pictorial(acg,                  # Valor e imagen proporcional.
              symbol =  imT,          # Cambiamos estos parámetros para mejor
              symbolRepeat = T,       # el diagrama.
              symbolSize = c(40, 40)) %>%
  e_theme("Acción") %>% 
  e_legend(F) %>% 
  e_title("Acciones ganancia 2022", left='center') %>%
  e_x_axis(splitLine=list(show = T)) %>%    #probar eje x con este.
  #  e_x_axis(show= T) %>%
  e_y_axis(show= T, 
           min=0, max= 14.0, 
           interval= 1, 
           splitLine=list(show = T)) %>%
  e_labels(fontSize = 12, 
           fontWeight ='bold',        # Parámetros sobre las etiquetas.    
           position = "upper", 
           offset=c(0, -20), 
           color= "orange")

# Apreciamos otra forma de visualizar los datos según los elementos
# de la varaible, en este caso bancos. Usamos el pictograma, ya que
# los logotipos suelen ser bien estudiados visualmente y fundamentalmente
# se conocen, se saben donde están sin saber sus nombres. 
#==========================================================================

# 2. Diagrama de barras.
# Para ello vamos a cosntruir un data.frame concatenando las etiquetas, 
# valores de un a web y lo pintamos. 

# Definición de clases por asignaturas es nuestra variable discreta.
# Catedráticos universitarios (chicas), 2017,
#https://www.rsme.es/2019/02/mujeres-y-matematicas-cientificas-en-cifras-2017/
# Sobre una variable discreta. Catedráticos Universitarios. 

# Etiquetas.
asig<-c("Álgebra", "Análisis Math","Estadística","Geometría","Mat. 
        Applicada")
# Puntajes.
nper<-c(10,5,25,3,26)
# Agrupamos en una matriz o tabla.
cu <- data.frame(asig,nper)
# Lo más básico para generar las barras.
barplot(height=cu$nper, names=cu$asig, las=2)# Rotamos etiquetas con "las"

# Algo más elaborado y prolijo. Lo que debe ser para presentar. 
barplot(height=cu$nper, names=cu$asig,las=2,
        main="Catedráticos Universitarios 2017", #Título 
        ylab="# CU Chicas",
        #        xlab="Asignatura",
        ylim=c(0,30),
        col=c("red", "blue", "orange",  "black", "grey"),
        legend =rownames(cu),
        density=c(3,9,17,35,19), 
        angle=c(10,25,60,41,80), 
)

# ¿Podemos pintar diagramas de barras para una variable continua?
# R: 
#==========================================================================

# 2.1 Diagrama de barras agrupados.
# Definición de clases 1er y 2do trimestre 2020.
# Son variables cuantitativas numéricas sin orden. Solo indican 
# frecuencias de ventas por trimestre según la marca de coche.
mc<-c("Ford", "Opel","BMW","Seat","Fiat")
nv1<-c(100,50,25,293,260)        # Ventas 1er trimestre 2020
nv2<-c(130,55,50,150,233)        # Ventas 2do trimestre 2020
# Agrupamos en una matriz o tabla.
d1 <- data.frame(mc,nv1)
d2 <- data.frame(mc,nv2)
# Lo más básico para generar las barras.
barplot(height=d1$nv1, names=d1$mc, las=1)   # las=1 horizontal las etiqueteas.
barplot(height=d2$nv2, names=d2$mc, las=1)   # las=1 horizontal las etiqueteas.
#       Algo más elaborado.

# Diagrama de barras 1er trimestre.
barplot(height=d1$nv1, names=d1$mc,las=1,
        main="Ventas de Coches 1er Trimestre, 2020", #Título 
        ylab="# NV Coches",
        #        xlab="Marca",
        ylim=c(0,300),
        col=c("red", "blue", "orange",  "black", "grey"),
        #        legend =rownames(d1),
        #        density=c(3,9,17,35,19), 
        #        angle=c(10,25,60,41,80), 
)
# Diagrama de barras 2do trimestre.
barplot(height=d2$nv2, names=d2$mc,las=1,
        main="Ventas de Coches 2do Trimestre, 2020", #Título 
        ylab="# NV Coches",
        #        xlab="Marca",
        ylim=c(0,300),
        col=c("red", "blue", "orange",  "black", "grey"),
        #        legend =rownames(d1),
        #        density=c(3,9,17,35,19), 
        #        angle=c(10,25,60,41,80), 
)
#==========================================================================
# Utilizaremos la librería AER. Contine más de 100 bases de datos.
# Otra forma de ser instalada vía comandos. En caso que no funcione INSTALL.

if(!require("AER")) {install.packages("AER")}
dt<-data(package="AER")
dt                      # Visualizamos las bases de datos disponibles
library(AER)           # Cargamos la librería. Applied Econometrics with R

# Para este ejemplo usamos CPSSW9298 (Stock and Watson CPS Data Sets)
data(CPSSW9298)
dim(CPSSW9298)    #1319 medidas
names(CPSSW9298)  # 12 variables.

attach(CPSSW9298)    # función para acceder a las variables.
gd <- table(year,gender) 

barplot(gd,
        beside = T,                   # para agrupar las barras
        col = c("pink", "black"),     # Color por nombre.
        names.arg= levels(gender),
        legend=levels(year),
        ylim = c(0,5000),
        #        xlim = c(0,550)
)

#==========================================================================
# 2.2 Diagrama de barras apiladas.
# Definimos una nueva tabla con la base de datos anterior.
# Comparamos las frecuencias de las variables "año" y "edad".

ga <- table(year,age) 
barplot(ga,
        beside = F,                   # para agrupar las barras
        col = c("#ff9900","#0040ff"), # colores por códigos
        names.arg= levels(age),       # etiquetas
        legend=levels(year),          # leyenda.
        ylim = c(0,2000),             # límites eje Y
        #xlim = c(20,70)
)

#==========================================================================
# 2.3 Diagrama de Pareto. Para dar soluciones a problemas.

library(qcc) #Librería Quality Control Charts & Statistical Process Control

#Construimos nuevos datos. 
# Supongamos una encuesta realizada a personas de Amazon Prime.
# ¿ Cuál ha sido su mejor año de trabajo en escala del 1 al 100 ?
Personas<- c (20,12, 97, 8, 32, 70, 46)
#Personas<- order(Personas)
names(Personas)<-c ('2009', '2010', '2011', '2012', '2013',
                    '2014','2015')

df <- data.frame(Personas)
pareto.chart(Personas, xlab = "Años",       # Etiqueta eje x.
             ylab="Frecuencia",             # Etiqueta eje Y (izq)
             col=heat.colors(length(df)),   # Color
             cumperc = seq(0, 100, by = 5), # Rango de % (drc)
             ylab2 = "% Acumulado",         # Etiqueta (drc)
             main = "Diagrama de Pareto"    # Título
)             
# ¿ Qué nos muestra el diagrama de Pareto?
# R: Facilita el uso de posibles causas y establece prioridades, 
#   según el fenómeno estudiado. Se coloca a la izquierda (pocos vitales) y
# derecha (muchos triviales) indicando que habrá muchos problemas sin 
# importancia frente a pocos importantes. 

#==========================================================================
# 3. Histogramas para variables continuas. 
# Nos muestra la distribución, forma y extensión de los valores que toma
# una variable. 

# Usamos para este ejemplo CigarettesSW (Cigarette Consumption Panel Data)
data(package="AER")
data(CigarettesSW)
dim(CigarettesSW)    #96 medidas
names(CigarettesSW)  # 9 variables.

attach(CigarettesSW)    # función para acceder a las variables.

hist(population,
     #breaks = 2, 
     main = "Distribución de la población", 
     ylab = "Frecuencia",
     xlab = "Problación"
)

# Pocas barras.
hist(tax, breaks = 2, main = "Pocas clases", 
     ylab = "Frecuencia",
     col="red")
# Exceso de barras. Datos atípicos.(outlier)
hist(tax, breaks = 60, main = "Demasiadas clases",
     ylab = "Frecuencia",
     col="green")
# Una forma ideal.
hist(tax, main = "Método de Sturges", 
     ylab = "Frecuencia",
     col="orange")

# Utilizar el método plug-in (Wand 1995) y comparar con el de Sturges
# Cargar la librería: library(KernSmooth) (Instalar si es necesario).

#==========================================================================
# 4. Polígono de frecuencias. 
# Representación para datos agrupados.
# Uso para datos cruzados con una conclusión generalizda.

# Definamos un conjunto de datos.
# Central de intervalos determinados por regla de la Raíz.
x=c(2,4,8,10,12,14,16,18,20,22)
# Frecuencias
y=c(3,5,2,14,13,25,1,8,10,32)
plot(x,y,type="p",pch=20,lty=1,
     xlab="Ciertas clases)",
     ylab="Sus frecuencias",
     main="El polígono",
     xaxt="n",
     yaxt="n",
     ylim=c(0,33))
axis(side=1,x,labels=TRUE)
axis(side=2,c(0,3,6,9,12,15,18,21,24,27,30,33),labels=TRUE,las=2) 
lines(x,y)      # Generarmos las lineas para el polígono.

# ¿ Cómo podemos generar un histrograma y colocar encima su
# polígono de frecuencias? ¿ Qué nos indican ambos ?

hist(y, main = "Método de Sturges", 
     ylab = "Frecuencia",
     col="grey")

#==========================================================================
#==========================================================================
# MEDIDAS DE TENDNCIA CENTRAL, DISPERSIÓN y FORMA

# 1. Tendencia central.
# Generamos un conjunto de datos. 
# Se pueden utilizar bases de datos ya existentes.

#==========================================================================
# Vector que ejemplifica un gasto, en $, por hora de 25 empresas.
v0=c(1670,2060,1710,2620,1710,2620,3670,2200,2000,2130,3350,1900,
     2180,2490,1850,2260,1540,2120,2310,2010,1040,1940,3030,1130,
     2070,2410,1930) 
v0
# Un histograma básico;
hist(v0,main = "Distribución de gasto por hora", ylab = "Frecuencia")

# Cálculo de medidas centrales 
mean(v0)     # Media aritmética
# Cambio de valores. a=1000 b=10
v0p<-(v0-1000)/10
v0p
# Media artimética de la muestra cambiada.
mean(v0p)
# Nueva media aritmética.
mean(v0p)*10+1000

# Un ejercicio Práctico.
# Calcular la media aritmética y la cambiada.
# Definir un valor de b1 y a1 adecuados. 
# Un vector de datos de una población.
n=10000                      # Numéro de puntos
# Generamo un vector con números aleatorios.
v0=c(sample(10000:999999,n,replace=T)) # Usamos la función "sample"  
v0
length(v0)            # Para conocer el tamaño de valores del vector
# Un histograma sencillo;
hist(v0, breaks=20,main = "Distribución de la variable", 
     ylab = "Frecuencia",ylim=c(0,600),
     col="orange")
mean(v0)
# Cambio de valores. a=1000 b=10
v0p<-(v0-b1)/a1
v0p
# Media artimética de la muestra cambiada.
mean(v0p)
# Nueva media aritmética.
mean(v0p)*b1+a1
# Responder:
#¿Es mejor o peor el cambio?

#==========================================================================
# Estadístico de la Media Ponderada.
# Utilizaremos la función weighted.mean del paquete "base"
# Definimos un vecotr y sus pesos. (pueden venir dados en una tabla)

pe <- c(6,  3,  7,  2, 5, 8)/30        #Recordar la porpiedad suma=1.
v1 <- c(2.7, 3.2, 5.5, 9.8,2.1,3.0)  # Datos 
xp <- weighted.mean(v1, pe)          # Media ponderada
xp
mean(v1)                      # Media aritmética.
hist(v1, breaks=4, main = "Distribución de la variable v1", 
     ylab = "Frecuencia", col="purple")
sum(pe)
#==========================================================================
# Estadístico de la Mediana. Variable discreta.
v2 <- c(5,6,3,1,8,9,4)

# Un diagrama de barras;
barplot(v2,main="Distribución de la variable discreta", #Título 
        ylab="Frecuencia",ylim=c(0,10),
        col=c("red", "blue", "orange",  "black", "grey",
              "green","purple"),legend =rownames(v2),
        density=c(3,9,17,35,19), angle=c(10,25,60,41,80,18,67))

# Estadístico de la Mediana. Variable continua.
set.seed(1)
v3 <- rnorm(1000)

# Un histograma sencillo;
hist(v3, main = "Distribución de la variable continua", 
     ylab = "Frecuencia",col="green")

#==========================================================================
# Estadístico de la Moda
# Definimos un vector de datos
# Instalamos la siguiente librería.
# "lientz", "naive", "venter", "grenander", "hsm", "parzen", "tsybakov" 
# y "asselin".

library(modeest)   
v4<-c(30,9,21,22,43,24,29,26,28,55,30,32,33,37,35,30,23,65,51,45,25)
hist(v4, col="cyan",main="Distribución de una variable",ylab="Frecuencia")   
mlv(v4, method = "mfv")   # 30. # Indagar que es el método mfv.

# Estadístico multimodal. Variable discreta.
# Definición de datos que representan cierta variable. 
v5 <- c(3, 5, 3, 8, 5, 6, 5,5,8,5,7,8,8,8)
hist(v5, col="pink",main="Distribución de una variable discreta",
     ylab="Frecuencia")
# Cálculo de las Moda múltimodal
mlv(v5, method = "mfv")   # 5 y 8.

#Estadístico multimodal. Variable continua.
# Definición de datos que representan cierta variable. 
n <- 200
bin <- rbinom(n, 1, 0.4)  #Función de distribución binomial
v6 <- rnorm(n, mean = 120, sd = 11) * bin +
  rnorm(n, mean = 40, sd = 5) * (1 - bin)  # Distribución normal.
# Histograma
hist(v6, col="blue",main="Distribución de una variable continua",
     ylab="Frecuencia")
# Moda teórica 1
abline(v = 40, col = 3, lwd = 2) # Linea sobre la distribución
# Moda teórica 2
abline(v = 120, col = 6, lwd = 2)

# Cargamos librería multimoda.
library(multimode)
modas <- locmodes(v6, mod0 = 2) # Localiza las modas
modas
plot(modas)                     # Uso del plot

#==========================================================================
#==========================================================================

# 2. Medidas de Dispersíon.
# Generamos un conjunto de datos. 
# Se puede utilizar bases de datos.

n=1000                      # Numéro de puntos
#Generamo un vector con números aleatorios.
v0=c(sample(7:9999,n,replace=T)) 
v0
# Un histograma sencillo;
hist(v0, breaks=100,main = "Distribución de la variable", 
     ylab = "Frecuencia",ylim=c(0,20),
     col="brown")
# 1.2 Cálculo del rango.
rg=max(v0)-min(v0)
rg

mean(v0)     # Media aritmética
median(v0)   # Mediana   
sd(v0)       # Desviación estándar
var(v0)

#==========================================================================
# Estadístico de la varianza.
v1 <- c(5,6,3,1,8,9,4,6,12,13,20,50,39)
hist(v1, breaks=5, main = "Distribución de la variable discreta", 
     ylab = "Frecuencia", col="purple")

mean(v1)     # Media aritmética
median(v1)   # Mediana
rg=max(v1)-min(v1)  #Rango
rg
var(v1)           # Varianza    

#==========================================================================
# Estadístico de la varianza. Para una población.
n=1000                      # Numéro de puntos
v0=c(sample(100:99999,n,replace=T)) # Generamo un vector con números aleatorios.
v0
# Un histograma sencillo;
hist(v0, breaks=100,main = "Distribución de la variable", 
     ylab = "Frecuencia",ylim=c(0,20),
     col="orange")
# 1.2 Cálculo del rango.
rg=max(v0)-min(v0)
rg

mean(v0)     # Media aritmética
median(v0)   # Mediana   
sd(v0)       # Desviación estándar
var(v0)
mean(v1)     # Media aritmética
median(v1)   # Mediana
rg=max(v1)-min(v1)  #Rango
rg
var(v1)           # Varianza    

#==========================================================================
# Coeficiente de Person.
# Generamos dos funciones de distribución. 
# Numéro de puntos
v4=c(sample(100:999,300,replace=T))   # Generar números aleatorios enteros.
v5=runif(300,min=100,max=999)
v5<-c(2,3,4,5,6,7,8,912,2,34,45,123,450,200)
v5<-c(12,13,14,15,26,37,8,912,32,344,465,123,40,20)

par(mfrow = c(1, 2))    # Varios paneles
# Un histograma sencillo;
hist(v4,main = "Distribución de la variable V4", 
     ylab = "Frecuencia",ylim=c(0,50),
     col="red")
hist(v5,main = "Distribución de la variable V5", 
     ylab = "Frecuencia",ylim=c(0,50),
     col="green")
mean(v4)       #Media 
mean(v5)
sd(v4)         # Desviacíon estándar
sd(v5)

#==========================================================================
#==========================================================================
# 3. Mdedidas de Formas.
# Distribuciones simétricas.

par(mfrow = c(1, 1))      # Para volver a un solo gráfico
# Generamos una distribución normal. N(0,1), con 100 valores.
n1<-rnorm(1000, mean = 0, sd = 1)   # Media=mean, Desciación=sr.
# Cosntruimos uns función de distribución.

# Mallado para una distribución normal no estándar
x <- seq(-10, 10, length = 250)
# Semilla para que la salida sea reproducible.
set.seed(1)
# Función de densidad
fd<-dnorm(x) 
hist(n1, main = "Distribución normal", xlab = "", ylab="Densidad",
     prob = TRUE, col="blue",ylim = c(0,0.44))
lines(x, fd, col = "red", lwd = 5)   # Pintar la Función de densidad.
abline(v = -1, col = 3, lwd = 5) # Linea de asimetría Izq
abline(v =  1, col = 2, lwd = 5) # Linea de asimetría Drc
abline(v =  0, col = 7, lwd = 5) # Linea de simetria.
#==========================================================================
# Medidas de Formas.
# Coeficiente de Fisher.1.1 asimetría positiva. 
# Generamos una distribución normal. N(3.26,3), con 100 valores.
n1<-rnorm(1000, mean = 3.26, sd = 40)   # Media=mean, Desciación=sr.
# Cosntruimos uns función de distribución.

# Mallado para una distribución normal no estándar
x <- seq(-10, 10, length = 250)
# Semilla para que la salida sea reproducible.
set.seed(4)
# Función de densidad
fd<-dnorm(x) 
hist(n1, main = "Distribución asimetríca positival", xlab = "", 
     ylab="Densidad",
     prob = TRUE, col="blue",ylim = c(0,0.01))
#lines(x,fd,col='red',lwd=3)
abline(v = 2.890587, col = 'red', lwd = 5) #curotsis
library(moments)   # Libreria para el calculo de los momentos.
skewness(n1)
kurtosis(n1)

# Distribución asimetríca negativa
n2<-rnorm(1000,mean=50)  
hist(n2, main = "Distribución asimetríca negativa", xlab = "", 
     ylab="Densidad",
     prob = TRUE, col="grey",ylim = c(0,0.48))
abline(v = 0, col = 5, lwd = 5) # Linea de asimetría Izq
skewness(n2)             # Para calcular la asimetría.
kurtosis(n2)

#==========================================================================
# Mdedidas de Formas.
# Coeficiente de Fisher.1.2 Curtosis (apuntalamiento). 
# Generamos una distribución normal.
cur1<-rnorm(100,mean=18.34)  
hist(cur1, main = "Distribución normal", xlab = "", ylab="Densidad",
     prob = TRUE, col="pink",ylim = c(0,0.48))
skewness(n2)             # Para calcular la asimetría.
kurtosis(n2)

#==========================================================================
#========= FIN DEL SCRIPT =================================================
