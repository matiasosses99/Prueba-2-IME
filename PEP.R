#lectura de datos
dir <- "~/../Downloads"
basename <- "datos-Forma02.csv"
file <- file.path(dir, basename)
poblacion <- read.csv(file = file, encoding = "UTF-8")
#pregunta 1

#pregunta: ¿existe diferencia significativa entre el promedio de la profundidad del pico 
#entre las especies?
#h0: la profundidad del pico promedio es igual entre las especies
#ha: la profundidad del pico promedio es diferente en al menos una de las especies

profundidad <- poblacion[,"Profundidad_pico"]
especie <- factor(poblacion[,"Especie"])
instancia <- factor(seq(1,50,by = 1))
datos <- data.frame(especie,profundidad)
datos <- datos[order(datos$especie),]
datos <- data.frame(instancia,datos)
#condicion 1 se cumple, La escala con que se mide la variable 
#dependiente tiene las propiedades de una escala de intervalos iguales. 

#comprobaciÃ³n de normalidad
g <- ggqqplot(datos,
              x = "profundidad",
              y= "especie",
              color= "especie")
g <- g + facet_wrap(~especie)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

#se cumple normalidad del punto 2
#las muestras son independientes entre si

#realizamos prueba de anova para muestras correlacionadas
prueba <- aov(profundidad~especie,data=datos)
anova <- ezANOVA(
  data = datos,
  dv = profundidad,
  between = especie,
  wid = instancia,
  return_aov=TRUE)

print(anova)
#ayuda a comprobar la condición 4


alfa <- 0.01

#prueba post_hoc
post_hoc <-TukeyHSD(prueba,
                    "especie",
                    ordered = TRUE,
                    conf.level = 1 - alfa)

print(post_hoc)

#conclusion: se falla al rechazar la hipotesis nula, por lo que existe al
#menos un promedio de profundidad del pico diferente entre las especies.
#gracias a la prueba de tukey se puede decir que existen diferencias 
#significativas solo entre la especie Adelia y Juanito.


#pregunta 2
#pregunta: ¿existe diferencia significativa entre el promedio de los pesos
#según la estacion?
#h0: el peso promedio es igual entre las estaciones
#ha: el peso promedio es diferente en al menos una de las estaciones

data <- poblacion[poblacion$Especie == "Barbijo",]

verano <- data$Peso_verano
invierno <- data$Peso_invierno
otono <- data$Peso_otono
primavera <- data$Peso_primavera

estaciones <- c(rep("Verano",length(verano)),
                rep("Invierno",length(invierno)),
                rep("Otono",length(otono)),
                rep("Primavera",length(primavera)))
pesos <- c(verano,invierno,otono,primavera)
instancia <- factor(seq(1,40,by = 1))

datos <- data.frame(instancia,estaciones,pesos)
#La escala con que se mide la variable dependiente tiene 
#las propiedades de una escala de intervalos iguales. 

#las muestras son independientes

#comprobaciÃ³n de normalidad
g <- ggqqplot(datos,
              x = "pesos",
              y= "estaciones",
              color= "estaciones")
g <- g + facet_wrap(~estaciones)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)
#se cumple la normalidad

#realizamos prueba de anova para muestras correlacionadas
prueba <- aov(pesos~estaciones,data=datos)
anova <- ezANOVA(
  data = datos,
  dv = pesos,
  between = estaciones,
  wid = instancia,
  return_aov=TRUE)

print(anova)
#ayuda a comprobar la condición 4

alfa <- 0.01

#prueba post_hoc
post_hoc <-TukeyHSD(prueba,
                    "estaciones",
                    ordered = TRUE,
                    conf.level = 1 - alfa)

print(post_hoc)

#conclusion: se rechaza la hipotesis nula, por lo que no existen
#diferencias significativas del peso promedio entre las estaciones
#gracias a la prueba de tukey se puede corroborar lo anterior 
#ya que ningun par de estaciones muestra una diferencia significativa