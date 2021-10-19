#Encuesta a turistas de Pinto#

# Observación: es importante para evitar confusiones, que al ejecutar una rutina se limpie el espacio de trabajo
# por si hubieran datos precargados, el comando para ello es el siguiente: 

rm(list = ls())


## Observación: Sobre la carga de paquetes, lo mejor es hacer una función que cargue el paquete en caso de que 
## esté instalado o que si no lo está, que lo instale de inmediato y lo vuelva a cargar 

# La función que presento a continuación permite realizar este cometido: 

load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

# De esta manera la forma correcta de cargar (e instalar un paquete) es: 

# Nota que cargaré el paquete tidyverse, en lugar de dplyr, porque de hecho tidyverse es un paquete de otros paquetes similares

paquetes = c("tidyverse", "sjPlot")

load_pkg(paquetes)

## Observación la carga de los datos siempre debe estar disponible en la misma rutina de cálculo,
## sino se cae en el paseo innecesario de cargar manualmente los datos una y otra vez. 
## La forma correcta de hacerlo es: 

encuesta_turistas = readxl::read_excel("20211013_Encuesta a Turistas Pinto modificada.xlsx")


names(encuesta_turistas)

#Encuesta a turistas de Pinto#
rm(list = ls())

library(dplyr)
library(tidyverse)
library(sjPlot)
library(ggplot2)
encuesta_turistas = readxl::read_excel("20211013_Encuesta a Turistas Pinto modificada.xlsx")



##ESTADISTICA DESCRIPTIVA##

## 1.Género##

genero = encuesta_turistas %>%
  group_by(Género) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))


# Gráfico de barras con ggplot
p_1.1 = genero %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Género`, y = prop, fill = `Género`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Para guardar el gráfico: 
ggsave("p_1.2.png", p_1)

# Gráfico de torta
p_1.2 = genero %>% 
  arrange(desc(`Género`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Género`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Para guardar el gráfico: 
ggsave("p_1.2.png", p_2)


# Para crear una tabla compatible con word: 
tab_df(genero, file = "cuadro_genero.doc")
        

##2. Edad##
edad = encuesta_turistas %>%
  group_by(Edad) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_2.1 = edad %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Edad`, y = prop, fill = `Edad`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_2.1.png", p_2.1)

# Gráfico de torta
p_2.2 = edad %>% 
  arrange(desc(`Edad`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Edad`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_2.2.png", p_2)
plot(p_2.2)

#Tabla word: 
tab_df(edad, file = "cuadro_edad.doc")




##3. ¿A qué se dedica actualmente?##

ocupacion = encuesta_turistas %>%
  group_by(`¿A qué se dedica actualmente`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_3.1 = ocupacion %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `¿A qué se dedica actualmente`, y = prop, fill = `¿A qué se dedica actualmente`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_3.1.png", p_3.1)

# Gráfico de torta
p_3.2 = ocupacion %>% 
  arrange(desc(`¿A qué se dedica actualmente`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿A qué se dedica actualmente`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_3.2.png", p_3.2)

#Tabla word: 
tab_df(ocupacion, file = "cuadro_ocupacion.doc")



##4. Indique el rango de ingresos que percibe su núcleo familiar##

ingresos = encuesta_turistas %>%
  group_by(`Indique el rango de ingresos que percibe su núcleo familiar`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_4.1 = ingresos %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Indique el rango de ingresos que percibe su núcleo familiar`, y = prop, fill = `Indique el rango de ingresos que percibe su núcleo familiar`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_4.1.png", p_4.1)

# Gráfico de torta
p_4.2 = ingresos %>% 
  arrange(desc(`Indique el rango de ingresos que percibe su núcleo familiar`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Indique el rango de ingresos que percibe su núcleo familiar`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_4.2.png", p_4.2)

#Tabla word: 
tab_df(ingresos, file = "cuadro_ingresos.doc")



##5. Nº de niños (de 0 a 12 años)]##

Nniños = encuesta_turistas %>%
  group_by(`Nº de niños (de 0 a 12 años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_5.1 = Nniños %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de niños (de 0 a 12 años)]`, y = prop, fill = `Nº de niños (de 0 a 12 años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_5.1.png", p_5.1)

# Gráfico de torta
p_5.2 = Nniños %>% 
  arrange(desc(`Nº de niños (de 0 a 12 años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de niños (de 0 a 12 años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_5.2.png", p_5.2)

#Tabla word: 
tab_df(Nniños, file = "cuadro_Nniños.doc")

##6. Nº de niñas (de 0 a 12 años)]##

Nniñas = encuesta_turistas %>%
  group_by(`Nº de niñas (de 0 a 12 años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_6.1 = Nniñas %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de niñas (de 0 a 12 años)]`, y = prop, fill = `Nº de niñas (de 0 a 12 años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_6.1.png", p_6.1)

# Gráfico de torta
p_6.2 = Nniñas %>% 
  arrange(desc(`Nº de niñas (de 0 a 12 años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de niñas (de 0 a 12 años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_6.2.png", p_6.2)

#Tabla word: 
tab_df(Nniñas, file = "cuadro_Nniñas.doc")




##7. Nº de adolescentes varones (de 13 a 17 años)]##

adolecentesv = encuesta_turistas %>%
  group_by(`Nº de adolescentes varones (de 13 a 17 años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_7.1 = adolecentesv %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de adolescentes varones (de 13 a 17 años)]`, y = prop, fill = `Nº de adolescentes varones (de 13 a 17 años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_7.1.png", p_7.1)

# Gráfico de torta
p_7.2 = adolecentesv %>% 
  arrange(desc(`Nº de adolescentes varones (de 13 a 17 años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de adolescentes varones (de 13 a 17 años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_7.2.png", p_7.2)

#Tabla word: 
tab_df(adolecentesv, file = "cuadro_adolecentesV.doc")




##8. Nº de adolescentes damas (de 13 a 17 años)]##

adolecentesd = encuesta_turistas %>%
  group_by(`Nº de adolescentes damas (de 13 a 17 años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_8.1 = adolecentesd %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de adolescentes damas (de 13 a 17 años)]`, y = prop, fill = `Nº de adolescentes damas (de 13 a 17 años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_8.1.png", p_8.1)

# Gráfico de torta
p_8.2 = adolecentesd %>% 
  arrange(desc(`Nº de adolescentes damas (de 13 a 17 años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de adolescentes damas (de 13 a 17 años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_8.2.png", p_8.2)

#Tabla word: 
tab_df(adolecentesd, file = "cuadro_adolecentesD.doc")




##9. Nº de adultos hombres (de 18 a 59 años)]##

adultosh = encuesta_turistas %>%
  group_by(`Nº de adultos hombres (de 18 a 59 años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_9.1 = adultosh %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de adultos hombres (de 18 a 59 años)]`, y = prop, fill = `Nº de adultos hombres (de 18 a 59 años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_9.1.png", p_9.1)

# Gráfico de torta
p_9.2 = adultosh %>% 
  arrange(desc(`Nº de adultos hombres (de 18 a 59 años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de adultos hombres (de 18 a 59 años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_9.2.png", p_9.2)

#Tabla word: 
tab_df(adultosh, file = "cuadro_adultosH.doc")




##10. Nº de adultos mujeres (de 18 a 59 años)]##

adultosm = encuesta_turistas %>%
  group_by(`Nº de adultos mujeres (de 18 a 59 años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_10.1 = adultosm %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de adultos mujeres (de 18 a 59 años)]`, y = prop, fill = `Nº de adultos mujeres (de 18 a 59 años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_10.1.png", p_10.1)

# Gráfico de torta
p_10.2 = adultosm %>% 
  arrange(desc(`Nº de adultos mujeres (de 18 a 59 años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de adultos mujeres (de 18 a 59 años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_10.2.png", p_10.2)

#Tabla word: 
tab_df(adultosm, file = "cuadro_adultosM.doc")




##11. Nº de adultos mayores hombres (de 60 o más años)]##

mayoresh = encuesta_turistas %>%
  group_by(`Nº de adultos mayores hombres (de 60 o más años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_11.1 = mayoresh %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de adultos mayores hombres (de 60 o más años)]`, y = prop, fill = `Nº de adultos mayores hombres (de 60 o más años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_11.1.png", p_11.1)

# Gráfico de torta
p_11.2 = mayoresh %>% 
  arrange(desc(`Nº de adultos mayores hombres (de 60 o más años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de adultos mayores hombres (de 60 o más años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_11.2.png", p_11.2)

#Tabla word: 
tab_df(mayoresh, file = "cuadro_mayoresH.doc")




##12. Nº de adultos mayores mujeres (de 60 o más años)]##

mayoresm = encuesta_turistas %>%
  group_by(`Nº de adultos mayores mujeres (de 60 o más años)]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_12.1 = mayoresm %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Nº de adultos mayores mujeres (de 60 o más años)]`, y = prop, fill = `Nº de adultos mayores mujeres (de 60 o más años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_12.1.png", p_12.1)

# Gráfico de torta
p_12.2 = mayoresm %>% 
  arrange(desc(`Nº de adultos mayores mujeres (de 60 o más años)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Nº de adultos mayores mujeres (de 60 o más años)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_12.2.png", p_12.2)

#Tabla word: 
tab_df(mayoresm, file = "cuadro_mayoresM.doc")




##13. ¿Cuál es su nivel educativo##

educacion = encuesta_turistas %>%
  group_by(`¿Cuál es su nivel educativo`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_13.1 = educacion %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `¿Cuál es su nivel educativo`, y = prop, fill = `¿Cuál es su nivel educativo`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_13.1.png", p_13.1)

# Gráfico de torta
p_13.2 = educacion %>% 
  arrange(desc(`¿Cuál es su nivel educativo`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿Cuál es su nivel educativo`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_13.2.png", p_13.2)

#Tabla word: 
tab_df(educacion, file = "cuadro_educacion.doc")




##14. Podría indicar su estado civil##

estado = encuesta_turistas %>%
  group_by(`Podría indicar su estado civil`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_14.1 = estado %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Podría indicar su estado civil`, y = prop, fill = `Podría indicar su estado civil`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_14.1.png", p_14.1)

# Gráfico de torta
p_14.2 = estado %>% 
  arrange(desc(`Podría indicar su estado civil`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Podría indicar su estado civil`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_14.2.png", p_14.2)

#Tabla word: 
tab_df(estado, file = "cuadro_estadocivil.doc")




##15. Indique si es Chileno o Extranjero##

nacionalidad = encuesta_turistas %>%
  group_by(`Indique si es Chileno o Extranjero`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_15.1 =  nacionalidad %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Indique si es Chileno o Extranjero`, y = prop, fill = `Indique si es Chileno o Extranjero`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_15.1.png", p_15.1)

# Gráfico de torta
p_15.2 = nacionalidad %>% 
  arrange(desc(`Indique si es Chileno o Extranjero`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Indique si es Chileno o Extranjero`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=7)

# Guardar el gráfico: 
ggsave("p_15.2.png", p_15.2)

#Tabla word: 
tab_df(nacionalidad, file = "cuadro_nacionalidad.doc")




##16. Especifique su ciudad de residencia##

ciudad = encuesta_turistas %>%
  group_by(`Especifique su ciudad de residencia`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_16.1 = ciudad %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Especifique su ciudad de residencia`, y = prop, fill = `Especifique su ciudad de residencia`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 5)

# Guardar el gráfico: 
ggsave("p_16.1.png", p_16.1)

# Gráfico de torta
p_16.2 = ciudad %>% 
  arrange(desc(`Especifique su ciudad de residencia`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Especifique su ciudad de residencia`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=5)

# Guardar el gráfico: 
ggsave("p_16.2.png", p_16.2)

#Tabla word: 
tab_df(ciudad, file = "cuadro_cuidad.doc")




##17. Especifique su país de residencia##

pais = encuesta_turistas %>%
  group_by(`Especifique su país de residencia`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)) %>%
  filter(`Especifique su país de residencia`!="N/A")

# Gráfico de barras
p_17.1 = pais %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `Especifique su país de residencia`, y = prop, fill = `Especifique su país de residencia`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_17.1.png", p_17.1)

# Gráfico de torta
p_17.2 = pais %>% 
  arrange(desc(`Especifique su país de residencia`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Especifique su país de residencia`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=5)

# Guardar el gráfico: 
ggsave("p_17.2.png", p_17.2)

#Tabla word: 
tab_df(pais, file = "cuadro_pais.doc")




##18 19 20 21 22 stand by##




##23. ¿Cuántas veces ha visitado la comuna##

visitas = encuesta_turistas %>%
  group_by(`¿Cuántas veces ha visitado la comuna`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_23.1 = visitas %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `¿Cuántas veces ha visitado la comuna`, y = prop, fill = `¿Cuántas veces ha visitado la comuna`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_23.1.png", p_23.1)

# Gráfico de torta
p_23.2 = visitas %>% 
  arrange(desc(`¿Cuántas veces ha visitado la comuna`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿Cuántas veces ha visitado la comuna`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_23.2.png", p_23.2)

#Tabla word: 
tab_df(visitas, file = "cuadro_numerodevisitas.doc")




##24. ¿Cuántas personas lo/a acompañaron en su último viaje a la comuna## sdfsdafadfasd

acompañantes = encuesta_turistas %>%
  group_by(`¿Cuántas personas lo/a acompañaron en su último viaje a la comuna`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_24.1 = acompañantes %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `¿Cuántas personas lo/a acompañaron en su último viaje a la comuna`, y = prop, fill = `¿Cuántas personas lo/a acompañaron en su último viaje a la comuna`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_24.1.png", p_24.1)

# Gráfico de torta
p_24.2 = acompañantes %>% 
  arrange(desc(`¿Cuántas personas lo/a acompañaron en su último viaje a la comuna`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿Cuántas personas lo/a acompañaron en su último viaje a la comuna`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_24.2.png", p_24.2)

#Tabla word: 
tab_df(acompañantes, file = "cuadro_numero_de_acompañantes.doc")




##25. stand by##


##26. ¿Cuánta noches pernoctó en la comuna##

noches = encuesta_turistas %>%
  group_by(`¿Cuánta noches pernoctó en la comuna`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_26.1 = noches %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `¿Cuánta noches pernoctó en la comuna`, y = prop, fill = `¿Cuánta noches pernoctó en la comuna`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_26.1.png", p_26.1)

# Gráfico de torta
p_26.2 = noches %>% 
  arrange(desc(`¿Cuánta noches pernoctó en la comuna`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿Cuánta noches pernoctó en la comuna`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_26.2.png", p_26.2)

#Tabla word: 
tab_df(noches, file = "cuadro_numero_de_noches.doc")




##27 28 29 stand by##




##30. [Alojamiento]##

alojamiento = encuesta_turistas %>%
  group_by(`[Alojamiento]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_30.1 = alojamiento %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Alojamiento]`, y = prop, fill = `[Alojamiento]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_30.1.png", p_30.1)

# Gráfico de torta
p_30.2 = alojamiento %>% 
  arrange(desc(`[Alojamiento]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Alojamiento]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_30.2.png", p_30.2)

#Tabla word: 
tab_df(alojamiento, file = "cuadro_alojamiento.doc")




##31. [Alimentación]##
alimentacion<- encuesta_turistas %>%
  select("[Alimentación]")%>%
  filter("[Alimentación]"!="N/A")
table(alimentacion)
(prop.table(table (alojamiento)))*100
names(which(table(alimentacion)==max(table(alimentacion))))

alimentacion = encuesta_turistas %>%
  group_by(`[Alimentación]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_31.1 = alimentacion %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Alimentación]`, y = prop, fill = `[Alimentación]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_31.1.png", p_31.1)

# Gráfico de torta
p_31.2 = alimentacion %>% 
  arrange(desc(`[Alimentación]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Alimentación]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_31.2.png", p_31.2)

#Tabla word: 
tab_df(alimentacion, file = "cuadro_alimentacion.doc")




##32. [Transporte]##

transporte = encuesta_turistas %>%
  group_by(`[Transporte]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_32.1 = transporte %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Transporte]`, y = prop, fill = `[Transporte]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_32.1.png", p_32.1)

# Gráfico de torta
p_32.2 = transporte %>% 
  arrange(desc(`[Transporte]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Transporte]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_32.2.png", p_32.2)

#Tabla word: 
tab_df(transporte, file = "cuadro_transporte.doc")




##33. [Servicios complementarios]##

servicios = encuesta_turistas %>%
  group_by(`[Servicios complementarios]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_33.1 = servicios %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Servicios complementarios]`, y = prop, fill = `[Servicios complementarios]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_33.1.png", p_33.1)

# Gráfico de torta
p_33.2 = servicios %>% 
  arrange(desc(`[Servicios complementarios]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Servicios complementarios]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_33.2.png", p_33.2)

#Tabla word: 
tab_df(servicios, file = "cuadro_servicios_complementarios.doc")




##34. [Vida nocturna]##

nocturna = encuesta_turistas %>%
  group_by(`[Vida nocturna]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_34.1 = nocturna %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Vida nocturna]`, y = prop, fill = `[Vida nocturna]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_34.1.png", p_34.1)

# Gráfico de torta
p_34.2 = nocturna %>% 
  arrange(desc(`[Vida nocturna]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Vida nocturna]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_34.2.png", p_34.2)

#Tabla word: 
tab_df(nocturna, file = "cuadro_vida_nocturna.doc")




##35. [Comercio]##
comercio<- encuesta_turistas %>%
  select("[Comercio]")%>%
  filter("[Comercio]"!="N/A")
table(comercio)
(prop.table(table (comercio)))*100
names(which(table(comercio)==max(table(comercio))))

comercio = encuesta_turistas %>%
  group_by(`[Comercio]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_35.1 = comercio %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Comercio]`, y = prop, fill = `[Comercio]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_35.1.png", p_35.1)

# Gráfico de torta
p_35.2 = comercio %>% 
  arrange(desc(`[Comercio]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Comercio]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_35.2.png", p_35.2)

#Tabla word: 
tab_df(comercio, file = "cuadro_comercio.doc")




##36. [Actividades programadas por las instituciones de la comuna]##
actividades<- encuesta_turistas %>%
  select("[Actividades programadas por las instituciones de la comuna]")
table(actividades)
(prop.table(table (actividades)))*100
names(which(table(actividades)==max(table(actividades))))


##37. [Estado de caminos]##
caminos<- encuesta_turistas %>%
  select("[Estado de caminos]")%>%
  filter("[Estado de caminos]"!="N/A")
table(caminos)
(prop.table(table (caminos)))*100
names(which(table(caminos)==max(table(caminos))))


##38. [Señalización vial]##
señalizacion<- encuesta_turistas %>%
  select("[Señalización vial]")%>%
  filter("[Señalización vial]"!="N/A")
table(señalizacion)
(prop.table(table (señalizacion)))*100
names(which(table(señalizacion)==max(table(señalizacion))))

##39. [Señalética turística]##
señaleticat<- encuesta_turistas %>%
  select("[Señalética turística]")%>%
  filter("[Señalética turística]"!="N/A")
table(señaleticat)
(prop.table(table (señaleticat)))*100
names(which(table(señaleticat)==max(table(señaleticat))))


##40. [Accesos para personas con movilidad limitada]##
accesos<- encuesta_turistas %>%
  select("[Accesos para personas con movilidad limitada]")%>%
  filter("[Accesos para personas con movilidad limitada]"!="N/A")
table(accesos)
(prop.table(table (accesos)))*100
names(which(table(accesos)==max(table(accesos))))


##41. [Paisaje natural]##
paisaje<- encuesta_turistas %>%
  select("[Paisaje natural]")
table(paisaje)
(prop.table(table (paisaje)))*100
names(which(table(paisaje)==max(table(paisaje))))


##42. [Amabilidad de los residentes]##
amabilidad<- encuesta_turistas %>%
  select("[Amabilidad de los residentes]")
table(amabilidad)
(prop.table(table (amabilidad)))*100
names(which(table(amabilidad)==max(table(amabilidad))))


##43. [Relación Precio - Calidad] revisar##
precio_calidad<- encuesta_turistas %>%
  select("[Relación Precio - Calidad]")
table(precio_calidad)
(prop.table(table (precio_calidad)))*100
names(which(table(precio_calidad)==max(table(precio_calidad))))


##44. [Limpieza del entorno]##
limpieza<- encuesta_turistas %>%
  select("[Limpieza del entorno]")
table(limpieza)
(prop.table(table (limpieza)))*100
names(which(table(limpieza)==max(table(limpieza))))


##45. [Mantención de infraestructura pública]##
mantecion<- encuesta_turistas %>%
  select("[Mantención de infraestructura pública]")
table(mantecion)
(prop.table(table (mantecion)))*100
names(which(table(mantecion)==max(table(mantecion))))


##46. [Percepción de seguridad en la comuna]##
seguridad<- encuesta_turistas %>%
  select("[Percepción de seguridad en la comuna]")
table(seguridad)
(prop.table(table (seguridad)))*100
names(which(table(seguridad)==max(table(seguridad)))) 


##47. [Servicios de salud]##
salud<- encuesta_turistas %>%
  select("[Servicios de salud]")
table(salud)
(prop.table(table (salud)))*100
names(which(table(salud)==max(table(salud))))


##48 49 50 stand by



##51. [Actividades agrícolas]##
agricolas<- encuesta_turistas %>%
  select("[Actividades agrícolas]")
table(agricolas)
(prop.table(table (agricolas)))*100
names(which(table(agricolas)==max(table(agricolas))))

##52. [Participar en fiestas costumbristas]##
costumbristas<- encuesta_turistas %>%
  select("[Participar en fiestas costumbristas]")
table(costumbristas)
(prop.table(table (costumbristas)))*100
names(which(table(costumbristas)==max(table(costumbristas))))


##52. [Vestir trajes tradicionales (aunque se para una sessión de fotos)]##
trajes<- encuesta_turistas %>%
  select("[Vestir trajes tradicionales (aunque se para una sessión de fotos)]")
table(trajes)
(prop.table(table (trajes)))*100
names(which(table(trajes)==max(table(trajes))))


##53. [Caminatas y/o ciclísmo]##
caminata_ciclismo<- encuesta_turistas %>%
  select("[Caminatas y/o ciclísmo]")
table(caminata_ciclismo)
(prop.table(table (caminata_ciclismo)))*100
names(which(table(caminata_ciclismo)==max(table(caminata_ciclismo))))


##54. [Pesca deportiva (Catch and release)]##
pesca<- encuesta_turistas %>%
  select("[Pesca deportiva (Catch and release)]")
table(pesca)
(prop.table(table (pesca)))*100
names(which(table(pesca)==max(table(pesca))))


##55. [Práctica deportes naúticos]
nauticos<- encuesta_turistas %>%
  select("[Práctica deportes naúticos]")
table(nauticos)
(prop.table(table (nauticos)))*100
names(which(table(nauticos)==max(table(nauticos))))


##56. [Probar platos típicos o exclusivos de la zona]##
platos<- encuesta_turistas %>%
  select("[Probar platos típicos o exclusivos de la zona]")
table(platos)
(prop.table(table (platos)))*100
names(which(table(platos)==max(table(platos))))


##57. [Aprender a prepara platos típicos de la zona]##
preparar<- encuesta_turistas %>%
  select("[Aprender a prepara platos típicos de la zona]")
table(preparar)
(prop.table(table (preparar)))*100
names(which(table(preparar)==max(table(preparar))))

##58. [Participar en talleres de artesanía]##
talleres<- encuesta_turistas %>%
  select("[Participar en talleres de artesanía]")
table(talleres)
(prop.table(table (talleres)))*100
names(which(table(talleres)==max(table(talleres))))


##59. [Dormir en casa de una familia de la comunidad]##
dormir<- encuesta_turistas %>%
  select("[Dormir en casa de una familia de la comunidad]")
table(dormir)
(prop.table(table (dormir)))*100
names(which(table(dormir)==max(table(dormir))))


##60. ¿Repetiría su visita##
repetiria<- encuesta_turistas %>%
  select("¿Repetiría su visita")
table(repetiria)
(prop.table(table (repetiria)))*100
names(which(table(repetiria)==max(table(repetiria))))


#######CRUCE DE DATOS#########

encuesta<-encuesta_turistas

##EdadxGenero##
edadxgenero<-table(encuesta_turistas$Edad, encuesta_turistas$Género)
edadxgenero
prop.table(edadxgenero)*100
prop.table(edadxgenero,1)*100
prop.table(edadxgenero,2)*100

##Genero y Nivel Educativo
generoxeducacion<-table(encuesta_turistas$Género, encuesta_turistas$`¿Cuál es su nivel educativo`)
generoxeducacion
prop.table(generoxeducacion)*100
prop.table(generoxeducacion,1)*100
prop.table(generoxeducacion,2)*100

##edad y noches en la zona
edadxnoches<-table(encuesta_turistas$Edad, encuesta_turistas$`¿Cuánta noches pernoctó en la comuna`)
edadxnoches
prop.table(edadxnoches)*100
prop.table(edadxnoches,1)*100
prop.table(edadxnoches,2)*100

##genero y noches en la zona
generoxnoches<-table(encuesta_turistas$Género, encuesta_turistas$`¿Cuánta noches pernoctó en la comuna`)
generoxnoches
prop.table(generoxnoches)*100
prop.table(generoxnoches,1)*100
prop.table(generoxnoches,2)*100

##edad y percepcion de alojamiento
edadxalojamiento<-table(encuesta$Edad, encuesta$`[Alojamiento]`)
edadxalojamiento
prop.table(edadxalojamiento)*100
prop.table(edadxalojamiento,1)*100
prop.table(edadxalojamiento,2)*100

##genero y percepcion de alojamiento
generoxalojamiento<-table(encuesta$Género, encuesta$`[Alojamiento]`)
generoxalojamiento
prop.table(generoxalojamiento)*100
prop.table(generoxalojamiento,1)*100
prop.table(generoxalojamiento,2)*100

##edad y percepcion de la alimentacion
edadxalimentacion<-table(encuesta$Edad, encuesta$`[Alimentación]`)
edadxalimentacion
prop.table(edadxalimentacion)*100
prop.table(edadxalimentacion,1)*100
prop.table(edadxalimentacion,2)*100

##genero y percepcion de la alimentacion
generoxalimentacion<-table(encuesta$Género, encuesta$`[Alimentación]`)
generoxalimentacion
prop.table(generoxalimentacion)*100
prop.table(generoxalimentacion,1)*100
prop.table(generoxalimentacion,2)*100

##edad y percepcion del trasporte
edadxtrasporte<-table(encuesta$Edad, encuesta$`[Transporte]`)
edadxtrasporte
prop.table(edadxtrasporte)*100
prop.table(edadxtrasporte,1)*100
prop.table(edadxtrasporte,2)*100

##genero y percepcion del trasporte
generoxtrasporte<-table(encuesta$Género, encuesta$`[Transporte]`)
generoxtrasporte
prop.table(generoxtrasporte)*100
prop.table(generoxtrasporte,1)*100
prop.table(generoxtrasporte,2)*100

##edad y persepcion de los servicios complementarios
edadxservicios<-table(encuesta$Edad, encuesta$`[Servicios complementarios]`)
edadxservicios
prop.table(edadxservicios)*100
prop.table(edadxservicios,1)*100
prop.table(edadxservicios,2)*100

##genero y percepcion de los servicios complementarios
generoxservicios<-table(encuesta$Género, encuesta$`[Servicios complementarios]`)
generoxservicios
prop.table(generoxservicios)*100
prop.table(generoxservicios,1)*100
prop.table(generoxservicios,2)*100

##edad y percepcion de la vida nocturna
edadxvidanocturna<-table(encuesta$Edad, encuesta$`[Vida nocturna]`)
edadxvidanocturna
prop.table(edadxvidanocturna)*100
prop.table(edadxvidanocturna,1)*100
prop.table(edadxvidanocturna,2)*100

##genero y percepcion de la vida nocturna
generoxvidanocturna<-table(encuesta$Género, encuesta$`[Vida nocturna]`)
generoxvidanocturna
prop.table(generoxvidanocturna)*100
prop.table(generoxvidanocturna,1)*100
prop.table(generoxvidanocturna,2)*100

##edad y percepcion del comercio 
edadxcomercio<-table(encuesta$Edad, encuesta$`[Comercio]`)
edadxcomercio
prop.table(edadxcomercio)*100
prop.table(edadxcomercio,1)*100
prop.table(edadxcomercio,2)*100

##genero y percepcion del comercio
generoxcomercio<-table(encuesta$Género, encuesta$`[Comercio]`)
generoxcomercio
prop.table(generoxcomercio)*100
prop.table(generoxcomercio,1)*100
prop.table(generoxcomercio,2)*100

##edad y actividades programdas
edadxactividadesprogramadas<-table(encuesta$Edad, encuesta$`[Actividades programadas por las instituciones de la comuna]`)
edadxactividadesprogramadas
prop.table(edadxactividadesprogramadas)*100
prop.table(edadxactividadesprogramadas,1)*100
prop.table(edadxactividadesprogramadas,2)*100

##genero y actividades programadas
generoxactividadesprogramadas<-table(encuesta$Género, encuesta$`[Actividades programadas por las instituciones de la comuna]`)
generoxactividadesprogramadas
prop.table(generoxactividadesprogramadas)*100
prop.table(generoxactividadesprogramadas,1)*100
prop.table(generoxactividadesprogramadas,2)*100

##edad y estado de los caminos
edadxestadocaminos<-table(encuesta$Edad, encuesta$`[Estado de caminos]`)
edadxestadocaminos
prop.table(edadxestadocaminos)*100
prop.table(edadxestadocaminos,1)*100
prop.table(edadxestadocaminos,2)*100

##genero y estado de los caminos
generoxestadocaminos<-table(encuesta$Género, encuesta$`[Estado de caminos]`)
generoxestadocaminos
prop.table(generoxestadocaminos)*100
prop.table(generoxestadocaminos,1)*100
prop.table(generoxestadocaminos,2)*100

##edad y señalizacion vial 
edadxseñalizacionvial<-table(encuesta$Edad, encuesta$`[Señalización vial]`)
edadxseñalizacionvial
prop.table(edadxseñalizacionvial)*100
prop.table(edadxseñalizacionvial,1)*100
prop.table(edadxseñalizacionvial,2)*100

##genero y señalizaicon vial 
generoxseñalizacionvial<-table(encuesta$Género, encuesta$`[Señalización vial]`)
generoxseñalizacionvial
prop.table(generoxseñalizacionvial)*100
prop.table(generoxseñalizacionvial,1)*100
prop.table(generoxseñalizacionvial,2)*100

##edad y señaletica turistuca
edadxseñaleticaturistica<-table(encuesta$Edad, encuesta$`[Señalética turística]`)
edadxseñaleticaturistica
prop.table(edadxseñaleticaturistica)*100
prop.table(edadxseñaleticaturistica,1)*100
prop.table(edadxseñaleticaturistica,2)*100

##genero y señalitca turistica
generoxseñaleticaturistica<-table(encuesta$Género, encuesta$`[Señalética turística]`)
generoxseñaleticaturistica
prop.table(edadxseñaleticaturistica)*100
prop.table(edadxseñaleticaturistica,1)*100
prop.table(edadxseñaleticaturistica,2)*100

##edad y acceso para persoans con movilidad limitada
edadxmovilidadlimitada<-table(encuesta$Edad, encuesta$`[Accesos para personas con movilidad limitada]`)
edadxmovilidadlimitada
prop.table(edadxmovilidadlimitada)*100
prop.table(edadxmovilidadlimitada,1)*100
prop.table(edadxmovilidadlimitada,2)*100

##genero y acceso para personas con movilidad limitada
generoxmovilidadlimitada<-table(encuesta$Género, encuesta$`[Accesos para personas con movilidad limitada]`)
generoxmovilidadlimitada
prop.table(generoxmovilidadlimitada)*100
prop.table(generoxmovilidadlimitada,1)*100
prop.table(generoxmovilidadlimitada,2)*100

##edad y paisaje natural
edadxpaisaje<-table(encuesta$Edad, encuesta$`[Paisaje natural]`)
edadxpaisaje
prop.table(edadxpaisaje)*100
prop.table(edadxpaisaje,1)*100
prop.table(edadxpaisaje,2)*100

##genero y paisaje natural
generoxpaisaje<-table(encuesta$Género, encuesta$`[Paisaje natural]`)
generoxpaisaje
prop.table(generoxpaisaje)*100
prop.table(generoxpaisaje,1)*100
prop.table(generoxpaisaje,2)*100

##edad y limpieza del entorno
edadxlimpieza<-table(encuesta$Edad, encuesta$`[Limpieza del entorno]`)
edadxlimpieza
prop.table(edadxlimpieza)*100
prop.table(edadxlimpieza,1)*100
prop.table(edadxlimpieza,2)*100

##genero y limplieza del entorno 
generoxlimpieza<-table(encuesta$Género, encuesta$`[Limpieza del entorno]`)
generoxlimpieza
prop.table(generoxlimpieza)*100
prop.table(generoxlimpieza,1)*100
prop.table(generoxlimpieza,2)*100

##edad y mantencion de infrastructura publica
edadxmantencion<-table(encuesta$Edad, encuesta$`[Mantención de infraestructura pública]`)
edadxmantencion
prop.table(edadxmantencion)*100
prop.table(edadxmantencion,1)*100
prop.table(edadxmantencion,2)*100

##genero y mantencion de la infrastructura publica
generoxmantencion<-table(encuesta$Género, encuesta$`[Mantención de infraestructura pública]`)
generoxmantencion
prop.table(generoxmantencion)*100
prop.table(generoxmantencion,1)*100
prop.table(generoxmantencion,2)*100

##edad y percepcion de la seguridad en la comuna
edadxseguridad<-table(encuesta$Edad, encuesta$`[Percepción de seguridad en la comuna]`)
edadxseguridad
prop.table(edadxseguridad)*100
prop.table(edadxseguridad,1)*100
prop.table(edadxseguridad,2)*100

##genero y percepcion de la seguridad en la comuna
generoxseguridad<-table(encuesta$Género, encuesta$`[Percepción de seguridad en la comuna]`)
generoxseguridad
prop.table(generoxseguridad)*100
prop.table(generoxseguridad,1)*100
prop.table(generoxseguridad,2)*100

##edad y servicios de salud
edadxserviciossalud<-table(encuesta$Edad, encuesta$`[Servicios de salud]`)
edadxserviciossalud
prop.table(edadxserviciossalud)*100
prop.table(edadxserviciossalud,1)*100
prop.table(edadxserviciossalud,2)*100

##genero y servicios de salud
generoxserviciossalud<-table(encuesta$Género, encuesta$`[Servicios de salud]`)
generoxserviciossalud
prop.table(generoxserviciossalud)*100
prop.table(generoxserviciossalud,1)*100
prop.table(generoxserviciossalud,2)*100

##edad y actividades agricolas
edadxagricolas<-table(encuesta$Edad, encuesta$`[Actividades agrícolas]`)
edadxagricolas
prop.table(edadxagricolas)*100
prop.table(edadxagricolas,1)*100
prop.table(edadxagricolas,2)*100

##genero y actividades agricolas
generoxagricolas<-table(encuesta$Género, encuesta$`[Actividades agrícolas]`)
generoxagricolas
prop.table(generoxagricolas)*100
prop.table(generoxagricolas,1)*100
prop.table(generoxagricolas,2)*100

##edad y participacor en fiestas costumbristas
edadxcostumbristas<-table(encuesta$Edad, encuesta$`[Participar en fiestas costumbristas]`)
edadxcostumbristas
prop.table(edadxcostumbristas)*100
prop.table(edadxcostumbristas,1)*100
prop.table(edadxcostumbristas,2)*100

##genero y participar en fistas costumbristas
generoxcostumbrista<-table(encuesta$Género, encuesta$`[Participar en fiestas costumbristas]`)
generoxcostumbrista
prop.table(generoxcostumbrista)*100
prop.table(generoxcostumbrista,1)*100
prop.table(generoxcostumbrista,2)*100

##edad y vestir trajes tradicionales
edadxtrajes<-table(encuesta$Edad, encuesta$`[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`)
edadxtrajes
prop.table(edadxtrajes)*100
prop.table(edadxtrajes,1)*100
prop.table(edadxtrajes,2)*100

##genero y vestir trajes tradicionales
generoxvestir<-table(encuesta$Género, encuesta$`[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`)
generoxvestir
prop.table(generoxvestir)*100
prop.table(generoxvestir,1)*100
prop.table(generoxvestir,2)*100

##edad y caminatas o ciclismo
edadxcamianatas<-table(encuesta$Edad, encuesta$`[Caminatas y/o ciclísmo]`)
edadxcamianatas
prop.table(edadxcamianatas)*100
prop.table(edadxcamianatas,1)*100
prop.table(edadxcamianatas,2)*100

##genero y caminatas o ciclismo 
generoxcaminatas<-table(encuesta$Género, encuesta$`[Caminatas y/o ciclísmo]`)
generoxcaminatas
prop.table(generoxcaminatas)*100
prop.table(generoxcaminatas,1)*100
prop.table(generoxcaminatas,2)*100

##edad y pesca deportica
edadxpesca<-table(encuesta$Edad, encuesta$`[Pesca deportiva (Catch and release)]`)
edadxpesca
prop.table(edadxpesca)*100
prop.table(edadxpesca,1)*100
prop.table(edadxpesca,2)*100

##genero y pesca deportiva
generoxpesca<-table(encuesta$Género, encuesta$`[Pesca deportiva (Catch and release)]`)
generoxpeca
prop.table(generoxpesca)*100
prop.table(generoxpesca,1)*100
prop.table(generoxpesca,2)*100

##edad y practicar deportes nauticos 
edadxnauticos<-table(encuesta$Edad, encuesta$`[Práctica deportes naúticos]`)
edadxnauticos
prop.table(edadxnauticos)*100
prop.table(edadxnauticos,1)*100
prop.table(edadxnauticos,2)*100

##genero y practicar deportes nauticos
generoxnauticos<-table(encuesta$Género, encuesta$`[Práctica deportes naúticos]`)
generoxnauticos
prop.table(generoxnauticos)*100
prop.table(generoxnauticos,1)*100
prop.table(generoxnauticos,2)*100

##edad y probar platos de la zona
edadxplatos<-table(encuesta$Edad, encuesta$`[Probar platos típicos o exclusivos de la zona]`)
edadxplatos
prop.table(edadxplatos)*100
prop.table(edadxplatos,1)*100
prop.table(edadxplatos,2)*100

##genero y probar platos de la zona
generoxplatos<-table(encuesta$Género, encuesta$`[Probar platos típicos o exclusivos de la zona]`)
generoxplatos
prop.table(generoxplatos)*100
prop.table(generoxplatos,1)*100
prop.table(generoxplatos,2)*100

##edad y aprender a preparar platos tipicos de la zona
edadxaprender<-table(encuesta$Edad, encuesta$`[Aprender a prepara platos típicos de la zona]`)
edadxaprender
prop.table(edadxaprender)*100
prop.table(edadxaprender,1)*100
prop.table(edadxaprender,2)*100

##genero y aprender a preparar platos tipicos de la zona
generoxaprender<-table(encuesta$Género, encuesta$`[Aprender a prepara platos típicos de la zona]`)
generoxaprender
prop.table(generoxaprender)*100
prop.table(generoxaprender,1)*100
prop.table(generoxaprender,2)*100

##edad y participar en talleres de artesania
edadxtalleres<-table(encuesta$Edad, encuesta$`[Participar en talleres de artesanía]`)
edadxtalleres
prop.table(edadxtalleres)*100
prop.table(edadxtalleres,1)*100
prop.table(edadxtalleres,2)*100

##genero y participar en talleres de artesania 
generoxtalleres<-table(encuesta$Género, encuesta$`[Participar en talleres de artesanía]`)
generoxtalleres
prop.table(generoxtalleres)*100
prop.table(generoxtalleres,1)*100
prop.table(generoxtalleres,2)*100

##edad y dormir en la casa de una famila de la comunidad
edadxdormir<-table(encuesta$Edad, encuesta$`[Dormir en casa de una familia de la comunidad]`)
edadxdormir
prop.table(edadxdormir)*100
prop.table(edadxdormir,1)*100
prop.table(edadxdormir,2)*100

##genero y dormir en la casa de una famila de la comunidad
generoxdormir<-table(encuesta$Género, encuesta$`[Dormir en casa de una familia de la comunidad]`)
generoxdormir
prop.table(generoxdormir)*100
prop.table(generoxdormir,1)*100
prop.table(generoxdormir,2)*100

##edad y ¿Repetiria su visita?
edadxrepetiria<-table(encuesta$Edad, encuesta$`¿Repetiría su visita`)
edadxrepetiria
prop.table(edadxrepetiria)*100
prop.table(edadxrepetiria,1)*100
prop.table(edadxrepetiria,2)*100

##genero y ¿repetiria su visita?
generoxrepetiria<-table(encuesta$Género, encuesta$`¿Repetiría su visita`)
generoxrepetiria
prop.table(generoxrepetiria)*100
prop.table(generoxrepetiria,1)*100
prop.table(generoxrepetiria,2)*100

