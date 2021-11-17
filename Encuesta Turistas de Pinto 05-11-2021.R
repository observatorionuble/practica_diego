#Encuesta a turistas de Pinto#

rm(list = ls())

load_pkg <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
}

paquetes = c("tidyverse", "sjPlot", "reshape2", "dummy") 
# Fíjate que agregué el paquete reshape2 para manipular los datos y abordar las preguntas de selección múltiple

load_pkg(paquetes)

# Ojo que al cargar los datos noté que hay un folio duplicado, el cual procedí a eliminar al momento de cargar la data

encuesta_turistas = readxl::read_excel("20211013_Encuesta a Turistas Pinto modificada.xlsx") %>% 
  mutate(duplicado = duplicated(Correlativo)) %>% 
  filter(duplicado == FALSE)


## Análisis de respuestas de selección múltiple 

# ¿Cuál o cuales medios de transporte fueron utilizados para llegar a este destino? Seleccione máximo 2 alternativas
# Que aspectos lo motivaron para visitar pinto?
# A través de que medios se informó acerca de la comuna?
# En que época visito pinto?
# Indique los servicios utilizados en su viaje
# Cuantas personas lo acompañaron en su último viaje a la comuna?
# Con quiénes realizó su último viaje?
# Indique cual fue su su gasto promedio por persona en su última visita
# Tipos de hospedaje utilizados en su estadía
# Servicios de alimentación utilizados en su visita/estadía
# Que lugares conoce en la comuna de pinto? 
# Que actividades realizó durante su visita? 
# Que actividades culturales conoce en la comuna de pinto? 
# Existe alguna actividad, producto o servicio que quiso realizar o adquirir y no estaba disponible? De ser así, que producto o servicio no estuvo disponible?
# Repetiría su visita, porque motivo?


# Para analizar este tipo de preguntas tenemos básicamente dos opciones: 
# a) Analizar el porcentaje de menciones
# b) Analizar el porcentaje de personas que hizo mención a alguna de las alternativas. 


# opción a): para analizar el porcentaje de menciones lo que tenemos que hacer es apilarlas todas: 

data = melt(encuesta_turistas %>% select(Correlativo, `¿Qué aspectos lo motivaron para visitar Pinto`, `...22`, `...23`),
                      id.vars = "Correlativo", 
                      measure.vars = c("¿Qué aspectos lo motivaron para visitar Pinto", "...22", "...23"), na.rm = TRUE)

#Nota que al apilar todas las respuestas ahora el conjunto de datos tiene 317 observaciones en lugar de las 178 originales. 

menciones_ = data %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  arrange(desc(prop))

# Para generar un cuadro:  
tab_df(menciones_, file = "cuadro_menciones.doc")

# Opción b): Para analizar el porcentaje de casos (personas) que mencionó alguno de los motivos: 

# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
dummies_ = data %>% 
  select(value) %>% 
  dummy(int = TRUE) 

casos_ = cbind(Correlativo = data$Correlativo, dummies_) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(dummies_), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(dummies_), .funs = ~sum(.)/n()) %>% 
  pivot_longer(cols = everything(), names_to = "Motivaciones", values_to = "% de casos")

##ESTADISTICA DESCRIPTIVA##

## 1.Género##

genero = encuesta_turistas %>%
  group_by(Género) %>% 
  summarise(n = n()) %>% 
  mutate(prop = (n/sum(n))*100)


# Gráfico de barras con ggplot
p_1.1 = genero %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `Género`, y = prop, fill = `Género`))+
  geom_col(col = "black")+
  theme(legend.position = "none")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(title= "Género", x= "Género", y = "% del Total")

p_1.1
# Para guardar el gráfico: 
ggsave("p_1.1.png", p_1.1)

# Gráfico de torta
p_1.2 = genero %>% 
  arrange(desc(`Género`)) %>% 
  mutate(prop = round(prop,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `Género`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Para guardar el gráfico: 
ggsave("p_1.2.png", p_1.2)


# Para crear una tabla compatible con word: 
tab_df(genero, file = "cuadro_genero.doc")
        

##2. Edad##
edad = encuesta_turistas %>%
  group_by(Edad) %>%
  summarise(n=n()) %>%
  mutate(prop= (n/sum(n))*100)

# Gráfico de barras
p_2.1 = edad %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `Edad`, y = prop, fill = `Edad`))+
  geom_col(col = "black")+
  theme(legend.position = "none")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(title ="Edad de los ecuestados(as)", x = "Edad", y = "% del Total")
  

p_2.1

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
  geom_text(aes(y = ypos, label = paste0(prop,"%")), color = "white", size=4)

# Guardar el gráfico: 
ggsave("p_2.2.png", p_2)

#Tabla word: 
tab_df(edad, file = "cuadro_edad.doc")




##3. ¿A qué se dedica actualmente?##

ocupacion = encuesta_turistas %>%
  group_by(`¿A qué se dedica actualmente`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_3.1 = ocupacion %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `¿A qué se dedica actualmente`, y = prop, fill = `¿A qué se dedica actualmente`))+
  theme(text = element_text(angle=8))+
  geom_col(col = "black")+
  theme(legend.position = "none")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(title = "", x="Ocupación", y="% del Total")


p_3.1
# Guardar el gráfico: 
ggsave("p_3.1.png", p_3.1)

# Gráfico de torta
p_3.2 = ocupacion %>% 
  arrange(desc(`¿A qué se dedica actualmente`)) %>% 
  mutate(prop = round(prop,1), 
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
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_4.1 = ingresos %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `Indique el rango de ingresos que percibe su núcleo familiar`, y = prop, fill = `Indique el rango de ingresos que percibe su núcleo familiar`))+
  theme(text = element_text(angle=8))+
  geom_col(col = "black")+
  theme(legend.position = "none")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 3)+
  labs(title = "", x= "Ingresos", y="% del Total")
p_4.1

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
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_5.1 = Nniños %>%
  mutate(prop = round(prop,1)) %>% 
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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_12.1 = mayoresm %>%
  mutate(prop = round(prop*1)) %>% 
  ggplot(aes(x = `Nº de adultos mayores mujeres (de 60 o más años)]`, y = prop, fill = `Nº de adultos mayores mujeres (de 60 o más años)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_12.1.png", p_12.1)

# Gráfico de torta
p_12.2 = mayoresm %>% 
  arrange(desc(`Nº de adultos mayores mujeres (de 60 o más años)]`)) %>% 
  mutate(prop = round(prop,1), 
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
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_13.1 = educacion %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `¿Cuál es su nivel educativo`, y = prop, fill = `¿Cuál es su nivel educativo`))+
  theme(legend.position = "none")+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(title= "¿Cúal es su nivel educativo?", x="¿Cúal es su nivel educativo?", y= "% del Total")

p_13.1
# Guardar el gráfico: 
ggsave("p_13.1.png", p_13.1)

# Gráfico de torta
p_13.2 = educacion %>% 
  arrange(desc(`¿Cuál es su nivel educativo`)) %>% 
  mutate(prop = round(prop,1), 
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
  filter(!is.na(`Podría indicar su estado civil`))%>%
  group_by(`Podría indicar su estado civil`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)
 
# Gráfico de barras
p_14.1 = estado %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `Podría indicar su estado civil`, y = prop, fill = `Podría indicar su estado civil`))+
  theme(legend.position = "none")+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(title = "¿Podira indicar su estado civil?", x= "Estado Civil", y= "% del Total")

p_14.1
# Guardar el gráfico: 
ggsave("p_14.1.png", p_14.1)

# Gráfico de torta
p_14.2 = estado %>% 
  arrange(desc(`Podría indicar su estado civil`)) %>% 
  mutate(prop = round(prop,1), 
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
  mutate(prop= n/sum(n)*100)

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
  mutate(prop= n/sum(n)*100)

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




##18 ##


##19 ¿Cuál o cuales medios de transporte fueron utilizados para llegar a este destino? Seleccione máximo 2 alternativas##

transporte = melt(encuesta_turistas %>% select(Correlativo, `¿Cuál o cuales medios de transporte fueron utilizados para llegar a este destino? Seleccione máximo 2 alternativas`, `...20`),
              id.vars = "Correlativo", 
              measure.vars = c("¿Cuál o cuales medios de transporte fueron utilizados para llegar a este destino? Seleccione máximo 2 alternativas", "...20"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d19= transporte %>% 
  select(value) %>% 
  dummy(int = TRUE) 

metodo_trasporte = cbind(Correlativo = transporte$Correlativo, d19) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d19), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d19), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "trasporte", values_to = "% de casos")

#Tabla word: 
tab_df(metodo_trasporte, file = "metodo_trasporte.doc")



#20 ¿Qué aspectos lo motivaron para visitar Pinto?##
motivo = melt(encuesta_turistas %>% select(Correlativo, `¿Qué aspectos lo motivaron para visitar Pinto`, `...22`, `...23`),
            id.vars = "Correlativo", 
            measure.vars = c("¿Qué aspectos lo motivaron para visitar Pinto", "...22", "...23"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d20= motivo %>% 
  select(value) %>% 
  dummy(int = TRUE) 

motivo_visita = cbind(Correlativo = motivo$Correlativo, d20) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d20), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d20), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Motivaciones", values_to = "% de casos")
  
#Tabla word: 
tab_df(motivo_visita, file = "motivo_visita.doc")


# Guardar el gráfico: 
ggsave("p_3.1.png", p_3.1)



##21 ¿A través de que medio/s se informó acerca de la comuna ##
informo_comuna = melt(encuesta_turistas %>% select(Correlativo, `¿A través de que medio/s se informó acerca de la comuna`, `...25`, `...26`, `...27`),
              id.vars = "Correlativo", 
              measure.vars = c("¿A través de que medio/s se informó acerca de la comuna", "...25", "...26", "...27"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d21= informo_comuna %>% 
  select(value) %>% 
  dummy(int = TRUE) 

informacion_sobre_comuna = cbind(Correlativo = informo_comuna$Correlativo, d21) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d21), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d21), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Medios de Información", values_to = "% de casos")

#Tabla word: 
tab_df(informacion_sobre_comuna, file = "informacion_sobre_comuna.doc")



##22 ¿En qué época del año visitó Pinto##

epoca = encuesta_turistas %>%
  group_by(`¿En qué época del año visitó Pinto`) %>%
  filter(`¿En qué época del año visitó Pinto`!="i9")%>%
  filter(`¿En qué época del año visitó Pinto`!="Verano (Diciembre a febrero), Invierno (Junio a Agosto)")%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_22.1 = epoca %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `¿En qué época del año visitó Pinto`, y = prop, fill = `¿En qué época del año visitó Pinto`))+
  theme(legend.position = "none")+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(x="¿En qué epoca del año visitó Pinto?", y= "% del Total")

p_22.1
# Guardar el gráfico: 
ggsave("p_22.1.png", p_22.1)

# Gráfico de torta
p_24.2 = epoca %>% 
  arrange(desc(`¿En qué época del año visitó Pinto`)) %>% 
  mutate(prop = round(prop,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿En qué época del año visitó Pinto`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_24.2.png", p_24.2)

#Tabla word: 
tab_df(epoca, file = "cuadro_epoca_año.doc")


##23. ¿Cuántas veces ha visitado la comuna##

visitas = encuesta_turistas %>%
  filter(!is.na(`¿Cuántas veces ha visitado la comuna`))%>%
  group_by(`¿Cuántas veces ha visitado la comuna`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

# Gráfico de barras
p_23.1 = visitas %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `¿Cuántas veces ha visitado la comuna`, y = prop, fill = `¿Cuántas veces ha visitado la comuna`))+
  theme(text = element_text(angle=0))+
  geom_col(col = "black")+
  theme(legend.position = "none")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(y="% del Total", x="¿Cúantas veces ha visitado la comuna?")

p_23.1
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




##24. ¿Con quien o quienes realizó su último viaje?
ultimo = melt(encuesta_turistas %>% select(Correlativo, `¿Con quien o quienes realizó su último viaje`, `...38`, `...39`),
                 id.vars = "Correlativo", 
                 measure.vars = c("¿Con quien o quienes realizó su último viaje", "...38", "...39"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d24= ultimo %>% 
  select(value) %>% 
  dummy(int = TRUE) 

acompañantes_ultimo_viaje = cbind(Correlativo = ultimo$Correlativo, d24) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d24), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d24), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Acompañantes en la visita", values_to = "% de casos")

#Tabla word: 
tab_df(acompañantes_ultimo_viaje, file = "acompañantes_ultimo_viaje.doc")





##25. Indique los servicios utilizados en su viaje. ##
servicios = melt(encuesta_turistas %>% select(Correlativo, `Indique los servicios utilizados en su viaje.`, `...30`, `...31`, `...32`, `...33`, `...34`),
              id.vars = "Correlativo", 
              measure.vars = c("Indique los servicios utilizados en su viaje.", "...30", "...31", "...32", "...33", "...34"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d25= servicios %>% 
  select(value) %>% 
  dummy(int = TRUE) 

servicios_utilizados = cbind(Correlativo = servicios$Correlativo, d25) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d25), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d25), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Servicios utilizados", values_to = "% de casos")

#Tabla word: 
tab_df(servicios_utilizados, file = "servicios_utilizados.doc")


##26. ¿Cuánta noches pernoctó en la comuna##

noches = encuesta_turistas %>%
  filter(!is.na(`¿Cuánta noches pernoctó en la comuna`)) %>%
  group_by(`¿Cuánta noches pernoctó en la comuna`) %>%
  summarise(n=n()) %>%
  mutate(prop= (n/sum(n))*100)

# Gráfico de barras
p_26.1 = noches %>%
  mutate(prop = round(prop,1)) %>% 
  ggplot(aes(x = `¿Cuánta noches pernoctó en la comuna`, y = prop, fill = `¿Cuánta noches pernoctó en la comuna`))+
  theme(legend.position = "none")+
  theme(text = element_text(angle=5))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)+
  labs(x="¿Cuántas noches peronectó en la comuna?", y="% del Total")

p_26.1
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




##27 Indique cuál fue su gasto diario promedio  por persona en su última visita.##  hay que arreglar la encusta 

gasto_promedio = encuesta_turistas %>%
  filter(!is.na(`Indique cuál fue su gasto diario promedio  por persona en su última visita.`)) %>%
  group_by(`Indique cuál fue su gasto diario promedio  por persona en su última visita.`) %>%
  summarise(n=n())*100 %>%
  mutate(prop= (n/sum(n)))





##28 Tipos de Hospedaje utilizados durante su estadía.##
hospedaje = melt(encuesta_turistas %>% select(Correlativo, `Tipos de Hospedaje utilizados durante su estadía.`, `...43`, `...44`, `...45`),
                      id.vars = "Correlativo", 
                      measure.vars = c("Tipos de Hospedaje utilizados durante su estadía.", "...43", "...44", "...45"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d28= hospedaje %>% 
  select(value) %>% 
  dummy(int = TRUE) 

tipos_de_hospedaje = cbind(Correlativo = hospedaje$Correlativo, d28) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d28), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d28), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Tipos de Hospedaje", values_to = "% de casos")

#Tabla word: 
tab_df(tipos_de_hospedaje, file = "tipos_de_hospedaje.doc")




###Servicios de Alimentación utilizados durante su visita/estadía.##
servalimentos = melt(encuesta_turistas %>% select(Correlativo, `Servicios de Alimentación utilizados durante su visita/estadía.`, `...47`, `...48`, `...49`),
                 id.vars = "Correlativo", 
                 measure.vars = c("Servicios de Alimentación utilizados durante su visita/estadía.", "...47", "...48", "...49"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d29= servalimentos %>% 
  select(value) %>% 
  dummy(int = TRUE) 

serivicios_de_alimentacion = cbind(Correlativo = servalimentos$Correlativo, d29) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d29), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d29), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Servicios Alimenticios", values_to = "% de casos")

#Tabla word: 
tab_df(serivicios_de_alimentacion, file = "serivicios_de_alimentacion.doc")




##30. [Alojamiento]##


alojamiento = encuesta_turistas %>%
  filter(`[Alojamiento]`!="N/A", !is.na(`[Alojamiento]`)) %>%
  group_by(`[Alojamiento]`) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)
aloj<-alojamiento %>% select(-n)
tab_df(aloj, file = "aloj.doc")

# Gráfico de barras
p_30.1 = alojamiento %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Alojamiento]`, y = prop, fill = `[Alojamiento]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

p_30.1
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

alimentacion = encuesta_turistas %>%
  group_by(`[Alimentación]`) %>%
  filter(`[Alimentación]`!="N/A", !is.na(`[Alimentación]`)) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

alim<-alimentacion %>% select(-n)
tab_df(alim, file = "alim.doc")

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
  filter(`[Transporte]`!="N/A", !is.na(`[Transporte]`)) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

trans<-transporte %>% select(-n)
tab_df(trans, file = "trans.doc")

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

servicios = encuesta_turistas%>%
  group_by(`[Servicios complementarios]`) %>%
  filter(`[Servicios complementarios]`!="N/A", !is.na(`[Servicios complementarios]`))
  summarise(n=n())%>%
  mutate(prop= n/sum(n))
  
serv<-servicios %>% select(-n)
tab_df(serv, file = "serv.doc")
  
  
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
  filter(`[Vida nocturna]`!="N/A", !is.na(`[Vida nocturna]`)) %>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)
noc<-nocturna %>% select(-n)
tab_df(noc, file = "noc.doc")

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

comercio = encuesta_turistas %>%
  group_by(`[Comercio]`) %>%
  filter(`[Comercio]`!="N/A", !is.na(`[Comercio]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

com<-comercio %>% select(-n)
tab_df(com, file = "com.doc")

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

actividades = encuesta_turistas %>%
  group_by(`[Actividades programadas por las instituciones de la comuna]`) %>%
  filter(`[Actividades programadas por las instituciones de la comuna]`!="N/A", !is.na(`[Actividades programadas por las instituciones de la comuna]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

activ<-actividades %>% select(-n)
tab_df(activ, file = "activ.doc")


# Gráfico de barras
p_36.1 = actividades %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Actividades programadas por las instituciones de la comuna]`, y = prop, fill = `[Actividades programadas por las instituciones de la comuna]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 4)

# Guardar el gráfico: 
ggsave("p_36.1.png", p_36.1)

# Gráfico de torta
p_36.2 = actividades %>% 
  arrange(desc(`[Actividades programadas por las instituciones de la comuna]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Actividades programadas por las instituciones de la comuna]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_36.2.png", p_35.2)

#Tabla word: 
tab_df(actividades, file = "cuadro_actividades_programadas_por_la_comuna.doc")





##37. [Estado de caminos]##

caminos = encuesta_turistas %>%
  group_by(`[Estado de caminos]`) %>%
  filter(`[Estado de caminos]`!="N/A", !is.na(`[Estado de caminos]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

cam<-caminos %>% select(-n)
tab_df(cam, file = "cam.doc")

# Gráfico de barras
p_37.1 = caminos %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Estado de caminos]`, y = prop, fill = `[Estado de caminos]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_37.1.png", p_37.1)

# Gráfico de torta
p_37.2 = caminos %>% 
  arrange(desc(`[Estado de caminos]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Estado de caminos]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_37.2.png", p_37.2)

#Tabla word: 
tab_df(caminos, file = "cuadro_estado_de_los_caminos.doc")




##38. [Señalización vial]##

señalizacion = encuesta_turistas %>%
  group_by(`[Señalización vial]`) %>%
  filter(`[Señalización vial]`!="N/A", !is.na(`[Señalización vial]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

seña<-señalizacion %>% select(-n)
tab_df(seña, file = "seña.doc")

# Gráfico de barras
p_38.1 = señalizacion %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Señalización vial]`, y = prop, fill = `[Señalización vial]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_38.1.png", p_38.1)

# Gráfico de torta
p_38.2 = señalizacion %>% 
  arrange(desc(`[Señalización vial]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Señalización vial]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_38.2.png", p_38.2)

#Tabla word: 
tab_df(señalizacion, file = "cuadro_señalizacion_vial.doc")




##39. [Señalética turística]##

señaleticat= encuesta_turistas %>%
  group_by(`[Señalética turística]`) %>%
  filter(`[Señalética turística]`!="N/A", !is.na(`[Señalética turística]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

señalet<-señaleticat %>% select(-n)
tab_df(señalet, file = "señalet.doc")

# Gráfico de barras
p_39.1 = señaleticat %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Señalética turística]`, y = prop, fill = `[Señalética turística]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_39.1.png", p_39.1)

# Gráfico de torta
p_39.2 = señaleticat %>% 
  arrange(desc(`[Señalética turística]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Señalética turística]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_39.2.png", p_39.2)

#Tabla word: 
tab_df(señaleticat, file = "cuadro_señaleticas_turisticas.doc")




##40. [Accesos para personas con movilidad limitada]## saque el "regular" con la r minuscula

accesos= encuesta_turistas %>%
  group_by(`[Accesos para personas con movilidad limitada]`) %>%
  filter(`[Accesos para personas con movilidad limitada]`!="regular")%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100) %>%
  filter(`[Accesos para personas con movilidad limitada]`!="N/A")

acc<-accesos %>% select(-n)
tab_df(acc, file = "acc.doc")

# Gráfico de barras
p_40.1 = accesos %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Accesos para personas con movilidad limitada]`, y = prop, fill = `[Accesos para personas con movilidad limitada]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_40.1.png", p_40.1)

# Gráfico de torta
p_40.2 = accesos %>% 
  arrange(desc(`[Accesos para personas con movilidad limitada]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Accesos para personas con movilidad limitada]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_40.2.png", p_40.2)

#Tabla word: 
tab_df(accesos, file = "cuadro_accesos_para_discapacitados.doc")


##41. [Paisaje natural]##

paisaje= encuesta_turistas %>%
  group_by(`[Paisaje natural]`) %>%
  filter(`[Paisaje natural]`!="N/A", !is.na(`[Paisaje natural]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

pai<-paisaje %>% select(-n)
tab_df(pai, file = "pai.doc")


# Gráfico de barras
p_41.1 = paisaje %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Paisaje natural]`, y = prop, fill = `[Paisaje natural]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_41.1.png", p_41.1)

# Gráfico de torta
p_41.2 = paisaje %>% 
  arrange(desc(`[Paisaje natural]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Paisaje natural]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_41.2.png", p_41.2)

#Tabla word: 
tab_df(paisaje, file = "cuadro_paisaje_natural.doc")




##42. [Amabilidad de los residentes]##

amabilidad= encuesta_turistas %>%
  group_by(`[Amabilidad de los residentes]`) %>%
  filter(`[Amabilidad de los residentes]`!="N/A", !is.na(`[Amabilidad de los residentes]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

ama<-amabilidad %>% select(-n)
tab_df(ama, file = "pai.doc")

# Gráfico de barras
p_42.1 = amabilidad %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Amabilidad de los residentes]`, y = prop, fill = `[Amabilidad de los residentes]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_42.1.png", p_42.1)

# Gráfico de torta
p_42.2 = amabilidad %>% 
  arrange(desc(`[Amabilidad de los residentes]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Amabilidad de los residentes]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_42.2.png", p_42.2)

#Tabla word: 
tab_df(amabilidad, file = "cuadro_amabilidad_de_los_residentes.doc")




##43. [Relación Precio - Calidad]##

precio_calidad= encuesta_turistas %>%
  group_by(`[Relación Precio - Calidad]`) %>%
  filter(`[Relación Precio - Calidad]`!="N/A", `[Relación Precio - Calidad]`!="p", !is.na(`[Relación Precio - Calidad]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

pre<-precio_calidad %>% select(-n)
tab_df(pre, file = "pre.doc")

# Gráfico de barras
p_43.1 = precio_calidad %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Relación Precio - Calidad]`, y = prop, fill = `[Relación Precio - Calidad]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_43.1.png", p_43.1)

# Gráfico de torta
p_43.2 = precio_calidad %>% 
  arrange(desc(`[Relación Precio - Calidad]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Relación Precio - Calidad]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_43.2.png", p_43.2)

#Tabla word: 
tab_df(precio_calidad, file = "cuadro_precio_calidad.doc")


##44. [Limpieza del entorno]##

limpieza= encuesta_turistas %>%
  group_by(`[Limpieza del entorno]`) %>%
  filter(`[Limpieza del entorno]`!="N/A", !is.na(`[Limpieza del entorno]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

lim<-limpieza %>% select(-n)
tab_df(lim, file = "lim.doc")

# Gráfico de barras
p_44.1 = limpieza %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Limpieza del entorno]`, y = prop, fill = `[Limpieza del entorno]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_44.1.png", p_44.1)

# Gráfico de torta
p_44.2 = limpieza %>% 
  arrange(desc(`[Limpieza del entorno]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Limpieza del entorno]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_44.2.png", p_44.2)

#Tabla word: 
tab_df(limpieza, file = "cuadro_limpieza_del_entorno.doc")




##45. [Mantención de infraestructura pública]##

mantencion= encuesta_turistas %>%
  group_by(`[Mantención de infraestructura pública]`) %>%
  filter(`[Mantención de infraestructura pública]`!="N/A", !is.na(`[Mantención de infraestructura pública]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

man<-mantencion %>% select(-n)
tab_df(man, file = "man.doc")

# Gráfico de barras
p_45.1 = mantencion %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Mantención de infraestructura pública]`, y = prop, fill = `[Mantención de infraestructura pública]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_45.1.png", p_45.1)

# Gráfico de torta
p_45.2 = mantencion %>% 
  arrange(desc(`[Mantención de infraestructura pública]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Mantención de infraestructura pública]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_45.2.png", p_45.2)

#Tabla word: 
tab_df(mantencion, file = "cuadro_mantencion_de_infraestructura_publica.doc")




##46. [Percepción de seguridad en la comuna]##

seguridad= encuesta_turistas %>%
  group_by(`[Percepción de seguridad en la comuna]`) %>%
  filter(`[Percepción de seguridad en la comuna]`!="N/A", !is.na(`[Percepción de seguridad en la comuna]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

seg<-seguridad %>% select(-n)
tab_df(seg, file = "seg.doc")


# Gráfico de barras
p_46.1 = seguridad %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Percepción de seguridad en la comuna]`, y = prop, fill = `[Percepción de seguridad en la comuna]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_46.1.png", p_46.1)

# Gráfico de torta
p_46.2 = seguridad %>% 
  arrange(desc(`[Percepción de seguridad en la comuna]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Percepción de seguridad en la comuna]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_46.2.png", p_46.2)

#Tabla word: 
tab_df(seguridad, file = "cuadro_percepcion_de_seguridad.doc")




##47. [Servicios de salud]##

salud= encuesta_turistas %>%
  group_by(`[Servicios de salud]`) %>%
  filter(`[Servicios de salud]`!="N/A", !is.na(`[Servicios de salud]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n)*100)

sal<-salud %>% select(-n)
tab_df(sal, file = "sal.doc")


# Gráfico de barras
p_47.1 = salud %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Servicios de salud]`, y = prop, fill = `[Servicios de salud]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_47.1.png", p_47.1)

# Gráfico de torta
p_47.2 = salud %>% 
  arrange(desc(`[Servicios de salud]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Servicios de salud]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_47.2.png", p_47.2)

#Tabla word: 
tab_df(seguridad, file = "cuadro_servicios_de_salud.doc")




##48 ¿Qué lugares conoce en la Comuna de Pinto##
lugares = melt(encuesta_turistas %>% select(Correlativo, `¿Qué lugares conoce en la Comuna de Pinto`, `...70`, `...71`, `...72`, `...73`, `...74`, `...75`, `...76`, `...77`, `...78`, `...79`, `...80`),
                   id.vars = "Correlativo", 
                   measure.vars = c("¿Qué lugares conoce en la Comuna de Pinto", "...70", "...71", "...72", "...73", "...74", "...75", "...76", "...77", "...78", "...79", "...80"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d48= lugares %>% 
  select(value) %>% 
  dummy(int = TRUE) 

lugares_conocidos= cbind(Correlativo = lugares$Correlativo, d48) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d48), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d48), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Lugares conocidos de la comuna", values_to = "% de casos")

#Tabla word: 
tab_df(lugares_conocidos, file = "lugares_conocidos.doc")




##49. ¿Qué actividades realizó durante su visita? ##

actividades = melt(encuesta_turistas %>% select(Correlativo, `¿Qué actividades realizó durante su visita?`, `...82`, `...83`, `...84`, `...85`, `...86`, `...87`),
              id.vars = "Correlativo", 
              measure.vars = c("¿Qué actividades realizó durante su visita?", "...82", "...83", "...84", "...85", "...86", "...87"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d49= actividades %>% 
  select(value) %>% 
  dummy(int = TRUE) 

actividades_realizadas = cbind(Correlativo = actividades$Correlativo, d49) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d49), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d49), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Actividades", values_to = "% de casos")

#Tabla word: 
tab_df(actividades_realizadas, file = "actividades_realizadas.doc")




## 50 ¿Qué actividades culturales conoce en la Comuna de Pinto ##
culturales = melt(encuesta_turistas %>% select(Correlativo, `¿Qué actividades culturales conoce en la Comuna de Pinto`, `...89`, `...90`, `...91`, `...92`, `...93`, `...94`, `...95`, `...96`),
                   id.vars = "Correlativo", 
                   measure.vars = c("¿Qué actividades culturales conoce en la Comuna de Pinto", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96"), na.rm = TRUE)


# Primero crearé variables dummy que tomarán el valor de 0 o 1, dependiendo de si el encuestado hizo mención a cada uno de los motivos de viaje: 
d50= culturales %>% 
  select(value) %>% 
  dummy(int = TRUE) 

actividades_culturales = cbind(Correlativo = culturales$Correlativo, d50) %>%  
  group_by(Correlativo) %>% 
  summarise_at(.vars = names(d50), .funs = ~sum(.)) %>% 
  ungroup() %>% 
  summarise_at(.vars = names(d50), .funs = ~sum((.)/n()*100)) %>% 
  pivot_longer(cols = everything(), names_to = "Actividades Culturales", values_to = "% de casos")

#Tabla word: 
tab_df(actividades_culturales, file = "actividades_culturales.doc")





##51. [Actividades agrícolas]##

agricolas= encuesta_turistas %>%
  group_by(`[Actividades agrícolas]`) %>%
  filter(!is.na(`[Actividades agrícolas]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_51.1 = agricolas %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Actividades agrícolas]`, y = prop, fill = `[Actividades agrícolas]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_51.1.png", p_51.1)

# Gráfico de torta
p_51.2 = agricolas %>% 
  arrange(desc(`[Actividades agrícolas]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Actividades agrícolas]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_51.2.png", p_51.2)

#Tabla word: 
tab_df(agricolas, file = "cuadro_actividades_agricolas.doc")




##52. [Participar en fiestas costumbristas]##

costumbristas= encuesta_turistas %>%
  group_by(`[Participar en fiestas costumbristas]`) %>%
  filter(!is.na(`[Participar en fiestas costumbristas]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_52.1 = costumbristas %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Participar en fiestas costumbristas]`, y = prop, fill = `[Participar en fiestas costumbristas]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_52.1.png", p_52.1)

# Gráfico de torta
p_52.2 = costumbristas %>% 
  arrange(desc(`[Participar en fiestas costumbristas]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Participar en fiestas costumbristas]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_52.2.png", p_52.2)

#Tabla word: 
tab_df(costumbristas, file = "cuadro_participar_en_fiestas_costumbristas.doc")




##53. [Vestir trajes tradicionales (aunque se para una sessión de fotos)]##

trajes= encuesta_turistas %>%
  group_by(`[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`) %>%
  filter(!is.na(`[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_53.1 = trajes %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`, y = prop, fill = `[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_53.1.png", p_53.1)

# Gráfico de torta
p_53.2 = trajes %>% 
  arrange(desc(`[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Vestir trajes tradicionales (aunque se para una sessión de fotos)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_53.2.png", p_53.2)

#Tabla word: 
tab_df(trajes, file = "cuadro_vestir_trajes_tradicionales.doc")




##54. [Caminatas y/o ciclísmo]##

caminata_ciclismo= encuesta_turistas %>%
  group_by(`[Caminatas y/o ciclísmo]`) %>%
  filter(!is.na(`[Caminatas y/o ciclísmo]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_54.1 = caminata_ciclismo %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Caminatas y/o ciclísmo]`, y = prop, fill = `[Caminatas y/o ciclísmo]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_54.1.png", p_54.1)

# Gráfico de torta
p_54.2 = caminata_ciclismo %>% 
  arrange(desc(`[Caminatas y/o ciclísmo]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Caminatas y/o ciclísmo]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_54.2.png", p_54.2)

#Tabla word: 
tab_df(caminata_ciclismo, file = "cuadro_caminata_ciclismo.doc")




##55. [Pesca deportiva (Catch and release)]##

pesca= encuesta_turistas %>%
  group_by(`[Pesca deportiva (Catch and release)]`) %>%
  filter(!is.na(`[Pesca deportiva (Catch and release)]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_55.1 = pesca %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Pesca deportiva (Catch and release)]`, y = prop, fill = `[Pesca deportiva (Catch and release)]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_55.1.png", p_55.1)

# Gráfico de torta
p_55.2 = pesca %>% 
  arrange(desc(`[Pesca deportiva (Catch and release)]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Pesca deportiva (Catch and release)]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_55.2.png", p_55.2)

#Tabla word: 
tab_df(pesca, file = "cuadro_pesca_deportiva.doc")




##56. [Práctica deportes naúticos]

nauticos= encuesta_turistas %>%
  group_by(`[Práctica deportes naúticos]`) %>%
  filter(!is.na(`[Práctica deportes naúticos]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_56.1 = nauticos %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Práctica deportes naúticos]`, y = prop, fill = `[Práctica deportes naúticos]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_56.1.png", p_56.1)

# Gráfico de torta
p_56.2 = nauticos %>% 
  arrange(desc(`[Práctica deportes naúticos]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Práctica deportes naúticos]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_56.2.png", p_56.2)

#Tabla word: 
tab_df(nauticos, file = "cuadro_deportes_nauticos.doc")




##57. [Probar platos típicos o exclusivos de la zona]##

platos= encuesta_turistas %>%
  group_by(`[Probar platos típicos o exclusivos de la zona]`) %>%
  filter(!is.na(`[Probar platos típicos o exclusivos de la zona]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_57.1 = platos %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Probar platos típicos o exclusivos de la zona]`, y = prop, fill = `[Probar platos típicos o exclusivos de la zona]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_57.1.png", p_57.1)

# Gráfico de torta
p_57.2 = platos %>% 
  arrange(desc(`[Probar platos típicos o exclusivos de la zona]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Probar platos típicos o exclusivos de la zona]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_57.2.png", p_57.2)

#Tabla word: 
tab_df(platos, file = "cuadro_probar_platos_tipicos.doc")




##58. [Aprender a prepara platos típicos de la zona]##

preparar= encuesta_turistas %>%
  group_by(`[Aprender a prepara platos típicos de la zona]`) %>%
  filter(!is.na(`[Aprender a prepara platos típicos de la zona]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_58.1 = preparar %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Aprender a prepara platos típicos de la zona]`, y = prop, fill = `[Aprender a prepara platos típicos de la zona]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_58.1.png", p_58.1)

# Gráfico de torta
p_58.2 = preparar %>% 
  arrange(desc(`[Aprender a prepara platos típicos de la zona]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Aprender a prepara platos típicos de la zona]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_58.2.png", p_58.2)

#Tabla word: 
tab_df(preparar, file = "cuadro_preparar_platos_tipicos.doc")




##59. [Participar en talleres de artesanía]##

talleres= encuesta_turistas %>%
  group_by(`[Participar en talleres de artesanía]`) %>%
  filter(!is.na(`[Participar en talleres de artesanía]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_59.1 = talleres %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Participar en talleres de artesanía]`, y = prop, fill = `[Participar en talleres de artesanía]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_59.1.png", p_59.1)

# Gráfico de torta
p_59.2 = talleres %>% 
  arrange(desc(`[Participar en talleres de artesanía]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Participar en talleres de artesanía]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_59.2.png", p_59.2)

#Tabla word: 
tab_df(talleres, file = "cuadro_talleres_de_artesania.doc")




##60. [Dormir en casa de una familia de la comunidad]##

dormir= encuesta_turistas %>%
  group_by(`[Dormir en casa de una familia de la comunidad]`) %>%
  filter(!is.na(`[Dormir en casa de una familia de la comunidad]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_60.1 = dormir %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `[Dormir en casa de una familia de la comunidad]`, y = prop, fill = `[Dormir en casa de una familia de la comunidad]`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_60.1.png", p_60.1)

# Gráfico de torta
p_60.2 = dormir %>% 
  arrange(desc(`[Dormir en casa de una familia de la comunidad]`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `[Dormir en casa de una familia de la comunidad]`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_60.2.png", p_60.2)

#Tabla word: 
tab_df(dormir, file = "cuadro_dormir_en_casa_de_una_familia_de_la_comunidad.doc")




##61. ¿Repetiría su visita##

repetiria= encuesta_turistas %>%
  group_by(`¿Repetiría su visita`) %>%
  filter(!is.na(`[Dormir en casa de una familia de la comunidad]`))%>%
  summarise(n=n()) %>%
  mutate(prop= n/sum(n))

# Gráfico de barras
p_61.1 = repetiria %>%
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(x = `¿Repetiría su visita`, y = prop, fill = `¿Repetiría su visita`))+
  geom_col(col = "black")+
  geom_text(aes(y = prop, label = paste0(prop, "%")), color = "black", size = 6)

# Guardar el gráfico: 
ggsave("p_61.1.png", p_61.1)

# Gráfico de torta
p_61.2 = repetiria %>% 
  arrange(desc(`¿Repetiría su visita`)) %>% 
  mutate(prop = round(prop*100,1), 
         ypos = cumsum(prop)-0.5*prop) %>% 
  ggplot(aes(x = "", y = prop, fill = `¿Repetiría su visita`))+
  geom_bar(stat = "identity", position = "stack")+
  coord_polar("y", start = 0)+
  theme_void()+
  geom_text(aes(y = prop, label = paste0(prop,"%")), color = "white", size=6)

# Guardar el gráfico: 
ggsave("p_61.2.png", p_61.2)

#Tabla word: 
tab_df(repetiria, file = "cuadro_repetiria_su_visita.doc")




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

