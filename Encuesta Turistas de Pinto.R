#Encuesta a turistas de Pinto#

library(dplyr)
X20211013_Encuesta_a_Turistas_Pinto_modificada
names(X20211013_Encuesta_a_Turistas_Pinto_modificada)

##ESTADISTICA DESCRIPTIVA##

## 1.Género##
genero<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select(Género)
table(genero)
prop.table(table (genero))*100
names(which(table(genero)==max(table(genero))))
barplot(table(genero))
        

##2. Edad##
edad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select(Edad)
table(edad)
prop.table(table (edad))*100
names(which(table(edad)==max(table(edad))))
barplot(table(edad))


##3. ¿A qué se dedica actualmente?##
ocupacion<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿A qué se dedica actualmente")
table(ocupacion)
prop.table(table (ocupacion))*100
names(which(table(ocupacion)==max(table(ocupacion))))


##4. Indique el rango de ingresos que percibe su núcleo familiar##
ingresos<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Indique el rango de ingresos que percibe su núcleo familiar")
table(ingresos)
prop.table(table (ingresos))*100
names(which(table(ingresos)==max(table(ingresos))))


##5. Nº de niños (de 0 a 12 años)]##
Nniños<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de niños (de 0 a 12 años)]") %>%
  filter("Nº de niños (de 0 a 12 años)]"!=0)
table(Nniños)
(prop.table(table (Nniños)))*100
names(which(table(Nniños)==max(table(Nniños))))


##6. Nº de niñas (de 0 a 12 años)]##
Nniñas<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de niñas (de 0 a 12 años)]")%>%
  filter("Nº de niñas (de 0 a 12 años)]"!=0)
table(Nniñas)
(prop.table(table (Nniñas)))*100
names(which(table(Nniñas)==max(table(Nniñas))))

##7. Nº de adolescentes varones (de 13 a 17 años)]##
adolecentesv<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adolescentes varones (de 13 a 17 años)]")%>%
  filter("Nº de adolescentes varones (de 13 a 17 años)]"!=0)
table(adolecentesv)
(prop.table(table (adolecentesv)))*100
names(which(table(adolecentesv)==max(table(adolecentesv))))


##8. Nº de adolescentes damas (de 13 a 17 años)]##
adolecentesd<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adolescentes damas (de 13 a 17 años)]")%>%
  filter("Nº de adolescentes damas (de 13 a 17 años)]"!=0)
table(adolecentesd)
(prop.table(table (adolecentesd)))*100
names(which(table(adolecentesd)==max(table(adolecentesd))))


##9. Nº de adultos hombres (de 18 a 59 años)]##
adultosh<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adultos hombres (de 18 a 59 años)]")%>%
  filter("Nº de adultos hombres (de 18 a 59 años)]"!=0)
table(adultosh)
(prop.table(table (adultosh)))*100
names(which(table(adultosh)==max(table(adultosh))))



##10. Nº de adultos mujeres (de 18 a 59 años)]##
adultosm<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adultos mujeres (de 18 a 59 años)]")%>%
  filter("Nº de adultos mujeres (de 18 a 59 años)]"!=0)
table(adultosm)
(prop.table(table (adultosm)))*100
names(which(table(adultosm)==max(table(adultosm))))


##11. Nº de adultos mayores hombres (de 60 o más años)]##
mayoresh<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adultos mayores hombres (de 60 o más años)]")%>%
  filter("Nº de adultos mayores hombres (de 60 o más años)]"!=0)
table(mayoresh)
(prop.table(table (mayoresh)))*100
names(which(table(mayoresh)==max(table(mayoresh))))

##12. Nº de adultos mayores mujeres (de 60 o más años)]##
mayoresm<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adultos mayores mujeres (de 60 o más años)]")%>%
  filter("Nº de adultos mayores mujeres (de 60 o más años)]"!=0)
table(mayoresm)
(prop.table(table (mayoresm)))*100
names(which(table(mayoresm)==max(table(mayoresm))))


##13. ¿Cuál es su nivel educativo##
educacion<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿Cuál es su nivel educativo")
table(educacion)
(prop.table(table (educacion)))*100
names(which(table(educacion)==max(table(educacion))))


##14. Podría indicar su estado civil##
estado<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Podría indicar su estado civil")
table(estado)
(prop.table(table (estado)))*100
names(which(table(estado)==max(table(estado))))


##15. Indique si es Chileno o Extranjero##
nacionalidad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Indique si es Chileno o Extranjero")
table(nacionalidad)
(prop.table(table (nacionalidad)))*100
names(which(table(nacionalidad)==max(table(nacionalidad))))


##16. Especifique su ciudad de residencia##
ciudad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Especifique su ciudad de residencia")%>%
  filter("Especifique su ciudad de residencia"!=0)
table(ciudad)
(prop.table(table (ciudad)))*100
names(which(table(ciudad)==max(table(ciudad))))


##17. Especifique su país de residencia##
pais<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Especifique su país de residencia")%>%
  filter("Especifique su país de residencia"!=0)
table(pais)
(prop.table(table (pais)))*100
names(which(table(pais)==max(table(pais))))


##18 19 20 21 22 stand by##




##23. ¿Cuántas veces ha visitado la comuna##
visitas<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿Cuántas veces ha visitado la comuna")
table(visitas)
(prop.table(table (visitas)))*100
names(which(table(visitas)==max(table(visitas))))


##24. ¿Cuántas personas lo/a acompañaron en su último viaje a la comuna## sdfsdafadfasd
acompañantes<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿Cuántas personas lo/a acompañaron en su último viaje a la comuna")
table(acompañantes)
(prop.table(table (acompañantes)))*100
names(which(table(acompañantes)==max(table(acompañantes))))

##25. stand by##


##26. ¿Cuánta noches pernoctó en la comuna##
noches<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿Cuánta noches pernoctó en la comuna")
table(noches)
(prop.table(table (noches)))*100
names(which(table(noches)==max(table(noches))))


##27 28 29 stand by##


##30. [Alojamiento]##
alojamiento<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Alojamiento]")%>%
  filter("[Alojamiento]"!="N/A")
table(alojamiento)
(prop.table(table (alojamiento)))*100
names(which(table(alojamiento)==max(table(alojamiento))))


##31. [Alimentación]##
alimentacion<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Alimentación]")%>%
  filter("[Alimentación]"!="N/A")
table(alimentacion)
(prop.table(table (alojamiento)))*100
names(which(table(alimentacion)==max(table(alimentacion))))


##32. [Transporte]##
trasporte<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Transporte]")%>%
  filter("[Transporte]"!="N/A")
table(trasporte)
(prop.table(table (trasporte)))*100
names(which(table(trasporte)==max(table(trasporte))))


##33. [Servicios complementarios]##
servicios<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Servicios complementarios]")%>%
  filter("[Servicios complementarios]"!="N/A")
table(servicios)
(prop.table(table (servicios)))*100
names(which(table(servicios)==max(table(servicios))))

##34. [Vida nocturna]##
nocturna<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Vida nocturna]")
table(nocturna)
(prop.table(table (nocturna)))*100
names(which(table(nocturna)==max(table(nocturna))))


##35. [Comercio]##
comercio<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Comercio]")%>%
  filter("[Comercio]"!="N/A")
table(comercio)
(prop.table(table (comercio)))*100
names(which(table(comercio)==max(table(comercio))))


##36. [Actividades programadas por las instituciones de la comuna]##
actividades<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Actividades programadas por las instituciones de la comuna]")
table(actividades)
(prop.table(table (actividades)))*100
names(which(table(actividades)==max(table(actividades))))


##37. [Estado de caminos]##
caminos<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Estado de caminos]")%>%
  filter("[Estado de caminos]"!="N/A")
table(caminos)
(prop.table(table (caminos)))*100
names(which(table(caminos)==max(table(caminos))))


##38. [Señalización vial]##
señalizacion<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Señalización vial]")%>%
  filter("[Señalización vial]"!="N/A")
table(señalizacion)
(prop.table(table (señalizacion)))*100
names(which(table(señalizacion)==max(table(señalizacion))))

##39. [Señalética turística]##
señaleticat<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Señalética turística]")%>%
  filter("[Señalética turística]"!="N/A")
table(señaleticat)
(prop.table(table (señaleticat)))*100
names(which(table(señaleticat)==max(table(señaleticat))))


##40. [Accesos para personas con movilidad limitada]##
accesos<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Accesos para personas con movilidad limitada]")%>%
  filter("[Accesos para personas con movilidad limitada]"!="N/A")
table(accesos)
(prop.table(table (accesos)))*100
names(which(table(accesos)==max(table(accesos))))


##41. [Paisaje natural]##
paisaje<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Paisaje natural]")
table(paisaje)
(prop.table(table (paisaje)))*100
names(which(table(paisaje)==max(table(paisaje))))


##42. [Amabilidad de los residentes]##
amabilidad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Amabilidad de los residentes]")
table(amabilidad)
(prop.table(table (amabilidad)))*100
names(which(table(amabilidad)==max(table(amabilidad))))


##43. [Relación Precio - Calidad] revisar##
precio_calidad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Relación Precio - Calidad]")
table(precio_calidad)
(prop.table(table (precio_calidad)))*100
names(which(table(precio_calidad)==max(table(precio_calidad))))


##44. [Limpieza del entorno]##
limpieza<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Limpieza del entorno]")
table(limpieza)
(prop.table(table (limpieza)))*100
names(which(table(limpieza)==max(table(limpieza))))


##45. [Mantención de infraestructura pública]##
mantecion<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Mantención de infraestructura pública]")
table(mantecion)
(prop.table(table (mantecion)))*100
names(which(table(mantecion)==max(table(mantecion))))


##46. [Percepción de seguridad en la comuna]##
seguridad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Percepción de seguridad en la comuna]")
table(seguridad)
(prop.table(table (seguridad)))*100
names(which(table(seguridad)==max(table(seguridad)))) 


##47. [Servicios de salud]##
salud<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Servicios de salud]")
table(salud)
(prop.table(table (salud)))*100
names(which(table(salud)==max(table(salud))))


##48 49 50 stand by



##51. [Actividades agrícolas]##
agricolas<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Actividades agrícolas]")
table(agricolas)
(prop.table(table (agricolas)))*100
names(which(table(agricolas)==max(table(agricolas))))

##52. [Participar en fiestas costumbristas]##
costumbristas<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Participar en fiestas costumbristas]")
table(costumbristas)
(prop.table(table (costumbristas)))*100
names(which(table(costumbristas)==max(table(costumbristas))))


##52. [Vestir trajes tradicionales (aunque se para una sessión de fotos)]##
trajes<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Vestir trajes tradicionales (aunque se para una sessión de fotos)]")
table(trajes)
(prop.table(table (trajes)))*100
names(which(table(trajes)==max(table(trajes))))


##53. [Caminatas y/o ciclísmo]##
caminata_ciclismo<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Caminatas y/o ciclísmo]")
table(caminata_ciclismo)
(prop.table(table (caminata_ciclismo)))*100
names(which(table(caminata_ciclismo)==max(table(caminata_ciclismo))))


##54. [Pesca deportiva (Catch and release)]##
pesca<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Pesca deportiva (Catch and release)]")
table(pesca)
(prop.table(table (pesca)))*100
names(which(table(pesca)==max(table(pesca))))


##55. [Práctica deportes naúticos]
nauticos<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Práctica deportes naúticos]")
table(nauticos)
(prop.table(table (nauticos)))*100
names(which(table(nauticos)==max(table(nauticos))))


##56. [Probar platos típicos o exclusivos de la zona]##
platos<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Probar platos típicos o exclusivos de la zona]")
table(platos)
(prop.table(table (platos)))*100
names(which(table(platos)==max(table(platos))))


##57. [Aprender a prepara platos típicos de la zona]##
preparar<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Aprender a prepara platos típicos de la zona]")
table(preparar)
(prop.table(table (preparar)))*100
names(which(table(preparar)==max(table(preparar))))

##58. [Participar en talleres de artesanía]##
talleres<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Participar en talleres de artesanía]")
table(talleres)
(prop.table(table (talleres)))*100
names(which(table(talleres)==max(table(talleres))))


##59. [Dormir en casa de una familia de la comunidad]##
dormir<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("[Dormir en casa de una familia de la comunidad]")
table(dormir)
(prop.table(table (dormir)))*100
names(which(table(dormir)==max(table(dormir))))


##60. ¿Repetiría su visita##
repetiria<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿Repetiría su visita")
table(repetiria)
(prop.table(table (repetiria)))*100
names(which(table(repetiria)==max(table(repetiria))))


#######CRUCE DE DATOS#########

encuesta<-X20211013_Encuesta_a_Turistas_Pinto_modificada

##EdadxGenero##
edadxgenero<-table(X20211013_Encuesta_a_Turistas_Pinto_modificada$Edad, X20211013_Encuesta_a_Turistas_Pinto_modificada$Género)
edadxgenero
prop.table(edadxgenero)*100
prop.table(edadxgenero,1)*100
prop.table(edadxgenero,2)*100

##Genero y Nivel Educativo
generoxeducacion<-table(X20211013_Encuesta_a_Turistas_Pinto_modificada$Género, X20211013_Encuesta_a_Turistas_Pinto_modificada$`¿Cuál es su nivel educativo`)
generoxeducacion
prop.table(generoxeducacion)*100
prop.table(generoxeducacion,1)*100
prop.table(generoxeducacion,2)*100

##edad y noches en la zona
edadxnoches<-table(X20211013_Encuesta_a_Turistas_Pinto_modificada$Edad, X20211013_Encuesta_a_Turistas_Pinto_modificada$`¿Cuánta noches pernoctó en la comuna`)
edadxnoches
prop.table(edadxnoches)*100
prop.table(edadxnoches,1)*100
prop.table(edadxnoches,2)*100

##genero y noches en la zona
generoxnoches<-table(X20211013_Encuesta_a_Turistas_Pinto_modificada$Género, X20211013_Encuesta_a_Turistas_Pinto_modificada$`¿Cuánta noches pernoctó en la comuna`)
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

