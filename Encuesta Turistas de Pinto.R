#Encuesta a turistas de Pinto#

library(dplyr)
names(X20211013_Encuesta_a_Turistas_Pinto_modificada)

## 1.Género##
genero<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select(Género)
  
table(genero)
prop.table(table (genero))
names(which(table(genero)==max(table(genero))))
barplot(table(genero))
        

##2. Edad##
edad<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select(Edad)
table(edad)
prop.table(table (edad))
names(which(table(edad)==max(table(edad))))
barplot(table(edad))


##3. ¿A qué se dedica actualmente?##
ocupacion<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("¿A qué se dedica actualmente")
table(ocupacion)
prop.table(table (ocupacion))
names(which(table(ocupacion)==max(table(ocupacion))))


##4. Indique el rango de ingresos que percibe su núcleo familiar##
ingresos<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Indique el rango de ingresos que percibe su núcleo familiar")
table(ingresos)
prop.table(table (ingresos))
names(which(table(ingresos)==max(table(ingresos))))


##5. Nº de niños (de 0 a 12 años)]##
Nniños<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de niños (de 0 a 12 años)]") %>%
  filter("Nº de niños (de 0 a 12 años)]"!=0)
table(Nniños)
(prop.table(table (Nniños)))
names(which(table(Nniños)==max(table(Nniños))))


##6. Nº de niñas (de 0 a 12 años)]##
Nniñas<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de niñas (de 0 a 12 años)]")%>%
  filter("Nº de niñas (de 0 a 12 años)]"!=0)
table(Nniñas)
(prop.table(table (Nniñas)))
names(which(table(Nniñas)==max(table(Nniñas))))

##7. Nº de adolescentes varones (de 13 a 17 años)]##
adolecentesv<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adolescentes varones (de 13 a 17 años)]")%>%
  filter("Nº de adolescentes varones (de 13 a 17 años)]"!=0)
table(adolecentesv)
(prop.table(table (adolecentesv)))
names(which(table(adolecentesv)==max(table(adolecentesv))))


##8. Nº de adolescentes damas (de 13 a 17 años)]##
adolecentesd<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adolescentes damas (de 13 a 17 años)]")%>%
  filter("Nº de adolescentes damas (de 13 a 17 años)]"!=0)
table(adolecentesd)
(prop.table(table (adolecentesd)))
names(which(table(adolecentesd)==max(table(adolecentesd))))


##9. Nº de adultos hombres (de 18 a 59 años)]##
adultosh<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adultos hombres (de 18 a 59 años)]")%>%
  filter("Nº de adultos hombres (de 18 a 59 años)]"!=0)
table(adultosh)
(prop.table(table (adultosh)))
names(which(table(adultosh)==max(table(adultosh))))

##10. Nº de adultos mujeres (de 18 a 59 años)]##
adultosm<- X20211013_Encuesta_a_Turistas_Pinto_modificada %>%
  select("Nº de adultos mujeres (de 18 a 59 años)]")%>%
  filter("Nº de adultos mujeres (de 18 a 59 años)]"!=0)
table(adultosm)
(prop.table(table (adultosm)))
names(which(table(adultosm)==max(table(adultosm))))








