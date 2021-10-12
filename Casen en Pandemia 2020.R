library(haven)
Casen_en_Pandemia_2020_SPSS <- read_sav("Casen en Pandemia 2020 SPSS.sav")
library(dplyr)
library(srvyr)

casen_2020 = Casen_en_Pandemia_2020_SPSS %>% 
  as_survey_design(ids = varunit, strata = varstrat, weights = expr)

ing_med = casen_2020 %>% 
  group_by(region, sexo) %>% 
  summarise(ingreso_mediano = survey_median(ytrabajocor, na.rm= TRUE, vartype = "se"))


ing_med2 = Casen_en_Pandemia_2020_SPSS %>%
  group_by(region, sexo) %>%
  summarise(ingreso_mediano = median(ytrabajocor, na.rm = TRUE))
