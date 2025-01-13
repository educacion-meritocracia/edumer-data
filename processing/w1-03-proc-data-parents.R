#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for EDUMER Parents Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Parents Survey Wave 1         
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               codebook,
               haven)

options(scipen=999)

# 2. Data --------------------------------------------------------------

db <- rio::import(file = here("input","data", "original", "310524_BDD_edumer.sav")) %>% 
  as_tibble()

names(db)

db_or_label <- sjlabelled::get_label(db)

# 3. Processing -----------------------------------------------------------

clean_labels <- function(x) {
  current_label <- sjlabelled::get_label(x)
  if (!is.null(current_label)) {
    new_label <- stringr::str_remove(current_label, "^\\s*\\d+\\.?\\d*\\s*-?\\s*")
    x <- sjlabelled::set_label(x, label = new_label)
  }
  return(x)
}

# Select ----

db3 <- db[,c(1,4,180:246)] %>%  # only parents variables 
  select(-starts_with("T")) %>% 
  janitor::clean_names()

# Colnames ----
names(db3)

db3 <- db3 %>% 
  rename(id_apoderado = sbj_num_apoderado,
         fecha_apoderado = date_apoderado,
         consentimiento_apoderado = acepta_apoderado,
         id_estudiante= sbj_num,
         d2_estudiante = d2)

names(db3)
glimpse(db3)

names(which(colSums(is.na(db3)) > 0))

db3 <- db3 %>% 
  rename_with(~str_remove(.,"_apoderado")) # just for operate, then restored

# Manipulation ----

## ID 
frq(db3$id)
get_label(db3$id)
db3$id <- sjlabelled::set_label(db3$id, label = "Identificador único apoderado")


# id_estudiante

sum(duplicated(db3$id_estudiante))

db3$id_estudiante[duplicated(db3$id_estudiante)]# only NA

## Fecha
frq(db3$fecha)
class(db3$fecha)
db3$fecha <- lubridate::as_datetime(db3$fecha)
get_label(db3$fecha)
db3$fecha <- sjlabelled::set_label(db3$fecha, label = "Fecha de aplicación")

## Consentimiento
frq(db3$consentimiento)
get_label(db3$consentimiento)
db3$consentimiento <- sjlabelled::set_label(db3$consentimiento, label = "Autorización adulto para participación de menor de edad en encuesta")
get_labels(db3$consentimiento)

## d2
frq(db3$d2)
get_label(db3$d2)
db3$d2 <- sjlabelled::set_label(db3$d2, label = "Establecimiento eduacional al que asiste su hijo/a")
get_labels(db3$d2)

## d3
frq(db3$d3)
get_label(db3$d3)
db3$d3 <- sjlabelled::set_label(db3$d3, label = "Ingresar curso en el que se encuentra actualmente su hijo/a")
get_labels(db3$d3)

## d3_nueva
frq(db3$d3_nueva)
get_label(db3$d3_nueva)
db3$d3_nueva <- sjlabelled::set_label(db3$d3_nueva, label = "Ingresar curso en el que se encuentra actualmente su hijo/a [Texto completo]")
get_labels(db3$d3_nueva)

## d6
frq(db3$d6)
get_label(db3$d6)
db3$d6 <- sjlabelled::set_label(db3$d6, label = "Relación de parentezco con pupilo/a")
get_labels(db3$d6)

## d6_otro
frq(db3$d6_otro)
get_label(db3$d6_otro)
db3$d6_otro <- sjlabelled::set_label(db3$d6_otro, label = "Relación de parentezco con pupilo/a [Otro (especifíque)]")
get_labels(db3$d6_otro)

## p1_

db3 %>% 
  select(starts_with("p1_")) %>% 
  frq(.)

labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db3 <- db3 %>% 
  mutate(across(.cols = starts_with("p1_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db3 %>% 
  select(starts_with("p1_")) %>% 
  get_label()

db3$p1_1 <- sjlabelled::set_label(db3$p1_1, 
                                  label = "En Chile, las personas son recompensadas por sus esfuerzos")

db3$p1_2 <- sjlabelled::set_label(db3$p1_2, 
                                  label = "En Chile, las personas son recompensadas por su inteligencia y habilidad")

db3$p1_3 <- sjlabelled::set_label(db3$p1_3, 
                                  label = "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida")

db3$p1_4 <- sjlabelled::set_label(db3$p1_4, 
                                  label = "En Chile, quienes tienen buenos contactos les va mejor en la vida")

db3$p1_5 <- sjlabelled::set_label(db3$p1_5, 
                                  label = "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

db3$p1_6 <- sjlabelled::set_label(db3$p1_6, 
                                  label = "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")

db3$p1_7 <- sjlabelled::set_label(db3$p1_7, 
                                  label = "Está bien que quienes tienen padres ricos les vaya bien en la vida")

db3$p1_8 <- sjlabelled::set_label(db3$p1_8, 
                                  label = "Está bien que quienes tienen buenos contactos les vaya bien en la vida")

db3$p1_9 <- sjlabelled::set_label(db3$p1_9, 
                                  label = "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

db3$p1_10 <- sjlabelled::set_label(db3$p1_10, 
                                   label = "En Chile, todas las personas obtienen lo que merecen")


## p2_

db3 %>% 
  select(starts_with("p2_")) %>% 
  frq(.)

db3 <- db3 %>% 
  mutate(across(.cols = starts_with("p2_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db3 %>% 
  select(starts_with("p2_")) %>% 
  get_label()

db3$p2_1 <- sjlabelled::set_label(db3$p2_1, 
                                  label = "En esta escuela, quienes se esfuerzan obtienen buenas notas")

db3$p2_2 <- sjlabelled::set_label(db3$p2_2, 
                                  label = "En esta escuela, quienes son inteligentes obtienen buenas notas")

db3$p2_3 <- sjlabelled::set_label(db3$p2_3, 
                                  label = "En esta escuela, los/as estudiantes obtienen las notas que merecen")

## p3
frq(db3$p3)
get_label(db3$p3)

db3 <- db3 %>% 
  mutate(
    p3 = if_else(p3 == 0, 1, p3),
    p3 = sjlabelled::set_label(p3, label = "¿Qué es más importante para obtener buenas notas, el esfuerzo o la inteligencia? Escala 1 (esfuerzo) a 10 (inteligencia)")
  )

## p4_1 y p4_2

db3 %>% 
  select(p4_1, p4_2) %>% 
  frq(.)

db3 <- db3 %>% 
  mutate(across(.cols = c(p4_1, p4_2),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db3 %>% 
  select(p4_1, p4_2) %>% 
  get_label()

db3$p4_1 <- sjlabelled::set_label(db3$p4_1, 
                                  label = "Las personas pueden esforzarse más si lo intentan")

db3$p4_2 <- sjlabelled::set_label(db3$p4_2, 
                                  label = "Las personas pueden ser más inteligentes si lo intentan")

## p5_
db3 %>% 
  select(starts_with("p5_")) %>% 
  frq(.)

db3 <- db3 %>% 
  mutate(across(.cols = starts_with("p5_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db3 %>% 
  select(starts_with("p5_")) %>% 
  get_label()

db3$p5_1 <- sjlabelled::set_label(db3$p5_1, 
                                  label = "Las diferencias económicas entre ricos y pobres en Chile son demasiado grandes")


db3$p5_2 <- clean_labels(db3$p5_2)
db3$p5_3 <- clean_labels(db3$p5_3)
db3$p5_4 <- clean_labels(db3$p5_4)
db3$p5_5 <- clean_labels(db3$p5_5)
db3$p5_6 <- clean_labels(db3$p5_6)

## p6_

db3 %>% 
  select(starts_with("p6_")) %>% 
  frq(.)

labels2 <- c("Nada importante" = 1, 
             "Algo importante" = 2, 
             "Importante" = 3, 
             "Muy importante" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db3 <- db3 %>% 
  mutate(across(.cols = starts_with("p6_"),.fns = ~ sjlabelled::set_labels(., labels = labels2)))

db3 %>% 
  select(starts_with("p6_")) %>% 
  get_label()

db3$p6_1 <- sjlabelled::set_label(db3$p6_1, label = "Votar en todas las elecciones nacionales")
db3$p6_2 <- clean_labels(db3$p6_2)
db3$p6_3 <- clean_labels(db3$p6_3)
db3$p6_4 <- clean_labels(db3$p6_4)
db3$p6_5 <- clean_labels(db3$p6_5)
db3$p6_6 <- clean_labels(db3$p6_6)
db3$p6_7 <- clean_labels(db3$p6_7)
db3$p6_8 <- clean_labels(db3$p6_8)


## p7_

db3 %>% 
  select(starts_with("p7_")) %>% 
  frq(.)

db3 <- db3 %>% 
  mutate(
    across(.cols = starts_with("p7_"),
           .fns = ~ sjlabelled::set_labels(., labels = c("Sí" = 1,
                                                         "No" = 2,
                                                         "No responde" = 99)))
  )

db3 %>% 
  select(starts_with("p7_")) %>% 
  get_label()

db3$p7_1 <- sjlabelled::set_label(db3$p7_1, label = "Participar en una marcha o manifestación pacífica")
db3$p7_2 <- clean_labels(db3$p7_2)
db3$p7_3 <- clean_labels(db3$p7_3)

## p8_
db3 %>% 
  select(starts_with("p8_")) %>% 
  frq(.)

labels3 <- c("Nunca" = 1, 
             "Una vez al año" = 2, 
             "Una vez al mes" = 3, 
             "Semanalmente" = 4,
             "Todos los días" = 5,
             "No sabe" = 88, 
             "No responde" = 99)

db3 <- db3 %>% 
  mutate(across(.cols = starts_with("p8_"),.fns = ~ sjlabelled::set_labels(., labels = labels3)))

db3 %>% 
  select(starts_with("p8_")) %>% 
  get_label()

db3$p8_1 <- sjlabelled::set_label(db3$p8_1, label = "Buscar sitios web de Internet para encontrar información sobre temas políticos o sociales")
db3$p8_2 <- clean_labels(db3$p8_2)
db3$p8_3 <- clean_labels(db3$p8_3)
db3$p8_4 <- clean_labels(db3$p8_4)
db3$p8_5 <- clean_labels(db3$p8_5)
db3$p8_6 <- clean_labels(db3$p8_6)

## p9_

db3 %>% 
  select(starts_with("p9_")) %>% 
  frq(.)

db3 <- db3 %>% 
  mutate(
    across(.cols = starts_with("p9_"),
           .fns = ~ sjlabelled::set_labels(., labels = c("Sí" = 1,
                                                         "No" = 2,
                                                         "No responde" = 99)))
  )

db3 %>% 
  select(starts_with("p9_")) %>% 
  get_label()

db3$p9_1 <- clean_labels(db3$p9_1)
db3$p9_2 <- clean_labels(db3$p9_2)
db3$p9_3 <- clean_labels(db3$p9_3)

## p10
frq(db3$p10)
db3$p10 <- sjlabelled::set_labels(db3$p10, 
                                  labels = c("Ninguna importancia" = 1, 
                                             "Algo de importancia" = 2, 
                                             "Mediana importancia" = 3, 
                                             "Mucha importancia" = 4,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db3$p10)
db3$p10 <- clean_labels(db3$p10)


## p11
frq(db3$p11)
get_label(db3$p11)
db3$p11 <- sjlabelled::set_label(db3$p11, label = "Género apoderado")

## p12_mes y p2_ano
db3 %>% 
  select(p12_mes, p12_anio) %>% 
  frq()

db3 %>% 
  select(p12_mes, p12_anio) %>% 
  get_label()

db3$p12_mes <- clean_labels(db3$p12_mes)
db3$p12_anio <- clean_labels(db3$p12_anio)

## p12_1
frq(db3$p12_1)

db3$p12_1 <- sjlabelled::set_labels(db3$p12_1, 
                                    labels = c("Sin estudios formales" = 1,                                                                                                
                                               "Básica incompleta / primaria o preparatoria incompleta" = 2,                                                              
                                               "Básica completa / primaria o preparatoria completa" = 3,                                                                  
                                               "Media científico humanista o media técnico profesional incompleta / humanidades incompleta" = 4,                          
                                               "Media científico humanista o media técnico profesional completa / humanidades completa" = 5,                              
                                               "Instituto técnico (CFT) o instituto profesional incompleto (carreras 1 a 3 años)" = 6,                                    
                                               "Instituto técnico (CFT) o instituto profesional completo (carreras 1 a 3 años) / hasta suboficial de FFAA/Carabineros" = 7,
                                               "Universitaria incompleta (carreras 4 o más años)" = 8,                                                                    
                                               "Universitaria completa (carreras 4 años o más) / Oficial de FF/Carabineros" = 9,                                          
                                                "Postgrado (postítulo, master, magíster, doctor)" = 10,                                                                     
                                                "No sabe" = 88,                                                                                                             
                                                "No responde" = 99))

get_label(db3$p12_1)
db3$p12_1 <- clean_labels(db3$p12_1)

## p13
frq(db3$p13)

db3 <- db3 %>% 
  mutate(p13 = if_else(p13 == 8, 88, p13),
         p13 = sjlabelled::set_labels(p13, 
                                      labels = c("Derecha" = 1, 
                                                 "Centro Derecha" = 2, 
                                                 "Centro" = 3, 
                                                 "Centro Izquierda" = 4,
                                                 "Izquierda" = 5,
                                                 "Independiente" = 6,
                                                 "Ninguna" = 7,
                                                 "No sabe" = 88, 
                                                 "No responde" = 99))
           )

get_label(db3$p13)
db3$p13 <- sjlabelled::set_label(db3$p13, label = "Posición política (eje izq. y der.)")


## p15
frq(db3$p15)
db3$p15 <- sjlabelled::set_labels(db3$p15, 
                                  labels = c("Mapuche" = 1, 
                                             "Aimara" = 2, 
                                             "Rapa Nui" = 3, 
                                             "Quechua" = 4,
                                             "No, a ninguno de las anteriores" = 5,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db3$p15)
db3$p15 <- sjlabelled::set_label(db3$p15, label = "¿Se considera perteneciente a alguno de los siguientes pueblos originarios?")

## p18
frq(db3$p18)
db3$p18 <- sjlabelled::set_labels(db3$p18, 
                                  labels = c("Católico" = 1, 
                                             "Evangélico" = 2, 
                                             "Protestante" = 3, 
                                             "Judío" = 4,
                                             "Creyente no adherente" = 5,
                                             "Ateo" = 6,
                                             "Agnóstico" = 7,
                                             "Ninguna" = 8,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db3$p18)
db3$p18 <- clean_labels(db3$p18)

## p19
frq(db3$p19)
db3$p19 <- sjlabelled::set_labels(db3$p19, 
                                  labels = c("Nunca" = 1, 
                                             "Menos de una vez al año" = 2, 
                                             "Una vez al año" = 3, 
                                             "Al menos una vez al año" = 4,
                                             "Al menos una vza la semana" = 5,
                                             "Diario" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db3$p19)
db3$p19 <- clean_labels(db3$p19)

## p20
frq(db3$p20)
db3$p20 <- sjlabelled::set_labels(db3$p20, 
                                  labels = c("Entre 0 y 10 libros" = 1, 
                                             "Entre 11 y 25 libros" = 2, 
                                             "Entre 26 y 100 libros" = 3, 
                                             "Entre 100 y 200 libros" = 4,
                                             "Entre 201 y 500 libros" = 5,
                                             "Más de 500 libros" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db3$p20)
db3$p20 <- clean_labels(db3$p20)

#4. Save and remove ----------------------------------------------------

db_parents <- db3 %>% 
  rename(p14 = p15, 
         p15 = p18,
         p16 = p19,
         p17 = p20) %>% 
  rename_with(~ paste0(.,"_apoderado")) %>% 
  janitor::clean_names() %>% 
  as.data.frame()

base::save(db_parents, file = here("output/data/db_proc_parents.RData"))
haven::write_dta(db_parents, path = here("output/data/db_proc_parents.dta"))
haven::write_sav(db_parents, path = here("output/data/db_proc_parents.sav"))

rm(list = ls(pattern = "^labels[0-9]+$"))