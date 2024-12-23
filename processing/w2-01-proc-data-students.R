#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for EDUMER Survey Wave 2
# Author: Andreas Laffert            
# Overview: EDUMER Survey Wave 2         
# Date: 29-10-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               magrittr,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               stringi,
               haven)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

db2 <- rio::import(file = here("input","data", "original", "221124 Edumer Ola2 ID_VF.sav")) %>% 
  as_tibble()

names(db2)

db2_or_label <- sjlabelled::get_label(db2)

# 3. Processing -----------------------------------------------------------

clean_labels <- function(x) {
  current_label <- sjlabelled::get_label(x)
  if (!is.null(current_label)) {
    new_label <- stringr::str_remove(current_label, "^\\s*\\d+\\.\\d+\\s|^\\s*\\d+\\.\\s")
    x <- sjlabelled::set_label(x, label = new_label)
  }
  return(x)
}

# Select ----

db2 <- db2 %>% 
  select(-starts_with("T_d")) 

# Colnames ----
names(db2)

db2 <- db2 %>% 
  rename(id_estudiante = SbjNum,
         fecha = Date,
         consentimiento_o2 = ACEPTA,
         nivel_estudiante_o2 = d3_o2)

names(db2)
glimpse(db2)

names(which(colSums(is.na(db2)) > 0))

# Manipulation ----

## ID 
frq(db2$id_estudiante)
sum(duplicated(db2$id_estudiante))
get_label(db2$id_estudiante)
db2$id_estudiante <- sjlabelled::set_label(db2$id_estudiante, label = "Identificador único estudiante")

## Fecha
frq(db2$fecha)
class(db2$fecha)
db2$fecha <- lubridate::as_datetime(db2$fecha)
get_label(db2$fecha)

## Consentimiento
frq(db2$consentimiento_o2)
get_label(db2$consentimiento_o2)
db2$consentimiento_o2 <- sjlabelled::set_label(db2$consentimiento_o2, label = "Autorización adulto para participación de menor de edad en encuesta")
get_labels(db2$consentimiento_o2)

## d1_03
frq(db2$d2_o2)
get_label(db2$d2_o2)
db2$d2_o2 <- sjlabelled::set_label(db2$d2_o2, label = "Establecimiento eduacional")
get_labels(db2$d2_o2)

## d3_def_o1
frq(db2$d3_def_o1)
db2$d3_def_o1 <- factor(tolower(db2$d3_def_o1), 
                     levels = c("6a", "6b", "6c", "7a", "7b", 
                                "1a", "1b", "1c", "2a", "2b", "2c"),
                     labels = c("6a", "6b", "6c", "7a", "7b", 
                                "1a", "1b", "1c", "2a", "2b", "2c"))

db2$d3_def_o1 <- sjlabelled::set_label(db2$d3_def_o1, label = "Curso al que perteneció en Ola 1")

## nivel_estudiante
frq(db2$nivel_estudiante_o2)
get_label(db2$nivel_estudiante_o2)
db2$nivel_estudiante_o2 <- sjlabelled::set_label(db2$nivel_estudiante_o2, label = "Nivel del estudiante")

## ************
## Experimento 
## ************

## T_pex_1_1_o2
frq(db2$T_pex_1_1_o2)
get_label(db2$T_pex_1_1_o2)
db2 <- rename(db2, exp_t1_a = T_pex_1_1_o2) 
db2$exp_t1_a <- sjlabelled::set_label(db2$exp_t1_a, label = "[Texto 1] Estudiante A (nota = 3.7). Décimas a regalar:")

## T_pex_1_2_o2
frq(db2$T_pex_1_2_o2)
get_label(db2$T_pex_1_2_o2)
db2 <- rename(db2, exp_t1_b = T_pex_1_2_o2) 
db2$exp_t1_b <- sjlabelled::set_label(db2$exp_t1_b, label = "[Texto 1] Estudiante B (nota = 3.7). Décimas a regalar:")

## T_pex_2_1_o2
frq(db2$T_pex_2_1_o2)
get_label(db2$T_pex_2_1_o2)
db2 <- rename(db2, exp_t2_a = T_pex_2_1_o2) 
db2$exp_t2_a <- sjlabelled::set_label(db2$exp_t2_a, label = "[Texto 2] Estudiante A (nota = 3.7) Se esfuerza más que la mayoría. Décimas a regalar:")

## T_pex_2_2_o2
frq(db2$T_pex_2_2_o2)
get_label(db2$T_pex_2_2_o2)
db2 <- rename(db2, exp_t2_b = T_pex_2_2_o2) 
db2$exp_t2_b <- sjlabelled::set_label(db2$exp_t2_b, label = "[Texto 2] Estudiante B (nota = 3.7) Se esfuerza menos que la mayoría. Décimas a regalar:")

## T_pex_3_1_o2
frq(db2$T_pex_3_1_o2)
get_label(db2$T_pex_3_1_o2)
db2 <- rename(db2, exp_t3_a = T_pex_3_1_o2) 
db2$exp_t3_a <- sjlabelled::set_label(db2$exp_t3_a, label = "[Texto 3] Estudiante A (nota = 3.7) Se esfuerza más que la mayoría - Su casa es pequeña, no tiene un espacio cómodo para estudiar. Décimas a regalar:")

## T_pex_3_2_o2
frq(db2$T_pex_3_2_o2)
get_label(db2$T_pex_3_2_o2)
db2 <- rename(db2, exp_t3_b = T_pex_3_2_o2) 
db2$exp_t3_b <- sjlabelled::set_label(db2$exp_t3_b, label = "[Texto 3] Estudiante B (nota = 3.7) Se esfuerza menos que la mayoría - Su casa es grande, tiene un espacio cómodo para estudiar. Décimas a regalar:")

## T_pex_4_1_o2
frq(db2$T_pex_4_1_o2)
get_label(db2$T_pex_4_1_o2)
db2 <- rename(db2, exp_t4_a = T_pex_4_1_o2) 
db2$exp_t4_a <- sjlabelled::set_label(db2$exp_t4_a, label = "[Texto 4] Estudiante A (nota = 3.7) Se esfuerza más que la mayoría - Su casa es pequeña, no tiene un espacio cómodo para estudiar. Décimas a regalar:")

## T_pex_4_2_o2
frq(db2$T_pex_4_2_o2)
get_label(db2$T_pex_4_2_o2)
db2 <- rename(db2, exp_t4_b = T_pex_4_2_o2) 
db2$exp_t4_b <- sjlabelled::set_label(db2$exp_t4_b, label = "[Texto 4] Estudiante B (nota = 3.7) Se esfuerza menos que la mayoría - Su casa es grande, tiene un espacio cómodo para estudiar. Décimas a regalar:")

## T_pex_4_3_o2
frq(db2$T_pex_4_3_o2)
get_label(db2$T_pex_4_3_o2)
db2 <- rename(db2, exp_t4_c = T_pex_4_3_o2) 
db2$exp_t4_c <- sjlabelled::set_label(db2$exp_t4_c, label = "[Texto 4] Estudiante C (nota = 3.7) Se esfuerza más que la mayoría - Su casa es grande, tiene un espacio cómodo para estudiar. Décimas a regalar:")

## T_pex_4_4_o2
frq(db2$T_pex_4_4_o2)
get_label(db2$T_pex_4_4_o2)
db2 <- rename(db2, exp_t4_d = T_pex_4_4_o2) 
db2$exp_t4_d <- sjlabelled::set_label(db2$exp_t4_d, label = "[Texto 4] Estudiante D (nota = 3.7) Se esfuerza menos que la mayoría - Su casa es pequeña, no tiene un espacio cómodo para estudiar. Décimas a regalar:")


## p1_

db2 %>% 
  select(starts_with("p1_")) %>% 
  frq(.)

labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p1_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p1_")) %>% 
  get_label()

db2$p1_1_o2 <- sjlabelled::set_label(db2$p1_1_o2, 
                                  label = "En Chile, las personas son recompensadas por sus esfuerzos")

db2$p1_2_o2 <- sjlabelled::set_label(db2$p1_2_o2, 
                                  label = "En Chile, las personas son recompensadas por su inteligencia y habilidad")

db2$p1_3_o2 <- sjlabelled::set_label(db2$p1_3_o2, 
                                  label = "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida")

db2$p1_4_o2 <- sjlabelled::set_label(db2$p1_4_o2, 
                                  label = "En Chile, quienes tienen buenos contactos les va mejor en la vida")

db2$p1_5_o2 <- sjlabelled::set_label(db2$p1_5_o2, 
                                  label = "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

db2$p1_6_o2 <- sjlabelled::set_label(db2$p1_6_o2, 
                                  label = "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")

db2$p1_7_o2 <- sjlabelled::set_label(db2$p1_7_o2, 
                                  label = "Está bien que quienes tienen padres ricos les vaya bien en la vida")

db2$p1_8_o2 <- sjlabelled::set_label(db2$p1_8_o2, 
                                  label = "Está bien que quienes tienen buenos contactos les vaya bien en la vida")

db2$p1_9_o2 <- sjlabelled::set_label(db2$p1_9_o2, 
                                  label = "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

db2$p1_10_o2 <- sjlabelled::set_label(db2$p1_10_o2, 
                                   label = "En Chile, todas las personas obtienen lo que merecen")

## p2_

db2 %>% 
  select(starts_with("p2_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p2_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p2_")) %>% 
  get_label()

db2$p2_1_o2 <- sjlabelled::set_label(db2$p2_1_o2, 
                                  label = "En esta escuela, quienes se esfuerzan obtienen buenas notas")

db2$p2_2_o2 <- sjlabelled::set_label(db2$p2_2_o2, 
                                  label = "En esta escuela, quienes son inteligentes obtienen buenas notas")

db2$p2_3_o2 <- sjlabelled::set_label(db2$p2_3_o2, 
                                  label = "En esta escuela, los/as estudiantes obtienen las notas que merecen")

## p3
frq(db2$p3_o2)
get_label(db2$p3_o2)

db2 <- db2 %>% 
  mutate(
    p3_o2 = if_else(p3_o2 == 0, 1, p3_o2),
    p3_o2 = sjlabelled::set_label(p3_o2, label = "¿Qué es más importante para obtener buenas notas, el esfuerzo o la inteligencia? Escala 1 (esfuerzo) a 10 (inteligencia)")
  )

## p4
frq(db2$p4_o2)
get_label(db2$p4_o2)
db2$p4_o2 <- clean_labels(db2$p4_o2)

## p5
frq(db2$p5_o2)
db2$p5_o2 <- sjlabelled::set_labels(db2$p5_o2, 
                                 labels = c("Menos de lo que merecía" = 1, 
                                            "Lo que merecía" = 2, 
                                            "Más de lo que merecía" = 3, 
                                            "No sabe" = 88, 
                                            "No responde" = 99))

get_label(db2$p5_o2)
db2$p5_o2 <- clean_labels(db2$p5_o2)

## p6
frq(db2$p6_o2)
get_label(db2$p6_o2)

db2 %>% 
  mutate(incon_p6_o2 = if_else(p5_o2 %in% c(1,3) & is.na(p6_o2), TRUE, FALSE)) %>% 
  tally(incon_p6_o2) 

db2 %>% 
  mutate(incon_p6_o2 = if_else(p5_o2 == 2 & !is.na(p6_o2), TRUE, FALSE)) %>% 
  tally(incon_p6_o2) # ok

db2$p6_o2 <- clean_labels(db2$p6_o2)

## p7
frq(db2$p7_o2)
db2$p7_o2 <- sjlabelled::set_labels(db2$p7_o2, 
                                 labels = c("Menos de lo que merezco" = 1, 
                                            "Lo que merezco" = 2, 
                                            "Más de lo que merezco" = 3, 
                                            "No sabe" = 88, 
                                            "No responde" = 99))
get_label(db2$p7_o2)
db2$p7_o2 <- clean_labels(db2$p7_o2)

## p8_

db2 %>% 
  select(starts_with("p8_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p8_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p8_")) %>% 
  get_label()

db2$p8_1_o2 <- sjlabelled::set_label(db2$p8_1_o2, "Las personas pueden esforzarse más si lo intentan")
db2$p8_2_o2 <- clean_labels(db2$p8_2_o2)

## p9_

db2 %>% 
  select(starts_with("p9_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p9_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p9_")) %>% 
  get_label()

db2$p9_1_o2 <- sjlabelled::set_label(db2$p9_1_o2, "Las diferencias económicas entre ricos y pobres en Chile son demasiado grandes")
db2$p9_2_o2 <- clean_labels(db2$p9_2_o2)
db2$p9_3_o2 <- clean_labels(db2$p9_3_o2)
db2$p9_4_o2 <- clean_labels(db2$p9_4_o2)
db2$p9_5_o2 <- clean_labels(db2$p9_5_o2)
db2$p9_6_o2 <- clean_labels(db2$p9_6_o2)

## check atencion
frq(db2$p10_check_atencion_o2)
db2$p10_check_atencion_o2 <- sjlabelled::set_labels(db2$p10_check_atencion_o2, 
                                             labels = labels1)

get_label(db2$p10_check_atencion_o2)
db2$p10_check_atencion_o2 <- clean_labels(db2$p10_check_atencion_o2)

## p10_

db2 %>% 
  select(starts_with("p11_")) %>% 
  frq(.)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p11_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db2 %>% 
  select(starts_with("p11_")) %>% 
  get_label()

db2$p11_1_o2 <- sjlabelled::set_label(db2$p11_1_o2, "Cuando sea adulto(a) podré participar en la política")
db2$p11_2_o2 <- clean_labels(db2$p11_2_o2)

## p12
frq(db2$p12_o2)
db2$p12_o2 <- sjlabelled::set_labels(db2$p12_o2, 
                                    labels = c("Ninguna importancia" = 1, 
                                               "Algo de importancia" = 2, 
                                               "Mediana importancia" = 3, 
                                               "Mucha importancia" = 4, 
                                               "No sabe" = 88, 
                                               "No responde" = 99))

get_label(db2$p12_o2)
db2$p12_o2 <- clean_labels(db2$p12_o2)

## p13_

db2 %>% 
  select(starts_with("p13_")) %>% 
  frq()

labels2 <- c("Sé como realizarlo" = 1, 
             "No sé como realizarlo, pero me siento capaz de aprenderlo" = 2, 
             "No me siento capaz de aprenderlo" = 3, 
             "No responde" = 99)

db2 <- db2 %>% 
  mutate(across(.cols = starts_with("p13_"),.fns = ~ sjlabelled::set_labels(., labels = labels2)))

db2 %>% 
  select(starts_with("p13_")) %>% 
  get_label()

db2$p13_1_o2 <- sjlabelled::set_label(db2$p13_1_o2, "Editar fotografías digitales u otras imágenes gráficas")
db2$p13_2_o2 <- clean_labels(db2$p13_2_o2)
db2$p13_3_o2 <- clean_labels(db2$p13_3_o2)
db2$p13_4_o2 <- clean_labels(db2$p13_4_o2)
db2$p13_5_o2 <- clean_labels(db2$p13_5_o2)
db2$p13_6_o2 <- clean_labels(db2$p13_6_o2)
db2$p13_7_o2 <- clean_labels(db2$p13_7_o2)
db2$p13_8_o2 <- clean_labels(db2$p13_8_o2)
db2$p13_9_o2 <- clean_labels(db2$p13_9_o2)
db2$p13_10_o2 <- clean_labels(db2$p13_10_o2)
db2$p13_11_o2 <- clean_labels(db2$p13_11_o2)
db2$p13_12_o2 <- clean_labels(db2$p13_12_o2)


## p14
frq(db2$p14_o2)
get_label(db2$p14_o2)
db2$p14_o2 <- sjlabelled::set_label(db2$p14_o2, label = "Género")

## p15
db2 %>% 
  select(starts_with("p15_")) %>% 
  frq()

db2 %>% 
  select(starts_with("p15_")) %>% 
  get_label()

db2$p15_mes_o2 <- clean_labels(db2$p15_mes_o2)
db2$p15_ano_o2 <- clean_labels(db2$p15_ano_o2)

## p16
frq(db2$p16_o2)
db2$p16_o2 <- sjlabelled::set_labels(db2$p16_o2, 
                                  labels = c("Educación Media" = 1, 
                                             "Instituto profesional o Centro de Formación Técnica" = 2, 
                                             "Una carrera en la Universidad" = 3, 
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p16_o2)
db2$p16_o2 <- clean_labels(db2$p16_o2)

## p17
frq(db2$p17_o2)
db2$p17_o2 <- sjlabelled::set_labels(db2$p17_o2, 
                                  labels = c("No terminó la educación básica" = 1, 
                                             "Completó la educación básica" = 2, 
                                             "Completó la enseñanza media" = 3, 
                                             "Completó estudios en un Instituto Profesional o Centro de Formación Técnica" = 4,
                                             "Completó una carrera en la Universidad" = 5,
                                             "Tiene estudios de posgrado" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p17_o2)
db2$p17_o2 <- clean_labels(db2$p17_o2)


## p18
frq(db2$p18_o2)
db2$p18_o2 <- sjlabelled::set_labels(db2$p18_o2, 
                                  labels = c("No terminó la educación básica" = 1, 
                                             "Completó la educación básica" = 2, 
                                             "Completó la enseñanza media" = 3, 
                                             "Completó estudios en un Instituto Profesional o Centro de Formación Técnica" = 4,
                                             "Completó una carrera en la Universidad" = 5,
                                             "Tiene estudios de posgrado" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p18_o2)
db2$p18_o2 <- clean_labels(db2$p18_o2)

## p19
frq(db2$p19_o2)
db2$p19_o2 <- sjlabelled::set_labels(db2$p19_o2, 
                                  labels = c("Entre 0 y 10 libros" = 1, 
                                             "Entre 11 y 25 libros" = 2, 
                                             "Entre 26 y 100 libros" = 3, 
                                             "Entre 100 y 200 libros" = 4,
                                             "Entre 201 y 500 libros" = 5,
                                             "Más de 500 libros" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db2$p19_o2)
db2$p19_o2 <- clean_labels(db2$p19_o2)

#4. Save and remove ----------------------------------------------------

db_students_w02 <- db2 %>% 
  select(-c(Codigo_aplicacion)) %>% 
  janitor::clean_names() %>% 
  as.data.frame()

base::save(db_students_w02, file = here("output/data/db_proc_students_w02.RData"))
haven::write_dta(db_students_w02, path = here("output/data/db_proc_students_w02.dta"))
haven::write_sav(db_students_w02, path = here("output/data/db_proc_students_w02.sav"))

rm(list = ls(pattern = "^labels[0-9]+$"))

