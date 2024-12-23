#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data verification for EDUMER Panel Data
# Author: Andreas Laffert            
# Date: 10-12-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               glue,
               naniar,
               conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(file = here("output/data/edumer_students_wide.RData"))

glimpse(edumer_students_wide)

# 3. Processing  -------------------------------------------------------------

# 3.1 Variables for consistency check: sexo, nivel y edad ----

db <- edumer_students_wide %>%
  select(id_estudiante, starts_with("fecha"),
         starts_with(c("d2", "p20", "p21_ano")),
         d3_def_w01, d3_def_w01_V_w02, nivel_def_w01, 
         nivel_estudiante_w01, nivel_estudiante_w02)

# Edad

db$edad_w01 <- (2023 - db$p21_ano_w01) 

db$edad_w02 <- (2024 - db$p21_ano_w02)

# Sexo/genero

db <- db %>% 
  mutate(
    across(
      .cols = starts_with("p20_"),
      .fns = ~ case_when(. == 1 ~ "H",
                         . == 2 ~ "M",
                         . == 3 ~ "O",
                         TRUE ~ NA_character_) %>% 
        as.factor()
    )
  ) %>%
  rename_with(~ sub("p20_", "sexo_", .x), starts_with("p20_"))


# Nivel estudiante

# los que en la Ola 2 están en 7mo deben coincidir con los que estuvieron en 
# 6to en la Ola 1. Los que estan en 2do en la Ola 2, deben coindicir con los que
# estuvieron en 1ero en la Ola 1. estudiantes salen en 7mo y 2do ola 1.


frq(db$nivel_estudiante_w01) # nivel de estidiante en ola 1
frq(db$nivel_estudiante_w02) # nivel estudiante ola 2

db <- db %>%  
  mutate(
    nivel_estudiante_w01 = case_when(nivel_estudiante_w01 == 1 ~ "6to",
                                     nivel_estudiante_w01 == 2 ~ "7mo",
                                     nivel_estudiante_w01 == 3 ~ "1ro",
                                     nivel_estudiante_w01 == 4 ~ "2do",
                                     TRUE ~ NA_character_),
    nivel_estudiante_w01 = factor(nivel_estudiante_w01, 
                                  levels = c("6to",
                                             "7mo",
                                             "1ro",
                                             "2do")),
    nivel_estudiante_w02 = if_else(nivel_estudiante_w02 == 1, "7mo", "2do"),
    nivel_estudiante_w02 = factor(nivel_estudiante_w02, levels = c("7mo", "2do"))
  )

db %>% 
  group_by(nivel_estudiante_w01) %>% 
  count(nivel_estudiante_w02)

# Escuela

frq(db$d2_w01)
frq(db$d2_w02)

colegio_map_w02 <- c(
  "1" = "2",   # Nuestra Señora del Carmen de Maipú (w02) corresponde a 2 (Carmen) en w01
  "2" = "3",   # San Alberto Estación Central (w02) -> 3 (San Alberto) en w01
  "3" = "4",   # Santa Isabel de Hungría (w02) -> 4 (Santa Isabel de Hungría) w01
  "4" = "5",   # Reina de Dinamarca (w02) -> 5 (Reina de Dinamarca) w01
  "5" = "7",   # San Francisco de Sales (w02) -> 7 (San Francisco de Sales) w01
  "6" = "8",   # Miguel Rafael Prado (w02) -> 8 (Miguel Rafael Prado) w01
  "7" = "10",  # Instituto del Puerto (w02) -> 10 (Instituto del Puerto) w01
  "8" = "11",  # Santa Teresa de Los Andes (w02) -> 11 (Santa Teresa de Los Andes) w01
  "9" = "12"   # San Alberto Hurtado (w02) -> 12 (San Alberto Hurtado) w01
)

db <- db %>%
  mutate(
    # d2_w01 se mantiene igual,
    # pero d2_w02 se recodifica según el diccionario anterior, convirtiendo a numeric
    d2_w02_homologado = as.numeric(recode(as.character(d2_w02), !!!colegio_map_w02))
  )

frq(db$d2_w01)
frq(db$d2_w02_homologado)

db <- db %>% 
  select(id_estudiante, d2_w01, d2_w02_homologado, everything())

# 3.2 Consistencia ----

# sexo

c_sexo <- db %>% 
  group_by(sexo_w01) %>% 
  count(sexo_w02) %>% 
  mutate(consistencia_sexo = case_when(sexo_w01 == "H" & sexo_w02 == "H" ~ TRUE,
                                       sexo_w01 == "M" & sexo_w02 == "M" ~ TRUE,
                                       sexo_w01 == "O" & sexo_w02 == "O" ~ TRUE,
                                       sexo_w01 == "H" & sexo_w02 == "O" ~ TRUE,
                                       sexo_w01 == "M" & sexo_w02 == "O" ~ TRUE,
                                       sexo_w01 == "O" & sexo_w02 == "H" ~ TRUE,
                                       sexo_w01 == "O" & sexo_w02 == "M" ~ TRUE,
                                       is.na(sexo_w01) & !is.na(sexo_w02) ~ TRUE,
                                       !is.na(sexo_w01) & is.na(sexo_w02) ~ TRUE,
                                       TRUE ~ FALSE))


db <- db %>% 
  group_by(id_estudiante, d2_w01, d2_w02_homologado) %>% 
  mutate(consistencia_sexo = case_when(
    # Casos donde el sexo coincide entre ambas olas
    sexo_w01 == sexo_w02 ~ TRUE,
    # Permite inconsistencias suaves con 'O'
    sexo_w01 == "H" & sexo_w02 == "O" ~ TRUE,
    sexo_w01 == "M" & sexo_w02 == "O" ~ TRUE,
    sexo_w01 == "O" & sexo_w02 %in% c("H", "M") ~ TRUE,
    # Casos con NA en una ola, pero datos válidos en la otra
    is.na(sexo_w01) & !is.na(sexo_w02) ~ TRUE,
    !is.na(sexo_w01) & is.na(sexo_w02) ~ TRUE,
    # Otros casos: inconsistencia
    TRUE ~ FALSE
  )) %>% 
  ungroup()

db %>% 
  count(consistencia_sexo)# hay 6 casos inconsistentes

# edad

db <- db %>% 
  group_by(id_estudiante, d2_w01, d2_w02_homologado) %>% 
  mutate(consistencia_edad = case_when(
    # Casos donde edad coincide entre ambas olas
    p21_ano_w01 == p21_ano_w02 ~ TRUE,
    # Casos con NA en una ola, pero datos válidos en la otra
    is.na(p21_ano_w01) & !is.na(p21_ano_w02) ~ TRUE,
    !is.na(p21_ano_w01) & is.na(p21_ano_w02) ~ TRUE,
    # Otros casos: inconsistencia
    TRUE ~ FALSE
  )) %>% 
  ungroup() 

db %>% 
  count(consistencia_edad) # 10 casos inconsistentes

# nivel

db <- db %>% 
  group_by(id_estudiante, d2_w01, d2_w02_homologado) %>% 
  mutate(consistencia_nivel = case_when(
    nivel_estudiante_w02 == "7mo" & nivel_estudiante_w01 == "6to" ~ TRUE, # 6to -> 7mo
    is.na(nivel_estudiante_w02) & nivel_estudiante_w01 == "7mo" ~ TRUE,  # 7mo -> no participa
    nivel_estudiante_w02 == "2do" & nivel_estudiante_w01 == "1ro" ~ TRUE, # 1ro -> 2do
    is.na(nivel_estudiante_w02) & nivel_estudiante_w01 == "2do" ~ TRUE,  # 2do -> no participa
    is.na(nivel_estudiante_w02) & !is.na(nivel_estudiante_w01) & is.na(d2_w02_homologado) ~ TRUE, # si hay valor en ola 1 pero no en ola 2 es consistente
    nivel_estudiante_w02 == "7mo" & is.na(nivel_estudiante_w01) & !is.na(d2_w02_homologado) ~ TRUE,
    nivel_estudiante_w02 == "2do" & is.na(nivel_estudiante_w01) & !is.na(d2_w02_homologado) ~ TRUE,
    TRUE ~ FALSE # Todo lo demás es inconsistente
  )) %>% 
  ungroup() 

db %>% 
  count(consistencia_nivel) # 289 casos inconsistentes

db <- db %>%
  group_by(id_estudiante, d2_w01, d2_w02_homologado) %>% 
  mutate(
    # Recodificar consistencia_nivel para casos específicos
    consistencia_nivel = case_when(
      # Si el nivel es el mismo, ambas encuestas son del mismo año, y el mes de la ola 2 es mayor al de la ola 1
      nivel_estudiante_w01 == nivel_estudiante_w02 & 
        year(fecha_w01) == year(fecha_w02) & 
        month(fecha_w02) > month(fecha_w01) ~ TRUE,
      
      # Mantener los valores originales para el resto
      TRUE ~ consistencia_nivel
    )
  ) %>% 
  ungroup() 

db %>% 
  count(consistencia_nivel) # ahora solo 1 caso mal digitado

# 3.3 Recodificar -----

frq(db$sexo_w01)

db <- db %>%
  mutate(
    sexo_w01 = if_else(consistencia_sexo == FALSE & sexo_w01 != sexo_w02, sexo_w02, sexo_w01)
  )

frq(db$p21_ano_w01)

db <- db %>%
  mutate(
    p21_ano_w01 = if_else(consistencia_edad == FALSE & p21_ano_w01 != p21_ano_w02, p21_ano_w02, p21_ano_w01)
  )


db <- db %>%
  mutate(
    nivel_estudiante_w01 = if_else(
      consistencia_nivel == FALSE & as.character(nivel_estudiante_w01) != as.character(nivel_estudiante_w02),
      as.character(nivel_estudiante_w02),
      as.character(nivel_estudiante_w01)
    ),
    nivel_estudiante_w01 = factor(nivel_estudiante_w01) # Reconversión a factor si es necesario
  )

names(db)

db <- db %>% 
  select(everything(),
         d2_homologado_w02 = d2_w02_homologado,
         -c(d2_w02, consistencia_sexo, consistencia_edad, consistencia_nivel))

names(db)


df <- db %>%
  pivot_longer(
    cols = -id_estudiante,
    names_pattern = "(.*)(_w01|_w02)$",
    names_to = c(".value", "ola"),
    values_drop_na = T
  ) %>% 
  select(1:4) %>% 
  group_by(id_estudiante) %>% 
  mutate(d2_def = if_else(is.na(d2) & !is.na(d2_homologado), d2_homologado, d2)) %>% 
  ungroup() 


frq(df$d2_def)

frq(df$d2)
frq(df$d2_homologado)

frq(df$ola)
