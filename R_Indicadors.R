# ---- Instalación de paquetes ----
if (!require("haven")) {
  install.packages("haven")
}
if (!require("rstatix")) {
  install.packages("rstatix")
}
if (!require("ggVennDiagram")) {
  install.packages("ggVennDiagram")
}
if (!require("viridisLite")) {
  install.packages("viridisLite")
}

library(haven)
library(dplyr)
library(lubridate)
library(knitr)
library(tidyr)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggVennDiagram)
library(viridisLite)


# ---- Preparación de los datos ----

# Carga de datos
datos <- read_sav("ENPIC_BMI_525.sav")

# Acceso a variables
attr(datos$SUPRE_NPT, "label")
attr(datos$SUPRE_NPT, "labels")

# Valores faltantes
datos <- datos %>% filter(!is.na(FINICNE2) | !is.na(FINICNPT2))

# Comprobación de grupos
datos <- datos %>%
  mutate(
    # Conversión a formato de fecha
    FINICNE2 = parse_date_time(FINICNE2,
                                orders = c("ymd HMS", "ymd HM", "ymd"),
                                tz = "UTC"),
    FINICNPT2 = parse_date_time(FINICNPT2,
                                 orders = c("ymd HMS", "ymd HM", "ymd"),
                                 tz = "UTC"),
    
    # Grupo teórico
    check = case_when(
      !is.na(FINICNE2) & is.na(FINICNPT2) ~ 1,
      is.na(FINICNE2) & !is.na(FINICNPT2) ~ 2,
      !is.na(FINICNE2) & !is.na(FINICNPT2) & FINICNE2 < FINICNPT2 ~ 3,
      !is.na(FINICNE2) & !is.na(FINICNPT2) & FINICNE2 > FINICNPT2 ~ 4,
      TRUE ~ NA_real_
    )
  )

# Registros erróneos
datos_comprobacion_grupos <- datos %>%
  select(IDICOMEP, TIPO_SN_Grupo, check, FINICNE2, FINICNPT2) %>%
  filter(is.na(check) | TIPO_SN_Grupo != check)

datos_comprobacion_fechas <- datos %>%
  select(IDICOMEP, FECHAING, INGUCI, FINICNE2, FFINNE2, FINICNPT2, FFINNPT2) %>%
  filter(FINICNE2 < FECHAING | FINICNPT2 < FECHAING)

# Corrección de errores
datos$FINICNPT2[datos$IDICOMEP == 4007] <- 
  datos$FINICNPT2[datos$IDICOMEP == 4007] - lubridate::years(90)

datos$TIPO_SN_Grupo[datos$IDICOMEP == 34006] <- 3
datos$TIPO_SN_Grupo[datos$IDICOMEP == 34007] <- 3


# Comprobación NUTRIC Score
calculo_nutric <- function(edad, apache, sofa, comorb, t_uci) {
  pts_edad = case_when(
    edad < 50 ~ 0,
    edad >= 50 & edad < 75 ~ 1,
    edad >= 75 ~ 2,
    TRUE ~ 0
  )
  
  pts_apache = case_when(
    apache < 15 ~ 0,
    apache >= 15 & apache < 20 ~ 1,
    apache >= 20 & apache < 28 ~ 2,
    apache >= 28 ~ 3,
    TRUE ~ 0
  )
  
  pts_sofa = case_when(
    sofa < 6 ~ 0,
    sofa >= 6 & sofa < 10 ~ 1,
    sofa >= 10 ~ 2,
    TRUE ~ 0
  )
  
  pts_comorb = if_else(comorb >=2, 1, 0, missing = 0)
  
  pts_t_uci = if_else(t_uci >= 1, 1, 0, missing = 0)
  
  return(pts_edad + pts_apache + pts_sofa + pts_comorb + pts_t_uci)
}


# Cálculo NUTRIC Score
datos <- datos %>%
  mutate(
    comorb = rowSums(select(., ALCOHOL, AHTA, ADBTM, ACARDIOP, AEPOC, AIRC,
                            ACIRROSIS, AINMUNO, ANEOPLAS), na.rm = TRUE),
    
    t_uci = round(as.numeric(difftime(INGUCI, FECHAING, units = "days")), 2),
    
    Calculo_NUTRIC = calculo_nutric(EDAD, APACHEII, SOFA1, comorb, t_uci)
  )

# Registros erróneos
datos_comprobacion_nutric <- datos %>%
  select(IDICOMEP, EDAD, APACHEII, SOFA1, comorb, t_uci, 
         NUTRIC_Score, Calculo_NUTRIC) %>%
  filter(abs(NUTRIC_Score - Calculo_NUTRIC) > 1)