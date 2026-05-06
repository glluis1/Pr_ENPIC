# ---- Instalación de paquetes ----
if (!require("haven")) {
  install.packages("haven")
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
    Inicio_NE = parse_date_time(FINICNE2,
                                orders = c("ymd HMS", "ymd HM", "ymd"),
                                tz = "UTC"),
    Inicio_NPT = parse_date_time(FINICNPT2,
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


# ---- Características de la muestra ----

