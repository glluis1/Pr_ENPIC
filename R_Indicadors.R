# ---- Instalación de paquetes ----

if (!require("haven")) {
  install.packages("haven")
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
if (!require("rstatix")) {
  install.packages("rstatix")
}
if (!require("gtsummary")) {
  install.packages("gtsummary")
}
if (!require("ggpubr")) {
  install.packages("ggpubr")
}
if (!require("ggvenn")) {
  install.packages("ggvenn")
}
if (!require("viridisLite")) {
  install.packages("viridisLite")
}

# Gestión de datos y archivos
library(haven)
library(tidyverse)

# Estadística y gráficos
library(rstatix)
library(gtsummary)
library(ggpubr)
library(ggvenn)
library(viridisLite)

# Presentación
library(knitr)


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