# Instalación de paquetes
if (!require("haven")) {
  install.packages("haven")
}

library(haven)
library(dplyr)


# Carga de datos
datos <- read_sav("ENPIC_BMI_525.sav")

# Acceso a variables
attr(datos$SUPRE_NPT, "label")
attr(datos$SUPRE_NPT, "labels")


# Características de la muestra


# ---- Indicador 1 ----

# Filtrado de pacientes con estancia > 48 h
datos_uci48 <- datos %>% filter(DIASUCI > 2) # Todos

# Cálculo del porcentaje
indic_1 <- datos_uci48 %>%
  summarise(
    n = n(),
    x = sum(!is.na(NUTRIC_Score)),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_1 <- prop.test(indic_1$x, indic_1$n)
round(IC_1$conf.int * 100, 2)

# No tiene sentido el contraste de hipótesis porque al comparar 
# con el estándar (100%, objetivo ideal) siempre dará significación


# ---- Indicador 2 ----

# Filtrado de pacientes con riesgo nutricional (NUTRIC Score > 5)
datos_riesgo_nutr <- datos_uci48 %>% filter(NUTRIC_Score > 5)

# Cálculo del porcentaje
indic_2 <- datos_riesgo_nutr %>%
  summarise(
    n = n(),
    x = sum(!is.na(VGSING2)),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_2 <- prop.test(indic_2$x, indic_2$n)
round(IC_2$conf.int * 100, 2)

# !! Pacientes sin NUTRIC Score no entran en el cálculo