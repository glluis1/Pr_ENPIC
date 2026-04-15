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