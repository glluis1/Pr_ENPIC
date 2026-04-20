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


# ---- Indicador 4a ----

# Media del aporte calórico a partir del 4º día
datos_calorias <- datos_uci48 %>%
  mutate(
    # 1. Cálculo de la media absoluta en kcal/día
    media = rowMeans(pick(num_range("CALORIA", 4:14)), na.rm = TRUE),
    
    # 2. Cálculo del peso ideal
    peso_ideal = 25 * (TALLA^2),
    
    # 3. Peso a utilizar según IMC
    peso_calculo = if_else(BMI < 30,
                           PESOACT,
                           (PESOACT - peso_ideal) * 0.33 + peso_ideal),
    
    # 4. Cálculo de la media en kcal/kg/día
    media_kg = media / peso_calculo
  )

# Cálculo del porcentaje
indic_4a <- datos_calorias %>%
  summarise(
    n = n(),
    x = sum(media_kg >= 25 & media_kg <= 30, na.rm = TRUE),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_4a <- prop.test(indic_4a$x, indic_4a$n, p = 0.8, alternative = "less")
round(IC_4a$conf.int * 100, 2)

# Valor p
IC_4a$p.value

# Peso ajustado = (actual - ideal) * 0.33 + ideal
# Peso ideal = 25 * talla^2

# Días dentro del rango objetivo
datos_calorias <- datos_calorias %>% 
  rowwise %>%
  mutate(
    # Días con datos (no NA)
    n_dias = sum(!is.na(c_across(CALORIA4:CALORIA14))),
    
    # Días dentro del rango
    n_dias_rango = sum(
      (c_across(CALORIA4:CALORIA14) / peso_calculo) >= 25 &
        (c_across(CALORIA4:CALORIA14) / peso_calculo) <= 30,
      na.rm = TRUE
    ),
    
    # Porcentaje de cumplimiento
    cumplimiento = if_else(n_dias > 0,
                           (n_dias_rango / n_dias) * 100,
                           NA_real_)
  ) %>%
  ungroup()


# ---- Indicador 4b ----

# Media del aporte proteico a partir del 4º día
datos_proteinas <- datos_uci48 %>%
  mutate(
    # 1. Cálculo de la media absoluta en g/día
    media = rowMeans(pick(num_range("PROT", 4:14)), na.rm = TRUE),
    
    # 2. Cálculo del peso ideal
    peso_ideal = 25 * (TALLA^2),
    
    # 3. Peso a utilizar según IMC
    peso_calculo = if_else(BMI < 30,
                           PESOACT,
                           (PESOACT - peso_ideal) * 0.33 + peso_ideal),
    
    # 4. Cálculo de la media en g/kg/día
    media_kg = media / peso_calculo
  )

# Cálculo del porcentaje
indic_4b <- datos_proteinas %>%
  summarise(
    n = n(),
    x = sum(media_kg >= 1.3, na.rm = TRUE),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_4b <- prop.test(indic_4b$x, indic_4b$n, p = 0.8, alternative = "less")
round(IC_4b$conf.int * 100, 2)

# Valor p
IC_4b$p.value

# Días dentro del rango objetivo
datos_proteinas <- datos_proteinas %>% 
  rowwise %>%
  mutate(
    # Días con datos (no NA)
    n_dias = sum(!is.na(c_across(PROT4:PROT14))),
    
    # Días dentro del rango
    n_dias_rango = sum(
      (c_across(PROT4:PROT14) / peso_calculo) >= 1.3,
      na.rm = TRUE
    ),
    
    # Porcentaje de cumplimiento
    cumplimiento = if_else(n_dias > 0,
                           (n_dias_rango / n_dias) * 100,
                           NA_real_)
  ) %>%
  ungroup()


# ---- Indicador 6 ----

# Filtrado de pacientes con NE o NE-NPT
datos_NE <- datos_uci48 %>% filter(TIPO_SN_Grupo %in% c(1, 3))

# Cálculo del porcentaje
indic_6 <- datos_NE %>%
  summarise(
    n = n(),
    x = sum(SN_Menos48h == 1),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_6 <- prop.test(indic_6$x, indic_6$n)
round(IC_6$conf.int * 100, 2)


# ---- Indicador 7 ----