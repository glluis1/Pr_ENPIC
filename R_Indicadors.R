# Instalación de paquetes
if (!require("haven")) {
  install.packages("haven")
}

library(haven)
library(dplyr)
library(lubridate)


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
datos_aporte <- datos_uci48 %>%
  mutate(
    # 1. Cálculo de la media absoluta en kcal/día
    media_calorias = rowMeans(pick(num_range("CALORIA", 4:14)), na.rm = TRUE),
    
    # 2. Cálculo del peso ideal
    peso_ideal = 25 * (TALLA^2),
    
    # 3. Peso a utilizar según IMC
    peso_calculo = if_else(BMI < 30,
                           PESOACT,
                           (PESOACT - peso_ideal) * 0.33 + peso_ideal),
    
    # 4. Cálculo de la media en kcal/kg/día
    media_kg_calorias = media_calorias / peso_calculo
  )

# Cálculo del porcentaje
indic_4a <- datos_aporte %>%
  summarise(
    n = n(),
    x = sum(media_kg_calorias >= 25 & media_kg_calorias <= 30, na.rm = TRUE),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_4a <- prop.test(indic_4a$x, indic_4a$n, p = 0.8, alternative = "less")
round(IC_4a$conf.int * 100, 2)

# Valor p
IC_4a$p.value

# Peso ajustado = (actual - ideal) * 0.33 + ideal
# Peso ideal = 25 * talla^2
# Referència: Singer et al. (2019)

# Días dentro del rango objetivo
datos_aporte <- datos_aporte %>% 
  rowwise %>%
  mutate(
    # Días con datos (no NA)
    n_dias_cal = sum(!is.na(c_across(CALORIA4:CALORIA14))),
    
    # Días dentro del rango
    n_dias_rango_cal = sum(
      (c_across(CALORIA4:CALORIA14) / peso_calculo) >= 25 &
        (c_across(CALORIA4:CALORIA14) / peso_calculo) <= 30,
      na.rm = TRUE
    ),
    
    # Porcentaje de cumplimiento
    cumplimiento_cal = if_else(n_dias_cal > 0,
                           (n_dias_rango_cal / n_dias_cal) * 100,
                           NA_real_)
  ) %>%
  ungroup()


# ---- Indicador 4b ----

# Media del aporte proteico a partir del 4º día
datos_aporte <- datos_aporte %>%
  mutate(
    # 1. Cálculo de la media absoluta en g/día
    media_proteinas = rowMeans(pick(num_range("PROT", 4:14)), na.rm = TRUE),
    
    # 2. Cálculo de la media en g/kg/día
    media_kg_proteinas = media_proteinas / peso_calculo
  )

# Cálculo del porcentaje
indic_4b <- datos_aporte %>%
  summarise(
    n = n(),
    x = sum(media_kg_proteinas >= 1.3, na.rm = TRUE),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_4b <- prop.test(indic_4b$x, indic_4b$n, p = 0.8, alternative = "less")
round(IC_4b$conf.int * 100, 2)

# Valor p
IC_4b$p.value

# Días dentro del rango objetivo
datos_aporte <- datos_aporte %>% 
  rowwise %>%
  mutate(
    # Días con datos (no NA)
    n_dias_prot = sum(!is.na(c_across(PROT4:PROT14))),
    
    # Días dentro del rango
    n_dias_rango_prot = sum(
      (c_across(PROT4:PROT14) / peso_calculo) >= 1.3,
      na.rm = TRUE
    ),
    
    # Porcentaje de cumplimiento
    cumplimiento_prot = if_else(n_dias_prot > 0,
                           (n_dias_rango_prot / n_dias_prot) * 100,
                           NA_real_)
  ) %>%
  ungroup()

# ---- Indicador 4 ----

# Cálculo del porcentaje conjunto
indic_4 <- datos_aporte %>%
  summarise(
    n = n(),
    x = sum(media_kg_calorias >= 25 & media_kg_proteinas >= 1.3, na.rm = TRUE),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_4 <- prop.test(indic_4$x, indic_4$n, p = 0.8, alternative = "less")
round(IC_4$conf.int * 100, 2)

# Valor p
IC_4$p.value


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

# Filtrado de pacientes con NPT o NPT-NE
datos_NPT <- datos_uci48 %>% filter(TIPO_SN_Grupo %in% c(2, 4))

# Cálculo del porcentaje
indic_7 <- datos_NPT %>%
  summarise(
    n = n(),
    x = sum(!is.na(INDICA_NTP)),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_7 <- prop.test(indic_7$x, indic_7$n, p = 0.9, alternative = "less")
round(IC_7$conf.int * 100, 2)

# Valor p
IC_7$p.value


# ---- Indicador 8 ----

# Filtrado
datos_NPC <- datos_aporte %>%
  # 1. Pacientes con NE o NE-NPT y valores registradps del 4º día 
  filter(TIPO_SN_Grupo %in% c(1, 3), !is.na(CALORIA4), !is.na(PROT4)) %>%
  
  # 2. Pacientes con <60% del aporte nutricional
  filter(CALORIA4 < (15 * peso_calculo) | PROT4 < (0.8 * peso_calculo)) %>%
  
  # 3. Tiempo hasta inicio de NPC
  mutate(
    Inicio_NE = ymd_hms(Inicio_NE),
    Inicio_NPT = ymd_hms(Inicio_NPT),
    dias_npc = as.numeric(difftime(Inicio_NPT, Inicio_NE, units = "days"))
  )
  
# Cálculo del porcentaje
indic_8 <- datos_NPC %>%
  summarise(
    n = n(),
    x = sum(dias_npc <= 5, na.rm = TRUE),
    porcentaje = (x / n) * 100
  )

# Intervalo de confianza
IC_8 <- prop.test(indic_8$x, indic_8$n, p = 0.9, alternative = "less")
round(IC_8$conf.int * 100, 2)

# Valor p
IC_8$p.value

# Aporte nutricional 60%: 15 kcal/kg/día y 0.8 g/kg/día


# ---- Indicador 9 ----