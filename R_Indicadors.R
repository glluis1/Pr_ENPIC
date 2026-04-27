# ---- Instalación de paquetes ----
if (!require("haven")) {
  install.packages("haven")
}

library(haven)
library(dplyr)
library(lubridate)
library(knitr)
library(tidyr)
library(ggplot2)


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
#


# ---- I1. Identificación de enfermos en riesgo nutricional ----

resumen_i1 <- datos %>%
  # 1. Filtrado de pacientes con estancia > 48 h
  filter(DIASUCI >= 2) %>% # Todos
  
  # 2. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "1. Riesgo nutricional",
    Numerador = sum(!is.na(NUTRIC_Score)),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador)$conf.int[2] * 100),
    Estándar = "100%"
  )

# No tiene sentido el contraste de hipótesis porque al comparar 
# con el estándar (100%, objetivo ideal) siempre dará significación


# ---- I2. Valoración del estado nutricional ----

resumen_i2 <- datos %>%
  # 1. Filtrado de pacientes con riesgo nutricional (NUTRIC Score > 5)
  filter(NUTRIC_Score >= 5) %>%
  
  # 2. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "2. Estado nutricional",
    Numerador = sum(!is.na(VGSING2)),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador)$conf.int[2] * 100),
    Estándar = "100%"
  )

# !! Pacientes sin NUTRIC Score no entran en el cálculo


# ---- I4. Adecuación del aporte calórico y proteico ----

# Cálculos adicionales
datos <- datos %>%
  mutate(
    # 1. Cálculo del peso ideal y del peso ajustado
    Peso_ideal = 25 * (TALLA^2),
    Peso_ajustado = (PESOACT - Peso_ideal) * 0.33 + Peso_ideal,
    
    # 2. Peso a utilizar según IMC
    Peso_calculo = if_else(BMI < 30, PESOACT, Peso_ajustado),
    
    # 3. Media del aporte calórico los 3 primeros días
    Media_cal_aguda = 
      rowMeans(pick(num_range("CALORIA", 1:3)), na.rm = TRUE) / Peso_calculo,
    
    # 4. Media del aporte calórico a partir del 4º día
    Media_cal_estable = 
      rowMeans(pick(num_range("CALORIA", 4:14)), na.rm = TRUE) / Peso_calculo,
    
    # 3. Media del aporte proteico los 3 primeros días
    Media_prot_aguda = 
      rowMeans(pick(num_range("PROT", 1:3)), na.rm = TRUE) / Peso_calculo,
    
    # 4. Media del aporte proteico a partir del 4º día
    Media_prot_estable = 
      rowMeans(pick(num_range("PROT", 4:14)), na.rm = TRUE) / Peso_calculo
  )

# Peso ajustado = (actual - ideal) * 0.33 + ideal
# Peso ideal = 25 * talla^2
# Referència: Singer et al. (2019)

# Aporte calórico
resumen_i4_cal <- datos %>%
  # 1. Filtrado de pacientes con datos en fase estable
  filter(!is.na(Media_cal_estable)) %>%
  
  # 2. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "4a. Aporte calórico",
    Numerador = sum(Media_cal_estable >= 25 & Media_cal_estable <= 30, 
                    na.rm = TRUE),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador)$conf.int[2] * 100),
    Estándar = ">80%"
  )

# Aporte proteico
resumen_i4_prot <- datos %>%
  # 1. Filtrado de pacientes con datos en fase estable
  filter(!is.na(Media_prot_estable)) %>%
  
  # 2. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "4b. Aporte proteico",
    Numerador = sum(Media_prot_estable >= 1.3, na.rm = TRUE),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador, p = 0.8)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador, p = 0.8)$conf.int[2] * 100),
    Estándar = ">80%"
  )

resumen_i4 <- bind_rows(resumen_i4_cal, resumen_i4_prot)
rm(resumen_i4_cal, resumen_i4_prot)

# Análisis del error en el aporte calórico
error_cal <- datos %>%
  # 1. Filtrado de pacientes con datos en fase estable
  filter(!is.na(Media_cal_estable)) %>%
  
  # 2. Casos de error
  mutate(
    Categoria_cal = case_when(
      Media_cal_estable < 25 ~ "Hiponutrición",
      Media_cal_estable >= 25 & Media_cal_estable <= 30 ~ "Aporte adecuado",
      Media_cal_estable > 30 ~ "Hipernutrición"
    )
  ) %>%
  
  # 3. Resumen
  group_by(Categoria_cal) %>%
  summarise(
    Total_grupo = n(),
    Porcentaje = round(
      (Total_grupo / nrow(filter(datos, !is.na(Media_cal_estable)))) * 100, 2
    )
  )

# Días dentro del rango objetivo (calorías)
datos <- datos %>%
  rowwise %>%
  mutate(
    # Días con datos (no NA)
    n_dias_cal = sum(!is.na(c_across(CALORIA4:CALORIA14))),
    
    # Días dentro del rango
    n_dias_rango_cal = sum(
      (c_across(CALORIA4:CALORIA14) / Peso_calculo) >= 25 &
        (c_across(CALORIA4:CALORIA14) / Peso_calculo) <= 30,
      na.rm = TRUE
    ),
    
    # Porcentaje de cumplimiento
    cumplimiento_cal = if_else(n_dias_cal > 0,
                           (n_dias_rango_cal / n_dias_cal) * 100,
                           NA_real_)
  ) %>%
  ungroup()

# Días dentro del rango objetivo (proteínas)
datos <- datos %>% 
  rowwise %>%
  mutate(
    # Días con datos (no NA)
    n_dias_prot = sum(!is.na(c_across(PROT4:PROT14))),
    
    # Días dentro del rango
    n_dias_rango_prot = sum(
      (c_across(PROT4:PROT14) / Peso_calculo) >= 1.3,
      na.rm = TRUE
    ),
    
    # Porcentaje de cumplimiento
    cumplimiento_prot = if_else(n_dias_prot > 0,
                           (n_dias_rango_prot / n_dias_prot) * 100,
                           NA_real_)
  ) %>%
  ungroup()

# Boxplot
datos_boxplot <- datos %>%
  select(cumplimiento_cal, cumplimiento_prot) %>%
  pivot_longer(cols = everything(),
               names_to = "Indicador",
               values_to = "Porcentaje") %>%
  mutate(Indicador = recode(Indicador,
                            "cumplimiento_cal" = "Calorías",
                            "cumplimiento_prot" = "Proteínas")) %>%
  filter(!is.na(Porcentaje))

ggplot(datos_boxplot, aes(x = Indicador, y = Porcentaje, fill = Indicador)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "Distribución del aporte nutricional en fase estable",
       y = "% de días en rango por paciente",
       x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Para inferencia: tabla de contingencia fase aguda - fase estable


# ---- I6. Nutrición enteral precoz ----

resumen_i6 <- datos %>%
  # 1. Filtrado de pacientes con NE o NE-NPT e indicación de NE
  filter(TIPO_SN_Grupo %in% c(1, 3), !is.na(INDICA_NE)) %>%
  
  # 2. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "6. NE precoz",
    Numerador = sum(SN_Menos48h == 1),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador)$conf.int[2] * 100),
    Estándar = "100%"
  )


# ---- I7. Uso adecuado de NPT ----

resumen_i7 <- datos %>%
  # 1. Filtrado de pacientes que reciben NPT
  filter(!is.na(FINICNPT2)) %>%
  
  # 2. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "7. NPT adecuada",
    Numerador = sum(!is.na(INDICA_NTP) | !is.na(INDICA_NTP_PocoAporte)),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador, 0.9)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador, 0.9)$conf.int[2] * 100),
    Estándar = ">90%"
  )


# ---- I8. Adecuación temporal de la NPC ----

resumen_i8 <- datos %>%
  # 1. Pacientes con NE o NE-NPT y valores registradps del 4º día 
  filter(TIPO_SN_Grupo %in% c(1, 3), !is.na(CALORIA4), !is.na(PROT4)) %>%
  
  # 2. Pacientes con <60% del aporte nutricional
  filter(CALORIA4 < (15 * Peso_calculo) | PROT4 < (0.8 * Peso_calculo)) %>%
  
  # 3. Tiempo hasta inicio de NPC
  mutate(
    FINICNE2 = ymd_hms(FINICNE2),
    FINICNPT2 = ymd_hms(FINICNPT2),
    dias_npc = as.numeric(difftime(FINICNPT2, FINICNE2, units = "days"))
  ) %>%
  
  # 4. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "8. NPC adecuada",
    Numerador = sum(dias_npc <= 5, na.rm = TRUE),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador, 0.9)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador, 0.9)$conf.int[2] * 100),
    Estándar = ">90%"
  )

# Aporte nutricional 60%: 15 kcal/kg/día y 0.8 g/kg/día


# ---- Indicador 9 ----


# ---- I12. Disfunción hepática asociada a la NP ----

resumen_i12 <- datos %>%
  # 1. Filtrado de pacientes que reciben NPT durante al menos 7 días
  filter(TOTALNPT22 >= 7) %>%
  
  # 2. Pacientes con disfunción hepática según grupo
  mutate(
    Colestasis = case_when(
      TIPO_SN_Grupo %in% c(2, 4) ~ 
        (FALCALI7 > 280 | GAMGT7 > 50 | FILIRUB7 > 1.2 |
           FALCALIA > 280 | GAMGTA > 50 | FILIRUBA > 1.2),
      TRUE ~ (FALCALIA > 280 | GAMGTA > 50 | FILIRUBA > 1.2)
    ),
    
    Necrosis = case_when(
      TIPO_SN_Grupo %in% c(2, 4) ~
        ((GOT7 > 40 | GPT7 > 42) & (FILIRUB7 > 1.2 | INR7 > 1.4)) |
        ((GOTA > 40 | GPTA > 42) & (FILIRUBA > 1.2 | INRA > 1.4)),
      TRUE ~ ((GOTA > 40 | GPTA > 42) & (FILIRUBA > 1.2 | INRA > 1.4))
    ),
    
    Lesion_mixta = case_when(
      TIPO_SN_Grupo %in% c(2, 4) ~
        ((FALCALIA > 280 | GAMGTA > 50) & (GOTA > 40 | GPTA > 42)) |
        ((FALCALIA > 280 | GAMGTA > 50) & (GOTA > 40 | GPTA > 42)),
      TRUE ~ ((FALCALIA > 280 | GAMGTA > 50) & (GOTA > 40 | GPTA > 42))
    ),
    
    DHANP = if_else(Colestasis | Necrosis | Lesion_mixta, 1, 0)
  ) %>%
  
  # 3. Cálculo del porcentaje y los IC
  summarise(
    Indicador = "12. DHANP",
    Numerador = sum(DHANP == 1, na.rm = TRUE),
    Denominador = n(),
    `Resultado (%)` = round((Numerador / Denominador) * 100, 2),
    `IC 95% (inf – sup)` = 
      sprintf("(%.2f – %.2f)", 
              prop.test(Numerador, Denominador, 0.2)$conf.int[1] * 100, 
              prop.test(Numerador, Denominador, 0.2)$conf.int[2] * 100),
    Estándar = "<20%"
  )

# DHANP por diagnóstico
tipo_dhanp <- datos %>%
  # 1. Filtrado de pacientes que reciben NPT durante al menos 7 días
  filter(TOTALNPT22 >= 7) %>%
  
  # 2. Pacientes con disfunción hepática según grupo
  mutate(
    Colestasis = case_when(
      TIPO_SN_Grupo %in% c(2, 4) ~ 
        (FALCALI7 > 280 | GAMGT7 > 50 | FILIRUB7 > 1.2 |
           FALCALIA > 280 | GAMGTA > 50 | FILIRUBA > 1.2),
      TRUE ~ (FALCALIA > 280 | GAMGTA > 50 | FILIRUBA > 1.2)
    ),
    
    Necrosis = case_when(
      TIPO_SN_Grupo %in% c(2, 4) ~
        ((GOT7 > 40 | GPT7 > 42) & (FILIRUB7 > 1.2 | INR7 > 1.4)) |
        ((GOTA > 40 | GPTA > 42) & (FILIRUBA > 1.2 | INRA > 1.4)),
      TRUE ~ ((GOTA > 40 | GPTA > 42) & (FILIRUBA > 1.2 | INRA > 1.4))
    ),
    
    Lesion_mixta = case_when(
      TIPO_SN_Grupo %in% c(2, 4) ~
        ((FALCALIA > 280 | GAMGTA > 50) & (GOTA > 40 | GPTA > 42)) |
        ((FALCALIA > 280 | GAMGTA > 50) & (GOTA > 40 | GPTA > 42)),
      TRUE ~ ((FALCALIA > 280 | GAMGTA > 50) & (GOTA > 40 | GPTA > 42))
    ),
    
    DHANP = if_else(Colestasis | Necrosis | Lesion_mixta, 1, 0)
  ) %>%
  
  summarise(
   Colestasis = mean(Colestasis, na.rm = TRUE) * 100,
   Necrosis = mean(Necrosis, na.rm = TRUE) * 100,
   Mixta = mean(Lesion_mixta, na.rm = TRUE) * 100
  )


# ---- Tabla resumen ----

# Tabla de indicadores
tabla_indicadores <- bind_rows(resumen_i1, resumen_i2, resumen_i4, resumen_i6,
                               resumen_i7, resumen_i8, resumen_i12)