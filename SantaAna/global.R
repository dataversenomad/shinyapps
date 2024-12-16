
rm(list=ls())  


####### LIBRARIES

library(dplyr)
library(magrittr)
library(tidyverse)
library(moments)
library(skimr)
library(scales)        
library(ggthemes)
library(readxl)  
library(lubridate)
library(plotly) ; 
library(knitr)
library(repr)
library(GGally)
library(scales)
library(purrr)
library(gridExtra)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(RColorBrewer) 
library(shinyWidgets)
library(timetk)
library(data.table)
library(ggridges)

library(tidyverse)

library(DT)


a <- list(
  text = "Despacho de combustible",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.99,
  showarrow = FALSE
)

b <- list(
  text = "No. de viajes registrados",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.99,
  showarrow = FALSE
)

colfunc <- colorRampPalette(c("#eecac1", "#8b0000"))
pal_ <- colfunc(25)

##### 1. DATA 
data <- read_xlsx("/srv/shiny-server/SantaAna/data.xlsx",
                  sheet = "No_1")

data_df_tbl <- readRDS("/srv/shiny-server/SantaAna/eda_1.rds" ) %>%
  bind_cols(data %>% select(CATEGORIA, CORRELATIVO))

data_df_tbl <- data_df_tbl %>% 
  mutate(ACT_ID = paste0(CATEGORIA, CORRELATIVO))

activos_desc_1 <- read_xlsx("/srv/shiny-server/SantaAna/MAESTRO DE ACTIVOS.xlsx",
                            sheet = "MAESTRO DE ACTIVOS")

activos_desc_2 <- read_xlsx("/srv/shiny-server/SantaAna/MAESTRO DE ACTIVOS.xlsx",
                            sheet = "HORAS LABORADAS")

activos_desc_1 <- activos_desc_1 %>% mutate(ACT_ID = paste0(CODIGO_CATEGORIA, CORRELATIVO))


activos_desc_2 <- activos_desc_2 %>% mutate(ACT_ID = paste0(COD_CAT, CORRELATIVO))

kilometros <- read_xlsx("/srv/shiny-server/SantaAna/kilometros.xlsx",
                        sheet = "kms")

activos_report_tbl <- read_xlsx("/srv/shiny-server/SantaAna/Propuesta - Activos.xlsx",
                                sheet = "RESUMEN") %>%
  mutate(ACTIVO = as.character(ACTIVO))

activos_report_tbl_desc <- activos_desc_1 %>% select(ACT_ID, ACTIVIDAD_DESCRIP, DESCRIPCION, APLICACION_DESCRIP) %>% 
  group_by(ACT_ID, ACTIVIDAD_DESCRIP, DESCRIPCION, APLICACION_DESCRIP) %>% summarise(count = n()) %>%
  rename(ACTIVO = ACT_ID)

activos_report_tbl <- activos_report_tbl %>% left_join(activos_report_tbl_desc, 
                                                       by = c('ACTIVO')) 
activos_abc_xyz_tbl <- read_xlsx("/srv/shiny-server/SantaAna/Propuesta - Activos.xlsx",
                                 sheet = "ABC-XYZ") %>%
  mutate(ACTIVO = as.character(ACT_ID))

horas_trabajadas_week <- activos_desc_2 %>%
  group_by(ACT_ID) %>%
  summarise_by_time(
    .date_var = FECHA,
    .by       = "week", 
    HORAS_PROM  = mean(HORAS)
  ) %>% ungroup() %>% rename(FECHA_DESPACHO = FECHA)


# activos macro

activos_macro_c <- data_df_tbl %>% select(ACT_ID, FECHA_DESPACHO, CARGA = CANTIDAD) %>%
  group_by(ACT_ID) %>%
  arrange(-desc(FECHA_DESPACHO)) %>% ungroup() %>%
  group_by(ACT_ID, FECHA_DESPACHO) %>%
  summarise_by_time(
    .date_var = FECHA_DESPACHO,
    .by       = "week", 
    value     = mean(CARGA) 
  ) %>% ungroup() %>% rename(CARGA = value)

activos_macro_f <- data_df_tbl  %>% select(ACT_ID, FECHA_DESPACHO, CARGA = CANTIDAD) %>%
  mutate(FRECUENCIA = 1) %>%
  group_by(ACT_ID) %>%
  arrange(-desc(FECHA_DESPACHO)) %>% ungroup() %>%
  group_by(ACT_ID, FECHA_DESPACHO) %>%
  summarise_by_time(
    .date_var = FECHA_DESPACHO,
    .by       = "week", 
    value     = sum(FRECUENCIA) 
  ) %>% ungroup() %>% rename(FRECUENCIA = value)


activos_kms <- kilometros %>% select(ACT_ID, FECHA_DESPACHO, TOTAL_KMS) %>%
  group_by(ACT_ID) %>%
  arrange(-desc(FECHA_DESPACHO)) %>% ungroup() %>%
  group_by(ACT_ID, FECHA_DESPACHO) %>%
  summarise_by_time(
    .date_var = FECHA_DESPACHO,
    .by       = "week", 
    value     = mean(TOTAL_KMS) 
  ) %>% ungroup() %>% rename(KMS = value)


activos_op_tbl <- activos_macro_c %>% 
  left_join(activos_macro_f, by = c('ACT_ID', 'FECHA_DESPACHO')) %>%
  left_join(horas_trabajadas_week, by = c('ACT_ID', 'FECHA_DESPACHO')) %>% 
  left_join(activos_kms, by = c('ACT_ID', 'FECHA_DESPACHO')) %>% 
  mutate(
    cnt_c = case_when(
      is.na(CARGA) | is.null(CARGA) ~ 0,
      TRUE ~ 1
    )
  ) %>%
  mutate(
    cnt_h = case_when(
      is.na(HORAS_PROM) | is.null(HORAS_PROM) ~ 0,
      TRUE ~ 1
    )
  ) %>%
  mutate(
    cnt_k = case_when(
      is.na(KMS) | is.null(KMS) ~ 0,
      TRUE ~ 1
    )
  )

#View(activos_op_tbl)

#View(correlaciones %>% filter(ACTIVO == '294170'))


sy2 <- list(
  overlaying = "y",
  side = "right",
  title = "<b>Horas Trabajadas</b> por activo",
  tickfont = list(size = 11, color = "black"),
  titlefont = list(size = 13)
  )

sy1 <- list( title = "Horas promedio" , tickfont = list(size = 11, color = "black"),
            titlefont = list(size = 13)); dx <- list( title = "" , 
              tickfont = list(size = 11, color = "black"), titlefont = list(size = 13))


m <- list(
  
  l = 50,
  
  r = 50,
  
  b = 20,
  
  t = 20,
  
  pad = 4
  
)


"Aproximadamente el 50% de activos no tienen registradas las horas trabajadas"

correlaciones <- activos_op_tbl %>% filter(!is.na(HORAS_PROM)) %>% 
  select(ACT_ID, CARGA, HORAS_PROM, cnt_c, cnt_h) %>% group_by(ACT_ID) %>% summarize(
    cor = cor(CARGA, HORAS_PROM), cnt_c = sum(cnt_c), cnt_h = sum(cnt_h)
  ) %>% arrange(desc(cor)) %>% 
  mutate(correlacion = ifelse(is.na(cor), 0, cor)) %>% rename(ACTIVO = ACT_ID)

correlaciones_k <- activos_op_tbl %>% filter(!is.na(KMS)) %>% 
  select(ACT_ID, CARGA, KMS, cnt_c, cnt_h, cnt_k) %>% group_by(ACT_ID) %>% summarize(
    cor = cor(CARGA, KMS), cnt_c = sum(cnt_c), cnt_h = sum(cnt_h), cnt_k = sum(cnt_k)
  ) %>% arrange(desc(cor)) %>% 
  mutate(correlacion = ifelse(is.na(cor), 0, cor)) %>% rename(ACTIVO = ACT_ID, correlacion_k = correlacion)


correlaciones_count <- activos_op_tbl  %>% 
  select(ACT_ID, cnt_c, cnt_h) %>% group_by(ACT_ID) %>% summarize(
    cnt_c = sum(cnt_c), cnt_h = sum(cnt_h)
  ) %>% rename(ACTIVO = ACT_ID)


correlaciones_k_count <- activos_op_tbl  %>% 
  select(ACT_ID, cnt_c, cnt_k) %>% group_by(ACT_ID) %>% summarize(
    cnt_kc = sum(cnt_c), cnt_k = sum(cnt_k)
  ) %>% rename(ACTIVO = ACT_ID)


## Agregando correlaciones GAL/HRS

activos_report_tbl <- activos_report_tbl %>% 
                left_join(correlaciones %>% 
                select(ACTIVO, correlacion),
                by = c("ACTIVO")) %>% 
                left_join(correlaciones_count %>% select(ACTIVO,cnt_c, cnt_h), 
                by = c("ACTIVO")) 


## Agregando correlaciones GAL/KMS

activos_report_tbl <- activos_report_tbl %>% 
  left_join(correlaciones_k %>% 
              select(ACTIVO, correlacion_k),
            by = c("ACTIVO")) %>% 
  left_join(correlaciones_k_count %>% select(ACTIVO,cnt_kc, cnt_k), 
            by = c("ACTIVO")) 


# joining abc-xyz

activos_report_tbl <- activos_report_tbl %>% left_join(activos_abc_xyz_tbl %>% 
                                                         select(ACTIVO, `ABC-XYZ` ),
                                                       by = c("ACTIVO")
)


# MAPEO DE CATEGORIAS CORRELACION CARGA/HRS

activos_report_tbl <- activos_report_tbl %>%
    mutate(corr_check = ifelse(cnt_c >=5 & cnt_h > 5, 1, 0)) %>%

    mutate(CORRELACION_CATEGORIA = as_factor(case_when(
    
    correlacion >= 0.85 & corr_check == 1 ~ "> 0.85 (alta)",
    (correlacion >= 0.70 & correlacion < 0.85) & corr_check == 1 ~ "0.70 a 0.85 (media)",
    (correlacion >= 0.55 & correlacion < 0.70) & corr_check == 1 ~ "0.55 a 0.70 (baja)",
    (correlacion >= 0  & correlacion < 0.55) & corr_check == 1 ~ "0 a 0.55 (ninguna)",
    
    (correlacion <= -0.85) & corr_check == 1 ~ "> -0.85% (alta)",
    (correlacion <= -0.70 & correlacion > -0.85) & corr_check == 1 ~ "-0.70 a -0.85 (media)",
    (correlacion <= 0.55 & correlacion > -0.70) & corr_check == 1 ~ "-0.55 a -0.70 (baja)",
    (correlacion <= 0  & correlacion > -0.55) & corr_check == 1 ~ "0 a -0.55 (ninguna)",
    
    TRUE ~ "N/A"
    
  ))) 

activos_report_tbl$CORRELACION_CATEGORIA <- factor(activos_report_tbl$CORRELACION_CATEGORIA,
                                                   levels = c("> 0.85 (alta)",
                                                              "0.70 a 0.85 (media)",
                                                              "0.55 a 0.70 (baja)",
                                                              "0 a 0.55 (ninguna)",
                                                              "> -0.85 (alta)",
                                                              "-0.70 a -0.85 (media)",
                                                              "-0.55 a -0.70 (baja)",
                                                              "0 a -0.55 (ninguna)",
                                                              "N/A"))
# MAPEO DE CATEGORICAS UTILIZACION - CATEGORIA

activos_report_tbl$`UTILIZACION (CATEGORIA)` <- factor(activos_report_tbl$`UTILIZACION (CATEGORIA)`,
                                                       
                                                       levels = c("> 75%",
                                                                  "50% - 75%",
                                                                  "25% - 50%",
                                                                  "< 25%"))


# MAPEO DE CATEGORIAS CORRELACION CARGA/KMS

activos_report_tbl <- activos_report_tbl %>%
  mutate(corr_check = ifelse(cnt_kc >=5 & cnt_k > 5, 1, 0)) %>%
  
  mutate(CORRELACION_K_CATEGORIA = as_factor(case_when(
    
     correlacion_k >= 0.85 & corr_check == 1 ~ "> 0.85 (alta)",
    (correlacion_k >= 0.70 & correlacion_k < 0.85) & corr_check == 1 ~ "0.70 a 0.85 (media)",
    (correlacion_k >= 0.55 & correlacion_k < 0.70) & corr_check == 1 ~ "0.55 a 0.70 (baja)",
    (correlacion_k >= 0  & correlacion_k < 0.55) & corr_check == 1 ~ "0 a 0.55 (ninguna)",
    
    (correlacion_k <= -0.85) & corr_check == 1 ~ "> -0.85% (alta)",
    (correlacion_k <= -0.70 & correlacion_k > -0.85) & corr_check == 1 ~ "-0.70 a -0.85 (media)",
    (correlacion_k <= 0.55 & correlacion_k > -0.70) & corr_check == 1 ~ "-0.55 a -0.70 (baja)",
    (correlacion_k <= 0  & correlacion_k > -0.55) & corr_check == 1 ~ "0 a -0.55 (ninguna)",
    
    TRUE ~ "N/A"
    
  ))) 

activos_report_tbl$CORRELACION_K_CATEGORIA <- factor(activos_report_tbl$CORRELACION_K_CATEGORIA,
                                                   levels = c("> 0.85 (alta)",
                                                              "0.70 a 0.85 (media)",
                                                              "0.55 a 0.70 (baja)",
                                                              "0 a 0.55 (ninguna)",
                                                              "> -0.85 (alta)",
                                                              "-0.70 a -0.85 (media)",
                                                              "-0.55 a -0.70 (baja)",
                                                              "0 a -0.55 (ninguna)",
                                                              "N/A"))


# activos 2 sumarizado

horas_trabajadas <- activos_desc_2 %>% filter(ACT_ID %in% c('2104', '2105') ) %>%
  group_by(ACT_ID) %>%
  summarise_by_time(
    .date_var = FECHA,
    .by       = "month", 
    # Summarization
    HORAS_PROM  = mean(HORAS)
  )

dy <- list( title = "Horas promedio" , tickfont = list(size = 8, color = "black"),
            titlefont = list(size = 10)); dx <- list( title = "" , 
                                                      tickfont = list(size = 8, color = "black"), titlefont = list(size = 10))


labores <- activos_desc_2 %>% filter(ACT_ID %in% c('2104', '2105') ) %>%
  group_by(DESCRILABOR) %>% summarize(HORAS_PROM = mean(HORAS)) %>% ungroup()

cy <- list( title = "", tickfont = list(size = 8, color = "black"),
            titlefont = list(size = 10)); cx <- list( title = "Horas promedio", tickfont = list(size = 8, color = "black"),
                                                      titlefont = list(size = 10))

wx <- list( title = "", tickfont = list(size = 8, color = "black"),
            titlefont = list(size = 10)); wy <- list( title = "Galones promedio", tickfont = list(size = 8, color = "black"),
                                                      titlefont = list(size = 10))

zx <- list( title = "", tickfont = list(size = 8, color = "black"),
            titlefont = list(size = 10)); zy <- list( title = "Total despachos", tickfont = list(size = 8, color = "black"),
                                                      titlefont = list(size = 10))

ky <- list( title = "Numero de viajes" , tickfont = list(size = 11, color = "black"),
             titlefont = list(size = 13)); kx <- list( title = "Porcentaje de Utilizacion(%)" , 
                                                       tickfont = list(size = 11, color = "black"), titlefont = list(size = 13))


# 2. VARIABLES EXTRA ----------------------------------------------------------------------

activos_tbl <- data_df_tbl  %>%  select(FECHA_DESPACHO, YEAR, MONTH, DAY, 
                                        BOD_ID, BOD_NAME, 
                                        ACT_ID, CATEGORIA,
                                        CAT_ACT_1, CAT_ACT_2, CAT_ACT_3,  
                                        CANTIDAD) %>%
  #filter((YEAR == 2021 | YEAR == 2022 )) %>%
  group_by(FECHA_DESPACHO, YEAR, MONTH, DAY, 
           BOD_ID, BOD_NAME, 
           ACT_ID, CATEGORIA,
           CAT_ACT_1, CAT_ACT_2, CAT_ACT_3,  
           CANTIDAD) %>%
  summarise(times = n(), CANTIDAD = sum(CANTIDAD)) %>% arrange(desc(times)) %>% 
  ungroup() %>% mutate(CATEGORIA = factor(CATEGORIA)) %>%
  mutate(CAT_ACT_1 = factor(CAT_ACT_1)) %>%
  mutate(key = paste0(BOD_ID, BOD_NAME, CAT_ACT_1, ACT_ID))

activos_list <- activos_tbl %>% pull(ACT_ID) %>% unique()
activos_tbl <- activos_tbl %>% mutate(CATEGORIA = as.character(CATEGORIA))
activos_tbl <- activos_tbl %>% mutate(CAT_ACT_1 = as.character(CAT_ACT_1))
activos_tbl <- activos_tbl %>% mutate(ACT_ID = as.character(ACT_ID))

muestras_tbl <- activos_tbl %>% 
  select(BOD_ID, BOD_NAME, CAT_ACT_1, ACT_ID, CANTIDAD) %>% 
  group_by(BOD_ID, BOD_NAME, CAT_ACT_1, ACT_ID) %>% 
  summarise(st_dev = sd(CANTIDAD), n = n()) %>% 
  ungroup() %>% mutate(rowid = row_number()) %>% 
  mutate(sep = 1) %>% 
  mutate(label_g = paste0("n = ",n, ", stdv = ", round(st_dev,1) )) %>% 
  group_by("BOD_ID", "BOD_NAME", "CAT_ACT_1", "ACT_ID", "st_dev","n", "rowid", "sep", "label_g" ) %>% 
  mutate(id = row_number()) %>% ungroup() %>% filter(n >= 5)

keep_out <- muestras_tbl %>% filter(n >= 10) %>% select(ACT_ID) %>% pull(ACT_ID) 
muestras_tbl <- muestras_tbl  
muestras_tbl <- muestras_tbl %>% mutate(ACT_ID = as.character(ACT_ID))

ids_out <- muestras_tbl %>% select(BOD_ID, BOD_NAME, CAT_ACT_1,ACT_ID, st_dev) %>% 
  mutate(key = paste0(BOD_ID, BOD_NAME, CAT_ACT_1)) %>%
  select(key, ACT_ID, st_dev) %>% arrange(desc(st_dev))

ids_out_sorted <- Reduce(rbind,
                         by(ids_out,
                            ids_out["key"],
                            head,
                            n = 30))  

muestras_tbl_fin <- ids_out_sorted %>% group_by(key) %>% mutate(muestra = row_number())

### HORAS TRABAJADAS Y LABORES

horas_trabajadas <- activos_desc_2 %>%
  group_by(ACT_ID) %>%
  summarise_by_time(
    .date_var = FECHA,
    .by       = "month", 
    HORAS_PROM  = mean(HORAS)
  ) %>% ungroup()

activos_desc_2$ACT_ID <- as_factor(activos_desc_2$ACT_ID)
activos_desc_1$ACT_ID <- as_factor(activos_desc_1$ACT_ID)
horas_trabajadas$ACT_ID <- as_factor(horas_trabajadas$ACT_ID)
# Maestra nueva
horas_labores <- horas_trabajadas %>% left_join(activos_desc_1, by = "ACT_ID")

# LABORES

lab_1 <- activos_desc_1 %>% select(ACT_ID, APLICACION_DESCRIP) %>% group_by(ACT_ID, APLICACION_DESCRIP) %>%
  summarize(n = n()) %>% ungroup() %>% select(ACT_ID, APLICACION_DESCRIP)

lab_2 <- activos_desc_2 %>% select(ACT_ID, DESCRILABOR, HORAS) %>% group_by(ACT_ID, DESCRILABOR) %>%
  summarize(HORAS_PROM = mean(HORAS)) %>% ungroup()

labores_activos <- lab_2 %>% left_join(lab_1, by = "ACT_ID")

mega_tests <- activos_tbl %>% select(BOD_ID,BOD_NAME,CAT_ACT_1,ACT_ID, CANTIDAD) %>% 
  group_by(BOD_ID,BOD_NAME,CAT_ACT_1,ACT_ID) %>%
  summarise(st_dev = sd(CANTIDAD), n = n(), variance = var(CANTIDAD)) %>% ungroup() %>%
  mutate(rowid = row_number()) %>% filter(n >= 5 )


collect_1 <- function(data, tipo, nombre, cat_act_1, muestra , margin,
                      top_n = NA, muestras = NA, activo)
{
  

  tests <- data %>% filter(BOD_ID == tipo,
                           BOD_NAME == nombre,
                           CAT_ACT_1 == cat_act_1, ACT_ID %in% activo) %>% 
    select(ACT_ID, CANTIDAD) %>% mutate(ACT_ID = reorder(ACT_ID, CANTIDAD)) %>%
    group_by(ACT_ID) %>% 
    summarise(st_dev = sd(CANTIDAD), n = n(), variance = var(CANTIDAD) ) %>% ungroup() %>%
    mutate(rowid = row_number()) %>% mutate(sep = margin) %>% mutate(
      label_g = paste0("n = ",n, ", stdv = ", round(st_dev,1) )
    )
  
  
  keep <- tests %>% filter(n >= muestra) %>% select(ACT_ID) %>% pull(ACT_ID) 
  
  
  
  act_times_ordered <- data %>%
    filter(BOD_ID == tipo & BOD_NAME == nombre & ACT_ID %in% activo,
           CAT_ACT_1 == cat_act_1 )
  
  levels_act <- 
    if(is.na(top_n)) {
      
      levels(reorder(act_times_ordered$ACT_ID, 
                     act_times_ordered$CANTIDAD)) } else {
                       levels(reorder(act_times_ordered$ACT_ID, 
                                      act_times_ordered$CANTIDAD))[1:top_n]
                     }
  
  data <- data %>% filter(ACT_ID %in% activo)

  return(list(data, keep, tests, act_times_ordered, levels_act, muestras, keep))
  
}


activos_list <- activos_tbl %>% pull(ACT_ID) %>% unique()



