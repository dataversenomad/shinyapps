
  
  # annotations
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
  
  
  ######## 1. DATA -----------------------------------------------------------------------------------------------
  
  data <- read_xlsx("/home/analytics/R/Projects/Python/Projects/ingenio/data.xlsx",
                    sheet = "No_1")
  
  data_df_tbl <- readRDS("/home/analytics/R/Projects/Python/Projects/ingenio/eda_1.rds" ) %>%
    bind_cols(data %>% select(CATEGORIA, CORRELATIVO))
  
  data_df_tbl <- data_df_tbl %>% 
    mutate(ACT_ID = paste0(CATEGORIA, CORRELATIVO))
  
  activos_desc_1 <- read_xlsx("/home/analytics/R/Projects/Python/Projects/ingenio/MAESTRO DE ACTIVOS.xlsx",
                              sheet = "MAESTRO DE ACTIVOS")
  
  activos_desc_2 <- read_xlsx("/home/analytics/R/Projects/Python/Projects/ingenio/MAESTRO DE ACTIVOS.xlsx",
                              sheet = "HORAS LABORADAS")
  
  activos_desc_1 <- activos_desc_1 %>% mutate(ACT_ID = paste0(CODIGO_CATEGORIA, CORRELATIVO))
  
  activos_desc_2 <- activos_desc_2 %>% mutate(ACT_ID = paste0(COD_CAT, CORRELATIVO))
  
  
  
  
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
    mutate(key = paste0(BOD_ID, BOD_NAME, CATEGORIA, ACT_ID))
  
  activos_list <- activos_tbl %>% pull(ACT_ID) %>% unique()
  
  activos_tbl <- activos_tbl %>% mutate(CATEGORIA = as.character(CATEGORIA))
  activos_tbl <- activos_tbl %>% mutate(ACT_ID = as.character(ACT_ID))
  
  # SECCION DE TABLA DE MUESTRAS PARA FILTRAR POR TOP STDV
  
  muestras_tbl <- activos_tbl %>% 
    select(BOD_ID, BOD_NAME, CATEGORIA, ACT_ID, CANTIDAD) %>%
    group_by(BOD_ID, BOD_NAME, CATEGORIA, ACT_ID) %>% 
    summarise(st_dev = sd(CANTIDAD), n = n()) %>% ungroup() %>%
    
    mutate(rowid = row_number()) %>% mutate(sep = 1) %>% mutate(
      label_g = paste0("n = ",n, ", stdv = ", round(st_dev,1) )
    ) %>% group_by("BOD_ID", "BOD_NAME", "CATEGORIA",
                   "ACT_ID", "st_dev","n",        
                   "rowid", "sep", "label_g" ) %>% mutate(id = row_number()) %>% ungroup() 
  
  keep_out <- muestras_tbl %>% filter(n >= 10) %>% select(ACT_ID) %>% pull(ACT_ID) 
  muestras_tbl <- muestras_tbl %>% filter(ACT_ID %in% keep_out) 
  muestras_tbl <- muestras_tbl %>% mutate(ACT_ID = as.character(ACT_ID))
  
  ids_out <- 
    
    muestras_tbl %>% select(BOD_ID, BOD_NAME, CATEGORIA,ACT_ID, st_dev) %>% 
    mutate(key = paste0(BOD_ID, BOD_NAME, CATEGORIA)) %>%
    select(key, ACT_ID, st_dev) %>% arrange(desc(st_dev))
  
  ids_out_sorted <- Reduce(rbind,
                           by(ids_out,
                              ids_out["key"],
                              head,
                              n = 30))  
  
  muestras_tbl_fin <- ids_out_sorted %>% group_by(key) %>% mutate(muestra = row_number())
  

