
server <- function(input, output, session) { 
  
  
  output$tipos <- renderUI({
    
    selectInput(inputId = "tipo_", "Tipo",choices = var_distribuidor(), selected = c("finca"), multiple = F)
  })
  
  output$nombres <- renderUI({
    
    selectInput(inputId = "nombre_", "Nombre",choices = var_nombres(), selected = ("bolivia"), multiple = F)
  })
  
  output$categorias <- renderUI({
    selectInput(inputId = "categoria_", "Categoria",choices = var_categorias(), selected = c("tractores"), multiple = F)
  })
  
  output$muestras <- renderUI({
    
    sliderInput( inputId = "muestra_", label = "Muestra >", min = 0, max = 30, value = 10, step = 5)
    #selectInput(inputId = "muestra_", "Muestra", choices = var_muestras(), multiple = F, selected = 10)
  })
  
  output$activos <- renderUI({
    
    pickerInput("activos_", "Activos",   
                choices = var_activos(), 
                options = list(`actions-box` = TRUE, `none-selected-text` = "Favor seleccionar activo"),
                selected = var_activos(),
                #selected = c("24535", "235124", "24542", "24534", "235112", "24539", "23599", "24529", "23590", "23598"),
                multiple = TRUE)
    
    
  })
  

  output$utilizacion <- renderUI({
    
    pickerInput(inputId = "utilizacion_", "Utilizacion(%)",choices = c("> 75%",
                                                                       "50% - 75%",
                                                                       "25% - 50%",
                                                                       "< 25%")  , 
                selected = c("25% - 50%") , multiple = FALSE, label = "Utilizacion(%)" )
  })
  

  
  output$correlacion <- renderUI({
    
    pickerInput(inputId = "correlacion_", "Correlacion(Carga/Hrs)",choices = c("> 0.85 (alta)",
                                                                               "0.70 a 0.85 (media)",
                                                                               "0.55 a 0.70 (baja)",
                                                                               "0 a 0.55 (ninguna)",
                                                                               "> -0.85 (alta)",
                                                                               "-0.70 a -0.85 (media)",
                                                                               "-0.55 a -0.70 (baja)",
                                                                               "0 a -0.55 (ninguna)",
                                                                               "N/A")  , 
                selected = c("> 0.85 (alta)", "> -0.85 (alta)",
                             "0.70 a 0.85 (media)", "-0.70 a -0.85 (media)" ) , multiple = TRUE,
                label = "Correlacion Carga/HRS")
  }) 
  
  output$abc <- renderUI({
    
    pickerInput(inputId = "abc_", "ABC/XYZ", choices = var_abc()  , 
                label = "ABC-XYZ",
                selected = var_abc() , multiple = TRUE)
  })
  
  output$abc <- renderUI({
    
    pickerInput(inputId = "abc_", "ABC/XYZ", choices = var_abc()  , 
                label = "ABC-XYZ",
                selected = var_abc() , multiple = TRUE)
  })
  
  
  output$actividad <- renderUI({
    
    pickerInput(inputId = "actividad_", "ACTIVIDAD", choices = var_actividad()  , 
                label = "ACTIVIDAD",
                selected = var_actividad() , multiple = TRUE)
  })

  
  output$correlacion_k <- renderUI({
    
    pickerInput(inputId = "correlacion_k", "Correlacion(Carga/KMS)",choices = c("> 0.85 (alta)",
                                                                               "0.70 a 0.85 (media)",
                                                                               "0.55 a 0.70 (baja)",
                                                                               "0 a 0.55 (ninguna)",
                                                                               "> -0.85 (alta)",
                                                                               "-0.70 a -0.85 (media)",
                                                                               "-0.55 a -0.70 (baja)",
                                                                               "0 a -0.55 (ninguna)",
                                                                               "N/A")  , 
                selected = c("> 0.85 (alta)", "0.70 a 0.85 (media)", "0.55 a 0.70 (baja)", "0 a 0.55 (ninguna)", "> -0.85% (alta)", "-0.70 a -0.85 (media)", "-0.55 a -0.70 (baja)", "0 a -0.55 (ninguna)", "N/A" ) , multiple = TRUE,
                label = "Correlacion Carga/KMS")
  }) 
  
  
  
    output$activos_detail <- renderUI({
  
      pickerInput(inputId = "activos_detail_", "ACTIVO", choices = var_activos_detail()  , 
                  label = "ACTIVO",
                  selected = var_activos_detail() , multiple = TRUE)
    })

  

    
  output$box1 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box1", width = NULL, height = 450,    #CAMBIO
              
              
              tabPanel( title = "Distribucion de despachos", value = "T3",
                        div( style = "position: relative; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: relative; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;", 
                             
                        ),
                        
                        
                        withSpinner(plotOutput("distribucion_plot", height = 350)))
              
      )
    )
  }
  )    
  
 
  output$box15 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box15", width = NULL, height = 450,
              tabPanel( title = "Total de despachos realizados", value = "T15",
                        div( style = "position:absolute;left:0.5em;bottom: 0.5em;", ),
                        withSpinner(plotOutput("despachos", height = 350)) )
      )
    )
  }
  )   
  
  
  output$box11 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box11", width = NULL, height = 350,
              tabPanel( title = "Horas de trabajo realizado", value = "T151",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100;",
                             dropdown(
                               pickerInput(inputId = 'activos_2',
                                           label = 'Activos',
                                           width = "100px",
                                           choices = var_activos(),
                                           selected = var_activos(),
                                           multiple = TRUE
                                           
                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ),     
                             
                        ),
                        withSpinner(plotlyOutput("horas_trabajadas", height = 300),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7
                                    
                        ) 
              )  
      )
    )
  }
  )
  
  output$box20 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box20", width = NULL, height = 350,
              tabPanel( title = "Detalle de actividades (por activo)", value = "T15",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100; ", 
                             
                             dropdown(
                               pickerInput(inputId = 'activos_3',
                                           label = 'Activos',
                                           width = "100px",
                                           choices = var_activos(),
                                           selected = var_activos(),
                                           multiple = FALSE
                                           
                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ), 
                             
                        ),
                        
                        withSpinner(plotlyOutput("labores_activo", height = 300),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7) 
              )  
      )
    )
  } )
  

  output$box22 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box22", width = NULL, height = 350,
              tabPanel( title = "Tendencia - galones despachados", value = "T152",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100;",
                             dropdown(
                               pickerInput(inputId = 'activos_4',
                                           label = 'Activos',
                                           width = "100px",
                                           choices = var_activos(),
                                           selected = var_activos(),
                                           multiple = TRUE

                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ),     
                             
                        ),
                        withSpinner(plotlyOutput("tendencia_despachos", height = 300),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7
                                    
                        ) 
              )  
      )
    )
  }
  )
  
  output$box23 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box23", width = NULL, height = 350,
              tabPanel( title = "Tendencia -  numero de despachos", value = "T153",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100;",
                             dropdown(
                               pickerInput(inputId = 'activos_5',
                                           label = 'Activos',
                                           width = "100px",
                                           choices = var_activos(),
                                           selected = var_activos(),
                                           multiple = TRUE
                                           
                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ),     
                             
                        ),
                        withSpinner(plotlyOutput("tendencia_despachos_num", height = 300),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7
                                    
                        ) 
              )  
      )
    )
  }
  )
  
  
  output$box25 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box25", width = NULL, height = 350,
              tabPanel( title = "Resumen: Estadisticas por Activo", value = "T156",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100;",
                        ),
                        withSpinner(DT::dataTableOutput("table_filtered", height = 300),
                                    
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7
                                    
                        ) 
              )  
      )
    )
  }
  )
  

  output$box26 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box26", width = NULL, height = 500,
              tabPanel( title = "Relacion: Carga vs Horas Trabajadas (SEMANAL)", value = "T25",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100; ", 
                             
                             dropdown(
                               pickerInput(inputId = 'activos_6',
                                           label = 'Activos',
                                           width = "500px",
                                           choices = var_activos_detail(),
                                           selected = var_activos_detail(),
                                           multiple = FALSE
                                           
                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ), 
                             
                        ),
                        
                        withSpinner(plotlyOutput("corrplot", height = 300, width = 1000),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7) 
              )  
      )
    )
  } )
  
  
  output$box27 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box27", width = NULL, height = 500,
              tabPanel( title = "Propuesta: Optimizacion de carga (% gal)", value = "T26",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100; ", 
                             
                             dropdown(
                               pickerInput(inputId = 'activos_7',
                                           label = 'Activos',
                                           width = "500px",
                                           choices = var_activos_detail(),
                                           selected = var_activos_detail(),
                                           multiple = FALSE
                                           
                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ), 
                             
                        ),
                        
                        withSpinner(plotlyOutput("propuesta", height = 500, width = 1000),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7) 
              )  
      )
    )
  } )
  
  
  output$box28 <- renderUI({
    div(
      style = "position: relative; ",
      tabBox( id = "box28", width = NULL, height = 500,
              tabPanel( title = "Relacion: Carga vs Kilometros recorridos (SEMANAL)", value = "T28",
                        div( style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100; ", 
                             
                             dropdown(
                               pickerInput(inputId = 'activos_6',
                                           label = 'Activos',
                                           width = "500px",
                                           choices = var_activos_detail(),
                                           selected = var_activos_detail(),
                                           multiple = FALSE
                                           
                               ),
                               
                               
                               size = "xs",animate = TRUE,
                               label = "Activos",
                               icon = icon("gear", class = "opt"),  
                               up = TRUE
                             ), 
                             
                        ),
                        
                        withSpinner(plotlyOutput("corrplot_k", height = 300, width = 1000),
                                    type = 4,
                                    color = "#d33724", 
                                    size = 0.7) 
              )  
      )
    )
  } )
  
  
  # FILTROS
  
  tipos_dist <- reactive({
    if (is.null(input$tipo_)) unique(activos_tbl$BOD_ID) else input$tipo_
  })
  
  nombres_dist <- reactive({
    input$nombre_
  })
  
  categorias_dist <- reactive({
    input$categoria_
  })
  
  muestras_dist <- reactive({
    input$muestra_
  })
  
  activos_dist <- reactive({
    input$activos_
  })
  
  utilizacion_dist <- reactive({
    input$utilizacion_
  })
  
  correlacion_dist <- reactive({
    input$correlacion_
  })
  
  actividad_dist <- reactive({
    input$actividad_
  })
  
  correlacion_k_dist <- reactive({
    input$correlacion_k
  })
  
  abc_dist <- reactive({
    input$abc_
  })
  
  

  
  var_distribuidor <- reactive({
    #c1 <- activos_tbl
    #if(is.null(activos_tbl)){return()}
    as.list(c("finca", "movil", 'gasolinera', "tanque (frente cosecha)"))
  })
  
  var_nombres <- reactive({
    filter(activos_tbl, BOD_ID %in% tipos_dist()) %>% 
      pull(BOD_NAME) %>% 
      unique()
  })
  
  var_categorias <- reactive({
    filter(activos_tbl, BOD_ID %in% tipos_dist() , BOD_NAME %in% nombres_dist()    ) %>% 
      pull(CAT_ACT_1) %>% 
      unique()
  })
  
  var_muestras <- reactive({
    
    muestras_tbl_fin %>% filter(key %like% tipos_dist() & key %like% nombres_dist() &
                                  key %like% categorias_dist()) %>% pull(muestra) %>%
      unique()
    
  
  })
  
  var_activos <- reactive({
    
    activos <- muestras_tbl_fin %>% filter(key %like% tipos_dist() & key %like% nombres_dist() &
                                             key %like% categorias_dist() & 
                                             muestra <= muestras_dist()) %>% pull(ACT_ID) %>% unique()
    #    message("ESTOS SON:              ")
    #    message(activos)
    
    act_inc <- mega_tests %>% pull(ACT_ID) %>% unique()
    
    out <- filter(activos_tbl, BOD_ID %in% tipos_dist() , BOD_NAME %in% nombres_dist(),
                  CAT_ACT_1 %in% categorias_dist(), ACT_ID %in% activos) %>% select(ACT_ID, CANTIDAD) %>%
      group_by(ACT_ID) %>% summarise(st_dev = sd(CANTIDAD), n = n()) %>% ungroup() %>%
      arrange(desc(st_dev)) %>%
      pull(ACT_ID) %>% 
      unique()
    
    out <- out[out %in% act_inc]
    out
    # unique(distribucion()[[4]])
    
  })
  
  
  var_activos_2 <- reactive({
    
    activos <- muestras_tbl_fin %>% filter(key %like% tipos_dist() & key %like% nombres_dist() &
                                             key %like% categorias_dist() & 
                                             muestra <= muestras_dist()) %>% pull(ACT_ID) %>% unique()
    
    out <- filter(activos_tbl, BOD_ID %in% tipos_dist() , BOD_NAME %in% nombres_dist(),
                  CAT_ACT_1 %in% categorias_dist(), ACT_ID %in% activos) %>% select(ACT_ID, CANTIDAD) %>%
      group_by(ACT_ID) %>% summarise(st_dev = sd(CANTIDAD), n = n()) %>% ungroup() %>%
      arrange(desc(st_dev)) %>%
      pull(ACT_ID) %>% 
      unique()
    

    activos_ht <- horas_labores %>% pull(ACT_ID) %>% unique()
    activos_ht <- activos_ht[activos_ht %in% out]

    activos_ht

    
  })
  
  var_utilizacion <- reactive({
    
    activos_report_tbl %>% pull(`UTILIZACION (CATEGORIA)`) %>% unique()
  })
  
  var_correlacion <- reactive({
      filter(activos_report_tbl, `UTILIZACION (CATEGORIA)`  %in% utilizacion_dist()) %>% 
      pull(CORRELACION_CATEGORIA) %>% 
      unique()
  })
  
  var_abc <- reactive({
    filter(activos_report_tbl, `UTILIZACION (CATEGORIA)`  %in% utilizacion_dist() &
             CORRELACION_CATEGORIA %in% correlacion_dist()) %>% 
      pull(`ABC-XYZ`) %>% 
      unique()
  })
  

  var_actividad <- reactive({
    filter(activos_report_tbl, `UTILIZACION (CATEGORIA)`%in% utilizacion_dist() &
             CORRELACION_CATEGORIA %in% correlacion_dist() &
             `ABC-XYZ` %in% abc_dist() ) %>% 
      pull(DESCRIPCION) %>% 
      unique()
  })

    
  var_correlacion_k <- reactive({
    filter(activos_report_tbl, `UTILIZACION (CATEGORIA)`%in% utilizacion_dist() &
             CORRELACION_CATEGORIA %in% correlacion_dist() &
             `ABC-XYZ` %in% abc_dist() & DESCRIPCION %in% actividad_dist()   ) %>% 
      pull(CORRELACION_K_CATEGORIA) %>% 
      unique()
  })
  

  var_activos_detail <- reactive({
    filter(activos_report_tbl, `UTILIZACION (CATEGORIA)`  %in% utilizacion_dist() &
             CORRELACION_CATEGORIA %in% correlacion_dist() &
             `ABC-XYZ` %in% abc_dist() & DESCRIPCION %in% actividad_dist() &
              CORRELACION_K_CATEGORIA %in% correlacion_k_dist() ) %>% 
      pull(ACTIVO) %>% 
      unique()
  })
  
  

  ##############################
  

  distribucion <- reactive ({
    
    #    input$go
    
    # if(input$go == TRUE) {
    #    activos_s <- isolate(input$activos)
    #  } else {
    #    activos_s <- input$activos
    #  }
    
    #message("*******ENTERING DISTRIBUCION REACTIVE ********************* ")
    distribucion_df <- collect_1(data = activos_tbl, tipo = input$tipo_, nombre = input$nombre_, 
                                 cat_act_1 = input$categoria_, muestra = 10, margin = 1,
                                 top_n = NA, muestras = input$muestra_, activo = input$activos_)
    
    data <- distribucion_df[[1]]
    tests <-  distribucion_df[[3]]
    keep <-  distribucion_df[[7]]
    
    #tests_t <- tests
    
    #message("VERIFICAAAAAAAAAAAAAR ACTIVOS")
    #message(data %>% pull(ACT_ID) %>% unique())
    
    # nuevo: sin keep
    tests <- tests %>% mutate(rowid = row_number())
    
    #    tests <- tests %>% filter(ACT_ID %in% keep) %>% mutate(rowid = row_number())
    
    tests <- tests %>% arrange(desc(st_dev)) 
    tests <- tests %>% slice(1: distribucion_df[[6]])
    tests <- tests %>% mutate(ACT_ID = as.character(ACT_ID))
    keep_muestra <- tests %>% pull(ACT_ID) %>% unique()
    
    data <- data %>% filter(ACT_ID %in% unique(tests %>% pull(ACT_ID)) )
    
    
    ids <- tests %>% mutate(sample = row_number()) %>% select(ACT_ID, sample)
    
    data <- data %>% merge(ids, by = ("ACT_ID"), all =TRUE)
    
    data <- data %>% filter(sample <= distribucion_df[[6]] )
    samples <- unique(data$sample)
    
    
    mean_gal <- data %>%
      filter(BOD_ID == input$tipo_ & BOD_NAME == input$nombre_,
             CAT_ACT_1 == input$categoria_ & ACT_ID %in% keep_muestra) %>% pull(CANTIDAD) %>% mean()
    
    
    data <- data %>% mutate(ACT_ID = as.character(ACT_ID))
    data <- data %>% mutate(BOD_NAME = as_factor(BOD_NAME))
    
    activos_out <- data %>% pull(ACT_ID) %>% unique()
    #levels_act <- distribucion_df[[5]]
    
    
    
    return(list(data, mean_gal, tests, activos_out, keep ))
    
  })  
  
  
  # reactive to return ACT_ID
  
  #  data_t <- data
  #  data <- data_t
  
  output$distribucion_plot <- renderPlot(
    { 
      
      px <- distribucion()
      pl <- px[[1]]
      tests <- px[[3]]
      tests <- tests %>% mutate(ACT_ID = as_factor(ACT_ID))
      tests <- tests %>% arrange(desc(st_dev)) 
      pl <- pl %>% mutate(ACT_ID = as_factor(ACT_ID))
      #pl <- data
      
      pl <- pl %>% filter(BOD_ID == input$tipo_, BOD_NAME == input$nombre_, 
                          CAT_ACT_1 %in% input$categoria_)
      
      "CONDICION DE MUESTRA"
      
      muestra <- 5
      tests <- tests %>% filter(n >= 5)
      pl <- pl %>% filter(ACT_ID %in% (tests %>% pull(ACT_ID) %>% unique()))
      
      
      #message("AFTER VARIANCEEEEEEEEEEEEEEEEEE")
      #  print(levels(tests$ACT_ID))
      
      #     message("TEST ORIGINAL")
      #      print(levels(tests$ACT_ID))
      
      lev <- levels(reorder(tests$ACT_ID, 
                            tests$st_dev))
      
      #      message("TEST CONVERTED")
      #      print(levels(tests$ACT_ID))
      
      pl <- pl %>% mutate(ACT_ID = factor(ACT_ID, levels = lev))
      
      
      mean_gal <- pl %>% pull(CANTIDAD) %>% mean()
      
      
      #message("********* ENTERING RENDER PLOT DISTRIBUCION ************")
      
      
      if (nrow(pl) == 0) 
        {
        empty <- activos_tbl %>% head(1) %>% select(CANTIDAD, ACT_ID, CANTIDAD)
        empty <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(empty) <- c("CANTIDAD", "ACT_ID")
        distribucion_out <- empty %>% select(CANTIDAD, ACT_ID) %>% 
          ggplot(aes(x = CANTIDAD , y = reorder(ACT_ID, CANTIDAD),
                     group = ACT_ID)) + geom_blank() + theme_fivethirtyeight() +annotate("text",
                                                                                         x = 1,
                                                                                         y = 1,
                                                                                         size = 3,
                                                                                         label = "En esta seleccion, no se encuentran registros suficientes para hacer una comparacion. Favor de seleccionar otra categoria.") + 
          theme_void()
        
      } else 
        {
        
        # View(pl)
        
        distribucion_out <- pl %>%  ggplot(aes(x = CANTIDAD , y = ACT_ID,
                                               group = ACT_ID,
                                               text = paste0("Fecha de despacho: ", FECHA_DESPACHO ,
                                                             "</br>","</br>Total de despacho (galones): ", CANTIDAD,
                                                             "</br>","Total de despachos: ", times))) + 
          
          geom_density_ridges(colour = "steelblue", fill = "steelblue", alpha = 0.5) +
          
          geom_vline(xintercept=mean_gal, colour="#d45d43", size = 0.5, linetype="dashed") +
          
          #distribucion()[[2]] + 16
          annotate("text", x = 0.7, y = 0.7,
                   label = paste0("Prom: ", round(mean_gal, 1)), colour = "#d45d43",
                   size = 3) +
          
          annotate("text",
                
                   label = distribucion()[[3]] %>% pull(label_g),
                   col = "#d45d43",
                   vjust = -0.7, size = 3) +
          
          theme_light() +
          theme(axis.title = element_text(size = 12), 
                axis.text = element_text(size = 12, face = "bold"),
                axis.line = element_line(size = 0.4, colour = "grey10"), 
                plot.caption = element_text(color = "gray45", 
                                            face = "italic", size = 12),
                plot.title = element_text(hjust = 00.5),
                
                legend.position = "bottom", legend.title=element_blank())  +
          
          labs(title ="") +
          xlab("Despacho (galones)") + 
          ylab("ID de Activo") 
        
      }
      #     }      
      distribucion_out
    }
  )
  
  
  
  ################
  despachos <- reactive (
    {
    
    distribucion_df <- collect_1(data = activos_tbl, tipo = input$tipo_, nombre = input$nombre_, 
                                 cat_act_1 = input$categoria_, muestra = 10, margin = 1,
                                 top_n = NA, muestras = input$muestra_, activo = input$activos_)
    
    data <- distribucion_df[[1]]
    keep <-  distribucion_df[[7]]
    levels_act <- distribucion_df[[5]]
    tests <- distribucion_df[[3]]
    tests <- tests %>% mutate(ACT_ID = as_factor(ACT_ID))
    tests <- tests %>% arrange(desc(st_dev)) 
    tests <- tests %>% slice(1: distribucion_df[[6]])
    
    data_w <- data %>%

      mutate(ACT_ID = as_factor(ACT_ID)) %>% 
      
      group_by(BOD_ID, BOD_NAME, CAT_ACT_1, ACT_ID) %>% summarise(times = sum(times),
                                                                  CANTIDAD = sum(CANTIDAD)) %>% ungroup()
    

    
    return(list(data_w, tests, data))
    
  }) 
  
  output$despachos <- renderPlot(
    { 
      
      pl <- despachos()[[1]]
      tests <- despachos()[[2]]
      
      #tests <- tests %>% mutate(ACT_ID = as_factor(ACT_ID))
      #tests <- tests %>% arrange(desc(variance)) 
      pl <- pl %>% mutate(ACT_ID = as_factor(ACT_ID))
      
      pl <- pl %>% filter(BOD_ID == input$tipo_, BOD_NAME == input$nombre_, 
                          CAT_ACT_1 %in% input$categoria_)
      
      "CONDICION DE MUESTRA"
      
      muestra <- 5
      tests <- tests %>% filter(n >= 5)
      pl <- pl %>% filter(ACT_ID %in% (tests %>% pull(ACT_ID) %>% unique()))
      
      
      lev <- levels(reorder(tests$ACT_ID, 
                            tests$st_dev))
      
      pl <- pl %>% mutate(ACT_ID = factor(ACT_ID, levels = lev))
      
      if (nrow(pl) == 0) 
        {
        empty <- activos_tbl %>% head(1) %>% select(ACT_ID, times)
        empty <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(empty) <- c("CANTIDAD", "ACT_ID")
        despachos_out <- empty %>% select(times, ACT_ID) %>% 
          ggplot(aes(x = times , y = reorder(ACT_ID, times),
                     group = ACT_ID)) + geom_blank() + theme_fivethirtyeight() +annotate("text",
                                                                                         x = 1,
                                                                                         y = 1,
                                                                                         size = 3,
                                                                                         label = "En esta seleccion, no se encuentran registros suficientes para hacer una comparacion. Favor de seleccionar otra categoria.") + 
          theme_void()
        
      } else 
        {

        despachos_out <- pl %>% ggplot(aes(x = ACT_ID, y = times)) + 
        
          geom_bar(stat = "identity", width=0.2, fill = "#1e1e93") +  
          geom_text(aes(label=times), hjust=-0.5, colour = "black", position = "dodge") +

          theme_light() +
          coord_flip() +
          
          theme(
            legend.position="none"
          ) + 
          theme(axis.title = element_text(size = 12), 
                axis.text = element_text(size = 12, face = "bold"),
                axis.line = element_line(size = 0.4, colour = "grey10"), 
                plot.caption = element_text(color = "gray45", 
                                            face = "italic", size = 12),
                plot.subtitle = element_text(size = 15),
                plot.title = element_text(hjust = 0.5),
                legend.position = "bottom", legend.title=element_blank()) +
          labs(title = "") +
          xlab("ID de Activo") + 
          ylab("Numero de despachos")   
      }
      
      despachos_out
    } 
  )
  
  
  horas_trabajadas <- reactive (
    {
    
    horas_tr <- horas_labores %>% filter(ACT_ID %in% input$activos_2) %>%
      mutate(ACT_ID = as.character(ACT_ID))
    
    return(horas_tr)  
  })
  
  
  output$horas_trabajadas <- renderPlotly(
    { 
      
      horas_t <- plot_ly(
        data = horas_trabajadas() ,
        x = ~FECHA, 
        y = ~round(HORAS_PROM,1),
        color = ~ACT_ID,
        colors = "Accent",
        type = "scatter",
        mode = "lines+markers"
      )  %>% layout(yaxis = dy) %>% layout(xaxis = dx) %>%
        layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, font = list(size = 12)), xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE, font = list(size = 12)) 
        )
      horas_t
    }
  )
  
  labores_activo <- reactive (
    {
    
    horas_lab <- labores_activos %>% filter(ACT_ID %in% input$activos_3) %>%
      mutate(ACT_ID = as.character(ACT_ID))
    
    return(horas_lab)  
  })
  
  output$labores_activo <- renderPlotly(
    { 
      #      %>% filter(ACT_ID == "2104")
      
      titulo <- labores_activo() %>% pull(APLICACION_DESCRIP) %>% unique()
      
      labores_g <- plot_ly(labores_activo () , x = ~round(HORAS_PROM,1), 
                           y = ~DESCRILABOR, type = 'bar', orientation = 'h', 
                           color = I("#8b0000")) %>%
        layout(yaxis = list(categoryorder = "total ascending", 
                            categoryarray = ~HORAS_PROM))  %>% layout(yaxis = cy) %>% 
        layout(xaxis = cx) %>%
        layout(yaxis = list(font = list(size = 12)), 
               xaxis = list(font = list(size = 12)) 
        ) %>% layout(title = paste0("APLICACION: ", titulo), 
                     titlefont=list(size=12, color = '#000000'))
      
      labores_g
      
    }
  )
  
  ############################ NEW GRAPHS ####################################
  
  tendencia_despachos <- reactive (
    {
  
      pl <- despachos()[[3]]
      tests <- despachos()[[2]]

      message(" REVISAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
      
      nrow(pl)
      
      pl <- pl %>% mutate(ACT_ID = as_factor(ACT_ID))
      pl <- pl %>% filter(BOD_ID == input$tipo_, BOD_NAME == input$nombre_, 
                          CAT_ACT_1 %in% input$categoria_)
      
      "CONDICION DE MUESTRA"
      
      muestra <- 5
      tests <- tests %>% filter(n >= 5)
      pl <- pl %>% filter(ACT_ID %in% (tests %>% pull(ACT_ID) %>% unique()))
      
      lev <- levels(reorder(tests$ACT_ID, 
                            tests$st_dev))
      
      pl <- pl %>% mutate(ACT_ID = factor(ACT_ID, levels = lev))
      
      pl <- pl %>% select(FECHA_DESPACHO, ACT_ID, CANTIDAD) %>% group_by(ACT_ID) %>%
        summarise_by_time(
          .date_var = FECHA_DESPACHO,
          .by       = "month", 
          DESPACHOS_PROM  = mean(CANTIDAD)
        )
      
      return(pl)
      
    })
  
  
  output$tendencia_despachos <- renderPlotly(
    { 
      
      message("********* ENTERING TENDENCIA DE ACTIVOS ************")
      
      pl <- tendencia_despachos() %>% filter(ACT_ID %in% input$activos_4) 
      
      if (nrow(pl) == 0) 
      {
        empty <- activos_tbl %>% head(1) %>% select(CANTIDAD, ACT_ID, CANTIDAD)
        empty <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(empty) <- c("CANTIDAD", "ACT_ID")
        horas_d_ <- empty %>% select(CANTIDAD, ACT_ID) %>% 
          ggplot(aes(x = CANTIDAD , y = reorder(ACT_ID, CANTIDAD),
                     group = ACT_ID)) + geom_blank() + theme_fivethirtyeight() +annotate("text",
                                                                                         x = 1,
                                                                                         y = 1,
                                                                                         size = 3,
                                                                                         label = "En esta seleccion, no se encuentran registros suficientes para hacer una comparacion. Favor de seleccionar otra categoria.") + 
          theme_void() %>% ggplotly()
        
      } else 
      {
        
        horas_d <- plot_ly(
          data = pl ,
          x = ~FECHA_DESPACHO, 
          y = ~round(DESPACHOS_PROM,1),
          color = ~ACT_ID,
          colors = "Accent",
          type = "scatter",
          mode = "lines+markers"
        )  %>% layout(yaxis = wy) %>% layout(xaxis = wx) %>%
          layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, font = list(size = 12)), xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE, font = list(size = 12)) 
          ) 
        
        
      }
      
      
      return(horas_d)
     
    }
  )
  
  

  tendencia_despachos_num <- reactive (
    {
      
      pl <- despachos()[[3]]
      tests <- despachos()[[2]]
 
      
      pl <- pl %>% mutate(ACT_ID = as_factor(ACT_ID))
      pl <- pl %>% filter(BOD_ID == input$tipo_, BOD_NAME == input$nombre_, 
                          CAT_ACT_1 %in% input$categoria_)
      
      "CONDICION DE MUESTRA"
      
      muestra <- 5
      tests <- tests %>% filter(n >= 5)
      pl <- pl %>% filter(ACT_ID %in% (tests %>% pull(ACT_ID) %>% unique()))
      
      lev <- levels(reorder(tests$ACT_ID, 
                            tests$st_dev))
      
      pl <- pl %>% mutate(ACT_ID = factor(ACT_ID, levels = lev))
      
      pl <- pl %>% select(FECHA_DESPACHO, ACT_ID, times) %>% group_by(ACT_ID) %>%
        summarise_by_time(
          .date_var = FECHA_DESPACHO,
          .by       = "month", 
          TIMES_PROM  = sum(times)
        )
      
      return(pl)
      
    })
  
  
  output$tendencia_despachos_num <- renderPlotly(
    { 
      
      message("********* ENTERING TENDENCIA DE ACTIVOS ************")
      
      pl <- tendencia_despachos_num() %>% filter(ACT_ID %in% input$activos_5) 
      
      if (nrow(pl) == 0) 
      {
        empty <- activos_tbl %>% head(1) %>% select(CANTIDAD, ACT_ID, CANTIDAD)
        empty <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(empty) <- c("CANTIDAD", "ACT_ID")
        horas_d_ <- empty %>% select(CANTIDAD, ACT_ID) %>% 
          ggplot(aes(x = CANTIDAD , y = reorder(ACT_ID, CANTIDAD),
                     group = ACT_ID)) + geom_blank() + theme_fivethirtyeight() +annotate("text",
                                                                                         x = 1,
                                                                                         y = 1,
                                                                                         size = 3,
                                                                                         label = "En esta seleccion, no se encuentran registros suficientes para hacer una comparacion. Favor de seleccionar otra categoria.") + 
          theme_void() %>% ggplotly()
        
      } else 
      {
        
        horas_d <- plot_ly(
          data = pl ,
          x = ~FECHA_DESPACHO, 
          y = ~round(TIMES_PROM,0),
          color = ~ACT_ID,
          colors = "Accent",
          type = "scatter",
          mode = "lines+markers"
        )  %>% layout(yaxis = zy) %>% layout(xaxis = zx) %>%
          layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, font = list(size = 12)), xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE, font = list(size = 12)) 
          )
        
        
      }
      
      return(horas_d)
      
    }
  )
  

  filter_table <- reactive ( {
    
  
    table <- activos_report_tbl %>% select(ACTIVO, `MAXIMO CARGA (Gal)` = `MAX_CARGA (Gal)`, `PROMEDIO CARGA (Gal)` = `PROMEDIO_CARGA (Gal)`,
                                           `UTILIZACION (CATEGORIA)` = `UTILIZACION (CATEGORIA)`, `%UTILIZACION`, 
                                           `CORRELACION CARGA Y HRS (CAT)` = CORRELACION_CATEGORIA,
                                           `CORRELACION CARGA Y HRS` = correlacion,
                                           
                                           `CORRELACION CARGA Y KMS (CAT)` = CORRELACION_K_CATEGORIA,
                                           `CORRELACION CARGA Y KMS` = correlacion_k,
                                           
                                           `FRECUENCIA (viajes)`, `ANALISIS ABC-XYZ` =   `ABC-XYZ`,
                                           ACTIVIDAD_DESCRIP, DESCRIPCION, APLICACION_DESCRIP) %>%
      mutate(`CORRELACION CARGA Y HRS` = round(`CORRELACION CARGA Y HRS` , 2)) %>%
      mutate(`CORRELACION CARGA Y KMS` = round(`CORRELACION CARGA Y KMS` , 2)) %>%
      mutate(`PROMEDIO CARGA (Gal)` = round(`PROMEDIO CARGA (Gal)`,1)) %>%
      mutate(`MAXIMO CARGA (Gal)` = round(`MAXIMO CARGA (Gal)`, 1)) %>%
      mutate(`%UTILIZACION` = 100 * round(`%UTILIZACION`, 1)) %>%
      
      filter( `UTILIZACION (CATEGORIA)` %in% input$utilizacion_ & `CORRELACION CARGA Y HRS (CAT)` %in% input$correlacion_ &
                `CORRELACION CARGA Y KMS (CAT)` %in% input$correlacion_k &
                `ANALISIS ABC-XYZ` %in% input$abc_ & DESCRIPCION %in% input$actividad_ & ACTIVO %in% input$activos_detail_) %>%
      arrange(desc(`%UTILIZACION`), desc(`CORRELACION CARGA Y HRS`))
    
    table

  })
  
  output$table_filtered <- DT::renderDataTable({
    
    DT::datatable(filter_table(), height = "700px", class = 'cell-border stripe', 
                  style = 'bootstrap', rownames = FALSE, options = list(dom = 'tp', 
                                                                        lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                                               
                                                                        language = list(
                                                                          zeroRecords = "Ningun registro. Favor, modificar el estado de los filtros"),
                                                                        columnDefs = list(
                                                                          list(className = "dt-center", targets = "_all"))) 
                  
                    
                    
    ) %>% formatStyle( 0, target= 'row', fontWeight ='bold', lineHeight='10%')
  })
  
  
  
  correlacion_data <- reactive (
    {
      
      pl <- activos_op_tbl %>% filter(ACT_ID %in% input$activos_6 ) 
      
      return(pl)
      
    })
  

  output$corrplot <- renderPlotly(
    { 
      
      fig <- plot_ly()
      
      fig <- fig %>% add_trace (
        data = correlacion_data(),
        name = "Carga (galones)",
        x = ~FECHA_DESPACHO, 
        y = ~round(CARGA,1),
        #color = "#8b0000",
        # colors = "Accent",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = '#00239c', width = 2)
      ) %>%
        layout(yaxis = sy1) %>% layout(xaxis = dx) %>%
        layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, 
                            font = list(size = 12)), 
               xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE, font = list(size = 12)) 
        ) %>%
   
        add_trace (
          data = correlacion_data(),
          name = "Horas trabajadas",
          x = ~FECHA_DESPACHO, 
          y = ~round(HORAS_PROM,1),
          type = "scatter",
          mode = "lines+markers",
          line = list(color = '#8b0000', width = 3),
          yaxis = "y2"
        ) %>% layout(
           yaxis2 = sy2,
           yaxis = list(title="<b>Carga (Galones)</b> por activo")
        ) %>% 
        layout(plot_bgcolor='#ffff',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = '#ffff'),
                 yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = '#ffff')
               
        ) %>% layout(legend = list(orientation = 'h')) %>%
        layout(autosize = F, width = 900, height = 450, margin = m)
      
      
      fig  
      
    }
  )
  
  
  output$corrplot_k <- renderPlotly(
    { 
      
      fig <- plot_ly()
      
      fig <- fig %>% add_trace (
        data = correlacion_data(),
        name = "Carga (galones)",
        x = ~FECHA_DESPACHO, 
        y = ~round(CARGA,1),
        #color = "#8b0000",
        # colors = "Accent",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = '#00239c', width = 2)
      ) %>%
        layout(yaxis = sy1) %>% layout(xaxis = dx) %>%
        layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, 
                            font = list(size = 12)), 
               xaxis = list(showgrid = FALSE, title = FALSE, showline = FALSE, font = list(size = 12)) 
        ) %>%
        
        add_trace (
          data = correlacion_data(),
          name = "Kilometros recorridos",
          x = ~FECHA_DESPACHO, 
          y = ~round(KMS,1),
          type = "scatter",
          mode = "lines+markers",
          line = list(color = '#8b0000', width = 3),
          yaxis = "y2"
        ) %>% layout(
          yaxis2 = sy2,
          yaxis = list(title="<b>Carga (Galones)</b> por activo")
        ) %>% 
        layout(plot_bgcolor='#ffff',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = '#ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = '#ffff')
               
        ) %>% layout(legend = list(orientation = 'h')) %>%
        layout(autosize = F, width = 900, height = 450, margin = m)
      
      
      fig  
      
    }
  )
  
  propuesta_data <- reactive (
    {
      
      activo_f <-  activos_report_tbl %>% filter(ACTIVO == input$activos_7)
      nuevos_viajes <- as_tibble(seq(0.05, 1, by = 0.01)) %>% arrange(-desc(value)) %>% mutate(
        n_f = (activo_f$`PROMEDIO_CARGA (Gal)` * activo_f$`FRECUENCIA (viajes)`) / (value * activo_f$`MAX_CARGA (Gal)`) 
      ) %>% mutate(utilizacion = round(value * 100,0)) %>% mutate(o_f = activo_f$`FRECUENCIA (viajes)`) 
      
      return(nuevos_viajes)
      
    })
  
  
  
  output$propuesta <- renderPlotly(
    { 
      
      viajes <- plot_ly()
      
      viajes <- viajes %>% add_trace (
        data = propuesta_data(),
        name = "Viajes (Optimizacion)",
        x = ~utilizacion, 
        y = ~round(n_f,0),
        type = "scatter",
        mode = "lines+markers",
        line = list(color = '#00239c', width = 2)
      ) %>%
        layout(yaxis = ky) %>% layout(xaxis = kx) %>%
        layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, 
                            font = list(size = 12)), 
               xaxis = list(showgrid = FALSE,  showline = FALSE, font = list(size = 12)) 
        ) %>%
        
        add_trace (
          data = propuesta_data(),
          name = "Promedio de Viajes",
          x = ~utilizacion, 
          y = ~o_f,
          type = "scatter",
          mode = "lines",
          line = list(color = '#8b0000', width = 3)
        )  %>%  layout(legend = list(orientation = 'h')) 
      
      viajes
      
    }
  )
  
  
}





