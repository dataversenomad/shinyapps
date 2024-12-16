
server <- function(input, output, session) { 
  
  ###################################  FILTERS ###################################
  

  # FILTERS & CONDITIONALS
  
  # Filtered Data
  filtered_data <- reactiveVal(expenses_df)
  
  observeEvent(input$apply_filters_button, {
    filtered_data(filter_logic())
  })
  
  observeEvent(input$reset_filters_button, {
    reset_filters()
  })
  
  reset_filters <- function() {
    updatePickerInput(session, "CARD_tbl", selected = "All")
    updatePickerInput(session, "CURRENCY_tbl", selected = "All")
    updatePickerInput(session, "Period_tbl", selected = "All")
    updatePickerInput(session, "year_tbl", selected = "All")
    updatePickerInput(session, "month_tbl", selected = "All")
    updatePickerInput(session, "day_tbl", selected = "All")
    updatePickerInput(session, "week_tbl", selected = "All")
    updatePickerInput(session, "CATEGORY_tbl", selected = "All")
    updatePickerInput(session, "CATEGORY_NEW_tbl", selected = "All")
    updatePickerInput(session, "CATEGORY_GROUP_tbl", selected = "All")
    updatePickerInput(session, "TIPO_DE_MOVMIENTO_tbl", selected = "All")
    updatePickerInput(session, "NO_DOC_tbl", selected = "All")
    updatePickerInput(session, "COMERCIO_tbl", selected = "All")
    updatePickerInput(session, "VALUE_tbl", selected = "All")
    
    
    filtered_data(expenses_df)
  }
  
  filter_logic <- function() {
    filtered <- expenses_df
    
    if ("All" %in% input$CARD_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$CARD %in% input$CARD_tbl, ]
    }
    
    if ("All" %in% input$CURRENCY_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$CURRENCY %in% input$CURRENCY_tbl, ]
    }
    
    
    # Filter 1
    if ("All" %in% input$Period_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$Period %in% input$Period_tbl, ]
    }
    
    # Filter 2
    if ("All" %in% input$year_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$year %in% input$year_tbl, ]
    }
    
    # Filter 3
    if ("All" %in% input$month_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$month %in% input$month_tbl, ]
    }
    
    # Filter 4
    if ("All" %in% input$day_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$day %in% input$day_tbl, ]
    }
    
    
    # Filter 6
    if ("All" %in% input$week_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$week %in% input$week_tbl, ]
    }
    
    
    # Filter 5
    if ("All" %in% input$CATEGORY_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$CATEGORY %in% input$CATEGORY_tbl, ]
    }
    
    
    # start
    
    # Filter 5
    if ("All" %in% input$CATEGORY_NEW_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$CATEGORY_NEW %in% input$CATEGORY_NEW_tbl, ]
    }
    
    # Filter 5
    if ("All" %in% input$CATEGORY_GROUP_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$CATEGORY_GROUP %in% input$CATEGORY_GROUP_tbl, ]
    }
    
    # Filter 5
    if ("All" %in% input$TIPO_DE_MOVMIENTO_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$`TIPO DE MOVMIENTO` %in% input$TIPO_DE_MOVMIENTO_tbl, ]
    }
    
    # Filter 5
    if ("All" %in% input$NO_DOC_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$`NO. DOC` %in% input$NO_DOC_tbl, ]
    }
    
    # Filter 5
    if ("All" %in% input$COMERCIO_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$COMERCIO %in% input$COMERCIO_tbl, ]
    }
    
    # VALUE
    if ("All" %in% input$VALUE_tbl) {
      # Include all values
    } else {
      filtered <- filtered[filtered$VALUE_CAT %in% input$VALUE_tbl, ]
    }
    
    
    filtered
  }
  
  
  output$card <- renderUI({
    
    pickerInput(inputId = "card_", "Card", choices = var_card()  , 
                label = "Card",
                selected = c("5524-XXXX-XXXX-1119") , multiple = FALSE)
  })
  
  output$currency <- renderUI({
    
    pickerInput(inputId = "currency_", "Currency", choices = var_currency()  , 
                label = "Currency",
                selected = c("local") , multiple = FALSE)
  })
  
  output$year <- renderUI({
    
    pickerInput(inputId = "year_", "Year", choices = var_year()  , 
                label = "Year",
                selected = var_year() , multiple = TRUE)
  })
  
  output$week <- renderUI({
    
    pickerInput(inputId = "week_", "Week", choices = var_week()  , 
                label = "Week",
                selected = var_week() , multiple = TRUE)
  })
  
  output$month <- renderUI({
    
    pickerInput(inputId = "month_", "Month", choices = var_month()  , 
                label = "Month",
                selected = var_month() , multiple = TRUE)
  })
  
  output$Period <- renderUI({
    
    pickerInput(inputId = "period_", "Period", choices = var_periods()  ,
                label = "Period",
                selected = var_period() , multiple = FALSE)
  })
  
  
  
  ############################### BOXES #########################################
  output$box1 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box1", width = NULL, height = 400,
              tabPanel(title = "Bal.$ (D-D)", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("tendencia_balance", height = 350)) ), 
              tabPanel( title = "Bal.% (ini)", value = "T2",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_inicial", height = 350))),
              tabPanel( title = "Bal.% (WoW)", value = "T3",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_week", height = 350))),
              tabPanel( title = "Bal.% (MoM)", value = "T4",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_month", height = 350))),
              tabPanel( title = "CM% (WoW)", value = "T4",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("contribution_margin", height = 350)))
              
      )
    )
  }
  ) 
  
  output$box2 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box2", width = NULL, height = 400,
              tabPanel(title = "Overall Exp%", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("debit_proportion", height = 350)) ), 
              tabPanel( title = "Controlled Exp vs Pay", value = "T2",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_exp_pay", height = 350))),
              tabPanel( title = "Control Exp WoW Tr.", value = "T3",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_pareto", height = 350))),
              
      )
    )
  }
  ) 
  
  output$box3 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box3", width = NULL, height = 400,
              tabPanel(title = "Exp.$ (WoW)", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("tendencia_gastos_w", height = 350)) ), 
              tabPanel( title = "Exp.$% (WoW)", value = "T2",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_gastos_wp", height = 350))),
              tabPanel( title = "Exp.$ (MoM)", value = "T3",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("tendencia_gastos_m", height = 350))),

              
      )
    )
  }
  )  
  
  output$box4 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box4", width = NULL, height = 400,
              tabPanel(title = "Exp.$ (Tot. Month)", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("gastos_month", height = 350)) ), 
              tabPanel( title = "Exp.$ (Dist. Month)", value = "T2",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("box_gastos", height = 350))),
              tabPanel( title = "Exp.$ (Avg. Month)", value = "T3",
                        div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                        div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                        withSpinner(plotlyOutput("gastos_month_avg", height = 350))),
              
              
      )
    )
  }
  ) 
  
  
  output$box5 <- renderUI({
    div(
      style = "position: relative;",
      tabBox(
        id = "box5", width = NULL, height = 350,
        tabPanel(
          title = "Exp.Det.$ (MoM)", value = "T151",
          div(
            style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100;",
            dropdown(
              pickerInput(
                inputId = "categorias",  # Keeps original `inputId` for Box 5
                label = "Categoria",
                width = "100px",
                choices = var_categorias(),
                selected = c("Controlled"),
                multiple = FALSE
              ),
              size = "xs", animate = TRUE,
              label = "Categoria",
              icon = icon("gear", class = "opt"),  
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("detalle_categoria", height = 300),
            type = 4, color = "#d33724", size = 0.7
          )
        )
      )
    )
  })
  
  output$box6 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox(
        id = "box6", width = NULL, height = 400,
        tabPanel(
          title = "Exp.Det.Tot.$ (MoM)", value = "T151",
          div(
            style = "position: absolute; left: 0 em; bottom: 0.5em;  z-index: 100;",
          ),
          withSpinner(
            plotlyOutput("detalle_categoria_tot", height = 300),
            type = 4, color = "#d33724", size = 0.7
          )
        )
      )
    )
  })
  
  
  output$box7 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box7", width = NULL, height = 400,
              tabPanel(title = "Expense Tracker Waterfall ($)", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("expense_tracker_plot", height = 350)) )
              
      )
    )
  }
  ) 
  
  
  output$box8 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box8", width = NULL, height = 400,
              tabPanel(title = "Bal. Tracker ($) VISA", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("tendencia_balance_tracker_c1", height = 350)) )
              
      )
    )
  }
  ) 
  
  output$box9 <- renderUI({
    div(
      style = "position: relative; background-color: #ECF0F5;",
      tabBox( id = "box9", width = NULL, height = 400,
              tabPanel(title = "Bal. Tracker ($) MASTER", value = "T1",
                       div( style = "position: absolute; left: 0.5em; bottom: 0.5em;background-color: #ECF0F5;",),
                       div( style = "position: absolute; left: 4em;bottom: 0.5em;background-color: #ECF0F5;", ),
                       withSpinner(plotlyOutput("tendencia_balance_tracker_c2", height = 350)) )
              
      )
    )
  }
  ) 
  
  
  
  # FILTROS
  
  
  var_card <- reactive({
    as.list(c("5524-XXXX-XXXX-1119", "4487-XXXX-XXXX-5978"))
  })
  
  var_currency <- reactive({
    
    expenses_df %>% filter(CARD_ %in% var_card()) %>% pull(CURRENCY) %>% unique()
    
  })
  var_year <- reactive({
    
    expenses_df %>% filter(
      CARD_ %in% var_card() &
        CURRENCY_ %in% var_currency()
      
    ) %>% pull(year) %>% unique()
    
  })
  
  var_week <- reactive({
    
    expenses_df %>% filter(
      CARD_ %in% var_card() &
        CURRENCY_ %in% var_currency() &
        year_ %in% var_year()
      
    ) %>% pull(week) %>% unique()
    
  })
  
  var_month <- reactive({
    
    expenses_df %>% filter(
      CARD_ %in% var_card() &
        CURRENCY_ %in% var_currency() &
        year_ %in% var_year() &
        week_ %in% var_week()
      
    ) %>% pull(month) %>% unique()
    
  })
  
  var_categorias <- reactive({
    
    expenses_df %>% filter(
      `TIPO DE MOVMIENTO` == 'DEBITO' &
        CARD_ %in% var_card() &
        CURRENCY_ %in% var_currency() &
        year_ %in% var_year() &
        week_ %in% var_week() &
        month %in% var_month()
      
    ) %>% pull(CATEGORY_GROUP) %>% unique()
    
  })
  
  
  var_period <- reactive({
    
    periods <- visa_pay %>% pull(Period) %>% unique()
    period <- periods[length(periods)]
    period
    
  })
  
  var_periods <- reactive({
    
    periods <- visa_pay %>% pull(Period) %>% unique()
    periods
    
  })
  
  
  
  expenses <- reactive ({
    
    master <- collect_1(data = expenses_df, 
                        card = input$card_,
                        curr = input$currency_,
                        ys = input$year_,
                        wk = input$week_,
                        mnt = input$month_
                        
    )
    
    master_df <- master
    
    return(master_df)
    
  })  
  
  credit_limit <- reactive ({
    
    master <- collect_2(data = expenses_df, 
                        card = input$card_,
                        curr = input$currency_
    )
    
    master_df <- master
    
    return(master_df)
    
  })  
  
  
  expenses_details <- reactive({
    master <- collect_1(
      data = expenses_df,
      card = input$card_,
      curr = input$currency_,
      ys = input$year_,
      wk = input$week_,
      mnt = input$month_
    )
    master %>% filter(CATEGORY_GROUP %in% input$categorias)
  })
  
  expenses_details_tot <- reactive({
    expenses_df %>%
      filter(CATEGORY == "DEBITO") %>%
      mutate(MONTH = month_) %>%
      group_by(MONTH, CATEGORY_NEW) %>%
      summarize(VALUE = sum(VALUE)) %>%
      ungroup() %>%
      mutate(VALUE = round(as.integer(VALUE)))
  })
  
  expenses_details_total <- reactive({
    master <- collect_1(
      data = expenses_df,
      card = input$card_,
      curr = input$currency_,
      ys = input$year_,
      wk = input$week_,
      mnt = input$month_
    )
   
  })  
  
  
  expense_tracker <- reactive ({
    
    master <- collect_3(period = input$period_)
    
    return(master)
    
  }) 
  

  
  output$tendencia_balance <- renderPlotly(
    { 
      
      df <- expenses()
      
      
      balance_q_d <- df %>% select(FECHA, CATEGORY, CURRENCY, CARD, VALUE, BALANCE) %>% 
        arrange(-desc(FECHA)) %>% group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "day",
          BALANCE_LAST  = last(BALANCE)
        )
      
      
      trend_line <- lm(BALANCE_LAST ~ as.numeric(FECHA), data = balance_q_d)
      m_value <- coef(trend_line)[2]  
      df_plot <- balance_q_d %>%  
        plot_ly(x = ~FECHA, y = ~BALANCE_LAST, type = "scatter", mode = "lines+markers", line = list(color = "steelblue", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = predict(trend_line), type = "scatter", mode = "lines", line = list(color = "red", dash = "dash", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = rep(0, nrow(balance_q_d)), type = "scatter", mode = "lines", line = list(color = "darkgray", dash = "dash", width = 1)) %>%
        layout(
          
          
          #xaxis = list(title = "Date"),
          
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 20),
          
          yaxis = list(title = "Balance (Q)"),
          showlegend = FALSE)
      
      df_plot
    }
  )
  
  
  output$tendencia_inicial <- renderPlotly(
    { 
      
      df <- expenses()
      
      balance_ini <- df  %>% 
        arrange(-desc(FECHA)) %>% head(1) %>% pull(BALANCE)
      

      balance_q_d <- df %>% select(FECHA, CATEGORY, CURRENCY, CARD, VALUE, BALANCE) %>%
        arrange(-desc(FECHA)) %>% group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "day",
          BALANCE_LAST  = last(BALANCE)
        ) %>%
        mutate(bal_pct = 100 * (BALANCE_LAST - balance_ini) / balance_ini)
      
      
      trend_line <- lm(bal_pct ~ as.numeric(FECHA), data = balance_q_d)
      m_value <- coef(trend_line)[2] 
      df_plot <- balance_q_d %>%
        plot_ly(x = ~FECHA, y = ~bal_pct, type = "scatter", mode = "lines+markers", line = list(color = "steelblue", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = predict(trend_line), type = "scatter", mode = "lines", line = list(color = "red", dash = "dash", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = rep(0, nrow(balance_q_d)), type = "scatter", mode = "lines", line = list(color = "darkgray", dash = "dash", width = 1)) %>%
        layout(
          title = list(text = paste("(m =", round(m_value, 4), ")"), size = 3, x = 0, y = 1),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 20),
          
          yaxis = list(title = "% Difference"),
          showlegend = FALSE)
      
      df_plot
      
    }
  )  
  
  
  output$contribution_margin <- renderPlotly(
    { 
      
      df <- expenses()
      
      credit_limit <- credit_limit() %>%  

        arrange(-desc(FECHA)) %>%
        select(CARD, LIMIT) %>% 
        mutate(
          CARD = as.character(CARD), 
          LIMIT = as.character(LIMIT)) %>% 
        group_by(CARD) %>% summarize(REVENUE = as.numeric(last(LIMIT))) %>%
        ungroup() %>% pull(REVENUE)
      
      
      # percent change initial balance
      balance_q_d <- df %>% select(FECHA, CATEGORY, CURRENCY, CARD, VALUE, BALANCE) %>%
        arrange(-desc(FECHA)) %>% group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "week",
          BALANCE_LAST  = last(BALANCE)
        ) %>%
        mutate(bal_pct = 100 * (BALANCE_LAST - credit_limit) / credit_limit)
      
      trend_line <- lm(bal_pct ~ as.numeric(FECHA), data = balance_q_d)
      m_value <- coef(trend_line)[2] 
      df_plot <- balance_q_d %>%
        plot_ly(x = ~FECHA, y = ~bal_pct, type = "scatter", mode = "lines+markers", line = list(color = "steelblue", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = predict(trend_line), type = "scatter", mode = "lines", line = list(color = "red", dash = "dash", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = rep(0, nrow(balance_q_d)), type = "scatter", mode = "lines", line = list(color = "darkred", dash = "dash", width = 2)) %>%
        add_trace(x = balance_q_d$FECHA, y = rep(-1*100, nrow(balance_q_d)), type = "scatter", mode = "lines", line = list(color = "darkgreen", dash = "dash", width = 2)) %>%
        layout(
          title = list(text = paste("(m =", round(m_value, 4), ")"), size = 3, x = 0, y = 1),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 20),
          yaxis = list(title = "% Difference (Balance vs Limit)"),
          showlegend = FALSE)
      
      df_plot
      
    }
  ) 
  
  
  
  
  output$tendencia_week <- renderPlotly(
    { 
      
      df <- expenses()
      
      balance_q_w <- df %>% select(FECHA, CATEGORY, CURRENCY, CARD, VALUE, BALANCE) %>%
        arrange(-desc(FECHA)) %>% group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "week",
          BALANCE_LAST  = last(BALANCE)
        ) %>% mutate(bal_pct = 100 * (BALANCE_LAST - lag(BALANCE_LAST)) / lag(BALANCE_LAST))
      

      df_plot <- balance_q_w %>%
        plot_ly(x = ~FECHA, y = ~bal_pct, type = "scatter", mode = "lines+markers", line = list(color = "steelblue", width = 2)) %>%
       
        add_trace(x = balance_q_w$FECHA, y = rep(0, nrow(balance_q_w)), type = "scatter", mode = "lines", line = list(color = "darkgray", dash = "dash", width = 1)) %>%
        layout(
          
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          

          yaxis = list(title = "% Difference"),
          showlegend = FALSE)
      
      df_plot
      
    }
  ) 
  
  
  output$tendencia_month <- renderPlotly(
    { 
      
      df <- expenses()
      
      balance_q_m <- df %>% select(FECHA, CATEGORY, CURRENCY, CARD, VALUE, BALANCE) %>%
        arrange(-desc(FECHA)) %>% group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "month",
          BALANCE_LAST  = last(BALANCE)
        ) %>% mutate(bal_pct = 100 * (BALANCE_LAST - lag(BALANCE_LAST)) / lag(BALANCE_LAST))
      

      df_plot <- balance_q_m %>%
        plot_ly(x = ~FECHA, y = ~bal_pct, type = "scatter", mode = "lines+markers", line = list(color = "steelblue", width = 2)) %>%
       
        add_trace(x = balance_q_m$FECHA, y = rep(0, nrow(balance_q_m)), type = "scatter", mode = "lines", line = list(color = "darkgray", dash = "dash", width = 1)) %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          
          yaxis = list(title = "% Difference"),
          showlegend = FALSE)
      
      df_plot
      
    }
  )   
  
  
  output$debit_proportion <- renderPlotly(
    { 
      
      df <- expenses()
      
      df <- df %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA, CATEGORY) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "month",
          VALUE  = sum(VALUE)) %>%
        mutate(Proportion = VALUE / sum(VALUE)) %>%
        mutate(TH = 0.5)
      
      
      # Plotting
      plot_df <- plot_ly(df, x = ~FECHA, y = ~Proportion, color = ~CATEGORY, type = 'bar', hoverinfo = 'y+name',
                         colors = c('DEBITO' = 'red', 'PAGO' = 'darkgreen')) %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          
          legend = list(
            x = 0.5,  
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal",  
            font = list(size = 5),  
            orientation = "h"  
          ),
          
          yaxis = list(title = 'Proportion %',  range = c(0, 1)),
          barmode = 'stack')
      
      plot_df
    }
  )    
  
  
  output$tendencia_exp_pay <- renderPlotly(
    { 
      
      df <- expenses()
      df <- df %>%
        filter( (CATEGORY == 'PAGO') |
                  ( CATEGORY == 'DEBITO' & CATEGORY_NEW %in% c('debito_elec', 'entertaining', 'groceries', 'health', 'other',  
                                                               'pets', 'restaurant', 'fixed') )) %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA, CATEGORY) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "month",
          VALUE  = sum(VALUE)) %>% ungroup()
      
      df_plot <- df %>%
        plot_ly(x = ~FECHA, y = ~VALUE, type = "scatter",color = ~CATEGORY , mode = "lines+markers",
                colors = c('DEBITO' = 'red', 'PAGO' = 'darkgreen')) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = "Amount"),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5, 
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal",  
            font = list(size = 5),  
            orientation = "h"  
          )
          
        )
      
      
      df_plot
    }
  )    
  
  output$tendencia_pareto <- renderPlotly(
    { 
      
      df <- expenses()
      
      df <- df %>%
        filter(CATEGORY == "DEBITO") %>%
        filter(CURRENCY == "local") %>%
        # gastos
        filter(CATEGORY_NEW %in% c('debito_elec', 'entertaining', 'groceries', 'health', 'other',  
                                   'pets', 'restaurant', 'fixed') ) %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "week",
          VALUE  = sum(VALUE)) %>% 
        mutate(wow = 100 * (VALUE - lag(VALUE)) / lag(VALUE)) %>%
        ungroup()
      
      
      # Plotting
      df_plot <- df %>%
        plot_ly(x = ~FECHA, y = ~VALUE,  type = "bar", name = "Expense Amt") %>%
        add_trace(x = ~FECHA, y = ~wow, type = "scatter", mode = "lines+markers", name = "WoW Exp %", yaxis = "y2",
                  line = list(color = "red", width = 2),
                  marker = list(color = "red", size = 8)) %>%
        layout(
          
          yaxis = list(title = "Expenses"),
          yaxis2 = list(title = "WoW %", overlaying = "y", side = "right", showgrid = FALSE),
          showlegend = FALSE,
          xaxis = list(title = ""),
          margin = list(l = 10, r = 25, b = 10, t = 10),
          shapes = list(
            list(
              type = "line",
              x0 = min(df$FECHA),
              x1 = max(df$FECHA),
              y0 = 1200,
              y1 = 1200,
              line = list(color = "darkgreen", width = 2, dash = "dash")
            )
          )
        )
      
      df_plot
    }
  )  
  
  
  output$tendencia_gastos_w <- renderPlotly(
    { 
      
      df <- expenses() %>%
        filter(CATEGORY == "DEBITO") %>%
        filter(CATEGORY_GROUP != "Finance") %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA, CATEGORY_GROUP) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "week",
          VALUE  = sum(VALUE)) %>% ungroup() 
      
      df_plot <- df %>%
        plot_ly(x = ~FECHA, y = ~VALUE, type = "scatter",color = ~CATEGORY_GROUP , 
                mode = "lines+markers",
                colors = c('Controlled' = 'darkred', 'Cuotas' = 'darkorange',
                           'Finance' = 'darkgoldenrod4', 'Fixed' = 'darkblue',
                           'Health' = 'darkgreen')) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = "Amount"),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  # Set to 0.5 for center
            y = 1,  # Set to 1 for top
            xanchor = "center",  # Set to "center" for top center
            yanchor = "bottom",  # Set to "bottom" for top-aligned
            traceorder = "normal",  # Set to "normal" for original order
            font = list(size = 5),  # Adjust font size as needed
            orientation = "h"  # Set to "h" for horizontal
          )
        )
      
      df_plot  
      
    }
  ) 
  
  
  output$tendencia_gastos_wp <- renderPlotly(
    { 
      
      balance_q_w <- expenses() %>%
        filter(CATEGORY == "DEBITO") %>%
        filter(CATEGORY_GROUP != "Finance") %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "week",
          VALUE  = sum(VALUE)) %>% 
        mutate(val_pct = 100 * (VALUE - lag(VALUE)) / lag(VALUE)) %>%
        ungroup() 
      
      
      df_plot <- balance_q_w %>%
        plot_ly(x = ~FECHA, y = ~val_pct, type = "scatter", mode = "lines+markers", line = list(color = "firebrick4", width = 2)) %>%
        
        add_trace(x = balance_q_w$FECHA, y = rep(0, nrow(balance_q_w)), type = "scatter", mode = "lines", line = list(color = "goldenrod4", dash = "dash", width = 2)) %>%
        layout(
          
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          
          yaxis = list(title = "WoW Expense Diff %"),
          showlegend = FALSE)
      
      df_plot
      
    }
  )  
  
  
  
  output$tendencia_gastos_m <- renderPlotly(
    { 
      
      df <- expenses() %>%
        filter(CATEGORY == "DEBITO") %>%
        filter(CATEGORY_GROUP != "Finance") %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA, CATEGORY_GROUP) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "month",
          VALUE  = sum(VALUE)) %>% ungroup() 
      
      df_plot <- df %>%
        plot_ly(x = ~FECHA, y = ~VALUE, type = "scatter",color = ~CATEGORY_GROUP , 
                mode = "lines+markers",
                colors = c('Controlled' = 'darkred', 'Cuotas' = 'darkorange',
                           'Finance' = 'darkgoldenrod4', 'Fixed' = 'darkblue',
                           'Health' = 'darkgreen')) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = "Amount"),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  # Set to 0.5 for center
            y = 1,  # Set to 1 for top
            xanchor = "center",  # Set to "center" for top center
            yanchor = "bottom",  # Set to "bottom" for top-aligned
            traceorder = "normal",  # Set to "normal" for original order
            font = list(size = 5),  # Adjust font size as needed
            orientation = "h"  # Set to "h" for horizontal
          )
        )
      
      df_plot  
      
    }
  )
  
  
  output$gastos_month <- renderPlotly(
    { 
      
      last_4_months <- expenses() %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = as.numeric(month_)) %>%
        group_by(MONTH) %>%
        summarise(cnt = n()) %>% ungroup() %>%
        arrange(desc(MONTH)) %>% head(4) %>% pull(MONTH) %>% unique()
      
      df <- expenses() %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = month_) %>%
        filter(MONTH %in% c(last_4_months)) %>%
        group_by(MONTH, CATEGORY_GROUP) %>%
        summarize(VALUE = sum(VALUE)) %>% ungroup() %>%
        mutate(VALUE = round(as.integer(VALUE)))
      
      # Define colors for each month
      month_colors <- c("firebrick4", "gold3", "gold2", "gold1")
      names(month_colors) <- c(last_4_months)
      
      df_plot <- plot_ly(df, x = ~CATEGORY_GROUP, y = ~round(VALUE), color = ~factor(MONTH),
                         type = "bar", colors = month_colors
                         
      ) %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5, 
            y = 1, 
            xanchor = "center", 
            yanchor = "bottom",  
            traceorder = "normal", 
            font = list(size = 5),  
            orientation = "h" 
          ),
          yaxis = list(title = "Expenses (TOTAL) $"),
          barmode = "group",
          showlegend = TRUE)
      
      df_plot  
      
    }
  )  
  
  output$box_gastos <- renderPlotly(
    { 
      
      last_4_months <- expenses() %>% 
        filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = as.numeric(month_)) %>%
        group_by(MONTH) %>%
        summarise(cnt = n()) %>% ungroup() %>%
        arrange(desc(MONTH)) %>% head(4) %>% pull(MONTH) %>% unique()
      
      df <- expenses_df %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = month_) %>%
        filter(MONTH %in% c(last_4_months)) %>%
        mutate(VALUE = round(as.integer(VALUE))) 
      
      month_colors <- c("firebrick4", "gold3", "gold2", "gold1")
      names(month_colors) <- c(last_4_months)
      
      fig <- plot_ly(df, x = ~CATEGORY_GROUP, y = ~VALUE, color = ~MONTH, type = "box",
                     jitter = 0.3, colors = month_colors,
                     hoverinfo = "text",
                     text = ~paste("Month: ", MONTH, "<br>Value: $", format(VALUE, big.mark = ","),
                                   "<br>DOC: ", `NO. DOC`)
      )
      fig <- fig %>% layout(boxmode = "group") %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal", 
            font = list(size = 5),  
            orientation = "h"  
          ),
          yaxis = list(title = "Expenses $"),
          
          showlegend = TRUE)
      
      fig
      
      
    }
  )  
  
  output$gastos_month_avg <- renderPlotly(
    { 
      
      last_4_months <- expenses() %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = as.numeric(month_)) %>%
        group_by(MONTH) %>%
        summarise(cnt = n()) %>% ungroup() %>%
        arrange(desc(MONTH)) %>% head(4) %>% pull(MONTH) %>% unique()
      
      df <- expenses() %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = month_) %>%
        filter(MONTH %in% c(last_4_months)) %>%
        group_by(MONTH, CATEGORY_GROUP) %>%
        summarize(VALUE = mean(VALUE)) %>% ungroup() %>%
        mutate(VALUE = round(as.integer(VALUE)))
      
      # Define colors for each month
      month_colors <- c("firebrick4", "gold3", "gold2", "gold1")
      names(month_colors) <- c(last_4_months)
      
      df_plot <- plot_ly(df, x = ~CATEGORY_GROUP, y = ~round(VALUE), color = ~factor(MONTH),
                         type = "bar", colors = month_colors
                         
      ) %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal", 
            font = list(size = 5),  
            orientation = "h" 
          ),
          yaxis = list(title = "Expenses (AVG) $"),
          barmode = "group",
          showlegend = TRUE)
      
      df_plot  
      
    }
  )  
  
  
  output$detalle_categoria <- renderPlotly(
    { 
      
      df <- expenses_details() %>%
        filter(CATEGORY == "DEBITO") %>%
        arrange(-desc(FECHA)) %>%
        group_by(FECHA, CATEGORY_NEW) %>%
        summarise_by_time(
          .date_var = FECHA,
          .by       = "month",
          VALUE  = sum(VALUE)) %>% ungroup() 
      
      message(df %>% head(5))
      
      df_plot <- df %>%
        plot_ly(x = ~FECHA, y = ~VALUE, type = "scatter",color = ~CATEGORY_NEW , 
                mode = "lines+markers",
                colors = c('groceries' = 'darkred',
                           'restaurant' = 'darkorange',
                           'entertaining' = 'darkgoldenrod4',
                           'other' = 'deepskyblue3',
                           'pets' = 'deepskyblue2',
                           'debito_elec' = 'deepskyblue1',
                           'health' = 'darkgreen',
                           'fixed' = 'darkred',
                           'interes_iva' = 'darkred',
                           'comissiones' = 'deepskyblue3',
                           'cuotas_cobros' = 'darkgoldenrod4',
                           'finan_int' = 'darkorange',
                           'finan_val' = 'darkred'
                           
                )
                
        ) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = "Amount"),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal",  
            font = list(size = 5),  
            orientation = "h" 
          )
        )
      
      df_plot  
      
    }
  )
  
  # start here
  
  output$detalle_categoria_tot <- renderPlotly({
  

  last_4_months <- expenses_details_total() %>% 
    filter(CATEGORY == "DEBITO") %>%
    mutate(MONTH = as.numeric(month_)) %>%
    group_by(MONTH) %>%
    summarise(cnt = n()) %>% 
    ungroup() %>%
    arrange(desc(MONTH)) %>% 
    head(4) %>% 
    pull(MONTH) %>% 
    unique()
  
  df <- expenses_details_total() %>% 
    filter(CATEGORY == "DEBITO") %>%
    mutate(MONTH = month_) %>%
    filter(MONTH %in% c(last_4_months)) %>%
    group_by(MONTH, CATEGORY_NEW) %>%
    summarize(VALUE = sum(VALUE, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(VALUE = round(as.integer(VALUE)))
  
  # Define colors for each month
  month_colors <- c("firebrick4", "gold3", "gold2", "gold1")
  names(month_colors) <- c(last_4_months)
  
  # Create the plot
  df_plot <- plot_ly(
    df, 
    x = ~CATEGORY_NEW, 
    y = ~VALUE, 
    color = ~factor(MONTH),
    type = "bar", 
    colors = month_colors
  ) %>%
    layout(
      xaxis = list(title = ""),
      margin = list(l = 10, r = 10, b = 10, t = 10),
      legend = list(
        x = 0.5,
        y = 1,
        xanchor = "center",
        yanchor = "bottom",
        traceorder = "normal",
        font = list(size = 5),
        orientation = "h"
      ),
      yaxis = list(title = "Expenses (TOTAL) $"),
      barmode = "group",
      showlegend = TRUE
    )
  
  df_plot
})
  
  output$detalle_categoria_dist <- renderPlotly(
    { 
      
      last_4_months <- expenses_details() %>% 
        filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = as.numeric(month_)) %>%
        group_by(MONTH) %>%
        summarise(cnt = n()) %>% ungroup() %>%
        arrange(desc(MONTH)) %>% head(4) %>% pull(MONTH) %>% unique()
      
      df <- expenses_df %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = month_) %>%
        filter(MONTH %in% c(last_4_months)) %>%
        mutate(VALUE = round(as.integer(VALUE))) 
      
      month_colors <- c("firebrick4", "gold3", "gold2", "gold1")
      names(month_colors) <- c(last_4_months)
      
      fig <- plot_ly(df, x = ~CATEGORY_NEW, y = ~VALUE, color = ~MONTH, type = "box",
                     jitter = 0.3, colors = month_colors,
                     hoverinfo = "text",
                     text = ~paste("Month: ", MONTH, "<br>Value: $", format(VALUE, big.mark = ","),
                                   "<br>DOC: ", `NO. DOC`)
      )
      fig <- fig %>% layout(boxmode = "group") %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5, 
            y = 1,  
            xanchor = "center", 
            yanchor = "bottom",  
            traceorder = "normal",  
            font = list(size = 5),  
            orientation = "h"  
          ),
          yaxis = list(title = "Expenses $"),
          
          showlegend = TRUE)
      
      fig
      
      
    }
  )  
  
  output$detalle_categoria_avg <- renderPlotly(
    { 
      
      last_4_months <- expenses() %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = as.numeric(month_)) %>%
        group_by(MONTH) %>%
        summarise(cnt = n()) %>% ungroup() %>%
        arrange(desc(MONTH)) %>% head(4) %>% pull(MONTH) %>% unique()
      
      df <- expenses() %>% filter(CATEGORY == "DEBITO") %>%
        mutate(MONTH = month_) %>%
        filter(MONTH %in% c(last_4_months)) %>%
        group_by(MONTH, CATEGORY_NEW) %>%
        summarize(VALUE = mean(VALUE)) %>% ungroup() %>%
        mutate(VALUE = round(as.integer(VALUE)))
      
      # Define colors for each month
      month_colors <- c("firebrick4", "gold3", "gold2", "gold1")
      names(month_colors) <- c(last_4_months)
      
      df_plot <- plot_ly(df, x = ~CATEGORY_NEW, y = ~round(VALUE), color = ~factor(MONTH),
                         type = "bar", colors = month_colors
                         
      ) %>%
        layout(
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom", 
            traceorder = "normal", 
            font = list(size = 5), 
            orientation = "h"  
          ),
          yaxis = list(title = "Expenses (AVG) $"),
          barmode = "group",
          showlegend = TRUE)
      
      df_plot  
      
    }
  )  
  
  
  output$expense_tracker_plot <- renderPlotly(
    { 
      
      df <- expense_tracker()  
      
      plot_df <- ggplotly(df) 
      plot_df
      
    }
  )
  
  
  output$tendencia_balance_tracker_c1 <- renderPlotly(
    { 
      
      balance_q_d <- expenses_period_c1


      df_plot <- balance_q_d %>%
        plot_ly(x = ~Period, y = ~BALANCE_LAST, type = "scatter",
                color = ~CURRENCY  , mode = "lines+markers",
                colors = c('local' = 'darkgreen', 'dollar' = 'darkblue')) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = "Balance ($)", rangemode = "tozero"),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5, 
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal",  
            font = list(size = 5), 
            orientation = "h" 
          )
          
        )
      
      
      df_plot
      
      
      
    }
  )
  
  output$tendencia_balance_tracker_c2 <- renderPlotly(
    { 
      
      balance_q_d <- expenses_period_c2

      
      df_plot <- balance_q_d %>%
        plot_ly(x = ~Period, y = ~BALANCE_LAST, type = "scatter",
                color = ~CURRENCY  , mode = "lines+markers",
                colors = c('local' = 'darkgreen', 'dollar' = 'darkblue')) %>%
        layout(
          showlegend = TRUE,
          yaxis = list(title = "Balance ($)", rangemode = "tozero"),
          xaxis = list(title = ""),
          margin = list(l = 10, r = 10, b = 10, t = 10),
          legend = list(
            x = 0.5,  
            y = 1,  
            xanchor = "center",  
            yanchor = "bottom",  
            traceorder = "normal",  
            font = list(size = 5),  
            orientation = "h"  
          )
          
        )
      
      
      df_plot
      
      
      
    }
  )
  
  ######## TABLE ARTIFACTS ##########
  
  
  
  output$table_filtered <- DT::renderDataTable({
    
    DT::datatable(filtered_data() %>% select(c("FECHA", "CARD", "CURRENCY", "NO. DOC", "COMERCIO", "VALUE",
                                               "TIPO DE MOVMIENTO",
                                               "CATEGORY", "CATEGORY_NEW", "CATEGORY_GROUP",
                                               "Period", "year", "month", "day", "week")), height = "700px", class = 'cell-border stripe',
                  style = 'bootstrap', rownames = FALSE, options = list(dom = 'tp',
                                                                        lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                                                                        
                                                                        language = list(
                                                                          zeroRecords = "No Data. Try again"),
                                                                        columnDefs = list(
                                                                          list(className = "dt-center", targets = "_all")))
                  
                  
                  
    ) %>% formatStyle( 0, target= 'row', fontWeight ='bold', lineHeight='10%')
  })
  
  
  output$box10 <- renderUI({
    fluidRow(
      actionButton("apply_filters_button", "Apply Filters", icon = icon("filter")),
      actionButton("reset_filters_button", "Reset Filters", icon = icon("undo")),
      uiOutput("box10_table")
    )
  })
  

  output$box10_table <- renderUI({
    div(
      style = "position: relative; ",
      withSpinner(DT::dataTableOutput("table_filtered", height = 300),
                  type = 4,
                  color = "#d33724",
                  size = 0.7
      )
    )
  })
  
  
}




