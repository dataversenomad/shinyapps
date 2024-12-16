ui <- fluidPage(
  
  style = "background-color: #ECF0F5",
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: darkblue}")),
  
  tags$style("
    .nav-tabs-custom .nav-tabs li.active a { color: #000000; 
    border-color: transparent; border-top-color: #686F84; }
    .nav-tabs-custom .nav-tabs li a { color: #d1351be6; font-size: 12px; background-color: #ffffff; color:#000000; border-bottom: 1px solid #BBC7D6; border-right:1px solid #BBC7D6; }
    .nav-tabs-custom .nav-tabs li.active a { font-size: 12px; border-bottom: 1px solid #BBC7D6; border-right:1px solid #BBC7D6; }
    th {
      background-color: #000000;
      color: white; font-size: 10px;
    }
    th, td {
      font-size: 10px;
    }
  "),
  
  navbarPage(
    theme = shinytheme("united"),
    collapsible = TRUE,
    id = "nav",
    title = div("INCOME - EXPENSES DASHBOARD"),
    
    tabPanel(
      "Dashboard",
      icon = icon("dashboard"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("card"),
          uiOutput("currency"),
          uiOutput("year"),
          uiOutput("week"),
          uiOutput("month"),
          width = 2
        ),
        mainPanel(
          fluidRow(
            div(
              id = "row1",
              column(width = 6, uiOutput("box1")),
              column(width = 6, uiOutput("box2"))
            )
          ),
          fluidRow(
            div(
              id = "row2",
              column(width = 6, uiOutput("box3")),
              column(width = 6, uiOutput("box4"))
            )
          ),
          fluidRow(
            div(
              id = "row3",
              column(width = 6, uiOutput("box5")),
              column(width = 6, uiOutput("box6"))
            )
          ),
          width = 10
        )
      )
    ),
    
    tabPanel(
      "Tracker",
      icon = icon("credit-card"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("Period"),
          width = 2
        ),
        mainPanel(
          fluidRow(
            div(
              id = "row1",
              column(width = 12, uiOutput("box7"))
            )
          ),
          fluidRow(
            div(
              id = "row2",
              column(width = 6, uiOutput("box8")),
              column(width = 6, uiOutput("box9"))
            )
          ),
          width = 12
        )
      )
    ),
    
    tabPanel(
      "Details",
      icon = icon("bar-chart"),
      sidebarLayout(
        sidebarPanel(
          tags$h5("Filters"),
          pickerInput(inputId = "CARD_tbl", "CARD", choices = c("All", c("5524-XXXX-XXXX-1119", "4487-XXXX-XXXX-5978")), selected = c("5524-XXXX-XXXX-1119"), multiple = FALSE, label = "Card"),
          pickerInput(inputId = "CURRENCY_tbl", "CURRENCY", choices = c("All", unique(expenses_df$CURRENCY)), selected = "local", multiple = FALSE, label = "Currency"),
          pickerInput(inputId = "Period_tbl", "Period", choices = c("All", unique(expenses_df$Period)), label = "Period", selected = unique(visa_pay$Period)[length(unique(visa_pay$Period))], multiple = TRUE),
          pickerInput(inputId = "year_tbl", "Year", choices = c("All", unique(expenses_df$year)), selected = unique(expenses_df$year), multiple = TRUE, label = "Year"),
          pickerInput(inputId = "month_tbl", "Month", choices = c("All", unique(expenses_df$month)), selected = unique(expenses_df$month), multiple = TRUE, label = "Month"),
          pickerInput(inputId = "day_tbl", "Day", choices = c("All", sort(unique(expenses_df$day))), selected = sort(unique(expenses_df$day)), multiple = TRUE, label = "Day"),
          pickerInput(inputId = "week_tbl", "Week", choices = c("All", sort(unique(expenses_df$week))), selected = sort(unique(expenses_df$week)), multiple = TRUE, label = "Week"),
          pickerInput(inputId = "CATEGORY_tbl", "Category", choices = c("All", expenses_df %>% mutate(CATEGORY = as.character(CATEGORY)) %>% pull(CATEGORY) %>% unique()), selected = expenses_df %>% mutate(CATEGORY = as.character(CATEGORY)) %>% pull(CATEGORY) %>% unique(), multiple = TRUE, label = "Category"),
          pickerInput(inputId = "CATEGORY_NEW_tbl", "Category New", choices = c("All", expenses_df %>% mutate(CATEGORY_NEW = as.character(CATEGORY_NEW)) %>% pull(CATEGORY_NEW) %>% unique()), selected = expenses_df %>% mutate(CATEGORY_NEW = as.character(CATEGORY_NEW)) %>% pull(CATEGORY_NEW) %>% unique(), multiple = TRUE, label = "Category New"),
          pickerInput(inputId = "CATEGORY_GROUP_tbl", "Category Group", choices = c("All", unique(expenses_df$CATEGORY_GROUP)), selected = unique(expenses_df$CATEGORY_GROUP), multiple = TRUE, label = "Category Group"),
          pickerInput(inputId = "TIPO_DE_MOVMIENTO_tbl", "Tipo Movimiento", choices = c("All", expenses_df %>% mutate(`TIPO DE MOVMIENTO` = as.character(`TIPO DE MOVMIENTO`)) %>% pull(`TIPO DE MOVMIENTO`) %>% unique()), selected = expenses_df %>% mutate(`TIPO DE MOVMIENTO` = as.character(`TIPO DE MOVMIENTO`)) %>% pull(`TIPO DE MOVMIENTO`) %>% unique(), multiple = TRUE, label = "Tipo Movimiento"),
          pickerInput(inputId = "NO_DOC_tbl", "No. Doc", choices = c("All", sort(unique(expenses_df$`NO. DOC`))), selected = sort(unique(expenses_df$`NO. DOC`)), multiple = TRUE, label = "No. Doc"),
          pickerInput(inputId = "COMERCIO_tbl", "Comercio", choices = c("All", expenses_df %>% mutate(COMERCIO = as.character(COMERCIO)) %>% pull(COMERCIO) %>% unique() %>% sort()), selected = "All", multiple = TRUE, label = "Comercio"),
          pickerInput(inputId = "VALUE_tbl", "Valor", choices = c("All", c("low", "medium", "high", "ultra high")), selected = c("low", "medium", "high", "ultra high"), multiple = TRUE, label = "Valor"),
          width = 2
        ),
        mainPanel(
          fluidRow(
            div(
              id = "row1",
              column(width = 5, uiOutput("box10"))
            )
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          width = 10
        )
      )
    )
  )
)
