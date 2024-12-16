ui <- fluidPage(
  
  style = "background-color: #ECF0F5", 
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: darkblue}")),
  
  tags$style("
    .nav-tabs-custom .nav-tabs li.active a { color: #000000; 
      border-color: transparent; border-top-color: #686F84; }
    .nav-tabs-custom .nav-tabs li a { color: #d1351be6; font-size: 12px; background-color: #ffffff; color: #000000; border-bottom: 1px solid #BBC7D6; border-right: 1px solid #BBC7D6; }
    .nav-tabs-custom .nav-tabs li.active a { font-size: 12px; border-bottom: 1px solid #BBC7D6; border-right: 1px solid #BBC7D6; }
    th { background-color: #000000; color: white; font-size: 10px; }
    th, td { font-size: 10px; }
  "),
  
  # Header
  navbarPage(
    theme = shinytheme("united"), collapsible = TRUE, id = "nav",
    title = div(
      div(
        id = "img-id",
        img(
          src = "santa.png",
          height = "50%", width = "50%", 
          style = "position: absolute; width: 47px; height: 47px; left: 96.2%; margin: -13px 0 0 -5px;"
        )
      ),
      "SANTA ANA ANALYTICS"
    ),
    
    tabPanel(
      "Dashboard", icon = icon("dashboard"), 
      sidebarLayout(
        sidebarPanel(
          tags$h5("PANEL DE CONTROL"),
          uiOutput("tipos"),
          uiOutput("nombres"),
          uiOutput("categorias"),
          uiOutput("muestras"),
          uiOutput("activos"),
          width = 2
        ),
        mainPanel(
          fluidRow(div(id = "row1", column(width = 12, uiOutput("box15")))),
          fluidRow(div(id = "row2", column(width = 6, uiOutput("box22")), column(width = 6, uiOutput("box23")))),
          fluidRow(div(id = "row3", column(width = 6, uiOutput("box11")), column(width = 6, uiOutput("box20")))),
          width = 10
        )
      )
    ),
    
    tabPanel(
      "Analisis por activo", icon = icon("bar-chart-o"), 
      sidebarLayout(
        sidebarPanel(
          tags$h5("PANEL DE CONTROL"),
          uiOutput("utilizacion"),
          uiOutput("correlacion"),
          uiOutput("abc"),
          uiOutput("actividad"),
          uiOutput("correlacion_k"),
          uiOutput("activos_detail"),
          width = 2
        ),
        mainPanel(
          fluidRow(div(id = "row4", column(width = 5, uiOutput("box25")))),
          br(), br(), br(), br(), br(), br(),
          p(
            strong("TIPO A(Acum 0-80%)"),
            span("Grupo AX: ", style = "color:blue"), "Gasto Alto y estables / ", 
            span("Grupo AY: ", style = "color:blue"), "Gasto Alto, fluctuacion media / ", 
            span("Grupo AZ: ", style = "color:blue"), "Gasto Alto, fluctuacion alta"
          ),
          p(
            strong("TIPO B(Acum 80-95%)"),
            span("Grupo BX: ", style = "color:orange"), "Gasto Medio y estables / ", 
            span("Grupo BY: ", style = "color:orange"), "Gasto Medio, fluctuacion media / ", 
            span("Grupo BZ: ", style = "color:orange"), "Gasto Medio, fluctuacion alta"
          ),
          p(
            strong("TIPO C(Acum >95%)"),
            span("Grupo CX: ", style = "color:red"), "Gasto Bajo y estables / ", 
            span("Grupo CY: ", style = "color:red"), "Gasto Bajo, fluctuacion media / ", 
            span("Grupo CZ: ", style = "color:red"), "Gasto Bajo, fluctuacion alta"
          ),
          fluidRow(div(id = "row5", column(width = 8, uiOutput("box26")))),
          fluidRow(div(id = "row6", column(width = 8, uiOutput("box28")))),
          fluidRow(div(id = "row7", column(width = 8, uiOutput("box27")))),
          width = 10
        )
      )
    ),
    
    ##### MODEL DESCRIPTION
    navbarMenu(
      "Info", icon = icon("info-circle"),
      tabPanel(
        "About", value = "about",
        mainPanel(
          width = 8,
          h4("Acerca de este sitio", style = "color:black;"),
          p("Esta aplicacion tiene como fin el mostrar al departamento administrativo y de operaciones del ingenio Santa Ana, el detalle a nivel granular de la distribucion de combustible de cada uno de sus centros y moviles de despacho. Por otro lado, tambien se muestra el detalle de actividades sobre los activos distribuidos."),
          p("Esta aplicacion fue desarrollada en R y se encuentra hosteando a partir de un servidor Shiny."),
          p("Si tiene alguna sugerencia o aporte, favor escribir a jaime_9_82@hotmail.com"),
          p("Desarrollado por Jaime Paz, Data Scientist."),
          p("Agosto 2022.")
        ),
        br()
      )
    )
  )
)
