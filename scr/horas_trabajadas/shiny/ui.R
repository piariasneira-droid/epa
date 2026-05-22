ui <- fluidPage(
  # Título de la aplicación
  titlePanel("Horas efectivas semanales trabajadas"),
  
  # Diseño con el panel lateral fijo
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # Fila con actividad, año, trimestre y edad
      fluidRow(
        column(6, uiOutput("sidebar_ccaa_selector1_ui")),
        column(6, uiOutput("sidebar_ccaa_selector2_ui"))
      ),
      
      fluidRow(
        column(4, uiOutput("sidebar_jornada_selector_ui")),
        column(4, uiOutput("sidebar_sexo_selector_ui")),
        column(4, uiOutput("sidebar_var_selector_ui"))
      ),
      
      fluidRow(
        column(4, uiOutput("sidebar_ano_slider_ui")),
        column(4, uiOutput("sidebar_trimestre_selector_ui")),
        column(4, uiOutput("sidebar_edad_selector_ui"))
      ),
      
      fluidRow(
        column(12, uiOutput("sidebar_actividad_selector_ui"))
      )
    ),
    
    mainPanel(
      width = 8,
      
      # Estructura de pestañas
      tabsetPanel(
        # Primera pestaña (Comparaciones regionales)
        tabPanel(
          "Comparaciones regionales",
          h2("Visualización datos filtrados"),
          plotlyOutput("tab1_grafico"),
          h2("Tabla de datos"),
          dataTableOutput("tabla_resultados"),
          downloadButton("download_tabla", "Descargar Tabla")
        ),
        
        # Segunda pestaña
        tabPanel(
          "Sexo y tipo de jornada",
          h2("Comportamiento por sexo y tipo de jornada"),
          tags$p("Nota: Los selectores de tipo de jornada, sexo y vaiable no están activos.", 
                 style = "color: black; font-weight: bold; margin-bottom: 10px;"),
          plotlyOutput("tab2_grafico_sexo"),
          plotlyOutput("tab2_grafico_parcialidad"),
          plotlyOutput("tab2_grafico_parmujeres"),
          dataTableOutput("tabla_resultados2"),
          dataTableOutput("tabla_resultados3")
        ),
        
        # Tercera pestaña
        tabPanel(
          "Estructura",
          h2("Estrucura ramas de activididad"),
          tags$p("Nota: Los selectores de tipo de jornada, sexo y vaiable no están activos.", 
                 style = "color: black; font-weight: bold; margin-bottom: 10px;"),
          plotOutput("tab3_grafico_estructura"),
          plotlyOutput("tab3_grafico_estructura_bis"),
          dataTableOutput("tabla_resultados4")

        )
      )
    )
  )
)