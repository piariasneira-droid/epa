# User interface (interfaz de usuario)
ui <- fluidPage(
  # Título de la aplicación
  titlePanel("Análisis EPA por Comunidades Autónomas"),
  
  # Panel de tabulaciones
  tabsetPanel(
    
    #Panel de tasas
    tabPanel("Tasas",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(2, uiOutput("ccaa1_selector_tasas_ui")),
            column(2, uiOutput("ccaa2_selector_tasas_ui")),
            column(2, uiOutput("periodo_min_tasas_ui")),
            column(2, uiOutput("periodo_max_tasas_ui")),
            column(2, uiOutput("sexo_tasas_ui")),
            column(2, uiOutput("edad_tasas_ui"))
          ),
          width = 12
        ),
        
        mainPanel(
          width = 12,
        
          #Subpaneles
          tabsetPanel(
            tabPanel ("Principales tasas mercado laborales (Comparación entre dos CCAA)", 
              fluidRow(
                column(6, 
                  selectInput("grafico_tasa", "Selecciona la tasa a mostrar:", 
                    choices = list("Tasa de actividad" = "actividad", 
                                  "Tasa de empleo" = "empleo",
                                  "Tasa de paro" = "paro")
                    )
                )
              ),
              plotlyOutput("plot_tasas_epa"))
          )
        )
      )
    ),
  
    #Panel de hogares
    tabPanel("Hogares (Microdatos)",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6, uiOutput("ccaa_selector_hogares_ui")),
            column(6, uiOutput("ciclo_slider_hogares_ui"))
          ),
          width = 12
        ),
      
        mainPanel(
          width = 12,
          
          # Subpanel Hogares 1
          tabsetPanel(
            tabPanel("Tasas de variación anuales y % hogares unipersonales",
              fluidRow(
                column(6, 
                  selectInput("grafico_tv", "Selecciona la variable a mostrar:", 
                    choices = list("Total hogares" = "hogares", 
                                  "Hogares unipersonales" = "unipersonales",
                                  "Proporción hogares unipersonales" = "puni")
                  )
                ),
                column(6, uiOutput("ccaa_selector2_tv_ui"))),
              plotlyOutput("plot_tv")
            ),
            
            #Subpanel hogares 2    
            tabPanel("Conteo y TV anual",
              fluidRow(
                column(12, 
                  selectInput("grafico_hogares_n_y_tv", "Selecciona la variable a mostrar:",
                    choices = list("Total Hogares" = "tot", 
                                  "Hogares unipersonales" = "uni"))
                ),
              ),
              plotlyOutput("plot_hogares_n_y_tv")
            ),
            
            # Subpanel hogares 3         
            tabPanel("Proporciones todos miembros",
              fluidRow(
                column(6, 
                  selectInput("grafico_todos_miembros", "Selecciona la variable a mostrar:",
                    choices = list("Ocupados" = "ocupados", 
                                  "Parados" = "parados", 
                                  "Inactivos" = "inactivos"))
                ),
                column(6, uiOutput("ccaa_selector2_todos_miembros_ui"))),
              plotlyOutput("plot_hogares_todos_miembros")
            ),
            
            # Subpanel hogares 4
            tabPanel("Proporciones hogares unipersonales",
              fluidRow(
                column(6, 
                  selectInput("grafico_hogares_unipersonales", "Selecciona la variable a mostrar:",
                    choices = list("Persona referencia ocupada" = "ocupado", 
                                  "Persona referencia parada" = "parado", 
                                  "Persona referencia inactiva" = "inactivo"))
                  ),
                column(6, uiOutput("ccaa_selector2_unipersonales_ui"))),
              plotlyOutput("plot_hogares_unipersonales")
            )
          )
        )
      )    
    ),
    
    #Panel de ho
    tabPanel("Horas (Microdatos)",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
              column(6, uiOutput("ccaa_selector_horas_ui")),
              column(6, uiOutput("ciclo_slider_horas_ui"))
          ),
        width = 12
        ),
               
        mainPanel(
          width = 12,
          
          # Subpanel Horas 1
          tabsetPanel(
            tabPanel("Horas trabajadas",
              fluidRow(
                column(6, 
                  selectInput("grafico_horas", "Selecciona la variable a mostrar:", 
                    choices = list("Total horas" = "tot", 
                                  "Media semanal" = "medsemana",
                                  "Tasas de variación" = "tv")
                  )
                ),
                ),
              plotlyOutput("plot_horas")
            ),
          
          # Subpanel Horas 2
            tabPanel("Comparaciones",
              fluidRow(
                column(6, 
                    selectInput("grafico_horas_comparaciones", "Selecciona la variable a mostrar:", 
                      choices = list("Tasas variaciones horas" = "tvhor", 
                                    "Tasas variaciones media" = "medsemanac",
                                    "Tasas de variación (MM4) horas" = "tvmmc")
                    )
                ),
                column(6, uiOutput("ccaa_selector2_horas_ui"))
                ),
              plotlyOutput("plot_horas_comparaciones")
            )
          )
        )
      )
    )    
  )
)