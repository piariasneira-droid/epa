render_sidebar_selectores <- function(output, actividades, ccaas, ciclos, edades, jornadas, sexos) {
  
  output$sidebar_actividad_selector_ui <- renderUI({
    checkboxGroupInput(inputId = "sidebar_actividad_selector", 
                       label = "Rama de actividad:",
                       choices = unique(actividades$NOMBRE),
                       selected = "Todas las ramas de actividad")
  })
  
  output$sidebar_ccaa_selector1_ui <- renderUI({
    selectInput("sidebar_ccaa_selector1", "Región 1:",
                choices = unique(ccaas$COMARCA),
                selected = "Madrid, Comunidad de")
  })
  
  output$sidebar_ccaa_selector2_ui <- renderUI({
    selectInput("sidebar_ccaa_selector2", "Región 2:",
                choices = unique(ccaas$COMARCA),
                selected = "España")
  })
  
  output$sidebar_edad_selector_ui <- renderUI({
    checkboxGroupInput(inputId = "sidebar_edad_selector", 
                       label = "Edad:",
                       choices = unique(edades$EDAD),
                       selected = "Todas las edades")
  })
  
  output$sidebar_jornada_selector_ui <- renderUI({
    checkboxGroupInput(
      inputId = "sidebar_jornada_selector", 
      label = "Jornada:", 
      choices = unique(jornadas$JORNADA),
      selected = "Ambas jornadas"
    )
  })
  
  output$sidebar_sexo_selector_ui <- renderUI({
    checkboxGroupInput(
      inputId = "sidebar_sexo_selector", 
      label = "Sexo:", 
      choices = unique(sexos$SEXO),
      selected = "Ambos sexos"
    )
  })
}

render_sidebar_slider_ano_trimestre <- function(output) {
  output$sidebar_ano_slider_ui <- renderUI({
    sliderInput("sidebar_ano_slider", "Año:",
                min = 2005,
                max = 2024,
                value = c(2005, 2024),
                step = 1,
                ticks = FALSE,
                animate = FALSE
    )
  })
  
  output$sidebar_trimestre_selector_ui <- renderUI({
    checkboxGroupInput(
      inputId = "sidebar_trimestre_selector", 
      label = "Trimestres:", 
      choices = c(1, 2, 3, 4), 
      selected = c(1, 2, 3, 4)
    )
  })
}

render_sidebar_selector_var <- function(output, variables){
  output$sidebar_var_selector_ui <- renderUI({
    selectInput("sidebar_var_selector", "Variable: ",
                choices = unique(variables$VARIABLE),
                selected = "HORAS_MED"
    )
  })
}