# Renderizar selectores tasas
render_selectores_tasas <- function(output, actividad, empleo, paro) {
  
  output$ccaa1_selector_tasas_ui <- renderUI({
    selectInput("ccaa1_selector_tasas", "Selecciona la región 1 a mostrar:",
                choices = unique(actividad$COMARCA),
                selected = "Madrid, Comunidad de")
  })
  
  output$ccaa2_selector_tasas_ui <- renderUI({
    selectInput("ccaa2_selector_tasas", "Selecciona la región 2 a mostrar:",
                choices = unique(actividad$COMARCA),
                selected = "España")
  })
  
  output$periodo_min_tasas_ui <- renderUI({
    selectInput("periodo_min_tasas", "Selecciona el período mínimo:",
                choices = unique(actividad$Periodo),
                selected = "2002T1")
  })
  
  output$periodo_max_tasas_ui <- renderUI({
    selectInput("periodo_max_tasas", "Selecciona el período máximo:",
                choices = unique(actividad$Periodo),
                selected = "2024T1")
  })
  
  output$sexo_tasas_ui <- renderUI({
    selectInput("sexo_tasas", "Selecciona el sexo:",
                choices = unique(actividad$Sexo),
                selected = "Ambos sexos")
  })
  
  output$edad_tasas_ui <- renderUI({
    selectInput("edad_tasas", "Selecciona el grupo de edad:",
                choices = unique(actividad$Edad),
                selected = "Total")
  })
}

# Render ciclo slider
render_ciclo_slider_hogares <- function(output, ciclo_ano_t) {
  output$ciclo_slider_hogares_ui <- renderUI({
    min_ciclo <- min(ciclo_ano_t$CICLO)
    max_ciclo <- max(ciclo_ano_t$CICLO)
    
    sliderInput("ciclo_slider_hogares_ui", "Selecciona el rango de trimestres a mostrar:",
                min = min_ciclo,
                max = max_ciclo,
                value = c(min_ciclo, max_ciclo),
                step = 1,
                ticks = FALSE,
                animate = FALSE
    )
  })
}

render_ciclo_slider_horas <- function(output, ciclo_ano_t) {
  output$ciclo_slider_horas_ui <- renderUI({
    min_ciclo <- min(ciclo_ano_t$CICLO)
    max_ciclo <- max(ciclo_ano_t$CICLO)
    
    sliderInput("ciclo_slider_horas_ui", "Selecciona el rango de trimestres a mostrar:",
                min = min_ciclo,
                max = max_ciclo,
                value = c(min_ciclo, max_ciclo),
                step = 1,
                ticks = FALSE,
                animate = FALSE
    )
  })
}

# Función para renderizar el primer selector de CCAA
render_ccaa_selector_hogares <- function(output, colores_hogares) {
  output$ccaa_selector_hogares_ui <- renderUI({
    selectInput("ccaa_selector_hogares_ui", "Selecciona la región a mostrar:",
                choices = unique(colores_hogares$COMARCA),
                selected = "Madrid, Comunidad de")
  })
}

render_ccaa_selector_horas <- function(output, colores_hogares) {
  output$ccaa_selector_horas_ui <- renderUI({
    selectInput("ccaa_selector_horas_ui", "Selecciona la región a mostrar:",
                choices = unique(colores_hogares$COMARCA),
                selected = "Madrid, Comunidad de")
  })
}

# Funciones para renderizar los segundos selectores de CCAA
render_ccaa_selector2_tv <- function(output, colores_hogares) {
  output$ccaa_selector2_tv_ui <- renderUI({
    selectInput("ccaa_selector2_tv", "Selecciona la región 2 a mostrar:",
                choices = unique(colores_hogares$COMARCA),
                selected = "Resto de España")
  })
}

render_ccaa_selector2_todos_miembros <- function(output, colores_hogares) {
  output$ccaa_selector2_todos_miembros_ui <- renderUI({
    selectInput("ccaa_selector2_todos_miembros", "Selecciona la región 2 a mostrar:",
                choices = unique(colores_hogares$COMARCA),
                selected = "Resto de España")
  })
}

render_ccaa_selector2_unipersonales<- function(output, colores_hogares) {
  output$ccaa_selector2_unipersonales_ui <- renderUI({
    selectInput("ccaa_selector2_unipersonales", "Selecciona la región 2 a mostrar:",
                choices = unique(colores_hogares$COMARCA),
                selected = "Resto de España")
  })
}

render_ccaa_selector2_horas<- function(output, colores_hogares) {
  output$ccaa_selector2_horas_ui <- renderUI({
    selectInput("ccaa_selector2_horas", "Selecciona la región 2 a mostrar:",
                choices = unique(colores_hogares$COMARCA),
                selected = "Resto de España")
  })
}

# Función para comparar tasas generales
crear_plot_tasas_epa <- function(input, actividad, empleo, paro, colores_ccaa) {
  selected_ccaa1 <- input$ccaa1_selector_tasas
  selected_ccaa2 <- input$ccaa2_selector_tasas
  selected_per_min <- input$periodo_min_tasas
  selected_per_max <- input$periodo_max_tasas
  selected_sexo <- input$sexo_tasas
  selected_edad <- input$edad_tasas
  
  # Convertir períodos seleccionados a formato numérico
  selected_per_min_num <- convertir_periodo_a_numero(selected_per_min)
  selected_per_max_num <- convertir_periodo_a_numero(selected_per_max)
  
  # Filtrar los datos según el período numérico
  actividad_filtrado <- actividad() %>%
    filter(
      aaaatt >= selected_per_min_num &
        aaaatt <= selected_per_max_num &
        (COMARCA == selected_ccaa1 | COMARCA == selected_ccaa2) &
        Sexo == selected_sexo &
        Edad == selected_edad
    )
  
  empleo_filtrado <- empleo %>%
    filter(
      aaaatt >= selected_per_min_num &
        aaaatt <= selected_per_max_num &
        (COMARCA == selected_ccaa1 | COMARCA == selected_ccaa2) &
        Sexo == selected_sexo &
        Edad == selected_edad
    )
  
  paro_filtrado <- paro %>%
    filter(
      aaaatt >= selected_per_min_num &
        aaaatt <= selected_per_max_num &
        (COMARCA == selected_ccaa1 | COMARCA == selected_ccaa2) &
        Sexo == selected_sexo &
        Edad == selected_edad
    )
  
  colores <- setNames(colores_ccaa$COLOR, colores_ccaa$COMARCA)
  
  if (input$grafico_tasa == "actividad") {
    plot <- crear_grafico_tasas(actividad_filtrado, "Tasa de actividad", "Tasas de actividad",
                                "Tasa de actividad", colores)
    
  } else if (input$grafico_tasa == "empleo") {
    plot <- crear_grafico_tasas(empleo_filtrado, "Tasa de empleo", "Tasas de empleo",
                                "Tasa de empleo", colores)
  } else if (input$grafico_tasa == "paro") {
    plot <- crear_grafico_tasas(paro_filtrado, "Tasa de paro", "Tasas de paro",
                                "Tasa de paro", colores)
  }
  
  return(plot)
}

# Función para crear el gráfico de conteo y tv
crear_plot_hogares_n_y_tv <- function(input, hogares, colores_ccaa) {
  # Filtrar dataframe
  selected_ciclo_min <- as.numeric(input$ciclo_slider_hogares_ui[1])
  selected_ciclo_max <- as.numeric(input$ciclo_slider_hogares_ui[2])
  selected_ccaa1 <- input$ccaa_selector_hogares_ui
  
  # Filtrar datos según las dos regiones seleccionadas
  hogares_filtrado <- subset(hogares, CICLO >= selected_ciclo_min & CICLO <= selected_ciclo_max &
                               (COMARCA == selected_ccaa1))
  
  colores <- setNames(colores_ccaa$COLOR, colores_ccaa$COMARCA)
  
  if (input$grafico_hogares_n_y_tv == "tot") {
    plot <- crear_grafico_hogares_n_y_tv(hogares_filtrado, "Hogares", "Tasa_var_hogares", 
                                         "Número de hogares y tasa de variación anualizada", colores)
    
  } else if (input$grafico_hogares_n_y_tv == "uni") {
    plot <- crear_grafico_hogares_n_y_tv(hogares_filtrado, "Hogares_uni", "Tasa_var_hogares_uni", 
                                         "Hogares con todos sus miembros inactivos", colores)
  }
  
  return(plot)
}

# Función para crear el gráfico de comparación de proporciones total de hogares
crear_plot_hogares_todos_miembros <- function(input, hogares, colores_ccaa) {
  # Filtrar dataframe
  selected_ciclo_min <- as.numeric(input$ciclo_slider_hogares_ui[1])
  selected_ciclo_max <- as.numeric(input$ciclo_slider_hogares_ui[2])
  selected_ccaa1 <- input$ccaa_selector_hogares_ui
  selected_ccaa2 <- input$ccaa_selector2_todos_miembros
  
  # Filtrar datos según las dos regiones seleccionadas
  hogares_filtrado <- subset(hogares, CICLO >= selected_ciclo_min & CICLO <= selected_ciclo_max &
                               (COMARCA == selected_ccaa1 | COMARCA == selected_ccaa2))
  
  colores <- setNames(colores_ccaa$COLOR, colores_ccaa$COMARCA)
  
  if (input$grafico_todos_miembros == "ocupados") {
    plot <- crear_grafico_hogares(hogares_filtrado, "P_Act_ocu", 
                                  "Hogares con todos sus miembros activos ocupados",
                                  "Proporción sobre el total de hogares", "Proporción en %", colores)
  } else if (input$grafico_todos_miembros == "parados") {
    plot <- crear_grafico_hogares(hogares_filtrado, "P_Act_par", 
                                  "Hogares con todos sus miembros activos parados",
                                  "Proporción sobre el total de hogares", "Proporción en %", colores)
  } else if (input$grafico_todos_miembros == "inactivos") {
    plot <- crear_grafico_hogares(hogares_filtrado, "P_Inac", 
                                  "Hogares con todos sus miembros inactivos",
                                  "Proporción sobre el total de hogares", "Proporción en %", colores)
  }
  
  return(plot)
}

crear_plot_hogares_unipersonales <- function(input, hogares, colores_ccaa) {
  # Filtrar dataframe
  selected_ciclo_min <- as.numeric(input$ciclo_slider_hogares_ui[1])
  selected_ciclo_max <- as.numeric(input$ciclo_slider_hogares_ui[2])
  selected_ccaa1 <- input$ccaa_selector_hogares_ui
  selected_ccaa2 <- input$ccaa_selector2_unipersonales
  
  # Filtrar datos según las dos regiones seleccionadas
  hogares_filtrado <- subset(hogares, CICLO >= selected_ciclo_min & CICLO <= selected_ciclo_max &
                               (COMARCA == selected_ccaa1 | COMARCA == selected_ccaa2))
  
  colores <- setNames(colores_ccaa$COLOR, colores_ccaa$COMARCA)
  
  if (input$grafico_hogares_unipersonales == "ocupado") {
    plot <- crear_grafico_hogares(hogares_filtrado, "P_Uni_ocu", 
                                  "Hogares unipersonales con su persona de referencia ocupada",
                                  "Proporción sobre el total de hogares unipersonales", "Proporción en %", colores)
  } else if (input$grafico_hogares_unipersonales == "parado") {
    plot <- crear_grafico_hogares(hogares_filtrado, "P_Uni_par", 
                                  "Hogares unipersonales con su persona de referencia parada",
                                  "Proporción sobre el total de hogares unipersonales", "Proporción en %", colores)
  } else if (input$grafico_hogares_unipersonales == "inactivo") {
    plot <- crear_grafico_hogares(hogares_filtrado, "P_Uni_ina", 
                                  "Hogares unipersonales con su persona de referencia inactiva",
                                  "Proporción sobre el total de hogares unipersonales", "Proporción en %", colores)
  }
  
  return(plot)
}

crear_grafico_horas <- function(input, horas, colores_ccaa) {
  # Filtrar dataframe
  selected_ciclo_min <- as.numeric(input$ciclo_slider_horas_ui[1])
  selected_ciclo_max <- as.numeric(input$ciclo_slider_horas_ui[2])
  selected_ccaa1 <- input$ccaa_selector_horas_ui
  
  # Filtrar datos según las dos regiones seleccionadas
  horas_filtrado <- subset(horas, CICLO >= selected_ciclo_min & CICLO <= selected_ciclo_max &
                               (COMARCA == selected_ccaa1))
  
  colores <- setNames(colores_ccaa$COLOR, colores_ccaa$COMARCA)
  
  if (input$grafico_horas == "tot") {
    plot <- crear_grafico_horas_totales(horas_filtrado, "HORAS_TOT", "MM_4T_HORAS_TOT",
                                        "Horas trabajadas", colores)
  } else if (input$grafico_horas == "medsemana") {
    plot <- crear_grafico_horas_media(horas_filtrado, "HORAS_MEDIA", "Horas semanales trabajadas", colores)
    
  } else if (input$grafico_horas == "tv") {
    plot <- crear_grafico_horas_tv(horas_filtrado, "TV_ANUAL_HORAS_MEDIA", "TV_ANUAL_HORAS_TOT", 
                                   "Tasas de variación con respecto al mismo trimestre del año anterior", colores)
  }
  
  return(plot)
}

crear_grafico_horas_comparaciones <- function(input, horas, colores_ccaa) {
  # Filtrar dataframe
  selected_ciclo_min <- as.numeric(input$ciclo_slider_horas_ui[1])
  selected_ciclo_max <- as.numeric(input$ciclo_slider_horas_ui[2])
  selected_ccaa1 <- input$ccaa_selector_horas_ui
  selected_ccaa2 <- input$ccaa_selector2_horas
  
  # Filtrar datos según las dos regiones seleccionadas
  horas_filtrado <- subset(horas, CICLO >= selected_ciclo_min & CICLO <= selected_ciclo_max &
                             (COMARCA == selected_ccaa1 | COMARCA == selected_ccaa2))
  
  colores <- setNames(colores_ccaa$COLOR, colores_ccaa$COMARCA)
  
  if (input$grafico_horas_comparaciones == "tvhor") {
    plot <- crear_grafico_comp(horas_filtrado, "TV_ANUAL_HORAS_TOT", 
                               "Tasas de variación anual del total de horas trabajadas", colores)
  } 
    else if (input$grafico_horas_comparaciones == "medsemanac") {
    plot <- crear_grafico_comp(horas_filtrado, "TV_ANUAL_HORAS_MEDIA", 
                               "Tasas de variación anual de la media de horas semanales trabajdas", colores)
    
  } else if (input$grafico_horas_comparaciones == "tvmmc") {
    plot <- crear_grafico_comp(horas_filtrado, "TV_MM_4T_HORAS_TOT", 
                               "Tasas de variación anual de la media móvil con período 4 del total de horas trabjadas", colores)
  }
  
  return(plot)
}