server <- function(input, output, session) {
  
  ####################SIDEBAR#######################
  # Renderizar selectores para el Tab 1
  render_sidebar_slider_ano_trimestre(output)
  render_sidebar_selectores(output, actividades(), ccaas(), ciclos(), edades(), jornadas(), sexos())
  render_sidebar_selector_var(output, variables())
  
  ####################FILTROS SIDE BAR #############
  filtros_sidebar <- reactive({
    # Determinación de regiones en función de las entradas
    region1 <- if (input$sidebar_ccaa_selector2 == "Madrid, Comunidad de") {
      input$sidebar_ccaa_selector2
    } else {
      input$sidebar_ccaa_selector1
    }
    
    region2 <- if (input$sidebar_ccaa_selector2 == "Madrid, Comunidad de") {
      input$sidebar_ccaa_selector1
    } else {
      input$sidebar_ccaa_selector2
    }
    
    # Etiquetas para las regiones seleccionadas
    lab1 <- ccaas()$LABEL[ccaas()$COMARCA == region1]
    lab2 <- if (region1 != region2) {
      ccaas()$LABEL[ccaas()$COMARCA == region2]
    } else {
      NULL
    }
    
    ano_min <- input$sidebar_ano_slider[1]
    ano_max <- input$sidebar_ano_slider[2]
    
    # Construcción de la lista con todos los filtros
    list(
      act = actividades()$COD[actividades()$NOMBRE %in% input$sidebar_actividad_selector],
      ccaa = ccaas()$COD[ccaas()$COMARCA %in% c(input$sidebar_ccaa_selector1, input$sidebar_ccaa_selector2)],
      edad = edades()$COD[edades()$EDAD %in% input$sidebar_edad_selector],
      jornada = jornadas()$COD[jornadas()$JORNADA %in% input$sidebar_jornada_selector],
      sexo = sexos()$COD[sexos()$SEXO %in% input$sidebar_sexo_selector],
      ano_min = ano_min,
      ano_max = ano_max,
      trimestre = input$sidebar_trimestre_selector,
      var = input$sidebar_var_selector,
      region1 = region1,
      region2 = region2,
      lab1 = lab1,
      lab2 = lab2
    )
  })
  
  ####################TAB1##########################  
  # Dataframe reactivo
  df_tab1 <- reactive({
    data() %>%
      filter(
        ACT1 %in% filtros_sidebar()$act,
        CCAA %in% filtros_sidebar()$ccaa,
        EDAD1 %in% filtros_sidebar()$edad,
        PARCO1 %in% filtros_sidebar()$jornada,
        SEXO1 %in% filtros_sidebar()$sexo
      ) %>%
      asignar_ano_trimestre() %>% 
      filter(
        ANO >= filtros_sidebar()$ano_min & ANO <= filtros_sidebar()$ano_max,   
        TRIMESTRE %in% filtros_sidebar()$trimestre              
      ) %>% 
      select("CICLO", "ANO_T", "CCAA", "EDAD1", "SEXO1", "ACT1", "PARCO1", filtros_sidebar()$var) %>% 
      asignar_comarca() %>%           
      asignar_actividad_label() %>%   
      asignar_label() %>%             
      asignar_sexo() %>%              
      asignar_jornada() %>%           
      select("CICLO", "ANO_T", "LABEL", "ACT_LAB", "SEXO", "EDAD1", "JORNADA", filtros_sidebar()$var)
  })
  
  # Renderizar tabla
  output$tabla_resultados <- renderDataTable({
    df_tab1() %>%
      select(-CICLO) %>%
      mutate(!!filtros_sidebar()$var := label_number(scale = 0.001, accuracy = 0.1, big.mark = ".", decimal.mark = ",")(.data[[input$sidebar_var_selector]]))
  })
  
  
  output$tab1_grafico <- renderPlotly({
    grafico_comparativo(filtros_sidebar(), df_tab1(), colorpal1, colorpal2)  
  })
  
  # Descargar tabla
  output$download_tabla <- downloadHandler(
    filename = function() {
      paste("tabla_resultados_sidebar_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_tab1(), file, row.names = FALSE)
    }
  )
  
  ####################TAB2###########################
  #Dataframe reactivo
  df_tab2_horas <- reactive({
    data() %>%
      filter(
        ACT1 %in% filtros_sidebar()$act,
        CCAA %in% filtros_sidebar()$ccaa,
        EDAD1 %in% filtros_sidebar()$edad,
        PARCO1 != 99,
        SEXO1 != 99
      ) %>%
      asignar_ano_trimestre() %>%
      filter(
        ANO >= filtros_sidebar()$ano_min & ANO <= filtros_sidebar()$ano_max,
        TRIMESTRE %in% filtros_sidebar()$trimestre
      ) %>%
      select("CICLO", "ANO_T", "CCAA", "EDAD1", "SEXO1", "ACT1", "PARCO1", "HORAS_MED") %>%
      asignar_comarca() %>%
      asignar_actividad_label() %>%
      asignar_label() %>%
      asignar_sexo() %>%
      asignar_jornada() %>%
      select("CICLO", "ANO_T", "LABEL", "ACT_LAB", "SEXO", "EDAD1", "JORNADA", "HORAS_MED") %>%
      group_by(ANO_T, LABEL, ACT_LAB, EDAD1) %>%
      mutate(TOTAL_HORAS = sum(HORAS_MED),
             PER = HORAS_MED/TOTAL_HORAS*100) 
    
  })
  
  df_tab2_horasb <- reactive({
    df_tab2_horas() %>%
      select(-TOTAL_HORAS, -PER) %>%
      filter(JORNADA == "Parcial") %>%       
      group_by(ANO_T, LABEL, ACT_LAB, EDAD1) %>%
      mutate(TOTAL_HORAS = sum(HORAS_MED),
             PER = HORAS_MED/TOTAL_HORAS*100)
  })
  
  output$tab2_grafico_sexo <- renderPlotly({
    grafico_porcentajes(filtros_sidebar(), df_tab2_horas(), colorpal1, colorpal2, "SEXO", "Mujeres", "Porcentaje horas realizadas por mujeres", "% del total de horas efectivas semanales trabajdas")
  })
  
  output$tab2_grafico_parcialidad <- renderPlotly({
    grafico_porcentajes(filtros_sidebar(), df_tab2_horas(), colorpal1, colorpal2, "JORNADA", "Parcial", "Porcentaje horas realizadas por trabajadores a jornada parcial", 
                        "% del total de horas efectivas semanales trabajdas")
  })
  
  output$tab2_grafico_parmujeres <- renderPlotly({
    grafico_porcentajes(filtros_sidebar(), df_tab2_horasb(), colorpal1, colorpal2, "SEXO", "Mujeres", "Porcentaje horas realizadas por mujeres entre los trabajadores a jornada parcial", 
                        "% del total de horas efectivas semanales trabajdas")
  })
  
  # Renderizar tablas
  output$tabla_resultados2 <- renderDataTable({
    df_tab2_horas()
  })
  
  output$tabla_resultados3 <- renderDataTable({
    df_tab2_horasb()
  })
  
  
  ####################TAB3###########################
  #Dataframe reactivo
  df_tab3_estructura <- reactive({
    data() %>%
      filter(
        ACT1 != 99,
        CCAA %in% filtros_sidebar()$ccaa,
        EDAD1 %in% filtros_sidebar()$edad,
        PARCO1 != 99,
        SEXO1 != 99
      ) %>%
      asignar_ano_trimestre() %>%
      filter(
        ANO >= filtros_sidebar()$ano_min & ANO <= filtros_sidebar()$ano_max,
        TRIMESTRE %in% filtros_sidebar()$trimestre
      ) %>%
      select("CICLO", "ANO_T", "CCAA", "EDAD1", "SEXO1", "ACT1", "PARCO1", "HORAS_MED") %>%
      asignar_comarca() %>%
      asignar_actividad_label() %>%
      asignar_label() %>%
      asignar_sexo() %>%
      asignar_jornada() %>%
      select("CICLO", "ANO_T", "LABEL", "ACT_LAB", "SEXO", "EDAD1", "JORNADA", "HORAS_MED") %>%
      group_by(ANO_T, LABEL, EDAD1) %>%
      mutate(TOTAL_HORAS = sum(HORAS_MED),
             PER_HORAS = HORAS_MED/TOTAL_HORAS*100) %>%
      asignar_sj_bis() %>%
      select(-TOTAL_HORAS)
  })
  
  last_tri <- reactive({
    # Obtener el último ciclo
    ultimo_ciclo <- max(df_tab3_estructura()$CICLO)
    
    # Filtrar ANO_T para el último ciclo
    ano_t_ultimo_ciclo <- max(df_tab3_estructura()$ANO_T[df_tab3_estructura()$CICLO == ultimo_ciclo])
    
    # Extraer los últimos dos caracteres de ANO_T
    substr(ano_t_ultimo_ciclo, nchar(ano_t_ultimo_ciclo) - 2, nchar(ano_t_ultimo_ciclo))
  })
  
  output$tab3_grafico_estructura <- renderPlot({
    grafico_horas_actividad(filtros_sidebar()$lab1, paste(filtros_sidebar()$ano_min, last_tri(), sep = ""), df_tab3_estructura(), "#526DB0", "#F5C201", "la C. de Madrid", 50000, -3000000, 25000000)
  })
  
  output$tab3_grafico_estructura_bis <- renderPlotly({
    grafico_horas_actividad_bis(filtros_sidebar()$lab1, paste(filtros_sidebar()$ano_min, last_tri(), sep = ""), df_tab3_estructura(), "#526DB0", "#F5C201", "la C. de Madrid")
  })
  
  output$tabla_resultados4 <- renderDataTable({
    df_tab3_estructura()%>%
      filter(LABEL == filtros_sidebar()$lab1) %>%
      group_by(CICLO, ACT_LAB) %>%
      mutate(
        HOR = sum(HORAS_MED, na.rm = TRUE),
        PER = sum(PER_HORAS, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      group_by(CICLO) %>%
      mutate(RANK = dense_rank(desc(HOR))) %>%
      ungroup()
    
  })
}