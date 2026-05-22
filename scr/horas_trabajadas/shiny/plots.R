grafico_comparativo <- function(filtros, df, col1, col2) {
  # Calcular ciclo min
  ciclo_min <- min(df$CICLO, na.rm = TRUE)
  
  # Agrupar y sumar las horas por 'CICLO' y 'LABEL'
  df_suma <- df %>%
    group_by(ANO_T, CICLO, LABEL) %>%
    summarise(TOT = sum(.data[[filtros$var]], na.rm = TRUE), .groups = "drop") %>%
    arrange(CICLO)
  
  # Filtrar datos y calcular límites para region1
  df_region1 <- df_suma %>% filter(LABEL == filtros$lab1)
  valor_inicial_1 <- df_region1 %>% filter(CICLO == ciclo_min) %>% pull(TOT)
  min1 <- min(df_region1$TOT)
  max1 <- max(df_region1$TOT)
  
  # Si las regiones son distintas, calcular para region2
  if (filtros$region1 != filtros$region2) {
    df_region2 <- df_suma %>% filter(LABEL == filtros$lab2)
    valor_inicial_2 <- df_region2 %>% filter(CICLO == ciclo_min) %>% pull(TOT)
    scale_factor <- valor_inicial_2 / valor_inicial_1
    miny <- min(min1, min(df_region2$TOT) / scale_factor)
    maxy <- max(max1, max(df_region2$TOT) / scale_factor)
    range_y2 <- c(miny * scale_factor, maxy * scale_factor)
  } else {
    miny <- min1
    maxy <- max1
    scale_factor <- 1  # No escala si las regiones son iguales
  }
  
  # Crear gráfico con Plotly
  plot <- plot_ly() %>%
    add_trace(
      x = factor(df_region1$ANO_T, levels = unique(df_region1$ANO_T[order(df_region1$CICLO)])),
      y = df_region1$TOT,
      type = 'scatter',
      mode = 'lines',
      name = filtros$region1,
      line = list(color = col1, width = 2),
      yaxis = 'y',
      text = paste0(filtros$region1, "<br>Variable: ", filtros$var, "<br>Trimestre: ", df_region1$ANO_T, "<br>Valor: ", 
                    label_number(scale = 0.001, accuracy = 0.1, big.mark = ".", decimal.mark = ",")(df_region1$TOT)),  
      hoverinfo = 'text'
    )
  
  # Si las regiones son diferentes, agregar la traza para region2
  if (filtros$region1 != filtros$region2) {
    plot <- plot %>%
      add_trace(
        x = factor(df_region2$ANO_T, levels = unique(df_region2$ANO_T[order(df_region2$CICLO)])),
        y = df_region2$TOT,
        type = 'scatter',
        mode = 'lines',
        name = filtros$region2,
        line = list(color = col2, width = 2),
        yaxis = "y2",
        text = paste0(filtros$region2, "<br>Variable: ", filtros$var, "<br>Trimestre: ", df_region2$ANO_T, "<br>Valor: ", 
                      label_number(scale = 0.001, accuracy = 0.1, big.mark = ".", decimal.mark = ",")(df_region2$TOT)),   
        hoverinfo = 'text'
      )
  }
  
  plot <- plot %>%
    layout(
      paper_bgcolor = 'rgba(0, 0, 0, 0)',
      plot_bgcolor = 'rgba(0, 0, 0, 0)', 
      title = list(text = filtros$var, font = list(size = 16, family = "Calibri", color = "#000000"), x = 0.5),
      yaxis = list(
        title = filtros$region1, titlefont = list(size = 12, color = col1),
        tickfont = list(size = 10, color = col1), 
        range = c(miny, maxy)
      ),
      yaxis2 = if (filtros$region1 != filtros$region2) {
        list(
          title = filtros$region2, titlefont = list(size = 12, color = col2),
          tickfont = list(size = 10, color = col2),
          overlaying = "y", side = "right", 
          range = c(miny * scale_factor, maxy * scale_factor)
        )
      } else {
        NULL
      },
      xaxis = list(
        title = "Trimestre",
        tickangle = 270, titlefont = list(size = 12), tickfont = list(size = 10)
      ),
      annotations = list(
        list(
          text = "Fuente: elaboración propia a partir de los microdatos de la EPA (INE)",
          x = 0, y = -0.3, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 10, family = "Calibri", color = "#000000")
        )
      ),
      legend = list(
        title = "Región",
        x = 1, 
        y = 1.15, 
        orientation = 'h', 
        xanchor = 'right'
      ),
      margin = list(l = 0, r = 50, t = 50, b = 100)
    )
  
  return(plot)
}

grafico_porcentajes <- function(filtros, df, col1, col2, var_filtro, valor_filtro, tit, tity) {
  # Calcular ciclo min
  ciclo_min <- min(df$CICLO, na.rm = TRUE)
  
  # Agrupar y sumar las horas por 'CICLO' y 'LABEL'
  df_mujeres <- df %>%
    filter(!!sym(var_filtro) == valor_filtro) %>%
    group_by(ANO_T, CICLO, LABEL) %>%
    summarise(TOT = sum(PER, na.rm = TRUE), .groups = "drop") %>%
    arrange(CICLO)
  
  # Calcular los límites para el eje Y
  miny <- min(df_mujeres$TOT, na.rm = TRUE)
  maxy <- max(df_mujeres$TOT, na.rm = TRUE)
  
  # Crear gráfico con Plotly
  plot <- plot_ly(data = df_mujeres) %>%
    add_trace(
      x = ~factor(ANO_T, levels = unique(df_mujeres$ANO_T[order(df_mujeres$CICLO)])),
      y = ~TOT,
      type = 'scatter',
      mode = 'lines',
      color = ~LABEL, 
      colors = setNames(c(col1, col2), c(filtros$lab1, filtros$lab2)),
      line = list(width = 2),
      text = ~paste0(LABEL, "<br>Variable: Porcentaje horas mujeres", 
                     "<br>Trimestre: ", ANO_T,
                     "<br>Región: ", LABEL,
                     "<br>Valor: ", label_percent(scale = 1, accuracy = 0.01, big.mark = ".", decimal.mark = ",")(TOT)),  
      hoverinfo = 'text'
    ) %>%
    layout(
      paper_bgcolor = 'rgba(0, 0, 0, 0)',
      plot_bgcolor = 'rgba(0, 0, 0, 0)', 
      title = list(text = tit, font = list(size = 16, family = "Calibri", color = "#000000"), x = 0.05),
      yaxis = list(
        title = tity, 
        titlefont = list(size = 12),
        tickfont = list(size = 10),
        range = c(miny, maxy)
      ),
      xaxis = list(
        title = "Trimestre",
        tickangle = 270, titlefont = list(size = 12), tickfont = list(size = 10)
      ),
      annotations = list(
        list(
          text = "Fuente: elaboración propia a partir de los microdatos de la EPA (INE)",
          x = 0, y = -0.3, xref = "paper", yref = "paper",
          showarrow = FALSE, font = list(size = 10, family = "Calibri", color = "#000000")
        )
      ),
      legend = list(
        title = "Región",
        x = 1, 
        y = 1.15, 
        orientation = 'h', 
        xanchor = 'right'
      ),
      margin = list(l = 20, r = 50, t = 50, b = 100)
    )
  
  return(plot)
}

grafico_horas_actividad <- function(lab, anot, df, col1, col2, sub, ajuste, minx, maxx) {
  df <- df %>%
    filter(LABEL == lab,
           ANO_T == anot) %>%
    group_by(CICLO, ACT_LAB) %>%
    mutate(
      HOR = sum(HORAS_MED, na.rm = TRUE),
      PER = sum(PER_HORAS, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(CICLO) %>%
    mutate(RANK = rank(desc(HOR)))
  
  plot <- ggplot(df, aes(y = factor(RANK), x = HORAS_MED, 
                         fill = factor(SJ_LAB, levels = c("MP", "MC", "HP", "HC")), 
                         alpha = factor(SJ_LAB, levels = c("MP", "MC", "HP", "HC")))) + 
    
    geom_col(position = "stack", width = 0.8) +
    geom_text(aes(x = 0, label = paste(ACT_LAB, " ")), 
              vjust = 0.2, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = HOR + ajuste, label = sprintf("%.1f M", HOR / 1e6)), 
              vjust = 0.2, hjust = 0, color = "black", show.legend = FALSE) +
    
    labs(
      x = "Total de horas efectivas semanales trabajadas",
      y = "Actividad",
      title = paste0("Horas trabajadas por actividad en ", sub, " en el ", anot),,
      fill = "Sexo y Jornada",
      alpha= "Sexo y Jornada",
      caption = "Elaboración propia a partir de los microdatos de la EPA (INE)"
    ) +
    
    scale_fill_manual(
      name = "Sexo y Jornada",
      values = c("HC" = col1, "HP" = col1, "MC" = col2, "MP" = col2), 
      labels = c(
        "HC" = "Hombres\ncompleta",
        "HP" = "Hombres\nparcial",
        "MC" = "Mujeres\ncompleta",
        "MP" = "Mujeres\nparcial"),
      breaks = c("HC", "HP", "MC", "MP")
    ) +
    
    scale_alpha_manual(
      name = "Sexo y Jornada",
      values = c("HC" = 1, "HP" = 0.5, "MC" = 1, "MP" = 0.5), 
      labels = c(
        "HC" = "Hombres\ncompleta",
        "HP" = "Hombres\nparcial",
        "MC" = "Mujeres\ncompleta",
        "MP" = "Mujeres\nparcial"),
      breaks = c("HC", "HP", "MC", "MP")) + 
    
    geom_vline(xintercept = 0, color = "black") +
    
    scale_x_continuous(limits = c(minx, maxx), 
                       labels = label_number(scale = 1e-6, suffix = " M"))
  
  ggplot_build(plot) -> gg_build
  x_breaks <- gg_build$layout$panel_params[[1]]$x$breaks
  x_breaks <- x_breaks[x_breaks > 0]
  
  # Añadir geom_vline con los breaks automáticos
  plot <- plot + geom_vline(xintercept = x_breaks, color = "grey", linetype = "dashed") +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(face = 'bold'),
      plot.subtitle = element_text(face = "bold"),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.box.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.justification = "right",
      plot.caption.position = "plot",
      plot.caption = element_text(size = 10, hjust = 0)
    ) 
  
  return(plot)
}


grafico_horas_actividad_bis <- function(lab, anot, df, col1, col2, sub) {
  df <- df %>%
    filter(LABEL == lab, ANO_T == anot) %>%
    group_by(CICLO, ACT_LAB) %>%
    mutate(
      HOR = sum(HORAS_MED, na.rm = TRUE),
      PER = sum(PER_HORAS, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(CICLO) %>%
    mutate(RANK = rank(desc(HOR)))
  
  # Preparar texto para etiquetas
  df <- df %>%
    mutate(
      Etiqueta_HORAS = sprintf("%.1f M (%.2f%%)", HORAS_MED / 1e6, PER_HORAS) %>%
        sub("\\.", ",", .)
    )
  
  # Crear gráfico interactivo
  plot <- plot_ly() 
  
  # Agregar barras para cada combinación de SEXO y JORNADA
  niveles_sj <- c("MP", "MC", "HP", "HC")
  etiquetas_leyenda <- c(
    "MP" = "Mujeres parcial", 
    "MC" = "Mujeres completa", 
    "HP" = "Hombres parcial", 
    "HC" = "Hombres completa"
  )
  colores <- c("HC" = col2, "HP" = col2, "MC" = col1, "MP" = col1)
  alphas <- c("HC" = 1, "HP" = 0.5, "MC" = 1, "MP" = 0.5)
  
  for (sj in niveles_sj) {
    plot <- plot %>%
      add_bars(
        data = df %>% filter(SJ_LAB == sj),
        x = ~HORAS_MED,
        y = ~ACT_LAB,
        name = etiquetas_leyenda[sj],
        marker = list(color = colores[sj], opacity = alphas[sj]),
        hovertemplate = ~paste("Actividad:", ACT_LAB, "<br>Sexo y jornada:", sj, "<br>Horas:", Etiqueta_HORAS),
        text = NULL
      )
  }
  
  
  # Líneas verticales para los ejes
  plot <- plot %>%
    layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickformat = "~s",
        zeroline = TRUE
      ),
      yaxis = list(
        title = "",
        categoryorder = "total ascending",
        zeroline = TRUE
      ),
      title = list(
        text = paste0("Horas trabajadas por actividad en ", sub, " en el ", anot),
        font = list(size = 16, face = "bold")
      ),
      legend = list(
        orientation = "h",
        x = 1,
        y = -0.2,
        xanchor = "right",
        yanchor = "top",
        title = list(text = "Sexo y Jornada")
      ),
      annotations = list(
        list(
          text = "Elaboración propia a partir de los microdatos de la EPA (INE)",
          x = 0,
          y = -0.3,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 10)
        )
      )
    )
  
  return(plot)
}

crear_grafico_rangos_ggplot <- function(df, var_filtro, ano_filtro) {
  # Filtrar los datos para la variable y año especificados
  datos_filtrados <- df %>%
    filter(var == var_filtro, ano == ano_filtro)
  
  # Definir título y título del eje Y según la variable
  if (var_filtro == "PPS_EU27_2020_HAB") {
    titulo <- paste("Renta per cápita en el año", ano_filtro, "en PPA en euros del 2020")
    titulo_y <- "Renta en euros"
    titulo_leg <- "Renta per cápita"
  } else {
    titulo <- paste("Índice renta per cápita en el año", ano_filtro, "en PPA (100 = Media anual UE)")
    titulo_y <- "Índice (100 = Media UE)"
    titulo_leg <- "Índice"
  }
  
  # Crear el gráfico con ggplot2
  ggplot(datos_filtrados) +
    # Añadir los segmentos entre mínimo y máximo
    geom_segment(aes(x = code, xend = code, 
                     y = minimo, yend = maximo),
                 size = 3, color = colorpal2) +
    # Añadir los puntos para la media
    geom_point(aes(x = code, y = media, color = "Media nacional"),
               size = 3) +
    # Añadir los puntos para la capital
    geom_point(aes(x = code, y = capital, color = "Región capital"),
               size = 3) +
    # Personalización del tema y colores
    scale_color_manual(values = c("Media nacional" = colorpal1,
                                  "Región capital" = colorpal6)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "black", linetype = "dashed"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    ) +
    labs(
      title = titulo,
      y = titulo_y,
      x = "",
      color = titulo_leg
    )
}

custom_theme <- theme(
  # Fondo y bordes
  panel.background = element_blank(),
  plot.background = element_blank(),
  
  # Títulos y texto
  plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
  plot.subtitle = element_text(size = 14, color = "black", hjust = 0.5),
  axis.title = element_text(size = 14, color = "black"),
  axis.text = element_text(size = 14, color = "black"),
  axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 10)),
  axis.text.y = element_text(margin = margin(r = 10)),
  
  # Ejes y líneas de la cuadrícula
  axis.ticks = element_line(color = "black"),
  axis.line = element_line(color = "black"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_line(color = "black", size = 0.25, linetype = "dashed"),
  panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
  
  # Leyenda
  legend.box.background = element_blank(),
  legend.background = element_blank(),
  legend.position = "bottom",
  legend.justification = "right",
  legend.text = element_text(size = 12, family = "Calibri"),
  legend.title = element_text(size = 14, family = "Calibri", face = "bold"),
  
  # Otros elementos
  plot.caption.position = "plot",
  plot.tag.position = c(0, -0.1),
  plot.tag = element_text(size = 12, hjust = 0),
  plot.caption = element_text(size = 10, hjust = 0),
  strip.background = element_rect(fill = NA)
)

crear_grafico_rangos_animado_ggplot <- function(df, var_filtro, anos = NULL) {
  # Si no se especifican años, usar todos los disponibles en el dataframe
  if (is.null(anos)) {
    anos <- unique(df$ano)
  }
  
  # Filtrar los datos
  datos_filtrados <- df %>%
    filter(var == var_filtro, ano %in% anos)
  
  # Definir título y título del eje Y según la variable
  if (var_filtro == "PPS_EU27_2020_HAB") {
    titulo_base <- "Renta per cápita en PPA en euros del 2020"
    titulo_y <- "Renta en euros"
    titulo_leg <- "Renta per cápita"
  } else {
    titulo_base <- "Índice renta per cápita en PPA (100 = Media anual UE)"
    titulo_y <- "Índice (100 = Media UE)"
    titulo_leg <- "Índice"
  }
  
  # Crear el gráfico base con ggplot2 y gganimate
  p <- ggplot(datos_filtrados) +
    # Añadir los segmentos entre mínimo y máximo
    geom_segment(aes(x = code, xend = code, 
                     y = minimo, yend = maximo),
                 size = 3, color = colorpal2) +
    # Añadir los puntos para la media y capital
    geom_point(aes(x = code, y = media, color = "Media nacional"),
               size = 3) +
    geom_point(aes(x = code, y = capital, color = "Región capital"),
               size = 3) +
    # Personalización de colores y tema
    scale_color_manual(values = c("Media nacional" = colorpal1,
                                  "Región capital" = colorpal6)) +
    custom_theme +
    labs(
      title = paste0(titulo_base, "\nAño: {frame_time}"),
      y = titulo_y,
      x = "",
      color = titulo_leg
    ) +
    # Animación
    transition_time(ano) +
    ease_aes('linear')
  
  # Renderizar la animación
  anim <- animate(p, 
                  nframes = length(anos) * 10,
                  duration = length(anos) * 2,
                  width = 800, 
                  height = 600,
                  renderer = gifski_renderer())
  
  return(anim)
}