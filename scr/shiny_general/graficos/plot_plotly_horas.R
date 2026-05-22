crear_grafico_horas_totales <- function(df, var1, var2, tit, colores) {
  # Crear el gráfico con plotly
  plot <- plot_ly(
    data = df,
    x = ~CICLO
  ) %>%
  
  # Histograma con el conteo
  add_trace(
    y = ~get(var1),
    type = 'bar',
    name = "Nº de hogares",
    color = ~as.factor(COMARCA),
    colors = colores,
    opacity = 0.6,
    text = ~paste("Año-Trimestre:", `ANO-T`,
                  "<br>Comarca:", COMARCA,
                  "<br>Nº de horas:", sprintf("%.1f", get(var1))),
    hoverinfo = 'text'
  ) %>%
    
    add_trace(
      y = ~get(var2),
      type = 'scatter',
      mode = 'lines+markers',
      name = "Nº de hogares (MM4)",
      color = ~as.factor(COMARCA),
      colors = colores,
      text = ~paste("Año-Trimestre:", `ANO-T`,
                    "<br>Comarca:", COMARCA,
                    "<br>Nº de horas:", sprintf("%.1f", get(var1))),
      hoverinfo = 'text'
    ) %>%
    
    layout(
      title = list(text = tit,
                   x = 0.0, y = 0.95, font = list(size = 14)),
      xaxis = list(
        title = 'Trimestre',
        tickvals = df$CICLO,
        ticktext = df$`ANO-T`
      ),
      yaxis = list(
        title = 'Total horas trabajadas (en miles)',
        side = 'left',
        tickformat = ','
      ),

      legend = list(
        title = list(text = 'Región'),
        orientation = 'h',
        x = 0.5,
        xanchor = 'center',
        y = -0.3
      ),
      margin = list(t = 50, b = 50),
      annotations = list(
        list(
          text = "Fuente: Elaboración propia. Microdatos EPA (INE)",
          x = 0,
          y = -0.3,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = 'left',
          yanchor = 'auto',
          font = list(size = 8)
        )
      )
    )
    
  return(plot)
}

crear_grafico_horas_media <- function(df, var1, tit, colores) {
  # Crear el gráfico con plotly
  plot <- plot_ly(
    data = df,
    x = ~CICLO
  ) %>%
    
    # Histograma con el conteo
    add_trace(
      y = ~get(var1),
      type = 'bar',
      color = ~as.factor(COMARCA),
      colors = colores,
      text = ~paste("Año-Trimestre:", `ANO-T`,
                    "<br>Comarca:", COMARCA,
                    "<br>Nº de horas:", sprintf("%.1f", get(var1))),
      hoverinfo = 'text'
    ) %>%
    
    layout(
      title = list(text = tit,
                   x = 0.0, y = 0.95, font = list(size = 14)),
      xaxis = list(
        title = 'Trimestre',
        tickvals = df$CICLO,
        ticktext = df$`ANO-T`
      ),
      yaxis = list(
        title = 'Media semanal horas trabajadas',
        side = 'left',
        tickformat = ','
      ),
      
      legend = list(
        title = list(text = 'Región'),
        orientation = 'h',
        x = 0.5,
        xanchor = 'center',
        y = -0.3
      ),
      margin = list(t = 50, b = 50),
      annotations = list(
        list(
          text = "Fuente: Elaboración propia. Microdatos EPA (INE)",
          x = 0,
          y = -0.3,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = 'left',
          yanchor = 'auto',
          font = list(size = 8)
        )
      )
    )
  
  return(plot)
}

crear_grafico_comp <- function(df, variable, tit, colores) {
  # Crear el gráfico con plotly
  plot <- plot_ly(
    data = df,
    x = ~CICLO,
    y = ~get(variable),
    type = 'bar',
    color = ~as.factor(COMARCA),
    colors = colores,
    text = ~paste("Año-Trimestre:", `ANO-T`,
                  "<br>Comarca:", COMARCA,
                  "<br>Tasa de variación:", sprintf("%.1f", get(variable) * 100), "%"),
    hoverinfo = 'text',
    texttemplate = ~sprintf("%.1f%%", get(variable) * 100),
    textposition = 'outside'
    
  ) %>%
    
    layout(
      title = list(text = tit,
                   x = 0.0, y = 0.95, font = list(size = 14)),
      xaxis = list(
        title = 'Trimestre',
        tickvals = df$CICLO,
        ticktext = df$`ANO-T`
      ),
      yaxis = list(
        title = 'Tasa de variación anual (en %)',
        side = 'left',
        tickformat = ',.0%'
      ),
      
      legend = list(
        title = list(text = 'Región'),
        orientation = 'h',
        x = 0.5,
        xanchor = 'center',
        y = -0.3
      ),
      margin = list(t = 50, b = 50),
      annotations = list(
        list(
          text = "Fuente: Elaboración propia. Microdatos EPA (INE)",
          x = 0,
          y = -0.3,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = 'left',
          yanchor = 'auto',
          font = list(size = 8)
        )
      )
    )
  
  return(plot)
}