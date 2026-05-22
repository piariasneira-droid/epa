crear_grafico_hogares_n_y_tv <- function(dataframe, var1, var2, tit, colores) {
  # Crear el gráfico con plotly
  plot <- plot_ly(
    data = dataframe,
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
                    "<br>Nº de hogares:", sprintf("%.1f", get(var1))),
      hoverinfo = 'text'
    ) %>%
    
    # Gráfico de líneas para la tasa de variación anual
    add_trace(
      y = ~get(var2),
      type = 'scatter',
      mode = 'lines+markers',
      name = "Tasa de variación anual",
      color = ~as.factor(COMARCA),
      colors = colores,
      text = ~paste("Año-Trimestre:", `ANO-T`,
                    "<br>Comarca:", COMARCA,
                    "<br>TV anual:", sprintf("%.1f", get(var2) * 100), "%"),
      hoverinfo = 'text',
      yaxis = 'y2'
    ) %>%
    
    layout(
      title = list(text = tit,
                   x = 0.0, y = 0.95, font = list(size = 14)),
      xaxis = list(
        title = 'Trimestre',
        tickvals = dataframe$CICLO,
        ticktext = dataframe$`ANO-T`
      ),
      yaxis = list(
        title = 'Total hogares (en miles)',
        side = 'left',
        overlaying = 'y2',
        tickformat = ','
      ),
      yaxis2 = list(
        title = 'Tasa de variación anual (en %)',
        side = 'right',
        tickformat = ',.0%',
        position = 0.98,
        showline = TRUE,
        anchor = 'free'
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

crear_grafico_hogares <- function(dataframe, variable, tit, sub, tity, colores) {
  # Crear el gráfico con plotly
  plot <- plot_ly(
    data = dataframe,
    x = ~CICLO,
    y = ~get(variable),
    type = 'scatter',
    mode = 'lines+markers',
    color = ~as.factor(COMARCA),
    colors = colores,
    text = ~paste("Año-Trimestre:", `ANO-T`,
                  "<br>Comarca:", COMARCA,
                  "<br>Proporción:", sprintf("%.1f", get(variable) * 100), "%"),
    hoverinfo = 'text'
  ) %>%
    layout(
      title = list(text = paste0("<b>", tit, "</b><br><sup>", sub, "</sup>"),
                   x = 0.0, y = 0.95, font = list(size = 14)),
      xaxis = list(
        title = 'Trimestre',
        tickvals = dataframe$CICLO,
        ticktext = dataframe$`ANO-T`
      ),
      yaxis = list(
        title = tity,
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
  
  return(plot)  # Devolver tanto el gráfico como los datos
}