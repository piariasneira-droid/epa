crear_grafico_tasas <- function(dataframe, tit, sub, tity, colores) {
  dataframe <- dataframe %>%
    arrange(aaaatt)  # Ordenar el dataframe por la columna 'aaaatt'
  
  # Crear el gráfico con plotly
  plot <- plot_ly(
    data = dataframe,
    x = ~Periodo,  # Usar 'Periodo' en el eje X
    y = ~Total,    # Usar 'Total' en el eje Y
    type = 'scatter',
    mode = 'lines+markers',
    color = ~as.factor(COMARCA),  # Colorear por 'COMARCA'
    colors = colores,  # Usar los colores proporcionados
    text = ~paste(
      "Periodo:", Periodo,
      "<br>Comarca:", COMARCA,
      "<br>Proporción:", sprintf("%.1f", Total), "%"
    ),
    hoverinfo = 'text',  # Mostrar solo texto en el hover
    texttemplate = ~sprintf("%.1f%%", Total),  # Mostrar el texto en los puntos del gráfico
    textposition = 'outside'  # Posición del texto fuera de los puntos
  ) %>%
    layout(
      title = list(
        text = paste0("<b>", tit, "</b><br><sup>", sub, "</sup>"),
        x = 0.0, y = 0.95, font = list(size = 14)
      ),
      xaxis = list(
        title = 'Periodo',
        tickvals = dataframe$Periodo,
        ticktext = dataframe$Periodo
      ),
      yaxis = list(
        title = tity
      ),
      legend = list(
        title = list(text = 'Comarca'),
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
  
  return(plot)  # Devolver el gráfico
}