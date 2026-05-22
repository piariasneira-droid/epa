custom_theme <- theme(
  panel.background = element_blank(),
  plot.background = element_blank(),
  plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5),
  plot.subtitle = element_text(size = 14, color = "black", hjust = 0.5),
  axis.title = element_text(size = 14, color = "black"),
  axis.text = element_text(size = 14, color = "black"),
  axis.text.x = element_text(margin = margin(t = 10)), # Etiquetas del eje X debajo
  axis.text.y = element_text(margin = margin(r = 10)),
  axis.ticks = element_line(color = "black"), # Marcas de graduación en negro
  axis.line = element_line(color = "black"), # Línea del eje en negro
  panel.grid.major = element_line(color = "grey", size = 0.25, linetype = "dashed"), # Rejilla principal en gris
  panel.grid.minor = element_line(color = "grey", size = 0.25, linetype = "dashed"), # Rejilla secundaria en gris
  legend.box.background = element_blank(),
  legend.background = element_blank(),
  legend.position = "bottom",
  legend.justification = "right",
  legend.text = element_text(size = 12, family = "Calibri"),
  legend.title = element_text(size = 14, family = "Calibri"),
  plot.caption.position = "plot",
  plot.tag.position = c(0, -0.1),
  plot.tag = element_text(size = 12, hjust=0),
  plot.caption = element_text(size = 10, hjust = 0),
  strip.background = element_rect(fill = NA)
)

custom_theme_plotly <- list(
  layout = list(
    plot_bgcolor = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    title = list(
      font = list(size = 16, family = "Calibri", color = "black"),
      x = 0.5
    ),
    subtitle = list(
      font = list(size = 14, family = "Calibri", color = "black"),
      x = 0.5
    ),
    xaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "grey",
      gridwidth = 0.25,
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(font = list(size = 14, family = "Calibri", color = "black")),
      tickfont = list(size = 14, family = "Calibri", color = "black"),
      ticks = "outside",
      tickcolor = "black",
      linecolor = "black",
      gridcolor = "grey",
      gridwidth = 0.25,
      zeroline = FALSE
    ),
    legend = list(
      bgcolor = "rgba(0, 0, 0, 0)",
      font = list(size = 12, family = "Calibri"),
      orientation = "h",
      x = 1,
      xanchor = "right",
      y = 1.02,
      yanchor = "bottom,"
    ),
    margin = list(t = 0, b = 0, l = 0, r = 0)
  )
)

# Paleta de colores
colorpal1 <- "#526DB0"
colorpal2 <- "#F5C201"
colorpal3 <- "#A0B8C8"
colorpal4 <- "#7A7A7A"
colorpal5 <- "grey75"
colorpal6 <- "#DC5924"
colorpal7 <- "#7B90C3"