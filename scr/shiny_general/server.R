# Server (servidor)

server <- function(input, output, session) {
  #Renderizar UI tasas
  render_selectores_tasas(output, actividad(), empleo(), paro())
  
  #Graficos tasas
  output$plot_tasas_epa <- renderPlotly({
    crear_plot_tasas_epa(input, actividad(), empleo(), paro(), colores_ccaa())
  })
  
  # Renderizar UI hogares
  render_ciclo_slider_hogares(output, ciclo_ano_t())
  render_ccaa_selector_hogares(output, colores_ccaa())
  render_ccaa_selector2_tv(output, colores_ccaa())
  render_ccaa_selector2_todos_miembros(output, colores_ccaa())
  render_ccaa_selector2_unipersonales(output, colores_ccaa())
  
  # Gráficos comparaciones TV
  output$plot_tv <- renderPlotly({
    crear_plot_tv(input, hogares(), colores_ccaa())
  })
  
  # Gráficos total hogares y tv
  output$plot_hogares_n_y_tv <- renderPlotly({
    crear_plot_hogares_n_y_tv(input, hogares(), colores_ccaa())
  })
  
  # Gráfico comparaciones proprociones dos comunidades
  output$plot_hogares_todos_miembros <- renderPlotly({
    crear_plot_hogares_todos_miembros(input, hogares(), colores_ccaa())
  })
  
  # Gráfico comparaciones proprociones unipersonales dos comunidades
  output$plot_hogares_unipersonales <- renderPlotly({
    crear_plot_hogares_unipersonales(input, hogares(), colores_ccaa())
  })
  
  # Renderizar UI horas
  render_ciclo_slider_horas(output, ciclo_ano_t())
  render_ccaa_selector_horas(output, colores_ccaa())
  render_ccaa_selector2_horas(output, colores_ccaa())
  
  # Gráfico horas
  output$plot_horas <- renderPlotly({
    crear_grafico_horas(input, horas(), colores_ccaa())
  })
  
  # Grácico comparaciones horas
  output$plot_horas_comparaciones <- renderPlotly({
    crear_grafico_horas_comparaciones(input, horas(), colores_ccaa())
  })
}