shinyServer(function(input, output) {
  # establish plot
  output$plot1 <- renderPlot({
    ggplot(offersCityCompany[offersCityCompany$city == input$city, ],
           aes(x=factor(n), y=reorder(company, n))) +
      # visualize data
      geom_point() +
      # name axes
      scale_x_discrete("Liczba ofert") +
      scale_y_discrete("Firma") +
      # remove unnecessary elements +
      theme(panel.grid.minor.x=element_blank())
  })
  
  # define ranges
  ranges <- reactiveValues(x=NULL, y=NULL)
  
  # establish plot
  output$plot2 <- renderPlot({
    ggplot(selectCity[selectCity$city == input$city, ],
           aes(x=city, y=reorder(company, n))) +
      # visualize data
      geom_dotplot(binaxis="y", stackdir="center", dotsize=0.5) +
      # name axes
      scale_x_discrete("Miasto") +
      scale_y_discrete("Firma") +
      # remove unnecessary elements
      theme(panel.grid.minor.x=element_blank())
  })
  
  # establish plot
  output$plot3 <- renderPlot({
    ggplot(selectCity[selectCity$city == input$city, ],
           aes(x=city, y=reorder(company, n))) +
      # visualize data
      geom_dotplot(binaxis="y", stackdir="center", dotsize=0.1) +
      # name axes
      scale_x_discrete("Miasto") +
      scale_y_discrete("Firma") +
      # remove unnecessary elements
      theme(panel.grid.minor.x=element_blank()) +
      # add coordinate system
      coord_cartesian(xlim=ranges$x, ylim=ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
})