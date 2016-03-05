# get data
source("pracuj_analysis.r")

shinyServer(function(input, output) {
  # establish plot
  output$plot1 <- renderPlot({
    ggplot(offersCityCompany[offersCityCompany$city == input$city, ],
           aes(x=factor(n), y=company)) +
      # visualize data
      geom_point() +
      #geom_dotplot(binaxis="y", method="histodot", stackdir="center", dotsize=0.1) +
      # remove unnecessary elements
      theme(panel.grid.minor.x=element_blank()) +
      # add title
      ggtitle("Ile firm złożyło konkretną liczbę ofert?")
  })
  
  # define ranges
  ranges <- reactiveValues(x=NULL, y=NULL)
  
  # establish plot
  output$plot2 <- renderPlot({
    ggplot(polish[polish$city == input$city, ],
           aes(x=city, y=employer)) +
      # visualize data
      geom_dotplot(binaxis="y", stackdir="center", dotsize=0.5) +
      # remove unnecessary elements
      theme(panel.grid.minor.x=element_blank()) +
      # add title
      ggtitle("Ile firm złożyło konkretną liczbę ofert?")
  })
  
  # establish plot
  output$plot3 <- renderPlot({
    ggplot(polish[polish$city == input$city, ],
           aes(x=city, y=employer)) +
      # visualize data
      geom_dotplot(binaxis="y", stackdir="center", dotsize=0.1) +
      # remove unnecessary elements
      theme(panel.grid.minor.x=element_blank()) +
      # add title
      ggtitle("Ile firm złożyło konkretną liczbę ofert?") +
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