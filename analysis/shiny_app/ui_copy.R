shinyUI(fluidPage(
  titlePanel("Oferty pracy związane z analizą danych."),
  
  fluidRow(
    column(width=12,
      helpText("Wybierz miasto:"),
      selectInput("city", "Miasto", offersCityCompany$city),
      plotOutput("plot1")
    )
  ),
  fluidRow(
    column(width=6,
           plotOutput("plot2",
                      brush=brushOpts(
                        id="plot2_brush",
                        resetOnNew=T
                      )
           )
    ),
    column(width=6,
           plotOutput("plot3")
    )
  )
))