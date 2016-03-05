# set working directory
setwd("../")

# get data
source("pracuj_analysis.r")

shinyUI(fluidPage(
  titlePanel("Oferty pracy związane z analizą danych."),
  
  fluidRow(
    column(width=12,
      helpText("Wybierz miasto:"),
      selectInput("city", "Miasto", sort(selectCity$city)),
      h3("Jaką liczbę ofert złożyła dana firma w wybranym mieście?"),
      plotOutput("plot1")
    )
  ),
  fluidRow(
    helpText("Przybliż dolny wykres zaznaczając obszar na górnym."),
    column(width=12,
           plotOutput("plot2",
                      brush=brushOpts(
                        id="plot2_brush",
                        resetOnNew=T
                      )
           )
    )
  ),
  fluidRow(
    column(width=12,
           plotOutput("plot3")
    )
  )
))