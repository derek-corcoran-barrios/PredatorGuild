#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
##load packages ggplot2,ggthemes,desolve,gridExtra

library(shiny)
library(ggplot2)
library(ggthemes)
library(deSolve)
library(gridExtra)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Population Dynamics of two predators"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Change the parameters for the predator dynamics"),
      p("This is the population dynamics of two predators that share a pathogen,"),
      p("The Large predator (Lp) has sometimes hunts the smaller predator (Sp)"),
      submitButton("Update View", icon("refresh")),
      sliderInput(inputId = "betaL",
                  label = "Large predator intraspecific transmition Rate",
                  min = 0,
                  max = 1,
                  value = 0.21,
                  step = 0.001),
      sliderInput(inputId = "betaS",
                  label = "Small predator intraspecific transmition Rate",
                  min = 0,
                  max = 1,
                  value = 0.21,
                  step = 0.001),
      sliderInput(inputId = "betaSL",
                  label = "Interespecific Transmition Rate from the small predator to the large one",
                  min = 0,
                  max = 1,
                  value = 0.03,
                  step = 0.001),
      sliderInput(inputId = "gamma",
                  label = "gamma",
                  min = 0,
                  max = 1,
                  value = 0.07,
                  step = 0.001),
      sliderInput(inputId = "rhoL",
                  label = "rhoL",
                  min = 0,
                  max = 1,
                  value = 0.7,
                  step = 0.001),
      sliderInput(inputId = "rhoS",
                  label = "rhoS",
                  min = 0,
                  max = 1,
                  value = 0.6,
                  step = 0.001),
      sliderInput(inputId = "kl",
                  label = "kl",
                  min = 0,
                  max = 1000,
                  value = 50),
      sliderInput(inputId = "ks",
                  label = "ks",
                  min = 0,
                  max = 1000,
                  value = 500),
      numericInput(inputId = "a",
                   label = "a",
                   min = 0,
                   max = 1,
                   value = 0.0106287,
                   step = 0.0000001),
      numericInput(inputId = "b",
                   label = "b",
                   min = 0,
                   max = 1,
                   value = 0.00181,
                   step = 0.0000001),
      numericInput(inputId = "igp",
                   label = "Intraguild predation",
                   min = 0,
                   max = 1,
                   value = 0.00317633,
                   step = 0.00000001)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )))


