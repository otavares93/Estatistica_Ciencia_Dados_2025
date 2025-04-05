library(tidyverse)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(maps)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Aula 7"),
    
    dashboardSidebar(
    #...
      ),
    
    dashboardBody(
    fluidPage(
      navbarPage("Aula 7 - Dashboardo Inicial",
                 
                 ######################################
                 ## Primeira Aba (Gráfico de Barras) ##
                 ######################################
                 
                 tabPanel("Gráfico de Barras", 
                          #Introduzindo a primeira página com sugestões de leitura 
                          #mainPanel(plotOutput("grafico_barras"), height = "2000px", width = "1000px")
                          mainPanel(plotOutput("grafico_barras"))
                 ),
                 
                 ######################################
                 ##         Segunda Aba (Mapa)       ##
                 ######################################
                 
                 selectInput(inputId = "pais_selec", 
                             label = "Selecione um país:", 
                             choices = unique(pop.mundial$Country_Dependency), 
                             selected = "Brazil"),
                 
                 tabPanel("Mapa com as informações de População", 
                          #Introduzindo a primeira página com sugestões de leitura 
                          #mainPanel(plotOutput("grafico_mapa"), height = "2000px", width = "1000px")
                          
                          splitLayout(cellWidths = c("50%", "50%"),
                              plotOutput("grafico_mapa"),
                              plotOutput("grafico_mapa_selecionado")
                             )
                )
              )
            )
          )
        )
      )
