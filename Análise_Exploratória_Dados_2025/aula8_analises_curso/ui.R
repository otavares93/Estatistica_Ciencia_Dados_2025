library(dplyr)
library(tidyr)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(maps)
#library(gganimate)


shinyUI(
  fluidPage(
    includeCSS("www/styles.css"),
    navbarPage("Análise exploratória de dados",
      
       ###########################################
       ######   Introdução (Primeira Aba).  ######
       ###########################################
       
       tabPanel("Referências do Curso", 
                #Introduzindo a primeira página com sugestões de leitura 
                mainPanel(includeMarkdown("introducao.Rmd"), height = "2000px", width = "1000px")
                ),
       
       ###########################################
       ######   Salários (Segunda Aba).     ######
       ###########################################
       
       tabPanel("Análise de Salários",
                
                
                p("Gráfico de linhas sob seleção de variáveis"),
                #Painel principa com plot de salarios por linha
                mainPanel(plotOutput("salarios_linha")),
                
                #Layout em flow para melhor justaposicao das opcoes
                flowLayout(
                  
                  #Selecao das variaveis na base salarios
                  varSelectInput(inputId = "variaveis_salarios_x", label = "Variáveis Salários (x):", data = salarios, multiple = FALSE),
                  
                  varSelectInput(inputId = "variaveis_salarios_y", label = "Variáveis Salários (y):", data = salarios, multiple = FALSE),
                  
                  #Selecao de cores
                  selectInput(inputId = 'cor', label = 'Escolha uma cor:',
                              choices = c("lightblue", "lightgreen", "red"), selected = "red"),
                  ),
                
                #Definindo o range do eixo x
                numericRangeInput(inputId = "x_lim", label = "Insira valor mínimo e máximo para eixo x:",
                                  value = c(min(salarios$n), max(salarios$n))),
                
                #Definindo o range do eixo y
                numericRangeInput(inputId = "y_lim", label = "Insira valor mínimo e máximo para eixo y:",
                                  value = c(min(salarios$n), max(salarios$n)))
                ),
        
       ###########################################
       ######   Crimes (Terceira Aba).      ######
       ###########################################
       
       tabPanel("Análise de crimes para o Estado do Rio de Janeiro",
                
                sidebarLayout(
                  
                  # Painel principal com os plots de crimes
                  mainPanel(
                    div(class="span6",plotOutput("crimes")),
                    div(class="span6",plotOutput("crimes_evo")),
                    ),
                  
                  sidebarPanel(
                    #Inserindo texto com titulo do gráfico histograma
                    textInput(inputId = "titulo_crimes", label = "Título do histograma:", value = "insira seu titulo"),
                    verbatimTextOutput(outputId = "titulo_crimes"),
                  
                    #Criando os botoes para trocar o tipo de gráfico
                    actionButton(inputId = "botao_cdf", label = "Trocar de histograma para acumulada"),
                    actionButton(inputId = "reiniciar", label = "Voltar para histograma"),
                  
                    #Slider para trocar qual batalhao mostrar como gráfico de linhas
                    sliderInput(inputId = "batalhoes_crimes",label = "Selecione os batalhões:", min = min(crimes.aisp$AISP), max = max(crimes.aisp$AISP), value = 19)
                    )
                )
            ),
       
       ###########################################
       ######   População (Quarta Aba).     ######
       ###########################################
       
       tabPanel("Mapa mundial com destaque para os 10 países mais populosos",
              mainPanel(
                
                plotOutput("mapa_mundo_pop"), height = "1000px", width = "1000px"
              
                )
            ),
       
       ########################################################
       ######   Imputando dados vazios (Quarta Aba).     ######
       ########################################################

       tabPanel("Imputando dados vazios",
               sidebarLayout(
                 
                 mainPanel(
                   div(class="span6",plotOutput("nfilhos_semimp")),
                   div(class="span6",plotOutput("nfilhos_comimp")),
                 ),
          
                 sidebarPanel(
                #Selecao de modelos de imputação
                    selectInput(
                      inputId = 'modelo', label = 'Escolha um modelo para realizar input:',
                       choices = c("mean","midastouch", "pmm", "rf"), selected = "pmm"
                    ),
                 
                 #Definindo o numero de particoes
                   selectInput(
                     inputId = 'particoes', label = 'Número de partições:',
                     choices = 1:20, selected = 5
                   )
                 )
               )
            )
        )
    )
)
