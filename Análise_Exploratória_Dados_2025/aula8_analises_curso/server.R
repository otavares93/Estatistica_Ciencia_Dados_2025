library(tidyverse)
library(data.table)
library(scales)
library(markdown)
library(shiny)
library(zoo)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(ggplot2)

shinyServer(function(input, output){
  
  
  ###########################################
  ######   Salários (Segunda Aba).     ######
  ###########################################
  
  #Criando um evento reativo que gera um plot quando uma das ações relacionadas 
  #ao gráfico de linhas muda, sendo elas, eixos, cores, e variáveis
  plot_salarios_reativo <- eventReactive(c(input$variaveis_salarios_x, input$variaveis_salarios_y, input$cor, input$x_lim, input$y_lim),{
    
    #Plotando o gráfico com as definições do eixo x, de cores, etc.
    ggplot(data = salarios, aes_string(x = input$variaveis_salarios_x, y = input$variaveis_salarios_y)) +
      geom_point(color = input$cor) + ggplot2::xlim(input$x_lim) + ggplot2::ylim(input$y_lim) + geom_smooth(method = "lm") + theme_classic()
  })
  
  #Atualizando o range do x quando uma variável é trocada
  update_xlim <- eventReactive(c(input$variaveis_salarios_x),{
    if(length(input$variaveis_salarios_x) == 0) return(numericRangeInput(inputId = "x_lim", label = "Insira valor mínimo e máximo para eixo x:", value = c(min(salarios$n), max(salarios$n))))
    updateNumericRangeInput(inputId = "x_lim", value = c(min(salarios[,input$variaveis_salarios_x], na.rm = T), max(salarios[,input$variaveis_salarios_x], na.rm = T))) 
  })
  
  #Atualizando o range do y quando uma variável é trocada
  update_ylim <- eventReactive(c(input$variaveis_salarios_y),{
    if(length(input$variaveis_salarios_y) == 0) return(numericRangeInput(inputId = "y_lim", label = "Insira valor mínimo e máximo para eixo y:", value = c(min(salarios$n), max(salarios$n))))
    updateNumericRangeInput(inputId = "y_lim", value = c(min(salarios[,input$variaveis_salarios_y], na.rm = T), max(salarios[,input$variaveis_salarios_y], na.rm = T))) 
  })
    
  #Renderizando o plot construído iterativamente 
  output$salarios_linha <- renderPlot({
    #Controlando para o caso de não selecionar nenhuma variável, ou de a variável não ser numérica
    #De modo a não introduzir limites ao eixo y, para uma variável que não é numérica
    if (((length(input$variaveis_salarios_x) == 0) | (!is.numeric(unlist(salarios[,input$variaveis_salarios_x][1]))))|((length(input$variaveis_salarios_y) == 0) | (!is.numeric(unlist(salarios[,input$variaveis_salarios_y][1])))))
    {
      if((!is.numeric(unlist(salarios[,input$variaveis_salarios_x][1]))) & (length(input$variaveis_salarios_x) != 0)) return(ggplot(salarios, aes_string(x=input$variaveis_salarios_x, y = input$variaveis_salarios_y)) + geom_point() + geom_point(color = input$cor) + geom_smooth(method = "lm") + theme_classic())
      if((!is.numeric(unlist(salarios[,input$variaveis_salarios_y][1]))) & (length(input$variaveis_salarios_y) != 0)) return(ggplot(salarios, aes_string(x=input$variaveis_salarios_x, y = input$variaveis_salarios_y)) + geom_point() + geom_point(color = input$cor) + geom_smooth(method = "lm") + theme_classic())
      else return(ggplot(salarios, aes(x=n, y = n)) + geom_point() + geom_point(color = input$cor) + geom_smooth(method = "lm"))
    }
    
    #Atualizando o eixo y
    update_ylim()
    #Atualizando o eixo x
    update_xlim()
    #Plotando o gráfico de linhas reativamente
    plot_salarios_reativo()
  })
  
  
  ###########################################
  ######   Crimes (Terceira Aba).      ######
  ###########################################
  
  
  ### Renderizando um gráfico de evoluacao temporal de crimes em graficos de linha ###
  output$crimes_evo <- renderPlot({
    ggplot(crimes, aes(x = mes.ano, y = roubo_transeunte, group = AISP, color = as.factor(AISP))) + geom_line() + theme_classic()
  })
  
  ### Criando um gráfico interativo selecionando um batalhao por vez para ver a evolucao temporal ###
  plot_crimes_reativo <- eventReactive(input$batalhoes_crimes,{
    ggplot(data = crimes.aisp %>% dplyr::filter(AISP %in% input$batalhoes_crimes), aes(x = mes.ano, y = roubo_transeunte, group = AISP)) +
      geom_line() + theme_classic()
  })
  
  #Renderizando o gráfico após a seleção dos batalhões
  output$crimes_evo <- renderPlot({
    plot_crimes_reativo()
  })
  
  ### Botoes para diferentes graficos de crimes ###
  #Renderizando o gráfico de histograma de crimes quando nenhum botão é apertado
  output$crimes <- renderPlot({
    crimes.aisp %>% ggplot(aes(x = roubo_transeunte)) + geom_histogram(aes(y = after_stat(density)), binwidth=fd, fill = 'lightblue') + labs(title = input$titulo_crimes) + geom_density(kernel = 'epanechnikov') + facet_wrap(~mes.ano)
  })
  
  #Evento interativo para trocar o gráfico exposto de histograma para distribuição acumulada
  observeEvent(input$botao_cdf,{
    output$crimes <- renderPlot({
      crimes.aisp %>% ggplot(aes(x = roubo_transeunte)) +stat_ecdf(geom = "step") + labs(title = input$titulo_crimes) + facet_wrap(~mes.ano)
    })
  })
  
  #Evento interativo para reiniciar o gráfico e trocar de histograma para distribuicao acumulada
  observeEvent(input$reiniciar,{
    output$crimes <- renderPlot({
      crimes.aisp %>% ggplot(aes(x = roubo_transeunte)) + geom_histogram(aes(y = after_stat(density)), binwidth=fd, fill = 'lightblue') + labs(title = input$titulo_crimes) + geom_density(kernel = 'epanechnikov') + facet_wrap(~mes.ano)
    })
  })

  ###########################################
  ######   População (Quarta Aba).     ######
  ###########################################
  
  output$mapa_mundo_pop <- renderPlot({
    
    # extraindo as coordenadas dos países através da função map_data()
    coordenadas_mundo <- map_data("world") 
    coordenadas_mundo <- coordenadas_mundo %>% dplyr::left_join(pop.mundial, by = c("region" = "Country_Dependency"))
    
    # criando um mapa do mundo com a função geom_map do ggplot
    ggplot() + 
      
      # geom_map() function takes world coordinates  
      # as input to plot world map 
      geom_map( 
        data = coordenadas_mundo, map = coordenadas_mundo, 
        aes(long, lat, map_id = region, fill = POW.01)) + 
      scale_fill_gradient(low = "green",high = "red") +
      guides(fill=guide_colourbar(title="Porcentagem do Mundo (0-1)"))
    
    })
  
  ##########################################################
  ######   Preenchendo dados vazios (Quinta Aba).     ######
  ##########################################################
  
  #Plot da distribuição da variável n_filhos sem imputação de dados vazios
  
  output$nfilhos_semimp <- renderPlot({
    
    salarios %>% dplyr::select(n_filhos) %>% ggplot(aes(x=n_filhos, y = after_stat(density))) + geom_density(linetype = 2) + xlab('numero de filhos') + ylab('Densidade de Frequência') + theme_classic()
  
  })
  
  #Plot da distribuição da variável n_filhos com imputação de dados vazios
  
  input_dados_vazios <- eventReactive(c(input$particoes, input$modelo),{
    imp <- mice(salarios %>% dplyr::mutate(estado_civil = as.factor(estado_civil), Grau_de_instrucao =   as.factor(Grau_de_instrucao), regiao = as.factor(regiao)), print = FALSE, m = as.numeric(input$particoes), method = as.character(input$modelo) , seed = 512) 
    names(imp$imp$n_filhos) <- paste("imp", names(imp$imp$n_filhos), sep = "_")
    imp_nfilhos <- imp$imp$n_filhos %>% tidyr::pivot_longer(cols = imp_1:ncol(imp$imp$n_filhos), values_to = "n_filhos", names_to   = "imp") %>% arrange(imp)
    ggplot(imp_nfilhos, aes(x = n_filhos, y = after_stat(density), colour = imp)) + geom_density(linetype = 2) + xlab('numero de    filhos') + ylab('Densidade de Frequência') + theme_classic()
    })
  
    output$nfilhos_comimp <- renderPlot({
        input_dados_vazios()
    })
  }
)
