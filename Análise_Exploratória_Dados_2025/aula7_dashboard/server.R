library(shiny)


shinyServer(function(input, output){
    
  
  ######################################
  ## Primeira Aba (Gráfico de Barras) ##
  ######################################

  output$grafico_barras <- renderPlot({
    
    ggplot(pop.mundial %>% dplyr::select(Country_Dependency, Population, Continent), aes(x = Country_Dependency, y = Population, fill = Continent)) + geom_bar(stat = "identity") + xlab("Países") + ylab("População") + labs(title = "População dos países top-10 do mundo", subtitle = "Coleta web para análise com ggplot", caption = "Fonte: Wikipedia") + theme_minimal()  
    
  })  
  
  ######################################
  ##         Segunda Aba (Mapa)       ##
  ######################################
  
  output$grafico_mapa <- renderPlot({
    
    # extraindo as coordenadas dos países através da função map_data()
    coordenadas_mundo <- map_data("world") 
    coordenadas_mundo <- coordenadas_mundo %>% dplyr::left_join(pop.mundial, by = c("region" = "Country_Dependency"))
    
    # criando um mapa do mundo com a função geom_map do ggplot
    ggplot() + 
      
      # geom_map() function takes world coordinates  
      # as input to plot world map 
      geom_map( 
        data = coordenadas_mundo, map = coordenadas_mundo, 
        aes(long, lat, map_id = region, fill = Population)) + 
      scale_fill_gradient(low = "green",high = "red") +
      guides(fill=guide_colourbar(title="Pop em abs."))
    
    
    })
  
  
  pop.mundial.selec <- reactive({
    pop.mundial <- pop.mundial %>% dplyr::filter(Country_Dependency == input$pais_selec)
  })
  
  output$grafico_mapa_selecionado <- renderPlot({
    
    # extraindo as coordenadas dos países através da função map_data()
    coordenadas_mundo <- map_data("world") 
    #pop.mundial <- pop.mundial %>% dplyr::filter(Country_Dependency == input$pais_selec)
    #coordenadas_mundo <- coordenadas_mundo %>% dplyr::left_join(pop.mundial, by = c("region" = "Country_Dependency"))
    coordenadas_mundo <- coordenadas_mundo %>% dplyr::left_join(pop.mundial.selec(), by = c("region" = "Country_Dependency"))
    # criando um mapa do mundo com a função geom_map do ggplot
    ggplot() + 
      
      # geom_map() function takes world coordinates  
      # as input to plot world map 
      geom_map( 
        data = coordenadas_mundo, map = coordenadas_mundo, 
        aes(long, lat, map_id = region, fill = Population)) + 
      scale_fill_gradient(low = "green",high = "red") +
      guides(fill=guide_colourbar(title="Pop em abs."))
    })
  }
)  


