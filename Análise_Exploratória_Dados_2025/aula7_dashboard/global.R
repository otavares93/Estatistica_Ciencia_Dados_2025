library(tidyverse)
library(rvest)
library(data.table)
library(knitr)
library(RColorBrewer)

#Elemento para registrar o passo a passo da utilização do app e vis. em grafos.
#options(shiny.reactlog = TRUE)

#Após rodar o app, chamar essa função para vis. a evolução de uso.
#shiny::reactlogShow()

#Importando os dados
populacao.mundial.web <- rvest::read_html("https://en.wikipedia.org/wiki/World_population")
lpop <- populacao.mundial.web %>% rvest::html_nodes("table") %>% html_table()
pop.mundial <- lpop[[4]]

#Funções auxiliares

limpeza.nomes <- function(nomes)
{
  nomes.limpos <- gsub("__", "_", gsub("\\s","_",gsub("/", "", nomes)))
  return(nomes.limpos)
}
names(pop.mundial) <- limpeza.nomes(names(pop.mundial))
names(pop.mundial)[3] <- "Percentage_of_the_world"

print(names(pop.mundial))

#Limpando os dados

pop.mundial <- pop.mundial %>% dplyr::mutate(Population = as.numeric(stringr::str_replace_all(Population, "\\,", "")))
pop.mundial <- pop.mundial %>% dplyr::mutate(Percentage_of_the_world = as.numeric(stringr::str_replace_all(Percentage_of_the_world, "%", ""))/100)
pop.mundial <- pop.mundial %>% dplyr::mutate(Date = as.IDate(gsub(" ", "-", Date), format = "%d-%b-%Y"))

#Importando os dados de continente e vinculando a base principal

#continentes <- readr::read_csv("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2025/dados_auxiliares/continents-according-to-our-world-in-data.csv")
#to-deploy
continentes <- readr::read_csv("dados_auxiliares/continents-according-to-our-world-in-data.csv")
pop.mundial <- pop.mundial %>% dplyr::left_join(continentes, by = c("Country_Dependency" = "Entity"))
pop.mundial <- pop.mundial %>% dplyr::mutate(Country_Dependency = ifelse(Country_Dependency == "United States", "USA", Country_Dependency))
pop.mundial <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::mutate(Population_Continent = sum(Population))



