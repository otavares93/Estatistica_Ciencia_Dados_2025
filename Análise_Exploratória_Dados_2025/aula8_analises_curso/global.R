library(tidyverse)
library(tidyr)
library(purrr)
library(dlookr)
library(summarytools)
library(readxl)
library(knitr)
library(data.table)
library(ggpubr)
library(corrplot)
library(rcompanion)
library(stargazer)
library(mice)
library(rmarkdown)
library(tinytex)
library(sandwich)
library(magrittr)
library(plm)
library(rvest)
library(shiny)

#Carregando funções auxiliares globais

#FD
fd <- function(x) {
  n <-length(x)
  return((2*IQR(x))/n^(1/3))
}


#Sturge
sr <- function(x) {
  n <-length(x)
  return((3.49*sd(x))/n^(1/3))
}

###################################
####Importando dados de salário####
###################################

salarios <- readxl::read_excel("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2025/dados_auxiliares/dados_bussab_m.xlsx")

###################################################
#### Importando dados de crimes e processando #####
###################################################

crimes <- readr::read_csv2("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2025/dados_auxiliares/BaseDPEvolucaoMensalCisp.csv", locale = readr::locale(encoding = "latin1"))
crimes.aisp <- crimes %>% dplyr::select(CISP, mes, ano,  AISP,  RISP, roubo_transeunte, roubo_celular, posse_drogas) %>% 
               dplyr::mutate(mes.ano = as.IDate(paste("01", mes, ano, sep = "-"), format = "%d-%m-%Y")) %>% 
               dplyr::filter(mes.ano %in% c(as.IDate("01-12-2019",format = "%d-%m-%Y"), as.IDate("01-12-2020",format = "%d-%m-%Y"), as.IDate("01-12-2021",format = "%d-%m-%Y") ,as.IDate("01-12-2022",format = "%d-%m-%Y"))) %>% 
               dplyr::group_by(AISP, mes.ano) %>% dplyr::summarise(roubo_transeunte = sum(roubo_transeunte), roubo_celular = sum(roubo_celular), posse_drogas = sum(posse_drogas))

crimes.aisp <- crimes.aisp %>% dplyr::left_join(crimes %>% dplyr::select(AISP, Regiao, mes, ano) %>% 
            dplyr::mutate(mes.ano = as.IDate(paste("01", mes, ano, sep = "-"), format = "%d-%m-%Y")) %>% 
            dplyr::filter(mes.ano %in% c(as.IDate("01-12-2019",format = "%d-%m-%Y"), as.IDate("01-12-2020",format = "%d-%m-%Y"), as.IDate("01-12-2021",format = "%d-%m-%Y") ,as.IDate("01-12-2022",format = "%d-%m-%Y"))) %>% 
            dplyr::distinct(AISP, mes.ano, Regiao), by = c("AISP" = "AISP", "mes.ano"="mes.ano"))

###########################################################################
#### Importando e processando dados de população a partir da wikipedia ####
###########################################################################

populacao.mundial.web <- read_html("https://en.wikipedia.org/wiki/World_population")
lpop <- populacao.mundial.web %>% rvest::html_nodes("table") %>% html_table()
pop.mundial <- lpop[[4]]
#Criando função auxiliar para limpar os nomes das variáveis, 
#tirando os espacos e substituindo por "_".

#Gabarito Aula 3
limpeza.nomes <- function(nomes)
{
  nomes.limpos <- gsub("__", "_", gsub("\\s","_",gsub("/", "", nomes)))
  return(nomes.limpos)
}


#Implementacao da Aula 4 - Substituindo o gsub pela funcao do stringr
limpeza.nomes <- function(nomes)
{
  nomes.limpos <- stringr::str_replace_all(stringr::str_replace_all(stringr::str_replace_all(nomes, "/", ""), "\\s", "_"), "__", "_")
  return(nomes.limpos)  
}

names(pop.mundial) <- limpeza.nomes(names(pop.mundial))

#Implementação utilizando pipe

limpeza.nomes.pipe <- function(nomes)
{
  nomes.limpos <- nomes %>% stringr::str_replace_all("/", "") %>% stringr::str_replace_all("\\s", "_") %>% stringr::str_replace_all("__", "_")
  return(nomes.limpos)  
}

#Função que padroniza uma determinada variável para fazer o máximo vira 1 e o mínimo virar 0, enquanto os outros valores são normalizados nessa amplitude.

padroniza <- function(vec)
{
  return((vec - min(vec))/(max(vec) - min(vec)))
}

names(pop.mundial) <- limpeza.nomes.pipe(names(pop.mundial))
names(pop.mundial)[3] <- "Percentage_of_the_world"

#Limpando a variável Population para se tornar uma variável numérica
pop.mundial <- pop.mundial %>% dplyr::mutate(Population = as.numeric(stringr::str_replace_all(Population, "\\,", "")))

#Limpando a variável Percentage_of_the_world and converting to proportions in classe numeric
pop.mundial <- pop.mundial %>% dplyr::mutate(Percentage_of_the_world = as.numeric(stringr::str_replace_all(Percentage_of_the_world, "%", ""))/100, POW.01 = padroniza(Percentage_of_the_world))

#Limpando a variável Date e convertendo de character para classe Date 

pop.mundial <- pop.mundial %>% dplyr::mutate(Date = as.IDate(gsub(" ", "-", Date), format = "%d-%b-%Y"))

#Vamos criar uma variável continente ?

#Importando uma base de dados que tenha a informacao de paises por continente
continentes <- readr::read_csv("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2025/dados_auxiliares/continents-according-to-our-world-in-data.csv")

#Vinculando esta base à nossa base limpa com os dados de populacao por pais no top-10

pop.mundial <- pop.mundial %>% dplyr::left_join(continentes, by = c("Country_Dependency" = "Entity"))

pop.mundial <- pop.mundial %>% dplyr::mutate(Country_Dependency = ifelse(Country_Dependency == "United States", "USA", Country_Dependency))

#Vamos agrupar os dados de populacao por continente e criar uma nova variavel ?

pop.mundial <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::mutate(Population_Continent = sum(Population))




