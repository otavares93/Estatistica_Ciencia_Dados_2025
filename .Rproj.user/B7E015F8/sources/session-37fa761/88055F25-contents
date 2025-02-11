library(tidyverse)
library(rvest)
library(data.table)
library(readxl)

################################
### Lendo arquivos txt ou csv ###
################################
##Arquivo txt
#header = FALSE
arquivo.texto <-read.table('dados_auxiliares/emotion.txt', sep = "\t")
#header = TRUE
arquivo.texto <-read.table('dados_auxiliares/emotion.txt',header = TRUE ,  sep = "\t")
##Arquivo csv
tomato <- read.csv("dados_auxiliares/TomatoFirst.csv", sep = ",")
tomato <- read.csv2("dados_auxiliares/TomatoFirst.csv")


########################################################
### Utilizando as bibliotecas tydeverse e data.table ###
########################################################

#Lendo o mesmo dado csv com esses pacotes
tomato.tbl <- readr::read_csv("dados_auxiliares/TomatoFirst.csv")
tomato.data.table <- data.table::fread("dados_auxiliares/TomatoFirst.csv")

#######################################################
### Vamos ler uma base com milhares de observações ###
#######################################################

ini.base <- Sys.time()
crimes.base <- read.csv2("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv", encoding = 'latin1')
fim.base <- Sys.time()
total.base <- fim.base - ini.base

ini.base.dt <- Sys.time()
crimes.base <- data.table::fread("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")
fim.base.dt <- Sys.time()
total.base.dt <- fim.base.dt - ini.base.dt

ini.base.rr <- Sys.time()
crimes.base <- readr::read_csv2("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")
fim.base.rr <- Sys.time()
total.base.rr <- fim.base.rr - ini.base.rr


#Com o fread oriundo do data table
crimes.data.table <- fread("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")

#Importando com read.csv da base do R sem tratatamento. O que há de errado?
crimes.base <- read.csv("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")

#Vamos tentar um separador diferente? read.csv2 seria suficiente?
crimes.base <- read.csv("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv", sep = ";")
#crimes.base <- read.csv2("BaseDPEvolucaoMensalCisp.csv")

######
## Lendo arquivo excel
######

salarios <- readxl::read_excel("dados_auxiliares/dados_bussab_m.xlsx")   

###############################################################
### Vamos escrever e contar o tempo levado para cada função ###
### e checar se a afirmação dos desenvolvedores do data table #
###    está correta.                                          #
###############################################################

#Base
ini.base <- Sys.time()
write.csv(crimes.base, file = "crimes_base_teste.csv")
fim.base <- Sys.time()
total.base <- fim.base - ini.base

print(paste('tempo para escrever um csv com função basica:', total.base, sep = ""))

#Data table - fwrite
ini.data.table <- Sys.time()
fwrite(crimes.base, "crimes_base_teste.csv")
fim.data.table <- Sys.time()
total.data.table <- fim.data.table - ini.data.table

print(paste('tempo para escrever um csv com função fwrite do data.table:', total.data.table, sep = ""))

###############################################################
####                 Exemplo com dados WEB                 ####
###############################################################

library(rvest)
#Lendo dados da populacao mundial a partir do wikipedia
#Checando se o site permite extracao dos dados
#paths_allowed(paths="https://en.wikipedia.org/wiki/World_population")

#Lendo os dados e identificando tabelas
populacao.mundial.web <- rvest::read_html("https://en.wikipedia.org/wiki/World_population")
lpop <- populacao.mundial.web %>% rvest::html_nodes("table") %>% html_table()
pop.mundial <- lpop[[4]]

g1.agora <- read_html("https://www.globo.com/")
titulos <- g1.agora %>% rvest::html_elements("h2") %>% html_text()
headlines <- titulos[1:16]
##################################################################
##Criando uma função customizada simples apenas para ilustração ##
##################################################################

minhaFuncao <- function(param1, param2, param3)
{
  #Corpo da função
  variavelFuncao <- (param1 + param2 ) - param3
  
  #Saída da função
  return(variavelFuncao)
  
}
#Chamando a função customizada
minhaFuncao(param1 = 10, param2 = 5, param3 =  5)



#########################################################################
##Definindo uma função customizada para auxiliar no tratamento de dados #
##Padrao de datas não está em formato de fácil tratamento               #
#########################################################################

crimes.data.table$mes_ano
#Serve para filtros, porém uma mera subtracao de datas não funcionaria
crimes.data.table$mes_ano[nrow(crimes.data.table)] - crimes.data.table$mes_ano[1]

#Vamos criar uma função que limpe a variável data em character e nos entregue em 
#formato IDate já limpa e pronta para utilização
corrige.data <- function(input)
{
  input.mes <- unlist(lapply(strsplit(input, split = "m"), function(x) x[2]))
  input.ano <- unlist(lapply(strsplit(input, split = "m"), function(x) x[1]))
  datas <- as.IDate(paste("01", input.mes, input.ano , sep = "-"), format = "%d-%m-%Y")
  return(datas)
}

crimes.data.table$mes.ano <- corrige.data(crimes.data.table$mes_ano)
crimes.data.table$mes.ano[nrow(crimes.data.table)] - crimes.data.table$mes.ano[1]
