library(tidyverse)
library(rvest)
library(data.table)

#Construindo uma função customizada
minhaFuncao <- function(param1, param2, param3)
{
  #Corpo da função
  variavelFuncao <- (param1 + param2 ) - param3
  
  #Saída da função
  return(variavelFuncao)
  
}
#Chamando a função customizada
minhaFuncao(param1 = 10, param2 = 5, param3 =  5)

#Exemplos iniciais
#O operador pipe. Exemplo: Ordenando um vetor hipotético qualquer.
c(100, 35, 67, 1, 9, 2, 15) %>% sort()

#O R possui uma função básica para retornar a mediana de um vetor
vetor.hipotetico <- c(100, 35, 67, 1, 9, 2, 15)
median(vetor.hipotetico)

#Podemos construir uma função customizada que pegue o elemento do meio do vetor.
#Porém, essa função só retornará a mediana, caso o vetor esteja ordenado.
#Construindo uma função customizada
mediana.apos.ordem <- function(vetor.ordenado)
{
  #o vetor é par
  if(length(vetor.ordenado) %% 2 == 0)
  {
    mediana <- mean(vetor.ordenado[(length(vetor.ordenado)/2):((length(vetor.ordenado)/2)+1)])
    return(mediana)
  #o vetor é ímpar
  }else
  {
    mediana <- vetor.ordenado[round(length(vetor.ordenado)/2)]
    return(mediana)
  }
}

#funcao caso vetor impar
caso.tamanho.impar.mediana <- function(vetor.ordenado)
{
  #checa se tamanho é ímpar
  if(length(vetor.ordenado) %% 2 != 0)
  {
    return(vetor.ordenado[round(length(vetor.ordenado) / 2)])
  }else
  {
    print('O tamanho do vetor não é ímpar')
    return(NULL)
  }
}

c(100, 35, 67, 1, 9, 2) %>% sort() %>% mediana.apos.ordem()

#O R já tem uma função para retornar a mediana

c(100, 35, 67, 1, 9, 2) 

##Construindo um tibble data frame hipotético. Cada elemento é um retangulo diferente.
df2 <- tibble::as_tibble(data.frame(cores = c("vermelho", "verde", "azul"), id = 1:3, altura = c(7, 35, 42), largura = c(22, 5, 3)))

#Selecionando variáveis estatísticas da nossa matriz de dados.
#Pacote básico
df2[, c("id", "altura", "largura")]
#Dplyr
df2 %>% dplyr::select(id, altura, largura)

#Filtrando valores em uma dada variável

df2 %>% dplyr::filter(cores == "vermelho")

#Construindo novas variáveis estatísticas na nossa matriz de dados.

df2 %>% dplyr::mutate(area = altura * largura, perimetro = 2*altura + 2*largura)

#Construindo um novo tibble data frame hipotético, expandindo o anterior

df3 <- tibble::as_tibble(data.frame(cores = c("vermelho", "verde", "azul", "verde", "vermelho", "azul"), id = 1:6, altura = c(7, 35, 42, 42, 21, 4), largura = c(22, 5, 3, 17, 5, 6)))
df3 <- df3 %>% dplyr::mutate(area = altura * largura, perimetro = 2*altura + 2*largura)

#Agrupando os dados por cor do elemento geométrico (retangular)
df3 %>% dplyr::select(cores, altura:perimetro) %>% dplyr::group_by(cores) %>% dplyr::summarise(altura.media = mean(altura), altura.sd = sd(altura))

#Agrupando e construindo nova variável com a soma das áreas por cor
df3 %>% dplyr::group_by(cores) %>% dplyr::mutate(tot.area.cor = sum(area))

df3 %>% dplyr::group_by(cores) %>% dplyr::summarise(tot.area.cor = sum(area))

##Retomando o exemplo da aula 3 para os dados da população mundial coletados na web
#Crianda função customizada para limpar nomes das variáveis estatísticas
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


##Retomando com os dados de população mundial
#Extraindo os dados da web
populacao.mundial.web <- rvest::read_html("https://en.wikipedia.org/wiki/World_population")
lpop <- populacao.mundial.web %>% rvest::html_nodes("table") %>% html_table()
pop.mundial <- lpop[[4]]

#Limpando os nomes das variáveis estatística com função customizada
names(pop.mundial) <- limpeza.nomes(names(pop.mundial))

#Implementação utilizando pipe

limpeza.nomes.pipe <- function(nomes)
{
  nomes.limpos <- nomes %>% stringr::str_replace_all("/", "") %>% stringr::str_replace_all("\\s", "_") %>% stringr::str_replace_all("__", "_")
  return(nomes.limpos)  
}

names(pop.mundial) <- limpeza.nomes.pipe(names(pop.mundial))
names(pop.mundial)[3] <- "Percentage_of_the_world"

#Selecionando as colunas úteis para analise
#Limpando a variável Population para se tornar uma variável numérica
pop.mundial <- pop.mundial %>% dplyr::mutate(Population = as.numeric(stringr::str_replace_all(Population, "\\,", "")))

#Limpando a variável Percentage_of_the_world and converting to proportions in classe numeric
pop.mundial <- pop.mundial %>% dplyr::mutate(Percentage_of_the_world = as.numeric(stringr::str_replace_all(Percentage_of_the_world, "%", ""))/100)

#Limpando a variável Date e convertendo de character para classe Date 

pop.mundial <- pop.mundial %>% dplyr::mutate(Date = as.IDate(gsub(" ", "-", Date), format = "%d-%b-%Y"))

#Vamos criar uma variável continente ?

#Importando uma base de dados que tenha a informacao de paises por continente
continentes <- readr::read_csv("dados_auxiliares/continents-according-to-our-world-in-data.csv")

#Vinculando esta base à nossa base limpa com os dados de populacao por pais no top-10

pop.mundial <- pop.mundial %>% dplyr::left_join(continentes, by = c("Country / Dependency" = "Entity"))

#Vamos agrupar os dados de populacao por continente e criar uma nova variavel ?

pop.mundial <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::mutate(Population_Continent = sum(Population))
pop.mundial.agg <- pop.mundial %>% dplyr::group_by(Continent) %>% dplyr::summarise(Population_Continent = sum(Population))


##Dever de Casa
#Importando uma base de dados reais para explorarmos durante a aula
crimes.rio <- data.table::fread("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")

#Importando dados com data.table utilizando o readr, oriundo do pacote readr a partir do tidyverse
crimes.rio <- readr::read_csv2("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")

#Importando dados em csv utilizando o fread, funcao oriunda do pacote data.table
crimes.rio <- data.table::fread("dados_auxiliares/BaseDPEvolucaoMensalCisp.csv")

##Vamos criar uma query iteragindo elementos do pacote tidyverse, com o dplyr, pacote pertecente ao tidyverse e o as.IDate a partir do data.table
#Criando variável data, para facilitar filtros e cálculos temporais
crimes.rio <- crimes.rio %>% dplyr::mutate(mes.ano = as.IDate(paste("01", mes, ano, sep = "-"), "%d-%m-%Y")) 

#Agregando por grupos, utilizando função agrupamento e a função summarise para somar uma variável de escolha
crimes.rio.agg <- crimes.rio %>% dplyr::group_by(AISP, mes.ano) %>% dplyr::summarise(furtos.agg = sum(furto_transeunte))

#Filtrando os dados agregado para leitura a partir do ano de 2022
#Filtrando os dados gerados e printando eles como tibble
crimes.rio.agg %>% dplyr::filter(mes.ano > as.IDate("01-01-2022", "%d-%m-%Y"))

#Transformando os dados gerados a partir de tibble para data.table
crimes.rio.agg %>% dplyr::filter(mes.ano > as.IDate("01-01-2022", "%d-%m-%Y")) %>% as.data.table()



