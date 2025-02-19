library(tidyverse)
library(readxl)
dados_educacionais <- readxl::read_excel("dados_auxiliares/Solucoes-Bibliotecas-IES-2024.xlsx")
dados_educacionais_agregados <- dados_educacionais %>% dplyr::group_by(UF) %>% dplyr::summarise(n_instituicoes = n())
#média
mean(dados_educacionais_agregados$n_instituicoes)
#desvio padrão
sd(dados_educacionais_agregados$n_instituicoes)



