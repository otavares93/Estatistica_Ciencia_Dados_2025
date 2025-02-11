#Exercício no final da Aula 1
#Instalando pacotes no ambiente de trabalho
#Aula 1
install.packages("tidyverse")

#Carregando o pacote no ambiente de trabalho

library(tidyverse)

#Aula 2

#Carregando pacote da Aula 1

library(tidyverse)

####################################
###   Slide - Criando variaveis  ###
####################################

num1 <- 3
num2 <- 4

exemplo.concat <- paste("a", 3, sep = "-")


###################################################
###   Slide - Operacoes Matematicas Algebricas  ###
###################################################
##Operador soma e subtracao
soma <- num1 + num2
subtracao <- num2 - num1

#Operando multiplicacoes
multiplicacao <- num1 * num2

#Operando razoes
razao <- multiplicacao / num1

##Operador de potencias de numeros
soma_ao_quadrado <- soma^2

##Duas maneiras de calcular raízes
raiz_quadrada_soma <- soma^(1/2)
raiz_quadrada_soma <- sqrt(soma)

#Raiz cubica
raiz_cubica_soma <- soma^(1/3)

#Operador de que extrai resto da divisão
resto <- num2 %% num1
10 %% 7
10 %% 4

#Operador de extração de inteiros a partir de uma divisão
inteiro <- num2 %/% num1
10 %/% 7
10 %/% 4
10 %/% 5

#Exercicio de aula - Para operadores relacionais, o aluno deve experimentar
#comparações semelhantes aos que estão no slide.


################################################
###   Slide - Operacoes Matematicas Logicas  ###
################################################
#Exemplo de comparações lógicas simples

c(TRUE, TRUE, FALSE) & c(FALSE, TRUE, FALSE)

#Exemplo com funções resumo de operadores logicos
#
all(c(1,4,7) != c(1, 4, 7))
#
any(c(1,3,5) != c(2, 4, 7))

##################################################################
### Definindo uma variável como um número ou um texto (string) ###
##################################################################

text1 <- "mares"
text2 <- "montanhas"

#Primeira frase - Concatenando numero com texto
paste(num1, text1, sep = " ") 
#Segunda frase - Concatenando numero com texto
paste(num2, text2, sep = " ") 

"1 praia, 3 mares e 4 montanhas"

text3 <- "praia"
num3 <- 1

frase1 <- paste(num3, text3, sep = " ")
frase2 <- paste(num1, text1, sep = " ")
frase3 <- paste(num2, text2, sep = " ")

frase4 <- paste(frase1, paste(frase2, frase3, sep = " e "), sep = ", ")
#Concatenando frases
paste(paste(num1, text1, sep = " "), paste(num2, text2, sep = " "), sep = " e ") 

gsub("", "e", "Otto Tavar3s")



################################################################
### Definindo uma variável como um vetor e suas propriedades ###
################################################################

vec3 <- c(1, "casa")
vec4 <- 50:120
vec5 <- seq(2,20, by = 2)
vec6  <- sample(x = 1:200, size = 50, replace = TRUE)


#############################################################################
###   Slide - O elemento Data Frame e sua importância para análises no R  ###
#############################################################################

df1 <- data.frame(cores = c("vermelho", "verde", "azul"), id = c(1, 2, 3))
#Indexando um data frame a partir do elemento [,]
df1[1,2]

#Indexando um data frame a partir do elemento $
df1$cores

## O elemento matriz e exemplo de multiplicação de matrizes e de data frames

#Multiplicação de matrizes deve realizar a multiplicação linha-coluna
#e somando os elementos multiplicados como se deve fazer a multiplicação de matrizes
mm1 <- matrix(c(1, 25, 90, 45, 12, 32), nrow = 3, ncol = 2)
mm2 <- matrix(c(1, 4, 10, 5, 12, 3), nrow = 2, ncol = 3)
mm1 %*% mm2


#Repare que a multiplicacao é elemento por elemento ao invés de ser a multiplicacao de matrizes
mm1 <- matrix(c(1, 25, 90, 45), nrow = 2, ncol = 2)
mm2 <- matrix(c(1, 4, 10, 5), nrow = 2, ncol = 2)
mm1 * mm2

###################################################################
###   Slide - Exemplo aplicado com uma tabela em excel da CNAE  ###
###################################################################

tabs <- 3:4
tabelas_cnae <- lapply(tabs, function(tab) readxl::read_excel(paste("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/IM_DME/Estatistica_DesenhoIndustrial/trabalhos/tabelas_2022/Tabela ", tab, ".xlsx" , sep = "")))

###################################################################
###   Slide - Instalando outros pacotes ao ambiente de trabalho ###
###################################################################

install.packages("wesanderson")
install.packages("data.table")

#Carregando outros pacotes

library(wesanderson)
library(data.table)

