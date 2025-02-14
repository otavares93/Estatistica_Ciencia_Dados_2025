Aula6 - Visualização de dados
================
2025-02-13

## Importando bibliotecas para análise

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(knitr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

## Importando os dados

Vamos importar os dados de crimes ocorridos e registrados pelos
batalhões do estado do Rio de Janeiro.

``` r
crimes <- readr::read_csv2("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2024/dados_auxiliares/BaseDPEvolucaoMensalCisp.csv", locale = readr::locale(encoding = "latin1"))
```

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

    ## Rows: 32245 Columns: 63
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr  (3): mes_ano, munic, Regiao
    ## dbl (60): CISP, mes, ano, AISP, RISP, mcirc, hom_doloso, lesao_corp_morte, l...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Limpando as datas e selecionando as variáveis de interesse

``` r
#Ajustando as datas, selecionando variáveis de interesse
#crimes$mes.ano <- as.IDate(paste("01", unlist(lapply(strsplit(crimes$mes_ano, split = "m"), function(x) x[2])), unlist(lapply(strsplit(crimes$mes_ano, split = "m"), function(x) x[1])), sep = "-"), format = "%d-%m-%Y")

crimes <- crimes %>% dplyr::mutate(mes.ano = as.IDate(paste("01", mes, ano, sep = "-"), format = "%d-%m-%Y")) %>% dplyr::select(CISP, mes.ano, AISP, roubo_cx_eletronico:apreensao_drogas_sem_autor)



#Filtrando apenas para as regi˜
```

\#Agregando variáveis por batalhão para facilitar a visualização e
selecionando apenas três batalhões de interesse

``` r
crimes.regioes <- crimes %>% dplyr::group_by(AISP, mes.ano) %>% dplyr::summarise_at(vars(furto_veiculos:total_furtos), sum)

#Dando nome aos bairros atendidos pelos batalhões em questão e em seguida os selecionando como batalhões de interesse

crimes.regioes$aisp.nm <- ifelse(crimes.regioes$AISP == 2, "Botafogo", ifelse(crimes.regioes$AISP == 26, "Teresópolis",  ifelse(crimes.regioes$AISP == 20, "Nova Iguaçu", NA)))
crimes.regioes <- crimes.regioes %>% dplyr::filter(AISP %in% c(2, 20, 26))
```

## Evolução de furtos por região

Podemos criar esse plot em sua versão básica de duas maneiras.

1.  Parametrizar (mapear os dados para elementos geométricos) o ggplot
    com a estética e adicionar o elemento geométrico em seguida sem
    precisar realizar sua parametrização.

``` r
ggplot(data = crimes.regioes, aes(x = mes.ano, y = total_furtos, group = AISP)) + geom_line()
```

![](Aula6_files/figure-gfm/evolucao%20furtos%20base%201-1.png)<!-- -->

2.  Apenas fornecer a base para o ggplot e deixar toda a parametrização
    para o elemento geométrico

``` r
ggplot(data = crimes.regioes) + geom_line(aes(x = mes.ano, y = total_furtos, group = AISP))
```

![](Aula6_files/figure-gfm/evolucao%20furtos%20base%202-1.png)<!-- -->
\## Adicionando cores ao gráfico

``` r
ggplot(data = crimes.regioes, aes(x = mes.ano, y = total_furtos, group = AISP, color = aisp.nm)) + 
  geom_line()
```

![](Aula6_files/figure-gfm/evolucao%20furtos%20base%201%20com%20cores-1.png)<!-- -->

## Alterando o tema gráfico

``` r
ggplot(data = crimes.regioes, aes(x = mes.ano, y = total_furtos, group = AISP, color = aisp.nm)) + 
  geom_line() +
  theme_bw()
```

![](Aula6_files/figure-gfm/evolucao%20furtos%20base%201%20com%20cores%20e%20com%20tema-1.png)<!-- -->

## Ajustando os eixos e títulos do gráfico

``` r
ggplot(data = crimes.regioes, aes(x = mes.ano, y = total_furtos, group = AISP, color = aisp.nm)) + 
  geom_line() +
  xlab("Período de Análise") +
  ylab("Ocorrências de roubo registradas") +
  labs(title = "Séries temporais de furto por região", subtitle = "Regiões selecionadas no Estado do Rio de Janeiro", caption = "Fonte: Instituto de Segurança Pública - RJ") +
  theme_bw() +
  guides(colour = guide_legend(title = "Regiões de Segurança Pública"))
```

![](Aula6_files/figure-gfm/evolucao%20furtos%20base%201%20com%20cores,%20com%20tema%20e%20titulos%20ajustados-1.png)<!-- -->

## Dever de casa

``` r
ggplot(data = crimes.regioes, aes(x = mes.ano, y = total_furtos, group = AISP, color = furto_transeunte, shape = aisp.nm)) + geom_line() +
xlab("Período de Análise") +
ylab("Ocorrências de roubo registradas") +
labs(title = "Séries temporais de furto por região", subtitle = "Regiões selecionadas no Estado do Rio de Janeiro", caption = "Fonte: Instituto de Segurança Pública - RJ") +
theme_bw() +
scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
guides(colour = guide_colourbar(title = "Furto à traseunte"), shape = guide_legend(title = "Regiões de Segurança Pública"))
```

![](Aula6_files/figure-gfm/evolucao%20furtos%20base%201%20com%20cores,%20com%20tema%20e%20titulos%20ajustados%20e%20variavels%20adicional-1.png)<!-- -->
