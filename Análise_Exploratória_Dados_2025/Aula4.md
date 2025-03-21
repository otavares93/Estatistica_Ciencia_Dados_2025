Análise de dados criminais - Teste de Hipótese
================
Otto Tavares
2025-03-20

## Introdução

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(purrr)
library(dlookr)
```

    ## Registered S3 methods overwritten by 'dlookr':
    ##   method          from  
    ##   plot.transform  scales
    ##   print.transform scales
    ## 
    ## Attaching package: 'dlookr'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     transform

``` r
library(summarytools)
```

    ## 
    ## Attaching package: 'summarytools'
    ## 
    ## The following object is masked from 'package:tibble':
    ## 
    ##     view

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

``` r
library(ggpubr)
library(corrplot)
```

    ## corrplot 0.95 loaded

``` r
library(rcompanion)
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

# Limpando os dados e realizando diagnóstico

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 234 × 8
    ##    variables types   data_count missing_count missing_percent unique_count
    ##    <chr>     <chr>        <int>         <dbl>           <dbl>        <int>
    ##  1 AISP      numeric          4             0               0            1
    ##  2 AISP      numeric          4             0               0            1
    ##  3 AISP      numeric          4             0               0            1
    ##  4 AISP      numeric          4             0               0            1
    ##  5 AISP      numeric          4             0               0            1
    ##  6 AISP      numeric          4             0               0            1
    ##  7 AISP      numeric          4             0               0            1
    ##  8 AISP      numeric          4             0               0            1
    ##  9 AISP      numeric          4             0               0            1
    ## 10 AISP      numeric          4             0               0            1
    ## # ℹ 224 more rows
    ## # ℹ 2 more variables: unique_rate <dbl>, AISP <dbl>

# Descrição via boxplot e tabelas de contingência

![](Aula4_files/figure-gfm/descrevendo%20a%20base%20com%20box%20plot%20roubo%20transeunte-1.png)<!-- -->

![](Aula4_files/figure-gfm/descrevendo%20a%20base%20com%20box%20plot%20roubo%20celular-1.png)<!-- -->

![](Aula4_files/figure-gfm/descrevendo%20a%20base%20com%20box%20plot%20roubo%20transeunte%20por%20regiao-1.png)<!-- -->

    ## Cross-Tabulation, Total Proportions  
    ## mes.ano * Regiao  
    ## Data Frame: crimes.aisp  
    ## 
    ## ------------ -------- -------------------- ------------ ---------------- ------------ --------------
    ##                Regiao   Baixada Fluminense      Capital   Grande Niterói     Interior          Total
    ##      mes.ano                                                                                        
    ##   2019-12-01                     6 ( 3.8%)   17 (10.9%)         2 (1.3%)   14 ( 9.0%)    39 ( 25.0%)
    ##   2020-12-01                     6 ( 3.8%)   17 (10.9%)         2 (1.3%)   14 ( 9.0%)    39 ( 25.0%)
    ##   2021-12-01                     6 ( 3.8%)   17 (10.9%)         2 (1.3%)   14 ( 9.0%)    39 ( 25.0%)
    ##   2022-12-01                     6 ( 3.8%)   17 (10.9%)         2 (1.3%)   14 ( 9.0%)    39 ( 25.0%)
    ##        Total                    24 (15.4%)   68 (43.6%)         8 (5.1%)   56 (35.9%)   156 (100.0%)
    ## ------------ -------- -------------------- ------------ ---------------- ------------ --------------

# Construindo os histogramas da Aula 1

![](Aula4_files/figure-gfm/filtrando%20os%20dados%20e%20visualizando%20dists.-1.png)<!-- -->

# Aplicando estimativa de densidade via kernel, através do kernel de epanechnikov

![](Aula4_files/figure-gfm/filtrando%20os%20dados%20e%20visualizando%20dists.%20com%20kernel-1.png)<!-- -->

# Calculando a distribuição acumulada empírica do fenômeno observado

![](Aula4_files/figure-gfm/filtrando%20os%20dados%20e%20visualizando%20dists.%20acumuladas-1.png)<!-- -->

    ## # A tibble: 4 × 2
    ##   mes.ano    roubo_transeunte_md
    ##   <IDate>                  <dbl>
    ## 1 2019-12-01                  84
    ## 2 2020-12-01                  56
    ## 3 2021-12-01                  56
    ## 4 2022-12-01                  46

# Calculando a dispersão e as correlações de Pearson

![](Aula4_files/figure-gfm/calculando%20dispersao%20para%20as%20duas%20datas-1.png)<!-- -->

# Calculando a dispersão e as correlações de Pearson e de Spearman

## Pearson

    ## `geom_smooth()` using formula = 'y ~ x'

![](Aula4_files/figure-gfm/calculando%20dispersao%20para%20análise%20de%20pearson%20em%20relação%20não%20linear-1.png)<!-- -->

## Spearman

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Aula4_files/figure-gfm/calculando%20dispersao%20para%20análise%20de%20separman%20em%20relação%20não%20linear-1.png)<!-- -->

![](Aula4_files/figure-gfm/analise%20temporal%20de%20roubo%20a%20transeunte%20vs%20roubo%20celular%20para%20o%20estado-1.png)<!-- -->

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

![](Aula4_files/figure-gfm/analise%20temporal%20de%20roubo%20a%20transeunte%20vs%20roubo%20celular%20para%20um%20dado%20batalhao-1.png)<!-- -->

# Calculando a dispersão e as correlações

![](Aula4_files/figure-gfm/calculando%20corrplot%20pearson-1.png)<!-- -->

![](Aula4_files/figure-gfm/calculando%20corrplot%20spearman-1.png)<!-- -->

# Realizando o teste de Hipótese (Há evidência estatística na queda de roubo a transeunte após a pandemia?)

## Teste de Shapiro Wilk (para checar normalidade) nas amostras pré e pós pandemia

A hipótese nula aqui é: A distribuição de roubos a transeunte por
batalhão segue distribuição normal no mês selecionado para representar o
pré pandemia.

A hipótese alternativa: A distribuição de roubos a transeunte por
batalhão não segue distribuição normal no mês selecionado para
representar o pré pandemia.

### Pré Pandemia

``` r
pre.pandemia <- crimes.aisp %>% dplyr::filter(mes.ano %in% as.IDate("01-12-2019",format = "%d-%m-%Y")) %>% dplyr::select(roubo_transeunte) 
```

    ## Adding missing grouping variables: `AISP`

``` r
shapiro.test(pre.pandemia$roubo_transeunte)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  pre.pandemia$roubo_transeunte
    ## W = 0.83822, p-value = 5.717e-05

### Realizando o QQ-Plot do pré pandemia para ilustrar sua utilização

``` r
qqnorm(pre.pandemia$roubo_transeunte)
```

![](Aula4_files/figure-gfm/qqplot%20pre%20pandemia-1.png)<!-- -->

### Pós Pandemia (Primeiro ano de Pandemia - 01/12/2020)

A hipótese nula aqui é: A distribuição de roubos a transeunte por
batalhão segue distribuição normal no mês selecionado para representar o
pós pandemia.

A hipótese alternativa: A distribuição de roubos a transeunte por
batalhão não segue distribuição normal no mês selecionado para
representar o pós pandemia.

    ## Adding missing grouping variables: `AISP`

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  pos.pandemia$roubo_transeunte
    ## W = 0.84508, p-value = 8.266e-05

### Realizando o QQ-Plot do pós pandemia para ilustrar sua utilização

![](Aula4_files/figure-gfm/qqplot%20pos%20pandemia-1.png)<!-- -->

### Teste de Wilcoxon para checar o pareamento - (Pré vs. Primeiro ano de Pandemia - 01/12/2020)

Realizado o teste de Wilcoxon para checar o pareamento entre as
distribuições de roubo a transeunte do pré e do pós pandemia.

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós não é igual a zero.

``` r
wilcox.test(pre.pandemia$roubo_transeunte, pos.pandemia$roubo_transeunte, paired = T, exact = F)
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 690, p-value = 3.709e-06
    ## alternative hypothesis: true location shift is not equal to 0

### Teste de Wilcoxon para checar o pareamento (Maior que) - (Pré vs. Primeiro ano de Pandemia - 01/12/2020)

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é maior do que zero.

``` r
wilcox.test(pre.pandemia$roubo_transeunte, pos.pandemia$roubo_transeunte, paired = T, exact = F, alternative = "greater")
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 690, p-value = 1.854e-06
    ## alternative hypothesis: true location shift is greater than 0

### Teste de Wilcoxon para checar o pareamento (Menor que) - (Pré vs. Primeiro ano de Pandemia - 01/12/2020)

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é menor do que zero.

``` r
wilcox.test(pre.pandemia$roubo_transeunte, pos.pandemia$roubo_transeunte, paired = T, exact = F, alternative = "less")
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 690, p-value = 1
    ## alternative hypothesis: true location shift is less than 0

### Pós Pandemia (Segundo ano de Pandemia - 01/12/2021)

A hipótese nula aqui é: A distribuição de roubos a transeunte por
batalhão segue distribuição normal no mês selecionado para representar o
pós pandemia.

A hipótese alternativa: A distribuição de roubos a transeunte por
batalhão não segue distribuição normal no mês selecionado para
representar o pós pandemia.

    ## Adding missing grouping variables: `AISP`

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  pos.pandemia$roubo_transeunte
    ## W = 0.88711, p-value = 0.0009614

### Teste de Wilcoxon para checar o pareamento - (Pré vs. Segundo ano de Pandemia - 01/12/2021)

Realizado o teste de Wilcoxon para checar o pareamento entre as
distribuições de roubo a transeunte do pré e do pós pandemia.

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós não é igual a zero.

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 654, p-value = 5.209e-06
    ## alternative hypothesis: true location shift is not equal to 0

### Teste de Wilcoxon para checar o pareamento (Maior que) - (Pré vs. Segundo ano de Pandemia - 01/12/2021)

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é maior do que zero.

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 654, p-value = 2.605e-06
    ## alternative hypothesis: true location shift is greater than 0

### Teste de Wilcoxon para checar o pareamento (Menor que) - (Pré vs. Segundo ano de Pandemia - 01/12/2021)

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é menor do que zero.

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 654, p-value = 1
    ## alternative hypothesis: true location shift is less than 0

### Pós Pandemia (Terceiro ano de Pandemia - 01/12/2022)

A hipótese nula aqui é: A distribuição de roubos a transeunte por
batalhão segue distribuição normal no mês selecionado para representar o
pós pandemia.

A hipótese alternativa: A distribuição de roubos a transeunte por
batalhão não segue distribuição normal no mês selecionado para
representar o pós pandemia.

    ## Adding missing grouping variables: `AISP`

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  pos.pandemia$roubo_transeunte
    ## W = 0.83333, p-value = 4.419e-05

### Teste de Wilcoxon para checar o pareamento (Pré vs. Terceiro ano de Pandemia - 01/12/2022)

Realizado o teste de Wilcoxon para checar o pareamento entre as
distribuições de roubo a transeunte do pré e do pós pandemia.

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós não é igual a zero.

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 669, p-value = 1.726e-06
    ## alternative hypothesis: true location shift is not equal to 0

### Teste de Wilcoxon para checar o pareamento (Maior que) - (Pré vs. Terceiro ano de Pandemia - 01/12/2022)

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é maior do que zero.

``` r
wilcox.test(pre.pandemia$roubo_transeunte, pos.pandemia$roubo_transeunte, paired = T, exact = F, alternative = "greater")
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 669, p-value = 8.631e-07
    ## alternative hypothesis: true location shift is greater than 0

### Teste de Wilcoxon para checar o pareamento (Menor que) - (Pré vs. Terceiro ano de Pandemia - 01/12/2022)

A hipótese nula aqui é: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é igual a zero.

A hipótese alternativa: A mediana das diferenças (pré - pós) entre as
distribuições de roubos a transeunte pré e pós é menor do que zero.

``` r
wilcox.test(pre.pandemia$roubo_transeunte, pos.pandemia$roubo_transeunte, paired = T, exact = F, alternative = "less")
```

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  pre.pandemia$roubo_transeunte and pos.pandemia$roubo_transeunte
    ## V = 669, p-value = 1
    ## alternative hypothesis: true location shift is less than 0

# (Extra) Realizando um teste de hipótese básico para reforçar o conceito

- Um gerente da indústria de telefonia acha que a conta mensal de
  telefone celular dos clientes em média acima de R\$ 52 por mês.

- A empresa deseja testar esta afirmação. (Suponha que o nível de
  significância é de 10%)

  𝐻_0: 𝜃= 52 𝐻_1: 𝜃\> 52

## Gerando amostras artificiais extraídas de uma Normal

Na vida real coletaríamos amostras para testar essa hipótese. Aqui
ilustramos dois casos, dentre os quais o primeiro supõe que coletamos
uma amostra com média acima de 52, enquanto o segundo faz a suposição de
que a amostra coletada tem média inferior a 52

### Primeira amostra (Com média acima de 52)

``` r
cel.acima <- rnorm(5000, mean = 55, sd = 1)
```

### Segunda amostra (Com média abaixo de 52)

``` r
cel.abaixo <- rnorm(5000, mean = 45, sd = 1)
```

### Teste para a primeira amostra (Com média acima de 52)

``` r
t.test(cel.acima, mu = 52, alternative = 'greater', conf.level = 0.9)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  cel.acima
    ## t = 210.17, df = 4999, p-value < 2.2e-16
    ## alternative hypothesis: true mean is greater than 52
    ## 90 percent confidence interval:
    ##  54.95554      Inf
    ## sample estimates:
    ## mean of x 
    ##  54.97367

### Teste para a segunda amostra (Com média abaixo de 52)

``` r
t.test(cel.abaixo, mu = 52, alternative = 'greater', conf.level = 0.9)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  cel.abaixo
    ## t = -494.97, df = 4999, p-value = 1
    ## alternative hypothesis: true mean is greater than 52
    ## 90 percent confidence interval:
    ##  44.98381      Inf
    ## sample estimates:
    ## mean of x 
    ##  45.00193
