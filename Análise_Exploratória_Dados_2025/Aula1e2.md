Visualização da Distribuição de Dados
================
Otto Tavares
2023-02-13

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

![](Aula1e2_files/figure-gfm/descrevendo%20a%20base%20com%20box%20plot%20roubo%20transeunte-1.png)<!-- -->

![](Aula1e2_files/figure-gfm/descrevendo%20a%20base%20com%20box%20plot%20roubo%20celular-1.png)<!-- -->

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

# Discussão da relação entre variáveis quantitativas e qualitativas

![](Aula1e2_files/figure-gfm/descrevendo%20a%20base%20com%20box%20plot%20roubo%20transeunte%20por%20regiao-1.png)<!-- -->

| mes.ano    | r.ao.quadrado |
|:-----------|--------------:|
| 2019-12-01 |     0.4447433 |
| 2020-12-01 |     0.4261336 |
| 2021-12-01 |     0.4370446 |
| 2022-12-01 |     0.4069619 |

# Construindo os histogramas da Aula 1

![](Aula1e2_files/figure-gfm/filtrando%20os%20dados%20e%20visualizando%20dists.-1.png)<!-- -->

# Aplicando estimativa de densidade via kernel, através do kernel de epanechnikov

![](Aula1e2_files/figure-gfm/filtrando%20os%20dados%20e%20visualizando%20dists.%20com%20kernel-1.png)<!-- -->

# Calculando a distribuição acumulada empírica do fenômeno observado

![](Aula1e2_files/figure-gfm/filtrando%20os%20dados%20e%20visualizando%20dists.%20acumuladas-1.png)<!-- -->

    ## # A tibble: 4 × 2
    ##   mes.ano    roubo_transeunte_md
    ##   <IDate>                  <dbl>
    ## 1 2019-12-01                  84
    ## 2 2020-12-01                  56
    ## 3 2021-12-01                  56
    ## 4 2022-12-01                  46

# Calculando a dispersão e as correlações de Pearson

``` r
crimes.aisp %>% ggplot(aes(x = roubo_transeunte, y = roubo_celular)) +geom_point() + facet_wrap(~mes.ano) + stat_cor(method="pearson") + xlab('Roubo a Transeunte') + ylab('Roubo de Celular')
```

![](Aula1e2_files/figure-gfm/calculando%20dispersao%20para%20as%20duas%20datas-1.png)<!-- -->

# Calculando a dispersão e as correlações de Pearson e de Spearman

## Pearson

``` r
crimes.aisp %>% ggplot(aes(x = roubo_transeunte, y = posse_drogas)) +geom_point() + facet_wrap(~mes.ano) + stat_cor(method="pearson") + xlab('Roubo a Transeunte') + ylab('Posse de Drogas') + geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Aula1e2_files/figure-gfm/calculando%20dispersao%20para%20análise%20de%20pearson%20em%20relação%20não%20linear-1.png)<!-- -->

## Spearman

``` r
crimes.aisp %>% ggplot(aes(x = roubo_transeunte, y = posse_drogas)) +geom_point() + facet_wrap(~mes.ano) + stat_cor(method="spearman") + xlab('Roubo a Transeunte') + ylab('Posse de Drogas') + geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Aula1e2_files/figure-gfm/calculando%20dispersao%20para%20análise%20de%20separman%20em%20relação%20não%20linear-1.png)<!-- -->

![](Aula1e2_files/figure-gfm/analise%20temporal%20de%20roubo%20a%20transeunte%20vs%20roubo%20celular%20para%20o%20estado-1.png)<!-- -->

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

![](Aula1e2_files/figure-gfm/analise%20temporal%20de%20roubo%20a%20transeunte%20vs%20roubo%20celular%20para%20um%20dado%20batalhao-1.png)<!-- -->

# Calculando a dispersão e as correlações

![](Aula1e2_files/figure-gfm/calculando%20corrplot%20pearson-1.png)<!-- -->

![](Aula1e2_files/figure-gfm/calculando%20corrplot%20spearman-1.png)<!-- -->
