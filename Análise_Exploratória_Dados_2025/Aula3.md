Dados Faltantes
================
Otto Tavares
2025-03-17

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
library(naniar)
library(mice)
```

    ## Warning in check_dep_version(): ABI version mismatch: 
    ## lme4 was built with Matrix ABI version 0
    ## Current Matrix ABI version is 1
    ## Please re-install lme4 from source or restore original 'Matrix' package

    ## 
    ## Attaching package: 'mice'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

``` r
library(simputation)
```

    ## 
    ## Attaching package: 'simputation'
    ## 
    ## The following object is masked from 'package:naniar':
    ## 
    ##     impute_median

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

    ## Rows: 32245 Columns: 63
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr  (3): mes_ano, munic, Regiao
    ## dbl (60): CISP, mes, ano, AISP, RISP, mcirc, hom_doloso, lesao_corp_morte, l...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Limpando os dados e realizando diagnóstico de dados faltantes

1.  Via diagnose, que já conhecemos, ou miss_var_summary da biblioteca
    naniar. Descrição estatística da distribuição de missing calculando
    as frequências relativas por variável

2.  Via visualização de dados faltantes através de um gráfico de barras
    percentual, gerado pela biblioteca naniar e função vis_miss

3.  Via ordenamento de variáveis com maior frequência de missing

``` r
#1
crimes %>% dlookr::diagnose() %>% kable()
```

| variables | types | missing_count | missing_percent | unique_count | unique_rate |
|:---|:---|---:|---:|---:|---:|
| CISP | numeric | 0 | 0.0000000 | 138 | 0.0042797 |
| mes | numeric | 0 | 0.0000000 | 12 | 0.0003722 |
| ano | numeric | 0 | 0.0000000 | 20 | 0.0006203 |
| mes_ano | character | 0 | 0.0000000 | 240 | 0.0074430 |
| AISP | numeric | 0 | 0.0000000 | 41 | 0.0012715 |
| RISP | numeric | 0 | 0.0000000 | 7 | 0.0002171 |
| munic | character | 0 | 0.0000000 | 90 | 0.0027911 |
| mcirc | numeric | 0 | 0.0000000 | 90 | 0.0027911 |
| Regiao | character | 0 | 0.0000000 | 4 | 0.0001241 |
| hom_doloso | numeric | 0 | 0.0000000 | 40 | 0.0012405 |
| lesao_corp_morte | numeric | 0 | 0.0000000 | 6 | 0.0001861 |
| latrocinio | numeric | 0 | 0.0000000 | 9 | 0.0002791 |
| cvli | numeric | 0 | 0.0000000 | 40 | 0.0012405 |
| hom_por_interv_policial | numeric | 0 | 0.0000000 | 24 | 0.0007443 |
| letalidade_violenta | numeric | 0 | 0.0000000 | 46 | 0.0014266 |
| tentat_hom | numeric | 0 | 0.0000000 | 44 | 0.0013646 |
| lesao_corp_dolosa | numeric | 0 | 0.0000000 | 321 | 0.0099550 |
| estupro | numeric | 0 | 0.0000000 | 37 | 0.0011475 |
| hom_culposo | numeric | 0 | 0.0000000 | 22 | 0.0006823 |
| lesao_corp_culposa | numeric | 0 | 0.0000000 | 195 | 0.0060474 |
| roubo_transeunte | numeric | 0 | 0.0000000 | 370 | 0.0114746 |
| roubo_celular | numeric | 0 | 0.0000000 | 135 | 0.0041867 |
| roubo_em_coletivo | numeric | 0 | 0.0000000 | 130 | 0.0040316 |
| roubo_rua | numeric | 0 | 0.0000000 | 479 | 0.0148550 |
| roubo_veiculo | numeric | 0 | 0.0000000 | 260 | 0.0080633 |
| roubo_carga | numeric | 0 | 0.0000000 | 92 | 0.0028532 |
| roubo_comercio | numeric | 0 | 0.0000000 | 38 | 0.0011785 |
| roubo_residencia | numeric | 0 | 0.0000000 | 17 | 0.0005272 |
| roubo_banco | numeric | 0 | 0.0000000 | 4 | 0.0001241 |
| roubo_cx_eletronico | numeric | 19 | 0.0589239 | 6 | 0.0001861 |
| roubo_conducao_saque | numeric | 0 | 0.0000000 | 9 | 0.0002791 |
| roubo_apos_saque | numeric | 0 | 0.0000000 | 24 | 0.0007443 |
| roubo_bicicleta | numeric | 17389 | 53.9277407 | 10 | 0.0003101 |
| outros_roubos | numeric | 0 | 0.0000000 | 170 | 0.0052721 |
| total_roubos | numeric | 0 | 0.0000000 | 752 | 0.0233214 |
| furto_veiculos | numeric | 0 | 0.0000000 | 111 | 0.0034424 |
| furto_transeunte | numeric | 0 | 0.0000000 | 272 | 0.0084354 |
| furto_coletivo | numeric | 0 | 0.0000000 | 72 | 0.0022329 |
| furto_celular | numeric | 0 | 0.0000000 | 188 | 0.0058304 |
| furto_bicicleta | numeric | 17389 | 53.9277407 | 37 | 0.0011475 |
| outros_furtos | numeric | 0 | 0.0000000 | 371 | 0.0115057 |
| total_furtos | numeric | 0 | 0.0000000 | 630 | 0.0195379 |
| sequestro | numeric | 0 | 0.0000000 | 4 | 0.0001241 |
| extorsao | numeric | 0 | 0.0000000 | 26 | 0.0008063 |
| sequestro_relampago | numeric | 0 | 0.0000000 | 10 | 0.0003101 |
| estelionato | numeric | 0 | 0.0000000 | 301 | 0.0093348 |
| apreensao_drogas | numeric | 0 | 0.0000000 | 150 | 0.0046519 |
| posse_drogas | numeric | 4646 | 14.4084354 | 130 | 0.0040316 |
| trafico_drogas | numeric | 4646 | 14.4084354 | 73 | 0.0022639 |
| apreensao_drogas_sem_autor | numeric | 4646 | 14.4084354 | 48 | 0.0014886 |
| recuperacao_veiculos | numeric | 0 | 0.0000000 | 224 | 0.0069468 |
| apf | numeric | 4646 | 14.4084354 | 130 | 0.0040316 |
| aaapai | numeric | 4646 | 14.4084354 | 58 | 0.0017987 |
| cmp | numeric | 4646 | 14.4084354 | 174 | 0.0053962 |
| cmba | numeric | 4646 | 14.4084354 | 31 | 0.0009614 |
| ameaca | numeric | 0 | 0.0000000 | 313 | 0.0097069 |
| pessoas_desaparecidas | numeric | 0 | 0.0000000 | 39 | 0.0012095 |
| encontro_cadaver | numeric | 0 | 0.0000000 | 16 | 0.0004962 |
| encontro_ossada | numeric | 0 | 0.0000000 | 8 | 0.0002481 |
| pol_militares_mortos_serv | numeric | 0 | 0.0000000 | 5 | 0.0001551 |
| pol_civis_mortos_serv | numeric | 0 | 0.0000000 | 3 | 0.0000930 |
| registro_ocorrencias | numeric | 0 | 0.0000000 | 1885 | 0.0584587 |
| fase | numeric | 0 | 0.0000000 | 2 | 0.0000620 |

``` r
crimes %>% naniar::miss_var_summary() %>% kable()
```

| variable                   | n_miss | pct_miss |
|:---------------------------|-------:|---------:|
| roubo_bicicleta            |  17389 |     53.9 |
| furto_bicicleta            |  17389 |     53.9 |
| posse_drogas               |   4646 |     14.4 |
| trafico_drogas             |   4646 |     14.4 |
| apreensao_drogas_sem_autor |   4646 |     14.4 |
| apf                        |   4646 |     14.4 |
| aaapai                     |   4646 |     14.4 |
| cmp                        |   4646 |     14.4 |
| cmba                       |   4646 |     14.4 |
| roubo_cx_eletronico        |     19 |   0.0589 |
| CISP                       |      0 |        0 |
| mes                        |      0 |        0 |
| ano                        |      0 |        0 |
| mes_ano                    |      0 |        0 |
| AISP                       |      0 |        0 |
| RISP                       |      0 |        0 |
| munic                      |      0 |        0 |
| mcirc                      |      0 |        0 |
| Regiao                     |      0 |        0 |
| hom_doloso                 |      0 |        0 |
| lesao_corp_morte           |      0 |        0 |
| latrocinio                 |      0 |        0 |
| cvli                       |      0 |        0 |
| hom_por_interv_policial    |      0 |        0 |
| letalidade_violenta        |      0 |        0 |
| tentat_hom                 |      0 |        0 |
| lesao_corp_dolosa          |      0 |        0 |
| estupro                    |      0 |        0 |
| hom_culposo                |      0 |        0 |
| lesao_corp_culposa         |      0 |        0 |
| roubo_transeunte           |      0 |        0 |
| roubo_celular              |      0 |        0 |
| roubo_em_coletivo          |      0 |        0 |
| roubo_rua                  |      0 |        0 |
| roubo_veiculo              |      0 |        0 |
| roubo_carga                |      0 |        0 |
| roubo_comercio             |      0 |        0 |
| roubo_residencia           |      0 |        0 |
| roubo_banco                |      0 |        0 |
| roubo_conducao_saque       |      0 |        0 |
| roubo_apos_saque           |      0 |        0 |
| outros_roubos              |      0 |        0 |
| total_roubos               |      0 |        0 |
| furto_veiculos             |      0 |        0 |
| furto_transeunte           |      0 |        0 |
| furto_coletivo             |      0 |        0 |
| furto_celular              |      0 |        0 |
| outros_furtos              |      0 |        0 |
| total_furtos               |      0 |        0 |
| sequestro                  |      0 |        0 |
| extorsao                   |      0 |        0 |
| sequestro_relampago        |      0 |        0 |
| estelionato                |      0 |        0 |
| apreensao_drogas           |      0 |        0 |
| recuperacao_veiculos       |      0 |        0 |
| ameaca                     |      0 |        0 |
| pessoas_desaparecidas      |      0 |        0 |
| encontro_cadaver           |      0 |        0 |
| encontro_ossada            |      0 |        0 |
| pol_militares_mortos_serv  |      0 |        0 |
| pol_civis_mortos_serv      |      0 |        0 |
| registro_ocorrencias       |      0 |        0 |
| fase                       |      0 |        0 |

``` r
#2 - Limitada para bases muito grandes, filtramos apenas para dados de furtos
crimes %>% dplyr::mutate(mes.ano = as.IDate(paste("01", mes, ano, sep = "-"), format = "%d-%m-%Y")) %>% dplyr::select(CISP, mes.ano, AISP, roubo_cx_eletronico:apreensao_drogas_sem_autor) %>% naniar::vis_miss()
```

![](Aula3_files/figure-gfm/filtrando%20os%20dados%20e%20descrevendo%20os%20dados%20faltantes-1.png)<!-- -->

``` r
#3
crimes %>% gg_miss_var()
```

![](Aula3_files/figure-gfm/filtrando%20os%20dados%20e%20descrevendo%20os%20dados%20faltantes-2.png)<!-- -->

# Realizando o teste de Little para checar se os dados faltantes são completamente aleatórios

## Para os dados de furtos e roubos

``` r
crimes %>% dplyr::select(roubo_transeunte:total_furtos) %>% naniar::mcar_test()
```

    ## # A tibble: 1 × 4
    ##   statistic    df p.value missing.patterns
    ##       <dbl> <dbl>   <dbl>            <int>
    ## 1     8147.    39       0                3

## Para os dados de posse de drogas

``` r
crimes %>% dplyr::select(apreensao_drogas:apreensao_drogas_sem_autor) %>% naniar::mcar_test()
```

    ## # A tibble: 1 × 4
    ##   statistic    df p.value missing.patterns
    ##       <dbl> <dbl>   <dbl>            <int>
    ## 1      256.     1       0                2

# Realizando análise da relação entre a distribuição de missing e das variáveis observadas

## Para roubo de bicicleta

``` r
crimes %>%
  dplyr::group_by(AISP, Regiao) %>% dplyr::filter(is.na(roubo_bicicleta)) %>%
  dplyr::summarise(n = n()) %>% dplyr::ungroup() %>% 
  dplyr::mutate(tot.miss = sum(n)) %>% dplyr::group_by(Regiao) %>% 
  dplyr::mutate(tot.miss.regiao = sum(n), freq.intra.regiao = n/tot.miss.regiao, freq.regiao = tot.miss.regiao/tot.miss) %>% dplyr::arrange(desc(freq.regiao), desc(n)) %>% kable()
```

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

| AISP | Regiao | n | tot.miss | tot.miss.regiao | freq.intra.regiao | freq.regiao |
|---:|:---|---:|---:|---:|---:|---:|
| 10 | Interior | 1257 | 17389 | 8477 | 0.1482836 | 0.4874921 |
| 11 | Interior | 1047 | 17389 | 8477 | 0.1235107 | 0.4874921 |
| 25 | Interior | 828 | 17389 | 8477 | 0.0976761 | 0.4874921 |
| 29 | Interior | 701 | 17389 | 8477 | 0.0826943 | 0.4874921 |
| 8 | Interior | 653 | 17389 | 8477 | 0.0770320 | 0.4874921 |
| 36 | Interior | 645 | 17389 | 8477 | 0.0760882 | 0.4874921 |
| 32 | Interior | 592 | 17389 | 8477 | 0.0698360 | 0.4874921 |
| 35 | Interior | 561 | 17389 | 8477 | 0.0661791 | 0.4874921 |
| 33 | Interior | 417 | 17389 | 8477 | 0.0491919 | 0.4874921 |
| 30 | Interior | 408 | 17389 | 8477 | 0.0481302 | 0.4874921 |
| 28 | Interior | 402 | 17389 | 8477 | 0.0474224 | 0.4874921 |
| 38 | Interior | 387 | 17389 | 8477 | 0.0456529 | 0.4874921 |
| 37 | Interior | 303 | 17389 | 8477 | 0.0357438 | 0.4874921 |
| 26 | Interior | 264 | 17389 | 8477 | 0.0311431 | 0.4874921 |
| 34 | Interior | 12 | 17389 | 8477 | 0.0014156 | 0.4874921 |
| 9 | Capital | 714 | 17389 | 5192 | 0.1375193 | 0.2985796 |
| 3 | Capital | 660 | 17389 | 5192 | 0.1271186 | 0.2985796 |
| 6 | Capital | 366 | 17389 | 5192 | 0.0704931 | 0.2985796 |
| 14 | Capital | 366 | 17389 | 5192 | 0.0704931 | 0.2985796 |
| 5 | Capital | 327 | 17389 | 5192 | 0.0629815 | 0.2985796 |
| 16 | Capital | 265 | 17389 | 5192 | 0.0510401 | 0.2985796 |
| 23 | Capital | 265 | 17389 | 5192 | 0.0510401 | 0.2985796 |
| 2 | Capital | 264 | 17389 | 5192 | 0.0508475 | 0.2985796 |
| 18 | Capital | 264 | 17389 | 5192 | 0.0508475 | 0.2985796 |
| 19 | Capital | 264 | 17389 | 5192 | 0.0508475 | 0.2985796 |
| 1 | Capital | 204 | 17389 | 5192 | 0.0392912 | 0.2985796 |
| 39 | Capital | 198 | 17389 | 5192 | 0.0381356 | 0.2985796 |
| 4 | Capital | 192 | 17389 | 5192 | 0.0369800 | 0.2985796 |
| 31 | Capital | 177 | 17389 | 5192 | 0.0340909 | 0.2985796 |
| 27 | Capital | 162 | 17389 | 5192 | 0.0312018 | 0.2985796 |
| 17 | Capital | 132 | 17389 | 5192 | 0.0254237 | 0.2985796 |
| 22 | Capital | 132 | 17389 | 5192 | 0.0254237 | 0.2985796 |
| 41 | Capital | 108 | 17389 | 5192 | 0.0208012 | 0.2985796 |
| 13 | Capital | 99 | 17389 | 5192 | 0.0190678 | 0.2985796 |
| 40 | Capital | 30 | 17389 | 5192 | 0.0057781 | 0.2985796 |
| 36 | Capital | 3 | 17389 | 5192 | 0.0005778 | 0.2985796 |
| 20 | Baixada Fluminense | 695 | 17389 | 2400 | 0.2895833 | 0.1380183 |
| 24 | Baixada Fluminense | 648 | 17389 | 2400 | 0.2700000 | 0.1380183 |
| 15 | Baixada Fluminense | 528 | 17389 | 2400 | 0.2200000 | 0.1380183 |
| 34 | Baixada Fluminense | 300 | 17389 | 2400 | 0.1250000 | 0.1380183 |
| 21 | Baixada Fluminense | 129 | 17389 | 2400 | 0.0537500 | 0.1380183 |
| 40 | Baixada Fluminense | 70 | 17389 | 2400 | 0.0291667 | 0.1380183 |
| 39 | Baixada Fluminense | 30 | 17389 | 2400 | 0.0125000 | 0.1380183 |
| 12 | Grande Niterói | 792 | 17389 | 1320 | 0.6000000 | 0.0759101 |
| 7 | Grande Niterói | 528 | 17389 | 1320 | 0.4000000 | 0.0759101 |

## Para posse de drogas

``` r
crimes %>%
  dplyr::group_by(AISP, Regiao) %>% dplyr::filter(is.na(posse_drogas)) %>%
  dplyr::summarise(n = n()) %>% dplyr::ungroup() %>% 
  dplyr::mutate(tot.miss = sum(n)) %>% dplyr::group_by(Regiao) %>% 
  dplyr::mutate(tot.miss.regiao = sum(n), freq.intra.regiao = n/tot.miss.regiao, freq.regiao = tot.miss.regiao/tot.miss) %>% dplyr::arrange(desc(freq.regiao), desc(n)) %>% kable()
```

    ## `summarise()` has grouped output by 'AISP'. You can override using the
    ## `.groups` argument.

| AISP | Regiao | n | tot.miss | tot.miss.regiao | freq.intra.regiao | freq.regiao |
|---:|:---|---:|---:|---:|---:|---:|
| 10 | Interior | 357 | 4646 | 2249 | 0.1587372 | 0.4840723 |
| 11 | Interior | 327 | 4646 | 2249 | 0.1453980 | 0.4840723 |
| 25 | Interior | 216 | 4646 | 2249 | 0.0960427 | 0.4840723 |
| 29 | Interior | 180 | 4646 | 2249 | 0.0800356 | 0.4840723 |
| 8 | Interior | 173 | 4646 | 2249 | 0.0769231 | 0.4840723 |
| 36 | Interior | 165 | 4646 | 2249 | 0.0733659 | 0.4840723 |
| 32 | Interior | 141 | 4646 | 2249 | 0.0626945 | 0.4840723 |
| 35 | Interior | 141 | 4646 | 2249 | 0.0626945 | 0.4840723 |
| 28 | Interior | 114 | 4646 | 2249 | 0.0506892 | 0.4840723 |
| 33 | Interior | 99 | 4646 | 2249 | 0.0440196 | 0.4840723 |
| 38 | Interior | 99 | 4646 | 2249 | 0.0440196 | 0.4840723 |
| 37 | Interior | 81 | 4646 | 2249 | 0.0360160 | 0.4840723 |
| 26 | Interior | 72 | 4646 | 2249 | 0.0320142 | 0.4840723 |
| 30 | Interior | 72 | 4646 | 2249 | 0.0320142 | 0.4840723 |
| 34 | Interior | 12 | 4646 | 2249 | 0.0053357 | 0.4840723 |
| 9 | Capital | 216 | 4646 | 1401 | 0.1541756 | 0.3015497 |
| 3 | Capital | 180 | 4646 | 1401 | 0.1284797 | 0.3015497 |
| 6 | Capital | 108 | 4646 | 1401 | 0.0770878 | 0.3015497 |
| 14 | Capital | 108 | 4646 | 1401 | 0.0770878 | 0.3015497 |
| 5 | Capital | 75 | 4646 | 1401 | 0.0535332 | 0.3015497 |
| 1 | Capital | 72 | 4646 | 1401 | 0.0513919 | 0.3015497 |
| 2 | Capital | 72 | 4646 | 1401 | 0.0513919 | 0.3015497 |
| 16 | Capital | 72 | 4646 | 1401 | 0.0513919 | 0.3015497 |
| 18 | Capital | 72 | 4646 | 1401 | 0.0513919 | 0.3015497 |
| 19 | Capital | 72 | 4646 | 1401 | 0.0513919 | 0.3015497 |
| 23 | Capital | 72 | 4646 | 1401 | 0.0513919 | 0.3015497 |
| 39 | Capital | 66 | 4646 | 1401 | 0.0471092 | 0.3015497 |
| 4 | Capital | 36 | 4646 | 1401 | 0.0256959 | 0.3015497 |
| 17 | Capital | 36 | 4646 | 1401 | 0.0256959 | 0.3015497 |
| 22 | Capital | 36 | 4646 | 1401 | 0.0256959 | 0.3015497 |
| 27 | Capital | 36 | 4646 | 1401 | 0.0256959 | 0.3015497 |
| 31 | Capital | 36 | 4646 | 1401 | 0.0256959 | 0.3015497 |
| 13 | Capital | 33 | 4646 | 1401 | 0.0235546 | 0.3015497 |
| 36 | Capital | 3 | 4646 | 1401 | 0.0021413 | 0.3015497 |
| 20 | Baixada Fluminense | 215 | 4646 | 636 | 0.3380503 | 0.1368920 |
| 24 | Baixada Fluminense | 168 | 4646 | 636 | 0.2641509 | 0.1368920 |
| 15 | Baixada Fluminense | 144 | 4646 | 636 | 0.2264151 | 0.1368920 |
| 34 | Baixada Fluminense | 72 | 4646 | 636 | 0.1132075 | 0.1368920 |
| 21 | Baixada Fluminense | 33 | 4646 | 636 | 0.0518868 | 0.1368920 |
| 40 | Baixada Fluminense | 4 | 4646 | 636 | 0.0062893 | 0.1368920 |
| 12 | Grande Niterói | 216 | 4646 | 360 | 0.6000000 | 0.0774860 |
| 7 | Grande Niterói | 144 | 4646 | 360 | 0.4000000 | 0.0774860 |

``` r
crimes.aisp <- crimes %>% dplyr::mutate(mes.ano = as.IDate(paste("01", mes, ano, sep = "-"), format = "%d-%m-%Y")) %>% dplyr::group_by(AISP, mes.ano) %>%
  dplyr::summarise_at(vars(apreensao_drogas:apreensao_drogas_sem_autor), sum)

crimes.aisp.imp <- crimes.aisp %>% dplyr::group_by(AISP) %>% dplyr::select(mes.ano, apreensao_drogas:apreensao_drogas_sem_autor) %>% 
  as.data.frame() %>% 
  simputation::impute_knn(. ~ ., seed = 512) %>% dplyr::mutate_at(vars(apreensao_drogas:apreensao_drogas_sem_autor), as.numeric)
```

    ## Adding missing grouping variables: `AISP`

# Comparando a imputação dos dados com o realizado para posse de drogas para o método knn

``` r
ultimo.missing <- crimes.aisp %>% dplyr::select(mes.ano, posse_drogas) %>% dplyr::filter(is.na(posse_drogas)) %>% last() %>% dplyr::select(mes.ano)
```

    ## Adding missing grouping variables: `AISP`
    ## Adding missing grouping variables: `AISP`

``` r
crimes.aisp %>% dplyr::filter((mes.ano < as.IDate("2010-01-01")) & AISP == 23) %>% ggplot(aes(x = mes.ano)) + geom_line(aes(y = posse_drogas, color = "Posse Drogas")) + geom_line(aes(y = apreensao_drogas, color = "Apreensão de Drogas")) + geom_vline(xintercept=c(ultimo.missing$mes.ano)) +  annotate("text", x=ultimo.missing$mes.ano + 40, y=30, label="Fim do período com NA", angle=90)
```

    ## Warning: Removed 36 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](Aula3_files/figure-gfm/comparando%20o%20input%20dos%20dados%20com%20o%20knn%20com%20o%20original-1.png)<!-- -->

``` r
crimes.aisp.imp %>% dplyr::filter((mes.ano < as.IDate("2010-01-01")) & AISP == 23) %>% ggplot(aes(x = mes.ano)) + geom_line(aes(y = posse_drogas, color = "Posse Drogas")) + geom_line(aes(y = apreensao_drogas, color = "Apreensão de Drogas")) + geom_vline(xintercept=c(ultimo.missing$mes.ano)) + annotate("text", x=ultimo.missing$mes.ano + 40, y=30, label="Fim do período com NA", angle=90) + labs(title = "Imputação de dados com KNN para a AISP 23")
```

![](Aula3_files/figure-gfm/comparando%20o%20input%20dos%20dados%20com%20o%20knn%20com%20o%20original-2.png)<!-- -->

# Comparando a imputação dos dados com o realizado para posse de drogas para o método de imputação multipla

``` r
imp.multiplo <- mice(crimes.aisp, print = FALSE, m = 10, max.iter = 5, method = "pmm" , seed   = 512) 

stripplot(imp.multiplo, pch = c(21, 20), cex = c(1, 1.5))
```

![](Aula3_files/figure-gfm/comparando%20o%20input%20dos%20dados%20com%20o%20pmm%20com%20o%20original-1.png)<!-- -->

# Comparando a imputação dos dados com o realizado para os dados de salário para o método de imputação multipla

## Com o método pmm

``` r
salarios <- readxl::read_excel("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2024/dados_auxiliares/dados_bussab_m.xlsx")

imp <- mice(salarios %>% dplyr::mutate(estado_civil = as.factor(estado_civil), Grau_de_instrucao =   as.factor(Grau_de_instrucao), regiao = as.factor(regiao)), print = FALSE, m = 5, max.iter = 5, method = "pmm" , seed   = 512) 
```

    ## Warning: Number of logged events: 25

``` r
names(imp$imp$n_filhos) <- paste("imp", names(imp$imp$n_filhos), sep = "_")
imp_nfilhos <- imp$imp$n_filhos %>% tidyr::pivot_longer(cols = imp_1:ncol(imp$imp$n_filhos), values_to = "n_filhos", names_to   = "imp") %>% arrange(imp)

#Densidade orignal
salarios %>% dplyr::select(n_filhos) %>% ggplot(aes(x=n_filhos, y = after_stat(density))) + geom_density(linetype = 2) + xlab('numero de filhos') + ylab('Densidade de Frequência') + theme_classic()
```

    ## Warning: Removed 16 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20pmm-1.png)<!-- -->

``` r
#Densidade dos inputs
ggplot(imp_nfilhos, aes(x = n_filhos, y = after_stat(density), colour = imp)) + geom_density(linetype = 2) + xlab('numero de    filhos') + ylab('Densidade de Frequência') + theme_classic()
```

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20pmm-2.png)<!-- -->

``` r
#preenchimento dos dados
stripplot(imp, pch = c(21, 20), cex = c(1, 1.5))
```

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20pmm-3.png)<!-- -->

## Com o método midastouch (pmm ponderado)

``` r
salarios <- readxl::read_excel("/Users/ottotavares/Library/Mobile Documents/com~apple~CloudDocs/Documents/infnet/Estatistica_Ciencia_Dados_2024/dados_auxiliares/dados_bussab_m.xlsx")

imp <- mice(salarios %>% dplyr::mutate(estado_civil = as.factor(estado_civil), Grau_de_instrucao =   as.factor(Grau_de_instrucao), regiao = as.factor(regiao)), print = FALSE, m = 5, max.iter = 5, method = "midastouch" , seed   = 512) 
```

    ## Warning: Number of logged events: 25

``` r
names(imp$imp$n_filhos) <- paste("imp", names(imp$imp$n_filhos), sep = "_")
imp_nfilhos <- imp$imp$n_filhos %>% tidyr::pivot_longer(cols = imp_1:ncol(imp$imp$n_filhos), values_to = "n_filhos", names_to   = "imp") %>% arrange(imp)

#Densidade orignal
salarios %>% dplyr::select(n_filhos) %>% ggplot(aes(x=n_filhos, y = after_stat(density))) + geom_density(linetype = 2) + xlab('numero de filhos') + ylab('Densidade de Frequência') + theme_classic()
```

    ## Warning: Removed 16 rows containing non-finite outside the scale range
    ## (`stat_density()`).

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20midastouch-1.png)<!-- -->

``` r
#Densidade dos inputs
ggplot(imp_nfilhos, aes(x = n_filhos, y = after_stat(density), colour = imp)) + geom_density(linetype = 2) + xlab('numero de    filhos') + ylab('Densidade de Frequência') + theme_classic()
```

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20midastouch-2.png)<!-- -->

``` r
#preenchimento dos dados
stripplot(imp, pch = c(21, 20), cex = c(1, 1.5))
```

![](Aula3_files/figure-gfm/multiplo%20input%20para%20os%20dados%20de%20salario%20midastouch-3.png)<!-- -->
