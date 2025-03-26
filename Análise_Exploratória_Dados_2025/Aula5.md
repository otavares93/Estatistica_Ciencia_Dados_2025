Análise de dados salariais - Regressão Linear
================
Otto Tavares
2025-03-25

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
library(tidyr)
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
library(stargazer)
```

    ## 
    ## Please cite as: 
    ## 
    ##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
    ##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer

``` r
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
library(plm)
```

    ## 
    ## Attaching package: 'plm'
    ## 
    ## The following object is masked from 'package:data.table':
    ## 
    ##     between
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, lag, lead

# Importando os dados de salário

\##Analisando a matriz de correlação da sub-amostra dos indivíduos que
preencheram a variável de filhos

Vamos filtrar apenas os indivíduos de um determinado setor de uma
empresa que tenham preenchido os dados de filhos no banco de dados. Aqui
é importante destacar, que ao fazer esse filtro, muda-se o espaço
amostral, esses valores não devem ser comparados com as tabelas
anteriores.

``` r
kable(cor(salarios %>% dplyr::filter(!is.na(n_filhos)) %>% dplyr::select(salario, n_filhos, idade_anos)))
```

|            |   salario |  n_filhos | idade_anos |
|:-----------|----------:|----------:|-----------:|
| salario    | 1.0000000 | 0.3580647 |  0.4816920 |
| n_filhos   | 0.3580647 | 1.0000000 |  0.7465385 |
| idade_anos | 0.4816920 | 0.7465385 |  1.0000000 |

É fácil ver que quanto maior a idade dos funcionários maior a quantidade
de filhos. Relação não tão direta quando o assunto são as comparações
entre salário e idade, ou salário e número de filhos.

Podemos a partir daí, contruir um scatterplot entre as variáveis idade e
quantidade de filhos a fim de ver a relação positiva de crescimento
propocional entre as variáveis, como segue:

``` r
salarios %>% dplyr::filter(!is.na(n_filhos)) %>% dplyr::select(idade_anos, n_filhos) %>% ggplot(aes(x=n_filhos, y =idade_anos)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + theme_classic()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Aula5_files/figure-gfm/analisando%20scatter%20entre%20variaveis%20mais%20correlacionadas%20-1.png)<!-- -->

### Rodando a regressao linear sem a variável n_filhos

``` r
#Simples, variavel explicativa idade
modelo.1 <- lm(salario ~ idade_anos, data = salarios)
stargazer(modelo.1, type = 'html')
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>
</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

salario
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

idade_anos
</td>

<td>

0.247<sup>\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.109)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant
</td>

<td>

2.566
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(3.831)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations
</td>

<td>

36
</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>
</td>

<td>

0.132
</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>
</td>

<td>

0.107
</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error
</td>

<td>

4.336 (df = 34)
</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic
</td>

<td>

5.172<sup>\*\*</sup> (df = 1; 34)
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>
</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01
</td>

</tr>

</table>

### Multivariada, com a variável estado civil de controle

``` r
modelo.2 <- lm(salario ~ idade_anos + factor(estado_civil), data = salarios)
stargazer(modelo.2, type = 'html')
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>
</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

salario
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

idade_anos
</td>

<td>

0.233<sup>\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.108)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(estado_civil)solteiro
</td>

<td>

-1.955
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.443)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant
</td>

<td>

3.917
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(3.914)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations
</td>

<td>

36
</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>
</td>

<td>

0.178
</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>
</td>

<td>

0.128
</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error
</td>

<td>

4.284 (df = 33)
</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic
</td>

<td>

3.567<sup>\*\*</sup> (df = 2; 33)
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>
</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01
</td>

</tr>

</table>

### Multivariada, com as variáveis estado civil, grau de instrucao de controle

``` r
modelo.3 <- lm(salario ~ idade_anos + factor(estado_civil) + factor(Grau_de_instrucao), data = salarios)

stargazer(modelo.3, type = 'html')
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>
</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

salario
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

idade_anos
</td>

<td>

0.345<sup>\*\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.071)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(estado_civil)solteiro
</td>

<td>

-1.144
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.951)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(Grau_de_instrucao)ensino médio
</td>

<td>

4.603<sup>\*\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.081)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(Grau_de_instrucao)superior
</td>

<td>

9.779<sup>\*\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.391)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant
</td>

<td>

-4.225
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.886)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations
</td>

<td>

36
</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>
</td>

<td>

0.687
</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>
</td>

<td>

0.647
</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error
</td>

<td>

2.726 (df = 31)
</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic
</td>

<td>

17.024<sup>\*\*\*</sup> (df = 4; 31)
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>
</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01
</td>

</tr>

</table>

### Multivariada, com as variáveis estado civil, grau de instrucao e regiao de controle

``` r
modelo.linear <- lm(salario ~ idade_anos + factor(estado_civil) + factor(Grau_de_instrucao) + factor(regiao), data = salarios)

stargazer(modelo.linear, type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>
</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

salario
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

idade_anos
</td>

<td>

0.351<sup>\*\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.074)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(estado_civil)solteiro
</td>

<td>

-1.052
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.010)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(Grau_de_instrucao)ensino médio
</td>

<td>

4.563<sup>\*\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.113)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(Grau_de_instrucao)superior
</td>

<td>

9.757<sup>\*\*\*</sup>
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.431)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(regiao)interior
</td>

<td>

0.587
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.190)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

factor(regiao)outra
</td>

<td>

-0.019
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.178)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant
</td>

<td>

-4.638
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(3.130)
</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations
</td>

<td>

36
</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>
</td>

<td>

0.691
</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>
</td>

<td>

0.627
</td>

</tr>

<tr>

<td style="text-align:left">

Residual Std. Error
</td>

<td>

2.802 (df = 29)
</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic
</td>

<td>

10.800<sup>\*\*\*</sup> (df = 6; 29)
</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>
</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01
</td>

</tr>

</table>

## Analise dos Residuos

### Modelo com todas as variáveis com excecao de n_filhos

``` r
res <- lm(salario ~ idade_anos + factor(estado_civil) + factor(Grau_de_instrucao)  + factor(regiao), data = salarios)$residuals


### Modelo com todas as variáveis com excecao de n_filhos e regiao
res.escolhido <- lm(salario ~ idade_anos + factor(estado_civil) + factor(Grau_de_instrucao)  , data = salarios)$residuals

ggplot(data.frame(residuos = c(res, res.escolhido), modelo = c(rep('Modelo com todas variáveis', times = length(res)), rep('Modelo escolhido', times = length(res.escolhido)))), aes(x = residuos, group = modelo)) + geom_density() + theme_bw()
```

![](Aula5_files/figure-gfm/residuos%20analise%20-1.png)<!-- -->

## Imputando dados com mice

### Imputando os dados com o pacote mice sem fazer nenhum pós processamento

``` r
imp <- mice(salarios %>% dplyr::mutate(estado_civil = as.factor(estado_civil), Grau_de_instrucao = as.factor(Grau_de_instrucao), regiao = as.factor(regiao)),method = "midastouch",print = FALSE, m = 5, max.iter = 5 , seed = 512) 
```

    ## Warning: Number of logged events: 25

``` r
#fit <- with(data = imp, exp = lm(salario ~idade_anos + factor(n_filhos) +factor(estado_civil) + factor(Grau_de_instrucao) + factor(regiao))) 

fit <- with(data = imp, exp = lm(salario ~idade_anos + n_filhos + factor(estado_civil) + factor(Grau_de_instrucao))) 

round.summary(fit, digits = 4)
```

    ##                                       estimate std.error statistic      df
    ## (Intercept)                            -4.4967    3.0382   -1.4801 26.6105
    ## idade_anos                              0.3561    0.0808    4.4065 24.9343
    ## n_filhos                               -0.1552    0.4801   -0.3232 14.9422
    ## factor(estado_civil)solteiro           -1.0881    0.9917   -1.0972 27.9159
    ## factor(Grau_de_instrucao)ensino médio   4.7949    1.1948    4.0130 24.7780
    ## factor(Grau_de_instrucao)superior       9.8926    1.4799    6.6847 27.5346
    ##                                       p.value    2.5 %  97.5 % conf.low
    ## (Intercept)                            0.1506 -10.7349  1.7414 -10.7349
    ## idade_anos                             0.0002   0.1896  0.5225   0.1896
    ## n_filhos                               0.7510  -1.1787  0.8684  -1.1787
    ## factor(estado_civil)solteiro           0.2819  -3.1199  0.9436  -3.1199
    ## factor(Grau_de_instrucao)ensino médio  0.0005   2.3329  7.2568   2.3329
    ## factor(Grau_de_instrucao)superior      0.0000   6.8589 12.9263   6.8589
    ##                                       conf.high
    ## (Intercept)                              1.7414
    ## idade_anos                               0.5225
    ## n_filhos                                 0.8684
    ## factor(estado_civil)solteiro             0.9436
    ## factor(Grau_de_instrucao)ensino médio    7.2568
    ## factor(Grau_de_instrucao)superior       12.9263

``` r
#est <- pool(fit)
#est %>% tidy() %>% kable()


#fit.glm <- with(data = imp, exp = multinom(factor(n_filhos) ~ salario + idade_anos +factor(estado_civil) + factor(Grau_de_instrucao) + factor(regiao))) 
#round.summary(fit.glm, digits = 4)
```

# Importando os dados de crimes para analisar regressão em painel

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

    ## Rows: 32245 Columns: 63
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr  (3): mes_ano, munic, Regiao
    ## dbl (60): CISP, mes, ano, AISP, RISP, mcirc, hom_doloso, lesao_corp_morte, l...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## `summarise()` has grouped output by 'AISP'. You can override using the `.groups` argument.

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = roubo_transeunte ~ factor(periodo), data = crimes.aisp, 
    ##     effect = "individual", model = "within", index = c("AISP", 
    ##         "mes.ano"))
    ## 
    ## Balanced Panel: n = 39, T = 4, N = 156
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -87.8846 -18.6394   2.6795  14.4183 144.3462 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t-value  Pr(>|t|)    
    ## factor(periodo)1 -39.3333     8.3689 -4.6999 7.335e-06 ***
    ## factor(periodo)2 -54.7692     8.3689 -6.5443 1.775e-09 ***
    ## factor(periodo)3 -52.5128     8.3689 -6.2747 6.510e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    230980
    ## Residual Sum of Squares: 155700
    ## R-Squared:      0.32593
    ## Adj. R-Squared: 0.083503
    ## F-statistic: 18.3741 on 3 and 114 DF, p-value: 8.5798e-10

    ## # A tibble: 4 × 2
    ##   mes.ano    roubo_transeunte_mean
    ##   <IDate>                    <dbl>
    ## 1 2019-12-01                    84
    ## 2 2020-12-01                    56
    ## 3 2021-12-01                    56
    ## 4 2022-12-01                    46

# Importando dados de países e analisando regressão em painel (pib e variáveis socioeconomicas)

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = Log.pib.real ~ educ.adultos + idade.mediana + Log.populacao, 
    ##     data = income, effect = "individual", model = "within", index = c("pais", 
    ##         "ano"))
    ## 
    ## Unbalanced Panel: n = 95, T = 1-8, N = 680
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.956303 -0.104743  0.020445  0.129643  0.527526 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t-value  Pr(>|t|)    
    ## educ.adultos  0.1106074  0.0171298  6.4570 2.254e-10 ***
    ## idade.mediana 0.0676096  0.0064477 10.4859 < 2.2e-16 ***
    ## Log.populacao 0.0349905  0.0575629  0.6079    0.5435    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    57.881
    ## Residual Sum of Squares: 26.407
    ## R-Squared:      0.54377
    ## Adj. R-Squared: 0.46773
    ## F-statistic: 231.221 on 3 and 582 DF, p-value: < 2.22e-16
