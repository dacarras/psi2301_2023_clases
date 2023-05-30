ANOVA para tres grupos y comparaciones múltiples
================
dacarras

# Taller 8: ANOVA para tres grupos

## Cargar datos

``` r
#----------------------------------------------------------
# cargar datos
#----------------------------------------------------------

#--------------------------------------
# cargar dplyr
#--------------------------------------

library(dplyr)

#--------------------------------------
# datos Vik (2014, p161)
#--------------------------------------

table_13_1 <- read.table(text = "
person  score   drug
      1     3   none
      2     5   none
      3     6   none
      4     8   none
      5     9   alcohol
      6    11   alcohol
      7    19   alcohol
      8    15   alcohol
      9    16   poly_drug
     10    16   poly_drug
     11    19   poly_drug
     12    17   poly_drug
     13     3   none
     14     5   none
     15     4   none
     16     6   none
     17     8   alcohol
     18    10   alcohol
     19    24   alcohol
     20    24   alcohol
     21    22   poly_drug
     22    23   poly_drug
     23    19   poly_drug
     24    20   poly_drug
", header = TRUE, stringsAsFactors = FALSE)

#--------------------------------------
# mostrar datos
#--------------------------------------

knitr::kable(table_13_1, digits = 3)
```

| person | score | drug      |
|-------:|------:|:----------|
|      1 |     3 | none      |
|      2 |     5 | none      |
|      3 |     6 | none      |
|      4 |     8 | none      |
|      5 |     9 | alcohol   |
|      6 |    11 | alcohol   |
|      7 |    19 | alcohol   |
|      8 |    15 | alcohol   |
|      9 |    16 | poly_drug |
|     10 |    16 | poly_drug |
|     11 |    19 | poly_drug |
|     12 |    17 | poly_drug |
|     13 |     3 | none      |
|     14 |     5 | none      |
|     15 |     4 | none      |
|     16 |     6 | none      |
|     17 |     8 | alcohol   |
|     18 |    10 | alcohol   |
|     19 |    24 | alcohol   |
|     20 |    24 | alcohol   |
|     21 |    22 | poly_drug |
|     22 |    23 | poly_drug |
|     23 |    19 | poly_drug |
|     24 |    20 | poly_drug |

## Descriptivos

``` r
#----------------------------------------------------------
# descriptivos
#----------------------------------------------------------

#--------------------------------------
# casos por grupo
#--------------------------------------

xtabs(~ drug, data = table_13_1)
```

    ## drug
    ##   alcohol      none poly_drug 
    ##         8         8         8

``` r
#--------------------------------------
# casos por grupo
#--------------------------------------

dplyr::count(table_13_1, drug)
```

    ##        drug n
    ## 1   alcohol 8
    ## 2      none 8
    ## 3 poly_drug 8

``` r
#--------------------------------------
# descriptivos por uso de drogas
#--------------------------------------

library(dplyr)
table_13_1 %>%
group_by(drug) %>%
summarize(
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE),
    n = n()
    ) %>%
knitr::kable(., digits = 2)
```

| drug      | mean |   sd |   n |
|:----------|-----:|-----:|----:|
| alcohol   |   15 | 6.59 |   8 |
| none      |    5 | 1.69 |   8 |
| poly_drug |   19 | 2.62 |   8 |

``` r
#--------------------------------------
# medias con intervalo de confianza
#--------------------------------------

library(dplyr)
library(srvyr)
data_13_srs <- table_13_1 %>% 
                as_survey_design(ids = 1)


data_13_srs %>%
group_by(drug) %>%
summarize(
  score = survey_mean(score, vartype = c('ci', 'se'))
) %>%
knitr::kable(., digits = 2)
```

| drug      | score | score_low | score_upp | score_se |
|:----------|------:|----------:|----------:|---------:|
| alcohol   |    15 |     10.39 |     19.61 |     2.23 |
| none      |     5 |      3.82 |      6.18 |     0.57 |
| poly_drug |    19 |     17.17 |     20.83 |     0.88 |

``` r
#--------------------------------------
# medias con intervalo de confianza
#--------------------------------------

data_plot <- data_13_srs %>%
group_by(drug) %>%
summarize(
  score = survey_mean(score, vartype = c('ci', 'se'))
)


library(ggplot2)
data_plot %>%
arrange(desc(score)) %>%
mutate(drug_lab = forcats::as_factor(drug)) %>%
ggplot(., aes(drug_lab, score)) +
geom_point(size = 3) +
geom_linerange(aes(ymin = score_low, ymax = score_upp), size = 2) +
ylab('') +
xlab('') +
scale_y_continuous(breaks = seq(0, 20, by = 2)) +
coord_flip() +
theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1)
    )
```

![](c12_m01_comparaciones_multiples_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## ANOVA por pasos

``` r
#----------------------------------------------------------
# cargar datos
#----------------------------------------------------------

#--------------------------------------
# medias de clusters
#--------------------------------------

c_mean <- function (x, j) {
    ave(x, j, FUN = function(x) mean(x, na.rm = T))
}

#--------------------------------------
# crear componentes
#--------------------------------------

data_model <- table_13_1 %>%
              mutate(all = 1) %>%
              # between
              ## gran media
              mutate(bet_g = c_mean(score, all))  %>%
              ## media de los grupos
              mutate(bet_c = c_mean(score, drug)) %>%
              ## desviaciones de las medias de los grupos
              mutate(bet_w = bet_c - bet_g)       %>%
              ## distancias cuadráticas (varianza entre)
              mutate(bet_s = bet_w^2)             %>%
              # within
              ## media de los grupos
              mutate(wit_c = c_mean(score, drug)) %>%
              ## desviaciones de las observaciones,
              #  respecto a las medias de los grupos
              mutate(wit_w = score - wit_c)      %>%
              ## distancias cuadráticas (varianza intra)
              mutate(wit_s = wit_w^2)            %>%              
              ## desviaciones de las observaciones,
              #  respecto a las medias de los grupos
              mutate(tot_w = score - bet_g) %>%
              mutate(tot_s = tot_w^2) %>%
              dplyr::glimpse()
```

    ## Rows: 24
    ## Columns: 13
    ## $ person <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, …
    ## $ score  <int> 3, 5, 6, 8, 9, 11, 19, 15, 16, 16, 19, 17, 3, 5, 4, 6, 8, 10, 2…
    ## $ drug   <chr> "none", "none", "none", "none", "alcohol", "alcohol", "alcohol"…
    ## $ all    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ bet_g  <dbl> 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,…
    ## $ bet_c  <dbl> 5, 5, 5, 5, 15, 15, 15, 15, 19, 19, 19, 19, 5, 5, 5, 5, 15, 15,…
    ## $ bet_w  <dbl> -8, -8, -8, -8, 2, 2, 2, 2, 6, 6, 6, 6, -8, -8, -8, -8, 2, 2, 2…
    ## $ bet_s  <dbl> 64, 64, 64, 64, 4, 4, 4, 4, 36, 36, 36, 36, 64, 64, 64, 64, 4, …
    ## $ wit_c  <dbl> 5, 5, 5, 5, 15, 15, 15, 15, 19, 19, 19, 19, 5, 5, 5, 5, 15, 15,…
    ## $ wit_w  <dbl> -2, 0, 1, 3, -6, -4, 4, 0, -3, -3, 0, -2, -2, 0, -1, 1, -7, -5,…
    ## $ wit_s  <dbl> 4, 0, 1, 9, 36, 16, 16, 0, 9, 9, 0, 4, 4, 0, 1, 1, 49, 25, 81, …
    ## $ tot_w  <dbl> -10, -8, -7, -5, -4, -2, 6, 2, 3, 3, 6, 4, -10, -8, -9, -7, -5,…
    ## $ tot_s  <dbl> 100, 64, 49, 25, 16, 4, 36, 4, 9, 9, 36, 16, 100, 64, 81, 49, 2…

``` r
#--------------------------------------
# mostrar datos
#--------------------------------------

data_model %>%
arrange(drug) %>%
knitr::kable(., digits = 3)
```

| person | score | drug      | all | bet_g | bet_c | bet_w | bet_s | wit_c | wit_w | wit_s | tot_w | tot_s |
|-------:|------:|:----------|----:|------:|------:|------:|------:|------:|------:|------:|------:|------:|
|      5 |     9 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |    -6 |    36 |    -4 |    16 |
|      6 |    11 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |    -4 |    16 |    -2 |     4 |
|      7 |    19 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |     4 |    16 |     6 |    36 |
|      8 |    15 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |     0 |     0 |     2 |     4 |
|     17 |     8 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |    -7 |    49 |    -5 |    25 |
|     18 |    10 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |    -5 |    25 |    -3 |     9 |
|     19 |    24 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |     9 |    81 |    11 |   121 |
|     20 |    24 | alcohol   |   1 |    13 |    15 |     2 |     4 |    15 |     9 |    81 |    11 |   121 |
|      1 |     3 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |    -2 |     4 |   -10 |   100 |
|      2 |     5 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |     0 |     0 |    -8 |    64 |
|      3 |     6 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |     1 |     1 |    -7 |    49 |
|      4 |     8 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |     3 |     9 |    -5 |    25 |
|     13 |     3 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |    -2 |     4 |   -10 |   100 |
|     14 |     5 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |     0 |     0 |    -8 |    64 |
|     15 |     4 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |    -1 |     1 |    -9 |    81 |
|     16 |     6 | none      |   1 |    13 |     5 |    -8 |    64 |     5 |     1 |     1 |    -7 |    49 |
|      9 |    16 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |    -3 |     9 |     3 |     9 |
|     10 |    16 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |    -3 |     9 |     3 |     9 |
|     11 |    19 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |     0 |     0 |     6 |    36 |
|     12 |    17 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |    -2 |     4 |     4 |    16 |
|     21 |    22 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |     3 |     9 |     9 |    81 |
|     22 |    23 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |     4 |    16 |    10 |   100 |
|     23 |    19 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |     0 |     0 |     6 |    36 |
|     24 |    20 | poly_drug |   1 |    13 |    19 |     6 |    36 |    19 |     1 |     1 |     7 |    49 |

``` r
#--------------------------------------
# ortogonalidad
#--------------------------------------

with(data_model, cor(bet_w, wit_w))
```

    ## [1] 0

``` r
#--------------------------------------
# ortogonalidad
#--------------------------------------

ssb <- sum(data_model$bet_s)

ssw <- sum(data_model$wit_s)

sst <- sum(data_model$tot_s)

#--------------------------------------
# grados de libertad
#--------------------------------------

n_groups <- 3   # tenemos 3 grupos
 
n_total  <- 24  # tenemos 24 observaciones

df_b <- n_groups - 1 

df_w <- (n_total - 1) - (n_groups - 1)

df_t <- df_b + df_w

#--------------------------------------
# alternativamente grados de libertad
#--------------------------------------

df_b <- n_groups - 1 

df_w <- n_total - n_groups

df_t <- n_total - 1


#--------------------------------------
# mean squares
#--------------------------------------

ms_b <- ssb/df_b

ms_w <- ssw/df_w


#--------------------------------------
# F
#--------------------------------------

f_value <- ms_b/ms_w


#--------------------------------------
# P value
#--------------------------------------

p_value <- pf(f_value, df1 = df_b, df2 = df_w, lower.tail = FALSE)
p_value
```

    ## [1] 0.000004406794

``` r
#--------------------------------------
# Tabla F (ver Vik, 2014, p165)
#--------------------------------------

options(scipen = 9999)
options(digits = 8)

tibble::tribble(
  ~source,        ~ss,    ~df,    ~ms,       ~F,      ~p,
  "Drug abuse",   ssb,   df_b,   ms_b,  f_value,      p_value,
  "Within"    ,   ssw,   df_w,   ms_w,       NA,      NA,
  "Total"     ,   sst,   df_t,     NA,       NA,      NA
) %>%
knitr::kable(., digits = 6)
```

| source     |   ss |  df |         ms |         F |        p |
|:-----------|-----:|----:|-----------:|----------:|---------:|
| Drug abuse |  832 |   2 | 416.000000 | 23.483871 | 0.000004 |
| Within     |  372 |  21 |  17.714286 |           |          |
| Total      | 1204 |  23 |            |           |          |

``` r
#--------------------------------------
# tabla de F de ANOVA
#--------------------------------------

table_13_1 %>%
aov(score ~ drug, data = .) %>%
summary()
```

    ##             Df Sum Sq Mean Sq F value      Pr(>F)    
    ## drug         2    832  416.00  23.484 0.000004407 ***
    ## Residuals   21    372   17.71                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# ANOVA enfoque tradicional

``` r
#----------------------------------------------------------
# ANOVA
#----------------------------------------------------------

#--------------------------------------
# anova two factors
#--------------------------------------

table_13_1 %>%
aov(score ~ drug, data = .) %>%
summary()
```

    ##             Df Sum Sq Mean Sq F value      Pr(>F)    
    ## drug         2    832  416.00  23.484 0.000004407 ***
    ## Residuals   21    372   17.71                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Diferencias de medias entre grupos

## Comparaciones via re-parametrización del modelo de regresión

``` r
#----------------------------------------------------------
# ANOVA via regresión
#----------------------------------------------------------

#--------------------------------------
# preparar datos
#--------------------------------------

data_model <- table_13_1 %>%
              mutate(drug_1 = case_when(
                drug == 'none'      ~  1,
                drug == 'alcohol'   ~  0,
                drug == 'poly_drug' ~  0
                )) %>%
              mutate(drug_2 = case_when(
                drug == 'none'      ~  0,
                drug == 'alcohol'   ~  1,
                drug == 'poly_drug' ~  0
                )) %>%
              mutate(drug_3 = case_when(
                drug == 'none'      ~  0,
                drug == 'alcohol'   ~  0,
                drug == 'poly_drug' ~  1
                )) %>%
              mutate(grand_mean = mean(score, na.rm = TRUE)) %>%
              mutate(center = score - grand_mean) %>%
              dplyr::glimpse()
```

    ## Rows: 24
    ## Columns: 8
    ## $ person     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    ## $ score      <int> 3, 5, 6, 8, 9, 11, 19, 15, 16, 16, 19, 17, 3, 5, 4, 6, 8, 1…
    ## $ drug       <chr> "none", "none", "none", "none", "alcohol", "alcohol", "alco…
    ## $ drug_1     <dbl> 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,…
    ## $ drug_2     <dbl> 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,…
    ## $ drug_3     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ grand_mean <dbl> 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,…
    ## $ center     <dbl> -10, -8, -7, -5, -4, -2, 6, 2, 3, 3, 6, 4, -10, -8, -9, -7,…

``` r
#--------------------------------------
# todos los modelos posibles
#--------------------------------------

# model 1: none
lm(score ~ drug_2 + drug_3, data = data_model) %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ drug_2 + drug_3, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value    Pr(>|t|)    
    ## (Intercept)   5.0000     1.4880  3.3601   0.0029626 ** 
    ## drug_2       10.0000     2.1044  4.7519   0.0001079 ***
    ## drug_3       14.0000     2.1044  6.6527 0.000001384 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.6616 
    ## F-statistic: 23.484 on 2 and 21 DF,  p-value: 0.0000044068

``` r
# model 2: alcohol
lm(score ~ drug_1 + drug_3, data = data_model) %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ drug_1 + drug_3, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value       Pr(>|t|)    
    ## (Intercept)  15.0000     1.4880 10.0803 0.000000001684 ***
    ## drug_1      -10.0000     2.1044 -4.7519      0.0001079 ***
    ## drug_3        4.0000     2.1044  1.9008      0.0711470 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.6616 
    ## F-statistic: 23.484 on 2 and 21 DF,  p-value: 0.0000044068

``` r
# model 3: poly_drug
lm(score ~ drug_1 + drug_2, data = data_model) %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = score ~ drug_1 + drug_2, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value         Pr(>|t|)    
    ## (Intercept)  19.0000     1.4880 12.7684 0.00000000002299 ***
    ## drug_1      -14.0000     2.1044 -6.6527 0.00000138373510 ***
    ## drug_2       -4.0000     2.1044 -1.9008          0.07115 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.6616 
    ## F-statistic: 23.484 on 2 and 21 DF,  p-value: 0.0000044068

``` r
#--------------------------------------
# modelos ajustados como objetos
#--------------------------------------

# model 1: none
m01 <- lm(score ~ drug_2 + drug_3, data = data_model)

# model 2: alcohol
m02 <- lm(score ~ drug_1 + drug_3, data = data_model)


# model 3: poly_drug
m03 <- lm(score ~ drug_1 + drug_2, data = data_model)

#--------------------------------------
# tabla de modelos ajustados
#--------------------------------------

texreg::screenreg(
    list(m01, m02, m03),
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row = FALSE
    )
```

    ## 
    ## ==============================================
    ##              Model 1    Model 2     Model 3   
    ## ----------------------------------------------
    ## (Intercept)   5.00 **    15.00 ***   19.00 ***
    ##              (1.49)      (1.49)      (1.49)   
    ## drug_2       10.00 ***               -4.00    
    ##              (2.10)                  (2.10)   
    ## drug_3       14.00 ***    4.00                
    ##              (2.10)      (2.10)               
    ## drug_1                  -10.00 ***  -14.00 ***
    ##                          (2.10)      (2.10)   
    ## ----------------------------------------------
    ## R^2           0.69        0.69        0.69    
    ## Adj. R^2      0.66        0.66        0.66    
    ## Num. obs.    24          24          24       
    ## ==============================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05

# Comparaciones múltiples con correcciones

## LSD: least statistical difference

``` r
#--------------------------------------
# least statistical difference
#--------------------------------------

aov(score ~ drug, data = table_13_1) %>%
agricolae::LSD.test(., 'drug') %>%
print()
```

    ## $statistics
    ##     MSerror Df Mean        CV   t.value      LSD
    ##   17.714286 21   13 32.375648 2.0796138 4.376375
    ## 
    ## $parameters
    ##         test p.ajusted name.t ntr alpha
    ##   Fisher-LSD      none   drug   3  0.05
    ## 
    ## $means
    ##           score       std r        LCL        UCL Min Max   Q25 Q50   Q75
    ## alcohol      15 6.5900358 8 11.9054356 18.0945644   8  24  9.75  13 20.25
    ## none          5 1.6903085 8  1.9054356  8.0945644   3   8  3.75   5  6.00
    ## poly_drug    19 2.6186147 8 15.9054356 22.0945644  16  23 16.75  19 20.50
    ## 
    ## $comparison
    ## NULL
    ## 
    ## $groups
    ##           score groups
    ## poly_drug    19      a
    ## alcohol      15      a
    ## none          5      b
    ## 
    ## attr(,"class")
    ## [1] "group"

## Tukey HSD

``` r
#--------------------------------------
# tukey with base
#--------------------------------------

anova_model <- aov(score ~ drug, data = data_model)
TukeyHSD(x=anova_model, 'drug', conf.level=0.95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = score ~ drug, data = data_model)
    ## 
    ## $drug
    ##                   diff         lwr        upr      p adj
    ## none-alcohol       -10 -15.3043312 -4.6956688 0.00030562
    ## poly_drug-alcohol    4  -1.3043312  9.3043312 0.16322078
    ## poly_drug-none      14   8.6956688 19.3043312 0.00000401

``` r
#--------------------------------------
# tukey with multcomp
#--------------------------------------

library(multcomp)
data_model <- data_model %>%
              mutate(drug_fct = as.factor(drug))

anova_model <- aov(score ~ drug_fct, data = data_model)
post_hocs <- glht(anova_model, linfct = mcp(drug_fct = "Tukey"))
summary(post_hocs)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: aov(formula = score ~ drug_fct, data = data_model)
    ## 
    ## Linear Hypotheses:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## none - alcohol == 0      -10.0000     2.1044 -4.7519   <0.001 ***
    ## poly_drug - alcohol == 0   4.0000     2.1044  1.9008   0.1632    
    ## poly_drug - none == 0     14.0000     2.1044  6.6527   <0.001 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)

## Bonferroni

``` r
#--------------------------------------
# bonferroni
#--------------------------------------

rstatix::emmeans_test(
score ~ drug, 
data = data_model,
p.adjust.method = "bonferroni"
) %>%
knitr::kable(., digits = 2)
```

| term | .y.   | group1  | group2    |  df | statistic |    p | p.adj | p.adj.signif |
|:-----|:------|:--------|:----------|----:|----------:|-----:|------:|:-------------|
| drug | score | alcohol | none      |  21 |      4.75 | 0.00 |  0.00 | \*\*\*       |
| drug | score | alcohol | poly_drug |  21 |     -1.90 | 0.07 |  0.21 | ns           |
| drug | score | none    | poly_drug |  21 |     -6.65 | 0.00 |  0.00 | \*\*\*\*     |

## Holm

``` r
#--------------------------------------
# holm
#--------------------------------------

rstatix::emmeans_test(
score ~ drug, 
data = data_model,
p.adjust.method = "holm"
) %>%
knitr::kable(., digits = 2)
```

| term | .y.   | group1  | group2    |  df | statistic |    p | p.adj | p.adj.signif |
|:-----|:------|:--------|:----------|----:|----------:|-----:|------:|:-------------|
| drug | score | alcohol | none      |  21 |      4.75 | 0.00 |  0.00 | \*\*\*       |
| drug | score | alcohol | poly_drug |  21 |     -1.90 | 0.07 |  0.07 | ns           |
| drug | score | none    | poly_drug |  21 |     -6.65 | 0.00 |  0.00 | \*\*\*\*     |

## FDR

``` r
#--------------------------------------
# fdr
#--------------------------------------

rstatix::emmeans_test(
score ~ drug, 
data = data_model,
p.adjust.method = "fdr"
) %>%
knitr::kable(., digits = 2)
```

| term | .y.   | group1  | group2    |  df | statistic |    p | p.adj | p.adj.signif |
|:-----|:------|:--------|:----------|----:|----------:|-----:|------:|:-------------|
| drug | score | alcohol | none      |  21 |      4.75 | 0.00 |  0.00 | \*\*\*       |
| drug | score | alcohol | poly_drug |  21 |     -1.90 | 0.07 |  0.07 | ns           |
| drug | score | none    | poly_drug |  21 |     -6.65 | 0.00 |  0.00 | \*\*\*\*     |

# ANOVA via regresión

``` r
#--------------------------------------
# mostrar datos
#--------------------------------------

knitr::kable(data_model)
```

| person | score | drug      | drug_1 | drug_2 | drug_3 | grand_mean | center | drug_fct  |
|-------:|------:|:----------|-------:|-------:|-------:|-----------:|-------:|:----------|
|      1 |     3 | none      |      1 |      0 |      0 |         13 |    -10 | none      |
|      2 |     5 | none      |      1 |      0 |      0 |         13 |     -8 | none      |
|      3 |     6 | none      |      1 |      0 |      0 |         13 |     -7 | none      |
|      4 |     8 | none      |      1 |      0 |      0 |         13 |     -5 | none      |
|      5 |     9 | alcohol   |      0 |      1 |      0 |         13 |     -4 | alcohol   |
|      6 |    11 | alcohol   |      0 |      1 |      0 |         13 |     -2 | alcohol   |
|      7 |    19 | alcohol   |      0 |      1 |      0 |         13 |      6 | alcohol   |
|      8 |    15 | alcohol   |      0 |      1 |      0 |         13 |      2 | alcohol   |
|      9 |    16 | poly_drug |      0 |      0 |      1 |         13 |      3 | poly_drug |
|     10 |    16 | poly_drug |      0 |      0 |      1 |         13 |      3 | poly_drug |
|     11 |    19 | poly_drug |      0 |      0 |      1 |         13 |      6 | poly_drug |
|     12 |    17 | poly_drug |      0 |      0 |      1 |         13 |      4 | poly_drug |
|     13 |     3 | none      |      1 |      0 |      0 |         13 |    -10 | none      |
|     14 |     5 | none      |      1 |      0 |      0 |         13 |     -8 | none      |
|     15 |     4 | none      |      1 |      0 |      0 |         13 |     -9 | none      |
|     16 |     6 | none      |      1 |      0 |      0 |         13 |     -7 | none      |
|     17 |     8 | alcohol   |      0 |      1 |      0 |         13 |     -5 | alcohol   |
|     18 |    10 | alcohol   |      0 |      1 |      0 |         13 |     -3 | alcohol   |
|     19 |    24 | alcohol   |      0 |      1 |      0 |         13 |     11 | alcohol   |
|     20 |    24 | alcohol   |      0 |      1 |      0 |         13 |     11 | alcohol   |
|     21 |    22 | poly_drug |      0 |      0 |      1 |         13 |      9 | poly_drug |
|     22 |    23 | poly_drug |      0 |      0 |      1 |         13 |     10 | poly_drug |
|     23 |    19 | poly_drug |      0 |      0 |      1 |         13 |      6 | poly_drug |
|     24 |    20 | poly_drug |      0 |      0 |      1 |         13 |      7 | poly_drug |

``` r
#--------------------------------------
# formulas
#--------------------------------------

f01 <- as.formula(score  ~ + 1)
f02 <- as.formula(score  ~ + 1 + drug_2 + drug_3)
f03 <- as.formula(score  ~ + 1 + drug_1 + drug_2)
f04 <- as.formula(score  ~ + 1 + drug_1 + drug_3)
f05 <- as.formula(score  ~ 0 + drug_1 + + drug_2 + drug_3)
f06 <- as.formula(center ~ 0 + drug_1 + + drug_2 + drug_3)

#--------------------------------------
# ajustar modelos
#--------------------------------------

m01 <- lm(f01, data = data_model)
m02 <- lm(f02, data = data_model)
m03 <- lm(f03, data = data_model)
m04 <- lm(f04, data = data_model)
m05 <- lm(f05, data = data_model)
m06 <- lm(f06, data = data_model)

#--------------------------------------
# resumen de cada modelo
#--------------------------------------

summary(m01) # modelo nulo
```

    ## 
    ## Call:
    ## lm(formula = f01, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##    -10     -7      0      6     11 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value      Pr(>|t|)    
    ## (Intercept)  13.0000     1.4769  8.8024 0.00000000801 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.2352 on 23 degrees of freedom

``` r
summary(m02) # modelo donde "none" es el intercepto
```

    ## 
    ## Call:
    ## lm(formula = f02, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value    Pr(>|t|)    
    ## (Intercept)   5.0000     1.4880  3.3601   0.0029626 ** 
    ## drug_2       10.0000     2.1044  4.7519   0.0001079 ***
    ## drug_3       14.0000     2.1044  6.6527 0.000001384 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.6616 
    ## F-statistic: 23.484 on 2 and 21 DF,  p-value: 0.0000044068

``` r
summary(m03) # modelo donde "alcholo" es el intercepto
```

    ## 
    ## Call:
    ## lm(formula = f03, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value         Pr(>|t|)    
    ## (Intercept)  19.0000     1.4880 12.7684 0.00000000002299 ***
    ## drug_1      -14.0000     2.1044 -6.6527 0.00000138373510 ***
    ## drug_2       -4.0000     2.1044 -1.9008          0.07115 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.6616 
    ## F-statistic: 23.484 on 2 and 21 DF,  p-value: 0.0000044068

``` r
summary(m04) # modelo donde "poly_drug" es el intercepto
```

    ## 
    ## Call:
    ## lm(formula = f04, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value       Pr(>|t|)    
    ## (Intercept)  15.0000     1.4880 10.0803 0.000000001684 ***
    ## drug_1      -10.0000     2.1044 -4.7519      0.0001079 ***
    ## drug_3        4.0000     2.1044  1.9008      0.0711470 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.6616 
    ## F-statistic: 23.484 on 2 and 21 DF,  p-value: 0.0000044068

``` r
summary(m05) # modelo sin intercepto
```

    ## 
    ## Call:
    ## lm(formula = f05, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##        Estimate Std. Error t value         Pr(>|t|)    
    ## drug_1    5.000      1.488  3.3601         0.002963 ** 
    ## drug_2   15.000      1.488 10.0803 0.00000000168385 ***
    ## drug_3   19.000      1.488 12.7684 0.00000000002299 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.92928,    Adjusted R-squared:  0.91917 
    ## F-statistic: 91.978 on 3 and 21 DF,  p-value: 0.0000000000030475

``` r
summary(m06) # modelo emulando a una prueba t de una muestra
```

    ## 
    ## Call:
    ## lm(formula = f06, data = data_model)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7.00  -2.25   0.00   1.50   9.00 
    ## 
    ## Coefficients:
    ##        Estimate Std. Error t value   Pr(>|t|)    
    ## drug_1   -8.000      1.488 -5.3762 0.00002479 ***
    ## drug_2    2.000      1.488  1.3440  0.1932761    
    ## drug_3    6.000      1.488  4.0321  0.0006017 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.2088 on 21 degrees of freedom
    ## Multiple R-squared:  0.69103,    Adjusted R-squared:  0.64689 
    ## F-statistic: 15.656 on 3 and 21 DF,  p-value: 0.000014131

``` r
#--------------------------------------
# tabla de modelos
#--------------------------------------

texreg::screenreg(
    list(m01, m02, m03, m04, m05, m06),
    star.symbol = "*", 
    center = TRUE, 
    doctype = FALSE,
    dcolumn = TRUE, 
    booktabs = TRUE,
    single.row = FALSE
    )
```

    ## 
    ## ===============================================================================
    ##              Model 1    Model 2    Model 3     Model 4     Model 5    Model 6  
    ## -------------------------------------------------------------------------------
    ## (Intercept)  13.00 ***   5.00 **    19.00 ***   15.00 ***                      
    ##              (1.48)     (1.49)      (1.49)      (1.49)                         
    ## drug_2                  10.00 ***   -4.00                  15.00 ***   2.00    
    ##                         (2.10)      (2.10)                 (1.49)     (1.49)   
    ## drug_3                  14.00 ***                4.00      19.00 ***   6.00 ***
    ##                         (2.10)                  (2.10)     (1.49)     (1.49)   
    ## drug_1                             -14.00 ***  -10.00 ***   5.00 **   -8.00 ***
    ##                                     (2.10)      (2.10)     (1.49)     (1.49)   
    ## -------------------------------------------------------------------------------
    ## R^2           0.00       0.69        0.69        0.69       0.93       0.69    
    ## Adj. R^2      0.00       0.66        0.66        0.66       0.92       0.65    
    ## Num. obs.    24         24          24          24         24         24       
    ## ===============================================================================
    ## *** p < 0.001; ** p < 0.01; * p < 0.05

``` r
#--------------------------------------
# descriptivos
#--------------------------------------

data_13_srs %>%
group_by(drug) %>%
summarize(
  score = survey_mean(score, vartype = c('ci', 'se'))
) %>%
knitr::kable(., digits = 2)
```

| drug      | score | score_low | score_upp | score_se |
|:----------|------:|----------:|----------:|---------:|
| alcohol   |    15 |     10.39 |     19.61 |     2.23 |
| none      |     5 |      3.82 |      6.18 |     0.57 |
| poly_drug |    19 |     17.17 |     20.83 |     0.88 |

``` r
#--------------------------------------
# sum of squares
#--------------------------------------

anova(m01)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##           Df Sum Sq Mean Sq F value Pr(>F)
    ## Residuals 23   1204 52.3478

``` r
anova(m02)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##           Df Sum Sq Mean Sq  F value       Pr(>F)    
    ## drug_2     1     48  48.000  2.70968      0.11463    
    ## drug_3     1    784 784.000 44.25806 0.0000013837 ***
    ## Residuals 21    372  17.714                          
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(m03)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##           Df Sum Sq Mean Sq F value       Pr(>F)    
    ## drug_1     1    768 768.000 43.3548 0.0000016065 ***
    ## drug_2     1     64  64.000  3.6129     0.071147 .  
    ## Residuals 21    372  17.714                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(m04)
```

    ## Analysis of Variance Table
    ## 
    ## Response: score
    ##           Df Sum Sq Mean Sq F value       Pr(>F)    
    ## drug_1     1    768 768.000 43.3548 0.0000016065 ***
    ## drug_3     1     64  64.000  3.6129     0.071147 .  
    ## Residuals 21    372  17.714                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#--------------------------------------
# F stat
#--------------------------------------

broom::glance(m01)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1         0             0  7.24        NA      NA    NA  -81.0  166.  168.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::glance(m02)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic    p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>      <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.691         0.662  4.21      23.5 0.00000441     2  -66.9  142.  147.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::glance(m03)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic    p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>      <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.691         0.662  4.21      23.5 0.00000441     2  -66.9  142.  147.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::glance(m04)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic    p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>      <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.691         0.662  4.21      23.5 0.00000441     2  -66.9  142.  147.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
#--------------------------------------
# wald test
#--------------------------------------

broom::tidy(m01)
```

    ## # A tibble: 1 × 5
    ##   term        estimate std.error statistic       p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>         <dbl>
    ## 1 (Intercept)       13      1.48      8.80 0.00000000801

``` r
broom::tidy(m02)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic    p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)        5      1.49      3.36 0.00296   
    ## 2 drug_2            10      2.10      4.75 0.000108  
    ## 3 drug_3            14      2.10      6.65 0.00000138

``` r
broom::tidy(m03)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)       19      1.49     12.8  2.30e-11
    ## 2 drug_1           -14      2.10     -6.65 1.38e- 6
    ## 3 drug_2            -4      2.10     -1.90 7.11e- 2

``` r
broom::tidy(m04)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic       p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>         <dbl>
    ## 1 (Intercept)     15        1.49     10.1  0.00000000168
    ## 2 drug_1         -10.0      2.10     -4.75 0.000108     
    ## 3 drug_3           4        2.10      1.90 0.0711
