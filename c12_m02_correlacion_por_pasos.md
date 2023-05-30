Covarianza y Correlaciones
================
dacarras

# Clase 12: covarianza y correlaciones

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
# datos Vik (2014, p63)
#--------------------------------------

table_5_2 <- read.table(text = "
person       y     x
      1      2     8
      2      3     9
      3      3     9
      4      4    10
      5      7     6
      6      5     7
      7      5     4
      8      7     5
      9      8     3
     10      9     1
     11      9     2
     12     10     2
", header = TRUE, stringsAsFactors = FALSE)

#--------------------------------------
# mostrar datos
#--------------------------------------

knitr::kable(table_5_2, digits = 3)
```

| person |   y |   x |
|-------:|----:|----:|
|      1 |   2 |   8 |
|      2 |   3 |   9 |
|      3 |   3 |   9 |
|      4 |   4 |  10 |
|      5 |   7 |   6 |
|      6 |   5 |   7 |
|      7 |   5 |   4 |
|      8 |   7 |   5 |
|      9 |   8 |   3 |
|     10 |   9 |   1 |
|     11 |   9 |   2 |
|     12 |  10 |   2 |

## Preparar datos

``` r
#----------------------------------------------------------
# preparar datos
#----------------------------------------------------------

#--------------------------------------
# cargar dplyr
#--------------------------------------

library(dplyr)

#--------------------------------------
# medias de clusters
#--------------------------------------

c_mean <- function (x, j) {
    ave(x, j, FUN = function(x) mean(x, na.rm = T))
}

#--------------------------------------
# datos Vik (2014, p63)
#--------------------------------------

data_model <- table_5_2 %>%
              mutate(all = 1) %>%
              # cuadrados de cada variable
              mutate(yq = y^2) %>%
              mutate(xq = x^2) %>%
              # medias de cada variable
              mutate(y_g = c_mean(y, all)) %>%
              mutate(x_g = c_mean(x, all)) %>%
              ## desviaciones de cada variable
              mutate(y_w = y - y_g) %>%
              mutate(x_w = x - x_g) %>%
              # producto cruzado
              mutate(yx = y_w*x_w) %>%
              # suma de cuadrados de las variables
              mutate(y_s = y_w^2) %>%
              mutate(x_s = x_w^2) %>%
              dplyr::glimpse()
```

    ## Rows: 12
    ## Columns: 13
    ## $ person [3m[38;5;246m<int>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
    ## $ y      [3m[38;5;246m<int>[39m[23m 2, 3, 3, 4, 7, 5, 5, 7, 8, 9, 9, 10
    ## $ x      [3m[38;5;246m<int>[39m[23m 8, 9, 9, 10, 6, 7, 4, 5, 3, 1, 2, 2
    ## $ all    [3m[38;5;246m<dbl>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ## $ yq     [3m[38;5;246m<dbl>[39m[23m 4, 9, 9, 16, 49, 25, 25, 49, 64, 81, 81, 100
    ## $ xq     [3m[38;5;246m<dbl>[39m[23m 64, 81, 81, 100, 36, 49, 16, 25, 9, 1, 4, 4
    ## $ y_g    [3m[38;5;246m<dbl>[39m[23m 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
    ## $ x_g    [3m[38;5;246m<dbl>[39m[23m 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5
    ## $ y_w    [3m[38;5;246m<dbl>[39m[23m -4, -3, -3, -2, 1, -1, -1, 1, 2, 3, 3, 4
    ## $ x_w    [3m[38;5;246m<dbl>[39m[23m 2.5, 3.5, 3.5, 4.5, 0.5, 1.5, -1.5, -0.5, -2.5, -4.5, -3.5, -3.5
    ## $ yx     [3m[38;5;246m<dbl>[39m[23m -10.0, -10.5, -10.5, -9.0, 0.5, -1.5, 1.5, -0.5, -5.0, -13.5, -…
    ## $ y_s    [3m[38;5;246m<dbl>[39m[23m 16, 9, 9, 4, 1, 1, 1, 1, 4, 9, 9, 16
    ## $ x_s    [3m[38;5;246m<dbl>[39m[23m 6.25, 12.25, 12.25, 20.25, 0.25, 2.25, 2.25, 0.25, 6.25, 20.25,…

``` r
#--------------------------------------
# mostrar datos
#--------------------------------------

knitr::kable(data_model, digits = 3)
```

| person |   y |   x | all |  yq |  xq | y_g | x_g | y_w |  x_w |    yx | y_s |   x_s |
|-------:|----:|----:|----:|----:|----:|----:|----:|----:|-----:|------:|----:|------:|
|      1 |   2 |   8 |   1 |   4 |  64 |   6 | 5.5 |  -4 |  2.5 | -10.0 |  16 |  6.25 |
|      2 |   3 |   9 |   1 |   9 |  81 |   6 | 5.5 |  -3 |  3.5 | -10.5 |   9 | 12.25 |
|      3 |   3 |   9 |   1 |   9 |  81 |   6 | 5.5 |  -3 |  3.5 | -10.5 |   9 | 12.25 |
|      4 |   4 |  10 |   1 |  16 | 100 |   6 | 5.5 |  -2 |  4.5 |  -9.0 |   4 | 20.25 |
|      5 |   7 |   6 |   1 |  49 |  36 |   6 | 5.5 |   1 |  0.5 |   0.5 |   1 |  0.25 |
|      6 |   5 |   7 |   1 |  25 |  49 |   6 | 5.5 |  -1 |  1.5 |  -1.5 |   1 |  2.25 |
|      7 |   5 |   4 |   1 |  25 |  16 |   6 | 5.5 |  -1 | -1.5 |   1.5 |   1 |  2.25 |
|      8 |   7 |   5 |   1 |  49 |  25 |   6 | 5.5 |   1 | -0.5 |  -0.5 |   1 |  0.25 |
|      9 |   8 |   3 |   1 |  64 |   9 |   6 | 5.5 |   2 | -2.5 |  -5.0 |   4 |  6.25 |
|     10 |   9 |   1 |   1 |  81 |   1 |   6 | 5.5 |   3 | -4.5 | -13.5 |   9 | 20.25 |
|     11 |   9 |   2 |   1 |  81 |   4 |   6 | 5.5 |   3 | -3.5 | -10.5 |   9 | 12.25 |
|     12 |  10 |   2 |   1 | 100 |   4 |   6 | 5.5 |   4 | -3.5 | -14.0 |  16 | 12.25 |

## Correlación por pasos

``` r
#----------------------------------------------------------
# correlación por pasos
#----------------------------------------------------------

#--------------------------------------
# coeficiente de correlacion estimates
#--------------------------------------

# varianza comun o compartida entre dos variables
shared_variance <- sum(data_model$yx)

# varianza combinanda o total entre dos variables
combined_variance <- sqrt(sum(data_model$y_s)*sum(data_model$x_s))

# coeficiente de correlación
shared_variance/combined_variance
```

    ## [1] -0.8971007

## Correlación

``` r
#----------------------------------------------------------
# correlación via función
#----------------------------------------------------------

#--------------------------------------
# coeficiente de correlacion, cor.test
#--------------------------------------

data_model %>%
dplyr::select(x,y) %>%
with(., cor.test(x,y))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  x and y
    ## t = -6.4208, df = 10, p-value = 0.00007627
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9710564 -0.6661805
    ## sample estimates:
    ##        cor 
    ## -0.8971007

``` r
#--------------------------------------
# tabla de correlaciones
#--------------------------------------

data_model %>%
dplyr::select(x,y) %>%
corrr::correlate()
```

    ## Correlation computed with
    ## • Method: 'pearson'
    ## • Missing treated using: 'pairwise.complete.obs'

    ## # A tibble: 2 × 3
    ##   term       x      y
    ##   <chr>  <dbl>  <dbl>
    ## 1 x     NA     -0.897
    ## 2 y     -0.897 NA

## Distribución muestral de una correlación

``` r
#----------------------------------------------------------
# correlación via función
#----------------------------------------------------------

#--------------------------------------
# coeficiente de correlacion, with infer
#--------------------------------------

library(infer)
correlation_hat <- data_model %>% 
  specify(y ~ x) %>% 
  calculate(stat = "correlation")
correlation_hat
```

    ## Response: y (numeric)
    ## Explanatory: x (numeric)
    ## # A tibble: 1 × 1
    ##     stat
    ##    <dbl>
    ## 1 -0.897

``` r
#--------------------------------------
# coeficiente de correlacion, with infer
#--------------------------------------

correlation_hat <- data_model %>% 
  observe(y ~ x, stat = "correlation")
correlation_hat
```

    ## Response: y (numeric)
    ## Explanatory: x (numeric)
    ## # A tibble: 1 × 1
    ##     stat
    ##    <dbl>
    ## 1 -0.897

``` r
#--------------------------------------
# distribución muestral
#--------------------------------------

null_dist <- data_model %>%
   specify(y ~ x) %>% 
   hypothesize(null = "independence") %>%
   generate(reps = 5000, type = "permute") %>%
   calculate(stat = "correlation")

#--------------------------------------
# valor p
#--------------------------------------

visualize(null_dist) +
shade_p_value(
  obs_stat = correlation_hat, 
  direction = "two-sided"
  )
```

![](c12_m02_correlacion_por_pasos_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Correlación como coeficiente estandarizado

``` r
#----------------------------------------------------------
# correlación via función
#----------------------------------------------------------

#--------------------------------------
# coeficiente de correlacion, with infer
#--------------------------------------

lm(y ~ x, data = data_model) %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = data_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.1635 -0.3365  0.1121  0.7804  1.4907 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value     Pr(>|t|)    
    ## (Intercept)  10.2664     0.7561  13.579 0.0000000907 ***
    ## x            -0.7757     0.1208  -6.421 0.0000762671 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.25 on 10 degrees of freedom
    ## Multiple R-squared:  0.8048, Adjusted R-squared:  0.7853 
    ## F-statistic: 41.23 on 1 and 10 DF,  p-value: 0.00007627

``` r
lm(
  scale(y, scale = TRUE) ~ 
  scale(x, scale = TRUE),
  data = data_model) %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = scale(y, scale = TRUE) ~ scale(x, scale = TRUE), 
    ##     data = data_model)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.80227 -0.12476  0.04159  0.28937  0.55275 
    ## 
    ## Coefficients:
    ##                                       Estimate              Std. Error t value
    ## (Intercept)             0.00000000000000008625  0.13376948718199746513   0.000
    ## scale(x, scale = TRUE) -0.89710072992176737028  0.13971767260222728302  -6.421
    ##                         Pr(>|t|)    
    ## (Intercept)                    1    
    ## scale(x, scale = TRUE) 0.0000763 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4634 on 10 degrees of freedom
    ## Multiple R-squared:  0.8048, Adjusted R-squared:  0.7853 
    ## F-statistic: 41.23 on 1 and 10 DF,  p-value: 0.00007627
