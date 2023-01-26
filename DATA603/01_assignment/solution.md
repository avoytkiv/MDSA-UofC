Assignment 1
================
Andrii Voitkiv
January 25th, 2023

-   [Problem 1](#problem-1)
-   [Problem 2](#problem-2)
-   [Problem 3](#problem-3)

``` r
knitr::opts_chunk$set(fig.path='Figs/')
```

``` r
library("mosaic")
```

## Problem 1

``` r
water = read.csv("/Users/berg/DataspellProjects/MDSA-UofC/DATA603/data/water.csv")
```

### A

``` r
water_full_model = lm(USAGE~PROD+TEMP+HOUR+DAYS, data=water)
water_full_model$coefficients
```

    ## (Intercept)        PROD        TEMP        HOUR        DAYS 
    ##  5.89162697  0.04020739  0.16867306 -0.07099009 -0.02162304

Model equation: USAGE_hat = 5.89162697 + 0.04020739 * PROD +
0.16867306 * TEMP - 0.07099009 * HOUR - 0.02162304 * DAYS

### B

H0: the model itself contributes nothing useful, and all slope
coefficients are zero: beta(PROD) = beta(TEMP) = beta(HOUR) = beta(DAYS)
= 0 Ha: at least one beta is not zero

``` r
anova(water_full_model)
```

    ## Analysis of Variance Table
    ## 
    ## Response: USAGE
    ##            Df Sum Sq Mean Sq   F value    Pr(>F)    
    ## PROD        1 4210.3  4210.3 1346.3213 < 2.2e-16 ***
    ## TEMP        1 1813.7  1813.7  579.9440 < 2.2e-16 ***
    ## HOUR        1   54.3    54.3   17.3516 4.313e-05 ***
    ## DAYS        1    1.4     1.4    0.4514    0.5023    
    ## Residuals 244  763.1     3.1                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of global anova test shows that F=486.02 with 4, 244 degrees
of freedom (p-value &lt; 2.2e-16 &lt; alpha = 0.05), indicating that we
should clearly reject the null hypothesis. The large F-test suggests
that at least one coefficient should be significant, meaning linearly
related to water usage.

### C

Partial test - individual coefficients test (t-test) H0: beta(i) = 0 Ha:
beta(i) is not equal to 0

``` r
summary(water_full_model)
```

    ## 
    ## Call:
    ## lm(formula = USAGE ~ PROD + TEMP + HOUR + DAYS, data = water)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.4030 -1.1433  0.0473  1.1677  5.3999 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.891627   1.028794   5.727  3.0e-08 ***
    ## PROD         0.040207   0.001629  24.681  < 2e-16 ***
    ## TEMP         0.168673   0.008209  20.546  < 2e-16 ***
    ## HOUR        -0.070990   0.016992  -4.178  4.1e-05 ***
    ## DAYS        -0.021623   0.032183  -0.672    0.502    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.768 on 244 degrees of freedom
    ## Multiple R-squared:  0.8885, Adjusted R-squared:  0.8867 
    ## F-statistic:   486 on 4 and 244 DF,  p-value: < 2.2e-16

From the individual coefficients test (t-test), the output shows that
the variable `DAYS` has a t-value=-0.672 with the p-value=0.502 &gt;
0.05, indicating that we should not to reject the null hypothesis.
Meaning that the variable `DAYS` has not significant influence on water
`USAGE` at 5% significance level. Other variables such as `PROD`, `TEMP`
and `HOUR` have t-values far beyond the t-distribution (given that
beta(i)=0) and respective p-values are too small to accept the
null-hypothesis, so can confidently reject it and therefore accept
alternative that their betas are not equal to zero.

``` r
water_current_model = lm(USAGE~PROD+TEMP+HOUR, data=water)
water_current_model$coefficients
```

    ## (Intercept)        PROD        TEMP        HOUR 
    ##  5.30751078  0.04011468  0.16918771 -0.07076858

Current valid model is: USAGE\_hat = 5.30751078 + 0.04011468 \* PROD +
0.16918771 \* TEMP - 0.07076858 \* HOUR

### D

Partial F-test to confirm that the `DAYS` variable should be out of the
model at significance level 0.05. H0: beta(DAYS) = 0 in the model
USAGE\_hat = Intercept + beta(PROD)\_hat \* PROD + beta(TEMP)\_hat \*
TEMP + beta(HOUR)\_hat \* HOUR + beta(DAYS)\_hat \* DAYS Ha: beta(DAYS)
is not equal to zero in the model stated above.

``` r
anova(water_current_model, water_full_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: USAGE ~ PROD + TEMP + HOUR
    ## Model 2: USAGE ~ PROD + TEMP + HOUR + DAYS
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    245 764.47                           
    ## 2    244 763.06  1    1.4117 0.4514 0.5023

With *p-value* equal to 0.5023 (&gt; 0.05), indicating that we *failed
to reject the null hypothesis* which mean that we definitely drop the
variable `DAYS` off the model

Current valid model is:
`USAGE_hat = 5.30751078 + 0.04011468 * PROD + 0.16918771 * TEMP - 0.07076858 * HOUR`

### E

A 95% confidence interval of regression coefficient for `TEMP` from the
model in part c

``` r
confint(water_current_model)
```

    ##                   2.5 %      97.5 %
    ## (Intercept)  4.22519744  6.38982411
    ## PROD         0.03692098  0.04330837
    ## TEMP         0.15310634  0.18526907
    ## HOUR        -0.10419445 -0.03734272

From the output above, we are 95% confident that usage of water
increases, on average, between 0.15310634 and 0.18526907 gallons/minute
for every increase in `TEMP` by 1 degree celsius, holding `HOUR` and
`PROD` constant.

### F

``` r
cat("The model containing all predictors has adjusted r-squared =",
    summary(water_full_model)$adj.r.squared,
    "\n",
    "On the other hand, the reduced model has adjusted r-squared =",
    summary(water_current_model)$adj.r.squared,
    "\n\n")
```

    ## The model containing all predictors has adjusted r-squared = 0.886658 
    ##  On the other hand, the reduced model has adjusted r-squared = 0.8869118

``` r
cat("The model containing all predictors has RMSE =",
    sigma(water_full_model),
    "\n",
    "On the other hand, the reduced model has RMSE =",
    sigma(water_current_model))
```

    ## The model containing all predictors has RMSE = 1.768414 
    ##  On the other hand, the reduced model has RMSE = 1.766433

This implies that the reduced model that uses PROD, TEMP and HOUR to
predict water USAGE is better than the full model. Higher adjusted
R-squared and lower RMSE indicate better fit. Then 88.69% of the
variation of the response variable which is water USAGE is explained by
the model. RMSE can be interpreted as the standard deviation of the
unexplained variance.

### G

``` r
water_interraction_model = lm(USAGE~PROD+TEMP+HOUR+PROD*TEMP+PROD*HOUR+TEMP*HOUR, data=water)
summary(water_interraction_model)
```

    ## 
    ## Call:
    ## lm(formula = USAGE ~ PROD + TEMP + HOUR + PROD * TEMP + PROD * 
    ##     HOUR + TEMP * HOUR, data = water)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1941 -0.3165 -0.0502  0.2755  7.0985 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.294e+01  7.113e-01  18.193   <2e-16 ***
    ## PROD        -3.642e-03  2.565e-03  -1.420    0.157    
    ## TEMP        -2.389e-02  2.129e-02  -1.122    0.263    
    ## HOUR        -2.340e-01  2.512e-02  -9.316   <2e-16 ***
    ## PROD:TEMP    1.189e-03  6.932e-05  17.154   <2e-16 ***
    ## PROD:HOUR    7.767e-04  7.820e-05   9.933   <2e-16 ***
    ## TEMP:HOUR    7.600e-04  7.683e-04   0.989    0.324    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9867 on 242 degrees of freedom
    ## Multiple R-squared:  0.9656, Adjusted R-squared:  0.9647 
    ## F-statistic:  1131 on 6 and 242 DF,  p-value: < 2.2e-16

For testing an interaction term in regression model, we use the
Individual Coefficients Test (t-test) method. From the output, t-values
are 17.154 and 9.933 (and zero p-values) for interaction terms
`PROD*TEMP` and `PROD*HOUR` respectively at alpha=0.05. Meaning that we
failed to reject the null hypothesis that these beta\_hat coefficients
are zero. I decided to include them into my model.

``` r
water_valid_model = lm(USAGE~PROD+TEMP+HOUR+PROD*TEMP+PROD*HOUR, data=water)
summary(water_valid_model)
```

    ## 
    ## Call:
    ## lm(formula = USAGE ~ PROD + TEMP + HOUR + PROD * TEMP + PROD * 
    ##     HOUR, data = water)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.1423 -0.3148 -0.0358  0.3029  7.2555 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.243e+01  4.839e-01  25.679   <2e-16 ***
    ## PROD        -2.529e-03  2.305e-03  -1.097    0.274    
    ## TEMP        -4.737e-03  8.859e-03  -0.535    0.593    
    ## HOUR        -2.151e-01  1.624e-02 -13.242   <2e-16 ***
    ## PROD:TEMP    1.142e-03  5.009e-05  22.795   <2e-16 ***
    ## PROD:HOUR    7.873e-04  7.745e-05  10.165   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9866 on 243 degrees of freedom
    ## Multiple R-squared:  0.9654, Adjusted R-squared:  0.9647 
    ## F-statistic:  1357 on 5 and 243 DF,  p-value: < 2.2e-16

``` r
water_valid_model$coefficients
```

    ##   (Intercept)          PROD          TEMP          HOUR     PROD:TEMP 
    ## 12.4257346600 -0.0025288273 -0.0047367734 -0.2150726648  0.0011417022 
    ##     PROD:HOUR 
    ##  0.0007873227

I can recommend this model:
`USAGE_hat = 12.4257346600 - 0.0025288273 * PROD - 0.0047367734 * TEMP - 0.2150726648 * HOUR + 0.0011417022 * PROD * TEMP + 0.0007873227 * PROD * HOUR`

It has significantly lower RMSE (0.9866) and substantially higher
adjusted r-squared (0.9647) than previous models.

## Problem 2

``` r
gfclocks = read.csv("/Users/berg/DataspellProjects/MDSA-UofC/DATA603/data/GFCLOCKS.csv")
```

### A

``` r
clocks_full_model = lm(PRICE~AGE+NUMBIDS, data=gfclocks)
clocks_full_model$coefficients
```

    ## (Intercept)         AGE     NUMBIDS 
    ## -1338.95134    12.74057    85.95298

Full model:
`Price_hat = -1338.95134 + 12.74057 * AGE + 85.95298 * NUMBIDS`

### B

``` r
# METHOD 1
sse_method1 = sigma(clocks_full_model)^2 * (32-2-1)
print(sse_method1)
```

    ## [1] 516726.5

``` r
# METHOD 2
price_hat = predict(clocks_full_model, gfclocks[c('AGE', 'NUMBIDS')])
errors = gfclocks$PRICE - price_hat
squared_errors = errors ^ 2
sse_method2 = sum(squared_errors)
print(sse_method2)
```

    ## [1] 516726.5

### C

``` r
rmse = sqrt(sse_method2 / (32-2-1))
print(rmse)
```

    ## [1] 133.4847

From my calculation, RMSE is 133.4847 dollars which is the standard
deviation of the unexplained variance. In other words, it tells us the
average distance (133.48 dollars) between the predicted price from the
model and the actual price in the dataset.

### D

``` r
summary(clocks_full_model)$adj.r.squared
```

    ## [1] 0.8849194

The adjusted r-squared is 0.8849 that is the proportion 88.49% of the
total variation in response variable `PRICE` that can be explained by
the regression model.

### E

H0: the model itself contributes nothing useful, and all slope
coefficients are zero: beta(AGE) = beta(NUMBIDS) = 0 Ha: at least one
beta is not zero

``` r
anova(lm(PRICE~1, data = gfclocks), clocks_full_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: PRICE ~ 1
    ## Model 2: PRICE ~ AGE + NUMBIDS
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1     31 4799790                                  
    ## 2     29  516727  2   4283063 120.19 9.216e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of the global anova test shows that F=120.19 (with df=2,29
and p-value &lt; 9.216e-15 &lt; alpha = 0.05), indicating that we should
clearly reject the null hypothesis. The large F-test suggests that at
least one coefficient should be significant, meaning linearly related to
`PRICE`.

### F

H0: beta(NUMBIDS) = 0, meaning price doesn’t change on average when
number of bidders change (age held constant); Ha: beta(NUMBIDS) != 0,
meaning price does change on average when number of bidders change (age
held constant).

``` r
summary(clocks_full_model)
```

    ## 
    ## Call:
    ## lm(formula = PRICE ~ AGE + NUMBIDS, data = gfclocks)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -206.49 -117.34   16.66  102.55  213.50 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1338.9513   173.8095  -7.704 1.71e-08 ***
    ## AGE            12.7406     0.9047  14.082 1.69e-14 ***
    ## NUMBIDS        85.9530     8.7285   9.847 9.34e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 133.5 on 29 degrees of freedom
    ## Multiple R-squared:  0.8923, Adjusted R-squared:  0.8849 
    ## F-statistic: 120.2 on 2 and 29 DF,  p-value: 9.216e-15

From the summary output, the t-value for `NUMBIDS` is 9.847 and p-value
is close to zero (far less than alpha 0.05). It means that it is
unlikely that beta(NUMBIDS) is zero. So, we can be 95% confident and
reject the null hypothesis that there is no linear relationship between
our tested variable and price. The estimated value is 85.953 which is
positive. That means price increase on average by 85.95 dollars when
number of bidders increase by 1 person, holding another variable
constant.

### G

``` r
confint(clocks_full_model)
```

    ##                   2.5 %     97.5 %
    ## (Intercept) -1694.43162 -983.47106
    ## AGE            10.89017   14.59098
    ## NUMBIDS        68.10115  103.80482

From this data, we can be 95% confident that the true value of beta1
(AGE) is somewhere between 10.89 and 14.59, meaning that the price
increases by these numbers on average, given an increase of clock’s age
by 1 year, holding other variable constant.

### H

``` r
clocks_interaction_model = lm(PRICE~AGE+NUMBIDS+AGE*NUMBIDS, data = gfclocks)
summary(clocks_interaction_model)
```

    ## 
    ## Call:
    ## lm(formula = PRICE ~ AGE + NUMBIDS + AGE * NUMBIDS, data = gfclocks)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -154.995  -70.431    2.069   47.880  202.259 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 320.4580   295.1413   1.086  0.28684    
    ## AGE           0.8781     2.0322   0.432  0.66896    
    ## NUMBIDS     -93.2648    29.8916  -3.120  0.00416 ** 
    ## AGE:NUMBIDS   1.2978     0.2123   6.112 1.35e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 88.91 on 28 degrees of freedom
    ## Multiple R-squared:  0.9539, Adjusted R-squared:  0.9489 
    ## F-statistic:   193 on 3 and 28 DF,  p-value: < 2.2e-16

From the output, the interaction term AGE\*NUMBIDS is statistically
significant as t-value is 6.112 and p-value is 1.35e-06 which is far
less than alpha=0.05. It should be included into our model.

``` r
cat("The model containing all predictors has adjusted r-squared =",
    summary(clocks_full_model)$adj.r.squared,
    "\n",
    "On the other hand, the model that includes interaction term has adjusted r-squared =",
    summary(clocks_interaction_model)$adj.r.squared,
    "\n\n")
```

    ## The model containing all predictors has adjusted r-squared = 0.8849194 
    ##  On the other hand, the model that includes interaction term has adjusted r-squared = 0.9489395

``` r
cat("The model containing all predictors has RMSE =",
    sigma(clocks_full_model),
    "\n",
    "On the other hand, the model that includes interaction term has RMSE =",
    sigma(clocks_interaction_model))
```

    ## The model containing all predictors has RMSE = 133.4847 
    ##  On the other hand, the model that includes interaction term has RMSE = 88.91451

This implies that the full model with main predictors that includes
interaction term is better than the full model. Higher adjusted
R-squared (0.95 vs 0.88)and lower RMSE (88.91 vs 133.48) indicate better
fit. Then 94.89% of the variation of the response variable which is
auction price is explained by the model.

The model that is recommended for predicting:
`PRICE_hat = 320.4579934 + 0.8781425 * AGE - 93.2648244 * NUMBIDS + 1.2978458 * AGE * NUMBIDS`

## Problem 3

``` r
turbine = read.csv("/Users/berg/DataspellProjects/MDSA-UofC/DATA603/data/TURBINE.csv")
```

### A

``` r
turbine_full_model = lm(HEATRATE~CPRATIO+RPM+INLET.TEMP+EXH.TEMP+AIRFLOW, data = turbine)
turbine_full_model$coefficients
```

    ##   (Intercept)       CPRATIO           RPM    INLET.TEMP      EXH.TEMP 
    ##  1.361446e+04  3.519043e-01  8.878591e-02 -9.200873e+00  1.439385e+01 
    ##       AIRFLOW 
    ## -8.479583e-01

A first-order model:
`HEATRATE_hat = 1.361446e+04 + 3.519043e-01 * CPRATIO + 8.878591e-02 * RPM - 9.200873 * INLET.TEMP + 1.439385e+01 * EXH.TEMP - 8.479583e-01 * AIRFLOW`

### B

H0: the model itself contributes nothing useful, and all slope
coefficients are zero: beta(RPM) = beta(INLET.TEMP) = beta(EXH.TEMP) =
beta(POWER) = beta(AIRFLOW) = 0 Ha: at least one beta is not zero

``` r
anova(lm(HEATRATE~1, data = turbine), turbine_full_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: HEATRATE ~ 1
    ## Model 2: HEATRATE ~ CPRATIO + RPM + INLET.TEMP + EXH.TEMP + AIRFLOW
    ##   Res.Df       RSS Df Sum of Sq     F    Pr(>F)    
    ## 1     66 167897208                                 
    ## 2     61  12841935  5 155055273 147.3 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of global anova test shows that F=149.88 with 5,61 degrees of
freedom (p-value &lt; 2.2e-16 &lt; alpha = 0.01), indicating that we
should clearly reject the null hypothesis. The large F-test suggests
that at least one coefficient should be significant, meaning linearly
related to `HEATRATE`.

### C

``` r
summary(turbine_full_model)
```

    ## 
    ## Call:
    ## lm(formula = HEATRATE ~ CPRATIO + RPM + INLET.TEMP + EXH.TEMP + 
    ##     AIRFLOW, data = turbine)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1007.0  -290.9  -105.8   240.8  1414.0 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.361e+04  8.700e+02  15.649  < 2e-16 ***
    ## CPRATIO      3.519e-01  2.956e+01   0.012 0.990539    
    ## RPM          8.879e-02  1.391e-02   6.382 2.64e-08 ***
    ## INLET.TEMP  -9.201e+00  1.499e+00  -6.137 6.86e-08 ***
    ## EXH.TEMP     1.439e+01  3.461e+00   4.159 0.000102 ***
    ## AIRFLOW     -8.480e-01  4.421e-01  -1.918 0.059800 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 458.8 on 61 degrees of freedom
    ## Multiple R-squared:  0.9235, Adjusted R-squared:  0.9172 
    ## F-statistic: 147.3 on 5 and 61 DF,  p-value: < 2.2e-16

Individual t-tests show that we can confidently remove `CPRATIO` from
our model with p-value 0.99 (&gt; alpha = 0.05). Regarding, the variable
`AIRFLOW`, it’s in the grey zone with p-value 0.0598 that is slightly
above alpha=0.05. I will compare *adjusted R-squared* of two models with
and without the `AIRFLOW` variable to decide if I should keep it or not.
NOTE: remove `CPRATIO` for both models as insignificant.

``` r
cat("The model with AIRFLOW predictor has adjusted r-squared =",
    summary(lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+AIRFLOW, data = turbine))$adj.r.squared,
    "\n",
    "On the other hand, the model without AIRFLOW predictor has adjusted r-squared =",
    summary(lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP, data = turbine))$adj.r.squared,
    "\n\n")
```

    ## The model with AIRFLOW predictor has adjusted r-squared = 0.9185783 
    ##  On the other hand, the model without AIRFLOW predictor has adjusted r-squared = 0.9150099

As the model that contains `AIRFLOW` predictor has slightly higher
adjusted r-squared, I am going to include this variable into my model as
the output above provides evidence that the model with that variable is
superior.

``` r
turbine_current_model = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+AIRFLOW, data = turbine)
turbine_current_model$coefficients
```

    ##   (Intercept)           RPM    INLET.TEMP      EXH.TEMP       AIRFLOW 
    ##  1.361792e+04  8.882334e-02 -9.185605e+00  1.436283e+01 -8.475203e-01

A model with coefficients above is currently the best model.

### D

``` r
turbine_interaction_model = lm(HEATRATE~(RPM+INLET.TEMP+EXH.TEMP+AIRFLOW)^2, data = turbine)
summary(turbine_interaction_model)
```

    ## 
    ## Call:
    ## lm(formula = HEATRATE ~ (RPM + INLET.TEMP + EXH.TEMP + AIRFLOW)^2, 
    ##     data = turbine)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -779.7 -211.0  -40.7  177.2 1370.3 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.650e+04  8.891e+03   2.981 0.004247 ** 
    ## RPM                  7.037e-02  1.485e-01   0.474 0.637512    
    ## INLET.TEMP          -2.366e+01  7.364e+00  -3.213 0.002180 ** 
    ## EXH.TEMP            -4.555e+00  1.795e+01  -0.254 0.800610    
    ## AIRFLOW              1.021e+01  6.279e+00   1.627 0.109455    
    ## RPM:INLET.TEMP      -1.133e-04  8.720e-05  -1.299 0.199266    
    ## RPM:EXH.TEMP         1.656e-04  3.116e-04   0.531 0.597314    
    ## RPM:AIRFLOW         -8.257e-04  4.653e-04  -1.775 0.081414 .  
    ## INLET.TEMP:EXH.TEMP  2.417e-02  1.457e-02   1.659 0.102791    
    ## INLET.TEMP:AIRFLOW   1.418e-02  3.852e-03   3.681 0.000523 ***
    ## EXH.TEMP:AIRFLOW    -5.049e-02  1.357e-02  -3.720 0.000463 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 394.6 on 56 degrees of freedom
    ## Multiple R-squared:  0.9481, Adjusted R-squared:  0.9388 
    ## F-statistic: 102.3 on 10 and 56 DF,  p-value: < 2.2e-16

From output above, there are two statistically significant interactions
between: `INLET.TEMP` x `AIRFLOW` and `EXH.TEMP` x `AIRFLOW`. Therefore,
I will include them into the model.

``` r
turbine_valid_model = lm(HEATRATE~RPM+INLET.TEMP+EXH.TEMP+AIRFLOW+INLET.TEMP*AIRFLOW+EXH.TEMP*AIRFLOW, data = turbine)
summary(turbine_valid_model)
```

    ## 
    ## Call:
    ## lm(formula = HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + 
    ##     INLET.TEMP * AIRFLOW + EXH.TEMP * AIRFLOW, data = turbine)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -787.68 -189.26  -22.34  145.15 1307.53 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         1.360e+04  9.930e+02  13.699  < 2e-16 ***
    ## RPM                 4.578e-02  1.577e-02   2.902 0.005174 ** 
    ## INLET.TEMP         -1.280e+01  1.090e+00 -11.741  < 2e-16 ***
    ## EXH.TEMP            2.327e+01  2.901e+00   8.024 4.46e-11 ***
    ## AIRFLOW             1.347e+00  3.496e+00   0.385 0.701414    
    ## INLET.TEMP:AIRFLOW  1.613e-02  3.640e-03   4.432 4.03e-05 ***
    ## EXH.TEMP:AIRFLOW   -4.150e-02  1.087e-02  -3.816 0.000323 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 401.4 on 60 degrees of freedom
    ## Multiple R-squared:  0.9424, Adjusted R-squared:  0.9367 
    ## F-statistic: 163.7 on 6 and 60 DF,  p-value: < 2.2e-16

Check with anova if the overall model makes sense:

``` r
anova(lm(HEATRATE~1, data = turbine), turbine_valid_model)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: HEATRATE ~ 1
    ## Model 2: HEATRATE ~ RPM + INLET.TEMP + EXH.TEMP + AIRFLOW + INLET.TEMP * 
    ##     AIRFLOW + EXH.TEMP * AIRFLOW
    ##   Res.Df       RSS Df Sum of Sq      F    Pr(>F)    
    ## 1     66 167897208                                  
    ## 2     60   9664946  6 158232262 163.72 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

From output above, with F=163.7 and degrees of freedom 6,60 we are
strongly confident to *reject* the null hypothesis that the model with
interaction terms contributes nothing useful, and all slope coefficients
are zero.

``` r
turbine_valid_model$coefficients
```

    ##        (Intercept)                RPM         INLET.TEMP           EXH.TEMP 
    ##       1.360331e+04       4.577613e-02      -1.279883e+01       2.327429e+01 
    ##            AIRFLOW INLET.TEMP:AIRFLOW   EXH.TEMP:AIRFLOW 
    ##       1.346949e+00       1.613280e-02      -4.149806e-02

Current valid model for prediction, that I recommend:

HEATRATE\_hat = 1.360331e+04 + 4.577613e-02 \* RPM - 1.279883e+01 \*
INLET.TEMP + 2.327429e+01 \* EXH.TEMP + 1.346949 \* AIRFLOW +
1.613280e-02 \* INLET.TEMP:AIRFLOW - 4.149806e-02 \* EXH.TEMP:AIRFLOW

### E

To interpret the coefficients, let’s collect the common terms first:
HEATRATE\_hat = 1.360331e+04 + 4.577613e-02 \* RPM - 1.279883e+01 \*
INLET.TEMP + 2.327429e+01 \* EXH.TEMP + AIRFLOW \* (1.346949 +
1.613280e-02 \* INLET.TEMP - 4.149806e-02 \* EXH.TEMP)

**Main effects**: `RPM`: increase of the cycle speed by 1 revolution per
minute *increases* the heat rate, on average, by 4.577613e-02 kilojoules
per kilowatt per hour, holding other variables constant; `INLET.TEMP`:
increase of the inlet temperature by 1 degree celsius *decreases* the
heat rate, on average, by 1.279883e+01 kilojoules per kilowatt per hour,
holding other variables constant; `EXH.TEMP`: increase of the exhaust
gas temperature by 1 degree celsius *increases* the heat rate, on
average, by 2.327429e+01 kilojoules per kilowatt per hour, holding other
variables constant; **Interaction effects**: The heat rate depends on
`AIRFLOW` variable which in turn has linear dependence on `INLET.TEMP`
and `EXH.TEMP`.

### F

``` r
sigma(turbine_valid_model)
```

    ## [1] 401.3508

### G

``` r
summary(turbine_valid_model)$adj.r.squared
```

    ## [1] 0.9366789

The adjusted r-squared is 0.9366789 that is the proportion 93.67% of the
total variation in response variable `HEATRATE` that can be explained by
the regression model.

### H

``` r
favstats(~RPM, data = turbine)
```

    ##   min   Q1 median    Q3   max     mean       sd  n missing
    ##  3000 3600   5100 12610 33000 8326.642 7023.311 67       0

``` r
favstats(~INLET.TEMP, data = turbine)
```

    ##  min   Q1 median   Q3  max     mean       sd  n missing
    ##  888 1078   1149 1288 1427 1174.313 137.4331 67       0

``` r
favstats(~EXH.TEMP, data = turbine)
```

    ##  min    Q1 median    Q3 max     mean       sd  n missing
    ##  444 512.5    532 568.5 626 536.0896 44.13984 67       0

``` r
favstats(~AIRFLOW, data = turbine)
```

    ##  min Q1 median    Q3 max    mean      sd  n missing
    ##    3 27    172 442.5 737 240.791 226.714 67       0

``` r
new_data = data.frame(RPM=273145, INLET.TEMP=1240, EXH.TEMP=920, AIRFLOW=25)
predict(turbine_valid_model, new_data, interval = "predict")
```

    ##        fit      lwr     upr
    ## 1 31227.97 24067.74 38388.2

The 95% confidence interval of the `HEATRATE` with the given parameters
is between 24067.74 and 38388.2 (kilojoules per kilowatt per hour) given
the new values for predictors. But we have to take this result with the
grain of salt as some of our new data predictors are beyond the sample
data. Namely, a cycle of speed = 273,145 is fat beyond the max point of
the cycle of speed from our sample which is 33,000. The same story for
exhaust temperature=920 with sample max 626. These values are not in the
range/bounds of the sample data.

1.  561.037 + 92.582 \* factor(weekend)1 + 4.303 \* snow - 6.843 \*
    temperature when weekend is 1 (weekend): step1: 561.037 + 92.582 \*
    1 + 4.303 \* snow - 6.843 \* temperature step2: 653.619 + 4.303 \*
    snow - 6.843 \* temperature when weekend is 0 (weekday): step1:
    561.037 + 92.582 \* 0 + 4.303 \* snow - 6.843 \* temperature step2:
    561.037 + 4.303 \* snow - 6.843 \* temperature

2.  552.949 + 208.279 \* factor(weekend)1 + 3.913 \* snow - 6.037 \*
    temperature - 13.021 \* factor(weekend)1 \* temperature when weekend
    is 1 (weekend): step1: 552.949 + 208.279 \* 1 + 3.913 \* snow -
    6.037 \* temperature - 13.021 \* 1 \* temperature step2: 761.228 +
    3.913 \* snow - 19.058 \* temperature

when weekend is 0 (weekday): step1: 552.949 + 208.279 \* 0 + 3.913 \*
snow - 6.037 \* temperature - 13.021 \* 0 \* temperature step2: 552.949
+ 3.913 \* snow - 6.037 \* temperature

MODEL 1: when weekend is 1 (weekend): 653.619 + 4.303 \* snow - 6.843 \*
temperature when weekend is 0 (weekday): 561.037 + 4.303 \* snow - 6.843
\* temperature Difference: 92.582 when any temperature

MODEL 2: when weekend is 1 (weekend): 761.228 + 3.913 \* snow - 19.058
\* temperature when weekend is 0 (weekday): 552.949 + 3.913 \* snow -
6.037 \* temperature Difference: 78.069 when temperature = 10

EFFECT OF TEMPERATURE: 552.949 + 208.279 \* factor(weekend)1 + 3.913 \*
snow - (6.037 + 13.021 \* factor(weekend)1) \* temperature when weekend
is 1 (weekend): 552.949 + 208.279 + 3.913 \* snow - (6.037 + 13.021) \*
temperature when weekend is 0 (weekday):

STATEMENT 1: temperature = 10 From model 1 the difference is 92.582 From
model 1 the difference is 78.069
