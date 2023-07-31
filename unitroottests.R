> # Unit Root tests - No differencing
> # ADF Trend
> summary(ur.df(log(ts_data$GWh), type = "trend")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression trend 


Call:
lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.76443 -0.14908  0.02361  0.20092  0.72149 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.565113   0.326535   7.856 5.74e-12 ***
z.lag.1     -0.334593   0.041486  -8.065 2.07e-12 ***
tt           0.001692   0.001069   1.583    0.117    
z.diff.lag   0.747614   0.065198  11.467  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3053 on 96 degrees of freedom
Multiple R-squared:  0.6173,	Adjusted R-squared:  0.6053 
F-statistic: 51.62 on 3 and 96 DF,  p-value: < 2.2e-16


Value of test-statistic is: -8.0653 21.7443 32.6117 

Critical values for test statistics: 
      1pct  5pct 10pct
tau3 -3.99 -3.43 -3.13
phi2  6.22  4.75  4.07
phi3  8.43  6.49  5.47

> 
> # ADF Drift
> summary(ur.df(log(ts_data$GWh), type = "drift")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression drift 


Call:
lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.75383 -0.17871  0.01602  0.21031  0.77138 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.57694    0.32897   7.833 6.05e-12 ***
z.lag.1     -0.32508    0.04136  -7.859 5.35e-12 ***
z.diff.lag   0.74125    0.06558  11.303  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3076 on 97 degrees of freedom
Multiple R-squared:  0.6073,	Adjusted R-squared:  0.5992 
F-statistic: 75.01 on 2 and 97 DF,  p-value: < 2.2e-16


Value of test-statistic is: -7.8588 30.8851 

Critical values for test statistics: 
      1pct  5pct 10pct
tau2 -3.46 -2.88 -2.57
phi1  6.52  4.63  3.81

> 
> # ADF None
> summary(ur.df(log(ts_data$GWh), type = "none")) #not stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression none 


Call:
lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.80517 -0.22857 -0.04849  0.21054  1.26121 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
z.lag.1    -0.002477   0.004924  -0.503    0.616    
z.diff.lag  0.591932   0.079761   7.421 4.26e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3911 on 98 degrees of freedom
Multiple R-squared:  0.3598,	Adjusted R-squared:  0.3468 
F-statistic: 27.54 on 2 and 98 DF,  p-value: 3.221e-10


Value of test-statistic is: -0.503 

Critical values for test statistics: 
      1pct  5pct 10pct
tau1 -2.58 -1.95 -1.62

> 
> # KPSS Trend
> summary(ur.kpss(log(ts_data$GWh), type = "tau")) #potentially stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: tau with 4 lags. 

Value of test-statistic is: 0.0147 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216

> 
> # KPSS None
> summary(ur.kpss(log(ts_data$GWh), type = "mu")) #potentially stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: mu with 4 lags. 

Value of test-statistic is: 0.1501 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.347 0.463  0.574 0.739

> 
> # KPSS seasonal
> summary(ur.kpss(log(ts_data$GWh), type = "tau", use.lag = 12)) # not stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: tau with 12 lags. 

Value of test-statistic is: 0.2267 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216

> 
> # KPSS seasonal
> summary(ur.kpss(log(ts_data$GWh), type = "mu", use.lag = 12)) # stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: mu with 12 lags. 

Value of test-statistic is: 0.7665 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.347 0.463  0.574 0.739

> 
> # Unit Root tests - Seasonal differencing
> # ADF Trend
> summary(ur.df(diff(log(ts_data$GWh),lag=12), type = "trend")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression trend 


Call:
lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals:
    Min      1Q  Median      3Q     Max 
-0.6001 -0.1259 -0.0027  0.1402  0.5115 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.0249215  0.0456467   0.546    0.587    
z.lag.1     -1.0572304  0.1506135  -7.019 5.33e-10 ***
tt           0.0006848  0.0008795   0.779    0.438    
z.diff.lag   0.0971454  0.1081649   0.898    0.372    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2082 on 84 degrees of freedom
Multiple R-squared:  0.4873,	Adjusted R-squared:  0.469 
F-statistic: 26.61 on 3 and 84 DF,  p-value: 3.407e-12


Value of test-statistic is: -7.0195 16.427 24.6368 

Critical values for test statistics: 
      1pct  5pct 10pct
tau3 -4.04 -3.45 -3.15
phi2  6.50  4.88  4.16
phi3  8.73  6.49  5.47

> 
> # ADF Drift
> summary(ur.df(diff(log(ts_data$GWh),lag=12), type = "drift")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression drift 


Call:
lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.58951 -0.14013 -0.00582  0.13089  0.51476 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.05540    0.02343   2.365   0.0203 *  
z.lag.1     -1.04396    0.14930  -6.992 5.75e-10 ***
z.diff.lag   0.09101    0.10763   0.846   0.4001    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2077 on 85 degrees of freedom
Multiple R-squared:  0.4836,	Adjusted R-squared:  0.4714 
F-statistic:  39.8 on 2 and 85 DF,  p-value: 6.353e-13


Value of test-statistic is: -6.9924 24.4506 

Critical values for test statistics: 
      1pct  5pct 10pct
tau2 -3.51 -2.89 -2.58
phi1  6.70  4.71  3.86

> 
> # ADF None
> summary(ur.df(diff(log(ts_data$GWh),lag=12),type = "none")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression none 


Call:
lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.53794 -0.07067  0.04403  0.20303  0.58366 

Coefficients:
           Estimate Std. Error t value Pr(>|t|)    
z.lag.1    -0.92870    0.14484  -6.412 7.44e-09 ***
z.diff.lag  0.03283    0.10754   0.305    0.761    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2132 on 86 degrees of freedom
Multiple R-squared:  0.4496,	Adjusted R-squared:  0.4368 
F-statistic: 35.13 on 2 and 86 DF,  p-value: 7.046e-12


Value of test-statistic is: -6.4121 

Critical values for test statistics: 
     1pct  5pct 10pct
tau1 -2.6 -1.95 -1.61

> 
> # KPSS Trend
> summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "tau")) #potentially stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: tau with 3 lags. 

Value of test-statistic is: 0.0535 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216

> 
> # KPSS None
> summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "mu")) #potentially stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: mu with 3 lags. 

Value of test-statistic is: 0.105 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.347 0.463  0.574 0.739

> 
> # KPSS seasonal
> summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "tau", use.lag = 12)) # not stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: tau with 12 lags. 

Value of test-statistic is: 0.0632 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216

> 
> # KPSS seasonal
> summary(ur.kpss(diff(log(ts_data$GWh),lag=12), type = "mu", use.lag = 12)) # stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: mu with 12 lags. 

Value of test-statistic is: 0.1154 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.347 0.463  0.574 0.739

> 
> 
> # Unit Root tests - first lag differencing
> # ADF Trend
> summary(ur.df(diff(log(ts_data$GWh)), type = "trend")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression trend 


Call:
lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.79753 -0.25288 -0.02583  0.22996  0.99357 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -0.0135272  0.0770898  -0.175 0.861082    
z.lag.1     -0.5483246  0.0860535  -6.372 6.66e-09 ***
tt           0.0003643  0.0013181   0.276 0.782838    
z.diff.lag   0.3267448  0.0954224   3.424 0.000912 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3743 on 95 degrees of freedom
Multiple R-squared:  0.3028,	Adjusted R-squared:  0.2808 
F-statistic: 13.75 on 3 and 95 DF,  p-value: 1.601e-07


Value of test-statistic is: -6.3719 13.5814 20.3593 

Critical values for test statistics: 
      1pct  5pct 10pct
tau3 -3.99 -3.43 -3.13
phi2  6.22  4.75  4.07
phi3  8.43  6.49  5.47

> 
> # ADF Drift
> summary(ur.df(diff(log(ts_data$GWh)), type = "drift")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression drift 


Call:
lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.79112 -0.25119 -0.02875  0.24452  1.00030 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.005065   0.037482   0.135 0.892797    
z.lag.1     -0.548572   0.085634  -6.406 5.53e-09 ***
z.diff.lag   0.328027   0.094850   3.458 0.000812 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3725 on 96 degrees of freedom
Multiple R-squared:  0.3022,	Adjusted R-squared:  0.2877 
F-statistic: 20.79 on 2 and 96 DF,  p-value: 3.148e-08


Value of test-statistic is: -6.406 20.5314 

Critical values for test statistics: 
      1pct  5pct 10pct
tau2 -3.46 -2.88 -2.57
phi1  6.52  4.63  3.81

> 
> # ADF None
> summary(ur.df(diff(log(ts_data$GWh)),type = "none")) #potentially stationary

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression none 


Call:
lm(formula = z.diff ~ z.lag.1 - 1 + z.diff.lag)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.78585 -0.24608 -0.02334  0.24948  1.00570 

Coefficients:
           Estimate Std. Error t value Pr(>|t|)    
z.lag.1    -0.54804    0.08511  -6.439  4.6e-09 ***
z.diff.lag  0.32763    0.09432   3.473 0.000769 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3706 on 97 degrees of freedom
Multiple R-squared:  0.3022,	Adjusted R-squared:  0.2879 
F-statistic: 21.01 on 2 and 97 DF,  p-value: 2.628e-08


Value of test-statistic is: -6.4393 

Critical values for test statistics: 
      1pct  5pct 10pct
tau1 -2.58 -1.95 -1.62

> 
> # KPSS Trend
> summary(ur.kpss(diff(log(ts_data$GWh)), type = "tau")) #potentially stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: tau with 4 lags. 

Value of test-statistic is: 0.0242 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216

> 
> # KPSS None
> summary(ur.kpss(diff(log(ts_data$GWh)), type = "mu")) #potentially stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: mu with 4 lags. 

Value of test-statistic is: 0.025 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.347 0.463  0.574 0.739

> 
> # KPSS seasonal
> summary(ur.kpss(diff(log(ts_data$GWh)), type = "tau", use.lag = 12)) # not stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: tau with 12 lags. 

Value of test-statistic is: 0.1988 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.119 0.146  0.176 0.216

> 
> # KPSS seasonal
> summary(ur.kpss(diff(log(ts_data$GWh)), type = "mu", use.lag = 12)) # stationary

####################### 
# KPSS Unit Root Test # 
####################### 

Test is of type: mu with 12 lags. 

Value of test-statistic is: 0.2061 

Critical value for a significance level of: 
                10pct  5pct 2.5pct  1pct
critical values 0.347 0.463  0.574 0.739