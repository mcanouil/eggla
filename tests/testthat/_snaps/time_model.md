# Cubic slope

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_slope", use_car1 = use_car1, id_var = "ID", quiet = TRUE)

---

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_slope", use_car1 = use_car1, id_var = "ID", quiet = TRUE)

# Linear Splines

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines", use_car1 = use_car1, id_var = "ID", quiet = TRUE)

---

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines", use_car1 = use_car1, id_var = "ID", quiet = TRUE)

# Cubic Splines

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_splines", use_car1 = use_car1, id_var = "ID", quiet = TRUE)

# Test covariates

    Code
      time_model(x = "age", y = "log(bmi)", cov = c("height"), data = bmigrowth[
        bmigrowth$sex == 0, ], method = "linear_splines", id_var = "ID", use_car1 = TRUE,
      quiet = TRUE)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 452.8264
        Fixed: log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) + height 
                                                                       (Intercept) 
                                                                       2.386178863 
         gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0) 
                                                                      -0.017834956 
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 
                                                                       0.046079538 
       gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1 
                                                                       0.003501546 
                                                                            height 
                                                                       0.006086980 
      
      Random effects:
       Formula: ~gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                                                                   StdDev    
      (Intercept)                                                                  0.03653973
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    0.02017970
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 0.02981305
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  0.03605583
      Residual                                                                     0.11189030
                                                                                   Corr  
      (Intercept)                                                                  (Intr)
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    -0.247
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1  0.237
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.233
      Residual                                                                           
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))D
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 -0.995                            
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1   0.993                            
      Residual                                                                                                       
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))C
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1                                   
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.995                            
      Residual                                                                                                       
      
      Correlation Structure: Continuous AR(1)
       Formula: ~1 | ID 
       Parameter estimate(s):
            Phi 
      0.5044046 
      Number of Observations: 534
      Number of Groups: 50 

# Test covariates with transformation

    Code
      time_model(x = "age", y = "log(bmi)", cov = c("log(height)"), data = bmigrowth[
        bmigrowth$sex == 0, ], method = "linear_splines", id_var = "ID", use_car1 = TRUE,
      quiet = TRUE)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 466.4923
        Fixed: log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) + log(height) 
                                                                       (Intercept) 
                                                                       0.768959726 
         gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0) 
                                                                      -0.015581627 
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 
                                                                       0.061930245 
       gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1 
                                                                      -0.003480598 
                                                                       log(height) 
                                                                       0.484255351 
      
      Random effects:
       Formula: ~gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                                                                   StdDev    
      (Intercept)                                                                  0.04455077
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    0.02091494
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 0.03053103
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  0.03729899
      Residual                                                                     0.10717076
                                                                                   Corr  
      (Intercept)                                                                  (Intr)
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    -0.297
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1  0.157
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.142
      Residual                                                                           
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))D
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 -0.985                            
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1   0.982                            
      Residual                                                                                                       
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))C
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1                                   
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.998                            
      Residual                                                                                                       
      
      Correlation Structure: Continuous AR(1)
       Formula: ~1 | ID 
       Parameter estimate(s):
            Phi 
      0.4898987 
      Number of Observations: 534
      Number of Groups: 50 

# time_model

    Code
      cor1 <- compute_correlations(fit = res1, method = "cubic_slope", period = c(0,
        0.5, 1.5, 3.5, 6.5, 10, 12, 17), knots = c(2, 8, 12))
    Message
      
      Correlation method: 'pearson'
      Missing treated using: 'pairwise.complete.obs'
      
      
      Correlation method: 'pearson'
      Missing treated using: 'pairwise.complete.obs'
      

---

    Code
      auc1 <- compute_aucs(fit = res1, method = "cubic_slope", period = c(0, 0.5, 1.5,
        3.5, 6.5, 10, 12, 17), knots = c(2, 8, 12))

---

    Code
      slopes1 <- compute_slopes(fit = res1, method = "cubic_slope", period = c(0, 0.5,
        1.5, 3.5, 6.5, 10, 12, 17), knots = c(2, 8, 12))

---

    Code
      compute_apar(fit = res1, from = s)[AP | AR]
    Output
      Empty data.table (0 rows and 5 cols): egg_id,egg_ageyears,egg_bmi,AP,AR

---

    Code
      compute_apar(fit = res1, from = s)[AP | AR]
    Output
          egg_id egg_ageyears  egg_bmi    AP    AR
       1:    001         0.25 18.29230  TRUE FALSE
       2:    001         0.50 17.67517 FALSE  TRUE
       3:    004         0.75 19.63785  TRUE FALSE
       4:    004         2.00 16.22700 FALSE  TRUE
       5:    006         0.50 15.43616  TRUE FALSE
       6:    006         0.75 14.79990 FALSE  TRUE
       7:    009         0.50 19.01471  TRUE FALSE
       8:    009         2.00 15.30554 FALSE  TRUE
       9:    010         0.50 17.53308  TRUE FALSE
      10:    010         1.00 16.64469 FALSE  TRUE
      11:    011         0.50 17.95501  TRUE FALSE
      12:    011         0.75 16.95240 FALSE  TRUE
      13:    012         0.25 17.87567 FALSE  TRUE
      14:    012         0.75 20.49335  TRUE FALSE
      15:    016         1.00 19.47950  TRUE FALSE
      16:    016         3.00 17.00218 FALSE  TRUE
      17:    019         0.75 17.51276  TRUE FALSE
      18:    019         2.00 14.84408 FALSE  TRUE
      19:    022         0.75 19.22457  TRUE FALSE
      20:    022         2.00 17.60505 FALSE  TRUE
      21:    025         0.75 19.18259  TRUE FALSE
      22:    025         4.00 18.46220 FALSE  TRUE
      23:    031         0.75 17.88286  TRUE FALSE
      24:    031         1.00 17.70139 FALSE  TRUE
      25:    032         0.25 16.90858  TRUE FALSE
      26:    032         0.50 16.25139 FALSE  TRUE
      27:    033         4.00 25.64615  TRUE FALSE
      28:    033         5.00 20.28575 FALSE  TRUE
      29:    034         0.50 19.82073  TRUE FALSE
      30:    034         1.00 17.30877 FALSE  TRUE
      31:    035         0.25 20.33201  TRUE FALSE
      32:    035         0.75 17.34018 FALSE  TRUE
      33:    039         1.00 21.61081  TRUE FALSE
      34:    039         6.00 19.00150 FALSE  TRUE
      35:    041         1.00 18.31564  TRUE FALSE
      36:    041         2.00 16.91935 FALSE  TRUE
      37:    044         1.00 23.75700  TRUE FALSE
      38:    044         3.00 20.27723 FALSE  TRUE
      39:    045         0.25 14.14560 FALSE  TRUE
      40:    045         0.75 18.96277  TRUE FALSE
      41:    046         0.50 18.93383  TRUE FALSE
      42:    046         1.00 17.53731 FALSE  TRUE
      43:    048         0.25 19.80349  TRUE FALSE
      44:    048         2.00 17.60903 FALSE  TRUE
      45:    049         0.75 16.99720  TRUE FALSE
      46:    049         1.00 16.48844 FALSE  TRUE
      47:    054         0.50 20.51292  TRUE FALSE
      48:    054         0.75 19.50746 FALSE  TRUE
      49:    057         0.50 17.99475  TRUE FALSE
      50:    057         0.75 17.54141 FALSE  TRUE
      51:    059         0.75 17.98464  TRUE FALSE
      52:    059         2.00 16.48296 FALSE  TRUE
      53:    061         0.50 18.98361  TRUE FALSE
      54:    061         0.75 18.73795 FALSE  TRUE
      55:    063         0.25 15.99917 FALSE  TRUE
      56:    063         0.50 17.12083  TRUE FALSE
      57:    064         0.25 11.07783 FALSE  TRUE
      58:    064         0.75 16.09719  TRUE FALSE
      59:    065         0.75 16.01349  TRUE FALSE
      60:    065         2.00 14.75985 FALSE  TRUE
      61:    068         0.50 17.83902  TRUE FALSE
      62:    068         0.75 17.61418 FALSE  TRUE
      63:    069         0.75 16.86160  TRUE FALSE
      64:    069         2.00 15.49005 FALSE  TRUE
      65:    072         0.50 19.05061  TRUE FALSE
      66:    072         1.00 17.58378 FALSE  TRUE
      67:    075         0.50 14.93427  TRUE FALSE
      68:    075         0.75 14.90528 FALSE  TRUE
      69:    076         0.50 18.99907  TRUE FALSE
      70:    076         1.00 17.02181 FALSE  TRUE
      71:    080         0.25 15.13898  TRUE FALSE
      72:    080         0.50 14.96752 FALSE  TRUE
      73:    081         0.50 17.01452  TRUE FALSE
      74:    081         0.75 15.52272 FALSE  TRUE
      75:    082         0.50 18.09997  TRUE FALSE
      76:    082         0.75 16.31650 FALSE  TRUE
      77:    083         0.75 17.41036  TRUE FALSE
      78:    083         3.00 15.03045 FALSE  TRUE
      79:    085         0.25 14.33707  TRUE FALSE
      80:    085         0.50 14.27659 FALSE  TRUE
      81:    086         0.50 16.76003  TRUE FALSE
      82:    086         0.75 16.55030 FALSE  TRUE
      83:    088         0.25 19.13413  TRUE FALSE
      84:    088         0.75 17.05547 FALSE  TRUE
      85:    089         1.00 17.53319  TRUE FALSE
      86:    089         2.00 15.63478 FALSE  TRUE
      87:    092         0.75 19.20770  TRUE FALSE
      88:    092         2.00 17.65842 FALSE  TRUE
      89:    093         0.50 17.12487  TRUE FALSE
      90:    093         0.75 16.11980 FALSE  TRUE
      91:    095         0.50 17.21890  TRUE FALSE
      92:    095         0.75 15.15802 FALSE  TRUE
      93:    099         0.50 18.80661  TRUE FALSE
      94:    099         2.00 17.97986 FALSE  TRUE
          egg_id egg_ageyears  egg_bmi    AP    AR

---

    Code
      outliers1 <- compute_outliers(fit = res1, method = "cubic_slope", period = c(0,
        0.5, 1.5, 3.5, 6.5, 10, 12, 17), knots = c(2, 8, 12))

