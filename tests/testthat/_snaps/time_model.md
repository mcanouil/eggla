# Cubic slope

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_slope", use_car1 = use_car1, id_var = "ID", quiet = TRUE)
      print(x, digits = 4)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 388.5
        Fixed: log(bmi) ~ stats::poly(age, degree = 3) 
                        (Intercept) stats::poly(age, degree = 3)1 
                             2.9693                        5.1084 
      stats::poly(age, degree = 3)2 stats::poly(age, degree = 3)3 
                             0.4256                        0.1261 
      
      Random effects:
       Formula: ~stats::poly(age, degree = 3) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                    StdDev  Corr                        
      (Intercept)                   0.07103 (Intr) s::(,d=3)1 s::(,d=3)2
      stats::poly(age, degree = 3)1 1.21859  0.376                      
      stats::poly(age, degree = 3)2 0.38455 -0.395 -0.736               
      stats::poly(age, degree = 3)3 0.75363  0.626  0.418     -0.870    
      Residual                      0.09850                             
      
      Number of Observations: 534
      Number of Groups: 50 

---

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_slope", use_car1 = use_car1, id_var = "ID", quiet = TRUE)
      print(x, digits = 4)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 430.4
        Fixed: log(bmi) ~ stats::poly(age, degree = 3) 
                        (Intercept) stats::poly(age, degree = 3)1 
                            2.95407                       5.36516 
      stats::poly(age, degree = 3)2 stats::poly(age, degree = 3)3 
                            0.16183                       0.01544 
      
      Random effects:
       Formula: ~stats::poly(age, degree = 3) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                    StdDev  Corr                        
      (Intercept)                   0.04381 (Intr) s::(,d=3)1 s::(,d=3)2
      stats::poly(age, degree = 3)1 0.89645  0.999                      
      stats::poly(age, degree = 3)2 0.18945 -0.995 -0.996               
      stats::poly(age, degree = 3)3 0.55549  0.999  0.998     -0.998    
      Residual                      0.12605                             
      
      Correlation Structure: Continuous AR(1)
       Formula: ~1 | ID 
       Parameter estimate(s):
        Phi 
      0.585 
      Number of Observations: 534
      Number of Groups: 50 

# Linear Splines

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines", use_car1 = use_car1, id_var = "ID", quiet = TRUE)
      print(x, digits = 4)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 389.9
        Fixed: log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) 
                                                                       (Intercept) 
                                                                          2.768214 
         gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0) 
                                                                          0.044675 
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 
                                                                          0.019728 
       gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1 
                                                                         -0.005017 
      
      Random effects:
       Formula: ~gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                                                                   StdDev 
      (Intercept)                                                                  0.07137
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    0.02613
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 0.03887
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  0.04264
      Residual                                                                     0.10006
                                                                                   Corr  
      (Intercept)                                                                  (Intr)
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    -0.415
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1  0.175
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.113
      Residual                                                                           
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))D
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 -0.940                            
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1   0.910                            
      Residual                                                                                                       
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))C
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1                                   
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.989                            
      Residual                                                                                                       
      
      Number of Observations: 534
      Number of Groups: 50 

---

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "linear_splines", use_car1 = use_car1, id_var = "ID", quiet = TRUE)
      print(x, digits = 4)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 428.3
        Fixed: log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) 
                                                                       (Intercept) 
                                                                           2.73551 
         gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0) 
                                                                           0.05024 
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 
                                                                           0.01466 
       gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1 
                                                                          -0.01405 
      
      Random effects:
       Formula: ~gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                                                                   StdDev   
      (Intercept)                                                                  0.0003224
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    0.0091444
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 0.0007244
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  0.0005353
      Residual                                                                     0.1292193
                                                                                   Corr  
      (Intercept)                                                                  (Intr)
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    -0.533
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1  0.008
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1   0.002
      Residual                                                                           
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))D
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 -0.054                            
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.005                            
      Residual                                                                                                       
                                                                                                                     
      (Intercept)                                                                  g(,k=c(5.5,11),d=r(1,3),s=r(0,2))C
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)                                      
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1                                   
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  -0.014                            
      Residual                                                                                                       
      
      Correlation Structure: Continuous AR(1)
       Formula: ~1 | ID 
       Parameter estimate(s):
         Phi 
      0.5984 
      Number of Observations: 534
      Number of Groups: 50 

# Cubic Splines

    Code
      x <- time_model(x = "age", y = "log(bmi)", data = bmigrowth[bmigrowth$sex == 0, ],
      method = "cubic_splines", knots = c(2, 8, 12), use_car1 = use_car1, id_var = "ID",
      quiet = TRUE)
      print(x, digits = 4)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 508.3
        Fixed: log(bmi) ~ gsp(age, knots = c(2, 8, 12), degree = rep(3, 4),      smooth = rep(2, 3)) 
                                                                       (Intercept) 
                                                                           2.63165 
        gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0) 
                                                                           0.52776 
        gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0) 
                                                                          -0.62637 
        gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0) 
                                                                           0.34209 
       gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3 
                                                                          -0.35548 
       gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3 
                                                                           0.02510 
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3 
                                                                          -0.03256 
      
      Random effects:
       Formula: ~gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2,      3)) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                                                                   StdDev            
      (Intercept)                                                                  0.0572644883817716
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)   0.0306233007930484
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)   0.0004049399933645
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)   0.0012223972986199
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3  0.0000000000001383
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3  0.0102638096397364
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3 0.0219470652570501
      Residual                                                                     0.0939545696372524
                                                                                   Corr  
      (Intercept)                                                                  (Intr)
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)   -0.456
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)    0.304
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)    0.312
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3   0.303
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3  -0.298
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3  0.307
      Residual                                                                           
                                                                                                                      
      (Intercept)                                                                  g(,k=c(2,8,12),d=r(3,4),s=r(2,3))D1
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)   -0.984                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)   -0.985                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3  -0.982                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3   0.983                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3 -0.984                             
      Residual                                                                                                        
                                                                                                                      
      (Intercept)                                                                  g(,k=c(2,8,12),d=r(3,4),s=r(2,3))D2
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)    0.998                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3   0.996                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3  -0.997                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3  0.999                             
      Residual                                                                                                        
                                                                                                                      
      (Intercept)                                                                  g(,k=c(2,8,12),d=r(3,4),s=r(2,3))D3
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3   0.996                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3  -0.997                             
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3  0.998                             
      Residual                                                                                                        
                                                                                                                       
      (Intercept)                                                                  g(,k=c(2,8,12),d=r(3,4),s=r(2,3))C(2
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)                                       
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)                                       
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)                                       
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3  -0.995                              
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3  0.997                              
      Residual                                                                                                         
                                                                                                                       
      (Intercept)                                                                  g(,k=c(2,8,12),d=r(3,4),s=r(2,3))C(8
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D1(0)                                       
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D2(0)                                       
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))D3(0)                                       
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(2).3                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(8).3                                      
      gsp(age, knots = c(2, 8, 12), degree = rep(3, 4), smooth = rep(2, 3))C(12).3 -0.996                              
      Residual                                                                                                         
      
      Correlation Structure: Continuous AR(1)
       Formula: ~1 | ID 
       Parameter estimate(s):
         Phi 
      0.4541 
      Number of Observations: 534
      Number of Groups: 50 

---

    Code
      print(compute_outliers(fit = x, method = "cubic_splines", period = c(0, 0.5,
        1.5, 3.5, 6.5, 10, 12, 17), knots = c(1, 8, 12)), digits = 4)
    Output
             parameter  ID Row Distance_Zscore Outlier_Zscore Distance_IQR
         1: AUC_global 082   1          1.6614              0       0.9743
         2: AUC_global 083   2          0.8780              0       0.4248
         3: AUC_global 080   3          0.6542              0       0.3015
         4: AUC_global 031   4          1.8423              0       0.8591
         5: AUC_global 007   5          2.6553              0       1.2322
        ---                                                               
      1136:     AR_bmi 088  44          0.5292              0       0.2511
      1137:     AR_bmi 089  45          0.4937              0       0.2062
      1138:     AR_bmi 092  46          0.5762              0       0.2720
      1139:     AR_bmi 095  47          0.2142              0       0.1102
      1140:     AR_bmi 099  48          1.1135              0       0.5122
            Outlier_IQR Outlier
         1:           0       0
         2:           0       0
         3:           0       0
         4:           0       0
         5:           0       0
        ---                    
      1136:           0       0
      1137:           0       0
      1138:           0       0
      1139:           0       0
      1140:           0       0

# Test covariates

    Code
      print(time_model(x = "age", y = "log(bmi)", cov = c("height"), data = bmigrowth[
        bmigrowth$sex == 0, ], method = "linear_splines", id_var = "ID", use_car1 = TRUE,
      quiet = TRUE), digits = 4)
    Output
      Linear mixed-effects model fit by maximum likelihood
        Data: data 
        Log-likelihood: 452.8
        Fixed: log(bmi) ~ gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) + height 
                                                                       (Intercept) 
                                                                          2.386179 
         gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0) 
                                                                         -0.017835 
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 
                                                                          0.046080 
       gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1 
                                                                          0.003502 
                                                                            height 
                                                                          0.006087 
      
      Random effects:
       Formula: ~gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0,      2)) | ID
       Structure: General positive-definite, Log-Cholesky parametrization
                                                                                   StdDev 
      (Intercept)                                                                  0.03654
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))D1(0)    0.02018
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(5.5).1 0.02981
      gsp(age, knots = c(5.5, 11), degree = rep(1, 3), smooth = rep(0, 2))C(11).1  0.03606
      Residual                                                                     0.11189
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
      0.5044 
      Number of Observations: 534
      Number of Groups: 50 

# time_model

    Code
      print(compute_correlations(fit = res1, method = "cubic_slope", period = c(0,
        0.5, 1.5, 3.5, 6.5, 10, 12, 17), knots = c(1, 8, 12)), digits = 4)
    Output
      $AUC
                 term auc_0--0.5 auc_1.5--3.5 auc_6.5--10 auc_12--17
      1:   auc_0--0.5          1            1           1          1
      2: auc_1.5--3.5          1            1           1          1
      3:  auc_6.5--10          1            1           1          1
      4:   auc_12--17          1            1           1          1
      
      $SLOPE
                   term slope_0--0.5 slope_1.5--3.5 slope_6.5--10 slope_12--17
      1:   slope_0--0.5            1              1             1            1
      2: slope_1.5--3.5            1              1             1            1
      3:  slope_6.5--10            1              1             1            1
      4:   slope_12--17            1              1             1            1
      

---

    Code
      print(compute_aucs(fit = res1, method = "cubic_slope", period = c(0, 0.5, 1.5,
        3.5, 6.5, 10, 12, 17), knots = c(1, 8, 12)), digits = 4)
    Output
          ID auc_0--0.5 auc_1.5--3.5 auc_6.5--10 auc_12--17
      1  082      2.237       49.070     924.790     6090.6
      2  083      2.176       38.929     412.929     2171.8
      3  080      2.193       41.729     553.608     3248.3
      4  031      2.031       15.101    -789.359    -7032.7
      5  007      2.430       80.869    2529.746    18378.5
      6  033      2.081       23.339    -374.062    -3853.7
      7  076      2.262       53.063    1125.804     7629.2
      8  063      2.135       32.189      72.653     -433.6
      9  004      2.125       30.589      -7.896    -1050.3
      10 054      2.239       49.276     934.224     6162.0
      11 089      2.147       34.157     172.458      331.0
      12 095      2.202       43.123     624.306     3789.8
      13 093      2.192       41.539     544.498     3178.9
      14 088      2.104       27.033    -187.147    -2422.4
      15 022      2.148       34.311     180.204      390.2
      16 006      2.016       12.490    -921.351    -8043.6
      17 019      2.265       53.560    1151.410     7825.6
      18 044      2.231       47.953     868.405     5659.0
      19 013      2.210       44.452     691.347     4303.0
      20 025      1.990        8.273   -1133.342    -9666.1
      21 061      2.195       42.172     577.052     3428.6
      22 048      2.265       53.578    1151.745     7827.6
      23 039      2.024       13.828    -853.785    -7526.3
      24 045      2.177       39.162     425.071     2264.9
      25 086      2.113       28.615    -107.448    -1812.2
      26 092      2.238       49.168     929.702     6128.2
      27 085      2.047       17.611    -662.582    -6062.2
      28 059      2.053       18.599    -613.050    -5683.3
      29 046      2.166       37.305     331.069     1545.1
      30 069      2.087       24.199    -330.193    -3517.4
      31 081      2.082       23.346    -373.928    -3852.9
      32 049      2.148       34.248     176.570      362.2
      33 072      2.161       36.387     284.267     1186.5
      34 099      2.195       42.159     576.032     3420.5
      35 064      2.089       24.595    -310.379    -3365.9
      36 009      2.183       40.156     475.194     2648.7
      37 032      2.125       30.537     -10.418    -1069.3
      38 011      2.236       48.878     914.634     6012.4
      39 075      2.292       58.036    1377.259     9554.8
      40 057      1.930       -1.547   -1629.185   -13462.4
      41 012      2.153       35.084     218.805      685.3
      42 065      1.956        2.681   -1415.818   -11828.7
      43 041      2.008       11.271    -982.854    -8514.4
      44 035      2.099       26.230    -227.823    -2733.9
      45 016      2.155       35.488     239.410      843.3
      46 010      2.150       34.684     198.853      532.9
      47 034      2.191       41.405     538.289     3131.8
      48 001      2.332       64.650    1710.750    12107.8
      49 005      2.218       45.859     762.502     4848.0
      50 068      2.254       51.787    1061.892     7140.3

---

    Code
      print(compute_slopes(fit = res1, method = "cubic_slope", period = c(0, 0.5, 1.5,
        3.5, 6.5, 10, 12, 17), knots = c(1, 8, 12)), digits = 4)
    Output
          ID pred_period_0 pred_period_0.5 pred_period_1.5 pred_period_3.5
      1  082         2.981           5.995          13.160         39.5368
      2  083         2.961           5.760          11.866         28.2697
      3  080         2.967           5.826          12.226         31.3748
      4  031         2.914           5.209           8.822          1.7991
      5  007         3.044           6.730          17.222         74.8661
      6  033         2.930           5.400           9.875         10.9474
      7  076         2.989           6.088          13.672         43.9689
      8  063         2.948           5.604          11.005         20.7810
      9  004         2.945           5.567          10.800         19.0053
      10 054         2.981           6.001          13.190         39.7573
      11 089         2.952           5.649          11.255         22.9708
      12 095         2.969           5.858          12.403         32.9269
      13 093         2.966           5.821          12.200         31.1679
      14 088         2.938           5.484          10.345         15.0563
      15 022         2.952           5.653          11.275         23.1422
      16 006         2.909           5.148           8.489         -1.1033
      17 019         2.990           6.098          13.734         44.5259
      18 044         2.979           5.969          13.018         38.2954
      19 013         2.972           5.888          12.572         34.4033
      20 025         2.901           5.049           7.947         -5.7806
      21 061         2.967           5.835          12.278         31.8771
      22 048         2.990           6.100          13.738         44.5407
      23 039         2.912           5.179           8.660          0.3839
      24 045         2.961           5.765          11.894         28.5323
      25 086         2.941           5.521          10.548         16.8124
      26 092         2.981           5.997          13.173         39.6457
      27 085         2.919           5.266           9.142          4.5887
      28 059         2.921           5.290           9.269          5.6840
      29 046         2.958           5.722          11.658         26.4666
      30 069         2.932           5.419           9.984         11.9071
      31 081         2.930           5.400           9.877         10.9539
      32 049         2.952           5.652          11.269         23.0678
      33 072         2.956           5.702          11.543         25.4418
      34 099         2.967           5.835          12.278         31.8593
      35 064         2.933           5.428          10.035         12.3458
      36 009         2.963           5.788          12.021         29.6359
      37 032         2.944           5.566          10.794         18.9479
      38 011         2.981           5.991          13.138         39.3198
      39 075         2.999           6.202          14.306         49.4977
      40 057         2.881           4.823           6.694        -16.6933
      41 012         2.953           5.671          11.375         23.9974
      42 065         2.889           4.921           7.234        -11.9964
      43 041         2.906           5.120           8.333         -2.4580
      44 035         2.936           5.466          10.244         14.1628
      45 016         2.954           5.680          11.426         24.4482
      46 010         2.953           5.662          11.323         23.5552
      47 034         2.966           5.817          12.180         31.0241
      48 001         3.012           6.356          15.152         56.8433
      49 005         2.975           5.921          12.752         35.9674
      50 068         2.986           6.058          13.508         42.5555
         pred_period_6.5 pred_period_10 pred_period_12 pred_period_17 slope_0--0.5
      1          141.312        423.371         696.86        1869.03        6.027
      2           72.632        174.282         266.13         641.04        5.599
      3           91.516        242.727         384.47         978.35        5.719
      4          -88.692       -410.787        -745.59       -2243.28        4.590
      5          356.658       1204.406        2047.48        5719.61        7.373
      6          -32.962       -208.701        -396.15       -1247.11        4.939
      7          168.290        521.181         865.99        2351.15        6.198
      8           26.977          8.686         -20.24        -175.41        5.313
      9           16.168        -30.510         -88.01        -368.64        5.245
      10         142.591        427.942         704.74        1891.39        6.039
      11          40.361         57.268          63.79          64.23        5.395
      12         100.998        277.139         443.98        1148.07        5.777
      13          90.288        238.305         376.84         956.64        5.709
      14          -7.887       -117.734        -238.84        -798.58        5.094
      15          41.402         61.035          70.30          82.77        5.402
      16        -106.398       -475.028        -856.69       -2560.08        4.479
      17         171.720        533.652         887.56        2412.71        6.217
      18         133.746        395.934         649.42        1733.79        5.980
      19         109.994        309.761         500.39        1308.87        5.833
      20        -134.852       -578.174       -1035.04       -3068.46        4.298
      21          94.649        254.158         404.26        1034.89        5.734
      22         171.773        533.802         887.80        2413.32        6.219
      23         -97.332       -442.147        -799.83       -2397.98        4.536
      24          74.257        180.197         276.36         670.25        5.607
      25           2.808        -78.951        -171.77        -607.39        5.161
      26         141.971        425.761         701.00        1880.82        6.032
      27         -71.681       -349.094        -638.91       -1939.16        4.695
      28         -65.030       -324.998        -597.25       -1820.44        4.738
      29          61.648        134.447         197.24         444.65        5.529
      30         -27.081       -187.343        -359.20       -1141.73        4.974
      31         -32.940       -208.641        -396.06       -1246.88        4.940
      32          40.919         59.260          67.23          73.98        5.401
      33          55.372        111.664         157.84         332.29        5.492
      34          94.516        253.654         403.38        1032.34        5.735
      35         -24.421       -177.704        -342.54       -1094.25        4.992
      36          80.983        204.588         318.54         790.48        5.649
      37          15.826        -31.730         -90.11        -374.57        5.243
      38         139.956        418.418         688.28        1844.52        6.021
      39         202.022        643.561        1077.63        2954.61        6.407
      40        -201.381       -819.473       -1452.31       -4258.09        3.883
      41          46.587         79.810         102.75         175.24        5.435
      42        -172.753       -715.637       -1272.74       -3746.13        4.063
      43        -114.652       -504.954        -908.44       -2707.58        4.428
      44         -13.343       -137.530        -273.07        -896.19        5.060
      45          49.349         89.842         120.11         224.75        5.452
      46          43.906         70.108          85.99         127.49        5.419
      47          89.448        235.293         371.64         941.87        5.702
      48         246.774        805.842        1358.24        3754.58        6.688
      49         119.539        344.393         560.29        1479.66        5.892
      50         159.708        490.090         812.24        2197.97        6.143
         slope_1.5--3.5 slope_6.5--10 slope_12--17
      1         13.1882        80.588    234.43372
      2          8.2019        29.043     74.98292
      3          9.5746        43.203    118.77617
      4         -3.5115       -92.027   -299.53786
      5         28.8222       242.214    734.42762
      6          0.5361       -50.211   -170.19274
      7         15.1482       100.826    297.03294
      8          4.8878        -5.226    -31.03394
      9          4.1026       -13.336    -56.12429
      10        13.2835        81.529    237.32939
      11         5.8579         4.831      0.08758
      12        10.2621        50.326    140.81644
      13         9.4841        42.291    115.96146
      14         2.3555       -31.385   -111.94969
      15         5.9337         5.610      2.49400
      16        -4.7961      -105.323   -340.67774
      17        15.3959       103.409    305.02903
      18        12.6387        74.911    216.87375
      19        10.9155        57.076    161.69605
      20        -6.8639      -126.663   -406.68517
      21         9.7993        45.574    126.12505
      22        15.4014       103.437    305.10321
      23        -4.1379       -98.519   -319.62914
      24         8.3190        30.268     78.77629
      25         3.1322       -23.360    -87.12421
      26        13.2361        81.083    235.96485
      27        -2.2765       -79.261   -260.05053
      28        -1.7926       -74.276   -244.63647
      29         7.4042        20.800     49.48195
      30         0.9618       -45.789   -156.50537
      31         0.5384       -50.200   -170.16454
      32         5.8996         5.240      1.35150
      33         6.9496        16.083     34.89050
      34         9.7906        45.468    125.79191
      35         1.1554       -43.795   -150.34084
      36         8.8075        35.316     94.38796
      37         4.0772       -13.588    -56.89111
      38        13.0910        79.561    231.24780
      39        17.5958       126.154    375.39515
      40       -11.6936      -176.598   -561.15668
      41         6.3114         9.492     14.49624
      42        -9.6154      -155.110   -494.67772
      43        -5.3956      -111.515   -359.82847
      44         1.9597       -35.482   -124.62449
      45         6.5113        11.570     20.92748
      46         6.1159         7.486      8.29936
      47         9.4219        41.670    114.04677
      48        20.8457       159.734    479.26778
      49        11.6079        64.244    183.87423
      50        14.5238        94.395    277.14660

---

    Code
      print(compute_apar(fit = res1, from = s)[AP | AR], digits = 4)
    Output
      Empty data.table (0 rows and 5 cols): egg_id,egg_ageyears,egg_bmi,AP,AR

---

    Code
      print(compute_apar(fit = res1, from = s)[AP | AR], digits = 4)
    Output
          egg_id egg_ageyears egg_bmi    AP    AR
       1:    001         0.25   18.29  TRUE FALSE
       2:    001         0.50   17.68 FALSE  TRUE
       3:    004         0.75   19.64  TRUE FALSE
       4:    004         2.00   16.23 FALSE  TRUE
       5:    006         0.50   15.44  TRUE FALSE
       6:    006         0.75   14.80 FALSE  TRUE
       7:    009         0.50   19.01  TRUE FALSE
       8:    009         2.00   15.31 FALSE  TRUE
       9:    010         0.50   17.53  TRUE FALSE
      10:    010         1.00   16.64 FALSE  TRUE
      11:    011         0.50   17.96  TRUE FALSE
      12:    011         0.75   16.95 FALSE  TRUE
      13:    012         0.25   17.88 FALSE  TRUE
      14:    012         0.75   20.49  TRUE FALSE
      15:    016         1.00   19.48  TRUE FALSE
      16:    016         3.00   17.00 FALSE  TRUE
      17:    019         0.75   17.51  TRUE FALSE
      18:    019         2.00   14.84 FALSE  TRUE
      19:    022         0.75   19.22  TRUE FALSE
      20:    022         2.00   17.61 FALSE  TRUE
      21:    025         0.75   19.18  TRUE FALSE
      22:    025         4.00   18.46 FALSE  TRUE
      23:    031         0.75   17.88  TRUE FALSE
      24:    031         1.00   17.70 FALSE  TRUE
      25:    032         0.25   16.91  TRUE FALSE
      26:    032         0.50   16.25 FALSE  TRUE
      27:    033         4.00   25.65  TRUE FALSE
      28:    033         5.00   20.29 FALSE  TRUE
      29:    034         0.50   19.82  TRUE FALSE
      30:    034         1.00   17.31 FALSE  TRUE
      31:    035         0.25   20.33  TRUE FALSE
      32:    035         0.75   17.34 FALSE  TRUE
      33:    039         1.00   21.61  TRUE FALSE
      34:    039         6.00   19.00 FALSE  TRUE
      35:    041         1.00   18.32  TRUE FALSE
      36:    041         2.00   16.92 FALSE  TRUE
      37:    044         1.00   23.76  TRUE FALSE
      38:    044         3.00   20.28 FALSE  TRUE
      39:    045         0.25   14.15 FALSE  TRUE
      40:    045         0.75   18.96  TRUE FALSE
      41:    046         0.50   18.93  TRUE FALSE
      42:    046         1.00   17.54 FALSE  TRUE
      43:    048         0.25   19.80  TRUE FALSE
      44:    048         2.00   17.61 FALSE  TRUE
      45:    049         0.75   17.00  TRUE FALSE
      46:    049         1.00   16.49 FALSE  TRUE
      47:    054         0.50   20.51  TRUE FALSE
      48:    054         0.75   19.51 FALSE  TRUE
      49:    057         0.50   17.99  TRUE FALSE
      50:    057         0.75   17.54 FALSE  TRUE
      51:    059         0.75   17.98  TRUE FALSE
      52:    059         2.00   16.48 FALSE  TRUE
      53:    061         0.50   18.98  TRUE FALSE
      54:    061         0.75   18.74 FALSE  TRUE
      55:    063         0.25   16.00 FALSE  TRUE
      56:    063         0.50   17.12  TRUE FALSE
      57:    064         0.25   11.08 FALSE  TRUE
      58:    064         0.75   16.10  TRUE FALSE
      59:    065         0.75   16.01  TRUE FALSE
      60:    065         2.00   14.76 FALSE  TRUE
      61:    068         0.50   17.84  TRUE FALSE
      62:    068         0.75   17.61 FALSE  TRUE
      63:    069         0.75   16.86  TRUE FALSE
      64:    069         2.00   15.49 FALSE  TRUE
      65:    072         0.50   19.05  TRUE FALSE
      66:    072         1.00   17.58 FALSE  TRUE
      67:    075         0.50   14.93  TRUE FALSE
      68:    075         0.75   14.91 FALSE  TRUE
      69:    076         0.50   19.00  TRUE FALSE
      70:    076         1.00   17.02 FALSE  TRUE
      71:    080         0.25   15.14  TRUE FALSE
      72:    080         0.50   14.97 FALSE  TRUE
      73:    081         0.50   17.01  TRUE FALSE
      74:    081         0.75   15.52 FALSE  TRUE
      75:    082         0.50   18.10  TRUE FALSE
      76:    082         0.75   16.32 FALSE  TRUE
      77:    083         0.75   17.41  TRUE FALSE
      78:    083         3.00   15.03 FALSE  TRUE
      79:    085         0.25   14.34  TRUE FALSE
      80:    085         0.50   14.28 FALSE  TRUE
      81:    086         0.50   16.76  TRUE FALSE
      82:    086         0.75   16.55 FALSE  TRUE
      83:    088         0.25   19.13  TRUE FALSE
      84:    088         0.75   17.06 FALSE  TRUE
      85:    089         1.00   17.53  TRUE FALSE
      86:    089         2.00   15.63 FALSE  TRUE
      87:    092         0.75   19.21  TRUE FALSE
      88:    092         2.00   17.66 FALSE  TRUE
      89:    093         0.50   17.12  TRUE FALSE
      90:    093         0.75   16.12 FALSE  TRUE
      91:    095         0.50   17.22  TRUE FALSE
      92:    095         0.75   15.16 FALSE  TRUE
      93:    099         0.50   18.81  TRUE FALSE
      94:    099         2.00   17.98 FALSE  TRUE
          egg_id egg_ageyears egg_bmi    AP    AR

