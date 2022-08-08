# egg_model

    Code
      res2 <- egg_model(formula = log(bmi) ~ age, data = bmigrowth[bmigrowth[["sex"]] ==
        0, ], id_var = "ID", random_complexity = 1, use_car1 = TRUE, quiet = TRUE)

---

    Code
      cor2 <- egg_correlations(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12,
        17), knots = c(2, 8, 12))

---

    Code
      auc2 <- egg_aucs(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12))

---

    Code
      slopes2 <- egg_slopes(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(2, 8, 12))

---

    Code
      compute_apar(fit = res2, from = s)[AP | AR]
    Output
           egg_id egg_ageyears  egg_bmi    AP    AR
        1:    001         1.35 19.14348  TRUE FALSE
        2:    001         2.70 18.56972 FALSE  TRUE
        3:    004         1.35 18.24414  TRUE FALSE
        4:    004         2.75 17.63306 FALSE  TRUE
        5:    005         1.35 18.57345  TRUE FALSE
        6:    005         2.65 18.08016 FALSE  TRUE
        7:    006         1.30 17.72611  TRUE FALSE
        8:    006         3.00 16.85654 FALSE  TRUE
        9:    007         1.45 18.92720  TRUE FALSE
       10:    007         2.40 18.66142 FALSE  TRUE
       11:    009         1.30 18.23983  TRUE FALSE
       12:    009         2.85 17.52267 FALSE  TRUE
       13:    010         1.40 18.66062  TRUE FALSE
       14:    010         2.50 18.27368 FALSE  TRUE
       15:    011         1.35 18.54129  TRUE FALSE
       16:    011         2.75 17.91046 FALSE  TRUE
       17:    012         1.30 18.93838  TRUE FALSE
       18:    012         2.85 18.18404 FALSE  TRUE
       19:    013         1.40 18.87964  TRUE FALSE
       20:    013         2.50 18.48814 FALSE  TRUE
       21:    016         1.30 18.17512  TRUE FALSE
       22:    016         2.85 17.45785 FALSE  TRUE
       23:    019         1.35 18.51276  TRUE FALSE
       24:    019         2.65 18.00413 FALSE  TRUE
       25:    022         1.35 18.74773  TRUE FALSE
       26:    022         2.70 18.18557 FALSE  TRUE
       27:    025         1.30 18.28952  TRUE FALSE
       28:    025         2.90 17.46855 FALSE  TRUE
       29:    031         1.30 18.30042  TRUE FALSE
       30:    031         2.90 17.49976 FALSE  TRUE
       31:    032         1.30 18.14175  TRUE FALSE
       32:    032         2.95 17.27980 FALSE  TRUE
       33:    033         1.40 19.23263  TRUE FALSE
       34:    033         2.60 18.77207 FALSE  TRUE
       35:    034         1.35 18.67371  TRUE FALSE
       36:    034         2.60 18.18115 FALSE  TRUE
       37:    035         1.35 18.57172  TRUE FALSE
       38:    035         2.65 18.07412 FALSE  TRUE
       39:    039         1.25 18.40668  TRUE FALSE
       40:    039         3.20 17.21829 FALSE  TRUE
       41:    041         1.30 18.44859  TRUE FALSE
       42:    041         2.80 17.75584 FALSE  TRUE
       43:    044         1.40 19.59162  TRUE FALSE
       44:    044         2.55 19.14135 FALSE  TRUE
       45:    045         1.30 18.25270  TRUE FALSE
       46:    045         2.95 17.37372 FALSE  TRUE
       47:    046         1.30 18.44357  TRUE FALSE
       48:    046         2.85 17.71208 FALSE  TRUE
       49:    048         1.30 18.37269  TRUE FALSE
       50:    048         2.85 17.62396 FALSE  TRUE
       51:    049         1.40 18.79827  TRUE FALSE
       52:    049         2.60 18.32169 FALSE  TRUE
       53:    054         1.30 18.45926  TRUE FALSE
       54:    054         3.00 17.51728 FALSE  TRUE
       55:    057         1.30 18.23656  TRUE FALSE
       56:    057         2.90 17.43064 FALSE  TRUE
       57:    059         1.35 18.39598  TRUE FALSE
       58:    059         2.75 17.74932 FALSE  TRUE
       59:    061         1.30 18.07717  TRUE FALSE
       60:    061         2.95 17.23973 FALSE  TRUE
       61:    063         1.35 18.44728  TRUE FALSE
       62:    063         2.75 17.83033 FALSE  TRUE
       63:    064         1.35 17.88300  TRUE FALSE
       64:    064         2.70 17.31475 FALSE  TRUE
       65:    065         1.35 18.12595  TRUE FALSE
       66:    065         2.65 17.62558 FALSE  TRUE
       67:    068         1.35 18.59519  TRUE FALSE
       68:    068         2.70 18.02235 FALSE  TRUE
       69:    069         1.30 18.04597  TRUE FALSE
       70:    069         2.90 17.25580 FALSE  TRUE
       71:    072         1.30 18.60264  TRUE FALSE
       72:    072         2.85 17.82834 FALSE  TRUE
       73:    075         1.30 17.99132  TRUE FALSE
       74:    075         2.85 17.27050 FALSE  TRUE
       75:    076         1.40 18.78452  TRUE FALSE
       76:    076         2.60 18.31317 FALSE  TRUE
       77:    080         1.40 18.56493  TRUE FALSE
       78:    080         2.60 18.11685 FALSE  TRUE
       79:    081         1.20 17.60487  TRUE FALSE
       80:    081         3.30 16.33509 FALSE  TRUE
       81:    082         1.35 18.54630  TRUE FALSE
       82:    082         2.70 17.97934 FALSE  TRUE
       83:    083         1.25 17.75899  TRUE FALSE
       84:    083         3.20 16.62091 FALSE  TRUE
       85:    085         1.30 17.78173  TRUE FALSE
       86:    085         3.00 16.91058 FALSE  TRUE
       87:    086         1.30 18.21552  TRUE FALSE
       88:    086         2.90 17.41084 FALSE  TRUE
       89:    088         1.30 18.57847  TRUE FALSE
       90:    088         2.80 17.86210 FALSE  TRUE
       91:    089         1.30 18.11834  TRUE FALSE
       92:    089         2.80 17.43654 FALSE  TRUE
       93:    092         1.30 18.52710  TRUE FALSE
       94:    092         2.85 17.77197 FALSE  TRUE
       95:    093         1.45 19.15325  TRUE FALSE
       96:    093         2.35 18.90999 FALSE  TRUE
       97:    095         1.35 18.40417  TRUE FALSE
       98:    095         2.65 17.90366 FALSE  TRUE
       99:    099         1.40 18.78684  TRUE FALSE
      100:    099         2.55 18.34631 FALSE  TRUE
           egg_id egg_ageyears  egg_bmi    AP    AR

---

    Code
      compute_apar(fit = res2, from = s)[AP | AR]
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
      outliers2 <- egg_outliers(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12,
        17), knots = c(2, 8, 12))

