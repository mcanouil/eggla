# egg_model

    Code
      res2 <- egg_model(formula = log(bmi) ~ age, data = bmigrowth[bmigrowth[["sex"]] ==
        0, ], id_var = "ID", random_complexity = 1, use_car1 = TRUE, quiet = TRUE)

---

    Code
      cor2 <- egg_correlations(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12,
        17), knots = c(1, 8, 12))

---

    Code
      auc2 <- egg_aucs(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12))

---

    Code
      slopes2 <- egg_slopes(fit = res2, period = c(0, 0.5, 1.5, 3.5, 6.5, 10, 12, 17),
      knots = c(1, 8, 12))

---

    Code
      compute_apar(fit = res2, from = s)[AP | AR]
    Output
           egg_id egg_ageyears  egg_bmi    AP    AR
        1:    001         0.80 18.78090  TRUE FALSE
        2:    001         1.90 18.43798 FALSE  TRUE
        3:    004         0.80 17.56589  TRUE FALSE
        4:    004         1.90 17.23783 FALSE  TRUE
        5:    005         0.80 17.87731  TRUE FALSE
        6:    005         1.75 17.62135 FALSE  TRUE
        7:    006         0.75 17.04349  TRUE FALSE
        8:    006         2.25 16.49791 FALSE  TRUE
        9:    007         0.85 18.14071  TRUE FALSE
       10:    007         1.40 18.06312 FALSE  TRUE
       11:    009         0.80 17.65039  TRUE FALSE
       12:    009         2.05 17.22419 FALSE  TRUE
       13:    010         0.85 17.89049  TRUE FALSE
       14:    010         1.60 17.72315 FALSE  TRUE
       15:    011         0.80 17.94326  TRUE FALSE
       16:    011         1.95 17.57238 FALSE  TRUE
       17:    012         0.75 18.65648  TRUE FALSE
       18:    012         2.15 18.12367 FALSE  TRUE
       19:    013         0.80 18.24675  TRUE FALSE
       20:    013         1.60 18.07070 FALSE  TRUE
       21:    016         0.80 17.56598  TRUE FALSE
       22:    016         2.05 17.12541 FALSE  TRUE
       23:    019         0.80 17.84645  TRUE FALSE
       24:    019         1.80 17.58028 FALSE  TRUE
       25:    022         0.80 18.22665  TRUE FALSE
       26:    022         1.85 17.91214 FALSE  TRUE
       27:    025         0.75 17.82719  TRUE FALSE
       28:    025         2.20 17.28074 FALSE  TRUE
       29:    031         0.75 17.81573  TRUE FALSE
       30:    031         2.15 17.29549 FALSE  TRUE
       31:    032         0.75 17.62468  TRUE FALSE
       32:    032         2.25 17.04937 FALSE  TRUE
       33:    033         0.80 18.80519  TRUE FALSE
       34:    033         1.70 18.56141 FALSE  TRUE
       35:    034         0.80 18.04390  TRUE FALSE
       36:    034         1.75 17.78726 FALSE  TRUE
       37:    035         0.80 17.90376  TRUE FALSE
       38:    035         1.75 17.64974 FALSE  TRUE
       39:    039         0.75 18.26307  TRUE FALSE
       40:    039         2.65 17.32074 FALSE  TRUE
       41:    041         0.80 17.91687  TRUE FALSE
       42:    041         2.05 17.49133 FALSE  TRUE
       43:    044         0.80 19.27156  TRUE FALSE
       44:    044         1.70 19.02214 FALSE  TRUE
       45:    045         0.75 17.80221  TRUE FALSE
       46:    045         2.25 17.20604 FALSE  TRUE
       47:    046         0.80 17.96000  TRUE FALSE
       48:    046         2.10 17.49673 FALSE  TRUE
       49:    048         0.80 17.87357  TRUE FALSE
       50:    048         2.10 17.39465 FALSE  TRUE
       51:    049         0.80 18.18459  TRUE FALSE
       52:    049         1.70 17.94992 FALSE  TRUE
       53:    054         0.75 18.14681  TRUE FALSE
       54:    054         2.35 17.48535 FALSE  TRUE
       55:    057         0.75 17.72935  TRUE FALSE
       56:    057         2.20 17.20160 FALSE  TRUE
       57:    059         0.80 17.81347  TRUE FALSE
       58:    059         1.95 17.44072 FALSE  TRUE
       59:    061         0.75 17.53897  TRUE FALSE
       60:    061         2.20 16.99922 FALSE  TRUE
       61:    063         0.80 17.84310  TRUE FALSE
       62:    063         1.90 17.49725 FALSE  TRUE
       63:    064         0.80 17.01552  TRUE FALSE
       64:    064         1.85 16.73622 FALSE  TRUE
       65:    065         0.80 17.28141  TRUE FALSE
       66:    065         1.75 17.04715 FALSE  TRUE
       67:    068         0.80 18.00295  TRUE FALSE
       68:    068         1.85 17.68576 FALSE  TRUE
       69:    069         0.75 17.44572  TRUE FALSE
       70:    069         2.15 16.95963 FALSE  TRUE
       71:    072         0.75 18.21242  TRUE FALSE
       72:    072         2.15 17.70636 FALSE  TRUE
       73:    075         0.80 17.30944  TRUE FALSE
       74:    075         2.05 16.88232 FALSE  TRUE
       75:    076         0.80 18.19348  TRUE FALSE
       76:    076         1.75 17.95391 FALSE  TRUE
       77:    080         0.80 17.78061  TRUE FALSE
       78:    080         1.65 17.58725 FALSE  TRUE
       79:    081         0.75 17.17750  TRUE FALSE
       80:    081         2.75 16.18643 FALSE  TRUE
       81:    082         0.80 17.93568  TRUE FALSE
       82:    082         1.90 17.61123 FALSE  TRUE
       83:    083         0.75 17.27087  TRUE FALSE
       84:    083         2.55 16.44660 FALSE  TRUE
       85:    085         0.75 17.12872  TRUE FALSE
       86:    085         2.25 16.56317 FALSE  TRUE
       87:    086         0.75 17.69606  TRUE FALSE
       88:    086         2.15 17.18322 FALSE  TRUE
       89:    088         0.80 18.13954  TRUE FALSE
       90:    088         2.05 17.68666 FALSE  TRUE
       91:    089         0.80 17.45917  TRUE FALSE
       92:    089         2.00 17.06914 FALSE  TRUE
       93:    092         0.80 18.10075  TRUE FALSE
       94:    092         2.10 17.61324 FALSE  TRUE
       95:    093         0.85 18.36532  TRUE FALSE
       96:    093         1.35 18.30365 FALSE  TRUE
       97:    095         0.80 17.63988  TRUE FALSE
       98:    095         1.80 17.38204 FALSE  TRUE
       99:    099         0.80 18.13163  TRUE FALSE
      100:    099         1.70 17.92347 FALSE  TRUE
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
        17), knots = c(1, 8, 12))

