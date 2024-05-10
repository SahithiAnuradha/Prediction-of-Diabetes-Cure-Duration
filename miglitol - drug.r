
R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> library(survival)
> library(survminer)
Loading required package: ggplot2
Loading required package: ggpubr
Loading required package: magrittr
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> library(ranger)
> library(ggfortify)
> library(ggplot2)
> dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR MIGLITOL DRUG
> 
> 
> acarbose_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ miglitol ,data = dia)
> acar_test = mean(summary(acarbose_km_fit$surv))
>  if(acar_test > 0.5)
+  { 
+       print("FURTHUR TREATMENT IS NOT REQUIRED")
+       print("PATIENT HAS BEEN CURED WITH THE DRUG ALONE")
+  }else{
+       #ACARBOSE WITH INSULIN
+       #KEPLER MEIER FIT
+ 
+       dia1 <- subset(dia , readmitted ==">30"|readmitted == "<30")
+       dia1 <- dia1 %>%
+               mutate(readmitted = ifelse(readmitted == ">30" ,0,1))
+       acra_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ miglitol + insulin ,data = dia1)
+       #VISUALISATION
+ 
+       
+       acra_with_insulin_test = mean(summary(acra_insulin_fit$surv))
+       if(acra_with_insulin_test > 0.5)
+       {
+              print("PATIENT'S SURVIVABILITY HAS BEEN INCREASED AFTER TREATMENT WITH INSULIN AND FURTHER TREATMENT IS NOT REAQUIRED")
+       }
+       else{
+              print("PATIENT HAS TO BE TREATED WITH COMBINATION ALONG WITH INSULIN")
+              #COMBINATION OF DRUGS ALONG WITH INSULIN
+              #COMBINATION - 1
+              acra_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ miglitol + glipizide  ,data = dia1)
+            
+              acra_combi_test_1 = mean(summary(acra_combi_fit_R$surv))
+              #COMBINATION - 2
+              acra_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + miglitol ,data = dia1)
+             
+              acra_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              acra_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + miglitol ,data = dia1)
+              
+              acra_combi_test_3 = mean(summary(acra_combi_test_3$surv))
+              #COMBINATION - 4
+              acra_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ miglitol + rosiglitazone ,data = dia1)
+             
+              acra_combi_test_4 = mean(summary(acra_combi_fit_4$surv))
+              
+              if(pio_combi_test_1 > 0.5)
+              { 
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_1)
+                          print("COMBINATION OF MIGLITOL + PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF MIGLITOL + GLYBURIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF MIGLITOL + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF MIGLITOL + ROSIGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
+                     }
+               else  {
+ 
+                    print("COMBINATIONS ALSO DID NOT GIVE ANY RESULT")
+                    }
+ 
+ }
+ }
[1] "PATIENT'S SURVIVABILITY HAS BEEN INCREASED AFTER TREATMENT WITH INSULIN AND FURTHER TREATMENT IS NOT REAQUIRED"
> #VISUALISATION AND SUMMARY
> #DRUG ALONE
> 
> autoplot( glipizide_km_fit,main = "ACARBOSE")
Error in autoplot(glipizide_km_fit, main = "ACARBOSE") : 
  object 'glipizide_km_fit' not found
> autoplot( acarbose_km_fit,main = "MIGLITOL")
> summary(acarbose_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ miglitol, 
    data = dia)

                miglitol=Down 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3      5       3      0.4   0.219        0.137            1
    5      2       2      0.0     NaN           NA           NA

                miglitol=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1 101728   14206   0.8604 0.001087      0.85823       0.8625
    2  87522   17220   0.6911 0.001449      0.68824       0.6939
    3  70302   17746   0.5166 0.001567      0.51357       0.5197
    4  52556   13919   0.3798 0.001522      0.37684       0.3828
    5  38637    9959   0.2819 0.001411      0.27916       0.2847
    6  28678    7537   0.2078 0.001272      0.20534       0.2103
    7  21141    5857   0.1502 0.001120      0.14806       0.1525
    8  15284    4390   0.1071 0.000970      0.10521       0.1090
    9  10894    2998   0.0776 0.000839      0.07599       0.0793
   10   7896    2342   0.0546 0.000712      0.05322       0.0560
   11   5554    1855   0.0364 0.000587      0.03523       0.0375
   12   3699    1447   0.0221 0.000461      0.02125       0.0231
   13   2252    1210   0.0102 0.000316      0.00964       0.0109
   14   1042    1042   0.0000      NaN           NA           NA

                miglitol=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     31       2    0.935  0.0441       0.8529        1.000
    2     29       4    0.806  0.0710       0.6787        0.958
    3     25       6    0.613  0.0875       0.4633        0.811
    4     19       5    0.452  0.0894       0.3064        0.666
    5     14       5    0.290  0.0815       0.1674        0.503
    6      9       2    0.226  0.0751       0.1177        0.433
    7      7       2    0.161  0.0661       0.0723        0.360
    8      5       1    0.129  0.0602       0.0517        0.322
    9      4       4    0.000     NaN           NA           NA

                miglitol=Up 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3      2       1      0.5   0.354        0.125            1
   12      1       1      0.0     NaN           NA           NA

> #DRUG + INSULIN
> glipi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ miglitol + insulin ,data = dia1)
> plot(glipi_insulin_fit,main = "MIGLITOL + INSULIN")
> summary(glipi_insulin_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ miglitol + 
    insulin, data = dia1)

                miglitol=Down, insulin=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                miglitol=Down, insulin=No     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                miglitol=Down, insulin=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           5            2            2            0          NaN           NA           NA 

                miglitol=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6445     129    0.980 0.00174        0.977        0.983
    2   5881     234    0.941 0.00301        0.935        0.947
    3   4949     266    0.890 0.00415        0.882        0.899
    4   3909     269    0.829 0.00528        0.819        0.840
    5   2994     167    0.783 0.00608        0.771        0.795
    6   2300     141    0.735 0.00692        0.721        0.749
    7   1752     118    0.685 0.00781        0.670        0.701
    8   1297     109    0.628 0.00889        0.611        0.645
    9    930      76    0.576 0.00992        0.557        0.596
   10    674      70    0.517 0.01118        0.495        0.539
   11    470      31    0.483 0.01200        0.460        0.507
   12    327      31    0.437 0.01338        0.411        0.464
   13    205      32    0.369 0.01582        0.339        0.401
   14     96      25    0.273 0.02023        0.236        0.315

                miglitol=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  20693     583    0.972 0.00115        0.970        0.974
    2  17692     778    0.929 0.00186        0.925        0.933
    3  14097     829    0.874 0.00254        0.869        0.879
    4  10480     646    0.821 0.00315        0.814        0.827
    5   7591     494    0.767 0.00375        0.760        0.775
    6   5597     364    0.717 0.00432        0.709        0.726
    7   4064     314    0.662 0.00499        0.652        0.672
    8   2902     225    0.611 0.00566        0.600        0.622
    9   2031     150    0.565 0.00633        0.553        0.578
   10   1449     119    0.519 0.00709        0.505        0.533
   11    994      81    0.477 0.00792        0.461        0.492
   12    654      71    0.425 0.00914        0.407        0.443
   13    406      53    0.369 0.01066        0.349        0.391
   14    189      48    0.276 0.01414        0.249        0.305

                miglitol=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  13914     334    0.976 0.00130        0.973        0.979
    2  12321     523    0.935 0.00216        0.930        0.939
    3   9962     587    0.879 0.00300        0.874        0.885
    4   7527     533    0.817 0.00381        0.810        0.825
    5   5434     372    0.761 0.00452        0.752        0.770
    6   3997     300    0.704 0.00525        0.694        0.715
    7   2910     210    0.653 0.00593        0.642        0.665
    8   2085     197    0.592 0.00681        0.578        0.605
    9   1455     116    0.544 0.00754        0.530        0.559
   10   1029      88    0.498 0.00837        0.482        0.515
   11    731      41    0.470 0.00897        0.453        0.488
   12    484      55    0.417 0.01045        0.397        0.438
   13    281      37    0.362 0.01236        0.338        0.387
   14    131      40    0.251 0.01690        0.220        0.287

                miglitol=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5826     116    0.980 0.00183        0.977        0.984
    2   5353     177    0.948 0.00298        0.942        0.954
    3   4610     212    0.904 0.00408        0.896        0.912
    4   3730     195    0.857 0.00508        0.847        0.867
    5   2923     164    0.809 0.00602        0.797        0.821
    6   2266     144    0.757 0.00700        0.744        0.771
    7   1732     110    0.709 0.00792        0.694        0.725
    8   1299      94    0.658 0.00894        0.641        0.676
    9    953      69    0.610 0.00996        0.591        0.630
   10    710      59    0.560 0.01111        0.538        0.582
   11    493      42    0.512 0.01236        0.488        0.537
   12    329      36    0.456 0.01410        0.429        0.484
   13    191      27    0.391 0.01669        0.360        0.426
   14     97      22    0.303 0.02106        0.264        0.347

                miglitol=Steady, insulin=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                miglitol=Steady, insulin=No     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       4.000        4.000        1.000        0.750        0.217        0.426        1.000 

                miglitol=Steady, insulin=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                miglitol=Steady, insulin=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       9.000        3.000        1.000        0.667        0.272        0.300        1.000 

                miglitol=Up, insulin=No     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + miglitol  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + MIGLITOL")
> summary(met_combi_fit_1)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    miglitol, data = dia1)

                pioglitazone=Down, miglitol=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     59       2    0.966  0.0236        0.921        1.000
    4     55       5    0.878  0.0431        0.798        0.967
    5     46       3    0.821  0.0515        0.726        0.928
    6     31       2    0.768  0.0603        0.659        0.896
    9     17       2    0.678  0.0802        0.537        0.855
   10     12       1    0.621  0.0912        0.466        0.828
   11      7       1    0.532  0.1134        0.351        0.808
   14      3       2    0.177  0.1498        0.034        0.928

                pioglitazone=No, miglitol=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           5            2            2            0          NaN           NA           NA 

                pioglitazone=No, miglitol=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  43349    1098    0.975 0.000755        0.973        0.976
    2  38103    1610    0.933 0.001237        0.931        0.936
    3  31034    1753    0.881 0.001691        0.877        0.884
    4  23668    1528    0.824 0.002117        0.820        0.828
    5  17459    1098    0.772 0.002495        0.767        0.777
    6  13054     888    0.720 0.002882        0.714        0.725
    7   9641     708    0.667 0.003284        0.660        0.673
    8   6989     581    0.611 0.003730        0.604        0.619
    9   4929     389    0.563 0.004161        0.555        0.571
   10   3531     304    0.515 0.004640        0.506        0.524
   11   2455     180    0.477 0.005081        0.467        0.487
   12   1630     181    0.424 0.005845        0.413        0.436
   13    976     137    0.364 0.006889        0.351        0.378
   14    466     125    0.267 0.009019        0.250        0.285

                pioglitazone=No, miglitol=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       9.000        4.000        1.000        0.750        0.217        0.426        1.000 

                pioglitazone=No, miglitol=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                pioglitazone=Steady, miglitol=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   3345      64    0.981 0.00237        0.976        0.986
    2   2964      99    0.948 0.00397        0.940        0.956
    3   2418     137    0.894 0.00582        0.883        0.906
    4   1827     108    0.842 0.00737        0.827        0.856
    5   1355      93    0.784 0.00897        0.766        0.802
    6   1004      55    0.741 0.01018        0.721        0.761
    7    733      40    0.700 0.01146        0.678        0.723
    8    528      42    0.645 0.01339        0.619        0.671
    9    387      17    0.616 0.01445        0.589        0.645
   10    292      30    0.553 0.01697        0.521        0.587
   11    208      12    0.521 0.01832        0.486        0.558
   12    148      12    0.479 0.02050        0.440        0.521
   13     96      11    0.424 0.02391        0.380        0.474
   14     40       6    0.360 0.03140        0.304        0.428

                pioglitazone=Steady, miglitol=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           4            1            1            0          NaN           NA           NA 

                pioglitazone=Up, miglitol=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2    117       3    0.974  0.0146       0.9461        1.000
    3    107       2    0.956  0.0192       0.9193        0.995
    4     96       2    0.936  0.0234       0.8915        0.983
    5     82       3    0.902  0.0297       0.8455        0.962
    6     71       4    0.851  0.0374       0.7810        0.928
    7     60       4    0.794  0.0444       0.7120        0.886
    8     46       2    0.760  0.0487       0.6702        0.862
    9     36       3    0.697  0.0567       0.5938        0.817
   10     27       1    0.671  0.0602       0.5625        0.800
   11     18       2    0.596  0.0730       0.4690        0.758
   13      7       1    0.511  0.1007       0.3474        0.752
   14      4       2    0.256  0.1373       0.0891        0.733

> #DRUG COMBI - 2
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + miglitol ,data = dia1)
Error in eval(predvars, data, env) : object 'glyrbuide' not found
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLYRUBIDE + MIGLITOL")
> summary(met_combi_fit_2)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glyburide + 
    pioglitazone, data = dia1)

                glyburide=Down, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    248       2    0.992 0.00568        0.981        1.000
    2    234       4    0.975 0.01009        0.955        0.995
    3    208       5    0.952 0.01429        0.924        0.980
    4    177       7    0.914 0.01956        0.876        0.953
    5    145       5    0.882 0.02342        0.838        0.930
    6    115       9    0.813 0.03089        0.755        0.876
    7     82       5    0.764 0.03610        0.696        0.838
    8     64       4    0.716 0.04098        0.640        0.801
    9     52       2    0.688 0.04379        0.608        0.780
   10     38       1    0.670 0.04623        0.586        0.767
   11     28       1    0.646 0.05040        0.555        0.753
   12     19       1    0.612 0.05811        0.508        0.738
   13     13       1    0.565 0.07018        0.443        0.721
   14      2       2    0.000     NaN           NA           NA

                glyburide=Down, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     18       1    0.944   0.054        0.844            1
    9      9       1    0.840   0.110        0.649            1
   10      6       1    0.700   0.157        0.450            1

                glyburide=Down, pioglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                glyburide=No, pioglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     50       2    0.960  0.0277        0.907        1.000
    4     46       4    0.877  0.0472        0.789        0.974
    5     39       3    0.809  0.0574        0.704        0.930
    6     25       2    0.744  0.0687        0.621        0.892
    9     12       2    0.620  0.0984        0.454        0.847
   10      9       1    0.551  0.1090        0.374        0.812
   11      5       1    0.441  0.1316        0.246        0.792
   14      2       2    0.000     NaN           NA           NA

                glyburide=No, pioglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  38972     993    0.975 0.000798        0.973        0.976
    2  34195    1466    0.933 0.001313        0.930        0.935
    3  27778    1604    0.879 0.001798        0.875        0.882
    4  21112    1377    0.822 0.002249        0.817        0.826
    5  15552     979    0.770 0.002646        0.765        0.775
    6  11611     794    0.717 0.003054        0.711        0.723
    7   8591     634    0.664 0.003478        0.657        0.671
    8   6232     526    0.608 0.003951        0.601        0.616
    9   4390     350    0.560 0.004405        0.551        0.568
   10   3132     269    0.512 0.004906        0.502        0.521
   11   2166     157    0.475 0.005369        0.464        0.485
   12   1432     161    0.421 0.006197        0.409        0.434
   13    853     123    0.360 0.007334        0.346        0.375
   14    405     108    0.264 0.009574        0.246        0.284

                glyburide=No, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2923      54    0.982 0.00249        0.977        0.986
    2   2599      86    0.949 0.00420        0.941        0.957
    3   2125     119    0.896 0.00618        0.884        0.908
    4   1600      92    0.844 0.00781        0.829        0.860
    5   1195      83    0.786 0.00956        0.767        0.805
    6    887      53    0.739 0.01095        0.718        0.761
    7    654      38    0.696 0.01233        0.672        0.720
    8    471      40    0.637 0.01440        0.609        0.666
    9    341      16    0.607 0.01554        0.577        0.638
   10    255      24    0.550 0.01792        0.516        0.586
   11    183      11    0.517 0.01942        0.480        0.556
   12    128       8    0.484 0.02130        0.444        0.528
   13     86      11    0.422 0.02548        0.375        0.475
   14     36       6    0.352 0.03376        0.292        0.425

                glyburide=No, pioglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2    102       3    0.971  0.0167       0.9383        1.000
    3     92       2    0.949  0.0220       0.9073        0.994
    4     81       2    0.926  0.0270       0.8746        0.981
    5     70       3    0.886  0.0342       0.8218        0.956
    6     60       4    0.827  0.0428       0.7474        0.916
    7     49       2    0.794  0.0473       0.7060        0.892
    8     39       2    0.753  0.0529       0.6560        0.864
    9     30       3    0.678  0.0630       0.5647        0.813
   10     23       1    0.648  0.0668       0.5296        0.793
   11     15       1    0.605  0.0750       0.4744        0.771
   13      7       1    0.518  0.1026       0.3517        0.764
   14      4       2    0.259  0.1394       0.0904        0.744

                glyburide=Steady, pioglitazone=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       4.000        7.000        1.000        0.857        0.132        0.633        1.000 

                glyburide=Steady, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   3818      99    0.974 0.00257        0.969        0.979
    2   3381     136    0.935 0.00411        0.927        0.943
    3   2787     136    0.889 0.00547        0.879        0.900
    4   2161     130    0.836 0.00686        0.822        0.849
    5   1588     105    0.781 0.00826        0.764        0.797
    6   1184      75    0.731 0.00951        0.713        0.750
    7    854      61    0.679 0.01093        0.658        0.701
    8    605      43    0.631 0.01238        0.607        0.655
    9    425      35    0.579 0.01414        0.552        0.607
   10    315      28    0.527 0.01587        0.497        0.559
   11    232      19    0.484 0.01739        0.451        0.519
   12    160      17    0.433 0.01951        0.396        0.473
   13     96      12    0.379 0.02247        0.337        0.425
   14     52      15    0.269 0.02866        0.219        0.332

                glyburide=Steady, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    376      10    0.973  0.0083        0.957        0.990
    2    324      12    0.937  0.0130        0.912        0.963
    3    255      18    0.871  0.0193        0.834        0.910
    4    193      16    0.799  0.0247        0.752        0.849
    5    133       9    0.745  0.0289        0.690        0.804
    6     94       2    0.729  0.0304        0.672        0.791
    7     62       2    0.706  0.0336        0.643        0.775
    8     41       2    0.671  0.0398        0.597        0.754
   10     26       5    0.542  0.0610        0.435        0.676
   11     17       1    0.510  0.0652        0.397        0.656
   12     13       4    0.353  0.0794        0.227        0.549

                glyburide=Steady, pioglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       7.000        9.000        2.000        0.778        0.139        0.549        1.000 

                glyburide=Up, pioglitazone=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                glyburide=Up, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    333       4    0.988 0.00597        0.976        1.000
    2    313       4    0.975 0.00861        0.959        0.992
    3    279       8    0.947 0.01284        0.923        0.973
    4    230      14    0.890 0.01920        0.853        0.928
    5    183      11    0.836 0.02387        0.791        0.884
    6    150      10    0.780 0.02805        0.727        0.837
    7    118       8    0.728 0.03178        0.668        0.793
    8     92       8    0.664 0.03604        0.597        0.739
    9     66       3    0.634 0.03838        0.563        0.714
   10     46       6    0.551 0.04589        0.468        0.649
   11     29       3    0.494 0.05162        0.403        0.607
   12     19       2    0.442 0.05783        0.342        0.572
   13     14       1    0.411 0.06173        0.306        0.551

                glyburide=Up, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     22       1    0.955  0.0444        0.871            1
    5     13       1    0.881  0.0816        0.735            1

                glyburide=Up, pioglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
          11            1            1            0          NaN           NA           NA 

> #DRUG COMBI - 3
> met_combi_fit_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + miglitol ,data = dia1)
> plot(met_combi_fit_3,main = "DRUG COMBINATION : METFORMIN + MIGLITOL")
> summary(met_combi_fit_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ metformin + 
    miglitol, data = dia1)

                metformin=Down, miglitol=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    257       4    0.984 0.00772       0.9694        1.000
    2    241       6    0.960 0.01242       0.9359        0.985
    3    211       7    0.928 0.01686       0.8956        0.962
    4    177       8    0.886 0.02166       0.8447        0.930
    5    125      10    0.815 0.02932       0.7598        0.875
    6     98       9    0.740 0.03570       0.6736        0.814
    7     76       8    0.662 0.04123       0.5864        0.748
    8     50       6    0.583 0.04736       0.4971        0.684
   10     26       4    0.493 0.05751       0.3925        0.620
   11     20       2    0.444 0.06143       0.3385        0.582
   13      9       2    0.345 0.07790       0.2219        0.537
   14      6       3    0.173 0.08053       0.0692        0.431

                metformin=Down, miglitol=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=No, miglitol=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           5            2            2            0          NaN           NA           NA 

                metformin=No, miglitol=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  38242    1000    0.974 0.000816        0.972        0.975
    2  33512    1418    0.933 0.001326        0.930        0.935
    3  27328    1555    0.880 0.001809        0.876        0.883
    4  20888    1358    0.822 0.002261        0.818        0.827
    5  15464     979    0.770 0.002660        0.765        0.776
    6  11598     787    0.718 0.003064        0.712        0.724
    7   8574     626    0.666 0.003484        0.659        0.672
    8   6244     508    0.611 0.003943        0.604        0.619
    9   4430     339    0.565 0.004384        0.556        0.573
   10   3203     285    0.514 0.004901        0.505        0.524
   11   2234     172    0.475 0.005374        0.464        0.485
   12   1478     168    0.421 0.006169        0.409        0.433
   13    885     117    0.365 0.007185        0.351        0.380
   14    421     103    0.276 0.009381        0.258        0.295

                metformin=No, miglitol=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       9.000        3.000        1.000        0.667        0.272        0.300        1.000 

                metformin=No, miglitol=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Steady, miglitol=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   7945     154    0.981 0.00155        0.978        0.984
    2   7083     280    0.942 0.00271        0.937        0.947
    3   5711     318    0.889 0.00384        0.882        0.897
    4   4278     265    0.834 0.00487        0.825        0.844
    5   3109     199    0.781 0.00585        0.770        0.792
    6   2271     141    0.732 0.00676        0.719        0.746
    7   1661     111    0.683 0.00774        0.668        0.699
    8   1187     106    0.622 0.00904        0.605        0.640
    9    831      67    0.572 0.01018        0.553        0.593
   10    575      43    0.529 0.01132        0.508        0.552
   11    397      19    0.504 0.01218        0.481        0.529
   12    276      24    0.460 0.01403        0.434        0.489
   13    173      28    0.386 0.01745        0.353        0.422
   14     80      27    0.256 0.02344        0.214        0.306

                metformin=Steady, miglitol=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Up, miglitol=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    434       4    0.991 0.00459        0.982        1.000
    2    411       8    0.971 0.00811        0.956        0.988
    3    368      14    0.935 0.01244        0.910        0.959
    4    303      12    0.898 0.01589        0.867        0.929
    5    244       9    0.864 0.01875        0.828        0.902
    6    193      12    0.811 0.02313        0.767        0.857
    7    147       7    0.772 0.02623        0.722        0.825
    8    102       5    0.734 0.02991        0.678        0.795
    9     75       5    0.685 0.03502        0.620        0.757
   10     58       4    0.638 0.03979        0.565        0.721
   11     37       2    0.604 0.04449        0.522        0.697
   12     26       1    0.580 0.04845        0.493        0.683
   13     16       2    0.508 0.06403        0.397        0.650
   14      6       2    0.339 0.10664        0.183        0.628

                metformin=Up, miglitol=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           4            1            1            0          NaN           NA           NA 

> #DRUG COMBI - 4
> met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ miglitol + rosiglitazone ,data = dia1)
> plot(met_combi_fit_4,main = "DRUG COMBINATION : MIGLITOL+ ROSIGLITAZONE")
> summary(met_combi_fit_4)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ miglitol + 
    rosiglitazone, data = dia1)

                miglitol=Down, rosiglitazone=No     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           5            2            2            0          NaN           NA           NA 

                miglitol=Down, rosiglitazone=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                miglitol=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     25       1    0.960  0.0392        0.886            1
    5     18       2    0.853  0.0792        0.711            1
    8      7       1    0.731  0.1317        0.514            1
   11      4       1    0.549  0.1866        0.282            1

                miglitol=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  43807    1098    0.975 0.000747        0.973        0.976
    2  38495    1619    0.934 0.001227        0.932        0.936
    3  31343    1792    0.881 0.001685        0.877        0.884
    4  23898    1511    0.825 0.002101        0.821        0.829
    5  17692    1127    0.772 0.002482        0.767        0.777
    6  13239     898    0.720 0.002864        0.714        0.726
    7   9779     714    0.667 0.003261        0.661        0.674
    8   7082     586    0.612 0.003704        0.605        0.619
    9   5025     390    0.565 0.004125        0.557        0.573
   10   3608     318    0.515 0.004610        0.506        0.524
   11   2514     177    0.479 0.005026        0.469        0.489
   12   1694     185    0.426 0.005762        0.415        0.438
   13   1014     144    0.366 0.006803        0.353        0.379
   14    483     129    0.268 0.008893        0.251        0.286

                miglitol=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2973      64    0.978 0.00266        0.973        0.984
    2   2654      92    0.945 0.00432        0.936        0.953
    3   2182     100    0.901 0.00591        0.890        0.913
    4   1662     128    0.832 0.00803        0.816        0.848
    5   1184      67    0.785 0.00941        0.767        0.803
    6    868      49    0.740 0.01080        0.720        0.762
    7    635      34    0.701 0.01218        0.677        0.725
    8    469      37    0.646 0.01421        0.618        0.674
    9    319      21    0.603 0.01602        0.572        0.635
   10    232      15    0.564 0.01787        0.530        0.600
   11    160      17    0.504 0.02107        0.464        0.547
   12     90       7    0.465 0.02408        0.420        0.515
   13     62       4    0.435 0.02680        0.385        0.491
   14     27       6    0.338 0.04056        0.267        0.428

                miglitol=No, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     71       1    0.986  0.0140        0.959        1.000
    3     67       2    0.956  0.0246        0.910        1.000
    4     61       3    0.909  0.0353        0.843        0.981
    5     48       1    0.890  0.0393        0.817        0.971
    6     40       2    0.846  0.0484        0.756        0.946
    7     35       4    0.749  0.0625        0.636        0.882
    8     25       1    0.719  0.0668        0.600        0.863
   10     17       3    0.592  0.0863        0.445        0.788
   12      8       1    0.518  0.1025        0.352        0.764
   13      6       1    0.432  0.1162        0.255        0.732

                miglitol=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4      9       1    0.889   0.105        0.706            1
    9      3       1    0.593   0.252        0.258            1

                miglitol=Steady, rosiglitazone=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                miglitol=Up, rosiglitazone=No     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> 
