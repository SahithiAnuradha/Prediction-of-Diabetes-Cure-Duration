
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

> library(ggfortify)
> library(ggplot2)
> library(ranger)
> dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : ROSIGLITAZONE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR ROSIGLITAZONE DRUG
> 
> 
>  rosiglitazone_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ rosiglitazone ,data = dia)
>  rosiglitazone_test = mean(summary(rosiglitazone_km_fit$surv))
>  if(pio_test > 0.5)
+  { 
+       print("FURTHUR TREATMENT IS NOT REQUIRED")
+       print("PATIENT HAS BEEN CURED WITH THE DRUG ALONE")
+  }else{
+       #ROSIGLITAZONE WITH INSULIN
+       #KEPLER MEIER FIT
+ 
+       dia1 <- subset(dia , readmitted ==">30"|readmitted == "<30")
+       dia1 <- dia1 %>%
+               mutate(readmitted = ifelse(readmitted == ">30" ,0,1))
+       rosi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ rosiglitazone + insulin ,data = dia1)
+       #VISUALISATION
+ 
+       
+       rosi_with_insulin_test = mean(summary(rosi_insulin_fit$surv))
+       if(pio_with_insulin_test > 0.5)
+       {
+              print("PATIENT'S SURVIVABILITY HAS BEEN INCREASED AFTER TREATMENT WITH INSULIN AND FURTHER TREATMENT IS NOT REAQUIRED")
+       }
+       else{
+              print("PATIENT HAS TO BE TREATED WITH COMBINATION ALONG WITH INSULIN")
+              #COMBINATION OF DRUGS ALONG WITH INSULIN
+              #COMBINATION - 1
+              rosi_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + rosiglitazone  ,data = dia1)
+            
+              pio_combi_test_1 = mean(summary(pio_combi_fit_R$surv))
+              #COMBINATION - 2
+              pio_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + rosiglitazone ,data = dia1)
+             
+              met_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              pio_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + rosiglitazone ,data = dia1)
+              
+              pio_combi_test_3 = mean(summary(met_combi_test_fit_met$surv))
+              #COMBINATION - 4
+              pio_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + rosiglitazone ,data = dia1)
+             
+              pio_combi_test_4 = mean(summary(pio_combi_test_fit_4$surv))
+              
+              if(pio_combi_test_1 > 0.5)
+              { 
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_1)
+                          print("COMBINATION OF PIOGLITAZONE~ROSIGLITAZONE  HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF GLYBURIDE ~ PIOGLITAZONE  HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF METFORMIN ~ PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF GLYBURIDE ~ PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
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
> autoplot( rosiglitazone_km_fit,main = "ROSIGLITAZONE")
> summary(rosiglitazone_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ rosiglitazone, 
    data = dia)

                rosiglitazone=Down 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     87       5   0.9425  0.0250       0.8949        0.993
    2     82      10   0.8276  0.0405       0.7519        0.911
    3     72       7   0.7471  0.0466       0.6612        0.844
    4     65      16   0.5632  0.0532       0.4681        0.678
    5     49      13   0.4138  0.0528       0.3222        0.531
    6     36       8   0.3218  0.0501       0.2372        0.437
    7     28       8   0.2299  0.0451       0.1565        0.338
    8     20       3   0.1954  0.0425       0.1276        0.299
    9     17       3   0.1609  0.0394       0.0996        0.260
   10     14       1   0.1494  0.0382       0.0905        0.247
   11     13       4   0.1034  0.0327       0.0557        0.192
   12      9       2   0.0805  0.0292       0.0395        0.164
   13      7       3   0.0460  0.0225       0.0177        0.120
   14      4       4   0.0000     NaN           NA           NA

                rosiglitazone=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  95401   13418   0.8594 0.001126      0.85715       0.8616
    2  81983   16177   0.6898 0.001498      0.68685       0.6927
    3  65806   16639   0.5154 0.001618      0.51221       0.5186
    4  49167   12935   0.3798 0.001571      0.37672       0.3829
    5  36232    9343   0.2819 0.001457      0.27901       0.2847
    6  26889    7077   0.2077 0.001313      0.20511       0.2103
    7  19812    5508   0.1499 0.001156      0.14769       0.1522
    8  14304    4102   0.1069 0.001001      0.10499       0.1089
    9  10202    2804   0.0775 0.000866      0.07587       0.0793
   10   7398    2189   0.0546 0.000736      0.05318       0.0561
   11   5209    1730   0.0365 0.000607      0.03530       0.0377
   12   3479    1378   0.0220 0.000475      0.02111       0.0230
   13   2101    1124   0.0102 0.000326      0.00962       0.0109
   14    977     977   0.0000      NaN           NA           NA

                rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6100     783  0.87164 0.00428      0.86329       0.8801
    2   5317    1024  0.70377 0.00585      0.69241       0.7153
    3   4293    1088  0.52541 0.00639      0.51303       0.5381
    4   3205     944  0.37066 0.00618      0.35873       0.3830
    5   2261     592  0.27361 0.00571      0.26264       0.2850
    6   1669     443  0.20098 0.00513      0.19117       0.2113
    7   1226     320  0.14852 0.00455      0.13986       0.1577
    8    906     274  0.10361 0.00390      0.09623       0.1115
    9    632     183  0.07361 0.00334      0.06734       0.0805
   10    449     139  0.05082 0.00281      0.04560       0.0566
   11    310     112  0.03246 0.00227      0.02830       0.0372
   12    198      64  0.02197 0.00188      0.01858       0.0260
   13    134      78  0.00918 0.00122      0.00707       0.0119
   14     56      56  0.00000     NaN           NA           NA

                rosiglitazone=Up 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    178       2   0.9888  0.0079       0.9734       1.0000
    2    176      13   0.9157  0.0208       0.8758       0.9575
    3    163      22   0.7921  0.0304       0.7347       0.8540
    4    141      29   0.6292  0.0362       0.5621       0.7043
    5    112      18   0.5281  0.0374       0.4596       0.6068
    6     94      11   0.4663  0.0374       0.3985       0.5457
    7     83      23   0.3371  0.0354       0.2743       0.4142
    8     60      12   0.2697  0.0333       0.2118       0.3434
    9     48      12   0.2022  0.0301       0.1511       0.2708
   10     36      13   0.1292  0.0251       0.0882       0.1892
   11     23       9   0.0787  0.0202       0.0476       0.1300
   12     14       4   0.0562  0.0173       0.0308       0.1026
   13     10       5   0.0281  0.0124       0.0118       0.0667
   14      5       5   0.0000     NaN           NA           NA

> #DRUG + INSULIN
> rosi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ rosiglitazone + insulin ,data = dia1)
> plot(rosi_insulin_fit,main = "PIOGLITAZONE + INSULIN")
> summary(rosi_insulin_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ rosiglitazone + 
    insulin, data = dia1)

                rosiglitazone=Down, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      5       1    0.800   0.179        0.516            1
    8      3       1    0.533   0.248        0.214            1

                rosiglitazone=Down, insulin=No     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
      11.000        2.000        1.000        0.500        0.354        0.125        1.000 

                rosiglitazone=Down, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4      9       1    0.889   0.105        0.706            1
    5      5       1    0.711   0.180        0.433            1

                rosiglitazone=Down, insulin=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                rosiglitazone=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6063     121    0.980 0.00180        0.977        0.984
    2   5528     227    0.940 0.00313        0.934        0.946
    3   4638     256    0.888 0.00432        0.879        0.896
    4   3655     249    0.827 0.00547        0.817        0.838
    5   2809     159    0.781 0.00630        0.768        0.793
    6   2160     137    0.731 0.00718        0.717        0.745
    7   1638     110    0.682 0.00808        0.666        0.698
    8   1218     106    0.623 0.00921        0.605        0.641
    9    869      71    0.572 0.01024        0.552        0.592
   10    631      69    0.509 0.01156        0.487        0.532
   11    438      29    0.476 0.01238        0.452        0.500
   12    306      31    0.427 0.01382        0.401        0.455
   13    186      32    0.354 0.01646        0.323        0.388
   14     89      23    0.262 0.02046        0.225        0.306

                rosiglitazone=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  19315     547    0.972 0.00119        0.969        0.974
    2  16495     729    0.929 0.00193        0.925        0.933
    3  13148     779    0.874 0.00264        0.869        0.879
    4   9789     599    0.820 0.00326        0.814        0.827
    5   7101     460    0.767 0.00387        0.760        0.775
    6   5243     345    0.717 0.00447        0.708        0.725
    7   3816     302    0.660 0.00517        0.650        0.670
    8   2713     212    0.608 0.00586        0.597        0.620
    9   1906     143    0.563 0.00654        0.550        0.576
   10   1354     114    0.515 0.00735        0.501        0.530
   11    923      72    0.475 0.00816        0.459        0.491
   12    617      70    0.421 0.00944        0.403        0.440
   13    380      52    0.364 0.01102        0.343        0.386
   14    180      46    0.271 0.01439        0.244        0.300

                rosiglitazone=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  12926     319    0.975 0.00136        0.973        0.978
    2  11427     493    0.933 0.00227        0.929        0.938
    3   9228     553    0.877 0.00314        0.871        0.883
    4   6951     480    0.817 0.00396        0.809        0.825
    5   5024     350    0.760 0.00471        0.751        0.769
    6   3694     280    0.702 0.00547        0.692        0.713
    7   2684     195    0.651 0.00617        0.639        0.663
    8   1927     180    0.590 0.00707        0.577        0.604
    9   1344     110    0.542 0.00785        0.527        0.558
   10    944      81    0.496 0.00871        0.479        0.513
   11    678      35    0.470 0.00927        0.452        0.489
   12    455      51    0.417 0.01078        0.397        0.439
   13    264      33    0.365 0.01269        0.341        0.391
   14    124      38    0.253 0.01749        0.221        0.290

                rosiglitazone=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5524     111    0.980 0.00189        0.976        0.984
    2   5064     170    0.947 0.00308        0.941        0.953
    3   4346     204    0.903 0.00422        0.894        0.911
    4   3514     184    0.855 0.00525        0.845        0.866
    5   2766     160    0.806 0.00623        0.794        0.818
    6   2147     136    0.755 0.00721        0.741        0.769
    7   1644     107    0.706 0.00816        0.690        0.722
    8   1227      88    0.655 0.00919        0.637        0.673
    9    909      67    0.607 0.01023        0.587        0.627
   10    679      54    0.559 0.01133        0.537        0.581
   11    475      41    0.510 0.01261        0.486        0.536
   12    316      33    0.457 0.01430        0.430        0.486
   13    184      27    0.390 0.01706        0.358        0.425
   14     90      22    0.295 0.02187        0.255        0.341

                rosiglitazone=Steady, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    364       8    0.978 0.00768        0.963        0.993
    2    334       7    0.958 0.01074        0.937        0.979
    3    292      10    0.925 0.01454        0.897        0.954
    4    234      18    0.854 0.02097        0.813        0.896
    5    170       7    0.818 0.02395        0.773        0.867
    6    131       3    0.800 0.02573        0.751        0.852
    7    105       6    0.754 0.03027        0.697        0.816
    8     72       2    0.733 0.03286        0.671        0.800
    9     56       5    0.668 0.04094        0.592        0.753
   10     39       1    0.650 0.04332        0.571        0.741
   11     29       2    0.606 0.05063        0.514        0.713
   14      6       2    0.404 0.12134        0.224        0.728

                rosiglitazone=Steady, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   1360      36    0.974 0.00435        0.965        0.982
    2   1178      49    0.933 0.00703        0.919        0.947
    3    931      49    0.884 0.00954        0.865        0.903
    4    671      47    0.822 0.01243        0.798        0.847
    5    471      33    0.764 0.01507        0.735        0.795
    6    337      18    0.724 0.01706        0.691        0.758
    7    234      12    0.686 0.01926        0.650        0.725
    8    176      13    0.636 0.02239        0.593        0.681
    9    113       7    0.596 0.02548        0.548        0.648
   10     84       5    0.561 0.02848        0.508        0.620
   11     63       8    0.490 0.03423        0.427        0.562
   12     32       1    0.474 0.03642        0.408        0.551
   13     22       1    0.453 0.04065        0.380        0.540
   14      7       2    0.323 0.08259        0.196        0.534

                rosiglitazone=Steady, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    956      15    0.984 0.00402        0.976        0.992
    2    862      29    0.951 0.00719        0.937        0.965
    3    704      33    0.907 0.01021        0.887        0.927
    4    549      52    0.821 0.01462        0.793        0.850
    5    390      21    0.777 0.01672        0.744        0.810
    6    286      20    0.722 0.01946        0.685        0.761
    7    212      13    0.678 0.02180        0.637        0.722
    8    153      16    0.607 0.02574        0.559        0.660
    9    107       6    0.573 0.02780        0.521        0.630
   10     81       5    0.538 0.03025        0.482        0.600
   11     51       6    0.474 0.03606        0.409        0.551
   12     27       3    0.422 0.04302        0.345        0.515
   13     16       3    0.343 0.05399        0.252        0.467
   14      7       2    0.245 0.07007        0.140        0.429

                rosiglitazone=Steady, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    296       5    0.983 0.00749        0.969        0.998
    2    283       7    0.959 0.01165        0.936        0.982
    3    258       8    0.929 0.01531        0.900        0.960
    4    210      11    0.880 0.02036        0.841        0.921
    5    154       6    0.846 0.02390        0.801        0.894
    6    115       8    0.787 0.02996        0.731        0.848
    7     85       3    0.759 0.03292        0.698        0.827
    8     69       6    0.693 0.03959        0.620        0.776
    9     44       3    0.646 0.04533        0.563        0.741
   10     28       4    0.554 0.05775        0.451        0.679
   11     17       1    0.521 0.06288        0.411        0.660
   12     12       3    0.391 0.08043        0.261        0.585

                rosiglitazone=Up, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     16       2    0.875  0.0827        0.727        1.000
    6      8       1    0.766  0.1253        0.556        1.000
    7      7       2    0.547  0.1584        0.310        0.965

                rosiglitazone=Up, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     19       1    0.947  0.0512        0.852        1.000
    4     17       1    0.892  0.0724        0.760        1.000
    5     15       1    0.832  0.0887        0.675        1.000
    6     13       1    0.768  0.1024        0.592        0.998

                rosiglitazone=Up, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     23       1    0.957  0.0425       0.8767        1.000
    3     22       1    0.913  0.0588       0.8049        1.000
    7     12       2    0.761  0.1098       0.5735        1.000
    8      5       1    0.609  0.1620       0.3613        1.000
   10      4       2    0.304  0.1724       0.1003        0.924
   12      2       1    0.152  0.1379       0.0258        0.898
   13      1       1    0.000     NaN           NA           NA

                rosiglitazone=Up, insulin=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
      10.000        2.000        1.000        0.500        0.354        0.125        1.000 

> #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + rosiglitazone  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + ROSIGLITAZONE")
> summary(met_combi_fit_1)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    rosiglitazone, data = dia1)

                pioglitazone=Down, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     59       2    0.966  0.0236        0.921        1.000
    4     55       5    0.878  0.0431        0.798        0.967
    5     46       3    0.821  0.0515        0.726        0.928
    6     31       2    0.768  0.0603        0.659        0.896
    9     17       2    0.678  0.0802        0.537        0.855
   10     12       1    0.621  0.0912        0.466        0.828
   11      7       1    0.532  0.1134        0.351        0.808
   14      3       2    0.177  0.1498        0.034        0.928

                pioglitazone=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     25       1    0.960  0.0392        0.886            1
    5     18       2    0.853  0.0792        0.711            1
    8      7       1    0.731  0.1317        0.514            1
   11      4       1    0.549  0.1866        0.282            1

                pioglitazone=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  40322    1034    0.974 0.000787        0.973        0.976
    2  35390    1519    0.933 0.001292        0.930        0.935
    3  28794    1651    0.879 0.001765        0.876        0.883
    4  21946    1397    0.823 0.002198        0.819        0.827
    5  16231    1030    0.771 0.002592        0.766        0.776
    6  12152     837    0.718 0.002993        0.712        0.724
    7   8979     670    0.664 0.003411        0.658        0.671
    8   6499     542    0.609 0.003868        0.601        0.616
    9   4595     369    0.560 0.004315        0.552        0.568
   10   3284     286    0.511 0.004807        0.502        0.521
   11   2288     162    0.475 0.005240        0.465        0.485
   12   1536     173    0.421 0.006025        0.410        0.433
   13    912     132    0.360 0.007118        0.347        0.375
   14    437     120    0.261 0.009268        0.244        0.280

                pioglitazone=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2951      64    0.978 0.00268        0.973        0.984
    2   2635      90    0.945 0.00432        0.936        0.953
    3   2165     100    0.901 0.00593        0.890        0.913
    4   1648     127    0.832 0.00806        0.816        0.848
    5   1171      67    0.784 0.00947        0.766        0.803
    6    855      49    0.739 0.01089        0.718        0.761
    7    622      34    0.699 0.01230        0.675        0.723
    8    462      37    0.643 0.01435        0.615        0.672
    9    313      21    0.600 0.01618        0.569        0.632
   10    225      15    0.560 0.01810        0.525        0.596
   11    153      17    0.498 0.02147        0.457        0.541
   12     84       7    0.456 0.02475        0.410        0.507
   13     57       4    0.424 0.02771        0.373        0.482
   14     26       5    0.343 0.03969        0.273        0.430

                pioglitazone=No, rosiglitazone=Up     
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

                pioglitazone=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   3324      64    0.981 0.00238        0.976        0.985
    2   2946      97    0.948 0.00396        0.941        0.956
    3   2402     137    0.894 0.00584        0.883        0.906
    4   1813     108    0.841 0.00741        0.827        0.856
    5   1342      93    0.783 0.00903        0.765        0.801
    6    991      55    0.739 0.01025        0.720        0.760
    7    720      40    0.698 0.01156        0.676        0.721
    8    520      42    0.642 0.01351        0.616        0.669
    9    380      17    0.613 0.01459        0.585        0.642
   10    285      30    0.549 0.01717        0.516        0.583
   11    201      12    0.516 0.01856        0.481        0.554
   12    142      12    0.472 0.02083        0.433        0.515
   13     91      11    0.415 0.02441        0.370        0.466
   14     39       5    0.362 0.03077        0.306        0.428

                pioglitazone=Steady, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     20       2     0.90  0.0671        0.778            1
    4     15       1     0.84  0.0853        0.688            1
   14      1       1     0.00     NaN           NA           NA

                pioglitazone=Up, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2    115       3    0.974  0.0149       0.9452        1.000
    3    105       2    0.955  0.0195       0.9178        0.994
    4     95       2    0.935  0.0237       0.8899        0.983
    5     81       3    0.901  0.0301       0.8435        0.962
    6     70       4    0.849  0.0378       0.7781        0.927
    7     59       4    0.792  0.0449       0.7083        0.885
    8     46       2    0.757  0.0491       0.6668        0.860
    9     36       3    0.694  0.0569       0.5910        0.815
   10     27       1    0.668  0.0604       0.5599        0.798
   11     18       2    0.594  0.0730       0.4669        0.756
   13      7       1    0.509  0.1004       0.3459        0.750
   14      4       2    0.255  0.1369       0.0888        0.730

                pioglitazone=Up, rosiglitazone=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 2
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + rosiglitazone ,data = dia1)
Error in eval(predvars, data, env) : object 'glyrbuide' not found
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLYBRIDE + PIOGLITAZONE")
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + rosiglitazone ,data = dia1)
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLYBRIDE + PIOGLITAZONE")
> summary(met_combi_fit_2)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glyburide + 
    rosiglitazone, data = dia1)

                glyburide=Down, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    250       2    0.992 0.00563        0.981        1.000
    2    232       4    0.975 0.01013        0.955        0.995
    3    207       4    0.956 0.01362        0.930        0.983
    4    177       8    0.913 0.01980        0.875        0.952
    5    142       5    0.881 0.02375        0.835        0.929
    6    112       8    0.818 0.03076        0.760        0.880
    7     80       5    0.767 0.03635        0.699        0.841
    8     63       3    0.730 0.04027        0.655        0.814
    9     52       3    0.688 0.04469        0.606        0.781
   10     34       2    0.648 0.05040        0.556        0.754
   11     26       1    0.623 0.05427        0.525        0.739
   12     18       1    0.588 0.06129        0.479        0.721
   13     11       1    0.535 0.07552        0.405        0.705
   14      2       2    0.000     NaN           NA           NA

                glyburide=Down, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     23       1    0.957  0.0425        0.877            1
    6     17       1    0.900  0.0677        0.777            1
    8     11       1    0.818  0.0994        0.645            1

                glyburide=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     23       1    0.957  0.0425        0.877            1
    5     16       1    0.897  0.0703        0.769            1
    8      6       1    0.747  0.1485        0.506            1
   11      3       1    0.498  0.2262        0.205            1

                glyburide=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  39409     988    0.975 0.000788        0.973        0.976
    2  34587    1472    0.933 0.001299        0.931        0.936
    3  28105    1637    0.879 0.001788        0.876        0.883
    4  21360    1361    0.823 0.002227        0.819        0.827
    5  15805    1016    0.770 0.002631        0.765        0.775
    6  11805     807    0.717 0.003035        0.712        0.723
    7   8747     642    0.665 0.003451        0.658        0.672
    8   6334     535    0.609 0.003922        0.601        0.616
    9   4482     351    0.561 0.004362        0.553        0.570
   10   3211     278    0.512 0.004861        0.503        0.522
   11   2226     154    0.477 0.005298        0.467        0.487
   12   1493     162    0.425 0.006087        0.413        0.437
   13    895     131    0.363 0.007228        0.349        0.377
   14    421     113    0.266 0.009456        0.248        0.285

                glyburide=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2562      59    0.977 0.00296        0.971        0.983
    2   2281      82    0.942 0.00476        0.933        0.951
    3   1863      88    0.897 0.00648        0.885        0.910
    4   1407     112    0.826 0.00880        0.809        0.843
    5    995      50    0.784 0.01013        0.765        0.805
    6    733      44    0.737 0.01175        0.715        0.761
    7    530      28    0.698 0.01324        0.673        0.725
    8    396      31    0.644 0.01542        0.614        0.675
    9    271      20    0.596 0.01756        0.563        0.632
   10    191      14    0.553 0.01978        0.515        0.593
   11    134      15    0.491 0.02313        0.447        0.538
   12     74       6    0.451 0.02635        0.402        0.506
   13     50       3    0.424 0.02903        0.371        0.485
   14     25       5    0.339 0.04110        0.267        0.430

                glyburide=No, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     57       1    0.982  0.0174        0.949        1.000
    3     54       2    0.946  0.0303        0.889        1.000
    4     49       1    0.927  0.0353        0.860        0.999
    5     40       1    0.904  0.0413        0.826        0.988
    6     33       2    0.849  0.0540        0.749        0.962
    7     28       4    0.728  0.0728        0.598        0.885
    8     21       1    0.693  0.0771        0.557        0.862
   10     13       3    0.533  0.1004        0.369        0.771
   12      5       1    0.426  0.1247        0.240        0.756
   13      3       1    0.284  0.1427        0.106        0.761

                glyburide=Steady, rosiglitazone=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       5.000        2.000        1.000        0.500        0.354        0.125        1.000 

                glyburide=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   3848     104    0.973 0.00261        0.968        0.978
    2   3394     138    0.933 0.00414        0.925        0.942
    3   2780     143    0.885 0.00554        0.875        0.896
    4   2150     132    0.831 0.00693        0.818        0.845
    5   1578      98    0.779 0.00823        0.763        0.796
    6   1186      73    0.731 0.00945        0.713        0.750
    7    846      60    0.680 0.01090        0.659        0.701
    8    600      41    0.633 0.01233        0.609        0.658
    9    431      34    0.583 0.01402        0.556        0.611
   10    318      32    0.525 0.01599        0.494        0.557
   11    234      19    0.482 0.01743        0.449        0.517
   12    165      21    0.421 0.01969        0.384        0.461
   13     96      11    0.372 0.02215        0.331        0.418
   14     53      14    0.274 0.02783        0.225        0.334

                glyburide=Steady, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    357       5    0.986 0.00622       0.9739        0.998
    2    321      10    0.955 0.01130       0.9334        0.978
    3    272      11    0.917 0.01574       0.8863        0.948
    4    216      14    0.857 0.02127       0.8165        0.900
    5    153      15    0.773 0.02816       0.7199        0.830
    6    102       4    0.743 0.03087       0.6848        0.806
    7     79       5    0.696 0.03536       0.6299        0.769
    8     52       4    0.642 0.04155       0.5658        0.729
    9     31       1    0.622 0.04508       0.5392        0.717
   10     25       1    0.597 0.04966       0.5069        0.702
   11     15       1    0.557 0.06021       0.4506        0.688
   13      5       1    0.446 0.11067       0.2738        0.725
   14      2       1    0.223 0.16697       0.0513        0.968

                glyburide=Steady, rosiglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       4.000        6.000        1.000        0.833        0.152        0.583        1.000 

                glyburide=Up, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    321       4    0.988 0.00619        0.975        1.000
    2    301       5    0.971 0.00949        0.953        0.990
    3    268       8    0.942 0.01366        0.916        0.969
    4    222      11    0.895 0.01889        0.859        0.933
    5    175      10    0.844 0.02375        0.799        0.892
    6    141      10    0.784 0.02864        0.730        0.843
    7    109       7    0.734 0.03252        0.673        0.801
    8     88       7    0.676 0.03666        0.607        0.751
    9     63       3    0.643 0.03934        0.571        0.725
   10     45       6    0.558 0.04718        0.472        0.658
   11     28       3    0.498 0.05326        0.404        0.614
   12     18       1    0.470 0.05704        0.371        0.596
   13     12       1    0.431 0.06435        0.322        0.578

                glyburide=Up, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     22       2    0.909  0.0613        0.797        1.000
    5     19       2    0.813  0.0843        0.664        0.997
    7     14       1    0.755  0.0962        0.588        0.970
    8     11       1    0.687  0.1093        0.503        0.938
   11      4       1    0.515  0.1698        0.270        0.983
   12      3       1    0.343  0.1801        0.123        0.960

                glyburide=Up, rosiglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       4.000        6.000        1.000        0.833        0.152        0.583        1.000 

> #DRUG COMBI - 3
> met_combi_fit_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + rosiglitazone ,data = dia1)
> plot(met_combi_fit_3,main = "DRUG COMBINATION : PIOGLITAZONE + METFORMIN")
> summary(met_combi_fit_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ metformin + 
    rosiglitazone, data = dia1)

                metformin=Down, rosiglitazone=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Down, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    223       4    0.982 0.00889       0.9648        1.000
    2    208       4    0.963 0.01278       0.9384        0.989
    3    182       6    0.931 0.01776       0.8973        0.967
    4    152       5    0.901 0.02183       0.8590        0.945
    5    108       9    0.826 0.03121       0.7668        0.889
    6     84       7    0.757 0.03793       0.6861        0.835
    7     68       7    0.679 0.04400       0.5980        0.771
    8     43       4    0.616 0.04997       0.5253        0.722
   10     23       3    0.536 0.06130       0.4279        0.670
   11     18       1    0.506 0.06472       0.3936        0.650
   13      9       2    0.393 0.08629       0.2559        0.605
   14      6       3    0.197 0.09115       0.0793        0.488

                metformin=Down, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     31       2    0.935  0.0441       0.8529        1.000
    3     28       1    0.902  0.0537       0.8027        1.000
    4     25       3    0.794  0.0753       0.6591        0.956
    5     17       1    0.747  0.0841       0.5992        0.932
    6     14       2    0.640  0.1004       0.4710        0.871
    7      8       1    0.560  0.1154       0.3742        0.839
    8      7       2    0.400  0.1263       0.2156        0.743
   10      3       1    0.267  0.1377       0.0971        0.734
   11      2       1    0.133  0.1168       0.0240        0.742

                metformin=Down, rosiglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5     13       1    0.923  0.0739       0.7890            1
    8      4       1    0.692  0.2074       0.3849            1
   11      2       1    0.346  0.2658       0.0768            1

                metformin=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  36180     953    0.974 0.000842        0.972        0.975
    2  31688    1362    0.932 0.001371        0.929        0.935
    3  25814    1484    0.878 0.001869        0.875        0.882
    4  19725    1270    0.822 0.002327        0.817        0.826
    5  14622     938    0.769 0.002741        0.764        0.774
    6  10970     752    0.716 0.003156        0.710        0.722
    7   8112     599    0.663 0.003587        0.656        0.670
    8   5901     484    0.609 0.004057        0.601        0.617
    9   4191     328    0.561 0.004513        0.553        0.570
   10   3022     271    0.511 0.005039        0.501        0.521
   11   2109     156    0.473 0.005500        0.463        0.484
   12   1411     161    0.419 0.006307        0.407        0.432
   13    839     116    0.361 0.007382        0.347        0.376
   14    400      98    0.273 0.009561        0.255        0.292

                metformin=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2013      47    0.977 0.00337        0.970        0.983
    2   1774      55    0.946 0.00518        0.936        0.957
    3   1468      70    0.901 0.00721        0.887        0.915
    4   1115      85    0.833 0.00978        0.814        0.852
    5    804      42    0.789 0.01134        0.767        0.812
    6    594      34    0.744 0.01307        0.719        0.770
    7    435      24    0.703 0.01479        0.674        0.732
    8    325      23    0.653 0.01700        0.621        0.687
    9    225      12    0.618 0.01883        0.582        0.656
   10    165      12    0.573 0.02147        0.533        0.617
   11    115      15    0.499 0.02594        0.450        0.552
   12     60       6    0.449 0.03030        0.393        0.512
   13     41       1    0.438 0.03147        0.380        0.504
   14     19       5    0.323 0.04993        0.238        0.437

                metformin=No, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     46       1    0.978  0.0215        0.937        1.000
    3     42       1    0.955  0.0311        0.896        1.000
    4     39       3    0.882  0.0499        0.789        0.985
    6     28       1    0.850  0.0572        0.745        0.970
    7     24       3    0.744  0.0761        0.609        0.909
   10     13       2    0.629  0.0984        0.463        0.855
   12      6       1    0.524  0.1261        0.327        0.840

                metformin=Steady, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4      5       1    0.800   0.179        0.516            1
    5      3       1    0.533   0.248        0.214            1

                metformin=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   7046     137    0.981 0.00164        0.977        0.984
    2   6262     246    0.942 0.00288        0.936        0.948
    3   5043     289    0.888 0.00411        0.880        0.896
    4   3768     225    0.835 0.00516        0.825        0.845
    5   2760     174    0.782 0.00619        0.770        0.795
    6   2025     128    0.733 0.00718        0.719        0.747
    7   1478     103    0.682 0.00826        0.666        0.698
    8   1052      93    0.622 0.00961        0.603        0.641
    9    744      58    0.573 0.01076        0.552        0.595
   10    515      41    0.527 0.01203        0.504        0.552
   11    356      18    0.501 0.01296        0.476        0.527
   12    249      23    0.455 0.01493        0.426        0.485
   13    153      25    0.380 0.01846        0.346        0.418
   14     72      26    0.243 0.02454        0.199        0.296

                metformin=Steady, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    881      17    0.981 0.00463        0.972        0.990
    2    803      34    0.939 0.00826        0.923        0.956
    3    648      28    0.899 0.01090        0.877        0.920
    4    491      39    0.827 0.01486        0.799        0.857
    5    336      23    0.771 0.01793        0.736        0.807
    6    238      12    0.732 0.02023        0.693        0.772
    7    175       8    0.698 0.02250        0.656        0.744
    8    128      12    0.633 0.02719        0.582        0.688
    9     83       9    0.564 0.03247        0.504        0.632
   10     57       1    0.554 0.03337        0.493        0.624
   11     39       1    0.540 0.03542        0.475        0.614
   12     26       1    0.519 0.03968        0.447        0.603
   13     19       2    0.465 0.05096        0.375        0.576
   14      8       1    0.407 0.07029        0.290        0.571

                metformin=Steady, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     19       1    0.947  0.0512       0.8521            1
    5     12       1    0.868  0.0890       0.7104            1
    6      8       1    0.760  0.1280       0.5463            1
    8      6       1    0.633  0.1573       0.3892            1
   10      2       1    0.317  0.2373       0.0729            1
   13      1       1    0.000     NaN           NA           NA

                metformin=Up, rosiglitazone=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Up, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    379       4    0.989 0.00525        0.979        1.000
    2    356       7    0.970 0.00892        0.953        0.988
    3    321      13    0.931 0.01368        0.904        0.958
    4    264      12    0.888 0.01769        0.854        0.924
    5    210       8    0.855 0.02067        0.815        0.896
    6    165      11    0.798 0.02545        0.749        0.849
    7    124       5    0.765 0.02819        0.712        0.823
    8     89       5    0.722 0.03251        0.661        0.789
    9     64       5    0.666 0.03854        0.595        0.746
   10     48       3    0.624 0.04298        0.546        0.715
   11     31       2    0.584 0.04874        0.496        0.688
   12     20       1    0.555 0.05435        0.458        0.672
   13     13       1    0.512 0.06480        0.400        0.656
   14      5       2    0.307 0.11876        0.144        0.655

                metformin=Up, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     49       1    0.980  0.0202       0.9408        1.000
    3     41       1    0.956  0.0307       0.8973        1.000
    4     33       1    0.927  0.0413       0.8493        1.000
    5     28       1    0.894  0.0514       0.7984        1.000
    6     23       1    0.855  0.0621       0.7413        0.986
    7     18       1    0.807  0.0746       0.6735        0.968
   10      7       1    0.692  0.1245       0.4864        0.984
   13      2       1    0.346  0.2524       0.0828        1.000

                metformin=Up, rosiglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       7.000        4.000        1.000        0.750        0.217        0.426        1.000 

> #DRUG COMBI - 4
> met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + rosiglitazone ,data = dia1)
> plot(met_combi_fit_4,main = "DRUG COMBINATION : GLIPIZIDE + PIOGLITAZONE")
> summary(met_combi_fit_4)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glipizide + 
    rosiglitazone, data = dia1)

                glipizide=Down, rosiglitazone=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                glipizide=Down, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    263       3    0.989 0.00655        0.976        1.000
    2    244       4    0.972 0.01030        0.952        0.993
    3    220      12    0.919 0.01779        0.885        0.955
    4    180      12    0.858 0.02383        0.813        0.906
    5    134       7    0.813 0.02797        0.760        0.870
    6    108       6    0.768 0.03192        0.708        0.833
    7     90       7    0.708 0.03656        0.640        0.784
    8     71      10    0.609 0.04292        0.530        0.699
    9     49       3    0.571 0.04536        0.489        0.667
   10     41       6    0.488 0.04994        0.399        0.596
   11     31       2    0.456 0.05143        0.366        0.569
   12     25       1    0.438 0.05251        0.346        0.554
   13     18       3    0.365 0.05827        0.267        0.499
   14     11       3    0.265 0.06479        0.165        0.428

                glipizide=Down, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     29       2    0.931  0.0471        0.843            1
    4     22       1    0.889  0.0610        0.777            1
    6     14       1    0.825  0.0834        0.677            1
   11      6       1    0.688  0.1435        0.457            1
   14      3       1    0.458  0.2102        0.187            1

                glipizide=Down, rosiglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                glipizide=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     19       1    0.947  0.0512        0.852            1
    5     12       2    0.789  0.1105        0.600            1

                glipizide=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  38235     972    0.975 0.000805        0.973        0.976
    2  33538    1440    0.933 0.001326        0.930        0.935
    3  27217    1564    0.879 0.001815        0.876        0.883
    4  20740    1302    0.824 0.002255        0.820        0.828
    5  15368     983    0.771 0.002665        0.766        0.776
    6  11508     784    0.719 0.003074        0.713        0.725
    7   8488     635    0.665 0.003507        0.658        0.672
    8   6165     506    0.610 0.003971        0.603        0.618
    9   4376     349    0.562 0.004427        0.553        0.570
   10   3133     280    0.511 0.004944        0.502        0.521
   11   2174     150    0.476 0.005378        0.466        0.487
   12   1461     165    0.422 0.006189        0.410        0.435
   13    871     133    0.358 0.007349        0.344        0.373
   14    406     103    0.267 0.009477        0.249        0.286

                glipizide=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2410      55    0.977 0.00304        0.971        0.983
    2   2147      72    0.944 0.00480        0.935        0.954
    3   1768      82    0.901 0.00658        0.888        0.914
    4   1359     107    0.830 0.00895        0.812        0.847
    5    958      58    0.779 0.01056        0.759        0.800
    6    708      39    0.737 0.01201        0.713        0.760
    7    514      25    0.701 0.01339        0.675        0.727
    8    381      31    0.644 0.01574        0.614        0.675
    9    264      17    0.602 0.01765        0.569        0.638
   10    189      12    0.564 0.01968        0.527        0.604
   11    127      11    0.515 0.02283        0.472        0.562
   12     77       6    0.475 0.02628        0.426        0.529
   13     55       4    0.440 0.02951        0.386        0.502
   14     22       5    0.340 0.04548        0.262        0.442

                glipizide=No, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     52       2    0.962  0.0267        0.911        1.000
    4     47       2    0.921  0.0381        0.849        0.998
    5     35       1    0.894  0.0452        0.810        0.987
    6     29       1    0.863  0.0531        0.765        0.974
    7     25       3    0.760  0.0730        0.629        0.917
    8     18       1    0.718  0.0803        0.576        0.894
   10     11       1    0.652  0.0959        0.489        0.870
   13      5       1    0.522  0.1397        0.309        0.882

                glipizide=Steady, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    8      3       1    0.667   0.272          0.3            1
   11      1       1    0.000     NaN           NA           NA

                glipizide=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   4988     120    0.976 0.00217        0.972        0.980
    2   4401     169    0.938 0.00351        0.932        0.945
    3   3615     204    0.886 0.00490        0.876        0.895
    4   2719     184    0.826 0.00625        0.813        0.838
    5   1976     133    0.770 0.00746        0.756        0.785
    6   1444      99    0.717 0.00863        0.701        0.734
    7   1055      64    0.674 0.00967        0.655        0.693
    8    731      63    0.616 0.01127        0.594        0.638
    9    512      35    0.574 0.01254        0.549        0.599
   10    361      26    0.532 0.01401        0.505        0.560
   11    254      23    0.484 0.01595        0.454        0.516
   12    170      17    0.436 0.01817        0.401        0.473
   13    100       6    0.410 0.01997        0.372        0.451
   14     53      16    0.286 0.02935        0.234        0.350

                glipizide=Steady, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    496       9    0.982 0.00599        0.970        0.994
    2    440      20    0.937 0.01130        0.915        0.960
    3    352      15    0.897 0.01480        0.869        0.927
    4    251      18    0.833 0.02005        0.795        0.873
    5    187       8    0.797 0.02281        0.754        0.843
    6    130       9    0.742 0.02768        0.690        0.798
    7     94       8    0.679 0.03313        0.617        0.747
    8     66       6    0.617 0.03852        0.546        0.698
    9     37       3    0.567 0.04495        0.486        0.662
   10     28       1    0.547 0.04769        0.461        0.649
   11     22       4    0.447 0.05954        0.345        0.581
   12      8       1    0.392 0.07384        0.271        0.567

                glipizide=Steady, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     12       1    0.917  0.0798       0.7729            1
    4      9       1    0.815  0.1194       0.6114            1
    6      7       1    0.698  0.1486       0.4603            1
   10      4       1    0.524  0.1878       0.2594            1
   12      2       1    0.262  0.2077       0.0554            1

                glipizide=Up, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    342       3    0.991 0.00504        0.981        1.000
    2    331       6    0.973 0.00879        0.956        0.991
    3    308      12    0.935 0.01366        0.909        0.963
    4    270      14    0.887 0.01808        0.852        0.923
    5    222       6    0.863 0.02007        0.824        0.903
    6    184       9    0.821 0.02351        0.776        0.868
    7    149       8    0.777 0.02692        0.726        0.831
    8    118       7    0.731 0.03044        0.673        0.793
    9     91       4    0.698 0.03306        0.637        0.766
   10     73       6    0.641 0.03775        0.571        0.719
   11     55       2    0.618 0.03981        0.544        0.701
   12     38       2    0.585 0.04385        0.505        0.678
   13     25       2    0.538 0.05134        0.447        0.649
   14     13       7    0.248 0.07812        0.134        0.460

                glipizide=Up, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     36       1    0.972  0.0274        0.920        1.000
    4     32       2    0.911  0.0489        0.821        1.000
    5     23       1    0.872  0.0607        0.761        0.999
    7     17       1    0.821  0.0758        0.685        0.983
    9     10       1    0.738  0.1035        0.561        0.972
   10      8       2    0.554  0.1371        0.341        0.900
   11      5       1    0.443  0.1478        0.230        0.852

                glipizide=Up, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    7      3       1    0.667   0.272          0.3            1
   10      1       1    0.000     NaN           NA           NA

> 
