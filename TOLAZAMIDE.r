
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
>  dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : TOLAZAMIDE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR TOLAZAMIDE DRUG
> 
> 
> acarbose_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ tolazamide ,data = dia)
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
+       acra_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ tolazamide + insulin ,data = dia1)
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
+              acra_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ tolazamide + glipizide  ,data = dia1)
+            
+              acra_combi_test_1 = mean(summary(acra_combi_fit_R$surv))
+              #COMBINATION - 2
+              acra_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + tolazamide ,data = dia1)
+             
+              acra_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              acra_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + tolazamide ,data = dia1)
+              
+              acra_combi_test_3 = mean(summary(acra_combi_test_3$surv))
+              #COMBINATION - 4
+              acra_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ tolazamide + rosiglitazone ,data = dia1)
+             
+              acra_combi_test_4 = mean(summary(acra_combi_fit_4$surv))
+              
+              if(pio_combi_test_1 > 0.5)
+              { 
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_1)
+                          print("COMBINATION OF TROGLITAZONE + PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF TROGLITAZONE + GLYBURIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF TROGLITAZONE + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF TROGLITAZONE + ROSIGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
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
> autoplot( acarbose_km_fit,main = "TOLAZAMIDE")
Error: Aesthetics can not vary with a ribbon
Run `rlang::last_error()` to see where the error occurred.
> summary(acarbose_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ tolazamide, 
    data = dia)

                tolazamide=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1 101727   14200   0.8604 0.001087      0.85828       0.8625
    2  87527   17217   0.6912 0.001449      0.68833       0.6940
    3  70310   17750   0.5167 0.001567      0.51362       0.5198
    4  52560   13921   0.3798 0.001522      0.37686       0.3828
    5  38639    9962   0.2819 0.001411      0.27915       0.2847
    6  28677    7535   0.2078 0.001272      0.20535       0.2103
    7  21142    5857   0.1503 0.001120      0.14808       0.1525
    8  15285    4388   0.1071 0.000970      0.10524       0.1090
    9  10897    3001   0.0776 0.000839      0.07599       0.0793
   10   7896    2341   0.0546 0.000712      0.05323       0.0560
   11   5555    1855   0.0364 0.000587      0.03524       0.0375
   12   3700    1448   0.0221 0.000461      0.02125       0.0231
   13   2252    1210   0.0102 0.000316      0.00964       0.0109
   14   1042    1042   0.0000      NaN           NA           NA

                tolazamide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     38       8   0.7895  0.0661       0.6699        0.930
    2     30       7   0.6053  0.0793       0.4682        0.782
    3     23       5   0.4737  0.0810       0.3388        0.662
    4     18       3   0.3947  0.0793       0.2663        0.585
    5     15       4   0.2895  0.0736       0.1759        0.476
    6     11       4   0.1842  0.0629       0.0943        0.360
    7      7       2   0.1316  0.0548       0.0581        0.298
    8      5       3   0.0526  0.0362       0.0137        0.203
    9      2       1   0.0263  0.0260       0.0038        0.182
   10      1       1   0.0000     NaN           NA           NA

                tolazamide=Up 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           3            1            1            0          NaN           NA           NA 

> plot( acarbose_km_fit,main = "TOLAZAMIDE")
> #DRUG + INSULIN
> glipi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ tolazamide + insulin ,data = dia1)
> plot(glipi_insulin_fit,main = "TOLAZAMIDE+ INSULIN")
> summary(glipi_insulin_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ tolazamide + 
    insulin, data = dia1)

                tolazamide=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6450     129    0.980 0.00174        0.977        0.983
    2   5885     234    0.941 0.00301        0.935        0.947
    3   4953     266    0.890 0.00414        0.882        0.899
    4   3912     269    0.829 0.00528        0.819        0.840
    5   2996     167    0.783 0.00608        0.771        0.795
    6   2302     141    0.735 0.00692        0.722        0.749
    7   1753     118    0.686 0.00781        0.670        0.701
    8   1298     109    0.628 0.00889        0.611        0.646
    9    931      76    0.577 0.00992        0.558        0.597
   10    674      70    0.517 0.01118        0.495        0.539
   11    470      31    0.483 0.01200        0.460        0.507
   12    327      31    0.437 0.01339        0.412        0.464
   13    205      32    0.369 0.01582        0.339        0.401
   14     96      25    0.273 0.02024        0.236        0.315

                tolazamide=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  20694     583    0.972 0.00115        0.970        0.974
    2  17694     778    0.929 0.00186        0.925        0.933
    3  14098     829    0.874 0.00254        0.870        0.879
    4  10479     647    0.820 0.00315        0.814        0.827
    5   7589     493    0.767 0.00375        0.760        0.775
    6   5595     363    0.717 0.00432        0.709        0.726
    7   4062     314    0.662 0.00499        0.652        0.672
    8   2901     224    0.611 0.00566        0.600        0.622
    9   2031     150    0.566 0.00632        0.553        0.578
   10   1449     119    0.519 0.00710        0.506        0.533
   11    994      81    0.477 0.00792        0.462        0.493
   12    654      71    0.425 0.00914        0.408        0.443
   13    406      53    0.370 0.01066        0.349        0.391
   14    189      48    0.276 0.01415        0.249        0.305

                tolazamide=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  13913     334    0.976 0.00130        0.973        0.979
    2  12321     523    0.935 0.00216        0.930        0.939
    3   9962     587    0.879 0.00300        0.874        0.885
    4   7527     533    0.817 0.00381        0.810        0.825
    5   5433     372    0.761 0.00452        0.752        0.770
    6   3996     300    0.704 0.00525        0.694        0.714
    7   2909     210    0.653 0.00593        0.642        0.665
    8   2084     197    0.592 0.00681        0.578        0.605
    9   1454     116    0.544 0.00754        0.530        0.559
   10   1028      88    0.498 0.00838        0.482        0.514
   11    731      41    0.470 0.00897        0.453        0.488
   12    484      55    0.416 0.01045        0.396        0.437
   13    281      37    0.362 0.01236        0.338        0.387
   14    131      40    0.251 0.01690        0.220        0.287

                tolazamide=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5832     116    0.980 0.00183        0.977        0.984
    2   5359     177    0.948 0.00297        0.942        0.954
    3   4616     212    0.904 0.00407        0.896        0.912
    4   3735     195    0.857 0.00507        0.847        0.867
    5   2928     166    0.808 0.00603        0.797        0.820
    6   2269     144    0.757 0.00700        0.744        0.771
    7   1735     110    0.709 0.00791        0.694        0.725
    8   1302      94    0.658 0.00893        0.641        0.676
    9    956      70    0.610 0.00996        0.591        0.630
   10    710      59    0.559 0.01110        0.538        0.581
   11    493      42    0.511 0.01235        0.488        0.536
   12    329      36    0.455 0.01409        0.429        0.484
   13    191      27    0.391 0.01668        0.360        0.425
   14     97      22    0.302 0.02104        0.264        0.347

                tolazamide=Steady, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      4       1     0.75   0.217        0.426            1
    6      3       1     0.50   0.250        0.188            1
    8      1       1     0.00     NaN           NA           NA

                tolazamide=Steady, insulin=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                tolazamide=Up, insulin=No     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + tolazamide  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + TOLAZAMIDE")
> summary(met_combi_fit_1)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    tolazamide, data = dia1)

                pioglitazone=Down, tolazamide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     59       2    0.966  0.0236        0.921        1.000
    4     55       5    0.878  0.0431        0.798        0.967
    5     46       3    0.821  0.0515        0.726        0.928
    6     31       2    0.768  0.0603        0.659        0.896
    9     17       2    0.678  0.0802        0.537        0.855
   10     12       1    0.621  0.0912        0.466        0.828
   11      7       1    0.532  0.1134        0.351        0.808
   14      3       2    0.177  0.1498        0.034        0.928

                pioglitazone=No, tolazamide=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  43358    1098    0.975 0.000755        0.973        0.976
    2  38113    1610    0.934 0.001237        0.931        0.936
    3  31043    1753    0.881 0.001691        0.877        0.884
    4  23674    1528    0.824 0.002117        0.820        0.828
    5  17463    1099    0.772 0.002495        0.767        0.777
    6  13056     887    0.720 0.002881        0.714        0.725
    7   9642     708    0.667 0.003283        0.660        0.673
    8   6991     580    0.611 0.003729        0.604        0.619
    9   4932     390    0.563 0.004161        0.555        0.571
   10   3530     304    0.515 0.004640        0.506        0.524
   11   2455     180    0.477 0.005081        0.467        0.487
   12   1630     181    0.424 0.005846        0.413        0.436
   13    976     137    0.364 0.006890        0.351        0.378
   14    466     125    0.267 0.009020        0.250        0.285

                pioglitazone=No, tolazamide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      5       1      0.8   0.179       0.5161            1
    6      4       1      0.6   0.219       0.2933            1
    8      2       1      0.3   0.239       0.0631            1

                pioglitazone=No, tolazamide=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                pioglitazone=Steady, tolazamide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   3347      64    0.981 0.00237        0.976        0.986
    2   2966      99    0.948 0.00396        0.940        0.956
    3   2420     137    0.894 0.00581        0.883        0.906
    4   1828     109    0.841 0.00738        0.827        0.856
    5   1355      93    0.783 0.00898        0.766        0.801
    6   1004      55    0.740 0.01018        0.721        0.761
    7    733      40    0.700 0.01146        0.678        0.723
    8    528      42    0.644 0.01339        0.619        0.671
    9    387      17    0.616 0.01445        0.588        0.645
   10    292      30    0.553 0.01697        0.521        0.587
   11    208      12    0.521 0.01832        0.486        0.558
   12    148      12    0.479 0.02049        0.440        0.521
   13     96      11    0.424 0.02390        0.379        0.473
   14     40       6    0.360 0.03139        0.304        0.427

                pioglitazone=Up, tolazamide=No     
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
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + tolazamide ,data = dia1)
Error in eval(predvars, data, env) : object 'glyrbuide' not found
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLYRUBIDE + TOLAZAMIDE")
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + tolazamide ,data = dia1)
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLYRUBIDE + TOLAZAMIDE")
> #DRUG COMBI - 3
> met_combi_fit_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + tolazamide ,data = dia1)
> plot(met_combi_fit_3,main = "DRUG COMBINATION : METFORMIN + TOLAZAMIDE")
> summary(met_combi_fit_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ metformin + 
    tolazamide, data = dia1)

                metformin=Down, tolazamide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    259       4    0.985 0.00766       0.9697        1.000
    2    242       6    0.960 0.01236       0.9362        0.985
    3    212       7    0.928 0.01678       0.8961        0.962
    4    178       8    0.887 0.02156       0.8455        0.930
    5    126      10    0.816 0.02915       0.7612        0.876
    6     99       9    0.742 0.03548       0.6758        0.815
    7     77       8    0.665 0.04095       0.5894        0.750
    8     51       6    0.587 0.04696       0.5016        0.686
   10     26       4    0.497 0.05747       0.3957        0.623
   11     20       2    0.447 0.06152       0.3412        0.585
   13      9       2    0.348 0.07826       0.2235        0.540
   14      6       3    0.174 0.08102       0.0697        0.433

                metformin=No, tolazamide=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  38247    1000    0.974 0.000816        0.972        0.975
    2  33519    1418    0.933 0.001325        0.930        0.935
    3  27336    1555    0.880 0.001808        0.876        0.883
    4  20893    1358    0.822 0.002260        0.818        0.827
    5  15468     981    0.770 0.002661        0.765        0.776
    6  11599     786    0.718 0.003063        0.712        0.724
    7   8576     626    0.666 0.003483        0.659        0.673
    8   6246     508    0.612 0.003942        0.604        0.619
    9   4432     340    0.565 0.004384        0.556        0.573
   10   3202     285    0.514 0.004902        0.505        0.524
   11   2234     172    0.475 0.005374        0.464        0.485
   12   1478     168    0.421 0.006169        0.409        0.433
   13    885     117    0.365 0.007184        0.351        0.380
   14    421     103    0.276 0.009380        0.258        0.295

                metformin=No, tolazamide=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       6.000        2.000        1.000        0.500        0.354        0.125        1.000 

                metformin=No, tolazamide=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Steady, tolazamide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   7950     154    0.981 0.00155        0.978        0.984
    2   7088     280    0.942 0.00271        0.937        0.947
    3   5714     318    0.889 0.00384        0.882        0.897
    4   4280     265    0.834 0.00487        0.825        0.844
    5   3110     198    0.781 0.00584        0.770        0.793
    6   2273     141    0.733 0.00676        0.720        0.746
    7   1661     111    0.684 0.00774        0.669        0.699
    8   1187     106    0.623 0.00904        0.605        0.641
    9    831      67    0.573 0.01018        0.553        0.593
   10    575      43    0.530 0.01132        0.508        0.552
   11    397      19    0.504 0.01218        0.481        0.529
   12    276      24    0.461 0.01403        0.434        0.489
   13    173      28    0.386 0.01745        0.353        0.422
   14     80      27    0.256 0.02345        0.214        0.306

                metformin=Steady, tolazamide=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           5            1            1            0          NaN           NA           NA 

                metformin=Up, tolazamide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    433       4    0.991 0.00460        0.982        1.000
    2    410       8    0.971 0.00813        0.956        0.987
    3    367      14    0.934 0.01247        0.910        0.959
    4    302      13    0.894 0.01617        0.863        0.926
    5    242       9    0.861 0.01899        0.824        0.899
    6    191      12    0.807 0.02335        0.762        0.854
    7    145       7    0.768 0.02646        0.718        0.822
    8    101       4    0.737 0.02946        0.682        0.798
    9     75       5    0.688 0.03474        0.623        0.760
   10     58       4    0.641 0.03963        0.568        0.723
   11     37       2    0.606 0.04442        0.525        0.700
   12     26       1    0.583 0.04845        0.495        0.686
   13     16       2    0.510 0.06418        0.399        0.653
   14      6       2    0.340 0.10707        0.183        0.630

                metformin=Up, tolazamide=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           8            1            1            0          NaN           NA           NA 

> #DRUG COMBI - 4
> met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ tolazamide + rosiglitazone ,data = dia1)
> plot(met_combi_fit_4,main = "DRUG COMBINATION : TOLAZAMIDE + ROSIGLITAZONE")
> summary(met_combi_fit_4)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ tolazamide + 
    rosiglitazone, data = dia1)

                tolazamide=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     25       1    0.960  0.0392        0.886            1
    5     18       2    0.853  0.0792        0.711            1
    8      7       1    0.731  0.1317        0.514            1
   11      4       1    0.549  0.1866        0.282            1

                tolazamide=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  43816    1098    0.975 0.000747        0.973        0.976
    2  38505    1619    0.934 0.001227        0.932        0.936
    3  31352    1792    0.881 0.001685        0.877        0.884
    4  23904    1512    0.825 0.002101        0.821        0.829
    5  17696    1129    0.772 0.002483        0.767        0.777
    6  13240     897    0.720 0.002864        0.714        0.726
    7   9779     714    0.667 0.003261        0.661        0.674
    8   7083     585    0.612 0.003703        0.605        0.620
    9   5027     391    0.565 0.004125        0.557        0.573
   10   3607     318    0.515 0.004610        0.506        0.524
   11   2514     177    0.479 0.005026        0.469        0.489
   12   1694     185    0.426 0.005762        0.415        0.438
   13   1014     144    0.366 0.006803        0.353        0.379
   14    483     129    0.268 0.008893        0.251        0.286

                tolazamide=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2975      64    0.978 0.00266        0.973        0.984
    2   2656      92    0.945 0.00432        0.936        0.953
    3   2184     100    0.901 0.00590        0.890        0.913
    4   1663     128    0.832 0.00802        0.816        0.848
    5   1184      66    0.786 0.00939        0.767        0.804
    6    869      49    0.741 0.01078        0.720        0.763
    7    636      34    0.702 0.01216        0.678        0.726
    8    470      37    0.646 0.01420        0.619        0.675
    9    320      21    0.604 0.01600        0.573        0.636
   10    232      15    0.565 0.01786        0.531        0.601
   11    160      17    0.505 0.02108        0.465        0.548
   12     90       7    0.466 0.02411        0.421        0.515
   13     62       4    0.436 0.02683        0.386        0.491
   14     27       6    0.339 0.04062        0.268        0.429

                tolazamide=No, rosiglitazone=Up     
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

                tolazamide=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    6      4       1    0.750   0.217       0.4259            1
    8      2       1    0.375   0.286       0.0839            1

                tolazamide=Steady, rosiglitazone=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           5            1            1            0          NaN           NA           NA 

                tolazamide=Up, rosiglitazone=No     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> 
