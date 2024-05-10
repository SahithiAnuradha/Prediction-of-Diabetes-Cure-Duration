
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
> library(ranger)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> library(ggfortify)
> library(ggplot2)
> dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : ACARBOSE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR ACARBOSE DRUG
> 
> 
> acarbose_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ acarbose ,data = dia)
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
+       acra_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ acrabose + insulin ,data = dia1)
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
+              acra_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ acrabose + glipizide  ,data = dia1)
+            
+              acra_combi_test_1 = mean(summary(acra_combi_fit_R$surv))
+              #COMBINATION - 2
+              acra_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + acrabose ,data = dia1)
+             
+              acra_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              acra_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + acrabose ,data = dia1)
+              
+              acra_combi_test_3 = mean(summary(acra_combi_test_3$surv))
+              #COMBINATION - 4
+              acra_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ acrabose + rosiglitazone ,data = dia1)
+             
+              acra_combi_test_4 = mean(summary(acra_combi_fit_4$surv))
+              
+              if(pio_combi_test_1 > 0.5)
+              { 
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_1)
+                          print("COMBINATION OF ACRABOSE + PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF ACRABOSE + GLYBURIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF ACRABOSE + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF ACRABOSE + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
+                     }
+               else  {
+ 
+                    print("COMBINATIONS ALSO DID NOT GIVE ANY RESULT")
+                    }
+ 
+ }
+ }
Error in eval(predvars, data, env) : object 'acrabose' not found
> dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : ACARBOSE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR ACARBOSE DRUG
> 
> 
> acarbose_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ acarbose ,data = dia)
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
+       acra_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ acarbose + insulin ,data = dia1)
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
+              acra_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ acarbose + glipizide  ,data = dia1)
+            
+              acra_combi_test_1 = mean(summary(acra_combi_fit_R$surv))
+              #COMBINATION - 2
+              acra_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + acarbose ,data = dia1)
+             
+              acra_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              acra_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + acarbose ,data = dia1)
+              
+              acra_combi_test_3 = mean(summary(acra_combi_test_3$surv))
+              #COMBINATION - 4
+              acra_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ acarbose + rosiglitazone ,data = dia1)
+             
+              acra_combi_test_4 = mean(summary(acra_combi_fit_4$surv))
+              
+              if(pio_combi_test_1 > 0.5)
+              { 
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_1)
+                          print("COMBINATION OF ACARBOSE + PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF ACARBOSE + GLYBURIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF ACARBOSE + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF ACARBOSE + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
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
> summary(acrabose_km_fit)
Error in summary(acrabose_km_fit) : object 'acrabose_km_fit' not found
> autoplot( acarbose_km_fit,main = "ACARBOSE")
> summary(acrabose_km_fit)
Error in summary(acrabose_km_fit) : object 'acrabose_km_fit' not found
> summary(acarbose_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ acarbose, 
    data = dia)

                acarbose=Down 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      3       2    0.333   0.272       0.0673            1
    6      1       1    0.000     NaN           NA           NA

                acarbose=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1 101458   14171   0.8603 0.001088       0.8582       0.8625
    2  87287   17178   0.6910 0.001451       0.6882       0.6939
    3  70109   17710   0.5165 0.001569       0.5134       0.5195
    4  52399   13874   0.3797 0.001524       0.3767       0.3827
    5  38525    9924   0.2819 0.001413       0.2791       0.2847
    6  28601    7519   0.2078 0.001274       0.2053       0.2103
    7  21082    5844   0.1502 0.001122       0.1480       0.1524
    8  15238    4378   0.1070 0.000971       0.1052       0.1090
    9  10860    2994   0.0775 0.000840       0.0759       0.0792
   10   7866    2337   0.0545 0.000713       0.0531       0.0559
   11   5529    1851   0.0363 0.000587       0.0351       0.0374
   12   3678    1438   0.0221 0.000461       0.0212       0.0230
   13   2240    1205   0.0102 0.000315       0.0096       0.0108
   14   1035    1035   0.0000      NaN           NA           NA

                acarbose=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    295      35   0.8814 0.01883       0.8452       0.9190
    2    260      46   0.7254 0.02598       0.6762       0.7782
    3    214      46   0.5695 0.02883       0.5157       0.6289
    4    168      49   0.4034 0.02856       0.3511       0.4634
    5    119      39   0.2712 0.02588       0.2249       0.3270
    6     80      16   0.2169 0.02400       0.1747       0.2695
    7     64      13   0.1729 0.02202       0.1347       0.2219
    8     51      12   0.1322 0.01972       0.0987       0.1771
    9     39       8   0.1051 0.01785       0.0753       0.1466
   10     31       5   0.0881 0.01651       0.0611       0.1272
   11     26       4   0.0746 0.01530       0.0499       0.1115
   12     22      10   0.0407 0.01150       0.0234       0.0708
   13     12       5   0.0237 0.00886       0.0114       0.0493
   14      7       7   0.0000     NaN           NA           NA

                acarbose=Up 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     10       2      0.8  0.1265       0.5868        1.000
    4      8       1      0.7  0.1449       0.4665        1.000
    5      7       1      0.6  0.1549       0.3617        0.995
    6      6       3      0.3  0.1449       0.1164        0.773
    7      3       2      0.1  0.0949       0.0156        0.642
    8      1       1      0.0     NaN           NA           NA

> #DRUG + INSULIN
> glipi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ acarbose + insulin ,data = dia1)
> plot(glipi_insulin_fit,main = "ACARBOSE + INSULIN")
> summary(glipi_insulin_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ acarbose + 
    insulin, data = dia1)

                acarbose=Down, insulin=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           6            1            1            0          NaN           NA           NA 

                acarbose=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6430     128    0.980 0.00174        0.977        0.984
    2   5869     234    0.941 0.00301        0.935        0.947
    3   4938     265    0.891 0.00415        0.882        0.899
    4   3901     269    0.829 0.00529        0.819        0.840
    5   2986     167    0.783 0.00609        0.771        0.795
    6   2295     141    0.735 0.00693        0.721        0.748
    7   1748     118    0.685 0.00783        0.670        0.701
    8   1294     109    0.627 0.00891        0.610        0.645
    9    930      76    0.576 0.00993        0.557        0.596
   10    673      70    0.516 0.01119        0.495        0.539
   11    469      31    0.482 0.01201        0.459        0.506
   12    327      31    0.436 0.01338        0.411        0.463
   13    205      32    0.368 0.01581        0.339        0.401
   14     96      25    0.272 0.02022        0.235        0.315

                acarbose=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  20599     581    0.972 0.00115        0.970        0.974
    2  17607     777    0.929 0.00187        0.925        0.933
    3  14021     825    0.874 0.00255        0.869        0.879
    4  10418     645    0.820 0.00316        0.814        0.826
    5   7548     493    0.767 0.00376        0.759        0.774
    6   5572     364    0.716 0.00434        0.708        0.725
    7   4041     313    0.661 0.00501        0.651        0.671
    8   2885     225    0.609 0.00568        0.598        0.621
    9   2018     149    0.564 0.00634        0.552        0.577
   10   1441     119    0.518 0.00711        0.504        0.532
   11    987      81    0.475 0.00794        0.460        0.491
   12    647      71    0.423 0.00917        0.406        0.442
   13    402      53    0.367 0.01070        0.347        0.389
   14    185      48    0.272 0.01424        0.246        0.301

                acarbose=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  13887     333    0.976 0.00130        0.973        0.979
    2  12298     523    0.935 0.00217        0.930        0.939
    3   9942     584    0.880 0.00300        0.874        0.886
    4   7512     533    0.817 0.00382        0.810        0.825
    5   5420     370    0.761 0.00453        0.753        0.770
    6   3987     299    0.704 0.00526        0.694        0.715
    7   2901     210    0.653 0.00594        0.642        0.665
    8   2078     196    0.592 0.00682        0.579        0.605
    9   1450     116    0.544 0.00756        0.530        0.559
   10   1025      88    0.498 0.00839        0.481        0.514
   11    728      41    0.470 0.00899        0.452        0.488
   12    482      55    0.416 0.01047        0.396        0.437
   13    281      37    0.361 0.01237        0.338        0.386
   14    131      40    0.251 0.01689        0.220        0.286

                acarbose=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5803     115    0.980 0.00183        0.977        0.984
    2   5333     176    0.948 0.00298        0.942        0.954
    3   4593     212    0.904 0.00409        0.896        0.912
    4   3716     193    0.857 0.00508        0.847        0.867
    5   2912     166    0.808 0.00604        0.797        0.820
    6   2257     142    0.757 0.00701        0.744        0.771
    7   1728     110    0.709 0.00793        0.694        0.725
    8   1297      94    0.658 0.00895        0.640        0.676
    9    952      70    0.609 0.00999        0.590        0.629
   10    706      59    0.559 0.01114        0.537        0.581
   11    490      42    0.511 0.01239        0.487        0.536
   12    327      36    0.454 0.01413        0.428        0.483
   13    190      27    0.390 0.01672        0.358        0.424
   14     97      22    0.301 0.02102        0.263        0.346

                acarbose=Steady, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     20       1    0.950  0.0487        0.859            1
    3     15       1    0.887  0.0762        0.749            1

                acarbose=Steady, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    102       2    0.980  0.0137        0.954        1.000
    2     93       1    0.970  0.0172        0.937        1.000
    3     82       4    0.923  0.0283        0.869        0.980
    4     63       2    0.893  0.0341        0.829        0.963
    7     22       1    0.853  0.0513        0.758        0.959
    9     13       1    0.787  0.0788        0.647        0.958

                acarbose=Steady, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     27       1    0.963  0.0363        0.894        1.000
    3     20       3    0.819  0.0829        0.671        0.998
    5     13       2    0.693  0.1078        0.510        0.940
    6      9       1    0.616  0.1202        0.420        0.903
    8      7       1    0.528  0.1313        0.324        0.859

                acarbose=Steady, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     26       1    0.962  0.0377        0.890            1
    2     23       1    0.920  0.0545        0.819            1
    4     16       2    0.805  0.0898        0.647            1

                acarbose=Up, insulin=No     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       5.000        3.000        1.000        0.667        0.272        0.300        1.000 

                acarbose=Up, insulin=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                acarbose=Up, insulin=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           6            1            1            0          NaN           NA           NA 

> 
> #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + acarbose  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + ACARBOSE")
> summary(met_combi_fit_1)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    acarbose, data = dia1)

                pioglitazone=Down, acarbose=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     59       2    0.966  0.0236        0.921        1.000
    4     55       5    0.878  0.0431        0.798        0.967
    5     46       3    0.821  0.0515        0.726        0.928
    6     31       2    0.768  0.0603        0.659        0.896
    9     17       2    0.678  0.0802        0.537        0.855
   10     12       1    0.621  0.0912        0.466        0.828
   11      7       1    0.532  0.1134        0.351        0.808
   14      3       2    0.177  0.1498        0.034        0.928

                pioglitazone=No, acarbose=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           6            1            1            0          NaN           NA           NA 

                pioglitazone=No, acarbose=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  43213    1093    0.975 0.000755        0.973        0.976
    2  37984    1608    0.933 0.001240        0.931        0.936
    3  30930    1746    0.881 0.001694        0.877        0.884
    4  23589    1524    0.824 0.002121        0.820        0.828
    5  17399    1098    0.772 0.002501        0.767        0.777
    6  13017     885    0.719 0.002887        0.714        0.725
    7   9612     707    0.666 0.003290        0.660        0.673
    8   6969     580    0.611 0.003736        0.604        0.618
    9   4917     389    0.563 0.004168        0.555        0.571
   10   3519     304    0.514 0.004647        0.505        0.523
   11   2445     180    0.476 0.005090        0.466        0.486
   12   1623     181    0.423 0.005856        0.412        0.435
   13    974     137    0.364 0.006895        0.350        0.377
   14    465     125    0.266 0.009016        0.249        0.284

                pioglitazone=No, acarbose=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    150       5    0.967  0.0147        0.938        0.996
    2    132       2    0.952  0.0177        0.918        0.987
    3    115       7    0.894  0.0270        0.843        0.949
    4     84       4    0.851  0.0330        0.789        0.919
    5     62       1    0.838  0.0352        0.771        0.910
    6     38       1    0.816  0.0406        0.740        0.899
    7     31       1    0.789  0.0471        0.702        0.887
    8     23       1    0.755  0.0562        0.653        0.874
    9     16       1    0.708  0.0697        0.584        0.859

                pioglitazone=No, acarbose=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      5       1      0.8   0.179        0.516            1
    6      4       1      0.6   0.219        0.293            1

                pioglitazone=Steady, acarbose=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   3324      64    0.981 0.00238        0.976        0.985
    2   2945      99    0.948 0.00399        0.940        0.956
    3   2400     136    0.894 0.00585        0.883        0.906
    4   1809     109    0.840 0.00743        0.826        0.855
    5   1341      92    0.783 0.00903        0.765        0.800
    6    994      55    0.739 0.01024        0.719        0.760
    7    724      40    0.698 0.01154        0.676        0.721
    8    520      42    0.642 0.01349        0.616        0.669
    9    381      17    0.613 0.01457        0.585        0.643
   10    288      30    0.549 0.01710        0.517        0.584
   11    205      12    0.517 0.01845        0.482        0.555
   12    145      12    0.474 0.02065        0.436        0.517
   13     94      11    0.419 0.02408        0.374        0.469
   14     38       6    0.353 0.03202        0.295        0.422

                pioglitazone=Steady, acarbose=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     20       1    0.950  0.0487        0.859            1
    5     14       1    0.882  0.0795        0.739            1

                pioglitazone=Up, acarbose=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2    115       3    0.974  0.0149       0.9452        1.000
    3    105       2    0.955  0.0195       0.9178        0.994
    4     94       2    0.935  0.0238       0.8895        0.983
    5     80       3    0.900  0.0303       0.8424        0.961
    6     69       4    0.848  0.0382       0.7762        0.926
    7     58       4    0.789  0.0454       0.7052        0.883
    8     45       2    0.754  0.0497       0.6629        0.858
    9     35       3    0.690  0.0578       0.5852        0.813
   10     26       1    0.663  0.0613       0.5531        0.795
   11     17       2    0.585  0.0749       0.4552        0.752
   13      6       1    0.488  0.1087       0.3149        0.755
   14      3       2    0.163  0.1376       0.0309        0.854

                pioglitazone=Up, acarbose=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 2
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + acarbose ,data = dia1)
Error in eval(predvars, data, env) : object 'glyrbuide' not found
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLYRUBIDE + ACARBOSE")
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
> met_combi_fit_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + acarbose ,data = dia1)
> plot(met_combi_fit_3,main = "DRUG COMBINATION : METFORMIN + ACARBOSE")
> summary(met_combi_fit_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ metformin + 
    acarbose, data = dia1)

                metformin=Down, acarbose=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    258       4    0.984 0.00769       0.9695        1.000
    2    241       6    0.960 0.01241       0.9360        0.985
    3    211       7    0.928 0.01685       0.8957        0.962
    4    177       8    0.886 0.02165       0.8447        0.930
    5    126      10    0.816 0.02920       0.7606        0.875
    6     99       9    0.742 0.03550       0.6753        0.815
    7     77       8    0.665 0.04095       0.5890        0.750
    8     51       6    0.586 0.04696       0.5013        0.686
   10     26       4    0.496 0.05745       0.3955        0.623
   11     20       2    0.447 0.06149       0.3410        0.585
   13      9       2    0.347 0.07822       0.2234        0.540
   14      6       3    0.174 0.08097       0.0696        0.433

                metformin=Down, acarbose=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=No, acarbose=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           6            1            1            0          NaN           NA           NA 

                metformin=No, acarbose=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  38117     997    0.974 0.000817        0.972        0.975
    2  33403    1417    0.933 0.001329        0.930        0.935
    3  27233    1549    0.879 0.001812        0.876        0.883
    4  20811    1356    0.822 0.002266        0.818        0.827
    5  15402     979    0.770 0.002667        0.765        0.775
    6  11557     784    0.718 0.003070        0.712        0.724
    7   8543     625    0.665 0.003491        0.658        0.672
    8   6222     507    0.611 0.003950        0.603        0.619
    9   4417     340    0.564 0.004393        0.555        0.573
   10   3192     285    0.514 0.004910        0.504        0.523
   11   2225     172    0.474 0.005383        0.463        0.485
   12   1472     168    0.420 0.006178        0.408        0.432
   13    882     117    0.364 0.007191        0.350        0.378
   14    418     103    0.274 0.009395        0.257        0.293

                metformin=No, acarbose=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    133       3    0.977  0.0129        0.953        1.000
    2    117       1    0.969  0.0152        0.940        0.999
    3    103       6    0.913  0.0266        0.862        0.966
    4     79       2    0.890  0.0305        0.832        0.951
    5     62       1    0.875  0.0332        0.812        0.943
    6     40       1    0.853  0.0389        0.780        0.933
    7     32       1    0.827  0.0460        0.741        0.922
    8     24       1    0.792  0.0555        0.691        0.909

                metformin=No, acarbose=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      4       1     0.75   0.217        0.426            1
    6      3       1     0.50   0.250        0.188            1

                metformin=Steady, acarbose=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   7913     152    0.981 0.00154        0.978        0.984
    2   7055     279    0.942 0.00272        0.937        0.947
    3   5684     316    0.890 0.00384        0.882        0.897
    4   4258     263    0.835 0.00488        0.825        0.844
    5   3096     198    0.781 0.00586        0.770        0.793
    6   2264     141    0.733 0.00678        0.719        0.746
    7   1653     111    0.683 0.00776        0.668        0.699
    8   1181     106    0.622 0.00907        0.605        0.640
    9    825      67    0.572 0.01022        0.552        0.592
   10    569      43    0.528 0.01137        0.507        0.551
   11    392      19    0.503 0.01225        0.479        0.527
   12    271      24    0.458 0.01414        0.431        0.487
   13    171      28    0.383 0.01755        0.350        0.419
   14     79      27    0.252 0.02349        0.210        0.303

                metformin=Steady, acarbose=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     37       2    0.946  0.0372        0.876        1.000
    2     33       1    0.917  0.0458        0.832        1.000
    3     30       2    0.856  0.0598        0.747        0.982
    4     22       2    0.778  0.0755        0.643        0.941
    5     14       1    0.723  0.0883        0.569        0.918

                metformin=Steady, acarbose=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Up, acarbose=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    431       4    0.991 0.00462        0.982        1.000
    2    408       8    0.971 0.00817        0.955        0.987
    3    366      14    0.934 0.01251        0.910        0.959
    4    301      13    0.894 0.01622        0.863        0.926
    5    242       9    0.861 0.01903        0.824        0.899
    6    191      12    0.806 0.02337        0.762        0.854
    7    145       7    0.768 0.02648        0.717        0.821
    8    100       5    0.729 0.03021        0.672        0.791
    9     74       4    0.690 0.03441        0.626        0.761
   10     58       4    0.642 0.03941        0.569        0.724
   11     37       2    0.607 0.04427        0.527        0.701
   12     26       1    0.584 0.04834        0.497        0.687
   13     16       2    0.511 0.06420        0.400        0.654
   14      6       2    0.341 0.10727        0.184        0.632

                metformin=Up, acarbose=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           9            1            1            0          NaN           NA           NA 

> #DRUG COMBI - 4
> met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ acarbose + rosiglitazone ,data = dia1)
> plot(met_combi_fit_4,main = "DRUG COMBINATION : ACARBOSE + ROSIGLITAZONE")
> summary(met_combi_fit_4)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ acarbose + 
    rosiglitazone, data = dia1)

                acarbose=Down, rosiglitazone=No     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
           6            1            1            0          NaN           NA           NA 

                acarbose=Down, rosiglitazone=Steady 
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                acarbose=No, rosiglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     25       1    0.960  0.0392        0.886            1
    5     18       2    0.853  0.0792        0.711            1
    8      7       1    0.731  0.1317        0.514            1
   11      4       1    0.549  0.1866        0.282            1

                acarbose=No, rosiglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  43662    1093    0.975 0.000748        0.974        0.976
    2  38368    1617    0.934 0.001230        0.931        0.936
    3  31231    1784    0.881 0.001688        0.877        0.884
    4  23806    1510    0.825 0.002106        0.821        0.829
    5  17620    1127    0.772 0.002489        0.767        0.777
    6  13190     895    0.720 0.002871        0.714        0.725
    7   9739     713    0.667 0.003269        0.660        0.673
    8   7052     585    0.612 0.003713        0.604        0.619
    9   5005     390    0.564 0.004134        0.556        0.572
   10   3591     318    0.514 0.004620        0.505        0.523
   11   2500     177    0.478 0.005038        0.468        0.488
   12   1683     185    0.425 0.005776        0.414        0.437
   13   1009     144    0.364 0.006814        0.351        0.378
   14    479     129    0.266 0.008908        0.249        0.284

                acarbose=No, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2960      64    0.978 0.00267        0.973        0.984
    2   2642      92    0.944 0.00434        0.936        0.953
    3   2171     100    0.901 0.00593        0.889        0.913
    4   1656     126    0.832 0.00803        0.817        0.848
    5   1180      66    0.786 0.00941        0.768        0.804
    6    868      49    0.741 0.01080        0.720        0.763
    7    635      34    0.702 0.01218        0.678        0.726
    8    470      37    0.646 0.01421        0.619        0.675
    9    320      21    0.604 0.01601        0.573        0.636
   10    232      15    0.565 0.01787        0.531        0.601
   11    160      17    0.505 0.02108        0.465        0.548
   12     90       7    0.466 0.02411        0.421        0.515
   13     62       4    0.436 0.02683        0.386        0.492
   14     27       6    0.339 0.04062        0.268        0.429

                acarbose=No, rosiglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     70       1    0.986  0.0142        0.958        1.000
    3     66       2    0.956  0.0249        0.908        1.000
    4     60       3    0.908  0.0358        0.840        0.981
    5     48       1    0.889  0.0398        0.815        0.971
    6     40       2    0.845  0.0486        0.755        0.946
    7     35       4    0.748  0.0626        0.635        0.881
    8     25       1    0.718  0.0669        0.598        0.862
   10     17       3    0.591  0.0863        0.444        0.787
   12      8       1    0.518  0.1024        0.351        0.763
   13      6       1    0.431  0.1161        0.254        0.731

                acarbose=Steady, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    159       5    0.969  0.0138        0.942        0.996
    2    140       2    0.955  0.0167        0.922        0.988
    3    123       8    0.893  0.0264        0.842        0.946
    4     97       2    0.874  0.0289        0.819        0.933
    5     74       1    0.862  0.0308        0.804        0.925
    6     49       1    0.845  0.0348        0.779        0.916
    7     41       1    0.824  0.0396        0.750        0.906
    8     32       1    0.798  0.0460        0.713        0.894
    9     23       1    0.764  0.0556        0.662        0.881

                acarbose=Steady, rosiglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4      7       2    0.714   0.171        0.447            1
    5      4       1    0.536   0.201        0.257            1

                acarbose=Steady, rosiglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                acarbose=Up, rosiglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      5       1      0.8   0.179        0.516            1
    6      4       1      0.6   0.219        0.293            1

> 
