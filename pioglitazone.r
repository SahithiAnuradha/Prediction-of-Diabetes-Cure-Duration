
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
> library(ggfortify)
> library(ggplot2)
> library(ranger)
> dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : PIOGLITAZONE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR PIOGLITAZONE DRUG
> 
> 
>  pioglitazone_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ pioglitazone ,data = dia)
>  pio_test = mean(summary(pioglitazone_km_fit$surv))
>  if(pio_test > 0.5)
+  { 
+       print("FURTHUR TREATMENT IS NOT REQUIRED")
+       print("PATIENT HAS BEEN CURED WITH THE DRUG ALONE")
+  }else{
+       #PIOGLITAZONE WITH INSULIN
+       #KEPLER MEIER FIT
+ 
+       dia1 <- subset(dia , readmitted ==">30"|readmitted == "<30")
+       dia1 <- dia1 %>%
+               mutate(readmitted = ifelse(readmitted == ">30" ,0,1))
+       pio_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + insulin ,data = dia1)
+       #VISUALISATION
+ 
+       
+       pio_with_insulin_test = mean(summary(pio_insulin_fit$surv))
+       if(pio_with_insulin_test > 0.5)
+       {
+              print("PATIENT'S SURVIVABILITY HAS BEEN INCREASED AFTER TREATMENT WITH INSULIN AND FURTHER TREATMENT IS NOT REAQUIRED")
+       }
+       else{
+              print("PATIENT HAS TO BE TREATED WITH COMBINATION ALONG WITH INSULIN")
+              #COMBINATION OF DRUGS ALONG WITH INSULIN
+              #COMBINATION - 1
+              pio_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + rosiglitazone  ,data = dia1)
+            
+              pio_combi_test_1 = mean(summary(pio_combi_fit_R$surv))
+              #COMBINATION - 2
+              pio_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + pioglitazone ,data = dia1)
+             
+              met_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              pio_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + pioglitazone ,data = dia1)
+              
+              pio_combi_test_3 = mean(summary(met_combi_test_fit_met$surv))
+              #COMBINATION - 4
+              pio_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + pioglitazone ,data = dia1)
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

> autoplot( metformin_km_fit,main = "PIOGLITAZONE")
> summary(pioglitazone_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ pioglitazone, 
    data = dia)

                pioglitazone=Down 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    118       5   0.9576  0.0185      0.92196       0.9947
    2    113       7   0.8983  0.0278      0.84539       0.9545
    3    106      14   0.7797  0.0382      0.70835       0.8581
    4     92      15   0.6525  0.0438      0.57204       0.7444
    5     77      22   0.4661  0.0459      0.38425       0.5654
    6     55      10   0.3814  0.0447      0.30306       0.4799
    7     45      11   0.2881  0.0417      0.21699       0.3826
    8     34       7   0.2288  0.0387      0.16430       0.3187
    9     27       9   0.1525  0.0331      0.09970       0.2334
   10     18       6   0.1017  0.0278      0.05949       0.1739
   11     12       4   0.0678  0.0231      0.03473       0.1324
   12      8       3   0.0424  0.0185      0.01797       0.0999
   13      5       2   0.0254  0.0145      0.00832       0.0777
   14      3       3   0.0000     NaN           NA           NA

                pioglitazone=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  94438   13224   0.8600 0.001129      0.85776       0.8622
    2  81214   16056   0.6900 0.001505      0.68701       0.6929
    3  65158   16433   0.5159 0.001626      0.51277       0.5191
    4  48725   12914   0.3792 0.001579      0.37612       0.3823
    5  35811    9231   0.2815 0.001463      0.27860       0.2843
    6  26580    6965   0.2077 0.001320      0.20513       0.2103
    7  19615    5435   0.1502 0.001162      0.14789       0.1524
    8  14180    4085   0.1069 0.001005      0.10494       0.1089
    9  10095    2788   0.0774 0.000869      0.07569       0.0791
   10   7307    2170   0.0544 0.000738      0.05297       0.0559
   11   5137    1714   0.0362 0.000608      0.03507       0.0375
   12   3423    1346   0.0220 0.000477      0.02108       0.0229
   13   2077    1118   0.0102 0.000326      0.00954       0.0108
   14    959     959   0.0000      NaN           NA           NA

                pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6976     974   0.8604 0.00415      0.85228       0.8686
    2   6002    1144   0.6964 0.00551      0.68568       0.7073
    3   4858    1283   0.5125 0.00598      0.50088       0.5243
    4   3575     967   0.3739 0.00579      0.36267       0.3854
    5   2608     691   0.2748 0.00534      0.26452       0.2855
    6   1917     541   0.1972 0.00476      0.18813       0.2068
    7   1376     387   0.1418 0.00418      0.13382       0.1502
    8    989     277   0.1021 0.00362      0.09520       0.1094
    9    712     188   0.0751 0.00316      0.06918       0.0816
   10    524     152   0.0533 0.00269      0.04831       0.0589
   11    372     126   0.0353 0.00221      0.03119       0.0399
   12    246      89   0.0225 0.00178      0.01928       0.0263
   13    157      84   0.0105 0.00122      0.00833       0.0131
   14     73      73   0.0000     NaN           NA           NA

                pioglitazone=Up 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    234       5   0.9786 0.00945       0.9603       0.9973
    2    229      17   0.9060 0.01908       0.8693       0.9442
    3    212      26   0.7949 0.02640       0.7448       0.8483
    4    186      28   0.6752 0.03061       0.6178       0.7380
    5    158      22   0.5812 0.03225       0.5213       0.6480
    6    136      23   0.4829 0.03267       0.4229       0.5514
    7    113      26   0.3718 0.03159       0.3148       0.4392
    8     87      22   0.2778 0.02928       0.2259       0.3415
    9     65      17   0.2051 0.02640       0.1594       0.2640
   10     48      14   0.1453 0.02304       0.1065       0.1983
   11     34      11   0.0983 0.01946       0.0667       0.1449
   12     23      10   0.0556 0.01497       0.0328       0.0942
   13     13       6   0.0299 0.01114       0.0144       0.0621
   14      7       7   0.0000     NaN           NA           NA

> #DRUG + INSULIN
> pio_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + insulin ,data = dia1)
> plot(pio_insulin_fit,main = "PIOGLITAZONE + INSULIN")
> summary(pio_insulin_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    insulin, data = dia1)

                pioglitazone=Down, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3      6       1    0.833   0.152        0.583            1
    4      5       2    0.500   0.204        0.225            1

                pioglitazone=Down, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     22       1    0.955  0.0444        0.871        1.000
    4     21       2    0.864  0.0732        0.732        1.000
    5     19       1    0.818  0.0822        0.672        0.996
    9      6       2    0.545  0.1667        0.300        0.993

                pioglitazone=Down, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5     14       1    0.929  0.0688       0.8030            1
    6     10       1    0.836  0.1077       0.6492            1
   10      6       1    0.696  0.1556       0.4494            1
   14      3       2    0.232  0.1965       0.0442            1

                pioglitazone=Down, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     13       1    0.923  0.0739       0.7890            1
    5     10       1    0.831  0.1100       0.6409            1
    6      7       1    0.712  0.1448       0.4781            1
   11      2       1    0.356  0.2620       0.0842            1

                pioglitazone=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5983     122    0.980 0.00183        0.976        0.983
    2   5445     224    0.939 0.00317        0.933        0.946
    3   4581     251    0.888 0.00435        0.879        0.896
    4   3610     255    0.825 0.00554        0.814        0.836
    5   2759     155    0.779 0.00636        0.766        0.791
    6   2115     133    0.730 0.00724        0.716        0.744
    7   1613     110    0.680 0.00815        0.664        0.696
    8   1192      99    0.624 0.00924        0.606        0.642
    9    846      68    0.573 0.01031        0.554        0.594
   10    611      60    0.517 0.01158        0.495        0.540
   11    424      30    0.481 0.01254        0.457        0.506
   12    290      30    0.431 0.01415        0.404        0.459
   13    178      27    0.365 0.01668        0.334        0.400
   14     88      23    0.270 0.02109        0.232        0.315

                pioglitazone=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  19073     551    0.971 0.00121        0.969        0.973
    2  16295     719    0.928 0.00195        0.924        0.932
    3  12999     763    0.874 0.00265        0.869        0.879
    4   9660     595    0.820 0.00328        0.814        0.826
    5   7007     448    0.768 0.00389        0.760        0.775
    6   5177     343    0.717 0.00450        0.708        0.726
    7   3761     298    0.660 0.00521        0.650        0.670
    8   2687     212    0.608 0.00590        0.596        0.620
    9   1875     145    0.561 0.00661        0.548        0.574
   10   1338     110    0.515 0.00738        0.500        0.529
   11    921      76    0.472 0.00823        0.456        0.489
   12    603      67    0.420 0.00949        0.402        0.439
   13    368      50    0.363 0.01111        0.342        0.385
   14    173      48    0.262 0.01473        0.235        0.293

                pioglitazone=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  12886     313    0.976 0.00136        0.973        0.978
    2  11401     499    0.933 0.00228        0.929        0.937
    3   9201     546    0.878 0.00314        0.872        0.884
    4   6955     497    0.815 0.00398        0.807        0.823
    5   5002     342    0.759 0.00471        0.750        0.768
    6   3683     277    0.702 0.00547        0.691        0.713
    7   2675     194    0.651 0.00617        0.639        0.663
    8   1921     184    0.589 0.00709        0.575        0.603
    9   1341     110    0.541 0.00786        0.525        0.556
   10    940      81    0.494 0.00872        0.477        0.511
   11    663      40    0.464 0.00938        0.446        0.483
   12    438      50    0.411 0.01090        0.390        0.433
   13    252      34    0.356 0.01293        0.331        0.382
   14    114      33    0.253 0.01768        0.220        0.290

                pioglitazone=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5429     112    0.979 0.00193        0.976        0.983
    2   4982     168    0.946 0.00312        0.940        0.952
    3   4271     193    0.904 0.00423        0.895        0.912
    4   3455     181    0.856 0.00528        0.846        0.867
    5   2700     155    0.807 0.00628        0.795        0.819
    6   2085     135    0.755 0.00731        0.741        0.769
    7   1596     106    0.705 0.00829        0.689        0.721
    8   1193      86    0.654 0.00933        0.636        0.672
    9    871      67    0.604 0.01044        0.583        0.624
   10    642      53    0.554 0.01161        0.531        0.577
   11    447      34    0.512 0.01278        0.487        0.537
   12    299      34    0.453 0.01471        0.426        0.483
   13    178      26    0.387 0.01738        0.355        0.423
   14     91      21    0.298 0.02171        0.258        0.344

                pioglitazone=Steady, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    445       7    0.984 0.00590        0.973        0.996
    2    418      10    0.961 0.00934        0.943        0.979
    3    351      14    0.922 0.01346        0.896        0.949
    4    282      11    0.886 0.01674        0.854        0.920
    5    222      12    0.839 0.02078        0.799        0.880
    6    174       7    0.805 0.02353        0.760        0.852
    7    128       7    0.761 0.02750        0.709        0.817
    8     96      10    0.682 0.03420        0.618        0.752
    9     76       6    0.628 0.03790        0.558        0.707
   10     56      10    0.516 0.04474        0.435        0.611
   11     41       1    0.503 0.04538        0.422        0.600
   12     34       1    0.488 0.04639        0.405        0.588
   13     25       5    0.391 0.05388        0.298        0.512
   14      7       1    0.335 0.06930        0.223        0.502

                pioglitazone=Steady, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   1560      32    0.979 0.00359        0.972        0.987
    2   1338      57    0.938 0.00641        0.925        0.950
    3   1044      65    0.879 0.00923        0.861        0.898
    4    766      50    0.822 0.01167        0.799        0.845
    5    534      43    0.756 0.01445        0.728        0.785
    6    381      19    0.718 0.01611        0.687        0.750
    7    272      14    0.681 0.01806        0.647        0.717
    8    192      12    0.639 0.02069        0.599        0.680
    9    139       3    0.625 0.02172        0.584        0.669
   10    102       9    0.570 0.02646        0.520        0.624
   11     67       4    0.536 0.02985        0.480        0.597
   12     48       4    0.491 0.03472        0.427        0.564
   13     35       3    0.449 0.03934        0.378        0.533

                pioglitazone=Steady, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    970      21    0.978 0.00467        0.969        0.988
    2    864      24    0.951 0.00711        0.937        0.965
    3    710      39    0.899 0.01055        0.878        0.920
    4    527      35    0.839 0.01386        0.812        0.867
    5    395      28    0.780 0.01683        0.747        0.813
    6    284      21    0.722 0.01974        0.684        0.762
    7    210      15    0.671 0.02237        0.628        0.716
    8    146      13    0.611 0.02579        0.562        0.663
    9     98       5    0.580 0.02799        0.527        0.637
   10     76       5    0.542 0.03091        0.484        0.606
   11     60       1    0.532 0.03168        0.474        0.598
   12     39       5    0.464 0.03969        0.393        0.549
   13     24       2    0.426 0.04483        0.346        0.523
   14     14       5    0.274 0.06164        0.176        0.425

                pioglitazone=Steady, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    372       4    0.989 0.00535        0.979        1.000
    2    346       8    0.966 0.00955        0.948        0.985
    3    315      19    0.908 0.01577        0.878        0.940
    4    253      13    0.861 0.01956        0.824        0.901
    5    204      10    0.819 0.02270        0.776        0.865
    6    165       8    0.779 0.02558        0.731        0.831
    7    123       4    0.754 0.02771        0.702        0.810
    8     94       7    0.698 0.03278        0.637        0.765
    9     74       3    0.670 0.03529        0.604        0.743
   10     58       6    0.600 0.04145        0.524        0.687
   11     40       6    0.510 0.04889        0.423        0.616
   12     27       2    0.473 0.05207        0.381        0.586
   13     12       1    0.433 0.06082        0.329        0.570

                pioglitazone=Up, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     15       1    0.933  0.0644        0.815        1.000
    6     12       1    0.856  0.0950        0.688        1.000
    7     11       1    0.778  0.1139        0.584        1.000
    9      8       2    0.583  0.1465        0.357        0.954
   14      1       1    0.000     NaN           NA           NA

                pioglitazone=Up, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     47       2    0.957  0.0294        0.901        1.000
    5     33       2    0.899  0.0484        0.809        1.000
    6     27       2    0.833  0.0638        0.717        0.968
    7     20       2    0.750  0.0801        0.608        0.924
    8     15       1    0.700  0.0890        0.545        0.898
   11      5       1    0.560  0.1440        0.338        0.927

                pioglitazone=Up, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     35       2    0.943  0.0392       0.8690        1.000
    4     30       1    0.911  0.0489       0.8204        1.000
    5     23       1    0.872  0.0608       0.7605        0.999
    6     20       1    0.828  0.0717       0.6990        0.981
    7     18       1    0.782  0.0811       0.6383        0.959
    9     10       1    0.704  0.1041       0.5268        0.941
   10      7       1    0.603  0.1290       0.3969        0.917
   13      2       1    0.302  0.2229       0.0709        1.000

                pioglitazone=Up, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     17       1    0.941  0.0571        0.836            1
    8     10       1    0.847  0.1030        0.667            1
   11      4       1    0.635  0.1990        0.344            1
   14      1       1    0.000     NaN           NA           NA

> #DRUG COMBI - 1
> met_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + rosiglitazone  ,data = dia1)
> plot(met_combi_fit_R,main = "DRUG COMBINATION:PIOGLITAZONE + ROSIGLITAZONE")
> summary(met_combi_fit_R)
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

> met_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + pioglitazone ,data = dia1)
> plot(met_combi_fit_P,main = "DRUG COMBINATION:GLYBRIDE + PIOGLITAZONE")
> summary(met_combi_fit_P)
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
> met_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + pioglitazone ,data = dia1)
> plot(met_combi_test_3,main = "DRUG COMBINATION : PIOGLITAZONE + METFORMIN")
> summary(met_combi_test_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ metformin + 
    pioglitazone, data = dia1)

                metformin=Down, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    229       3   0.9869 0.00751       0.9723        1.000
    2    214       6   0.9592 0.01332       0.9335        0.986
    3    188       5   0.9337 0.01717       0.9007        0.968
    4    156       7   0.8918 0.02255       0.8487        0.937
    5    110       9   0.8189 0.03117       0.7600        0.882
    6     88       9   0.7351 0.03851       0.6634        0.815
    7     66       8   0.6460 0.04491       0.5637        0.740
    8     43       6   0.5559 0.05156       0.4635        0.667
   10     20       4   0.4447 0.06460       0.3345        0.591
   11     15       2   0.3854 0.06825       0.2724        0.545
   13      6       1   0.3212 0.08169       0.1951        0.529
   14      4       3   0.0803 0.07247       0.0137        0.471

                metformin=Down, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     28       1    0.964  0.0351        0.898        1.000
    3     22       2    0.877  0.0672        0.754        1.000
    4     20       1    0.833  0.0768        0.695        0.998
    5     15       1    0.777  0.0895        0.620        0.974
   13      3       1    0.518  0.2198        0.226        1.000

                metformin=Down, pioglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=No, pioglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     47       2    0.957  0.0294       0.9014        1.000
    4     44       4    0.870  0.0494       0.7788        0.973
    5     38       2    0.825  0.0564       0.7211        0.943
    6     26       2    0.761  0.0676       0.6396        0.906
    9     15       2    0.660  0.0889       0.5066        0.859
   10     10       1    0.594  0.1015       0.4246        0.830
   11      6       1    0.495  0.1238       0.3030        0.808
   14      3       2    0.165  0.1408       0.0309        0.879

                metformin=No, pioglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  35606     946    0.973 0.000852        0.972        0.975
    2  31176    1336    0.932 0.001383        0.929        0.934
    3  25415    1447    0.879 0.001880        0.875        0.882
    4  19416    1264    0.821 0.002347        0.817        0.826
    5  14371     905    0.770 0.002758        0.764        0.775
    6  10784     745    0.717 0.003182        0.710        0.723
    7   7965     590    0.663 0.003620        0.656        0.671
    8   5801     479    0.609 0.004096        0.601        0.617
    9   4102     322    0.561 0.004559        0.552        0.570
   10   2954     262    0.511 0.005086        0.501        0.521
   11   2055     159    0.472 0.005576        0.461        0.483
   12   1358     159    0.416 0.006416        0.404        0.429
   13    805     110    0.359 0.007490        0.345        0.374
   14    383      96    0.269 0.009741        0.251        0.289

                metformin=No, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2510      54    0.978 0.00290        0.973        0.984
    2   2211      79    0.944 0.00477        0.934        0.953
    3   1800     104    0.889 0.00686        0.876        0.903
    4   1363      88    0.832 0.00873        0.815        0.849
    5    996      71    0.772 0.01057        0.752        0.793
    6    733      36    0.734 0.01179        0.712        0.758
    7    542      33    0.690 0.01340        0.664        0.716
    8    391      27    0.642 0.01529        0.613        0.673
    9    287      13    0.613 0.01659        0.581        0.646
   10    218      21    0.554 0.01936        0.517        0.593
   11    159      10    0.519 0.02104        0.479        0.562
   12    108       9    0.476 0.02372        0.432        0.525
   13     71       6    0.436 0.02680        0.386        0.491
   14     32       3    0.395 0.03307        0.335        0.465

                metformin=No, pioglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     89       3    0.966  0.0191       0.9295        1.000
    3     80       2    0.942  0.0251       0.8941        0.993
    4     73       2    0.916  0.0304       0.8587        0.978
    5     65       3    0.874  0.0375       0.8035        0.951
    6     58       4    0.814  0.0455       0.7294        0.908
    7     49       3    0.764  0.0510       0.6703        0.871
    8     38       2    0.724  0.0557       0.6225        0.841
    9     29       3    0.649  0.0645       0.5339        0.789
   10     21       1    0.618  0.0685       0.4973        0.768
   11     14       2    0.530  0.0824       0.3905        0.718
   13      6       1    0.441  0.1059       0.2759        0.706
   14      3       2    0.147  0.1252       0.0278        0.780

                metformin=Steady, pioglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     11       1    0.909  0.0867        0.754            1
    5      8       1    0.795  0.1306        0.577            1

                metformin=Steady, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   7147     145    0.980 0.00167        0.976        0.983
    2   6366     260    0.940 0.00291        0.934        0.945
    3   5122     289    0.887 0.00409        0.879        0.895
    4   3840     245    0.830 0.00518        0.820        0.840
    5   2774     178    0.777 0.00620        0.765        0.789
    6   2019     126    0.728 0.00716        0.714        0.743
    7   1484     103    0.678 0.00822        0.662        0.694
    8   1062      92    0.619 0.00952        0.601        0.638
    9    740      63    0.566 0.01078        0.546        0.588
   10    509      35    0.527 0.01188        0.505        0.551
   11    355      17    0.502 0.01279        0.478        0.528
   12    243      21    0.459 0.01478        0.431        0.489
   13    152      24    0.386 0.01841        0.352        0.424
   14     74      25    0.256 0.02449        0.212        0.309

                metformin=Steady, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    768       9    0.988 0.00388       0.9807        0.996
    2    689      20    0.960 0.00736       0.9453        0.974
    3    561      29    0.910 0.01137       0.8880        0.933
    4    413      19    0.868 0.01434       0.8405        0.897
    5    317      20    0.813 0.01792       0.7790        0.849
    6    240      15    0.763 0.02106       0.7223        0.805
    7    166       7    0.730 0.02342       0.6859        0.778
    8    116      14    0.642 0.03020       0.5857        0.704
    9     84       4    0.612 0.03240       0.5513        0.679
   10     60       8    0.530 0.03885       0.4592        0.612
   11     39       2    0.503 0.04134       0.4281        0.591
   12     30       3    0.453 0.04629       0.3704        0.553
   13     19       4    0.357 0.05592       0.2629        0.486
   14      5       2    0.214 0.08517       0.0984        0.467

                metformin=Steady, pioglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       7.000        8.000        1.000        0.875        0.117        0.673        1.000 

                metformin=Up, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    389       4    0.990 0.00511        0.980        1.000
    2    367       8    0.968 0.00905        0.951        0.986
    3    327      12    0.933 0.01332        0.907        0.959
    4    268      12    0.891 0.01734        0.858        0.925
    5    213       8    0.857 0.02033        0.818        0.898
    6    169       8    0.817 0.02390        0.771        0.865
    7    130       7    0.773 0.02780        0.720        0.829
    8     87       4    0.737 0.03169        0.678        0.802
    9     63       5    0.679 0.03849        0.607        0.759
   10     48       3    0.636 0.04318        0.557        0.727
   11     30       2    0.594 0.04964        0.504        0.700
   12     20       1    0.564 0.05533        0.466        0.684
   13     13       2    0.477 0.07335        0.353        0.645
   14      5       1    0.382 0.10362        0.224        0.650

                metformin=Up, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     37       2    0.946  0.0372        0.876        1.000
    4     32       1    0.916  0.0463        0.830        1.000
    5     27       1    0.882  0.0556        0.780        0.999
    6     20       4    0.706  0.0906        0.549        0.908
    8     13       1    0.652  0.0986        0.484        0.877
   10      8       1    0.570  0.1151        0.384        0.847
   14      1       1    0.000     NaN           NA           NA

                metformin=Up, pioglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 4
> met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + pioglitazone ,data = dia1)
> plot(met_combi_fit_4,main = "DRUG COMBINATION : GLIPIZIDE + PIOGLITAZONE")
> summary(met_combi_fit_4)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glipizide + 
    pioglitazone, data = dia1)

                glipizide=Down, pioglitazone=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                glipizide=Down, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    270       3    0.989 0.00638        0.976        1.000
    2    252       4    0.973 0.01000        0.954        0.993
    3    227      14    0.913 0.01815        0.878        0.949
    4    185      12    0.854 0.02370        0.809        0.902
    5    137       7    0.810 0.02764        0.758        0.866
    6    112       5    0.774 0.03077        0.716        0.837
    7     94       7    0.716 0.03537        0.650        0.789
    8     75       9    0.631 0.04113        0.555        0.716
    9     53       3    0.595 0.04366        0.515        0.687
   10     43       5    0.526 0.04831        0.439        0.629
   11     32       3    0.476 0.05148        0.385        0.589
   12     25       1    0.457 0.05283        0.365        0.574
   13     18       3    0.381 0.05960        0.280        0.518
   14     11       4    0.243 0.06704        0.141        0.417

                glipizide=Down, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     17       1    0.941  0.0571        0.836            1
    6     10       1    0.847  0.1030        0.667            1
    8      8       1    0.741  0.1339        0.520            1
   10      7       1    0.635  0.1509        0.399            1

                glipizide=Down, pioglitazone=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       6.000        2.000        1.000        0.500        0.354        0.125        1.000 

                glipizide=No, pioglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     47       2    0.957  0.0294       0.9014        1.000
    4     43       5    0.846  0.0536       0.7474        0.958
    5     36       3    0.776  0.0627       0.6620        0.909
    6     22       1    0.740  0.0690       0.6167        0.889
    9     11       2    0.606  0.1030       0.4341        0.845
   14      2       1    0.303  0.2203       0.0728        1.000

                glipizide=No, pioglitazone=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  37839     975    0.974 0.000815        0.973        0.976
    2  33196    1427    0.932 0.001336        0.930        0.935
    3  26955    1526    0.880 0.001819        0.876        0.883
    4  20560    1312    0.823 0.002269        0.819        0.828
    5  15175     961    0.771 0.002677        0.766        0.777
    6  11360     774    0.719 0.003090        0.713        0.725
    7   8378     631    0.665 0.003530        0.658        0.672
    8   6099     504    0.610 0.003997        0.602        0.618
    9   4309     346    0.561 0.004459        0.552        0.570
   10   3077     264    0.513 0.004963        0.503        0.522
   11   2133     150    0.477 0.005417        0.466        0.487
   12   1423     161    0.423 0.006253        0.411        0.435
   13    852     127    0.360 0.007410        0.345        0.374
   14    397     101    0.268 0.009609        0.250        0.288

                glipizide=No, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2736      52    0.981 0.00261        0.976        0.986
    2   2423      83    0.947 0.00442        0.939        0.956
    3   1971     118    0.891 0.00655        0.878        0.904
    4   1485      93    0.835 0.00831        0.819        0.851
    5   1097      77    0.776 0.01006        0.757        0.796
    6    812      46    0.732 0.01139        0.710        0.755
    7    586      30    0.695 0.01270        0.670        0.720
    8    415      32    0.641 0.01483        0.613        0.671
    9    306      16    0.608 0.01626        0.577        0.640
   10    229      28    0.533 0.01941        0.497        0.573
   11    158       9    0.503 0.02078        0.464        0.545
   12    111      10    0.458 0.02333        0.414        0.506
   13     71      10    0.393 0.02755        0.343        0.451
   14     28       4    0.337 0.03513        0.275        0.413

                glipizide=No, pioglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     91       2    0.978  0.0154       0.9484        1.000
    3     84       2    0.955  0.0221       0.9123        0.999
    4     77       2    0.930  0.0276       0.8773        0.986
    5     65       3    0.887  0.0358       0.8196        0.960
    6     59       3    0.842  0.0424       0.7628        0.929
    7     50       2    0.808  0.0469       0.7213        0.906
    8     39       2    0.767  0.0529       0.6699        0.878
    9     30       2    0.716  0.0605       0.6065        0.845
   10     24       1    0.686  0.0649       0.5698        0.826
   11     16       2    0.600  0.0802       0.4618        0.780
   13      7       1    0.514  0.1050       0.3447        0.768
   14      4       2    0.257  0.1389       0.0892        0.741

                glipizide=Steady, pioglitazone=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    6      6       1    0.833   0.152        0.583            1
   10      4       1    0.625   0.213        0.320            1
   11      1       1    0.000     NaN           NA           NA

                glipizide=Steady, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   4925     118    0.976 0.00218        0.972        0.980
    2   4346     173    0.937 0.00357        0.930        0.944
    3   3563     201    0.884 0.00495        0.875        0.894
    4   2666     190    0.821 0.00637        0.809        0.834
    5   1940     127    0.768 0.00753        0.753        0.782
    6   1410     101    0.713 0.00875        0.696        0.730
    7   1026      61    0.670 0.00977        0.651        0.690
    8    705      62    0.611 0.01142        0.589        0.634
    9    485      36    0.566 0.01284        0.541        0.592
   10    343      26    0.523 0.01436        0.496        0.552
   11    242      24    0.471 0.01638        0.440        0.504
   12    154      17    0.419 0.01881        0.384        0.458
   13     89       5    0.396 0.02049        0.357        0.438
   14     48      16    0.264 0.03018        0.211        0.330

                glipizide=Steady, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    547      11    0.980 0.00600        0.968        0.992
    2    483      16    0.947 0.00987        0.928        0.967
    3    392      18    0.904 0.01375        0.877        0.931
    4    294      13    0.864 0.01703        0.831        0.898
    5    216      14    0.808 0.02152        0.767        0.851
    6    161       7    0.773 0.02434        0.727        0.822
    7    120       9    0.715 0.02919        0.660        0.774
    8     91       8    0.652 0.03405        0.589        0.722
    9     62       1    0.642 0.03508        0.576        0.714
   10     45       1    0.627 0.03709        0.559        0.704
   11     35       3    0.573 0.04506        0.492        0.669
   12     25       2    0.528 0.05184        0.435        0.640
   13     16       1    0.495 0.05815        0.393        0.623

                glipizide=Steady, pioglitazone=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     20       1    0.950  0.0487        0.859            1
    7      8       2    0.712  0.1500        0.472            1
    9      4       1    0.534  0.1909        0.265            1

                glipizide=Up, pioglitazone=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
          14            1            1            0          NaN           NA           NA 

                glipizide=Up, pioglitazone=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    337       2    0.994 0.00418        0.986        1.000
    2    329       6    0.976 0.00841        0.960        0.993
    3    307      12    0.938 0.01348        0.912        0.965
    4    269      14    0.889 0.01802        0.854        0.925
    5    216       5    0.868 0.01981        0.830        0.908
    6    178       8    0.829 0.02324        0.785        0.876
    7    147       9    0.779 0.02729        0.727        0.834
    8    114       6    0.738 0.03055        0.680        0.800
    9     86       5    0.695 0.03427        0.631        0.765
   10     68       9    0.603 0.04122        0.527        0.689
   11     48       3    0.565 0.04401        0.485        0.658
   12     28       2    0.525 0.04926        0.437        0.631
   13     17       2    0.463 0.05976        0.360        0.596
   14     10       4    0.278 0.08019        0.158        0.489

                glipizide=Up, pioglitazone=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     42       1    0.976  0.0235        0.931        1.000
    3     36       1    0.949  0.0352        0.883        1.000
    4     32       2    0.890  0.0523        0.793        0.998
    5     28       2    0.826  0.0651        0.708        0.964
    6     21       1    0.787  0.0729        0.656        0.944
    7     18       1    0.743  0.0809        0.600        0.920
    8     14       1    0.690  0.0909        0.533        0.893
   14      2       2    0.000     NaN           NA           NA

                glipizide=Up, pioglitazone=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI


> q()
> 
