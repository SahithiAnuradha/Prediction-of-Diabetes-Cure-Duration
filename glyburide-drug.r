"
> dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : GLYBURIDE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR GLYBURIDE DRUG
> 
> 
>  glyburide_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ glyburide,data = dia)
>  gly_test = mean(summary(glyburide_km_fit$surv))
>  if(gly_test > 0.5)
+  { 
+       print("FURTHUR TREATMENT IS NOT REQUIRED")
+       print("PATIENT HAS BEEN CURED WITH THE DRUG ALONE")
+  }else{
+       #GLYBURIDE WITH INSULIN
+       #KEPLER MEIER FIT
+ 
+       dia1 <- subset(dia , readmitted ==">30"|readmitted == "<30")
+       dia1 <- dia1 %>%
+               mutate(readmitted = ifelse(readmitted == ">30" ,0,1))
+       gly_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + insulin ,data = dia1)
+       #VISUALISATION
+ 
+       
+       gly_with_insulin_test = mean(summary(gly_insulin_fit$surv))
+       if(gly_with_insulin_test > 0.5)
+       {
+              print("PATIENT'S SURVIVABILITY HAS BEEN INCREASED AFTER TREATMENT WITH INSULIN AND FURTHER TREATMENT IS NOT REAQUIRED")
+       }
+       else{
+              print("PATIENT HAS TO BE TREATED WITH COMBINATION ALONG WITH INSULIN")
+              #COMBINATION OF DRUGS ALONG WITH INSULIN
+              #COMBINATION - 1
+              met_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + rosiglitazone  ,data = dia1)
+            
+              met_combi_test_1 = mean(summary(met_combi_fit$surv))
+              #COMBINATION - 2
+              met_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + pioglitazone ,data = dia1)
+             
+              met_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              met_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + glyburide ,data = dia1)
+              
+              met_combi_test_3 = mean(summary(met_combi_test_fit_gly$surv))
+              #COMBINATION - 4
+              met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + glyburide ,data = dia1)
+             
+              met_combi_test_4 = mean(summary(met_combi_test_fit_gli$surv))
+              
+              if(met_combi_test_1 > 0.5)
+              { 
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_1)
+                          print("COMBINATION OF GLYBURIDE~ROSAGLITAZONE WITH INSULIN HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF GLYBURIDE ~ PIOGLITAZONE WITH INSULIN HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF METFORMIN ~ GLYBURIDE WITH INSULIN HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF GLYBURIDE ~ GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
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
> glyburide_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ glyburide)
Error in Surv(time_in_hospital, diabetesMed) : 
  object 'time_in_hospital' not found
> glyburide_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ glyburide,data = dia)
> summary(glyburide_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ glyburide, 
    data = dia)

                glyburide=Down 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    564      33   0.9415 0.00988      0.92232       0.9611
    2    531      57   0.8404 0.01542      0.81074       0.8712
    3    474      76   0.7057 0.01919      0.66905       0.7443
    4    398      71   0.5798 0.02078      0.54045       0.6220
    5    327      63   0.4681 0.02101      0.42866       0.5111
    6    264      66   0.3511 0.02010      0.31380       0.3928
    7    198      45   0.2713 0.01872      0.23696       0.3106
    8    153      26   0.2252 0.01759      0.19321       0.2624
    9    127      37   0.1596 0.01542      0.13204       0.1928
   10     90      20   0.1241 0.01388      0.09968       0.1545
   11     70      15   0.0975 0.01249      0.07587       0.1253
   12     55      20   0.0621 0.01016      0.04502       0.0855
   13     35      27   0.0142 0.00498      0.00713       0.0282
   14      8       8   0.0000     NaN           NA           NA

                glyburide=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  91116   12933  0.85806 0.001156      0.85580       0.8603
    2  78183   15570  0.68718 0.001536      0.68418       0.6902
    3  62613   15918  0.51248 0.001656      0.50924       0.5157
    4  46695   12414  0.37623 0.001605      0.37310       0.3794
    5  34281    8866  0.27893 0.001486      0.27603       0.2819
    6  25415    6673  0.20569 0.001339      0.20309       0.2083
    7  18742    5196  0.14867 0.001179      0.14638       0.1510
    8  13546    3910  0.10576 0.001019      0.10378       0.1078
    9   9636    2662  0.07654 0.000881      0.07483       0.0783
   10   6974    2090  0.05360 0.000746      0.05216       0.0551
   11   4884    1653  0.03546 0.000613      0.03428       0.0367
   12   3231    1272  0.02150 0.000481      0.02058       0.0225
   13   1959    1049  0.00999 0.000329      0.00936       0.0107
   14    910     910  0.00000      NaN           NA           NA

                glyburide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   9274    1203   0.8703 0.00349      0.86347       0.8771
    2   8071    1518   0.7066 0.00473      0.69739       0.7159
    3   6553    1649   0.5288 0.00518      0.51873       0.5390
    4   4904    1326   0.3858 0.00505      0.37603       0.3958
    5   3578     944   0.2840 0.00468      0.27499       0.2933
    6   2634     719   0.2065 0.00420      0.19842       0.2149
    7   1915     554   0.1468 0.00367      0.13973       0.1541
    8   1361     390   0.1047 0.00318      0.09865       0.1111
    9    971     262   0.0765 0.00276      0.07123       0.0821
   10    709     196   0.0553 0.00237      0.05085       0.0602
   11    513     158   0.0383 0.00199      0.03457       0.0424
   12    355     130   0.0243 0.00160      0.02132       0.0276
   13    225     114   0.0120 0.00113      0.00995       0.0144
   14    111     111   0.0000     NaN           NA           NA

                glyburide=Up 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    812      39   0.9520 0.00750      0.93738       0.9668
    2    773      79   0.8547 0.01237      0.83078       0.8793
    3    694     113   0.7155 0.01583      0.68515       0.7472
    4    581     113   0.5764 0.01734      0.54335       0.6114
    5    468      93   0.4618 0.01750      0.42877       0.4974
    6    375      81   0.3621 0.01687      0.33048       0.3967
    7    294      64   0.2833 0.01581      0.25390       0.3160
    8    230      65   0.2032 0.01412      0.17733       0.2329
    9    165      41   0.1527 0.01262      0.12987       0.1796
   10    124      36   0.1084 0.01091      0.08897       0.1320
   11     88      29   0.0727 0.00911      0.05683       0.0929
   12     59      26   0.0406 0.00693      0.02910       0.0568
   13     33      20   0.0160 0.00440      0.00934       0.0275
   14     13      13   0.0000     NaN           NA           NA

> autoplot(glyburide_km_fit)
> autoplot(glyburide_km_fit,main = "GLYBURIDE")
> glyburide_km_fit <- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + insulin,data = dia1)
> summary(glyburide_km_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glyburide + 
    insulin, data = dia1)

                glyburide=Down, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     16       1    0.938  0.0605        0.826            1
    5     14       1    0.871  0.0856        0.718            1
    6     11       1    0.791  0.1084        0.605            1
   11      7       1    0.678  0.1399        0.453            1

                glyburide=Down, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    166       2    0.988 0.00847        0.971        1.000
    2    151       1    0.981 0.01064        0.961        1.000
    3    131       4    0.951 0.01800        0.917        0.987
    4    107       4    0.916 0.02459        0.869        0.965
    5     88       3    0.885 0.02963        0.828        0.945
    6     69       4    0.833 0.03740        0.763        0.910
    7     50       5    0.750 0.04882        0.660        0.852
    9     31       2    0.702 0.05640        0.599        0.821
   10     20       2    0.631 0.06922        0.509        0.783
   12     10       1    0.568 0.08643        0.422        0.766
   13      8       1    0.497 0.10067        0.334        0.739
   14      1       1    0.000     NaN           NA           NA

                glyburide=Down, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     68       3    0.956  0.0249        0.908        1.000
    3     63       1    0.941  0.0288        0.886        0.999
    4     57       2    0.908  0.0360        0.840        0.981
    6     40       4    0.817  0.0539        0.718        0.930
    8     24       4    0.681  0.0767        0.546        0.849

                glyburide=Down, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     16       1    0.938  0.0605        0.826            1
    5     13       1    0.865  0.0890        0.707            1
    9      6       1    0.721  0.1511        0.478            1
   14      1       1    0.000     NaN           NA           NA

                glyburide=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6122     126    0.979 0.00181        0.976        0.983
    2   5580     228    0.939 0.00313        0.933        0.946
    3   4688     256    0.888 0.00429        0.880        0.897
    4   3689     256    0.826 0.00546        0.816        0.837
    5   2825     164    0.778 0.00630        0.766        0.791
    6   2161     139    0.728 0.00718        0.714        0.743
    7   1646     109    0.680 0.00806        0.665        0.696
    8   1216     108    0.620 0.00920        0.602        0.638
    9    865      71    0.569 0.01024        0.549        0.589
   10    621      64    0.510 0.01151        0.488        0.533
   11    435      28    0.477 0.01233        0.454        0.502
   12    300      30    0.430 0.01384        0.403        0.458
   13    184      29    0.362 0.01641        0.331        0.396
   14     83      23    0.262 0.02137        0.223        0.307

                glyburide=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  17858     501    0.972 0.00124        0.970        0.974
    2  15225     684    0.928 0.00201        0.924        0.932
    3  12082     735    0.872 0.00277        0.866        0.877
    4   8937     555    0.818 0.00342        0.811        0.824
    5   6447     417    0.765 0.00406        0.757        0.773
    6   4746     313    0.714 0.00469        0.705        0.724
    7   3453     271    0.658 0.00542        0.648        0.669
    8   2470     190    0.608 0.00612        0.596        0.620
    9   1731     129    0.562 0.00684        0.549        0.576
   10   1222      96    0.518 0.00765        0.503        0.533
   11    837      72    0.474 0.00861        0.457        0.491
   12    542      56    0.425 0.00989        0.406        0.445
   13    333      47    0.365 0.01174        0.342        0.388
   14    157      40    0.272 0.01541        0.243        0.304

                glyburide=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  12574     309    0.975 0.00138        0.973        0.978
    2  11099     473    0.934 0.00229        0.929        0.938
    3   8945     532    0.878 0.00318        0.872        0.885
    4   6721     481    0.815 0.00404        0.808        0.823
    5   4841     330    0.760 0.00479        0.751        0.769
    6   3558     265    0.703 0.00555        0.692        0.714
    7   2590     192    0.651 0.00629        0.639        0.664
    8   1856     178    0.589 0.00722        0.575        0.603
    9   1292     107    0.540 0.00801        0.524        0.556
   10    917      82    0.492 0.00890        0.475        0.509
   11    641      34    0.466 0.00948        0.447        0.485
   12    427      49    0.412 0.01105        0.391        0.434
   13    253      33    0.358 0.01298        0.334        0.385
   14    119      37    0.247 0.01764        0.215        0.284

                glyburide=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5498     111    0.980 0.00190        0.976        0.984
    2   5045     170    0.947 0.00309        0.941        0.953
    3   4330     204    0.902 0.00424        0.894        0.911
    4   3492     183    0.855 0.00526        0.845        0.865
    5   2743     157    0.806 0.00625        0.794        0.818
    6   2118     136    0.754 0.00725        0.740        0.769
    7   1624     102    0.707 0.00817        0.691        0.723
    8   1215      92    0.653 0.00927        0.635        0.672
    9    885      64    0.606 0.01031        0.586        0.627
   10    659      53    0.557 0.01145        0.535        0.580
   11    456      36    0.513 0.01268        0.489        0.539
   12    304      34    0.456 0.01459        0.428        0.485
   13    179      26    0.390 0.01731        0.357        0.425
   14     88      18    0.310 0.02169        0.270        0.356

                glyburide=Steady, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    275       3    0.989 0.00626        0.977        1.000
    2    255       6    0.966 0.01121        0.944        0.988
    3    217       8    0.930 0.01640        0.899        0.963
    4    183      11    0.874 0.02247        0.831        0.919
    5    136       2    0.861 0.02391        0.816        0.910
    6    112       1    0.854 0.02490        0.806        0.904
    7     82       7    0.781 0.03482        0.716        0.852
    8     59       1    0.768 0.03666        0.699        0.843
    9     45       4    0.699 0.04665        0.614        0.797
   10     35       4    0.619 0.05587        0.519        0.739
   11     23       1    0.593 0.05958        0.487        0.722
   12     19       1    0.561 0.06409        0.449        0.702
   13     15       3    0.449 0.07740        0.320        0.630
   14     11       2    0.367 0.08208        0.237        0.569

                glyburide=Steady, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2491      76    0.969 0.00345        0.963        0.976
    2   2150      89    0.929 0.00532        0.919        0.940
    3   1743      87    0.883 0.00700        0.869        0.897
    4   1319      81    0.829 0.00879        0.812        0.846
    5    965      69    0.769 0.01067        0.749        0.791
    6    705      42    0.724 0.01215        0.700        0.748
    7    499      36    0.671 0.01405        0.644        0.700
    8    347      33    0.608 0.01654        0.576        0.641
    9    232      18    0.560 0.01862        0.525        0.598
   10    176      17    0.506 0.02094        0.467        0.549
   11    124       8    0.474 0.02255        0.431        0.520
   12     88      13    0.404 0.02628        0.355        0.459
   13     55       4    0.374 0.02817        0.323        0.434
   14     27       7    0.277 0.03784        0.212        0.362

                glyburide=Steady, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   1159      25    0.978 0.00427        0.970        0.987
    2   1048      46    0.935 0.00741        0.921        0.950
    3    857      52    0.879 0.01033        0.859        0.899
    4    664      45    0.819 0.01289        0.794        0.845
    5    478      36    0.757 0.01549        0.728        0.788
    6    347      27    0.699 0.01796        0.664        0.735
    7    255      15    0.657 0.01979        0.620        0.697
    8    178      11    0.617 0.02204        0.575        0.662
    9    131       8    0.579 0.02439        0.533        0.629
   10     93       6    0.542 0.02717        0.491        0.598
   11     74       6    0.498 0.03031        0.442        0.561
   12     45       5    0.443 0.03564        0.378        0.518
   13     22       4    0.362 0.04663        0.281        0.466
   14     11       3    0.263 0.05928        0.169        0.409

                glyburide=Steady, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    291       5    0.983 0.00762        0.968        0.998
    2    273       7    0.958 0.01198        0.934        0.981
    3    245       7    0.930 0.01547        0.900        0.961
    4    208      10    0.886 0.02018        0.847        0.926
    5    157       7    0.846 0.02418        0.800        0.895
    6    128       7    0.800 0.02849        0.746        0.858
    7     93       7    0.740 0.03424        0.675        0.810
    9     57       5    0.675 0.04176        0.598        0.762
   10     42       6    0.578 0.05107        0.486        0.688
   11     31       5    0.485 0.05740        0.385        0.612
   12     23       2    0.443 0.05965        0.340        0.577
   13     11       1    0.403 0.06644        0.291        0.556
   14      8       3    0.252 0.08046        0.134        0.471

                glyburide=Up, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     30       2    0.933  0.0455        0.848        1.000
    4     24       1    0.894  0.0579        0.788        1.000
    7     16       2    0.783  0.0896        0.625        0.980
    9     12       1    0.717  0.1032        0.541        0.951
   10      9       2    0.558  0.1278        0.356        0.874
   11      5       1    0.446  0.1429        0.238        0.836

                glyburide=Up, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    190       4    0.979  0.0104        0.959        1.000
    2    177       4    0.957  0.0149        0.928        0.987
    3    150       3    0.938  0.0183        0.903        0.974
    4    121       7    0.883  0.0263        0.833        0.937
    5     93       5    0.836  0.0324        0.775        0.902
    6     78       5    0.782  0.0381        0.711        0.861
    7     62       2    0.757  0.0409        0.681        0.842
    8     50       2    0.727  0.0445        0.645        0.819
    9     37       1    0.707  0.0474        0.620        0.807
   10     31       4    0.616  0.0593        0.510        0.744
   11     20       1    0.585  0.0639        0.472        0.725
   12     14       1    0.543  0.0717        0.420        0.704
   13     10       1    0.489  0.0826        0.351        0.681

                glyburide=Up, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2    107       1    0.991  0.0093        0.973        1.000
    3     98       2    0.970  0.0168        0.938        1.000
    4     86       5    0.914  0.0292        0.859        0.973
    5     70       6    0.836  0.0406        0.760        0.919
    6     52       4    0.771  0.0485        0.682        0.873
    7     38       3    0.710  0.0560        0.609        0.829
    8     27       4    0.605  0.0681        0.485        0.755
    9     16       1    0.567  0.0736        0.440        0.732
   11      6       1    0.473  0.1059        0.305        0.733
   12      4       1    0.355  0.1296        0.173        0.726

                glyburide=Up, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     23       1    0.957  0.0425        0.877        1.000
    4     19       1    0.906  0.0634        0.790        1.000
    5     15       1    0.846  0.0831        0.698        1.000
    6     14       1    0.785  0.0967        0.617        1.000
    7     11       1    0.714  0.1112        0.526        0.969
    8     10       2    0.571  0.1268        0.370        0.882
   11      3       1    0.381  0.1769        0.153        0.947
>#COMBINATION - 1
> plot(glyburide_km_fit,main = "GLYBURIDE + INSULIN")
> gly_combi_test_1<- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + pioglitazone,data = dia1)
> summary(gly_combi_test_1)
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

> plot(gly_combi_test_1,main = "GLYBURIDE + PIOGLITAZONE")
>#COMBINATION-2
> gly_combi_test_2<- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + rosiglitazone,data = dia1)
> summary(gly_combi_test_2)
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

> plot(gly_combi_test_2,main = "ROSIGLITAZONE + GLYBURIDE")
>#COMBINATION-3
> gly_combi_test_3<- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + glipizide,data = dia1)
> summary(gly_combi_test_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glyburide + 
    glipizide, data = dia1)

                glyburide=Down, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    272       2    0.993 0.00518        0.983        1.000
    2    253       4    0.977 0.00931        0.959        0.995
    3    227       5    0.955 0.01317        0.930        0.982
    4    193       7    0.921 0.01807        0.886        0.957
    5    158       5    0.892 0.02169        0.850        0.935
    6    128       9    0.829 0.02851        0.775        0.887
    7     92       5    0.784 0.03333        0.721        0.852
    8     73       4    0.741 0.03779        0.670        0.819
    9     61       3    0.705 0.04138        0.628        0.790
   10     43       2    0.672 0.04548        0.588        0.767
   11     33       1    0.651 0.04844        0.563        0.754
   12     24       1    0.624 0.05349        0.528        0.738
   13     16       1    0.585 0.06278        0.474        0.722
   14      2       2    0.000     NaN           NA           NA

                glyburide=Down, glipizide=Steady 
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       4.000        3.000        1.000        0.667        0.272        0.300        1.000 

                glyburide=No, glipizide=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    293       3    0.990 0.00588        0.978        1.000
    2    274       4    0.975 0.00922        0.957        0.994
    3    248      14    0.920 0.01673        0.888        0.954
    4    201      13    0.861 0.02236        0.818        0.906
    5    150       7    0.821 0.02596        0.771        0.873
    6    121       7    0.773 0.03003        0.716        0.834
    7    102       7    0.720 0.03401        0.656        0.790
    8     82      10    0.632 0.03961        0.559        0.715
    9     60       3    0.601 0.04162        0.524        0.688
   10     50       6    0.529 0.04586        0.446        0.627
   11     38       3    0.487 0.04815        0.401        0.591
   12     29       1    0.470 0.04933        0.383        0.577
   13     21       3    0.403 0.05546        0.308        0.528
   14     14       4    0.288 0.06273        0.188        0.441

                glyburide=No, glipizide=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  35966     912    0.975 0.000829        0.973        0.976
    2  31533    1355    0.933 0.001367        0.930        0.935
    3  25553    1484    0.879 0.001876        0.875        0.882
    4  19426    1246    0.822 0.002338        0.818        0.827
    5  14349     916    0.770 0.002758        0.764        0.775
    6  10732     729    0.717 0.003179        0.711        0.724
    7   7934     587    0.664 0.003621        0.657        0.672
    8   5771     484    0.609 0.004109        0.601        0.617
    9   4081     325    0.560 0.004577        0.551        0.569
   10   2916     254    0.511 0.005101        0.501        0.521
   11   2008     141    0.475 0.005567        0.465        0.487
   12   1335     147    0.423 0.006414        0.411        0.436
   13    806     124    0.358 0.007640        0.343        0.373
   14    370      93    0.268 0.009895        0.249        0.288

                glyburide=No, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5423     129    0.976 0.00207        0.972        0.980
    2   4783     190    0.937 0.00340        0.931        0.944
    3   3911     217    0.885 0.00470        0.876        0.895
    4   2920     200    0.825 0.00602        0.813        0.837
    5   2122     138    0.771 0.00716        0.757        0.785
    6   1539     108    0.717 0.00834        0.701        0.734
    7   1120      71    0.672 0.00939        0.653        0.690
    8    780      68    0.613 0.01093        0.592        0.635
    9    537      38    0.570 0.01222        0.546        0.594
   10    377      27    0.529 0.01363        0.503        0.556
   11    268      24    0.481 0.01546        0.452        0.513
   12    174      19    0.429 0.01787        0.395        0.465
   13    100       6    0.403 0.01965        0.366        0.444
   14     51      15    0.285 0.02922        0.233        0.348

                glyburide=No, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    370       3    0.992 0.00466        0.983        1.000
    2    359       6    0.975 0.00813        0.960        0.991
    3    333      12    0.940 0.01267        0.916        0.965
    4    292      16    0.889 0.01733        0.855        0.923
    5    235       7    0.862 0.01949        0.825        0.901
    6    191       9    0.822 0.02279        0.778        0.867
    7    157       9    0.774 0.02634        0.725        0.828
    8    124       6    0.737 0.02918        0.682        0.796
    9     95       5    0.698 0.03239        0.638        0.765
   10     76       8    0.625 0.03800        0.554        0.704
   11     55       2    0.602 0.03987        0.529        0.685
   12     35       2    0.568 0.04439        0.487        0.662
   13     22       2    0.516 0.05328        0.421        0.632
   14     12       6    0.258 0.07910        0.141        0.471

                glyburide=Steady, glipizide=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                glyburide=Steady, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   4132     109    0.974 0.00249        0.969        0.979
    2   3645     148    0.934 0.00398        0.926        0.942
    3   2985     151    0.887 0.00532        0.876        0.897
    4   2305     146    0.831 0.00672        0.818        0.844
    5   1675     111    0.776 0.00805        0.760        0.792
    6   1239      76    0.728 0.00922        0.710        0.746
    7    886      63    0.676 0.01063        0.656        0.697
    8    628      43    0.630 0.01202        0.607        0.654
    9    444      35    0.580 0.01369        0.554        0.608
   10    327      31    0.525 0.01556        0.496        0.557
   11    238      16    0.490 0.01683        0.458        0.524
   12    166      21    0.428 0.01939        0.392        0.468
   13     95      12    0.374 0.02235        0.333        0.420
   14     52      13    0.280 0.02802        0.231        0.341

                glyburide=Steady, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     61       2    0.967  0.0228        0.924        1.000
    4     54       1    0.949  0.0286        0.895        1.000
    5     46       3    0.887  0.0437        0.806        0.977
    6     39       1    0.865  0.0481        0.775        0.964
    7     32       1    0.838  0.0537        0.739        0.950
    8     20       2    0.754  0.0741        0.622        0.914
   10     14       1    0.700  0.0862        0.550        0.891
   11     10       3    0.490  0.1180        0.306        0.786
   14      4       1    0.368  0.1382        0.176        0.768

                glyburide=Steady, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     13       1    0.923  0.0739        0.789            1
    7     10       1    0.831  0.1100        0.641            1
   10      5       1    0.665  0.1727        0.399            1
   11      4       1    0.498  0.1936        0.233            1
   14      1       1    0.000     NaN           NA           NA

                glyburide=Up, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    350       4    0.989 0.00568        0.977        1.000
    2    329       5    0.974 0.00870        0.957        0.991
    3    292       8    0.947 0.01258        0.923        0.972
    4    241      13    0.896 0.01820        0.861        0.932
    5    191      12    0.840 0.02320        0.795        0.886
    6    154      10    0.785 0.02736        0.733        0.841
    7    120       8    0.733 0.03117        0.674        0.796
    8     95       7    0.679 0.03492        0.614        0.751
    9     70       3    0.650 0.03724        0.581        0.727
   10     50       6    0.572 0.04433        0.491        0.665
   11     32       3    0.518 0.04982        0.429        0.626
   12     21       2    0.469 0.05597        0.371        0.592
   13     15       1    0.437 0.06034        0.334        0.573

                glyburide=Up, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4      7       1    0.857   0.132        0.633            1
   11      1       1    0.000     NaN           NA           NA

                glyburide=Up, glipizide=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       8.000        2.000        1.000        0.500        0.354        0.125        1.000 


> plot(gly_combi_test_3,main= "glyburide + glipizide")
> #COMBINATION - 4
> gly_combi_test_4<- survfit(Surv(time_in_hospital,readmitted) ~ glyburide + metformin,data = dia1)
> summary(gly_combi_test_4)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glyburide + 
    metformin, data = dia1)

                glyburide=Down, metformin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    6     10       2     0.80   0.126        0.587            1
    7      8       1     0.70   0.145        0.467            1
    8      5       1     0.56   0.171        0.308            1

                glyburide=Down, metformin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    180       2    0.989 0.00781        0.974        1.000
    2    166       2    0.977 0.01139        0.955        1.000
    3    152       3    0.958 0.01569        0.927        0.989
    4    127       6    0.912 0.02342        0.868        0.960
    5    102       4    0.877 0.02853        0.822        0.934
    6     78       7    0.798 0.03846        0.726        0.877
    7     53       1    0.783 0.04058        0.707        0.867
    8     44       2    0.747 0.04588        0.663        0.843
    9     38       3    0.688 0.05342        0.591        0.801
   10     28       2    0.639 0.05986        0.532        0.768
   11     20       1    0.607 0.06484        0.493        0.749
   12     14       1    0.564 0.07329        0.437        0.727
   13      9       1    0.501 0.08794        0.355        0.707
   14      1       1    0.000     NaN           NA           NA

                glyburide=Down, metformin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     67       2    0.970  0.0208        0.930        1.000
    3     58       1    0.953  0.0263        0.903        1.000
    4     52       1    0.935  0.0316        0.875        0.999
    5     44       1    0.914  0.0373        0.844        0.990
    7     30       3    0.822  0.0603        0.712        0.949
    8     23       1    0.787  0.0674        0.665        0.931
   14      1       1    0.000     NaN           NA           NA

                glyburide=Down, metformin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3      8       1    0.875   0.117        0.673            1
    4      5       1    0.700   0.182        0.420            1

                glyburide=No, metformin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    204       4    0.980 0.00971       0.9615        1.000
    2    191       6    0.950 0.01554       0.9196        0.981
    3    166       6    0.915 0.02034       0.8763        0.956
    4    137       7    0.869 0.02586       0.8193        0.921
    5     91       7    0.802 0.03404       0.7377        0.871
    6     71       7    0.723 0.04178       0.6452        0.809
    7     54       6    0.642 0.04832       0.5543        0.744
    8     36       5    0.553 0.05570       0.4541        0.674
   10     18       2    0.492 0.06426       0.3806        0.635
   11     16       2    0.430 0.06939       0.3136        0.590
   13      7       1    0.369 0.08231       0.2381        0.571
   14      5       3    0.148 0.08724       0.0463        0.470

                glyburide=No, metformin=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  34989     914    0.974 0.000853        0.972        0.976
    2  30628    1314    0.932 0.001392        0.929        0.935
    3  24915    1442    0.878 0.001903        0.874        0.882
    4  19008    1246    0.821 0.002376        0.816        0.825
    5  14063     890    0.769 0.002792        0.763        0.774
    6  10545     716    0.716 0.003212        0.710        0.723
    7   7812     577    0.664 0.003653        0.656        0.671
    8   5689     473    0.608 0.004137        0.600        0.617
    9   4031     312    0.561 0.004596        0.552        0.570
   10   2908     254    0.512 0.005122        0.502        0.522
   11   2020     153    0.473 0.005613        0.463        0.485
   12   1335     151    0.420 0.006452        0.407        0.433
   13    798     108    0.363 0.007548        0.349        0.378
   14    376      93    0.273 0.009877        0.255        0.293

                glyburide=No, metformin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6526     125    0.981 0.00170        0.978        0.984
    2   5813     231    0.942 0.00299        0.936        0.948
    3   4678     270    0.888 0.00428        0.879        0.896
    4   3456     215    0.832 0.00542        0.822        0.843
    5   2503     162    0.778 0.00651        0.766        0.791
    6   1810     119    0.727 0.00759        0.713        0.742
    7   1330      84    0.681 0.00861        0.665        0.698
    8    955      85    0.621 0.01004        0.601        0.641
    9    662      55    0.569 0.01136        0.547        0.592
   10    449      36    0.523 0.01275        0.499        0.549
   11    306      13    0.501 0.01362        0.475        0.529
   12    207      17    0.460 0.01574        0.430        0.492
   13    131      24    0.376 0.02018        0.338        0.417
   14     63      20    0.256 0.02599        0.210        0.313

                glyburide=No, metformin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    333       4    0.988 0.00597       0.9764        1.000
    2    317       4    0.976 0.00855       0.9589        0.992
    3    286       9    0.945 0.01304       0.9196        0.971
    4    238       7    0.917 0.01635       0.8855        0.950
    5    199       9    0.876 0.02064       0.8360        0.917
    6    157      11    0.814 0.02620       0.7644        0.867
    7    117       7    0.766 0.03042       0.7081        0.828
    8     77       5    0.716 0.03566       0.6492        0.789
    9     56       4    0.665 0.04127       0.5885        0.751
   10     44       3    0.619 0.04601       0.5354        0.716
   11     27       2    0.573 0.05281       0.4788        0.687
   12     20       1    0.545 0.05743       0.4431        0.670
   13     13       2    0.461 0.07303       0.3379        0.629
   14      3       2    0.154 0.12780       0.0301        0.784

                glyburide=Steady, metformin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     26       1    0.962  0.0377       0.8904        1.000
    4     23       1    0.920  0.0545       0.8188        1.000
    5     19       2    0.823  0.0811       0.6784        0.998
   10      4       1    0.617  0.1883       0.3395        1.000
   13      2       1    0.309  0.2376       0.0682        1.000

                glyburide=Steady, metformin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2844      80    0.972 0.00310        0.966        0.978
    2   2503      98    0.934 0.00480        0.924        0.943
    3   2072     104    0.887 0.00639        0.875        0.900
    4   1597      97    0.833 0.00801        0.818        0.849
    5   1174      80    0.776 0.00966        0.758        0.795
    6    871      55    0.727 0.01108        0.706        0.749
    7    633      44    0.677 0.01266        0.652        0.702
    8    450      26    0.638 0.01406        0.611        0.666
    9    318      24    0.590 0.01607        0.559        0.622
   10    233      25    0.526 0.01867        0.491        0.564
   11    169      14    0.483 0.02044        0.444        0.524
   12    114      15    0.419 0.02342        0.376        0.468
   13     66       7    0.375 0.02628        0.327        0.430
   14     38       9    0.286 0.03271        0.229        0.358

                glyburide=Steady, metformin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   1275      29    0.977 0.00418        0.969        0.985
    2   1129      47    0.937 0.00705        0.923        0.951
    3    905      46    0.889 0.00957        0.870        0.908
    4    707      46    0.831 0.01217        0.808        0.855
    5    511      32    0.779 0.01447        0.751        0.808
    6    383      21    0.736 0.01641        0.705        0.769
    7    264      21    0.678 0.01945        0.641        0.717
    8    180      19    0.606 0.02332        0.562        0.654
    9    130      10    0.560 0.02577        0.511        0.612
   10     99       7    0.520 0.02795        0.468        0.578
   11     73       6    0.477 0.03062        0.421        0.541
   12     55       6    0.425 0.03386        0.364        0.497
   13     32       4    0.372 0.03868        0.303        0.456
   14     15       6    0.223 0.05248        0.141        0.354

                glyburide=Steady, metformin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     66       3    0.955  0.0256        0.906        1.000
    3     59       3    0.906  0.0366        0.837        0.981
    4     47       3    0.848  0.0471        0.761        0.946
    6     24       1    0.813  0.0569        0.709        0.932
    9     13       1    0.750  0.0798        0.609        0.924

                glyburide=Up, metformin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5      6       1    0.833   0.152       0.5827            1
    7      4       1    0.625   0.213       0.3200            1
   10      2       1    0.312   0.245       0.0671            1

                glyburide=Up, metformin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    244       4    0.984 0.00813        0.968        1.000
    2    229       4    0.966 0.01167        0.944        0.990
    3    203       6    0.938 0.01613        0.907        0.970
    4    164       9    0.886 0.02260        0.843        0.932
    5    131       7    0.839 0.02759        0.787        0.895
    6    107       9    0.768 0.03384        0.705        0.838
    7     79       4    0.730 0.03730        0.660        0.806
    8     64       7    0.650 0.04375        0.569        0.741
    9     46       1    0.636 0.04502        0.553        0.730
   10     34       4    0.561 0.05302        0.466        0.675
   11     25       4    0.471 0.06062        0.366        0.606
   12     15       1    0.440 0.06420        0.330        0.585
   13     12       1    0.403 0.06851        0.289        0.562

                glyburide=Up, metformin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     74       1    0.986  0.0134        0.961        1.000
    4     66       3    0.942  0.0284        0.888        0.999
    5     53       4    0.871  0.0431        0.790        0.959
    6     42       1    0.850  0.0468        0.763        0.947
    7     37       3    0.781  0.0575        0.676        0.902
    8     29       1    0.754  0.0615        0.643        0.885
    9     21       2    0.682  0.0737        0.552        0.843
   12      6       1    0.569  0.1206        0.375        0.862

                glyburide=Up, metformin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     20       1    0.950  0.0487        0.859        1.000
    3     16       1    0.891  0.0734        0.758        1.000
    4     14       2    0.763  0.1044        0.584        0.998
   10      2       1    0.382  0.2749        0.093        1.000

> plot(gly_combi_test_4)
> plot(gly_combi_test_4,main = "glyburide + metformin")
> 
