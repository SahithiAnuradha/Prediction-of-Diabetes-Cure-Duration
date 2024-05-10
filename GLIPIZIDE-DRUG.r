
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
> library(ggplot2)
> library(ggfortify)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> libarary(ranger)
Error in libarary(ranger) : could not find function "libarary"
> library(ranger)
> dia <- read.csv("diabetic_data.csv",TRUE,",")
>  #DRUG : GLIPIZIDE
>  dia <- dia %>%
+         mutate(diabetesMed = ifelse(diabetesMed == "NO",0,1))
>  #KEPLER MEIER FIT FOR GLIPIZIDE DRUG
> 
> 
> glipizide_km_fit <- survfit(Surv(time_in_hospital,diabetesMed) ~ glipizide ,data = dia)
> gli_test = mean(summary(glipizide_km_fit$surv))
>  if(gli_test > 0.5)
+  { 
+       print("FURTHUR TREATMENT IS NOT REQUIRED")
+       print("PATIENT HAS BEEN CURED WITH THE DRUG ALONE")
+  }else{
+       #GLIPIZIDE WITH INSULIN
+       #KEPLER MEIER FIT
+ 
+       dia1 <- subset(dia , readmitted ==">30"|readmitted == "<30")
+       dia1 <- dia1 %>%
+               mutate(readmitted = ifelse(readmitted == ">30" ,0,1))
+       glipi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + insulin ,data = dia1)
+       #VISUALISATION
+ 
+       
+       glipi_with_insulin_test = mean(summary(glipi_insulin_fit$surv))
+       if(glipi_with_insulin_test > 0.5)
+       {
+              print("PATIENT'S SURVIVABILITY HAS BEEN INCREASED AFTER TREATMENT WITH INSULIN AND FURTHER TREATMENT IS NOT REAQUIRED")
+       }
+       else{
+              print("PATIENT HAS TO BE TREATED WITH COMBINATION ALONG WITH INSULIN")
+              #COMBINATION OF DRUGS ALONG WITH INSULIN
+              #COMBINATION - 1
+              glipi_combi_fit_R <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + glipizide  ,data = dia1)
+            
+              pio_combi_test_1 = mean(summary(pio_combi_fit_R$surv))
+              #COMBINATION - 2
+              pio_combi_fit_P <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + glipizide ,data = dia1)
+             
+              met_combi_test_2 = mean(summary(met_combi_fit_P$surv))
+              #COMBINATION - 3
+              pio_combi_test_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + glipizide ,data = dia1)
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
+                          print("COMBINATION OF GLIPIZIDE + PIOGLITAZONE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+              }else if(met_combi_test_2 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_2)
+                          print("COMBIMATION OF GLIPIZIDE + GLYBURIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")
+                      }
+               else if(met_combi_test_3 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_3)
+                          print("COMBIMATION OF METFORMIN + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY") 
+                     }
+               else if(met_combi_test_4 > 0.5){
+                          print("SURVIVABILITY CAPABILITY")
+                          print(met_combi_test_4)
+                          print("COMBIMATION OF ROSIGLITAZONE + GLIPIZIDE HAS INCREASED THE SURVIVABILITY CAPABILITY")  
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
> autoplot( glipizide_km_fit,main = "GLIPIZIDE")
> summary(glipizide_km_fit)
Call: survfit(formula = Surv(time_in_hospital, diabetesMed) ~ glipizide, 
    data = dia)

                glipizide=Down 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    560      33   0.9411 0.00995       0.9218       0.9608
    2    527      46   0.8589 0.01471       0.8306       0.8882
    3    481      83   0.7107 0.01916       0.6741       0.7493
    4    398      85   0.5589 0.02098       0.5193       0.6016
    5    313      57   0.4571 0.02105       0.4177       0.5003
    6    256      46   0.3750 0.02046       0.3370       0.4173
    7    210      45   0.2946 0.01926       0.2592       0.3349
    8    165      47   0.2107 0.01723       0.1795       0.2473
    9    118      23   0.1696 0.01586       0.1412       0.2038
   10     95      28   0.1196 0.01371       0.0956       0.1498
   11     67      19   0.0857 0.01183       0.0654       0.1123
   12     48      13   0.0625 0.01023       0.0453       0.0861
   13     35      12   0.0411 0.00839       0.0275       0.0613
   14     23      23   0.0000     NaN           NA           NA

                glipizide=No 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  89080   12645   0.8580 0.001169      0.85576       0.8603
    2  76435   15193   0.6875 0.001553      0.68446       0.6905
    3  61242   15521   0.5133 0.001675      0.50999       0.5166
    4  45721   12108   0.3773 0.001624      0.37417       0.3805
    5  33613    8665   0.2801 0.001504      0.27713       0.2830
    6  24948    6608   0.2059 0.001355      0.20324       0.2086
    7  18340    5074   0.1489 0.001193      0.14660       0.1513
    8  13266    3794   0.1063 0.001033      0.10433       0.1084
    9   9472    2592   0.0772 0.000894      0.07550       0.0790
   10   6880    2039   0.0543 0.000760      0.05288       0.0559
   11   4841    1603   0.0363 0.000627      0.03514       0.0376
   12   3238    1266   0.0221 0.000493      0.02119       0.0231
   13   1972    1063   0.0102 0.000337      0.00957       0.0109
   14    909     909   0.0000      NaN           NA           NA

                glipizide=Steady 
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  11356    1500  0.86791 0.003177      0.86171       0.8742
    2   9856    1927  0.69822 0.004308      0.68983       0.7067
    3   7929    2057  0.51708 0.004689      0.50797       0.5264
    4   5872    1620  0.37443 0.004542      0.36563       0.3834
    5   4252    1151  0.27307 0.004181      0.26500       0.2814
    6   3101     816  0.20122 0.003762      0.19398       0.2087
    7   2285     671  0.14213 0.003277      0.13585       0.1487
    8   1614     483  0.09959 0.002810      0.09424       0.1053
    9   1131     344  0.06930 0.002383      0.06479       0.0741
   10    787     231  0.04896 0.002025      0.04515       0.0531
   11    556     203  0.03108 0.001629      0.02805       0.0344
   12    353     145  0.01832 0.001258      0.01601       0.0210
   13    208     115  0.00819 0.000846      0.00669       0.0100
   14     93      93  0.00000      NaN           NA           NA

                glipizide=Up 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    770      30   0.9610 0.00697       0.9475       0.9748
    2    740      58   0.8857 0.01147       0.8635       0.9085
    3    682      95   0.7623 0.01534       0.7329       0.7930
    4    587     111   0.6182 0.01751       0.5848       0.6535
    5    476      93   0.4974 0.01802       0.4633       0.5340
    6    383      69   0.4078 0.01771       0.3745       0.4440
    7    314      69   0.3182 0.01679       0.2869       0.3528
    8    245      67   0.2312 0.01519       0.2032       0.2629
    9    178      43   0.1753 0.01370       0.1504       0.2043
   10    135      44   0.1182 0.01163       0.0974       0.1433
   11     91      30   0.0792 0.00973       0.0623       0.1008
   12     61      24   0.0481 0.00771       0.0351       0.0658
   13     37      20   0.0221 0.00530       0.0138       0.0353
   14     17      17   0.0000     NaN           NA           NA

> #DRUG + INSULIN
> glipi_insulin_fit <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + insulin ,data = dia1)
> plot(glipi_insulin_fit,main = "GLIPIZIDE + INSULIN")
> summary(glipi_insulin_fit)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ glipizide + 
    insulin, data = dia1)

                glipizide=Down, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     25       2    0.920  0.0543       0.8196        1.000
    6     18       1    0.869  0.0714       0.7397        1.000
   10     12       3    0.652  0.1211       0.4528        0.938
   13      4       1    0.489  0.1678       0.2494        0.958
   14      2       1    0.244  0.1921       0.0524        1.000

                glipizide=Down, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    155       2    0.987 0.00906        0.969        1.000
    2    141       1    0.980 0.01139        0.958        1.000
    3    124       9    0.909 0.02516        0.861        0.960
    4     96       7    0.843 0.03355        0.779        0.911
    5     66       3    0.804 0.03863        0.732        0.884
    6     52       2    0.773 0.04290        0.694        0.862
    7     42       3    0.718 0.05031        0.626        0.824
    8     32       3    0.651 0.05872        0.545        0.777
   10     20       2    0.586 0.06855        0.466        0.737
   11     16       2    0.513 0.07710        0.382        0.688
   14      5       2    0.308 0.12145        0.142        0.667

                glipizide=Down, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     88       1    0.989  0.0113        0.967        1.000
    2     86       2    0.966  0.0195        0.928        1.000
    3     79       4    0.917  0.0302        0.859        0.978
    4     64       4    0.859  0.0396        0.785        0.941
    5     50       3    0.808  0.0471        0.721        0.906
    6     40       2    0.767  0.0527        0.671        0.878
    7     35       3    0.702  0.0603        0.593        0.831
    8     29       7    0.532  0.0721        0.408        0.694
    9     19       1    0.504  0.0736        0.379        0.671
   10     15       1    0.471  0.0760        0.343        0.646
   11     11       1    0.428  0.0802        0.296        0.618
   12     10       1    0.385  0.0828        0.253        0.587
   13      8       2    0.289  0.0856        0.162        0.516
   14      6       1    0.241  0.0838        0.122        0.476

                glipizide=Down, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     23       1    0.957  0.0425        0.877        1.000
    3     21       1    0.911  0.0601        0.800        1.000
    5     18       1    0.860  0.0751        0.725        1.000
    6     14       2    0.737  0.1031        0.561        0.970
    7     10       1    0.664  0.1162        0.471        0.935
    9      6       2    0.442  0.1494        0.228        0.858

                glipizide=No, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5910     123    0.979 0.00186        0.976        0.983
    2   5386     209    0.941 0.00313        0.935        0.947
    3   4526     249    0.889 0.00435        0.881        0.898
    4   3564     244    0.829 0.00553        0.818        0.839
    5   2716     153    0.782 0.00638        0.769        0.794
    6   2089     128    0.734 0.00726        0.720        0.748
    7   1583     107    0.684 0.00820        0.668        0.701
    8   1172     100    0.626 0.00935        0.608        0.645
    9    844      70    0.574 0.01043        0.554        0.595
   10    608      66    0.512 0.01179        0.489        0.535
   11    424      28    0.478 0.01262        0.454        0.503
   12    292      27    0.434 0.01403        0.407        0.462
   13    188      29    0.367 0.01647        0.336        0.401
   14     89      23    0.272 0.02095        0.234        0.316

                glipizide=No, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  17457     496    0.972 0.00126        0.969        0.974
    2  14899     660    0.929 0.00203        0.925        0.933
    3  11850     694    0.874 0.00277        0.869        0.880
    4   8803     535    0.821 0.00342        0.814        0.828
    5   6395     409    0.769 0.00407        0.761        0.777
    6   4729     310    0.718 0.00470        0.709        0.727
    7   3424     269    0.662 0.00545        0.651        0.672
    8   2449     190    0.610 0.00617        0.598        0.623
    9   1715     128    0.565 0.00690        0.551        0.579
   10   1223     101    0.518 0.00773        0.503        0.534
   11    829      61    0.480 0.00857        0.464        0.497
   12    551      64    0.424 0.01001        0.405        0.444
   13    342      50    0.362 0.01178        0.340        0.386
   14    152      36    0.276 0.01539        0.248        0.308

                glipizide=No, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1  12070     300    0.975 0.00142        0.972        0.978
    2  10632     480    0.931 0.00238        0.926        0.936
    3   8527     508    0.876 0.00327        0.869        0.882
    4   6450     455    0.814 0.00413        0.806        0.822
    5   4644     330    0.756 0.00491        0.746        0.766
    6   3407     259    0.699 0.00569        0.688        0.710
    7   2465     184    0.646 0.00644        0.634        0.659
    8   1769     165    0.586 0.00735        0.572        0.601
    9   1233     107    0.535 0.00819        0.519        0.552
   10    870      73    0.490 0.00904        0.473        0.508
   11    616      34    0.463 0.00966        0.445        0.483
   12    405      46    0.411 0.01125        0.389        0.433
   13    228      33    0.351 0.01357        0.326        0.379
   14    101      28    0.254 0.01846        0.220        0.293

                glipizide=No, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   5283     108    0.980 0.00195        0.976        0.983
    2   4843     163    0.947 0.00316        0.940        0.953
    3   4154     197    0.902 0.00434        0.893        0.910
    4   3348     178    0.854 0.00539        0.843        0.864
    5   2618     152    0.804 0.00641        0.792        0.817
    6   2028     127    0.754 0.00740        0.739        0.768
    7   1560     103    0.704 0.00838        0.688        0.721
    8   1177      83    0.654 0.00940        0.636        0.673
    9    864      61    0.608 0.01043        0.588        0.629
   10    635      53    0.557 0.01166        0.535        0.581
   11    442      38    0.510 0.01299        0.485        0.536
   12    298      34    0.451 0.01485        0.423        0.481
   13    174      26    0.384 0.01756        0.351        0.420
   14     89      21    0.293 0.02188        0.253        0.340

                glipizide=Steady, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    465       6    0.987 0.00523        0.977        0.997
    2    428      23    0.934 0.01184        0.911        0.958
    3    360      16    0.893 0.01520        0.863        0.923
    4    287      20    0.830 0.01949        0.793        0.869
    5    232      11    0.791 0.02188        0.749        0.835
    6    172      10    0.745 0.02498        0.698        0.796
    7    133       9    0.695 0.02839        0.641        0.752
    8     92       7    0.642 0.03250        0.581        0.709
    9     63       5    0.591 0.03705        0.522        0.668
   11     31       3    0.534 0.04587        0.451        0.632
   12     25       4    0.448 0.05491        0.353        0.570
   13     10       2    0.359 0.07173        0.242        0.531
   14      5       1    0.287 0.08607        0.159        0.516

                glipizide=Steady, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   2931      83    0.972 0.00306        0.966        0.978
    2   2507     114    0.927 0.00499        0.918        0.937
    3   1989     119    0.872 0.00681        0.859        0.885
    4   1462      99    0.813 0.00855        0.796        0.830
    5   1037      79    0.751 0.01036        0.731        0.772
    6    739      50    0.700 0.01189        0.677        0.724
    7    533      38    0.650 0.01352        0.624        0.677
    8    371      30    0.598 0.01547        0.568        0.629
    9    254      20    0.551 0.01747        0.517        0.586
   10    177      15    0.504 0.01971        0.467        0.544
   11    129      16    0.441 0.02263        0.399        0.488
   12     81       6    0.409 0.02458        0.363        0.460
   13     49       2    0.392 0.02625        0.344        0.447
   14     27       8    0.276 0.03910        0.209        0.364

                glipizide=Steady, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   1634      33    0.980 0.00348        0.973        0.987
    2   1483      40    0.953 0.00533        0.943        0.964
    3   1243      70    0.900 0.00801        0.884        0.916
    4    914      69    0.832 0.01080        0.811        0.853
    5    657      39    0.782 0.01273        0.758        0.808
    6    480      37    0.722 0.01512        0.693        0.752
    7    353      19    0.683 0.01673        0.651        0.717
    8    246      22    0.622 0.01966        0.585        0.662
    9    171       7    0.597 0.02108        0.557        0.639
   10    118      10    0.546 0.02462        0.500        0.597
   11     83       5    0.513 0.02718        0.463        0.569
   12     52       7    0.444 0.03381        0.383        0.516
   13     32       2    0.416 0.03696        0.350        0.495
   14     16       6    0.260 0.05543        0.171        0.395

                glipizide=Steady, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    471       7    0.985 0.00558        0.974        0.996
    2    440      13    0.956 0.00962        0.937        0.975
    3    390      14    0.922 0.01293        0.897        0.947
    4    321      15    0.879 0.01642        0.847        0.911
    5    250      12    0.836 0.01964        0.799        0.876
    6    194      12    0.785 0.02342        0.740        0.832
    7    139       6    0.751 0.02618        0.701        0.804
    8     96      11    0.665 0.03366        0.602        0.734
    9     67       6    0.605 0.03843        0.534        0.685
   10     54       3    0.572 0.04091        0.497        0.658
   11     36       4    0.508 0.04710        0.424        0.609
   12     22       2    0.462 0.05295        0.369        0.578
   14      7       1    0.396 0.07611        0.272        0.577

                glipizide=Up, insulin=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     44       2    0.955  0.0314        0.895        1.000
    3     40       1    0.931  0.0386        0.858        1.000
    4     36       3    0.853  0.0556        0.751        0.969
    5     29       3    0.765  0.0694        0.640        0.914
    6     23       2    0.698  0.0777        0.562        0.868
    7     21       2    0.632  0.0833        0.488        0.818
    8     19       2    0.565  0.0868        0.418        0.764
    9     11       1    0.514  0.0929        0.361        0.732
   10      9       1    0.457  0.0986        0.299        0.697

                glipizide=Up, insulin=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    162       2    0.988 0.00868        0.971        1.000
    2    156       3    0.969 0.01380        0.942        0.996
    3    143       7    0.921 0.02185        0.879        0.965
    4    123       6    0.876 0.02743        0.824        0.932
    5     95       3    0.849 0.03087        0.790        0.911
    6     78       2    0.827 0.03369        0.763        0.896
    7     65       4    0.776 0.04009        0.701        0.859
    8     50       2    0.745 0.04409        0.663        0.837
    9     40       2    0.708 0.04912        0.618        0.811
   10     29       1    0.683 0.05315        0.587        0.796
   11     20       2    0.615 0.06625        0.498        0.760
   12     11       1    0.559 0.08043        0.422        0.741
   13      7       1    0.479 0.10109        0.317        0.725
   14      5       2    0.288 0.12125        0.126        0.657

                glipizide=Up, insulin=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2    121       1    0.992 0.00823       0.9757        1.000
    3    114       5    0.948 0.02058       0.9087        0.989
    4    100       5    0.901 0.02845       0.8468        0.958
    6     70       2    0.875 0.03295       0.8128        0.942
    7     57       4    0.814 0.04261       0.7343        0.902
    8     41       3    0.754 0.05152       0.6596        0.862
    9     32       1    0.731 0.05504       0.6303        0.847
   10     26       4    0.618 0.06958       0.4958        0.771
   11     21       1    0.589 0.07222       0.4629        0.749
   12     17       1    0.554 0.07583       0.4238        0.725
   14      8       5    0.208 0.09901       0.0817        0.529

                glipizide=Up, insulin=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     55       1    0.982  0.0180       0.9471        1.000
    4     47       2    0.940  0.0337       0.8763        1.000
    5     42       1    0.918  0.0396       0.8432        0.999
    6     33       3    0.834  0.0584       0.7274        0.957
    9     19       1    0.790  0.0699       0.6646        0.940
   10     18       3    0.659  0.0906       0.5029        0.862
   13      2       1    0.329  0.2372       0.0802        1.000

> #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + glipizide  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + ROSIGLITAZONE")
> summary(met_combi_fit_1)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    glipizide, data = dia1)

                pioglitazone=Down, glipizide=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                pioglitazone=Down, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     47       2    0.957  0.0294       0.9014        1.000
    4     43       5    0.846  0.0536       0.7474        0.958
    5     36       3    0.776  0.0627       0.6620        0.909
    6     22       1    0.740  0.0690       0.6167        0.889
    9     11       2    0.606  0.1030       0.4341        0.845
   14      2       1    0.303  0.2203       0.0728        1.000

                pioglitazone=Down, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    6      6       1    0.833   0.152        0.583            1
   10      4       1    0.625   0.213        0.320            1
   11      1       1    0.000     NaN           NA           NA

                pioglitazone=Down, glipizide=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
          14            1            1            0          NaN           NA           NA 

                pioglitazone=No, glipizide=Down   
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

                pioglitazone=No, glipizide=No     
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

                pioglitazone=No, glipizide=Steady 
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

                pioglitazone=No, glipizide=Up     
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

                pioglitazone=Steady, glipizide=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     17       1    0.941  0.0571        0.836            1
    6     10       1    0.847  0.1030        0.667            1
    8      8       1    0.741  0.1339        0.520            1
   10      7       1    0.635  0.1509        0.399            1

                pioglitazone=Steady, glipizide=No     
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

                pioglitazone=Steady, glipizide=Steady 
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

                pioglitazone=Steady, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     42       1    0.976  0.0235        0.931        1.000
    3     36       1    0.949  0.0352        0.883        1.000
    4     32       2    0.890  0.0523        0.793        0.998
    5     28       2    0.826  0.0651        0.708        0.964
    6     21       1    0.787  0.0729        0.656        0.944
    7     18       1    0.743  0.0809        0.600        0.920
    8     14       1    0.690  0.0909        0.533        0.893
   14      2       2    0.000     NaN           NA           NA

                pioglitazone=Up, glipizide=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       6.000        2.000        1.000        0.500        0.354        0.125        1.000 

                pioglitazone=Up, glipizide=No     
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

                pioglitazone=Up, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     20       1    0.950  0.0487        0.859            1
    7      8       2    0.712  0.1500        0.472            1
    9      4       1    0.534  0.1909        0.265            1

                pioglitazone=Up, glipizide=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + glipizide  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + GLIPIZIDE)
+ summary(met_combi_fit_1)
+ 
+ 
+ 

+ > #DRUG COMBI - 1
> met_combi_fit_1 <- survfit(Surv(time_in_hospital,readmitted) ~ pioglitazone + glipizide  ,data = dia1)
> plot(met_combi_fit_1,main = "DRUG COMBINATION:PIOGLITAZONE + GLIPIZIDE")
> summary(met_combi_fit_1)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ pioglitazone + 
    glipizide, data = dia1)

                pioglitazone=Down, glipizide=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                pioglitazone=Down, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     47       2    0.957  0.0294       0.9014        1.000
    4     43       5    0.846  0.0536       0.7474        0.958
    5     36       3    0.776  0.0627       0.6620        0.909
    6     22       1    0.740  0.0690       0.6167        0.889
    9     11       2    0.606  0.1030       0.4341        0.845
   14      2       1    0.303  0.2203       0.0728        1.000

                pioglitazone=Down, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    6      6       1    0.833   0.152        0.583            1
   10      4       1    0.625   0.213        0.320            1
   11      1       1    0.000     NaN           NA           NA

                pioglitazone=Down, glipizide=Up     
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
          14            1            1            0          NaN           NA           NA 

                pioglitazone=No, glipizide=Down   
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

                pioglitazone=No, glipizide=No     
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

                pioglitazone=No, glipizide=Steady 
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

                pioglitazone=No, glipizide=Up     
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

                pioglitazone=Steady, glipizide=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     17       1    0.941  0.0571        0.836            1
    6     10       1    0.847  0.1030        0.667            1
    8      8       1    0.741  0.1339        0.520            1
   10      7       1    0.635  0.1509        0.399            1

                pioglitazone=Steady, glipizide=No     
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

                pioglitazone=Steady, glipizide=Steady 
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

                pioglitazone=Steady, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     42       1    0.976  0.0235        0.931        1.000
    3     36       1    0.949  0.0352        0.883        1.000
    4     32       2    0.890  0.0523        0.793        0.998
    5     28       2    0.826  0.0651        0.708        0.964
    6     21       1    0.787  0.0729        0.656        0.944
    7     18       1    0.743  0.0809        0.600        0.920
    8     14       1    0.690  0.0909        0.533        0.893
   14      2       2    0.000     NaN           NA           NA

                pioglitazone=Up, glipizide=Down   
        time       n.risk      n.event     survival      std.err lower 95% CI upper 95% CI 
       6.000        2.000        1.000        0.500        0.354        0.125        1.000 

                pioglitazone=Up, glipizide=No     
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

                pioglitazone=Up, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     20       1    0.950  0.0487        0.859            1
    7      8       2    0.712  0.1500        0.472            1
    9      4       1    0.534  0.1909        0.265            1

                pioglitazone=Up, glipizide=Up     
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

> #DRUG COMBI - 2
> met_combi_fit_2 <- survfit(Surv(time_in_hospital,readmitted) ~ glyrbuide + glipizide ,data = dia1)
Error in eval(predvars, data, env) : object 'glyrbuide' not found
> plot(met_combi_fit_2,main = "DRUG COMBINATION:GLIPIZIDE + GLYBURIDE")
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
> met_combi_fit_3 <- survfit(Surv(time_in_hospital,readmitted) ~ metformin + glipizide ,data = dia1)
> plot(met_combi_fit_3,main = "DRUG COMBINATION : GLIPIZIDE + METFORMIN")
> summary(met_combi_fit_3)
Call: survfit(formula = Surv(time_in_hospital, readmitted) ~ metformin + 
    glipizide, data = dia1)

                metformin=Down, glipizide=Down   
     time n.risk n.event survival std.err lower 95% CI upper 95% CI

                metformin=Down, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    201       4    0.980 0.00985       0.9610        1.000
    2    186       4    0.959 0.01420       0.9316        0.987
    3    162       6    0.924 0.01973       0.8856        0.963
    4    134       4    0.896 0.02347       0.8511        0.943
    5     97       7    0.831 0.03207       0.7707        0.897
    6     76       7    0.755 0.04010       0.6801        0.838
    7     58       7    0.664 0.04781       0.5762        0.764
    8     35       5    0.569 0.05674       0.4678        0.692
   10     16       3    0.462 0.07216       0.3403        0.628
   13      5       2    0.277 0.11012       0.1273        0.604
   14      2       1    0.139 0.11244       0.0283        0.680

                metformin=Down, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     46       1    0.978  0.0215       0.9370        1.000
    3     43       1    0.956  0.0308       0.8971        1.000
    4     37       3    0.878  0.0514       0.7829        0.985
    5     25       3    0.773  0.0728       0.6424        0.929
    6     19       2    0.691  0.0849       0.5435        0.879
    7     16       1    0.648  0.0899       0.4939        0.851
    8     13       1    0.598  0.0958       0.4371        0.819
   10      8       1    0.523  0.1092       0.3478        0.788
   11      7       1    0.449  0.1164       0.2698        0.746
   14      3       2    0.150  0.1281       0.0279        0.802

                metformin=Down, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2      4       1     0.75   0.217        0.426            1
    4      3       1     0.50   0.250        0.188            1
   11      1       1     0.00     NaN           NA           NA

                metformin=No, glipizide=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    223       2    0.991 0.00631        0.979        1.000
    2    207       3    0.977 0.01032        0.957        0.997
    3    191      10    0.926 0.01853        0.890        0.963
    4    154      10    0.865 0.02526        0.817        0.916
    5    117       2    0.851 0.02691        0.800        0.905
    6     99       6    0.799 0.03248        0.738        0.865
    7     83       6    0.741 0.03773        0.671        0.819
    8     66       9    0.640 0.04520        0.558        0.735
    9     46       3    0.598 0.04825        0.511        0.701
   10     38       5    0.520 0.05322        0.425        0.635
   11     28       3    0.464 0.05640        0.366        0.589
   13     14       2    0.398 0.06497        0.289        0.548
   14      9       2    0.309 0.07478        0.193        0.497

                metformin=No, glipizide=No     
 time n.risk n.event survival  std.err lower 95% CI upper 95% CI
    1  33654     900    0.973 0.000879        0.972        0.975
    2  29444    1260    0.932 0.001424        0.929        0.934
    3  23947    1357    0.879 0.001934        0.875        0.883
    4  18314    1187    0.822 0.002414        0.817        0.827
    5  13558     866    0.769 0.002844        0.764        0.775
    6  10178     685    0.718 0.003269        0.711        0.724
    7   7515     555    0.665 0.003722        0.657        0.672
    8   5493     447    0.611 0.004207        0.602        0.619
    9   3895     304    0.563 0.004683        0.554        0.572
   10   2803     249    0.513 0.005230        0.503        0.523
   11   1947     144    0.475 0.005719        0.464        0.486
   12   1291     150    0.420 0.006595        0.407        0.433
   13    773     108    0.361 0.007719        0.346        0.377
   14    365      88    0.274 0.009984        0.255        0.294

                metformin=No, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   4105      95    0.977 0.00235        0.972        0.981
    2   3610     151    0.936 0.00396        0.928        0.944
    3   2958     178    0.880 0.00553        0.869        0.891
    4   2214     150    0.820 0.00697        0.807        0.834
    5   1622     109    0.765 0.00827        0.749        0.781
    6   1183      90    0.707 0.00965        0.688        0.726
    7    859      58    0.659 0.01084        0.638        0.681
    8    595      48    0.606 0.01239        0.582        0.631
    9    415      30    0.562 0.01384        0.536        0.590
   10    298      24    0.517 0.01550        0.487        0.548
   11    213      23    0.461 0.01766        0.428        0.497
   12    136      17    0.403 0.02024        0.366        0.445
   13     79       5    0.378 0.02195        0.337        0.423
   14     37       8    0.296 0.03082        0.242        0.363

                metformin=No, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    275       3    0.989 0.00626        0.977        1.000
    2    265       4    0.974 0.00964        0.955        0.993
    3    246      10    0.935 0.01536        0.905        0.965
    4    214      11    0.887 0.02028        0.848        0.927
    5    173       4    0.866 0.02225        0.823        0.911
    6    141       6    0.829 0.02590        0.780        0.882
    7    120       7    0.781 0.03016        0.724        0.842
    8     93       4    0.747 0.03321        0.685        0.815
    9     77       3    0.718 0.03592        0.651        0.792
   10     64       7    0.640 0.04252        0.561        0.729
   11     46       2    0.612 0.04499        0.530        0.707
   12     30       1    0.591 0.04789        0.505        0.693
   13     19       2    0.529 0.05975        0.424        0.660
   14     10       5    0.265 0.08883        0.137        0.511

                metformin=Steady, glipizide=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1     60       1    0.983  0.0165       0.9515        1.000
    2     57       1    0.966  0.0236       0.9210        1.000
    3     49       3    0.907  0.0398       0.8322        0.988
    4     40       3    0.839  0.0528       0.7416        0.949
    5     29       3    0.752  0.0670       0.6317        0.896
    6     21       1    0.716  0.0727       0.5870        0.874
    7     18       1    0.677  0.0788       0.5384        0.850
    8     15       1    0.631  0.0855       0.4842        0.823
   10     10       1    0.568  0.0975       0.4060        0.796
   12      7       1    0.487  0.1124       0.3099        0.766
   13      6       1    0.406  0.1194       0.2280        0.723
   14      4       2    0.203  0.1177       0.0651        0.633

                metformin=Steady, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   6519     119    0.982 0.00166        0.979        0.985
    2   5806     240    0.941 0.00302        0.935        0.947
    3   4661     274    0.886 0.00431        0.877        0.894
    4   3484     213    0.832 0.00541        0.821        0.842
    5   2531     165    0.777 0.00650        0.765        0.790
    6   1849     121    0.727 0.00754        0.712        0.742
    7   1347      96    0.675 0.00866        0.658        0.692
    8    962      82    0.617 0.00998        0.598        0.637
    9    684      57    0.566 0.01124        0.544        0.588
   10    476      38    0.521 0.01250        0.497        0.546
   11    324      15    0.497 0.01339        0.471        0.524
   12    228      20    0.453 0.01535        0.424        0.484
   13    143      26    0.371 0.01927        0.335        0.410
   14     60      18    0.259 0.02574        0.214        0.315

                metformin=Steady, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1   1283      34    0.973 0.00448        0.965        0.982
    2   1138      38    0.941 0.00676        0.928        0.954
    3    922      38    0.902 0.00894        0.885        0.920
    4    684      46    0.842 0.01201        0.818        0.865
    5    491      28    0.794 0.01434        0.766        0.822
    6    353      16    0.758 0.01627        0.726        0.790
    7    260      12    0.723 0.01839        0.687        0.760
    8    179      20    0.642 0.02358        0.597        0.690
    9    116       8    0.598 0.02665        0.548        0.652
   10     77       3    0.574 0.02880        0.521        0.634
   11     53       4    0.531 0.03381        0.469        0.602
   12     34       2    0.500 0.03837        0.430        0.581
   13     19       1    0.473 0.04446        0.394        0.569
   14     14       6    0.271 0.06757        0.166        0.441

                metformin=Steady, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    2     88       1    0.989  0.0113       0.9667        1.000
    3     83       3    0.953  0.0230       0.9089        0.999
    4     73       3    0.914  0.0312       0.8545        0.977
    5     60       3    0.868  0.0393       0.7944        0.949
    6     50       3    0.816  0.0470       0.7288        0.914
    7     36       2    0.771  0.0543       0.6713        0.885
    8     31       3    0.696  0.0638       0.5815        0.833
    9     19       2    0.623  0.0753       0.4914        0.789
   10     12       1    0.571  0.0850       0.4264        0.764
   12      7       1    0.489  0.1049       0.3214        0.745
   14      2       1    0.245  0.1808       0.0575        1.000

                metformin=Up, glipizide=Down   
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3      7       1    0.857   0.132        0.633            1
    5      5       2    0.514   0.204        0.236            1

                metformin=Up, glipizide=No     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    346       4    0.988 0.00575        0.977        1.000
    2    324       8    0.964 0.01020        0.944        0.984
    3    287      11    0.927 0.01468        0.899        0.956
    4    233       8    0.895 0.01798        0.861        0.931
    5    187       6    0.867 0.02088        0.827        0.908
    6    150      11    0.803 0.02673        0.752        0.857
    7    112       5    0.767 0.02996        0.711        0.828
    8     77       4    0.727 0.03440        0.663        0.798
    9     56       5    0.662 0.04183        0.585        0.750
   10     41       3    0.614 0.04721        0.528        0.714
   11     29       2    0.572 0.05259        0.477        0.685
   12     19       1    0.541 0.05779        0.439        0.667
   13     11       2    0.443 0.07874        0.313        0.628
   14      4       1    0.332 0.11264        0.171        0.646

                metformin=Up, glipizide=Steady 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    3     59       2    0.966  0.0236        0.921        1.000
    4     49       4    0.887  0.0435        0.806        0.977
    5     38       1    0.864  0.0483        0.774        0.964
    6     30       1    0.835  0.0546        0.735        0.949
    7     23       1    0.799  0.0631        0.684        0.933
    8     18       1    0.754  0.0736        0.623        0.913

                metformin=Up, glipizide=Up     
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    4     16       1    0.938  0.0605        0.826            1
    7     11       1    0.852  0.0981        0.680            1
   10      5       1    0.682  0.1715        0.416            1
   14      1       1    0.000     NaN           NA           NA

> #DRUG COMBI - 4
> met_combi_fit_4 <- survfit(Surv(time_in_hospital,readmitted) ~ glipizide + rosiglitazone ,data = dia1)
> plot(met_combi_fit_4,main = "DRUG COMBINATION : GLIPIZIDE + ROSIGLITAZONE")
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
