---
layout: single   
title: "[시계열 분석]R에서 tidyverse를 활용한 분해법(Decomposition)"   
excerpt: "이동평균법과 지수평활법을 소개하고 R을 활용하여 실습을 진행합니다."   
tags:
  - time series
  - R
comments: true
toc: true
toc_sticky: true
categories:
  - time series   
use_math: true   
output:
  md_document:
    variant: markdown_github
    preserve_yaml: true

---



``` r
library(tidyverse)
library(ggplot2)
library(forecast)
library(fpp)
library(aTSA)
library(seasonal)
library(timsac)
library(gridExtra)
```

``` r
autoplot(AirPassengers)
```

![](/assets/images/time_series/decomposition/unnamed-chunk-2-1.png)

클래식
------

\#\#\#가법

``` r
decompose(AirPassengers, type="additive") %>% autoplot()
```

![](/assets/images/time_series/decomposition/unnamed-chunk-3-1.png)

가법

``` r
#decomposition
add_decompose = decompose(AirPassengers, type="additive")
add_decompose %>% names()
```

    ## [1] "x"        "seasonal" "trend"    "random"   "figure"   "type"

``` r
add_decompose$trend
```

    ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
    ## 1949       NA       NA       NA       NA       NA       NA 126.7917 127.2500
    ## 1950 131.2500 133.0833 134.9167 136.4167 137.4167 138.7500 140.9167 143.1667
    ## 1951 157.1250 159.5417 161.8333 164.1250 166.6667 169.0833 171.2500 173.5833
    ## 1952 183.1250 186.2083 189.0417 191.2917 193.5833 195.8333 198.0417 199.7500
    ## 1953 215.8333 218.5000 220.9167 222.9167 224.0833 224.7083 225.3333 225.3333
    ## 1954 228.0000 230.4583 232.2500 233.9167 235.6250 237.7500 240.5000 243.9583
    ## 1955 261.8333 266.6667 271.1250 275.2083 278.5000 281.9583 285.7500 289.3333
    ## 1956 309.9583 314.4167 318.6250 321.7500 324.5000 327.0833 329.5417 331.8333
    ## 1957 348.2500 353.0000 357.6250 361.3750 364.5000 367.1667 369.4583 371.2083
    ## 1958 375.2500 377.9167 379.5000 380.0000 380.7083 380.9583 381.8333 383.6667
    ## 1959 402.5417 407.1667 411.8750 416.3333 420.5000 425.5000 430.7083 435.1250
    ## 1960 456.3333 461.3750 465.2083 469.3333 472.7500 475.0417       NA       NA
    ##           Sep      Oct      Nov      Dec
    ## 1949 127.9583 128.5833 129.0000 129.7500
    ## 1950 145.7083 148.4167 151.5417 154.7083
    ## 1951 175.4583 176.8333 178.0417 180.1667
    ## 1952 202.2083 206.2500 210.4167 213.3750
    ## 1953 224.9583 224.5833 224.4583 225.5417
    ## 1954 247.1667 250.2500 253.5000 257.1250
    ## 1955 293.2500 297.1667 301.0000 305.4583
    ## 1956 334.4583 337.5417 340.5417 344.0833
    ## 1957 372.1667 372.4167 372.7500 373.6250
    ## 1958 386.5000 390.3333 394.7083 398.6250
    ## 1959 437.7083 440.9583 445.8333 450.6250
    ## 1960       NA       NA       NA       NA

``` r
#add_decompose$seasonal
#add_decompose$$random
```

``` r
# remove seasonal effects
add_no_seasonal <- AirPassengers - add_decompose$seasonal
# remove trend effects
add_no_trend <- AirPassengers - add_decompose$trend 

grid.arrange(
  autoplot(add_no_seasonal, main="Additivity model without seasonal effects"),
  autoplot(add_no_trend, main="Additivity model without trend effects")
)
```

![](/assets/images/time_series/decomposition/unnamed-chunk-6-1.png)

### 승법

``` r
decompose(AirPassengers, type="multiplicative") %>% autoplot()
```

![](/assets/images/time_series/decomposition/unnamed-chunk-7-1.png)

``` r
#decomposition
mul_decompose <- decompose(AirPassengers, type="multiplicative")

# remove seasonal, trend effects
mul_no_seasonal <- AirPassengers / mul_decompose$seasonal
mul_no_trend <- AirPassengers / mul_decompose$trend 

# make plot
mul_no_season_plot <- autoplot(mul_no_seasonal) + 
  ggtitle("Multiplicative model without seasonal effects")
mul_no_trend_plot <- autoplot(mul_no_trend) +
  ggtitle("Multiplicative model without trend effects")

# output plot
grid.arrange(
  mul_no_season_plot, mul_no_trend_plot
)
```

![](/assets/images/time_series/decomposition/unnamed-chunk-8-1.png)

### stationary test

``` r
#Stationary for random variation after trend and seaso
tseries::kpss.test(add_decompose$random, null = "Level") 
```

    ## Warning in tseries::kpss.test(add_decompose$random, null = "Level"): p-value
    ## greater than printed p-value
    
    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  add_decompose$random
    ## KPSS Level = 0.016442, Truncation lag parameter = 4, p-value = 0.1

``` r
#Stationary for random variation after trend and seasona
tseries::kpss.test(mul_decompose$random, null = "Level") 
```

    ## Warning in tseries::kpss.test(mul_decompose$random, null = "Level"): p-value
    ## greater than printed p-value
    
    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  mul_decompose$random
    ## KPSS Level = 0.033058, Truncation lag parameter = 4, p-value = 0.1

``` r
ggtsdisplay(add_decompose$random, main="Random/ Additive model")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-11-1.png)

``` r
ggtsdisplay(mul_decompose$random, main="Random/ Multiplicative model")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-12-1.png)

SEAS
----

``` r
data(elecequip)
autoplot(elecequip)
```

![](/assets/images/time_series/decomposition/unnamed-chunk-13-1.png)

``` r
#seasonal::seas
seas(elecequip) %>%
  autoplot() +
  ggtitle("SEATS decomposition of electrical equipment index")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-14-1.png)

X11

``` r
# seasonal effect fitting

fit_X11 <- elecequip %>% seas(x11="")
#A decomposition of the new orders index for electrical equipment.
autoplot(fit_X11) +
  ggtitle("X11 decomposition of electrical equipment index")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-15-1.png)

``` r
#the trend-cycle component and the seasonally adjusted data, along with the original data.
elecequip %>% autoplot(series="Data") +
  autolayer(trendcycle(fit_X11), series="Trend") +
  autolayer(seasadj(fit_X11), series="Seasonally Adjusted") +
  scale_colour_manual(values=c("gray", "blue", "red"),
                      breaks=c("Data", "Seasonally Adjusted", "Trend")) +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-16-1.png)

``` r
#Seasonal sub-series plot of the seasonal component from the X11 decomposition of the new orders index for electrical equipment.
fit_X11 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-17-1.png)

``` r
fit_X11 %>% seasonal()
```

    ##            Jan       Feb       Mar       Apr       May       Jun       Jul
    ## 1996 0.9628125 0.9562196 1.0787077 0.9240527 0.9602618 1.0654318 0.9937084
    ## 1997 0.9613081 0.9559650 1.0793118 0.9245328 0.9610442 1.0639506 0.9915282
    ## 1998 0.9554082 0.9555897 1.0813380 0.9255580 0.9620210 1.0644458 0.9863023
    ## 1999 0.9475910 0.9543890 1.0847655 0.9269032 0.9619855 1.0664640 0.9799758
    ## 2000 0.9413035 0.9519621 1.0875453 0.9269032 0.9589119 1.0712342 0.9748868
    ## 2001 0.9365600 0.9490127 1.0899088 0.9280976 0.9536751 1.0748703 0.9732997
    ## 2002 0.9340240 0.9452993 1.0904548 0.9295271 0.9463729 1.0805392 0.9750599
    ## 2003 0.9332209 0.9428297 1.0881845 0.9328272 0.9416744 1.0844164 0.9782025
    ## 2004 0.9351781 0.9410086 1.0840027 0.9356228 0.9401677 1.0881185 0.9811048
    ## 2005 0.9378424 0.9388581 1.0806029 0.9406105 0.9414372 1.0884971 0.9820920
    ## 2006 0.9405471 0.9360723 1.0797830 0.9440883 0.9430846 1.0899306 0.9826868
    ## 2007 0.9424575 0.9321518 1.0801219 0.9476678 0.9451361 1.0909026 0.9826233
    ## 2008 0.9448655 0.9289804 1.0816976 0.9481082 0.9475395 1.0922946 0.9843773
    ## 2009 0.9460184 0.9264027 1.0826834 0.9486275 0.9496769 1.0930685 0.9869891
    ## 2010 0.9475988 0.9249435 1.0845336 0.9468247 0.9513670 1.0940984 0.9913414
    ## 2011 0.9474725 0.9238434 1.0857337 0.9464715 0.9526125 1.0953216 0.9937929
    ##            Aug       Sep       Oct       Nov       Dec
    ## 1996 0.8090371 1.0717420 1.0337734 1.0381106 1.1071093
    ## 1997 0.8115577 1.0735762 1.0334726 1.0365928 1.1105889
    ## 1998 0.8157359 1.0745788 1.0310465 1.0361709 1.1170680
    ## 1999 0.8178071 1.0759546 1.0290429 1.0372293 1.1243332
    ## 2000 0.8195772 1.0767566 1.0281509 1.0386754 1.1298090
    ## 2001 0.8187270 1.0792815 1.0286044 1.0405238 1.1322190
    ## 2002 0.8184566 1.0809589 1.0285355 1.0426357 1.1295827
    ## 2003 0.8178020 1.0845353 1.0306346 1.0418448 1.1216434
    ## 2004 0.8196732 1.0877592 1.0323953 1.0373860 1.1130418
    ## 2005 0.8238924 1.0898014 1.0346772 1.0320863 1.1046076
    ## 2006 0.8282716 1.0883718 1.0341749 1.0307196 1.0988676
    ## 2007 0.8327034 1.0841421 1.0340074 1.0320989 1.0924416
    ## 2008 0.8358559 1.0775197 1.0318958 1.0352849 1.0898591
    ## 2009 0.8383901 1.0706803 1.0295571 1.0384927 1.0871217
    ## 2010 0.8384502 1.0652673 1.0263742 1.0419164 1.0864768
    ## 2011 0.8379636 1.0623103 1.0244167 1.0436695

``` r
#fit_X11 %>% trendcycle()
#fit_X11 %>% remainder()
#fit_X11 %>% seasadj()
```

STL
---

``` r
#stats::stl
air_stl <- stl(AirPassengers, "periodic")
air_stl %>% str()
```

    ## List of 8
    ##  $ time.series: Time-Series [1:144, 1:3] from 1949 to 1961: -25.5 -35.22 -3.03 -8.3 -5.74 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : NULL
    ##   .. ..$ : chr [1:3] "seasonal" "trend" "remainder"
    ##  $ weights    : num [1:144] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ call       : language stl(x = AirPassengers, s.window = "periodic")
    ##  $ win        : Named num [1:3] 1441 19 13
    ##   ..- attr(*, "names")= chr [1:3] "s" "t" "l"
    ##  $ deg        : Named int [1:3] 0 1 1
    ##   ..- attr(*, "names")= chr [1:3] "s" "t" "l"
    ##  $ jump       : Named num [1:3] 145 2 2
    ##   ..- attr(*, "names")= chr [1:3] "s" "t" "l"
    ##  $ inner      : int 2
    ##  $ outer      : int 0
    ##  - attr(*, "class")= chr "stl"

``` r
air_stl %>% autoplot()
```

![](/assets/images/time_series/decomposition/unnamed-chunk-20-1.png)

``` r
#forecast::seasadj
#Returns seasonally adjusted data constructed by removing the seasonal component.
air_ss <- seasadj(air_stl)
seasonal_index <- sindexf(air_stl, h=24)
seasonal_index
```

    ##             Jan        Feb        Mar        Apr        May        Jun
    ## 1961 -25.497718 -35.220935  -3.027478  -8.299054  -5.737289  32.336634
    ## 1962 -25.497718 -35.220935  -3.027478  -8.299054  -5.737289  32.336634
    ##             Jul        Aug        Sep        Oct        Nov        Dec
    ## 1961  70.243882  68.049433  17.438326 -21.063432 -57.481851 -31.740516
    ## 1962  70.243882  68.049433  17.438326 -21.063432 -57.481851 -31.740516

``` r
forecast_air <- forecast::forecast(air_stl)
forecast_air
```

    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Jan 1961       438.2412 416.3447 460.1376 404.7535 471.7289
    ## Feb 1961       428.5180 397.5533 459.4826 381.1616 475.8743
    ## Mar 1961       460.7114 422.7882 498.6346 402.7129 518.7099
    ## Apr 1961       455.4398 411.6503 499.2294 388.4695 522.4102
    ## May 1961       458.0016 409.0436 506.9596 383.1268 532.8764
    ## Jun 1961       496.0755 442.4449 549.7061 414.0546 578.0964
    ## Jul 1961       533.9828 476.0552 591.9103 445.3903 622.5753
    ## Aug 1961       531.7883 469.8613 593.7153 437.0791 626.4975
    ## Sep 1961       481.1772 415.4938 546.8607 380.7231 581.6314
    ## Oct 1961       442.6755 373.4391 511.9118 336.7876 548.5633
    ## Nov 1961       406.2570 333.6414 478.8727 295.2010 517.3131
    ## Dec 1961       431.9984 356.1539 507.8429 316.0042 547.9925
    ## Jan 1962       438.2412 359.2998 517.1826 317.5107 558.9716
    ## Feb 1962       428.5180 346.5966 510.4393 303.2301 553.8058
    ## Mar 1962       460.7114 375.9148 545.5080 331.0262 590.3966
    ## Apr 1962       455.4398 367.8623 543.0173 321.5016 589.3780
    ## May 1962       458.0016 367.7288 548.2744 319.9413 596.0619
    ## Jun 1962       496.0755 403.1856 588.9654 354.0127 638.1384
    ## Jul 1962       533.9828 438.5475 629.4180 388.0271 679.9384
    ## Aug 1962       531.7883 433.8738 629.7028 382.0410 681.5356
    ## Sep 1962       481.1772 380.8447 581.5097 327.7319 634.6225
    ## Oct 1962       442.6755 339.9819 545.3690 285.6192 599.7317
    ## Nov 1962       406.2570 301.2555 511.2586 245.6711 566.8430
    ## Dec 1962       431.9984 324.7385 539.2582 267.9586 596.0382

``` r
autoplot(forecast_air)
```

![](/assets/images/time_series/decomposition/unnamed-chunk-23-1.png)

Visualization
-------------

``` r
data(a10)
a10 %>% head(18)
```

    ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
    ## 1991                                                       3.526591 3.180891
    ## 1992 5.088335 2.814520 2.985811 3.204780 3.127578 3.270523 3.737851 3.558776
    ##           Sep      Oct      Nov      Dec
    ## 1991 3.252221 3.611003 3.565869 4.306371
    ## 1992 3.777202 3.924490 4.386531 5.810549

``` r
#glimpse(a10)
#str(a10)
```

``` r
#Monthly sales of antidiabetic drugs in Australia.
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  xlab("Year") + ylab("$ million")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-25-1.png)

``` r
#Seasonal plot of monthly antidiabetic drug sales in Australia.
ggseasonplot(a10, year.labels=TRUE, year.labels.left=F) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-26-1.png)

``` r
#Polar seasonal plot of monthly antidiabetic drug sales in Australia.
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-27-1.png)

``` r
#Seasonal subseries plot of monthly antidiabetic drug sales in Australia.
ggsubseriesplot(a10) +
ylab("$ million") +
ggtitle("Seasonal subseries plot: antidiabetic drug sales")
```

![](/assets/images/time_series/decomposition/unnamed-chunk-28-1.png)

``` r
# ACF
grid.arrange(
  ggAcf(a10) + ggtitle("ACF with default lag"), 
  ggPacf(a10) + ggtitle("PACF with default lag"), 
  ggAcf(a10, lag=48) + ggtitle("ACF with 48 lag"),
  ggPacf(a10, lag=48) + ggtitle("PACF with 48 lag"),
  nrow=2, ncol=2
)
```

![](/assets/images/time_series/decomposition/unnamed-chunk-29-1.png)
