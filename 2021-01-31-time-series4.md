---
layout: single   
title: "[시계열 분석]R에서 tidyverse를 활용한 요소분해법(Decomposition)"   
excerpt: "이동평균법과 지수평활법을 소개하고 R을 활용하여 실습을 진행합니다."   
tags:
  - time series
  - R
  - ggplot2
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

![](../assets/images/time_series/decomposition/unnamed-chunk-2-1.png)

``` r
AirPassengers %>%
stl(t.window=13, s.window="periodic", robust=TRUE) %>%
autoplot()
```

![](../assets/images/time_series/decomposition/unnamed-chunk-3-1.png)

``` r
autoplot(stl(AirPassengers, t.window=13, s.window="periodic", robust=TRUE))
```

![](../assets/images/time_series/decomposition/unnamed-chunk-4-1.png)

\#\#\#d

``` r
dd2=matrix(c(1142, 1242, 1452, 1543, 1125, 1262, 1456, 1572, 1143, 1259,
1462, 1553, 1121, 1258, 1472, 1546, 1154, 1249, 1477, 1548))
dd2.ts=ts(data=dd2, start=c(2006,1), frequency=4)
dd4=matrix(c(1142, 1242, 1452, 1543, 1225, 1362, 1556, 1672, 1343, 1459,
1662, 1753, 1421, 1558, 1772, 1846, 1554, 1649, 1877, 1948))
dd4.ts=ts(data=dd4, start=c(2006,1), frequency=4)

par(mfrow=c(1,2))
plot(decompose(dd2.ts, type="additive"))
```

![](../assets/images/time_series/decomposition/unnamed-chunk-5-1.png)

``` r
plot(decompose(dd4.ts, type="additive")) # additive model
```

![](../assets/images/time_series/decomposition/unnamed-chunk-5-2.png)

가법

``` r
#decomposition
dd=decompose(dd4.ts, type="additive")
#decomposition results
names(dd)
```

    ## [1] "x"        "seasonal" "trend"    "random"   "figure"   "type"

``` r
dd$seasonal # seaso
```

    ##            Qtr1       Qtr2       Qtr3       Qtr4
    ## 2006 -178.85156  -83.53906   97.49219  164.89844
    ## 2007 -178.85156  -83.53906   97.49219  164.89844
    ## 2008 -178.85156  -83.53906   97.49219  164.89844
    ## 2009 -178.85156  -83.53906   97.49219  164.89844
    ## 2010 -178.85156  -83.53906   97.49219  164.89844

``` r
#dd$trend
#dd$$random
#dd$trend
```

``` r
dd_noseasonal=dd4-dd$seasonal # remove seasonal effects
dd_notrend=dd4-dd$trend # remove trend effects
par(mfrow=c(1,2))
plot(dd_noseasonal, main="TS without seasonal effects")
plot(dd_notrend, main="TS without trend effects")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-8-1.png)

승법

``` r
#decomposition
dd_m=decompose(dd4.ts, type="multiplicative")
#decomposition results
names(dd_m)
```

    ## [1] "x"        "seasonal" "trend"    "random"   "figure"   "type"

``` r
dd$seasonal # seaso
```

    ##            Qtr1       Qtr2       Qtr3       Qtr4
    ## 2006 -178.85156  -83.53906   97.49219  164.89844
    ## 2007 -178.85156  -83.53906   97.49219  164.89844
    ## 2008 -178.85156  -83.53906   97.49219  164.89844
    ## 2009 -178.85156  -83.53906   97.49219  164.89844
    ## 2010 -178.85156  -83.53906   97.49219  164.89844

``` r
#dd$trend
#dd$$random
#dd$trend
```

``` r
ddM_noseasonal=dd4/dd_m$seasonal # remove seasonal effects
ddM_notrend=dd4/dd_m$trend # remove trend effects

par(mfrow=c(1,2))
plot(ddM_noseasonal, main="TS without seasonal effects")
plot(ddM_notrend, main="TS without trend effects")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-11-1.png)

실제 데이터

``` r
data("AirPassengers")
par(mfrow=c(1,2))
plot(decompose(AirPassengers, type="additive"))
```

![](../assets/images/time_series/decomposition/unnamed-chunk-12-1.png)

``` r
plot(decompose(AirPassengers, type="multiplicative"))
```

![](../assets/images/time_series/decomposition/unnamed-chunk-12-2.png)

``` r
ddd<-decompose(AirPassengers, type="multiplicative")
plot(AirPassengers/ddd$seasonal, main="Remove seasonal variation")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-13-1.png)

``` r
plot(AirPassengers/ddd$trend, main="Remove trend variation")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-13-2.png)

``` r
AirPassengers %>% decompose(type="additive") %>%
autoplot() +
ggtitle("Classical additive decomposition of airPassengers data")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-14-1.png)

``` r
AirPassengers %>% decompose(type="multiplicative") %>%
autoplot() +
ggtitle("Classical multiplicative decomposition of airPassengers data")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-15-1.png)

``` r
dda=decompose(AirPassengers, type="additive")
ddm=decompose(AirPassengers, type="multiplicative")
tseries::kpss.test(dda$random, null = "Level") #Stationary for random variation after trend and seaso
```

    ## Warning in tseries::kpss.test(dda$random, null = "Level"): p-value greater than
    ## printed p-value
    
    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  dda$random
    ## KPSS Level = 0.016442, Truncation lag parameter = 4, p-value = 0.1

``` r
tseries::kpss.test(ddm$random, null = "Level") #Stationary for random variation after trend and seasona
```

    ## Warning in tseries::kpss.test(ddm$random, null = "Level"): p-value greater than
    ## printed p-value
    
    ## 
    ##  KPSS Test for Level Stationarity
    ## 
    ## data:  ddm$random
    ## KPSS Level = 0.033058, Truncation lag parameter = 4, p-value = 0.1

``` r
tsdisplay(dda$random, main="Random/ Additive model")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-18-1.png)

``` r
tsdisplay(ddm$random, main="Random/ Multiplicative model")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-19-1.png)

seats

``` r
data(elecequip)
autoplot(elecequip)
```

![](../assets/images/time_series/decomposition/unnamed-chunk-20-1.png)

``` r
elecequip %>% seas() %>%
autoplot() +
ggtitle("SEATS decomposition of electrical equipment index")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-21-1.png)

``` r
elecequip %>% seas()->seats_elece
```

X11

``` r
autoplot(elecequip)
```

![](../assets/images/time_series/decomposition/unnamed-chunk-23-1.png)

``` r
# seasonal effect fitting
elecequip %>% seas(x11="") -> fit
#A decomposition of the new orders index for electrical equipment.
autoplot(fit) +
ggtitle("X11 decomposition of electrical equipment index")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-24-1.png)

``` r
#the trend-cycle component and the seasonally adjusted data, along with the original data.
autoplot(elecequip, series="Data") +
autolayer(trendcycle(fit), series="Trend") +
autolayer(seasadj(fit), series="Seasonally Adjusted") +
xlab("Year") + ylab("New orders index") +
ggtitle("Electrical equipment manufacturing (Euro area)") +
scale_colour_manual(values=c("gray","blue","red"),
breaks=c("Data","Seasonally Adjusted","Trend"))
```

![](../assets/images/time_series/decomposition/unnamed-chunk-25-1.png)

``` r
#Seasonal sub-series plot of the seasonal component from the X11 decomposition of the new orders index for electrical equipment.
fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-26-1.png)

``` r
#fit %>% seasonal()
#fit %>% trendcycle()
#fit %>% remainder()
#fit %>% seasadj()
```

``` r
air_stl=stl(AirPassengers,"periodic")
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
plot(air_stl)
```

![](../assets/images/time_series/decomposition/unnamed-chunk-29-1.png)

``` r
air_ss=seasadj(air_stl) #Returns seasonally adjusted data constructed by removing the seasonal component.
seasonal_index=sindexf(air_stl, h=24)
seasonal_index
```

    ##             Jan        Feb        Mar        Apr        May        Jun
    ## 1961 -25.497718 -35.220935  -3.027478  -8.299054  -5.737289  32.336634
    ## 1962 -25.497718 -35.220935  -3.027478  -8.299054  -5.737289  32.336634
    ##             Jul        Aug        Sep        Oct        Nov        Dec
    ## 1961  70.243882  68.049433  17.438326 -21.063432 -57.481851 -31.740516
    ## 1962  70.243882  68.049433  17.438326 -21.063432 -57.481851 -31.740516

``` r
autoplot(air_stl) # decomposition plot
```

![](../assets/images/time_series/decomposition/unnamed-chunk-31-1.png)

``` r
forecast_air<-forecast::forecast(air_stl)
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

![](../assets/images/time_series/decomposition/unnamed-chunk-33-1.png)

``` r
data(a10)
head(a10)
```

    ##           Jul      Aug      Sep      Oct      Nov      Dec
    ## 1991 3.526591 3.180891 3.252221 3.611003 3.565869 4.306371

``` r
#glimpse(a10)
#str(a10)
```

``` r
# ACF
grid.arrange(
  ggAcf(a10), ggAcf(a10, lag=48), ncol=2
)
```

![](../assets/images/time_series/decomposition/unnamed-chunk-35-1.png)

``` r
# PACF
grid.arrange(
  ggPacf(a10), ggPacf(a10, lag=48), ncol=2
)
```

![](../assets/images/time_series/decomposition/unnamed-chunk-36-1.png)

``` r
#Monthly sales of antidiabetic drugs in Australia.
autoplot(a10) +
ggtitle("Antidiabetic drug sales") +
ylab("$ million") +
xlab("Year")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-37-1.png)

``` r
#Seasonal plot of monthly antidiabetic drug sales in Australia.
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
ylab("$ million") +
ggtitle("Seasonal plot: antidiabetic drug sales")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-38-1.png)

``` r
#Polar seasonal plot of monthly antidiabetic drug sales in Australia.
ggseasonplot(a10, polar=TRUE) +
ylab("$ million") +
ggtitle("Polar seasonal plot: antidiabetic drug sales")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-39-1.png)

``` r
#Seasonal subseries plot of monthly antidiabetic drug sales in Australia.
ggsubseriesplot(a10) +
ylab("$ million") +
ggtitle("Seasonal subseries plot: antidiabetic drug sales")
```

![](../assets/images/time_series/decomposition/unnamed-chunk-40-1.png)







