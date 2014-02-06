Factorial ANOVAs
================


```r
rm(list = ls())
require(ez)
```

```
## Loading required package: ez
```


Generation of a dataset S<A3*B2>


```r
subject = factor(paste("sub", 1:30, sep = ""))
A = gl(3, 10, 30, labels = c("a1", "a2", "a3"))
B = gl(2, 5, 30, labels = c("a2", "b2"))
x = rnorm(30, mean = 10) + 1 * (A == "a1" & B == "b2")
dat = data.frame(subject, A, B, x)
```



```r
rm(subject, A, B, x)
attach(dat)
```


Classical R approach


```r
table(A, B)
```

```
##     B
## A    a2 b2
##   a1  5  5
##   a2  5  5
##   a3  5  5
```

```r
tapply(x, list(A, B), mean)
```

```
##        a2    b2
## a1 10.257 10.31
## a2 10.153 10.27
## a3  9.918 10.21
```

```r
interaction.plot(A, B, x)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
summary(aov(x ~ A * B, data = dat))
```

```
##             Df Sum Sq Mean Sq F value Pr(>F)
## A            2   0.25   0.125    0.11   0.89
## B            1   0.17   0.174    0.16   0.69
## A:B          2   0.08   0.039    0.04   0.97
## Residuals   24  26.48   1.103
```


Using ez


```r
### We should be able to do a nicer interaction plot, but I do not manage yet
### :( ezPlot(data=dat, dv=.(x), wid=.(subject), between=.(A,B),x=.(B)) TODO:
### Check with example(ezPlot)

ezANOVA(data = dat, dv = x, wid = subject, between = c("A", "B"))
```

```
## $ANOVA
##   Effect DFn DFd      F      p p<.05      ges
## 1      A   2  24 0.1131 0.8935       0.009338
## 2      B   1  24 0.1575 0.6950       0.006520
## 3    A:B   2  24 0.0350 0.9657       0.002909
## 
## $`Levene's Test for Homogeneity of Variance`
##   DFn DFd   SSn   SSd      F      p p<.05
## 1   5  24 1.811 15.43 0.5635 0.7269
```



```r
detach(dat)
```


Same dataset but with A & B within subject 



