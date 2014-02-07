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
##       a2     b2
## a1 10.30 10.957
## a2 10.56  9.938
## a3 10.42  9.622
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
## A            2   1.87   0.937    0.75   0.48
## B            1   0.48   0.479    0.38   0.54
## A:B          2   3.16   1.582    1.27   0.30
## Residuals   24  29.88   1.245
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
##   Effect DFn DFd      F      p p<.05     ges
## 1      A   2  24 0.7523 0.4821       0.05899
## 2      B   1  24 0.3844 0.5411       0.01576
## 3    A:B   2  24 1.2708 0.2988       0.09576
## 
## $`Levene's Test for Homogeneity of Variance`
##   DFn DFd   SSn   SSd      F      p p<.05
## 1   5  24 1.595 14.96 0.5117 0.7647
```



```r
detach(dat)
```


Same dataset but with A & B within subject 


```r
subject = gl(5, 1, 30, labels = paste("sub", 1:5, sep = ""))
dat$subject = subject
table(dat$subject, dat$A, dat$B)
```

```
## , ,  = a2
## 
##       
##        a1 a2 a3
##   sub1  1  1  1
##   sub2  1  1  1
##   sub3  1  1  1
##   sub4  1  1  1
##   sub5  1  1  1
## 
## , ,  = b2
## 
##       
##        a1 a2 a3
##   sub1  1  1  1
##   sub2  1  1  1
##   sub3  1  1  1
##   sub4  1  1  1
##   sub5  1  1  1
```



```r
attach(dat)
```

```
## The following object is masked _by_ .GlobalEnv:
## 
##     subject
```

```r
interaction.plot(A:B, subject, x)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
summary(aov(x ~ A * B + Error(subject/(A * B))))
```

```
## 
## Error: subject
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals  4   9.46    2.37               
## 
## Error: subject:A
##           Df Sum Sq Mean Sq F value Pr(>F)
## A          2   1.87   0.937       1   0.41
## Residuals  8   7.47   0.934               
## 
## Error: subject:B
##           Df Sum Sq Mean Sq F value Pr(>F)
## B          1   0.48   0.479    0.49   0.52
## Residuals  4   3.94   0.986               
## 
## Error: subject:A:B
##           Df Sum Sq Mean Sq F value Pr(>F)
## A:B        2   3.16    1.58    1.41    0.3
## Residuals  8   9.01    1.13
```

```r
summary(aov(x ~ A + Error(subject/A), data = dat, subset = (B == 1)))
```

```
## Error: contrasts can be applied only to factors with 2 or more levels
```

```r
summary(aov(x ~ A + Error(subject/A), data = dat, subset = (B == 2)))
```

```
## Error: contrasts can be applied only to factors with 2 or more levels
```

```r

for (a in levels(A)) {
    print(paste("Effect of B for A =", a))
    print(summary(aov(x ~ B + Error(subject/(B), data = dat, subset = (A == 
        a)))))
}
```

```
## [1] "Effect of B for A = a1"
## 
## Error: subject
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals  4   9.46    2.37               
## 
## Error: subject:B
##           Df Sum Sq Mean Sq F value Pr(>F)
## B          1   0.48   0.479    0.49   0.52
## Residuals  4   3.94   0.986               
## 
## Error: Within
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 20   21.5    1.08               
## [1] "Effect of B for A = a2"
## 
## Error: subject
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals  4   9.46    2.37               
## 
## Error: subject:B
##           Df Sum Sq Mean Sq F value Pr(>F)
## B          1   0.48   0.479    0.49   0.52
## Residuals  4   3.94   0.986               
## 
## Error: Within
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 20   21.5    1.08               
## [1] "Effect of B for A = a3"
## 
## Error: subject
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals  4   9.46    2.37               
## 
## Error: subject:B
##           Df Sum Sq Mean Sq F value Pr(>F)
## B          1   0.48   0.479    0.49   0.52
## Residuals  4   3.94   0.986               
## 
## Error: Within
##           Df Sum Sq Mean Sq F value Pr(>F)
## Residuals 20   21.5    1.08
```

```r
detach(dat)
```



```r
ezANOVA(data = dat, dv = x, wid = subject, within = .(A, B))
```

```
## $ANOVA
##   Effect DFn DFd      F      p p<.05     ges
## 2      A   2   8 1.0035 0.4085       0.05899
## 3      B   1   4 0.4854 0.5243       0.01576
## 4    A:B   2   8 1.4052 0.2999       0.09576
## 
## $`Mauchly's Test for Sphericity`
##   Effect      W      p p<.05
## 2      A 0.8902 0.8399      
## 4    A:B 0.7178 0.6081      
## 
## $`Sphericity Corrections`
##   Effect    GGe  p[GG] p[GG]<.05   HFe  p[HF] p[HF]<.05
## 2      A 0.9011 0.4038           1.595 0.4085          
## 4    A:B 0.7799 0.3028           1.188 0.2999
```



Split-plot ANOVA (A within, B between)


```r
subject = gl(10, 1, 30, labels = paste("sub", 1:10, sep = ""))
dat$subject = subject
table(dat$subject, dat$A, dat$B)
```

```
## , ,  = a2
## 
##        
##         a1 a2 a3
##   sub1   1  1  1
##   sub2   1  1  1
##   sub3   1  1  1
##   sub4   1  1  1
##   sub5   1  1  1
##   sub6   0  0  0
##   sub7   0  0  0
##   sub8   0  0  0
##   sub9   0  0  0
##   sub10  0  0  0
## 
## , ,  = b2
## 
##        
##         a1 a2 a3
##   sub1   0  0  0
##   sub2   0  0  0
##   sub3   0  0  0
##   sub4   0  0  0
##   sub5   0  0  0
##   sub6   1  1  1
##   sub7   1  1  1
##   sub8   1  1  1
##   sub9   1  1  1
##   sub10  1  1  1
```

```r
table(dat$subject, dat$B:dat$A)
```

```
##        
##         a2:a1 a2:a2 a2:a3 b2:a1 b2:a2 b2:a3
##   sub1      1     1     1     0     0     0
##   sub2      1     1     1     0     0     0
##   sub3      1     1     1     0     0     0
##   sub4      1     1     1     0     0     0
##   sub5      1     1     1     0     0     0
##   sub6      0     0     0     1     1     1
##   sub7      0     0     0     1     1     1
##   sub8      0     0     0     1     1     1
##   sub9      0     0     0     1     1     1
##   sub10     0     0     0     1     1     1
```

```r
summary(aov(x ~ A * B + Error(subject/A), data = dat))
```

```
## 
## Error: subject
##           Df Sum Sq Mean Sq F value Pr(>F)
## B          1   0.48   0.479    0.29   0.61
## Residuals  8  13.41   1.676               
## 
## Error: subject:A
##           Df Sum Sq Mean Sq F value Pr(>F)
## A          2   1.87   0.937    0.91   0.42
## A:B        2   3.16   1.582    1.54   0.25
## Residuals 16  16.48   1.030
```



```r
ezANOVA(data = dat, dv = x, wid = subject, within = .(A), between = .(B))
```

```
## $ANOVA
##   Effect DFn DFd      F      p p<.05     ges
## 2      B   1   8 0.2856 0.6076       0.01576
## 3      A   2  16 0.9096 0.4225       0.05899
## 4    B:A   2  16 1.5366 0.2452       0.09576
## 
## $`Mauchly's Test for Sphericity`
##   Effect      W      p p<.05
## 3      A 0.9753 0.9161      
## 4    B:A 0.9753 0.9161      
## 
## $`Sphericity Corrections`
##   Effect    GGe  p[GG] p[GG]<.05   HFe  p[HF] p[HF]<.05
## 3      A 0.9759 0.4207           1.287 0.4225          
## 4    B:A 0.9759 0.2458           1.287 0.2452
```

