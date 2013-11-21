Comparing two groups (continous dependent variable)
===================================================

christophe@pallier.org







```r
g1 <- 500 + rnorm(30, sd = 40)
g2 <- 520 + rnorm(20, sd = 30)
write(g1, "group1.dat")
write(g2, "group2.dat")
rm(g1, g2)
```


Data for this example are in two text files `group1.dat` and `group2.dat`. 


```r
g1 <- scan("group1.dat")
g2 <- scan("group2.dat")
```



We arrange them into a data frame with two columns: `group` (a factor with two modalities: `Gr1` and `Gr2`), and `y` which contains the values themselves.


```r
tg <- data.frame(group = factor(rep(c("Gr1", "Gr2"), c(length(g1), length(g2)))), 
    y = c(g1, g2))

head(tg)
```

```
##   group     y
## 1   Gr1 431.0
## 2   Gr1 513.1
## 3   Gr1 539.8
## 4   Gr1 465.9
## 5   Gr1 554.8
## 6   Gr1 483.7
```

```r
str(tg)
```

```
## 'data.frame':	50 obs. of  2 variables:
##  $ group: Factor w/ 2 levels "Gr1","Gr2": 1 1 1 1 1 1 1 1 1 1 ...
##  $ y    : num  431 513 540 466 555 ...
```

```r
table(tg$group)
```

```
## 
## Gr1 Gr2 
##  30  20
```


### Graphical explorations


```r
hist(tg$y)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



```r
boxplot(tg$y ~ tg$group)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


When the samples are small, stripchart may be the best:


```r
stripchart(tg$y ~ tg$group, vertical = TRUE, pch = 1)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


If the samples are large enough, you can create density plots:


```r
par(mfrow = (c(2, 1)))
xsca <- range(tg$y)
for (gr in levels(tg$group)) {
    with(subset(tg, group == gr), {
        plot(density(y), xlim = xsca, main = gr, bty = "l")
        rug(y, ticksize = 0.1)
    })
}
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


Obtain the basic descriptive stats


```r
attach(tg)
signif(tapply(y, group, mean), 3)
```

```
## Gr1 Gr2 
## 510 526
```

```r
signif(tapply(y, group, median), 3)
```

```
## Gr1 Gr2 
## 507 515
```

```r
signif(tapply(y, group, sd), 3)
```

```
##  Gr1  Gr2 
## 33.0 31.2
```

```r
signif(tapply(y, group, se), 3)
```

```
##  Gr1  Gr2 
## 6.03 6.98
```

```r
detach(tg)
```


### Inferential statistics


Student T-tests. First assuming equal variance, then relaxing this assumption

```r
t.test(y ~ group, data = tg, var.equal = TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  y by group
## t = -1.753, df = 48, p-value = 0.08597
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -35.123   2.403
## sample estimates:
## mean in group Gr1 mean in group Gr2 
##             509.5             525.9
```

```r
t.test(y ~ group, data = tg)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  y by group
## t = -1.773, df = 42.45, p-value = 0.08334
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -34.972   2.252
## sample estimates:
## mean in group Gr1 mean in group Gr2 
##             509.5             525.9
```


Somewhat more information can be obtained by fitting linear models. 

First with a parametrisation (`contr.treatment`) of group where the intercept will correspond to the mean of group 1 and the effect will estimate the difference between the two groups. 


```r
contrasts(tg$group) <- contr.treatment
contrasts(tg$group)
```

```
##     2
## Gr1 0
## Gr2 1
```

```r
summary(lm(y ~ group, data = tg))
```

```
## 
## Call:
## lm(formula = y ~ group, data = tg)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -78.52 -21.91  -4.42  22.96  64.88 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   509.52       5.90   86.33   <2e-16 ***
## group2         16.36       9.33    1.75    0.086 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.3 on 48 degrees of freedom
## Multiple R-squared:  0.0602,	Adjusted R-squared:  0.0406 
## F-statistic: 3.07 on 1 and 48 DF,  p-value: 0.086
```


Alternatively, one can prefer a parametrisation where the intercept estimates the global mean and the first parameter is the deviation from the global mean.


```r
contrasts(tg$group) <- contr.sum
contrasts(tg$group)
```

```
##     [,1]
## Gr1    1
## Gr2   -1
```

```r
summary(lm(y ~ group, data = tg))
```

```
## 
## Call:
## lm(formula = y ~ group, data = tg)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -78.52 -21.91  -4.42  22.96  64.88 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   517.70       4.67  110.95   <2e-16 ***
## group1         -8.18       4.67   -1.75    0.086 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32.3 on 48 degrees of freedom
## Multiple R-squared:  0.0602,	Adjusted R-squared:  0.0406 
## F-statistic: 3.07 on 1 and 48 DF,  p-value: 0.086
```


### Barplot with standard errors

Barplot with the means and their associated standard errors (note this is not the standard error for the difference between the groups' means, which is roughly $\sqrt{2}$ larger and, maybe for this reason, rarely used in psychology papers (like they rarely report confidence intervals))


```r
attach(tg)
par(mfrow = c(1, 1))
means <- tapply(y, group, mean)
ses <- tapply(y, group, se)

ysca = c(min(means - 3 * ses), max(means + 3 * ses))

mp <- barplot(means, ylim = ysca, xpd = F)
arrows(mp, means - ses, mp, means + ses, code = 3, angle = 90)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r

detach(tg)
```


A much nicer plot can be constructed, with confidence intervals for the means and for their difference (Cumming, Geoff, and Sue Finch. 2005. “Inference by Eye: Confidence Intervals and How to Read Pictures of Data.” American Psychologist 60 (2): 170–180.)



```r
attach(tg)
m1 <- t.test(y[group == "Gr1"])$conf.int
m2 <- t.test(y[group == "Gr2"])$conf.int
di <- diff(t.test(y ~ group)$conf.int)
ysca <- c(min(c(m1, m2) - 0.3 * diff(range(c(m1, m2)))), max(c(m1, m2) + 0.3 * 
    diff(range(c(m1, m2)))))

plot(c(Gr1 = 1, Gr2 = 2, difference = 3), c(mean(m1), mean(m2), mean(m2)), pch = c(16, 
    16, 17), ylim = ysca, xlim = c(0.5, 3.5), axes = F, xlab = "", ylab = "")
axis(2, las = 1)
axis(1, at = 1:3, labels = c("Gr1", "Gr2", "difference"))
arrows(1:3, c(m1[1], m2[1], mean(m2) - di/2), 1:3, c(m1[2], m2[2], mean(m2) + 
    di/2), code = 3, angle = 90)
abline(h = mean(m1), lty = 2)
abline(h = mean(m2), lty = 2)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r

detach(tg)
```




