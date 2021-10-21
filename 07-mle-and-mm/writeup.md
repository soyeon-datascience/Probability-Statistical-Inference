Modeling the unknown distribution with maximum likelihood and method of
moments
================
Soyeon Park
10/25/2020

Maximum likelihood (MLE) and method of moments (MM) are two common
methods for constructing a model.

# Assignment

In this deliverable, I am going to write a tutorial in which I will
explain to the reader how one might use MLE and MM to model (a)
Glycohemoglobin and (b) Height of adult females. The data will be from
National Health and Nutrition Examination Survey 2009-2010 (NHANES),
available from the Hmisc package. I will compare and contrast the two
methods in addition to comparing and contrasting the choice of
underlying distribution.

# Checklist

|                                      | Normal | Gamma | Weibull |
| :----------------------------------- | :----: | :---: | :-----: |
| Estimates of parameters              |        |       |         |
| Overlay estimated pdf onto histogram |        |       |         |
| Overlay estimated CDF onto eCDF      |        |       |         |
| QQ plot (sample vs estimated dist)   |        |       |         |
| Estimated Median                     |        |       |         |
| Median Samp Dist (hist)              |        |       |         |
| Range of middle 95% of Samp Dist     |        |       |         |

# Result

``` r
require(dplyr)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
require(stats4)
```

    ## Loading required package: stats4

``` r
Hmisc::getHdata(nhgh)
d1 <- nhgh %>% 
  filter(sex == "female") %>% 
  filter(age >= 18) %>% 
  select(gh, ht) %>% 
  filter(1:n()<=1000)
```

## Glycohemoglobin

### Method of Moments

1.  Estimates of parameters
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
(mm.norm.mean = mean(d1$gh))
```

    ## [1] 5.7246

``` r
(mm.norm.sd = sd(d1$gh))
```

    ## [1] 1.052246

    * Gamma Distribution

``` r
# Gamma Distribution
(mm.gam.shape = mean(d1$gh)^2 / var(d1$gh))
```

    ## [1] 29.59754

``` r
(mm.gam.scale = var(d1$gh) / mean(d1$gh))
```

    ## [1] 0.1934147

    * Weibull Distribution

``` r
# Weibull Distribution

# Weibull Distribution Mean...(1)
mean.wbll = function(lambda, k){
  lambda * gamma(1 + 1/k)
}
# Weibull Distribution Variance...(2)
var.wbll = function(lambda, k) {
  lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2)
}
# Lambda definition in Mean of Weibull Distribution...(3)
lambda = function(sample.mean, k){
  sample.mean / gamma(1 + 1/k)
}
# Combine (2) and (3)
var.wbll = function(sample.mean, k) {
  lambda(sample.mean, k)^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2)
}

# Find k value
find.var.wbll = function(sample.mean, k, sample.var) {
  lambda(sample.mean, k)^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2) - sample.var
}

x = seq(10, 100, by = 0.01)
plot(x, y = find.var.wbll(mean(d1$gh), k = x, sample.var = var(d1$gh)), xlab = "k", ylab = "function (find.var.wbll)", main = "Function for variance of Weibull Distribution")
```

![](writeup_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
mm.wbll.opt <- optimize(f = function(x) {abs(find.var.wbll(k = x, mean(d1$gh), sample.var = var(d1$gh)))}, lower = 10, upper = 100)

(mm.wbll.k = mm.wbll.opt$minimum)
```

    ## [1] 10.00008

``` r
(mm.wbll.lambda = lambda(sample.mean = mean(d1$gh), k = mm.wbll.k))
```

    ## [1] 6.017337

2.  Overlay estimated pdf onto histogram

<!-- end list -->

``` r
hist(d1$gh, main = "Glycohemoglobin of Adult Females; MM", breaks = 100, freq = FALSE, xlab = "Glycohemogolbin")
curve(dnorm(x, mm.norm.mean, mm.norm.sd), add = TRUE, col = "red", lwd = 3)
curve(dgamma(x, shape = mm.gam.shape, scale = mm.gam.scale), add = TRUE, col = "blue", lwd = 3)
curve(dweibull(x, shape = mm.wbll.k, scale = mm.wbll.lambda), add=TRUE, col = "green", lwd = 3)
legend("topleft", legend = c("Normal", "Gamma", "Weibull"), col = c("red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

3.  Overlay estimated CDF onto eCDF

<!-- end list -->

``` r
plot(ecdf(d1$gh), main = "CDF and eCDF of GH of Adult Females; MM", lwd = 3, ylab = "Probability")
curve(pnorm(x, mm.norm.mean, mm.norm.sd), add = TRUE, col = "red", lwd = 3)
curve(pgamma(x, shape = mm.gam.shape, scale = mm.gam.scale), add = TRUE, col = "blue", lwd = 3)
curve(pweibull(x, shape = mm.wbll.k, scale = mm.wbll.lambda), add=TRUE, col = "green", lwd = 3)
legend("topleft", legend = c("eCDF of GH", "Normal CDF", "Gamma CDF", "Weibull CDF"), col = c("black", "red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

4.  QQ plot (Sample vs estimated dist)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$gh, qs)
theo_qs <- qnorm(qs, mean = mm.norm.mean, sd = mm.norm.sd)

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Normal Distrbution; MM", 
     xlab = "The sample quantile", ylab = "The theoretical Normal distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$gh, qs)
theo_qs <- qgamma(qs, scale = mm.gam.scale, shape = mm.gam.shape)

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Gamma Distrbution; MM", 
     xlab = "The sample quantile", ylab = "The theoretical Gamma distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$gh, qs)
theo_qs <- qweibull(qs, shape = mm.wbll.k, scale = mm.wbll.lambda)

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Weibull Distrbution; MM", 
     xlab = "The sample quantile", ylab = "The theoretical Weibull distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

5.  Estimated Median

<!-- end list -->

``` r
# Sample Median
median(d1$gh)
```

    ## [1] 5.5

``` r
# Normal Distribution
qnorm(.5, mean = mm.norm.mean, sd = mm.norm.sd)
```

    ## [1] 5.7246

``` r
# Gamma Distribution
qgamma(.5, shape = mm.gam.shape, scale = mm.gam.scale)
```

    ## [1] 5.660259

``` r
# Weibull Distribution
qweibull(.5, shape = mm.wbll.k, scale = mm.wbll.lambda)
```

    ## [1] 5.800788

6.  Median Sample Distribution(Histogram)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
M <- 5000
N <- 1000
out <- rnorm(N * M, mean = mm.norm.mean, sd = mm.norm.sd) %>% array(dim = c(M,N))
sample_dist_norm_gh <- apply(out, 1, median)
hist(sample_dist_norm_gh, breaks = 100, main = "Histogram of GH Sample Normal Distribution; MM", xlab = "Glycohemoglobin", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
M <- 5000
N <- 1000
out <- rgamma(N * M, shape = mm.gam.shape, scale = mm.gam.scale) %>% array(dim = c(M,N))
sample_dist_gam_gh <- apply(out, 1, median)
hist(sample_dist_gam_gh, breaks = 100, main = "Histogram of GH Sample Gamma Distribution; MM", xlab = "Glycohemoglobin", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
M <- 5000
N <- 1000
out <- rweibull(N * M, shape = mm.wbll.k, scale = mm.wbll.lambda) %>% array(dim = c(M,N))
sample_dist_wbll_gh <- apply(out, 1, median)
hist(sample_dist_wbll_gh, breaks = 100, main = "Histogram of GH Sample Weibull Distribution; MM", xlab = "Glycohemoglobin", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

7.  Range of middle 95% of Sample Distribution

<!-- end list -->

``` r
# Normal Distribution
quantile(sample_dist_norm_gh, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 5.642816 5.806302

``` r
# Gamma Distribution
quantile(sample_dist_gam_gh, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 5.578119 5.740450

``` r
# Weibull Distribution
quantile(sample_dist_wbll_gh, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 5.748654 5.853705

Normal distribution : (5.65, 5.81) Gamma distribution : (5.58, 5.74)
Weibull distribution : (5.75, 5.85)

### Maximum Likelihood Estimation

1.  Estimate of parameters
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
normal.ll <- function(mean, sd){
  fs <- dnorm(x = d1$gh, mean = mean, sd = sd, log = TRUE)
  -sum(fs)
}

fit_norm <- mle(
  normal.ll,
  start = list(mean = 160, sd = 5),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)
par(mfrow = c(1,2)); plot(profile(fit_norm), absVal = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
coef(fit_norm)
```

    ##     mean       sd 
    ## 5.724600 1.051721

    * Gamma Distribution

``` r
# Gamma Distribution
gamma.ll <- function(shape, scale){
  fs <- dgamma(x = d1$gh, shape = shape, scale = scale, log = TRUE)
  -sum(fs)
}

fit_gamma <- mle(
  gamma.ll,
  start = list(shape = 482, scale = 0.33),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)
par(mfrow = c(1,2)); plot(profile(fit_gamma), absVal = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
coef(fit_gamma)
```

    ##      shape      scale 
    ## 40.7113688  0.1406192

    * Weibull Distribution

``` r
# Weibull Distribution
wbll.ll <- function(shape, scale){
  fs <- dweibull(x = d1$gh, shape = shape, scale = scale, log = TRUE)
  -sum(fs)
}

fit_wbll <- mle(
  wbll.ll,
  start = list(shape = 10, scale = 6),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)
par(mfrow = c(1,2)); plot(profile(fit_wbll), absVal = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
coef(fit_wbll)
```

    ##    shape    scale 
    ## 4.125255 6.173885

2.  Overlay estimated pdf onto histogram

<!-- end list -->

``` r
hist(d1$gh, breaks = 100, freq = FALSE, main = "GH of Adult Females; MLE", xlab = "GH")
curve(dnorm(x, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]), col = "red", lwd = 3, add = TRUE)
curve(dgamma(x, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2]), col = "blue", lwd = 3, add = TRUE)
curve(dweibull(x, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2]), col = "green", lwd = 3, add = TRUE)
legend("topleft", legend = c("Normal", "Gamma", "Weibull"), col = c("red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

3.  Overlay estimated CDF onto eCDF

<!-- end list -->

``` r
plot(ecdf(d1$gh), main = "CDF and eCDF of GH of Adult Females; MLE", lwd = 3)
curve(pnorm(x, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]), col = "red", lwd = 3, add = TRUE)
curve(pgamma(x, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2]), add = TRUE, col = "blue", lwd = 3)
curve(pweibull(x, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2]), add=TRUE, col = "green", lwd = 3)
legend("topleft", legend = c("eCDF of Height", "Normal CDF", "Gamma CDF", "Weibull CDF"), col = c("black", "red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

4.  QQ plot(Sample vs estimated distribution)

<!-- end list -->

``` r
# Normal Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$gh, qs)
theo_qs <- qnorm(qs, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Normal Distrbution; MLE", 
     xlab = "The sample quantile", ylab = "The theoretical Normal distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# Gamma Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$gh, qs)
theo_qs <- qgamma(qs, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2])

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Gamma Distrbution; MLE", 
     xlab = "The sample quantile", ylab = "The theoretical Gamma distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# Weibull Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$gh, qs)
theo_qs <- qweibull(qs, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2])

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Weibull Distrbution; MLE", 
     xlab = "The sample quantile", ylab = "The theoretical Weibull distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

5.  Estimated Median

<!-- end list -->

``` r
# Sample Median
median(d1$gh)
```

    ## [1] 5.5

``` r
# Normal Distribution
qnorm(.5, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
```

    ## [1] 5.7246

``` r
# Gamma Distribution
qgamma(.5, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2])
```

    ## [1] 5.677994

``` r
# Weibull Distribution
qweibull(.5, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2])
```

    ## [1] 5.64902

6.  Median Sample Distribution (Histogram)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
M <- 5000
N <- 1000
out <- rnorm(N*M, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]) %>% array(dim = c(M,N))
sample_normal_dist_mle <- apply(out, 1, median)
hist(sample_normal_dist_mle, breaks = 100, main = "Histogram of GH Sample Normal Distribution; MLE", xlab = "Glycohemoglobin", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
M <- 5000
N <- 1000
out <- rgamma(N*M, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2]) %>% array(dim = c(M,N))
sample_gamma_dist_mle <- apply(out, 1, median)
hist(sample_gamma_dist_mle, breaks = 100, main = "Histogram of GH Sample Gamma Distribution; MLE", xlab = "Glycohemoglobin", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
M <- 5000
N <- 1000
out <- rweibull(N*M, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2]) %>% array(dim = c(M,N))
sample_wbll_dist_mle <- apply(out, 1, median)
hist(sample_wbll_dist_mle, breaks = 100, main = "Histogram of GH Sample Weibull Distribution; MLE", xlab = "Glycohemoglobin", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

7.  Range of middle 95% of Sample Distribution

<!-- end list -->

``` r
# Normal Distribution
quantile(sample_normal_dist_mle, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 5.642533 5.805520

``` r
# Gamma Distribution
quantile(sample_gamma_dist_mle, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 5.609476 5.749121

``` r
# Weibull Distribution
quantile(sample_wbll_dist_mle, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 5.525813 5.768710

Normal distribution : (5.64, 5.81) Gamma distribution : (5.61, 5.75)
Weibull distribution : (5.52, 5.78)

## Height

### Method of Moments

1.  Estimates of parameters
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
(mm.norm.mean = mean(d1$ht))
```

    ## [1] 160.7419

``` r
(mm.norm.sd = sd(d1$ht))
```

    ## [1] 7.320161

    * Gamma Distribution

``` r
# Gamma Distribution
(mm.gam.shape = mean(d1$ht)^2 / var(d1$ht))
```

    ## [1] 482.1886

``` r
(mm.gam.scale = var(d1$ht) / mean(d1$ht))
```

    ## [1] 0.333359

    * Weibull Distribution

``` r
# Weibull Distribution

# Weibull Distribution Mean...(1)
mean.wbll = function(lambda, k){
  lambda * gamma(1 + 1/k)
}
# Weibull Distribution Variance...(2)
var.wbll = function(lambda, k) {
  lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2)
}
# Lambda definition in Mean of Weibull Distribution...(3)
lambda = function(sample.mean, k){
  sample.mean / gamma(1 + 1/k)
}
# Combine (2) and (3)
var.wbll = function(sample.mean, k) {
  lambda(sample.mean, k)^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2)
}

# Find k value
find.var.wbll = function(sample.mean, k, sample.var) {
  lambda(sample.mean, k)^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2) - sample.var
}

x = seq(10, 100, by = 0.01)
plot(x, y = find.var.wbll(mean(d1$ht), k = x, sample.var = var(d1$ht)), main = "Function for variance of Weibull Distribution", xlab = "k", ylab = "Function(find.var.wbll)")
```

![](writeup_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
mm.wbll.opt <- optimize(f = function(x) {abs(find.var.wbll(k = x, mean(d1$ht), sample.var = var(d1$ht)))}, lower = 10, upper = 100)

(mm.wbll.k = mm.wbll.opt$minimum)
```

    ## [1] 27.45942

``` r
(mm.wbll.lambda = lambda(sample.mean = mean(d1$ht), k = mm.wbll.k))
```

    ## [1] 163.9807

2.  Overlay estimated pdf onto histogram

<!-- end list -->

``` r
hist(d1$ht, main = "Height of Adult Females; MM", breaks = 100, freq = FALSE, xlab = "Height(cm)")
curve(dnorm(x, mm.norm.mean, mm.norm.sd), add = TRUE, col = "red", lwd = 3)
curve(dgamma(x, shape = mm.gam.shape, scale = mm.gam.scale), add = TRUE, col = "blue", lwd = 3)
curve(dweibull(x, shape = mm.wbll.k, scale = mm.wbll.lambda), add=TRUE, col = "green", lwd = 3)
legend("topleft", legend = c("Normal", "Gamma", "Weibull"), col = c("red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

3.  Overlay estimated CDF onto eCDF

<!-- end list -->

``` r
plot(ecdf(d1$ht), main = "CDF and eCDF of Height of Adult Females; MM", lwd = 1)
curve(pnorm(x, mm.norm.mean, mm.norm.sd), add = TRUE, col = "red", lwd = 3)
curve(pgamma(x, shape = mm.gam.shape, scale = mm.gam.scale), add = TRUE, col = "blue", lwd = 3)
curve(pweibull(x, shape = mm.wbll.k, scale = mm.wbll.lambda), add=TRUE, col = "green", lwd = 3)
legend("topleft", legend = c("eCDF of Height", "Normal CDF", "Gamma CDF", "Weibull CDF"), col = c("black", "red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

4.  QQ plot (Sample vs estimated dist)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$ht, qs)
theo_qs <- qnorm(qs, mean = mm.norm.mean, sd = mm.norm.sd)

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Normal Distrbution; MM", 
     xlab = "The sample quantile", ylab = "The theoretical Normal distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$ht, qs)
theo_qs <- qgamma(qs, scale = mm.gam.scale, shape = mm.gam.shape)

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Gamma Distrbution; MM", 
     xlab = "The sample quantile", ylab = "The theoretical Gamma distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$ht, qs)
theo_qs <- qweibull(qs, shape = mm.wbll.k, scale = mm.wbll.lambda)

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Weibull Distrbution; MM", 
     xlab = "The sample quantile", ylab = "The theoretical Weibull distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

5.  Estimated Median

<!-- end list -->

``` r
# Sample Median
median(d1$ht)
```

    ## [1] 160.8

``` r
# Normal Distribution
qnorm(.5, mean = mm.norm.mean, sd = mm.norm.sd)
```

    ## [1] 160.7419

``` r
# Gamma Distribution
qgamma(.5, shape = mm.gam.shape, scale = mm.gam.scale)
```

    ## [1] 160.6308

``` r
# Weibull Distribution
qweibull(.5, shape = mm.wbll.k, scale = mm.wbll.lambda)
```

    ## [1] 161.8065

Normal distribution : 160.74 Gamma distribution : 160.63 Weibull
distribution : 161.81

6.  Median Sample Distribution(Histogram)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
M <- 5000
N <- 1000
out <- rnorm(N * M, mean = mm.norm.mean, sd = mm.norm.sd) %>% array(dim = c(M,N))
sample_dist_norm_ht <- apply(out, 1, median)
hist(sample_dist_norm_ht, breaks = 100, main = "Histogram of Hegiht Sample Normal Distribution; MM", xlab = "Height(cm)", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
M <- 5000
N <- 1000
out <- rgamma(N * M, shape = mm.gam.shape, scale = mm.gam.scale) %>% array(dim = c(M,N))
sample_dist_gam_ht <- apply(out, 1, median)
hist(sample_dist_gam_ht, breaks = 100, main = "Histogram of Hegiht Sample Gamma Distribution; MM", xlab = "Height(cm)", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
M <- 5000
N <- 1000
out <- rweibull(N * M, shape = mm.wbll.k, scale = mm.wbll.lambda) %>% array(dim = c(M,N))
sample_dist_wbll_ht <- apply(out, 1, median)
hist(sample_dist_wbll_ht, breaks = 100, main = "Histogram of Hegiht Sample Weibull Distribution; MM", xlab = "Height(cm)", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

7.  Range of middle 95% of Sample Distribution

<!-- end list -->

``` r
# Normal Distribution
quantile(sample_dist_norm_ht, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 160.1705 161.2982

``` r
# Gamma Distribution
quantile(sample_dist_gam_ht, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 160.0607 161.2026

``` r
# Weibull Distribution
quantile(sample_dist_wbll_ht, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 161.2688 162.3137

Normal distribution : (161.2827, 162.3287) Gamma distribution :
(160.0423, 161.1965) Weibull distribution : (161.2771, 162.3309)

### Maximum Likelihood Estimation

1.  Estimate of parameters
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
normal.ll <- function(mean, sd){
  fs <- dnorm(x = d1$ht, mean = mean, sd = sd, log = TRUE)
  -sum(fs)
}

fit_norm <- mle(
  normal.ll,
  start = list(mean = 160, sd = 5),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)
par(mfrow = c(1,2)); plot(profile(fit_norm), absVal = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
coef(fit_norm)
```

    ##       mean         sd 
    ## 160.741906   7.316505

    * Gamma Distribution

``` r
# Gamma Distribution
gamma.ll <- function(shape, scale){
  fs <- dgamma(x = d1$ht, shape = shape, scale = scale, log = TRUE)
  -sum(fs)
}

fit_gamma <- mle(
  gamma.ll,
  start = list(shape = 482, scale = 0.33),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)
par(mfrow = c(1,2)); plot(profile(fit_gamma), absVal = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
coef(fit_gamma)
```

    ##       shape       scale 
    ## 482.0000024   0.3334906

    * Weibull Distribution

``` r
# Weibull Distribution
wbll.ll <- function(shape, scale){
  fs <- dweibull(x = d1$ht, shape = shape, scale = scale, log = TRUE)
  -sum(fs)
}

fit_wbll <- mle(
  wbll.ll,
  start = list(shape = 27, scale = 164),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)
par(mfrow = c(1,2)); plot(profile(fit_wbll), absVal = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
coef(fit_wbll)
```

    ##     shape     scale 
    ##  21.85395 164.24719

2.  Overlay estimated pdf onto histogram

<!-- end list -->

``` r
hist(d1$ht, breaks = 100, freq = FALSE, main = "Height of Adult Females; MLE", xlab = "Height(cm)")
curve(dnorm(x, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]), col = "red", lwd = 3, add = TRUE)
curve(dgamma(x, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2]), col = "blue", lwd = 3, add = TRUE)
curve(dweibull(x, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2]), col = "green", lwd = 3, add = TRUE)
legend("topleft", legend = c("Normal", "Gamma", "Weibull"), col = c("red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

3.  Overlay estimated CDF onto eCDF

<!-- end list -->

``` r
plot(ecdf(d1$ht), main = "CDF and eCDF of Height of Adult Females; MLE", lwd = 3)
curve(pnorm(x, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]), col = "red", lwd = 3, add = TRUE)
curve(pgamma(x, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2]), add = TRUE, col = "blue", lwd = 3)
curve(pweibull(x, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2]), add=TRUE, col = "green", lwd = 3)
legend("topleft", legend = c("eCDF of Height", "Normal CDF", "Gamma CDF", "Weibull CDF"), col = c("black", "red", "blue", "green"), lty = 1:1, cex = 0.8)
```

![](writeup_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

4.  QQ plot(Sample vs estimated distribution)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$ht, qs)
theo_qs <- qnorm(qs, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Normal Distrbution; MLE", 
     xlab = "The sample quantile", ylab = "The theoretical Normal distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$ht, qs)
theo_qs <- qgamma(qs, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2])

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Gamma Distrbution; MLE", 
     xlab = "The sample quantile", ylab = "The theoretical Gamma distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
qs <- seq(0.05, 0.95, length = 50)

sample_qs <- quantile(d1$ht, qs)
theo_qs <- qweibull(qs, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2])

plot(sample_qs, theo_qs, pch = 16, main = "QQ plot for sample distribution and estimated Weibull Distrbution; MLE", 
     xlab = "The sample quantile", ylab = "The theoretical Weibull distribution quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

5.  Estimated Median

<!-- end list -->

``` r
# Sample Median
median(d1$ht)
```

    ## [1] 160.8

``` r
# Normal Distribution
qnorm(.5, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
```

    ## [1] 160.7419

``` r
# Gamma Distribution
qgamma(.5, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2])
```

    ## [1] 160.6313

``` r
# Weibull Distribution
qweibull(.5, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2])
```

    ## [1] 161.5156

Normal distribution : 160.74 Gamma distribution : 160.63 Weibull
distribution : 161.51

6.  Median Sample Distribution (Histogram)
    
      - Normal Distribution

<!-- end list -->

``` r
# Normal Distribution
M <- 5000
N <- 1000
out <- rnorm(N*M, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]) %>% array(dim = c(M,N))
sample_normal_dist_mle <- apply(out, 1, median)
hist(sample_normal_dist_mle, breaks = 100, main = "Histogram of Hegiht Sample Normal Distribution; MLE", xlab = "Height(cm)", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

    * Gamma Distribution

``` r
# Gamma Distribution
M <- 5000
N <- 1000
out <- rgamma(N*M, shape = coef(fit_gamma)[1], scale = coef(fit_gamma)[2]) %>% array(dim = c(M,N))
sample_gamma_dist_mle <- apply(out, 1, median)
hist(sample_gamma_dist_mle, breaks = 100, main = "Histogram of Hegiht Sample Gamma Distribution; MLE", xlab = "Height(cm)", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

    * Weibull Distribution

``` r
# Weibull Distribution
M <- 5000
N <- 1000
out <- rweibull(N*M, shape = coef(fit_wbll)[1], scale = coef(fit_wbll)[2]) %>% array(dim = c(M,N))
sample_wbll_dist_mle <- apply(out, 1, median)
hist(sample_wbll_dist_mle, breaks = 100, main = "Histogram of Hegiht Sample Weibull Distribution; MLE", xlab = "Height(cm)", freq = FALSE)
```

![](writeup_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

7.  Range of middle 95% of Sample Distribution

<!-- end list -->

``` r
# Normal Distribution
quantile(sample_normal_dist_mle, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 160.1713 161.3105

``` r
# Gamma Distribution
quantile(sample_gamma_dist_mle, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 160.0808 161.1793

``` r
# Weibull Distribution
quantile(sample_wbll_dist_mle, c(0.05/2, 1 - 0.05/2))
```

    ##     2.5%    97.5% 
    ## 160.8439 162.1697

Normal distribution : (160.18, 161.31) Gamma distribution : (160.06,
161.22) Weibull distribution : (160.83, 162.18)

# Conclusion

The distribution of female adults’ Glycohemoglobin does not fit in
either normal, gamma, or Weibull distribution. The estimated PDF, CDF,
and QQ plot does not match with any those theoretical distributions. On
the other hand, the distribution of female adults’ height fits well in
normal and gamma distribution. Since PDF, CDF, and QQ plot of normal and
gamma distribution look almost same with the distribution of the height
data, it is difficult to say which distribution match with the data
better. However, it is obvious that it does not fit in weibull
distribution.

There is difference in values of estimates of parameters between when we
use MM and MLE. However, their difference is not that big. For example,
let’s say that the height data is following gamma distribution. The
model’s shape is 482.1886, and scale is 0.333359 with MM method, and,
with MLE, the shape is 482.0000024 and scale is 0.3334906. Their values
are not same but their difference is small. Interestingly, when we
assume data is normally distributed, no matter which method you use,
mean and median from MM and MLE are same.
