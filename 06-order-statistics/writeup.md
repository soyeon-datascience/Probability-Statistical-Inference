writeup
================
Soyeon Park
10/17/2020

# Which quantiles of a continuous distribution can one estimate with more precision?

The median is an important quantity in data analysis. It represents the
middle value of the data distribution. Estimates of the median, however,
have a degree of uncertainty because (a) the estimates are calculated
from a finite sample and (b) the data distribution of the underlying
data is generally unknown. One important roles of a data scientist is to
quantify and to communicate the degree of uncertainty in his or her data
analysis.

## Assignment

In this assignment, you will write a blog post to answer a series of
questions related to the variation of the median (and a range of other
quantiles). You will use analytic methods to answer the questions.

The audience of your blog post is a Senior Data Scientist who you hope
to work with in the future. Your goal is to document how you wrote
flexible functions for the distribution of order statistics.

## Backgrounds

## What is order statistics?

If *X*<sub>*1*</sub>, *X*<sub>*2*</sub>, …, *X*<sub>*n*</sub> are
observations of a random sample of size from a continuous distribution,
we let the random variables:

*Y*<sub>*1*</sub> \< *Y*<sub>*2*</sub> \< … \< *Y*<sub>*n*

denote the order statistics of the sample, with:

  - *Y*<sub>*1*</sub> being the smallest of the *X*<sub>*1*</sub>,
    *X*<sub>*2*</sub>, …, *X*<sub>*n*</sub> observations
  - *Y*<sub>*2*</sub> being the second smallest of the
    *X*<sub>*1*</sub>, *X*<sub>*2*</sub>, …, *X*<sub>*n*</sub>
    observations
  - ….
  - *Y*<sub>*n-1* being the next-to-largest of the *X*<sub>*1*</sub>,
    *X*<sub>*2*</sub>, …, *X*<sub>*n*</sub> observations
  - *Y*<sub>*n* being the largest of the *X*<sub>*1*</sub>,
    *X*<sub>*2*</sub>, …, *X*<sub>*n*</sub> observations

## Questions

**Q:** Begin with the median from a sample of *N* = 200 from the
standard normal distribution. Write an R function that is the density
function for the median in this sample. Note that the 100th order
statistic is approximately the median, and use the order statistic
formula discussed in class. Generate a plot of the function.

``` r
dorder <- function(x){
  100*choose(200,100)*
    (pnorm(x))^(100-1)*
    (1-pnorm(x))^(200-100)*
    dnorm(x)
}

x = seq(-1, 1, by = 0.001)
plot(x, dorder(x), type = "l", main = "PDF of Median Order Statistic of Standard Normal Density", ylab = "Density Probability", xlab = "X(100)")
```

![](writeup_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

**Q:** Write an R function that is the probability function for the
median in this sample. Use the order statistic formula discussed in
class. Generate a plot of the function.

``` r
porder <- function(x) {
  pbinom(100-1, 200, pnorm(x, 0, 1), lower.tail = FALSE)
}

x = seq(-1, 1, by = 0.001)
plot(x, porder(x), type = "l", main = "CDF of Median Order Statistic of Standard Normal Density", xlab = "X(100)", ylab = "Probability")
```

![](writeup_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

**Q:** Write an R function that is the quantile function for the median
in this sample. (You have several options for how to write this
function.) Generate a plot of the function.

``` r
# qorder is inverse of porder
qorder <- function(p) {
  out <- p
  for(i in seq_along(p)) {
    out[i] <- uniroot(function(x){porder(x) - p[i]}, c(-100, 100))$root
  }
  out
}

p <- seq(0.05, 0.95, by = 0.01)
plot(p, qorder(p), type = "l", main = "Quantile function for the Median of Normal Distribution", xlab = parse(text = "X(100)"), ylab = "")
```

![](writeup_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

**Q:** Simulate the sampling distribution for the median. Create a plot
of the empirical CDF (ECDF). Overlay the plot of the ECDF with a plot of
the CDF.

``` r
sample <- NA
for(i in 1:1000){
  data = rnorm(200)
  sample[i] = median(data)
}

plot(ecdf(sample), main = "ECDF and CDF of the Median Order Statistics of Normal Distribution", xlab = parse(text = "X(100)"), ylab = "Probability")
curve(porder(x), add = TRUE, col = "red")
legend("topleft", c("ECDF", "CDF"), col = c(1,2), lwd = 3, bty = "n")
```

![](writeup_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Q:** Using the simulated sampling distribution from the previous
question, create a histogram (on the density scale). Overlay the
histogram with a plot of the density function.

``` r
hist(sample, breaks = 100, freq = FALSE)
curve(dorder(x), add = TRUE, lwd = 3, col = "blue")
```

![](writeup_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Q:** One very common way to compare a random sample to a theoretical
candidate distribution is the QQ plot. It is created by ploting
quantiles of the theoretical distribution on the x-axis and empirical
quantiles from the sample on the y-axis.

If sample and theoretical quantiles come from the same distribution,
then the plotted points will fall along the line *y* = *x*,
approximately. Here are two examples when the sample and theoretical
quantiles came from the same distribution.

``` r
random_sample <- rexp(200)
q_candidate <- qexp

x <- q_candidate((1:200)/200)
y <- quantile(random_sample, probs = (1:200)/200)

plot(x,y, asp = 1)
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
random_sample <- rnorm(200)
q_candidate <- qnorm

x <- q_candidate((1:200)/200)
y <- quantile(random_sample, probs = (1:200)/200)

plot(x,y, asp = 1, xlab = "Theoretical quantile", ylab = "Sample quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Here is an example when the sample distribution does not match with the
theoretical distribution. The sample distribution is *t*<sub>3</sub>
where as the theoretical distribution is *N*(0, 1). Notice the deviation
from *y* = *x*.

``` r
random_sample <- rt(200, df = 3)
q_candidate <- qnorm

x <- q_candidate((1:200)/200)
y <- quantile(random_sample, probs = (1:200)/200)

plot(x,y, asp = 1, xlab = "Theoretical quantile", ylab = "Sample quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

For the assignment, generate a QQ plot for the simulated data of the
median relative to the known sampling distribution of the median.

Does the simulated data agree with the theoretical sampling
distribution?

``` r
# Sampling Distribution by Simulation
N <- 200
M <- 1000
medians <- rep(NA, M)
for(i in 1:M){
  medians[i] <- median(rnorm(N))
}

# The theoretical largest order statistic distribution
porder <- function(x) {
  pbinom(100-1, 200, pnorm(x, 0, 1), lower.tail = FALSE)
}

qorder <- function(p){
  out <- p
  for(i in seq_along(p)) {
    out[i] <- uniroot(function(x){porder(x) - p[i]}, c(-100, 100))$root
  }
  out
}

# QQ plot for simulated data from the sampling distribution of the sample max and the theoretical largest order statistic distribution.
quants_sample <- quantile(medians, seq(0.01, 0.99, by = 0.01))
quants_analytic <- qorder(seq(0.01, 0.99, by = 0.01))
plot(quants_analytic, quants_sample, main = "QQ plot for the sample median and the theoretical median order statistic", xlab = "The theoretical median order statistic quantile", ylab = "The sample median quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The simulated data agrees with the theoretical sampling distribution.

**Q:** Modify the `dorder`, `porder`, and `qorder` functions so that the
functions take a new parameter `k` (for the *k*<sup>\*t\*\*h\*</sup>
order statistic) so that the functions will work for any order statistic
and not just the median.

``` r
dorder <- function(x, n, k){
  k*choose(n,k)*
    (pnorm(x))^(k-1)*
    (1-pnorm(x))^(n-k)*
    dnorm(x)
}
```

``` r
porder <- function(x, n, k) {
  pbinom(k-1, n, pnorm(x, 0, 1), lower.tail = FALSE)
}
```

``` r
qorder <- function(p, n, k){
  out <- p
  for(i in seq_along(p)) {
    out[i] <- uniroot(function(x){porder(x, n, k) - p[i]}, c(-100, 100))$root
  }
  out
}
```

**Q:** Generate the QQ plot for simulated data from the sampling
distribution of the sample max and the theoretical largest order
statistic distribution.

``` r
sample <- NA
for(i in 1:200) {
  sample[i] = max(rnorm(200))
}

# QQ plot for simulated data from the sampling distribution of the sample max and the theoretical largest order statistic distribution.
x <- qorder(seq(0.01, 0.99, by = 0.01), n = 200, k = 200)
y <- quantile(sample, probs = seq(0.01, 0.99, by = 0.01))
plot(x, y, asp = 1, main = "QQ plot for the sample max and the theoretical largest order statistic", xlab = "The theoretical largest order statistic quantile", ylab = "The sample max quantile")
abline(0,1)
```

![](writeup_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

**Q:** Modify the `dorder`, `porder`, and `qorder` functions so that the
functions take new parameters `dist` and `...` so that the functions
will work for any continuous distribution that has `d` and `p` functions
defined in R.

``` r
dorder <- function(x, k, n, dist = "norm", ...){
  pf <- get(paste0("p", dist))
  df <- get(paste0("d", dist))
  
  k*choose(n,k)*
    (pf(x, ...))^(k-1)*
    (1-pf(x, ...))^(n-k)*
    df(x, ...)
}
```

``` r
porder <- function(x, n, k, dist = "norm", ...) {
  pf <- get(paste0("p", dist))
  
  pbinom(k-1, n, pf(x, ...), lower.tail = FALSE)
}
```

``` r
qorder <- function(p, n, k, dist = "norm", ...){
  out <- p
  for(i in seq_along(p)) {
    out[i] <- uniroot(function(x){porder(x, n, k, dist, ...) - p[i]}, c(-100, 100))$root
  }
  out
}
```

**Q:** Use the newly modified functions to plot the probability and
density functions for the sample min (*N* = 200).

``` r
x = seq(-5,5, by = 0.001)
plot(x, dorder(x, n = 200, k = 1), type = "l", main = "PDF of Min Order Statistic of Standard Normal Density", ylab = "Density Probability", xlab = "X(1)")
```

![](writeup_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
x = seq(-5, 5, by = 0.001)
plot(x, porder(x, n = 200, k = 1), type = "l", main = "CDF of Min Order Statistic of Standard Normal Density", ylab = "Probability", xlab = "X(1)")
```

![](writeup_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
