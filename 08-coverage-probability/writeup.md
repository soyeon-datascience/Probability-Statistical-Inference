08-coverage-probability
================
Soyeon Park
11/14/2020

In this post, I am going to perform a simulation to calculate the
coverage probability of the 95% confidence interval of the median when
computed from *F̂*<sub>*X*</sub><sup>*m**l**e*</sup>. For the purpose of
the post, I defined the 95% confidence interval of the median to be the
middle 95% of sampling distribution of the median. Moreover, I defined
the coverage provability as the long run proportion of intervals that
capture the population parameter of interest.

## First step

I am going to generate a single sample from a standard normal
distribution of size *N* = 201.

``` r
library(tidyverse)
library(stats4)

N = 201
data = rnorm(N)
```

I am going to use Maximum Likelihood Estimation(MLE) to estimate the
distribution. Let’s say we think that the data I generated above might
be normally distributed. After calculating log likelihood, I am going to
find the best mean and standard deviation, both of which have the
maximum likelihood, for this one sample data.

``` r
# Log likelihood for normal distribution
nll <- function(mean, sd) {
  fs <- dnorm(x = data, mean, sd, log = TRUE)
  -sum(fs)
}

# Find a mean and standard deviation which has the maximum likelihood
fit_norm <- mle(
  nll,
  start = list(mean = mean(data), sd = sqrt(var(data))),
  method = "L-BFGS-B",
  lower = c(0, 0.01)
)

coef(fit_norm)
```

    ##      mean        sd 
    ## 0.0859385 0.9686724

Let’s look at the sample’s estimated PDF onto histogram

``` r
hist(data, freq = FALSE)
curve(dnorm(x, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2]), add = TRUE, col = "red", lwd = 2)
```

![](writeup_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Second step

I am going to show how I approximate the sampling distribution of the
median, conditional on the estimate of the distribution in the previous
step. In order to get 5000 medians from the sample, I used B + Q method.

``` r
n = 5000
# B+Q method 
meds = rbeta(n, 101, 101) %>% qnorm(mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
```

Another way which substitutes for the code above is using a for loop.
However, as B+Q method is much faster than for loop, I used the code
above.

``` r
n = 5000
medsAnotherWay = NA
for(i in 1:n){
  re.samp = rnorm(N, mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
  medsAnotherWay[i] = median(re.samp)
}
```

Let’s see the histogram of medians.

``` r
hist(meds, main = "Histogram of Medians")
```

![](writeup_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Third step

Let’s calculate a 95% confidence interval from the approximated sampling
distribution. Since I am interested in the middle 95% confidence
interval, I am going to calculate the 2.5th and 97.5th percentile of the
sampling distribution called `meds` by using the `quantile` function.

``` r
# confidence interval
ci = quantile(meds, c(0.025, 0.975))
ci
```

    ##        2.5%       97.5% 
    ## -0.07932513  0.25167833

## Fourth step

Finally, I am going to calculate the coverage probability.

Here is what my code below is going to do. It generates a single sample
from a standard normal distribution of size 201(N). This is original
sample data. Using MLE method, we generate 201 sample data and calculate
the median of this sample(Bootstrap), and repeats these steps 100(n)
times. Then we have 100(n) medians from the sampling distribution. Find
a confidence interval and check whether the interval includes
population’s parameter of interest(In this simulation, the
population’s parameter of interest is 0). Simulate these steps 1000
times. Finally, we get 1000 intervals and calculate the proportion of
intervals that capture the population parameter of interest.

``` r
gen.ci.med = function(n = 100, N = 201, parm.int = 0) {
  data = rnorm(N)

  normal.ll <- function(mean, sd){
    fs <- dnorm(x = data, mean = mean, sd = sd, log = TRUE)
    -sum(fs)
  }

  fit_norm <- mle(
    normal.ll,
    start = list(mean = mean(data), sd = sqrt(var(data))),
    method = "L-BFGS-B",
    lower = c(0, 0.01)
  )
  
  mean = coef(fit_norm)[1]
  sd = coef(fit_norm)[2]
  
  meds = rbeta(n, 101, 101) %>% qnorm(mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
  
  ci = quantile(meds, c(0.025, 0.975))
  return (ci[1] < parm.int & ci[2] > parm.int)
}

ci.contain = NA
repeat_num = 1000
for(i in 1:repeat_num){
  ci.contain[i] = gen.ci.med()
}

mean(ci.contain)
```

    ## [1] 0.986

In this simulation above, 983 intervals of 1000 intervals capture the
population parameter of interest. So, the coverage probability for this
simulation is 0.983.

## Additional Question

Q. How I might change the simulation to learn more about the operating
characteristics of my chosen method for constructing the 95% confidence
interval?

In this simulation, there are two variables we can change: `n`(the
number of bootstrap samples) and `repeat_num`(the number of simulation
or the number of intervals). For people who think N is also a variable,
we cannot change N in reality. N stands for the size of original sample
data. In reality, original sample data is already set, which means we
can’t control it.

First of all, let’s change the variable `n`(the number of bootstrap
samples). Let’s increase the variable to 1000.

``` r
gen.ci.med = function(n = 1000, N = 201, parm.int = 0) {
  data = rnorm(N)

  normal.ll <- function(mean, sd){
    fs <- dnorm(x = data, mean = mean, sd = sd, log = TRUE)
    -sum(fs)
  }

  fit_norm <- mle(
    normal.ll,
    start = list(mean = mean(data), sd = sqrt(var(data))),
    method = "L-BFGS-B",
    lower = c(0, 0.01)
  )
  
  mean = coef(fit_norm)[1]
  sd = coef(fit_norm)[2]
  
  meds = rbeta(n, 101, 101) %>% qnorm(mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
  
  ci = quantile(meds, c(0.025, 0.975))
  return (ci[1] < parm.int & ci[2] > parm.int)
}

ci.contain = NA
repeat_num = 1000
for(i in 1:repeat_num){
  ci.contain[i] = gen.ci.med()
}

mean(ci.contain)
```

    ## [1] 0.989

When we increase `n`(the number of bootstrap samples) into 1000, the
coverage probability is 0.993, which is higher than the coverage
probability when `n` was 100. This time, let’s decrease `n` into 10.

``` r
gen.ci.med = function(n = 10, N = 201, parm.int = 0) {
  data = rnorm(N)

  normal.ll <- function(mean, sd){
    fs <- dnorm(x = data, mean = mean, sd = sd, log = TRUE)
    -sum(fs)
  }

  fit_norm <- mle(
    normal.ll,
    start = list(mean = mean(data), sd = sqrt(var(data))),
    method = "L-BFGS-B",
    lower = c(0, 0.01)
  )
  
  mean = coef(fit_norm)[1]
  sd = coef(fit_norm)[2]
  
  meds = rbeta(n, 101, 101) %>% qnorm(mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
  
  ci = quantile(meds, c(0.025, 0.975))
  return (ci[1] < parm.int & ci[2] > parm.int)
}

ci.contain = NA
repeat_num = 1000
for(i in 1:repeat_num){
  ci.contain[i] = gen.ci.med()
}

mean(ci.contain)
```

    ## [1] 0.934

When we decrease `n`(the number of bootstrap samples) into 10, the
coverage probability is 0.931, which is lower than the coverage
probability when `n` was 100. For this trial, we can know that the more
bootstrap samples we have, the higher the coverage probability we can
get.

Second, let’s control `repeat_num`(the number of simulation or the
number of intervals). In the simulation above, I simulated 1000 times.
Let’s increase the number of simulation into 10000.

``` r
gen.ci.med = function(n = 100, N = 201, parm.int = 0) {
  data = rnorm(N)

  normal.ll <- function(mean, sd){
    fs <- dnorm(x = data, mean = mean, sd = sd, log = TRUE)
    -sum(fs)
  }

  fit_norm <- mle(
    normal.ll,
    start = list(mean = mean(data), sd = sqrt(var(data))),
    method = "L-BFGS-B",
    lower = c(0, 0.01)
  )
  
  mean = coef(fit_norm)[1]
  sd = coef(fit_norm)[2]
  
  meds = rbeta(n, 101, 101) %>% qnorm(mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
  
  ci = quantile(meds, c(0.025, 0.975))
  return (ci[1] < parm.int & ci[2] > parm.int)
}

ci.contain = NA
repeat_num = 10000
for(i in 1:repeat_num){
  ci.contain[i] = gen.ci.med()
}

mean(ci.contain)
```

    ## [1] 0.9881

It turns out that there is no big change in coverage probability when
you increase the number of simulation. Then, this time, let’s decrease
it into 100.

``` r
gen.ci.med = function(n = 100, N = 201, parm.int = 0) {
  data = rnorm(N)

  normal.ll <- function(mean, sd){
    fs <- dnorm(x = data, mean = mean, sd = sd, log = TRUE)
    -sum(fs)
  }

  fit_norm <- mle(
    normal.ll,
    start = list(mean = mean(data), sd = sqrt(var(data))),
    method = "L-BFGS-B",
    lower = c(0, 0.01)
  )
  
  mean = coef(fit_norm)[1]
  sd = coef(fit_norm)[2]
  
  meds = rbeta(n, 101, 101) %>% qnorm(mean = coef(fit_norm)[1], sd = coef(fit_norm)[2])
  
  ci = quantile(meds, c(0.025, 0.975))
  return (ci[1] < parm.int & ci[2] > parm.int)
}

ci.contain = NA
repeat_num = 100
for(i in 1:repeat_num){
  ci.contain[i] = gen.ci.med()
}

mean(ci.contain)
```

    ## [1] 0.99

In conclusion, the number of simulation does not give a big impact on
the coverage probability, while the number of bootstrap sample change
the coverage probability. The more bootstrap samples we have, the higher
the coverage probability we can get.
