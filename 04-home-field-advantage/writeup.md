---
title: "writeup"
author: "Soyeon Park"
date: "9/26/2020"
output: 
  html_document:
    keep_md: true
---
# Introduction

In this blog post, I am going to use __simulation and analytic probability calculations.__ to answer a series of questions related to the World Series.

## Problem

__If home field advantage exists, how much of an impact does it have on winning the world series?__

-   Suppose that the Braves and the Yankees are teams competing in the
    World Series.

-   The table below has the two possible schedules for each game of the
    series. (NYC = New York City, ATL = Atlanta)

| Overall advantage | Game 1 | Game 2 | Game 3 | Game 4 | Game 5 | Game 6 | Game 7 |
|:-----------------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
|       Braves      |   ATL  |   ATL  |   NYC  |   NYC  |   NYC  |   ATL  |   ATL  |
|      Yankees      |   NYC  |   NYC  |   ATL  |   ATL  |   ATL  |   NYC  |   NYC  |

-   Let *P*<sub>*B*</sub> be the probability that the Braves win a
    single head-to-head match-up with the Yankees, under the assumption
    that home field advantage doesn’t exist. Let
    *P*<sub>*B*</sub><sup>*H*</sup> denote the probability that the
    Braves win a single head-to-head match-up with the Yankees as the
    home team (H for home). Let *P*<sub>*B*</sub><sup>*A*</sup> denote
    the probability that the Braves win a single head-to-head match-up
    as the away team (A for away).

| Game location |    No advantage   | Advantage                                                            |
|:-------------:|:-----------------:|:---------------------------------------------------------------------|
|      ATL      | *P*<sub>*B*</sub> | *P*<sub>*B*</sub><sup>*H*</sup> = *P*<sub>*B*</sub> \* 1.1           |
|      NYC      | *P*<sub>*B*</sub> | *P*<sub>*B*</sub><sup>*A*</sup> = 1 − (1 − *P*<sub>*B*</sub>) \* 1.1 |

Questions to answer:

1.  Compute analytically the probability that the Braves win the world
    series when the sequence of game locations is {NYC, NYC, ATL, ATL,
    ATL, NYC, NYC}. (The code below computes the probability for the
    alternative sequence of game locations. **Note:** The code uses
    `data.table` syntax, which may be new to you. This is intential, as
    a gentle way to introduce `data.table`.) Calculate the probability
    with and without home field advantage when *P*<sub>*B*</sub> = 0.55.
    What is the difference in probabilities?

2.  Calculate the same probabilities as the previous question by
    simulation.

3.  What is the absolute and relative error for your simulation in the
    previous question?

4.  **Bonus.** Does the difference in probabilites (with vs without home
    field advantage) depend on *P*<sub>*B*</sub>?

5.  **Bonus.** Does the difference in probabilites (with vs without home
    field advantage) depend on the advantage factor? (The advantage
    factor in *P*<sub>*B*</sub><sup>*H*</sup> and
    *P*<sub>*B*</sub><sup>*A*</sup> is the 1.1 multiplier that results
    in a 10% increase for the home team.)

## Background

### Absolute Error VS Relative Error

Let p̂ denote the probability estimated from simulation, and let p denote the true underlying probability.

***

* Absolute Error
        |p̂−p|
* Relative Error
        |p̂−p|/p

***   
        
To help your understanding, I will take an example of car's speed. While you are driving, your car's speedometer says your car is going 100mph. However, you are actually going 105mph. In this case, the absolute error of your speedometer is |100 - 105 | = 5mph. The relative error is 5 mph / 105 mph = 0.047 or 4.7%. 

Let's apply this example to the "statistical" concept. I will replicate the exact same process using computer and record how many times result "A" happens. "The number of result A happening / the number of replicated action" is p̂ ( the probability estimated from simulation). The real probability is p(the true underlying probability.) The difference between these probabilities are "Absolute Error" and dividing this into the true probability is "Relative Error" 

# Results
##  1.Compute analytically the probability that the Braves win the world series when the sequence of game locations is {NYC, NYC, ATL, ATL, ATL, NYC, NYC}.

Calculate the probability with and without home field advantage when *P*<sub>*B*</sub> = 0.55. What is the difference in probabilities?

```r
# Importing Data
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
require(data.table)
```

```
## Loading required package: data.table
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
# Get all possible outcomes
apo <- fread("./all-possible-world-series-outcomes.csv")
```

The World Series is a best-of-7 match-up game and a team, who wins the first 4 games, wins the World Series. 

In the `apo` dataframe, every case to finish the World Series is listed. In this blog post, __I am going to explain from the Braves' view.__ "W" means that the Braves win, and "L" means that the Braves lose. 

```r
head(apo)
```

```
##    game_1 game_2 game_3 game_4 game_5 game_6 game_7 overall_outcome
## 1:      W      W      W      W                                    W
## 2:      W      W      W      L      W                             W
## 3:      W      W      L      W      W                             W
## 4:      W      L      W      W      W                             W
## 5:      L      W      W      W      W                             W
## 6:      W      W      W      L      L      W                      W
```
I made the `hfi` vector to indicate whether the Braves plays in their home field. "0" means the game is held in New York City, which is not the Braves' home field, and "1" means the game is held in Atlanta, which is the Braves' home field.

`adv_multi` stands for the advantage factor, 1.1, that results in a 10% increase for the home team. `pb` is the probability that the Braves win a single head-to-head match-up with the Yankees, under the assumption that home field advantage doesn’t exist. Let `pbh` denote the probability that the Braves win a single head-to-head match-up with the Yankees as the home team (H for home). Let `pba` denote the probability that the Braves win a single head-to-head match-up as the away team (A for away).

I made a new column called `p` to save each row case's probability. By using the `for` loop, I calculated each case's probability. I placed the multiply of each row's probability in the `p` column as a final probability for each case.

For example, let's say the `for` loop is calculating row 2 case(W, W, W, L, W). The value of `p` is calculated like this:

| Game # | Game 1 | Game 2 | Game 3 | Game 4 | Game 5 | Game 6 | Game 7 |           p           |
|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:--------------------------:|
| Field |   NYC  |   NYC  |   ATL  |   ATL  |   ATL  |   NYC  |   NYC  |                         |
| hfi indicator |   0  |   0  |   1  |   1  |   1  |   0  |   0  |                        |
| outcome |   W  |   W  |   W  |   L  |   W  |     |     |                         |
| probability |   __pba__  |   __pba__  |   __pbh__  |   __1 - pbh__  |   __pbh__  |   __1__  |   __1__  |   __pba * pba * pbh * (1-pbh) * pbh * 1 * 1__  |

After calculating, each row's probability, I calculated the probability that the Braves win the World Series, and the probability that the Braves lose the World Series by summing up each case's probability. This whole process was repeated one more time with the condition that the advantage factor is 1, which means the probability that the Braves win the world series when the sequence of game locations is {NYC, NYC, ATL, ATL, ATL, NYC, NYC} __without home field advantage.__

```r
analytic_ws <- function(hfi = c(0, 0, 1, 1, 1, 0, 0), adv_multi = 1.1, pb = 0.55) { #hfi = home field indicator
  pbh <- pb * adv_multi
  pba <- 1 - (1 - pb) * adv_multi
  apo[, p := NA_real_]
  
  for(i in 1:nrow(apo)) {
    prob_game <- rep(1, 7)
    for (j in 1:7) {
      p_win <- ifelse(hfi[j], pbh, pba)
      prob_game[j] <- case_when(
        apo[i, j, with = FALSE] == "W" ~ p_win,
        apo[i, j, with = FALSE] == "L" ~ 1 - p_win,
        TRUE ~ 1
      )
    }
   apo[i, p := prod(prob_game)]
  }
  apo[, sum(p), overall_outcome]
}

final <- merge(analytic_ws(), analytic_ws(adv_multi = 1), by = "overall_outcome")
names(final)[names(final) == "V1.x"] <- "w/ home field advantage"
names(final)[names(final) == "V1.y"] <- "w/o home field advantage"
final[2,]
```

```
##    overall_outcome w/ home field advantage w/o home field advantage
## 1:               W                0.604221                0.6082878
```

The difference in the probabilities is 0.004:

```r
abs(final[[2,2]] - final[[2,3]])
```

```
## [1] 0.004066825
```

## 2.Calculate the same probabilities as the previous question by simulation.

This time, I am going to use simulation to calculate the same probabilities. I made `simulation_ws` and this describes one World series game. The assumptions are same with question #1. If either the number of the Braves' winning or the number of the Braves' losing is 4, the loop stops.

Finally, if the Braves wins four games in this simulation, `simulation_ws()` returns "TRUE". If the Braves lose the World Series in this simulation, `simulation_ws()` returns `FLASE`.

```r
simulation_ws <- function(hfi = c(0, 0, 1, 1, 1, 0, 0), adv_multi = 1.1, pb = 0.55) {
  pbh <- 0.55 * adv_multi
  pba <- 1 - (1 -0.55) * adv_multi
  
  win_count = 0
  
  for(i in 1:7) {
    if(hfi[i]) {
      p_win = pbh
    } else {
      p_win = pba
    }
    game_out = rbinom(1, 1, p_win)
    win_count = win_count + game_out
    
    if(win_count == 4 | (1 - win_count) == 4) break
  }
  return(win_count == 4)
}
```

I repeated the `simulation_ws()` function 100,000 times. `simulation_ws()` is when the advantage factor __is considered__ and `simulation_ws(adv_multi = 1)` is when the advantage factor __is not considered.__

```r
saved_adv = NA
for(i in 1:100000){
  saved_adv[i] <- simulation_ws()
}

saved_not_adv = NA
for(i in 1:100000) {
  saved_not_adv[i] <- simulation_ws(adv_multi = 1)
}

mean(saved_adv)
```

```
## [1] 0.60578
```

```r
mean(saved_not_adv)
```

```
## [1] 0.60918
```

By using the simulation, I calculated these two probabilities:
The probability that the Braves win the Wolrd Series __with__ home field advantage is 0.604, while the probability that the Braves win the Wolrd Series __without__ home field advantage is 0.607. These results are close with the one from the first question. The more simulation we operate, the more similar analytic and simulation's results will be.

## 3.What is the absolute and relative error for your simulation in the previous question?

```r
# The absolute error(When the advantage factor is considered.)
abs(mean(saved_adv) - analytic_ws()$V1[1]) # result 1
```

```
## [1] 0.001559028
```

```r
# The relative error(When the advantage factor is considered.)
abs((mean(saved_adv) - analytic_ws()$V1[1])) / analytic_ws()$V1[1] # result 2
```

```
## [1] 0.002580229
```

```r
# The absolute error(When the advantage factor is not considered.)
abs(mean(saved_not_adv) - analytic_ws(adv_multi = 1)$V1[1]) #result 3
```

```
## [1] 0.0008922031
```

```r
# The relative error(When the advantage factor is not considered.)
abs((mean(saved_not_adv) - analytic_ws(adv_multi = 1)$V1[1])) / analytic_ws(adv_multi = 1)$V1[1] #result 4
```

```
## [1] 0.001466745
```

| case | absolute error | relative error |
|:-----------------:|:------:|:------:|
|       with the advantage factor      |   result 1  |   result 2  |
|      without the advantage factor      |   result 3  |   result 4  |

## 4.Does the difference in probabilites (with vs without home field advantage) depend on *P*<sub>*B*</sub>?

```r
# x-axis = prob_braves, y-axis = diff
prob_braves <- seq(0, 1, by = 0.01)
diff <- NA

for(i in 1 : length(prob_braves)) {
# Calculating the probability that the Braves win the World Series with the advantage factor 1.1
  ws_adv <- function(pb = prob_braves[i]) {
    hfi = c(0, 0, 1, 1, 1, 0, 0)
    pbh <- pb * 1.1
    pba <- 1 - (1 - pb) * 1.1
    
    apo[, p := NA_real_]
    for(i in 1:nrow(apo)) {
      prob_game <- rep(1, 7)
      for (j in 1:7) {
        p_win <- ifelse(hfi[j], pbh, pba)
        prob_game[j] <- case_when(
          apo[i, j, with = FALSE] == "W" ~ p_win,
          apo[i, j, with = FALSE] == "L" ~ 1 - p_win,
          TRUE ~ 1
      )
    }
   apo[i, p := prod(prob_game)]
  }
  apo[, sum(p), overall_outcome]$V1[1]
  }
  
# Calculating the probability that the Braves win the World Series without the advantage factor
  ws_no_adv <- function(pb = prob_braves[i]) {
    hfi = c(0, 0, 1, 1, 1, 0, 0)
    pbh <- pb * 1
    pba <- 1 - (1 - pb) * 1
    
    apo[, p := NA_real_]
    for(i in 1:nrow(apo)) {
      prob_game <- rep(1, 7)
      for (j in 1:7) {
        p_win <- ifelse(hfi[j], pbh, pba)
        prob_game[j] <- case_when(
          apo[i, j, with = FALSE] == "W" ~ p_win,
          apo[i, j, with = FALSE] == "L" ~ 1 - p_win,
          TRUE ~ 1
        )
    }
   apo[i, p := prod(prob_game)]
  }
  apo[, sum(p), overall_outcome]$V1[1]
  }

#Calculating difference between in the probability with and witout the advantage factor & Assigning the result in the "diff" vector.
diff[i] <- ws_no_adv(pb = prob_braves[i]) - ws_adv(pb = prob_braves[i])
}
plot(prob_braves, diff, type = "l", main = "with VS without home field advantage", xlab = "The probability that the Braves win a single head-to-head match-up with the Yankees", ylab = "The difference in probabilities(with vs without holme field advantage)", cex.lab = 0.7)
```

![](writeup_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

In this plot, the x-axis is the probability that the Braves win a single head-to-head match-up with the Yankees(*P*<sub>*B*</sub>), and the y-axis is the difference in probabilities with and without the advantage factor. 

Within the x's range (0.1, 0.5), the value of y is positive, which means the probability that the Braves win the World Series __without_ the advantage factor is bigger than the probability that the one __with__ the advantage factor. On the other hand, within the x's range (0.5, 0.9), the value of y is negative, which means that the probability that the Braves win the World Series __with__ the advantage factor is bigger than the one __without__ the advantage factor. 

At the extremes (x = 0, x = 1) there is no difference in probabilities because the probability that the Braves win per game is so extreme that they will lose eery game if x=0, or win every game if x=1.

Therefore, __the difference in probabilities (with vs without home field advantage) depends on *P*<sub>*B*</sub>__.

## 5.Does the difference in probabilites (with vs without home field advantage) depend on the advantage factor? 

(The advantage factor in *P*<sub>*B*</sub><sup>*H*</sup> and *P*<sub>*B*</sub><sup>*A*</sup> is the 1.1 multiplier that results in a 10% increase for the home team.)

```r
# x-axis = adv_factor, y-axis = diff
adv_factor <- seq(1, 2, by = 0.01)
diff <- NA

for(i in 1 : length(adv_factor)) {
  ws_adv <- function(multi_advantage = adv_factor[i]) {
    hfi = c(0, 0, 1, 1, 1, 0, 0)
    pbh <- 0.55 * multi_advantage
    pba <- 1 - (1 - 0.55) * multi_advantage
    
    apo[, p := NA_real_]
    for(i in 1:nrow(apo)) {
      prob_game <- rep(1, 7)
      for (j in 1:7) {
        p_win <- ifelse(hfi[j], pbh, pba)
        prob_game[j] <- case_when(
          apo[i, j, with = FALSE] == "W" ~ p_win,
          apo[i, j, with = FALSE] == "L" ~ 1 - p_win,
          TRUE ~ 1
      )
    }
   apo[i, p := prod(prob_game)]
  }
  apo[, sum(p), overall_outcome]$V1[1]
  }

  ws_no_adv <- function(multi_advantage = adv_factor[i]) {
    hfi = c(0, 0, 1, 1, 1, 0, 0)
    pbh <- 0.55 * 1
    pba <- 1 - (1 - 0.55) * 1
    
    apo[, p := NA_real_]
    for(i in 1:nrow(apo)) {
      prob_game <- rep(1, 7)
      for (j in 1:7) {
        p_win <- ifelse(hfi[j], pbh, pba)
        prob_game[j] <- case_when(
          apo[i, j, with = FALSE] == "W" ~ p_win,
          apo[i, j, with = FALSE] == "L" ~ 1 - p_win,
          TRUE ~ 1
        )
    }
   apo[i, p := prod(prob_game)]
  }
  apo[, sum(p), overall_outcome]$V1[1]
  }
diff[i] <- ws_no_adv(multi_advantage = adv_factor[i]) - ws_adv(multi_advantage = adv_factor[i])
}
plot(adv_factor, diff, type = "l", main = "with VS without home field advantage", xlab = "The advantage factor", ylab = "The difference in probabilities(with vs without holme field advantage", cex.lab = 0.7)
```

![](writeup_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

For this question, I set a value of the advantage factor as x-axis, and the probabilities difference between with vs without home field advantage as y-axis. The plot shows that the bigger the advantage factor is, the bigger the difference in probabilities(with vs without the home field advantage). As a result, __the difference in probabilities (with vs without home field advantage) depend on the advantage factor.__

# Conclusions

By using either analytic method or simulation, we can calculate one specific situation's probability. The bigger the number of simulation being operated is, the less difference between analytic probability and simulation probability is. In other words, we can decrease the absolute and relative error as we simulate more times.

Intuitively, if the advantage factor exists, it seems that the probability that the Braves wins the World Series is bigger than the one without the advantage factor. However, in this question, there is assumption that the Braves can have the chance to play in their home field only 3 times among 7 games. This means, the opponent team, the Yankees, has 4 chances to play in their home field with the home advantage. Since we are giving more chances to the Yankees, the probability that the Braves win the World Series __with__ the home advantage is less than the probability that the Braves win the World Series __without__ the home advantage.

The difference in probabilities (with vs without home field advantage) depend on not *P*<sub>*B*</sub>, but the value of the advantage factor. 
