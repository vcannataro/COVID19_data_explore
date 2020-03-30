COVID19 data exploration
================
Vincent L. Cannataro
last update 2020-March-29

Data obtained from curated Johns Hopkins University Center for Systems
Science and Engineering database here:
<https://github.com/CSSEGISandData/COVID-19.git> and from from The New
York Times, based on reports from state and local health agencies
<https://github.com/nytimes/covid-19-data>

# Current USA confirmed cases

It looks like the total number of confirmed cases in the USA is
currently growing
exponentially.

![](COVID19_initial_data_analyses_CannataroV_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->![](COVID19_initial_data_analyses_CannataroV_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

Indeed, if we pull out the data from 2020-03-01 until the current date,
and fit a linear model to the log10(y) ~ x,

``` r
start_date
```

    ## [1] "2020-03-01"

``` r
fit_lm_exp <- lm(formula = log10(all_cases) ~ date, 
                 data = subset(US_data,date>start_date))

# summary(fit_lm_exp)$r.squared # removed r.squared because 
# it is not a correct statistic on this time series data 
# https://twitter.com/vsbuffalo/status/1239233074203746304 
```

<!-- We find an $R^2$ value of 0.9982 -->

# Fitting into the future, assuming\* exponential growth continues

\* of course, exponential growth never continues indefinitely in
population models. Eventually, you will either run out of new
susceptible people to infect. Or, we can intervene in disease spread,
and deviate from this continued growth.

But, let’s naïvely assume exponential growth continues. What would
happen to the number of confirmed cases over time?

We can use our fit to the recent data generated above to predict into
the future.

``` r
end_date
```

    ## [1] "2020-04-12"

``` r
# fitting into the future 
future_predictions <- data.frame(Date = seq(start_date,end_date,by = "1 day"))

# future_predictions$log10_count <- predict(fit_lm_exp,newdata = future_predictions)
future_predictions$log10_count <- (as.numeric(future_predictions$Date) * fit_lm_exp$coefficients[2]) + fit_lm_exp$coefficients[1]
```

![](COVID19_initial_data_analyses_CannataroV_files/figure-gfm/fitting%20into%20the%20future%20plot-1.png)<!-- -->

Keep in mind, that the axis above is on a log10 scale, meaning that
\(6\) on the axis is really \(1000000\)

*Let’s do everything we can do deviate from this
fit\!*

# By State

![](COVID19_initial_data_analyses_CannataroV_files/figure-gfm/plotting%20states-1.png)<!-- -->![](COVID19_initial_data_analyses_CannataroV_files/figure-gfm/plotting%20states-2.png)<!-- -->

## Table of confirmed cases in the USA:

The total number of confirmed cases in the USA by date:

| date       | all\_cases |
| :--------- | ---------: |
| 2020-01-21 |          1 |
| 2020-01-22 |          1 |
| 2020-01-23 |          1 |
| 2020-01-24 |          2 |
| 2020-01-25 |          3 |
| 2020-01-26 |          5 |
| 2020-01-27 |          5 |
| 2020-01-28 |          5 |
| 2020-01-29 |          5 |
| 2020-01-30 |          6 |
| 2020-01-31 |          7 |
| 2020-02-01 |          8 |
| 2020-02-02 |         11 |
| 2020-02-03 |         11 |
| 2020-02-04 |         11 |
| 2020-02-05 |         12 |
| 2020-02-06 |         12 |
| 2020-02-07 |         12 |
| 2020-02-08 |         12 |
| 2020-02-09 |         12 |
| 2020-02-10 |         13 |
| 2020-02-11 |         13 |
| 2020-02-12 |         14 |
| 2020-02-13 |         15 |
| 2020-02-14 |         15 |
| 2020-02-15 |         15 |
| 2020-02-16 |         15 |
| 2020-02-17 |         25 |
| 2020-02-18 |         25 |
| 2020-02-19 |         25 |
| 2020-02-20 |         27 |
| 2020-02-21 |         30 |
| 2020-02-22 |         30 |
| 2020-02-23 |         30 |
| 2020-02-24 |         43 |
| 2020-02-25 |         45 |
| 2020-02-26 |         60 |
| 2020-02-27 |         60 |
| 2020-02-28 |         65 |
| 2020-02-29 |         70 |
| 2020-03-01 |         88 |
| 2020-03-02 |        104 |
| 2020-03-03 |        125 |
| 2020-03-04 |        161 |
| 2020-03-05 |        228 |
| 2020-03-06 |        311 |
| 2020-03-07 |        428 |
| 2020-03-08 |        547 |
| 2020-03-09 |        748 |
| 2020-03-10 |       1018 |
| 2020-03-11 |       1263 |
| 2020-03-12 |       1668 |
| 2020-03-13 |       2224 |
| 2020-03-14 |       2898 |
| 2020-03-15 |       3600 |
| 2020-03-16 |       4507 |
| 2020-03-17 |       5905 |
| 2020-03-18 |       8345 |
| 2020-03-19 |      12413 |
| 2020-03-20 |      17996 |
| 2020-03-21 |      24532 |
| 2020-03-22 |      33061 |
| 2020-03-23 |      43499 |
| 2020-03-24 |      54168 |
| 2020-03-25 |      68775 |
| 2020-03-26 |      85615 |
| 2020-03-27 |     102913 |
| 2020-03-28 |     123618 |
