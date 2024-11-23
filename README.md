<!-- README.md is generated from CancerMortality.Rmd. Please edit that file. -->

# Data Preparation

    ## Warning: package 'crayon' was built under R version 4.4.2

    ## Warning: package 'car' was built under R version 4.4.2

    ## Loading required package: carData

    ## Warning: package 'carData' was built under R version 4.4.2

    ## Warning: package 'FactoMineR' was built under R version 4.4.2

    ## Warning: package 'chemometrics' was built under R version 4.4.2

    ## Loading required package: rpart

    ## Warning: package 'corrplot' was built under R version 4.4.2

    ## corrplot 0.95 loaded

    ## Warning: package 'PerformanceAnalytics' was built under R version 4.4.2

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 4.4.2

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.4.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

    ## Warning: package 'mice' was built under R version 4.4.2

    ## 
    ## Attaching package: 'mice'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

    ## Warning: package 'dplyr' was built under R version 4.4.2

    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:xts':
    ## 
    ##     first, last

    ## The following object is masked from 'package:car':
    ## 
    ##     recode

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Warning: package 'readr' was built under R version 4.4.2

    ## Warning: package 'stringr' was built under R version 4.4.2

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    ## Warning: package 'lmtest' was built under R version 4.4.2

    ## 
    ## Attaching package: 'lmtest'

    ## The following object is masked from 'package:crayon':
    ## 
    ##     reset

    ## Warning: package 'sandwich' was built under R version 4.4.2

First we import the data and save it as the variable “df” for future
modifications.

``` r
par(mfrow=c(1,1))
df <- read_csv("data/train.csv")
```

    ## Rows: 1831 Columns: 33
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): binnedinc, geography
    ## dbl (31): avganncount, avgdeathsperyear, target_deathrate, incidencerate, me...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Here’s a breakdown of all the variables in the dataset.

| **Variable Name** | **Num/Fac** | **Description** |
|-----------------------------|---------------|-----------------------------|
| avganncount | N | Mean number of reported cases of cancer diagnosed annually (2010-2015 |
| avgdeathsperyear | N | Mean number of reported mortalities due to cancer |
| target_deathrate | N | Response variable. Mean per capita (100,000) cancer mortalities |
| incidencerate | N | Mean per capita (100,000) cancer diagnoses |
| medincome | N | Median income per county |
| popest2015 | N | Population of county |
| povertypercent | N | Percent of population in poverty |
| studypercap | N | Per capita number of cancer-related clinical trials per county |
| binnedinc | F | Median income per capita binned by decile |
| medianage | N | Median age of county residents |
| medianagemale | N | Median age of male county residents |
| medianagefemale | N | Median age of female county residents |
| geography | F | County name |
| percentmarried | N | Percent of county residents who are married |
| pctnohs18_24 | N | Percent of county residents ages 18-24 highest education attained: less than high school |
| pcths18_24 | N | Percent of county residents ages 18-24 highest education attained: high school diploma |
| pctsomecol18_24 | N | Percent of county residents ages 18-24 highest education attained: some college |
| pcths25_over | N | Percent of county residents ages 25 and over highest education attained: high school diploma |
| pctbachdeg25_over | N | Percent of county residents ages 25 and over highest education attained: bachelor’s degree |
| pctemployed16_over | N | Percent of county residents ages 16 and over employed |
| pctunemployed16_over | N | Percent of county residents ages 16 and over unemployed |
| pctprivatecoverage | N | Percent of county residents with private health coverage |
| pctprivatecoveragealone | N | Percent of county residents with private health coverage alone (no public assistance) |
| pctempprivcoverage | N | Percent of county residents with employee-provided private health coverage |
| pctpubliccoverage | N | Percent of county residents with government-provided health coverage |
| pctpubliccoveragealone | N | Percent of county residents with government-provided health coverage alone |
| pctwhite | N | Percent of county residents who identify as White |
| pctblack | N | Percent of county residents who identify as Black |
| pctasian | N | Percent of county residents who identify as Asian |
| pctotherrace | N | Percent of county residents who identify in a category which is not White, Black, or Asian |
| pctmarriedhouseholds | N | Percent of married households |
| birthrate | N | Number of live births relative to number of women in county |

## Variable Types

All variables but `geography` should be numeric. Only one variable
requires any change: `binnedinc` is a string variable right now, but two
things can be created from it: a factor variable (`f.binnedinc`), and a
numeric variable holding the midpoint of each bin, which we’ll
conviniently call `binnedinc`.

``` r
df$f.binnedinc <- as.factor(df$binnedinc)
# Use regex to remove the [,],( and ) from the rows:
inc.midpoints.text <- gsub("[\\[\\]()]", "", df$binnedinc, perl = T)
# Separate them into two numbers
inc.midpoints.text.sep <- strsplit(inc.midpoints.text, ",")
# Convert them to numbers and apply a mean between them to find the midpoint
df$binnedinc <- sapply(inc.midpoints.text.sep, function(x) mean(as.numeric(x)))
```

Note how `geography`, altough being non-numeric, it’s not a factor
variable. This can be shown by looking at the number of unique values in
the column.

``` r
nrow(df); length(unique(df$geography))
```

    ## [1] 1831

    ## [1] 1831

Because the number of unique values is the same as the number of rows,
we can safely assume that `geography` is a unique identifier for each
row. It won’t be removed yet, because, as proven in a later section, a
factor variable can be derived from it.

## Duplicated Data Points

Before proceeding with the analysis, we should check if there are any
duplicated rows in the data set. If there are, we shall remove them.

``` r
duplicated_row_count <- sum(duplicated(df))
if (duplicated_row_count > 0) {
    print(sprintf("There are %d duplicated rows.", duplicated_row_count))
    df <- unique(df)
}
```

No duplicated rows were found in the data set.

## Missing data

Glancing over the data set, one can see that there are some missing
values:

``` r
for (colname in colnames(df)) {
    na.count <- sum(is.na(df[[colname]]))
    if (na.count > 0) {
        cat(sprintf("%s has %s\n", colname, red(sprintf("%d N/As", sum(is.na(df[colname]))))))
    }
}
```

    ## pctsomecol18_24 has 1376 N/As
    ## pctemployed16_over has 82 N/As
    ## pctprivatecoveragealone has 356 N/As

-   Column `pctsomecol18_24` has 1376 N/As, which makes for more than
    75% of the data. Due to that, it would most likely provide little to
    no meaningful information, so a decision was made to remove it from
    the study.
-   Column `pctemployed16_over` has 82 N/As, which is a manageable
    amount. They can easily be imputed using the MICE method.
-   Column `pctprivatecoveragealone` has 356 N/As, which acounts for
    less than 20% of the rows. This is a small enough amount to be
    imputed using the MICE method. Nontheless, this column will be
    removed from the study later on, for reasons explained in the
    exploratory data analysis section.

``` r
# Drop the column with too many missing values
df <- subset(df, select = -c(pctsomecol18_24))

# Impute missing values
res.mice <- mice(df)
```

    ## 
    ##  iter imp variable
    ##   1   1  pctemployed16_over  pctprivatecoveragealone
    ##   1   2  pctemployed16_over  pctprivatecoveragealone
    ##   1   3  pctemployed16_over  pctprivatecoveragealone
    ##   1   4  pctemployed16_over  pctprivatecoveragealone
    ##   1   5  pctemployed16_over  pctprivatecoveragealone
    ##   2   1  pctemployed16_over  pctprivatecoveragealone
    ##   2   2  pctemployed16_over  pctprivatecoveragealone
    ##   2   3  pctemployed16_over  pctprivatecoveragealone
    ##   2   4  pctemployed16_over  pctprivatecoveragealone
    ##   2   5  pctemployed16_over  pctprivatecoveragealone
    ##   3   1  pctemployed16_over  pctprivatecoveragealone
    ##   3   2  pctemployed16_over  pctprivatecoveragealone
    ##   3   3  pctemployed16_over  pctprivatecoveragealone
    ##   3   4  pctemployed16_over  pctprivatecoveragealone
    ##   3   5  pctemployed16_over  pctprivatecoveragealone
    ##   4   1  pctemployed16_over  pctprivatecoveragealone
    ##   4   2  pctemployed16_over  pctprivatecoveragealone
    ##   4   3  pctemployed16_over  pctprivatecoveragealone
    ##   4   4  pctemployed16_over  pctprivatecoveragealone
    ##   4   5  pctemployed16_over  pctprivatecoveragealone
    ##   5   1  pctemployed16_over  pctprivatecoveragealone
    ##   5   2  pctemployed16_over  pctprivatecoveragealone
    ##   5   3  pctemployed16_over  pctprivatecoveragealone
    ##   5   4  pctemployed16_over  pctprivatecoveragealone
    ##   5   5  pctemployed16_over  pctprivatecoveragealone

    ## Warning: Number of logged events: 51

``` r
complete_df = complete(res.mice, action = 1)
```

Before substituting the missing values, let’s check the deciles of the
variables to see if the imputation makes sense.

``` r
quantile(df$pctemployed16_over, na.rm = TRUE, probs = seq(0, 1, 0.1))
```

    ##    0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
    ## 23.90 43.30 47.30 49.94 52.50 54.50 57.10 59.20 61.50 64.50 80.10

``` r
quantile(df$pctprivatecoveragealone, na.rm = TRUE, probs = seq(0, 1, 0.1))
```

    ##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
    ## 16.8 35.6 39.7 42.9 45.8 49.0 51.6 54.3 57.0 61.6 78.9

Now let’s substitute the missing values in the original data set. And
check that the deciles are still roughly the same.

``` r
df$pctemployed16_over <- complete_df$pctemployed16_over
df$pctprivatecoveragealone <- complete_df$pctprivatecoveragealone

quantile(df$pctemployed16_over, na.rm = TRUE, probs = seq(0, 1, 0.1))
```

    ##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
    ## 23.9 43.3 47.4 50.0 52.5 54.6 57.2 59.3 61.5 64.5 80.1

``` r
quantile(df$pctprivatecoveragealone, na.rm = TRUE, probs = seq(0, 1, 0.1))
```

    ##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
    ## 16.8 35.3 39.7 42.9 45.8 48.9 51.5 54.3 57.2 61.6 78.9

Which they are.

## Univariate Outliers

Column-wise, we can count how many univariate outliers each numeric
variable has:

``` r
for (colname in colnames(Filter(is.numeric, df))) {
  col = df[[colname]]
  q1 <- quantile(col, 0.25)
  q3 <- quantile(col, 0.75)
  iqr <- q3 - q1

  severe <- list(top = q3 + 3 * iqr, bot = q1 - 3 * iqr)
  mild <- list(top = q3 + 1.5 * iqr, bot = q1 - 1.5 * iqr)

  severe_out <- sum(col > severe$top | col < severe$bot)
  mild_out <- sum((col > mild$top & col < severe$top) | (col < mild$bot & col > severe$bot))
  if (mild_out > 0 | severe_out > 0) {
    cat(sprintf("Column %s has %d mild outliers and %d severe outliers\n", colname, mild_out, severe_out))
  }
}
```

    ## Column avganncount has 49 mild outliers and 224 severe outliers
    ## Column avgdeathsperyear has 87 mild outliers and 138 severe outliers
    ## Column target_deathrate has 34 mild outliers and 1 severe outliers
    ## Column incidencerate has 53 mild outliers and 7 severe outliers
    ## Column medincome has 54 mild outliers and 15 severe outliers
    ## Column popest2015 has 90 mild outliers and 162 severe outliers
    ## Column povertypercent has 42 mild outliers and 0 severe outliers
    ## Column studypercap has 85 mild outliers and 222 severe outliers
    ## Column binnedinc has 186 mild outliers and 0 severe outliers
    ## Column medianage has 47 mild outliers and 18 severe outliers
    ## Column medianagemale has 46 mild outliers and 0 severe outliers
    ## Column medianagefemale has 55 mild outliers and 0 severe outliers
    ## Column percentmarried has 34 mild outliers and 0 severe outliers
    ## Column pctnohs18_24 has 30 mild outliers and 5 severe outliers
    ## Column pcths18_24 has 33 mild outliers and 0 severe outliers
    ## Column pctbachdeg18_24 has 46 mild outliers and 10 severe outliers
    ## Column pcths25_over has 18 mild outliers and 0 severe outliers
    ## Column pctbachdeg25_over has 56 mild outliers and 3 severe outliers
    ## Column pctemployed16_over has 11 mild outliers and 0 severe outliers
    ## Column pctunemployed16_over has 38 mild outliers and 4 severe outliers
    ## Column pctprivatecoverage has 17 mild outliers and 0 severe outliers
    ## Column pctprivatecoveragealone has 2 mild outliers and 0 severe outliers
    ## Column pctempprivcoverage has 7 mild outliers and 0 severe outliers
    ## Column pctpubliccoverage has 13 mild outliers and 0 severe outliers
    ## Column pctpubliccoveragealone has 21 mild outliers and 0 severe outliers
    ## Column pctwhite has 90 mild outliers and 7 severe outliers
    ## Column pctblack has 125 mild outliers and 99 severe outliers
    ## Column pctasian has 91 mild outliers and 107 severe outliers
    ## Column pctotherrace has 79 mild outliers and 102 severe outliers
    ## Column pctmarriedhouseholds has 55 mild outliers and 2 severe outliers
    ## Column birthrate has 87 mild outliers and 17 severe outliers

Row-wise, we’ll count the numeric variables in which each data point is
an outlier, and create a new object called `univariate_outlier_count`.
As a gut-driven criterion, we shall consider a row to be an outlier if
it is an outlier in 10 or more variables. Based on this criterion, only
9 counties are.

``` r
count_outliers <- function(data) {
  # Function to check for outliers based on IQR
  is_outlier <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    return(x < lower_bound | x > upper_bound)
  }
  
  # Apply the outlier function to each column and sum the results for each row using dplyr
  data %>%
    mutate(outlier_count = rowSums(sapply(., is_outlier), na.rm = TRUE))
}

univariate_outlier_count <- count_outliers(Filter(is.numeric, df))$outlier_count
df[which(univariate_outlier_count >= 10),]
```

    ## # A tibble: 9 × 33
    ##   avganncount avgdeathsperyear target_deathrate incidencerate medincome
    ##         <dbl>            <dbl>            <dbl>         <dbl>     <dbl>
    ## 1        862               283             136.          365.    122641
    ## 2        135                23             162.         1014.     46954
    ## 3       4139              1292             120.          393.     97279
    ## 4       3648              1186             140           447     100806
    ## 5       7334              2355             135           420      97219
    ## 6        984               259             128.          424     107250
    ## 7       1963.              796             147.          454.     76104
    ## 8       8236              3303             212.          534.     39037
    ## 9        954               327             146.          398.     89861
    ## # ℹ 28 more variables: popest2015 <dbl>, povertypercent <dbl>,
    ## #   studypercap <dbl>, binnedinc <dbl>, medianage <dbl>, medianagemale <dbl>,
    ## #   medianagefemale <dbl>, geography <chr>, percentmarried <dbl>,
    ## #   pctnohs18_24 <dbl>, pcths18_24 <dbl>, pctbachdeg18_24 <dbl>,
    ## #   pcths25_over <dbl>, pctbachdeg25_over <dbl>, pctemployed16_over <dbl>,
    ## #   pctunemployed16_over <dbl>, pctprivatecoverage <dbl>,
    ## #   pctprivatecoveragealone <dbl>, pctempprivcoverage <dbl>, …

All of them have high percentages of non-white population, both black
and asian, a low median age, a high mortality count and a high bias
towards private and employee health coverage. Of these 9 counties, 7 are
wealthy (Low poverty percent) and 2 have a large poor population (over
20%).

Outliers can sometimes provide valuable information, so they won’t be
removed from the data set just yet.

### The case of medianage

`medianage` is a continuous variable which contains some data points
that make no sense, for instance, median ages over 100. Thankfully, we
have data for male and female median age, which allow us to replace
outlier points by a mean of male and female age.

``` r
boxplot(df$medianage, horizontal = TRUE, main = "Median Age") 
```

![](images/unnamed-chunk-12-1.png)

``` r
out = which(df$medianage > 100)
df$medianage[out] <- (df$medianagemale[out] + df$medianagefemale[out]) / 2
```

As the following boxplot shows, the meaninglessly high values have taken
a more reasonable value.

``` r
boxplot(df$medianage, horizontal = TRUE, main = "Median Age") 
```

![](images/unnamed-chunk-13-1.png)

## Multivariate Outliers

We will apply Moutlier on the numerical variables in order to find
multivariate outliers. We have to perform the calculation excluding the
variable studypercap because otherwise the method is unable to execute
due to multicollinearity casuing a singularity matrix in the
intermediate calculations. An extremely mild threshold is chosen
(0.00005%) because even using this threshold we get a significant amount
of multivariate outliers, 4% of the total sample. Lowering the threshold
even further doesn’t change much the amount of outliers and rising it
higher makes the amount of outliers rise too much (10% outliers at 0.1%
significance level).

``` r
numeric.df <- Filter(is.numeric, df)
numeric.df <- numeric.df[, !colnames(numeric.df) %in% c("studypercap")]

res.out_95 <- Moutlier(numeric.df, quantile = 0.95, plot=F)
multi_outliers_95 = which((res.out_95$md > res.out_95$cutoff)&(res.out_95$rd > res.out_95$cutoff))
length(multi_outliers_95)
```

    ## [1] 269

``` r
res.out <- Moutlier(numeric.df, quantile = 0.9999995, plot=F)
multi_outliers = which((res.out$md > res.out$cutoff)&(res.out$rd > res.out$cutoff))
length(multi_outliers)
```

    ## [1] 82

``` r
par(mfrow = c(1,1))
plot(res.out$rd, res.out$md )
abline(h=res.out$cutoff, col="red")
abline(v=res.out$cutoff, col="red")
```

![](images/unnamed-chunk-14-1.png)

There are 91 multivariate outliers in the data set (265 if we take a 95%
quantile).

# Exploratory Data Analysis

This section will be devided in two parts: single-variable analysis and
multi-variable analysis.

## Single-Variable analysis

This sub-section presents an analysis for each variable of the data set
as a standalone sample.

We’ll be performing lots of discretisation of continuous variables based
on their quartiles, so let’s create a function to do that.

``` r
discretize_quartiles <- function(column, label_name) {
  res <- cut(column, breaks = quantile(column, probs = seq(0, 1, 0.25)), 
    include.lowest = T,
    labels=c(
      sprintf("Low%s", label_name),
      sprintf("LowMid%s", label_name),
      sprintf("HighMid%s", label_name),
      sprintf("High%s", label_name)
    )
  )
  print(table(res)) # Print the table
  return(res)
}
```

### Variable 1 - avganncount

This is a continuous ratio variable. The data does not look normally
distributed, which is confirmed by the near-null p-value of the shapiro
normallity test. A histogram is used to visualize the data.

``` r
summary(df$avganncount)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     7.0    80.0   175.0   623.2   509.0 38150.0

``` r
hist(df$avganncount, breaks = 30, freq = F)
curve(dnorm(x, mean(df$avganncount), sd(df$avganncount)), add = T)
```

![](images/unnamed-chunk-16-1.png)

``` r
shapiro.test(df$avganncount)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$avganncount
    ## W = 0.33377, p-value < 2.2e-16

An additional factor `f.avganncount` is created to discretize the data
according to the quartiles.

``` r
df$f.avganncount <- discretize_quartiles(df$avganncount, "CaseCount")
```

    ## res
    ##     LowCaseCount  LowMidCaseCount HighMidCaseCount    HighCaseCount 
    ##              460              458              455              458

### Variable 2 - avgdeathsperyear

This is also a continuous ratio variable similar to variable 1. The data
does not look normally distributed, which is confirmed by the near-null
p-value of the shapiro normallity test. Again a histogram is used to
visualize the data.

``` r
summary(df$avgdeathsperyear)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     3.0    29.0    62.0   191.6   140.5 14010.0

``` r
hist(df$avgdeathsperyear, breaks = 30, freq = F)
curve(dnorm(x, mean(df$avgdeathsperyear), sd(df$avgdeathsperyear)), add = T)
```

![](images/unnamed-chunk-18-1.png)

``` r
shapiro.test(df$avgdeathsperyear)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$avgdeathsperyear
    ## W = 0.26769, p-value < 2.2e-16

An additional factor `f.avgdeathsperyear` is created to discretize the
data according to the quartiles.

``` r
df$f.avgdeathsperyear <- discretize_quartiles(df$avgdeathsperyear, "MortCount")
```

    ## res
    ##     LowMortCount  LowMidMortCount HighMidMortCount    HighMortCount 
    ##              462              455              456              458

### Variable 3 - target_deathrate

This is the response variable. This is also a continuous ratio variable
similar to the previous variables. The data looks normally distributed,
but it is not and will be further discussed in the next section.

``` r
summary(df$target_deathrate)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    59.7   161.3   178.3   178.8   195.3   362.8

``` r
hist(df$target_deathrate, breaks = 30, freq = F)
curve(dnorm(x, mean(df$target_deathrate), sd(df$target_deathrate)), add = T)
```

![](images/unnamed-chunk-20-1.png)

``` r
shapiro.test(df$target_deathrate)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$target_deathrate
    ## W = 0.98647, p-value = 4.149e-12

An additional factor `f.target_deathrate` is created to discretize the
data according to the quartiles.

``` r
df$f.target_deathrate <- discretize_quartiles(df$target_deathrate, "DeathRate")
```

    ## res
    ##     LowDeathRate  LowMidDeathRate HighMidDeathRate    HighDeathRate 
    ##              459              459              456              457

### Variable 4 - incidencerate

We have another continuous ratio variable similar to the previous
variables. It is not normally distributed according to the Shapiro test.

``` r
summary(df$incidencerate)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   201.3   421.4   453.5   449.0   481.3  1206.9

``` r
hist(df$incidencerate, breaks = 30, freq = F)
curve(dnorm(x, mean(df$incidencerate), sd(df$incidencerate)), add = T)
```

![](images/unnamed-chunk-22-1.png)

``` r
shapiro.test(df$incidencerate)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$incidencerate
    ## W = 0.89577, p-value < 2.2e-16

An additional factor `f.incidencerate` is created to discretize the data
according to the quartiles.

``` r
df$f.incidencerate <- discretize_quartiles(df$incidencerate, "DiagnPerCap")
```

    ## res
    ##     LowDiagnPerCap  LowMidDiagnPerCap HighMidDiagnPerCap    HighDiagnPerCap 
    ##                460                535                378                458

### Variable 5 - medincome

Very similar to all the previous variables we have a continuous ratio
variable not normally distributed.

``` r
summary(df$medincome)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   22640   39031   45454   47278   52612  122641

``` r
hist(df$medincome, breaks = 30, freq = F)
curve(dnorm(x, mean(df$medincome), sd(df$medincome)), add = T)
```

![](images/unnamed-chunk-24-1.png)

``` r
shapiro.test(df$medincome)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$medincome
    ## W = 0.9105, p-value < 2.2e-16

An additional factor `f.medincome` is created to discretize the data
according to the quartiles.

``` r
df$f.medincome <- discretize_quartiles(df$medincome, "MedianInc")
```

    ## res
    ##     LowMedianInc  LowMidMedianInc HighMidMedianInc    HighMedianInc 
    ##              458              458              457              458

### Variable 6 - popest2015

Another continuous ratio variable not normally distributed.

``` r
summary(df$popest2015)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##      829    12191    27158   106841    66880 10170292

``` r
hist(df$popest2015, breaks = 30, freq = F)
curve(dnorm(x, mean(df$popest2015), sd(df$popest2015)), add = T)
```

![](images/unnamed-chunk-26-1.png)

``` r
shapiro.test(df$popest2015)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$popest2015
    ## W = 0.22666, p-value < 2.2e-16

An additional factor `f.popest2015` is created to discretize the data
according to the quartiles.

``` r
df$f.popest2015 <- discretize_quartiles(df$popest2015, "MidPop")
```

    ## res
    ##     LowMidPop  LowMidMidPop HighMidMidPop    HighMidPop 
    ##           458           458           457           458

### Variable 7 - povertypercent

Another continuous ratio variable not normally distributed.

``` r
summary(df$povertypercent)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    3.70   12.15   15.70   16.79   20.40   44.00

``` r
hist(df$povertypercent, breaks = 30, freq = F)
curve(dnorm(x, mean(df$povertypercent), sd(df$povertypercent)), add = T)
```

![](images/unnamed-chunk-28-1.png)

``` r
shapiro.test(df$povertypercent)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$povertypercent
    ## W = 0.95557, p-value < 2.2e-16

An additional factor `f.povertypercent` is created to discretize the
data according to the quartiles.

``` r
df$f.povertypercent <- discretize_quartiles(df$povertypercent, "Pov%")
```

    ## res
    ##     LowPov%  LowMidPov% HighMidPov%    HighPov% 
    ##         458         468         451         454

### Variable 8 - studypercap

Another continuous ratio variable. This variable has the peculiarity of
having a lot of 0s (median is also 0 so more than half of the counties
don’t perform cancer related clinical trials). It is not normally
distributed.

``` r
summary(df$studypercap)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0     0.0     0.0   148.2    76.0  9762.3

``` r
hist(df$studypercap, breaks = 30, freq = F)
curve(dnorm(x, mean(df$studypercap), sd(df$studypercap)), add = T)
```

![](images/unnamed-chunk-30-1.png)

``` r
shapiro.test(df$studypercap)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$studypercap
    ## W = 0.30754, p-value < 2.2e-16

An additional factor `f.studypercap` is created to discretize the data,
this time groupping the data in only 3 groups: 0, and the two median
splits of the non-zero values.

``` r
non_zero_median <- median(df$studypercap[df$studypercap > 0])

df$f.studypercap <- cut(df$studypercap, breaks = c(-Inf, 0, non_zero_median, Inf),
    include.lowest = T,
    labels=c("NoTrials", "MidTrials", "HighTrials")
  )
table(df$f.studypercap)
```

    ## 
    ##   NoTrials  MidTrials HighTrials 
    ##       1162        335        334

### Variable 9 - binnedinc

After having converted it from a string representation of the bin into a
numeric variable, analyzing its normality with Shapiro Test, we can
safely say it’s not a normally-distributed variable.

``` r
summary(df$binnedinc)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   28429   38888   46611   49082   52796   93565

``` r
hist(df$binnedinc, breaks = 30, freq = F)
curve(dnorm(x, mean(df$binnedinc), sd(df$binnedinc)), add = T)
```

![](images/unnamed-chunk-32-1.png)

``` r
shapiro.test(df$binnedinc)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$binnedinc
    ## W = 0.79199, p-value < 2.2e-16

No further discretisation is needed for this variable, as it is already
categorised.

### Variable 10 - medianage

After having cleaned it, running it through the Shapiro test shows that
it is most likely not normally distributed, altough the histogram shows
a closely bell-shaped curve.

``` r
summary(df$medianage)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   23.30   37.85   40.90   40.85   43.85   59.00

``` r
hist(df$medianage, breaks = 30, freq = F)
curve(dnorm(x, mean(df$medianage), sd(df$medianage)), add = T)
```

![](images/unnamed-chunk-33-1.png)

``` r
shapiro.test(df$medianage)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$medianage
    ## W = 0.99506, p-value = 9.423e-06

An additional factor `f.medianage` is created to discretize the data
according to the quartiles.

``` r
df$f.medianage <- discretize_quartiles(df$medianage, "Age")
```

    ## res
    ##     LowAge  LowMidAge HighMidAge    HighAge 
    ##        458        466        449        458

### Variable 11 - medianagemale

Very similar to the previous variable, this is a continuous interval
variable, but with no apparent erroneous values. It is most likely not
normally distributed, according to the Shapiro test, but, as with the
previous variable, the histogram shows a closely bell-shaped curve.

The summary shows that male median age is slightly lower than median age
(we can assume that it will also be lower than the female median age).

``` r
summary(df$medianagemale)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   23.00   36.40   39.50   39.59   42.60   60.20

``` r
hist(df$medianagemale, breaks = 30, freq = F)
curve(dnorm(x, mean(df$medianagemale), sd(df$medianagemale)), add = T)
```

![](images/unnamed-chunk-35-1.png)

``` r
shapiro.test(df$medianagemale)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$medianagemale
    ## W = 0.99404, p-value = 9.877e-07

An additional factor `f.medianagemale` is created to discretize the data
according to the quartiles.

``` r
df$f.medianagemale <- discretize_quartiles(df$medianagemale, "AgeMale")
```

    ## res
    ##     LowAgeMale  LowMidAgeMale HighMidAgeMale    HighAgeMale 
    ##            465            471            446            449

### Variable 12 - medianagefemale

Repeating the analysis of the previous two variables, it is not normally
distributed according to the Shapiro test, but the histogram, again,
shows a closely bell-shaped curve.

As expected, the female median age is slightly higher than the median
age, as well as the male median age.

``` r
summary(df$medianagefemale)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   23.60   39.20   42.40   42.17   45.30   58.20

``` r
hist(df$medianagefemale, breaks = 30, freq = F)
curve(dnorm(x, mean(df$medianagefemale), sd(df$medianagefemale)), add = T)
```

![](images/unnamed-chunk-37-1.png)

``` r
shapiro.test(df$medianagefemale)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$medianagefemale
    ## W = 0.99321, p-value = 1.817e-07

An additional factor `f.medianagefemale` is created to discretize the
data according to the quartiles.

``` r
df$f.medianagefemale <- discretize_quartiles(df$medianagefemale, "AgeFemale")
```

    ## res
    ##     LowAgeFemale  LowMidAgeFemale HighMidAgeFemale    HighAgeFemale 
    ##              460              471              448              452

#### A small addendum on the median age variables

Leaving correlation analysis for later, let’s check whether one can
assume that the expected value of the median age of a population is the
same for male as is for female populations. We’ll use a set of wilcox
tests (as we’ve already established that the data is not normally
distributed) with the null hypothesis of their means being equal.

``` r
wilcox.test(df$medianage, df$medianagefemale)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  df$medianage and df$medianagefemale
    ## W = 1402826, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(df$medianage, df$medianagemale)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  df$medianage and df$medianagemale
    ## W = 1936447, p-value = 4.192e-16
    ## alternative hypothesis: true location shift is not equal to 0

``` r
wilcox.test(df$medianagefemale, df$medianagemale)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  df$medianagefemale and df$medianagemale
    ## W = 2189083, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

The p-values are all very low, so we can safely reject the null
hypothesis and assume that the median age of a population is different
depending on the gender.

### Variable 13 - geography

This is a string variable that is unique for each row of data. Since it
is unique we could delete it, but it has info on not only the unique
county of each observation, but also on its state. We will take this
information and create a new variable named State that could be
beneficial to our analysis. The new variable is a Nominal variable
without missing values. However it has a lot of levels (50) with a few
of them sparsly populated so it’s not feasible to convert it to factor.

``` r
sample(df$geography, 10)
```

    ##  [1] "Union County, New Jersey"        "Meriwether County, Georgia"     
    ##  [3] "Carroll County, Indiana"         "Van Buren County, Tennessee"    
    ##  [5] "Graves County, Kentucky"         "Lancaster County, Nebraska"     
    ##  [7] "Hancock County, Kentucky"        "Grenada County, Mississippi"    
    ##  [9] "Carlisle County, Kentucky"       "Pleasants County, West Virginia"

``` r
# Use regex to get the state (everything after the comma and white space):
df$state <- sub(".*,\\s*", "", df$geography)

summary(df$state)
```

    ##    Length     Class      Mode 
    ##      1831 character character

``` r
table(df$state)
```

    ## 
    ##        Alabama         Alaska        Arizona       Arkansas     California 
    ##             35             10              8             41             32 
    ##       Colorado    Connecticut       Delaware        Florida        Georgia 
    ##             34              7              1             38            100 
    ##         Hawaii          Idaho       Illinois        Indiana           Iowa 
    ##              2             25             56             56             59 
    ##         Kansas       Kentucky      Louisiana          Maine       Maryland 
    ##             61             75             40             10             14 
    ##  Massachusetts       Michigan      Minnesota    Mississippi       Missouri 
    ##              8             51             51             59             66 
    ##        Montana       Nebraska         Nevada  New Hampshire     New Jersey 
    ##             22             52             14              6             11 
    ##     New Mexico       New York North Carolina   North Dakota           Ohio 
    ##             20             41             62             32             49 
    ##       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina 
    ##             45             19             42              3             31 
    ##   South Dakota      Tennessee          Texas           Utah        Vermont 
    ##             39             60            136             18              7 
    ##       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
    ##             74             22             33             41             13

``` r
unique(df$state)
```

    ##  [1] "Washington"     "West Virginia"  "Wisconsin"      "Nebraska"      
    ##  [5] "Nevada"         "New Hampshire"  "New Jersey"     "New Mexico"    
    ##  [9] "New York"       "Virginia"       "Michigan"       "Minnesota"     
    ## [13] "North Carolina" "North Dakota"   "Alabama"        "Arkansas"      
    ## [17] "California"     "Montana"        "Tennessee"      "Texas"         
    ## [21] "Louisiana"      "Maine"          "Maryland"       "Massachusetts" 
    ## [25] "Utah"           "Vermont"        "Colorado"       "Wyoming"       
    ## [29] "Mississippi"    "Missouri"       "Kansas"         "Kentucky"      
    ## [33] "Connecticut"    "Delaware"       "Florida"        "Oklahoma"      
    ## [37] "Oregon"         "Ohio"           "Pennsylvania"   "Rhode Island"  
    ## [41] "South Carolina" "Indiana"        "Iowa"           "Georgia"       
    ## [45] "Hawaii"         "Idaho"          "Illinois"       "Alaska"        
    ## [49] "Arizona"        "South Dakota"

### Variable 13 - percentmarried

Another continuous ratio variable not normally distributed.

``` r
summary(df$percentmarried)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    23.1    47.8    52.5    51.9    56.4    68.0

``` r
hist(df$percentmarried, breaks = 30, freq = F)
curve(dnorm(x, mean(df$percentmarried), sd(df$percentmarried)), add = T)
```

![](images/unnamed-chunk-41-1.png)

``` r
shapiro.test(df$percentmarried)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$percentmarried
    ## W = 0.97753, p-value = 2.346e-16

An additional factor `f.percentmarried` is created to discretize the
data according to the quartiles.

``` r
df$f.percentmarried <- discretize_quartiles(df$percentmarried, "Married%")
```

    ## res
    ##     LowMarried%  LowMidMarried% HighMidMarried%    HighMarried% 
    ##             460             459             455             457

### Variable 14 - pctnohs18_24

Another continuous ratio variable not normally distributed.

``` r
summary(df$pctnohs18_24)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.50   12.90   17.20   18.29   22.70   59.10

``` r
hist(df$pctnohs18_24, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctnohs18_24), sd(df$pctnohs18_24)), add = T)
```

![](images/unnamed-chunk-43-1.png)

``` r
shapiro.test(df$pctnohs18_24)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctnohs18_24
    ## W = 0.96205, p-value < 2.2e-16

An additional factor `f.pctnohs18_24` is created to discretize the data
according to the quartiles.

``` r
df$f.pctnohs18_24 <- discretize_quartiles(df$pctnohs18_24, "NoHighsc%")
```

    ## res
    ##     LowNoHighsc%  LowMidNoHighsc% HighMidNoHighsc%    HighNoHighsc% 
    ##              459              461              455              456

### Variable 15 - pcths18_24

Another continuous ratio variable (related to the previous one) not
normally distributed. There is one really severe outlier with 0 percent
of High School Graduates, Greeley County, Kansas. It also has only 4.8%
non High School Graduates (really low). It seems like those values could
be incorrect. For now, however, we will leave it as is and deal with it
later.

``` r
summary(df$pcths18_24)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0    29.2    34.7    35.0    40.5    72.5

``` r
hist(df$pcths18_24, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pcths18_24), sd(df$pcths18_24)), add = T)
```

![](images/unnamed-chunk-45-1.png)

``` r
shapiro.test(df$pcths18_24)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pcths18_24
    ## W = 0.99323, p-value = 1.922e-07

An additional factor `f.pcths18_24` is created to discretize the data
according to the quartiles.

``` r
df$f.pcths18_24 <- discretize_quartiles(df$pcths18_24, "Highsc%")
```

    ## res
    ##     LowHighsc%  LowMidHighsc% HighMidHighsc%    HighHighsc% 
    ##            461            463            456            451

### Variable 16 - pctsomecol18_24

This variable has been removed due to having too many missing values, so
analyzing it is left outside the scope for this project.

### Variable 17 - pcths25_over

Another continuous ratio variable not normally distributed.

``` r
summary(df$pcths25_over)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    8.30   30.35   35.30   34.73   39.65   52.70

``` r
hist(df$pcths25_over, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pcths25_over), sd(df$pcths25_over)), add = T)
```

![](images/unnamed-chunk-47-1.png)

``` r
shapiro.test(df$pcths25_over)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pcths25_over
    ## W = 0.99107, p-value = 3.741e-09

An additional factor `f.pcths25_over` is created to discretize the data
according to the quartiles.

``` r
df$f.pcths25_over <- discretize_quartiles(df$pcths25_over, "25Highsc%")
```

    ## res
    ##     Low25Highsc%  LowMid25Highsc% HighMid25Highsc%    High25Highsc% 
    ##              458              469              446              458

### Variable 18 - pctbachdeg25_over

Another continuous ratio variable (related to the previous one) not
normally distributed.

``` r
summary(df$pctbachdeg25_over)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     2.5     9.3    12.3    13.3    16.0    42.2

``` r
hist(df$pctbachdeg25_over, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctbachdeg25_over), sd(df$pctbachdeg25_over)), add = T)
```

![](images/unnamed-chunk-49-1.png)

``` r
shapiro.test(df$pctbachdeg25_over)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctbachdeg25_over
    ## W = 0.92998, p-value < 2.2e-16

An additional factor `f.pctbachdeg25_over` is created to discretize the
data according to the quartiles.

``` r
df$f.pctbachdeg25_over <- discretize_quartiles(df$pctbachdeg25_over, "Bach%")
```

    ## res
    ##     LowBach%  LowMidBach% HighMidBach%    HighBach% 
    ##          459          458          463          451

### Variable 19 - pctemployed16_over

Another continuous ratio variable not normally distributed.

``` r
summary(df$pctemployed16_over)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   23.90   48.60   54.60   54.29   60.30   80.10

``` r
hist(df$pctemployed16_over, breaks = 30, freq = F)
```

![](images/unnamed-chunk-51-1.png)

``` r
shapiro.test(df$pctemployed16_over)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctemployed16_over
    ## W = 0.99199, p-value = 1.852e-08

An additional factor `f.pctemployed16_over` is created to discretize the
data according to the quartiles.

``` r
df$f.pctemployed16_over <- discretize_quartiles(df$pctemployed16_over, "Employ%")
```

    ## res
    ##     LowEmploy%  LowMidEmploy% HighMidEmploy%    HighEmploy% 
    ##            459            464            455            453

### Variable 20 - pctunemployed16_over

One might assume that this variable is 100 minus the previous variable,
but looking at some observations this is proven to not be. It is a
continuous ratio variable not normally distributed.

``` r
summary(df$pctunemployed16_over)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.700   5.500   7.500   7.861   9.750  29.400

``` r
hist(df$pctunemployed16_over, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctunemployed16_over), sd(df$pctunemployed16_over)), add = T)
```

![](images/unnamed-chunk-53-1.png)

``` r
shapiro.test(df$pctunemployed16_over)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctunemployed16_over
    ## W = 0.9612, p-value < 2.2e-16

An additional factor `f.pctunemployed16_over` is created to discretize
the data according to the quartiles.

``` r
df$f.pctunemployed16_over <- discretize_quartiles(df$pctunemployed16_over, "Unemploy%")
```

    ## res
    ##     LowUnemploy%  LowMidUnemploy% HighMidUnemploy%    HighUnemploy% 
    ##              467              453              453              458

### Variable 21 - pctprivatecoverage

Another continuous ratio variable not normally distributed.

``` r
summary(df$pctprivatecoverage)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   23.40   57.50   65.20   64.47   72.10   89.60

``` r
hist(df$pctprivatecoverage, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctprivatecoverage), sd(df$pctprivatecoverage)), add = T)
```

![](images/unnamed-chunk-55-1.png)

``` r
shapiro.test(df$pctprivatecoverage)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctprivatecoverage
    ## W = 0.98964, p-value = 3.725e-10

An additional factor `f.pctprivatecoverage` is created to discretize the
data according to the quartiles.

``` r
df$f.pctprivatecoverage <- discretize_quartiles(df$pctprivatecoverage, "Private%")
```

    ## res
    ##     LowPrivate%  LowMidPrivate% HighMidPrivate%    HighPrivate% 
    ##             460             464             451             456

### Variable 22 - pctprivatecoveragealone

This is a continuous ratio variable very closely related with the
previous variable. In the data quality section, this variable was shown
to have a high amount of missing data, but it was imputed nontheless.
However, it has a 0.93 correlation with variable `pctprivatecoverage`,
which is high enough to consider removing it for being redundant.

``` r
summary(df$pctprivatecoveragealone)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.80   41.20   48.90   48.57   55.60   78.90

``` r
cor.test(df$pctprivatecoverage, df$pctprivatecoveragealone)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$pctprivatecoverage and df$pctprivatecoveragealone
    ## t = 110.02, df = 1829, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.9257713 0.9378207
    ## sample estimates:
    ##       cor 
    ## 0.9320533

``` r
df <- subset(df, select = -pctprivatecoveragealone)
```

### Variable 23 - pctempprivcoverage

Another continuous ratio variable normally distributed (with a 99%
confidence level for the shapiro test).

``` r
summary(df$pctempprivcoverage)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   14.30   34.60   41.10   41.29   47.70   70.20

``` r
hist(df$pctempprivcoverage, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctempprivcoverage), sd(df$pctempprivcoverage)), add = T)
```

![](images/unnamed-chunk-58-1.png)

``` r
shapiro.test(df$pctempprivcoverage)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctempprivcoverage
    ## W = 0.99807, p-value = 0.02861

An additional factor `f.pctempprivcoverage` is created to discretize the
data according to the quartiles.

``` r
df$f.pctempprivcoverage <- discretize_quartiles(df$pctempprivcoverage, "EmployeeHealth%")
```

    ## res
    ##     LowEmployeeHealth%  LowMidEmployeeHealth% HighMidEmployeeHealth% 
    ##                    465                    454                    456 
    ##    HighEmployeeHealth% 
    ##                    456

### Variable 24 - pctpubliccoverage

Another continuous ratio variable normally distributed, this time with a
very high p-value for the shapiro test.

``` r
summary(df$pctpubliccoverage)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.20   30.90   36.30   36.15   41.40   62.70

``` r
hist(df$pctpubliccoverage, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctpubliccoverage), sd(df$pctpubliccoverage)), add = T)
```

![](images/unnamed-chunk-60-1.png)

``` r
shapiro.test(df$pctpubliccoverage)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctpubliccoverage
    ## W = 0.99947, p-value = 0.9186

An additional factor `f.pctpubliccoverage` is created to discretize the
data according to the quartiles.

``` r
df$f.pctpubliccoverage <- discretize_quartiles(df$pctpubliccoverage, "GovHealth%")
```

    ## res
    ##     LowGovHealth%  LowMidGovHealth% HighMidGovHealth%    HighGovHealth% 
    ##               463               459               454               455

### Variable 25 - pctpubliccoveragealone

Another continuous ratio variable related to the previous variable with
a 0.87 correlation. It is not normally distributed.

``` r
summary(df$pctpubliccoveragealone)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    2.60   14.90   18.70   19.15   23.00   46.60

``` r
cor.test(df$pctpubliccoverage, df$pctpubliccoveragealone)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$pctpubliccoverage and df$pctpubliccoveragealone
    ## t = 74.592, df = 1829, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.8557240 0.8784263
    ## sample estimates:
    ##       cor 
    ## 0.8675263

``` r
hist(df$pctpubliccoveragealone, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctpubliccoveragealone), sd(df$pctpubliccoveragealone)), add = T)
```

![](images/unnamed-chunk-62-1.png)

``` r
shapiro.test(df$pctpubliccoveragealone)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctpubliccoveragealone
    ## W = 0.98784, p-value = 2.648e-11

An additional factor `f.pctpubliccoveragealone` is created to discretize
the data according to the quartiles.

``` r
df$f.pctpubliccoveragealone <- discretize_quartiles(df$pctpubliccoveragealone, "GovHealthAlone%")
```

    ## res
    ##     LowGovHealthAlone%  LowMidGovHealthAlone% HighMidGovHealthAlone% 
    ##                    463                    463                    455 
    ##    HighGovHealthAlone% 
    ##                    450

### Variable 25 - pctwhite

Another continuous ratio variable clearly not normally distributed.

``` r
summary(df$pctwhite)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   12.27   77.31   89.90   83.85   95.57   99.69

``` r
hist(df$pctwhite, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctwhite), sd(df$pctwhite)), add = T)
```

![](images/unnamed-chunk-64-1.png)

``` r
shapiro.test(df$pctwhite)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctwhite
    ## W = 0.80758, p-value < 2.2e-16

An additional factor `f.pctwhite` is created to discretize the data
according to the quartiles.

``` r
df$f.pctwhite <- discretize_quartiles(df$pctwhite, "White%")
```

    ## res
    ##     LowWhite%  LowMidWhite% HighMidWhite%    HighWhite% 
    ##           458           458           457           458

### Variable 26 - pctblack

This one is really similar to the previous variable, with a correlation
of -0.84. It is another continuous ratio variable clearly not normally
distributed.

``` r
summary(df$pctblack)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.648   2.323   9.082  10.867  85.948

``` r
cor.test(df$pctwhite, df$pctblack)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$pctwhite and df$pctblack
    ## t = -67.439, df = 1829, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8571535 -0.8308366
    ## sample estimates:
    ##        cor 
    ## -0.8445041

``` r
hist(df$pctblack, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctblack), sd(df$pctblack)), add = T)
```

![](images/unnamed-chunk-66-1.png)

``` r
shapiro.test(df$pctblack)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctblack
    ## W = 0.65926, p-value < 2.2e-16

An additional factor `f.pctblack` is created to discretize the data
according to the quartiles.

``` r
df$f.pctblack <- discretize_quartiles(df$pctblack, "Black%")
```

    ## res
    ##     LowBlack%  LowMidBlack% HighMidBlack%    HighBlack% 
    ##           458           458           457           458

### Variable 27 - pctasian

Also related to the previous 2 variables. It is a continuous ratio
variable clearly not normally distributed. Looking at the boxplot, there
are some points with a high asian population percentage (probably those
from asian ghetto counties), but none of them higher than 100%.

``` r
summary(df$pctasian)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.2582  0.5495  1.2743  1.2515 37.1569

``` r
boxplot(df$pctasian, horizontal=T)
```

![](images/unnamed-chunk-68-1.png)

``` r
hist(df$pctasian, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctasian), sd(df$pctasian)), add = T)
```

![](images/unnamed-chunk-68-2.png)

``` r
shapiro.test(df$pctasian)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctasian
    ## W = 0.41908, p-value < 2.2e-16

An additional factor `f.pctasian` is created to discretize the data
according to the quartiles.

``` r
df$f.pctasian <- discretize_quartiles(df$pctasian, "Asian%")
```

    ## res
    ##     LowAsian%  LowMidAsian% HighMidAsian%    HighAsian% 
    ##           458           458           457           458

### Variable 28 - pctotherrace

This variable should be 100 minus the sum of the three previous
variables but looking at a sample of observations it is clearly not, and
also if we check for multicollinearity using VIF, since the values are
lower than 5 we can use the rule of thumb to say that there is not a
severe multicollinearity so we will keep the variable for now (if it was
always equal to 100 we would erase it since it wouldn’t add any new
info).

The variable is a continuous ratio variable clearly not normally
distributed.

``` r
summary(df$pctotherrace)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.0000  0.2867  0.7826  2.0031  2.1066 41.9303

``` r
model <- lm(pctotherrace ~ pctwhite + pctblack + pctasian, data=df)
vif(model)
```

    ## pctwhite pctblack pctasian 
    ## 4.501114 4.193772 1.291071

``` r
hist(df$pctotherrace, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctotherrace), sd(df$pctotherrace)), add = T)
```

![](images/unnamed-chunk-70-1.png)

``` r
shapiro.test(df$pctotherrace)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctotherrace
    ## W = 0.50981, p-value < 2.2e-16

An additional factor `f.pctotherrace` is created to discretize the data
according to the quartiles.

``` r
df$f.pctotherrace <- discretize_quartiles(df$pctotherrace, "OtherRace%")
```

    ## res
    ##     LowOtherRace%  LowMidOtherRace% HighMidOtherRace%    HighOtherRace% 
    ##               458               458               457               458

#### County race clustering

Having discretized the previous race-related variables, we’ll define a
new factor variable called `f.race` which will probably come in handy in
future analysis. This variable will have 4 levels: “White”, “Black”,
“Asian” and “Other”, which will be decided based on the maximum value of
the 4 columns.

``` r
getRace <- function (row) {
  races = row[c("pctwhite", "pctblack", "pctasian", "pctotherrace")]
  max_race = which.max(races)
  return(c("White", "Black", "Asian", "Other")[max_race])
}

df$f.race <- as.factor(apply(df, 1, getRace))
table(df$f.race)
```

    ## 
    ## Asian Black White 
    ##     2    66  1763

As expected, the majority of the counties are predominantly white,
followed by those with a black majority. The number of counties with an
asian majority is negligible, and there are no counties with an “other”
majority.

### Variable 29 - pctmarriedhouseholds

Another continuous ratio variable not normally distributed.

``` r
summary(df$pctmarriedhouseholds)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   22.99   47.85   51.73   51.40   55.48   71.40

``` r
hist(df$pctmarriedhouseholds, breaks = 30, freq = F)
curve(dnorm(x, mean(df$pctmarriedhouseholds), sd(df$pctmarriedhouseholds)), add = T)
```

![](images/unnamed-chunk-73-1.png)

``` r
shapiro.test(df$pctmarriedhouseholds)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$pctmarriedhouseholds
    ## W = 0.9816, p-value = 1.341e-14

An additional factor `f.pctmarriedhouseholds` is created to discretize
the data according to the quartiles.

``` r
df$f.pctmarriedhouseholds <- discretize_quartiles(df$pctmarriedhouseholds, "Married%")
```

    ## res
    ##     LowMarried%  LowMidMarried% HighMidMarried%    HighMarried% 
    ##             458             458             457             458

### Variable 30 - birthrate

The last variable is yet another continuous ratio variable not normally
distributed.

``` r
summary(df$birthrate)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   4.528   5.355   5.597   6.414  21.326

``` r
hist(df$birthrate, breaks = 30, freq = F)
curve(dnorm(x, mean(df$birthrate), sd(df$birthrate)), add = T)
```

![](images/unnamed-chunk-75-1.png)

``` r
shapiro.test(df$birthrate)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$birthrate
    ## W = 0.93107, p-value < 2.2e-16

An additional factor `f.birthrate` is created to discretize the data
according to the quartiles.

``` r
df$f.birthrate <- discretize_quartiles(df$birthrate, "Birth%")
```

    ## res
    ##     LowBirth%  LowMidBirth% HighMidBirth%    HighBirth% 
    ##           458           458           457           458

## Autocorrelation

Before proceeding with the multivariate analysis, let’s check for
autocorrelation in the target variable. We’ll use the `acf` function to
plot the correlation of the target variable with itself at different
lags.

``` r
acf(df$target_deathrate, type="correlation", plot=T, main="Autocorrelation of Target Death Rate")
```

![](images/unnamed-chunk-77-1.png)

The plot shows that there is a slight positive correlation for lag
values lower than 22, although none of them exceeds 0.35. This is not a
significant autocorrelation.

## Profiling

Let us start by profiling the target variable with respect to the
others, using the function `condes` from the `FactoMineR` package.

``` r
num.df = Filter(is.numeric, df)
res.con = condes(num.df,num.var = which(colnames(num.df) == "target_deathrate"))

correlation_df = as.data.frame(res.con$quanti)
high_correlation_df <- correlation_df[abs(correlation_df$correlation) > 0.35, ]
sorted_correlation_df <- high_correlation_df[order(abs(high_correlation_df$correlation), decreasing = TRUE), ]
sorted_correlation_df
```

    ##                        correlation       p.value
    ## pctbachdeg25_over       -0.4977716 3.106333e-115
    ## pctpubliccoveragealone   0.4662958  1.687857e-99
    ## povertypercent           0.4500846  5.144264e-92
    ## incidencerate            0.4495485  8.953861e-92
    ## medincome               -0.4436828  3.606607e-89
    ## pctemployed16_over      -0.4425599  1.122079e-88
    ## pctpubliccoverage        0.4243675  6.056303e-81
    ## pcths25_over             0.4069017  5.910723e-74
    ## binnedinc               -0.4046327  4.462085e-73
    ## pctprivatecoverage      -0.4035415  1.173008e-72
    ## pctunemployed16_over     0.3978068  1.777035e-70

Setting an arbitrary threshold of 0.35, we can see that the variables
with the highest correlation with the target variable are
`pctbachdeg25_over`, `medincome`, `pctemployed16_over`, `binnedinc` and
`pctprivatecoverage` negatively, and `pctpubliccoveragealone`,
`povertypercent`, `incidencerate`, `pctpubliccoverage`, `pcths25_over`
and `pctunemployed16_over` positively; all of them with a significance
level much lower than 1%.

## Multicolinearity Analysis

Before proceeding, we must identify those variables that are very
correlated and combine them. We do so because high correlations (close
to 1 or -1) between two or more predictors indicate potential
multicollinearity.

``` r
cor_matrix <- cor(Filter(is.numeric, df))
corrplot(cor_matrix, method = "circle")
```

![](images/unnamed-chunk-79-1.png)

Initially, let us combine or eliminate those variables with a
correlation above 0.9 or below -0.9.

``` r
high_corr_vars <- data.frame(l=character(), r=character())

for(i in 1:ncol(cor_matrix)){
  for(j in i:ncol(cor_matrix)){
    if(abs(cor_matrix[i,j]) > 0.9 & i != j){
      high_corr_vars <- rbind(high_corr_vars, data.frame(l=colnames(cor_matrix)[i], r=colnames(cor_matrix)[j]))
    }
  }
}

high_corr_vars
```

    ##                  l                r
    ## 1      avganncount avgdeathsperyear
    ## 2      avganncount       popest2015
    ## 3 avgdeathsperyear       popest2015
    ## 4        medincome        binnedinc
    ## 5        medianage    medianagemale
    ## 6        medianage  medianagefemale
    ## 7    medianagemale  medianagefemale

We can see that the following variables have a high likelihood of
multicollinearity:

``` r
unique(c(high_corr_vars$l, high_corr_vars$r))
```

    ## [1] "avganncount"      "avgdeathsperyear" "medincome"        "medianage"       
    ## [5] "medianagemale"    "popest2015"       "binnedinc"        "medianagefemale"

Let us treat each high correlation pair individually:

-   `binnedinc` is highly correlated with `medincome`, as expected. We
    won’t remove any of them, as one is presumably a factorized version
    of the other.

-   `medianage`, `medianagemale` and `medianagefemale` are highly
    correlated with each other. They are almost the same variable. Also,
    all three of them are very poorly correlated with the target
    variable. We will remove at least the two gender-specific variables.

``` r
par(mfrow = c(1, 2))
plot(df$medianage, df$medianagemale)
plot(df$medianage, df$medianagefemale)
```

![](images/unnamed-chunk-82-1.png)

``` r
par(mfrow = c(1, 3))
plot(df$medianage, df$target_deathrate)
plot(df$medianagemale, df$target_deathrate)
plot(df$medianagefemale, df$target_deathrate)
```

![](images/unnamed-chunk-83-1.png)

``` r
df <- subset(df, select = -c(medianagemale, medianagefemale))
```

-   `avganncount`, `avgdeathsperyear` and `popest2015` are highly
    correlated with each other. This is expected, as the number of cases
    and deaths is directly related to the population. However, we won’t
    be removing any of them, as there might be some information in the
    ratio of cases to population.

``` r
par(mfrow = c(1, 2))
plot(df$popest2015, df$avgdeathsperyear)
plot(df$popest2015, df$avganncount)
```

![](images/unnamed-chunk-85-1.png)

## Combining Variables

Finally, we will combine those variables that are syntactically related,
such as the percentage of people with a high school education in the
18-24 age range and the percentage of people with a bachelor’s degree in
the 25 and over age range into a single variable representing the
percentage of people with a high school education.

``` r
# Education-related variables
df$pcths <- df$pcths18_24 + df$pcths25_over
df$pctbach <- df$pctbachdeg18_24 + df$pctbachdeg25_over

# Race and Ethnicity-related Variables
df$racindex <- df$pctblack + df$pctasian + df$pctotherrace

# Public Coverage and Poverty-related Variables
df$social_welfare <- df$pctpubliccoverage + df$povertypercent

new_cor_matrix <- cor(Filter(is.numeric, df))
corrplot(new_cor_matrix, method = "circle")
```

![](images/unnamed-chunk-87-1.png)

# Model Fitting

Let’s start by exploring a linear model with all the variables, with the
intention of getting a first glance at the most significant predictors.

``` r
base_df = Filter(is.numeric, df)
base_df$f.race = df$f.race
model <- lm(target_deathrate ~ ., data = base_df)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ ., data = base_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -99.306 -10.946  -0.497  10.570 133.185 
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.059e+02  2.665e+01   7.728 1.80e-14 ***
    ## avganncount            -3.054e-03  9.753e-04  -3.131 0.001771 ** 
    ## avgdeathsperyear        1.460e-02  4.679e-03   3.121 0.001830 ** 
    ## incidencerate           1.868e-01  9.193e-03  20.314  < 2e-16 ***
    ## medincome               1.220e-04  1.379e-04   0.885 0.376281    
    ## popest2015             -1.095e-05  6.378e-06  -1.717 0.086226 .  
    ## povertypercent          3.550e-01  2.173e-01   1.634 0.102486    
    ## studypercap             1.272e-03  9.357e-04   1.360 0.174109    
    ## binnedinc              -2.429e-06  7.185e-05  -0.034 0.973032    
    ## medianage              -5.260e-01  2.020e-01  -2.604 0.009280 ** 
    ## percentmarried          1.359e+00  2.227e-01   6.100 1.30e-09 ***
    ## pctnohs18_24           -1.059e-01  7.331e-02  -1.445 0.148664    
    ## pcths18_24              1.488e-01  6.546e-02   2.274 0.023103 *  
    ## pctbachdeg18_24        -5.578e-02  1.435e-01  -0.389 0.697610    
    ## pcths25_over            4.341e-01  1.268e-01   3.424 0.000631 ***
    ## pctbachdeg25_over      -1.087e+00  2.014e-01  -5.399 7.61e-08 ***
    ## pctemployed16_over     -7.750e-01  1.462e-01  -5.303 1.28e-07 ***
    ## pctunemployed16_over    7.299e-02  2.184e-01   0.334 0.738256    
    ## pctprivatecoverage     -3.653e-01  1.726e-01  -2.117 0.034392 *  
    ## pctempprivcoverage      3.363e-01  1.328e-01   2.532 0.011411 *  
    ## pctpubliccoverage      -4.508e-01  2.957e-01  -1.525 0.127535    
    ## pctpubliccoveragealone  5.920e-01  3.641e-01   1.626 0.104109    
    ## pctwhite               -1.305e-01  8.152e-02  -1.601 0.109648    
    ## pctblack               -3.872e-03  8.293e-02  -0.047 0.962766    
    ## pctasian               -2.015e-01  2.925e-01  -0.689 0.490997    
    ## pctotherrace           -7.343e-01  1.599e-01  -4.592 4.70e-06 ***
    ## pctmarriedhouseholds   -1.264e+00  2.109e-01  -5.993 2.48e-09 ***
    ## birthrate              -8.063e-01  2.587e-01  -3.117 0.001856 ** 
    ## pcths                          NA         NA      NA       NA    
    ## pctbach                        NA         NA      NA       NA    
    ## racindex                       NA         NA      NA       NA    
    ## social_welfare                 NA         NA      NA       NA    
    ## f.raceBlack            -3.946e+01  1.751e+01  -2.254 0.024310 *  
    ## f.raceWhite            -3.669e+01  1.710e+01  -2.146 0.032042 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.48 on 1801 degrees of freedom
    ## Multiple R-squared:  0.5212, Adjusted R-squared:  0.5135 
    ## F-statistic:  67.6 on 29 and 1801 DF,  p-value: < 2.2e-16

According to this first model, the following variables seem to be very
significant (p-value \< 0.01):

``` r
coefs <- summary(model)$coefficients
significant_vars <- coefs[coefs[,'Pr(>|t|)'] < 0.01,]
significant_vars
```

    ##                           Estimate   Std. Error   t value     Pr(>|t|)
    ## (Intercept)          205.947058223 2.664941e+01  7.728017 1.802086e-14
    ## avganncount           -0.003053622 9.753439e-04 -3.130815 1.771208e-03
    ## avgdeathsperyear       0.014602414 4.678610e-03  3.121101 1.830361e-03
    ## incidencerate          0.186751899 9.193288e-03 20.313939 9.041513e-83
    ## medianage             -0.525956529 2.019505e-01 -2.604383 9.279675e-03
    ## percentmarried         1.358551800 2.227128e-01  6.100017 1.295004e-09
    ## pcths25_over           0.434050740 1.267698e-01  3.423927 6.310610e-04
    ## pctbachdeg25_over     -1.087338870 2.014136e-01 -5.398538 7.609898e-08
    ## pctemployed16_over    -0.775014452 1.461514e-01 -5.302818 1.280887e-07
    ## pctotherrace          -0.734327174 1.599165e-01 -4.591940 4.695570e-06
    ## pctmarriedhouseholds  -1.264045798 2.109088e-01 -5.993329 2.477433e-09
    ## birthrate             -0.806281307 2.586690e-01 -3.117038 1.855635e-03

## Analyze behaviour of what seem to be the main predictors

``` r
#Boxplot most relevant predictors
boxplot(target_deathrate ~ f.incidencerate, data = df, main = "Death Rate vs. Incidence Rate")
```

![](images/unnamed-chunk-90-1.png)

``` r
boxplot(target_deathrate ~ f.medincome, data = df, main = "Death Rate vs. Median Income")
```

![](images/unnamed-chunk-90-2.png)

``` r
boxplot(target_deathrate ~ f.percentmarried, data = df, main = "Death Rate vs. Percent Married")
```

![](images/unnamed-chunk-90-3.png)

``` r
boxplot(target_deathrate ~ f.pcths18_24, data = df, main = "Death Rate vs. Percent 18-24 HS")
```

![](images/unnamed-chunk-90-4.png)

``` r
boxplot(target_deathrate ~ f.povertypercent, data = df, main = "Death Rate vs. Percent 18-24 HS")
```

![](images/unnamed-chunk-90-5.png)

``` r
boxplot(target_deathrate ~ f.pctpubliccoverage, data = df, main = "Death Rate vs. Percent 18-24 HS")
```

![](images/unnamed-chunk-90-6.png)

``` r
boxplot(target_deathrate ~ f.pctpubliccoveragealone, data = df, main = "Death Rate vs. Percent 18-24 HS")
```

![](images/unnamed-chunk-90-7.png)

``` r
#Pairwise tests
#The ANOVA result confirms that marital status percentage (f.percentmarried) has a significant effect on the target_deathrate, suggesting that different levels of marriage percentages correspond to different death rate levels.
pairwise.wilcox.test(df$target_deathrate, df$f.percentmarried)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.percentmarried 
    ## 
    ##                 LowMarried% LowMidMarried% HighMidMarried%
    ## LowMidMarried%  0.16539     -              -              
    ## HighMidMarried% 0.00015     0.00407        -              
    ## HighMarried%    < 2e-16     < 2e-16        2.3e-08        
    ## 
    ## P value adjustment method: holm

``` r
oneway.test(target_deathrate ~ f.percentmarried, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.percentmarried
    ## F = 32.419, num df = 3.0, denom df = 1012.3, p-value < 2.2e-16

``` r
# Incidence Rate
#ANOVA results indicate that the f.incidencerate factor has a statistically significant impact on target_deathrate.
pairwise.wilcox.test(df$target_deathrate, df$f.incidencerate)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.incidencerate 
    ## 
    ##                    LowDiagnPerCap LowMidDiagnPerCap HighMidDiagnPerCap
    ## LowMidDiagnPerCap  1.4e-13        -                 -                 
    ## HighMidDiagnPerCap < 2e-16        7.8e-10           -                 
    ## HighDiagnPerCap    < 2e-16        < 2e-16           1.2e-05           
    ## 
    ## P value adjustment method: holm

``` r
oneway.test(target_deathrate ~ f.incidencerate, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.incidencerate
    ## F = 113.93, num df = 3.00, denom df = 989.62, p-value < 2.2e-16

``` r
# Median Income
# ANOVA tests indicate that f.medincome (median income) has a statistically significant impact on target_deathrate. Specifically, different levels of median income (such as LowMedianInc, LowMidMedianInc, etc.) are associated with different death rates, with higher income groups generally showing lower death rates.
pairwise.wilcox.test(df$target_deathrate, df$f.medincome)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.medincome 
    ## 
    ##                  LowMedianInc LowMidMedianInc HighMidMedianInc
    ## LowMidMedianInc  1.8e-13      -               -               
    ## HighMidMedianInc < 2e-16      1.1e-11         -               
    ## HighMedianInc    < 2e-16      < 2e-16         4.9e-11         
    ## 
    ## P value adjustment method: holm

``` r
oneway.test(target_deathrate ~ f.medincome, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.medincome
    ## F = 133.32, num df = 3.0, denom df = 1009.9, p-value < 2.2e-16

``` r
# Percent 18-24 HS
#ANOVA indicate a significant relationship between f.pcths18_24 (the percentage of people with high school education in the 18-24 age range) and target_deathrate. The results suggest that as the percentage of high school education increases, the death rates tend to decrease
pairwise.wilcox.test(df$target_deathrate, df$f.pcths18_24)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.pcths18_24 
    ## 
    ##                LowHighsc% LowMidHighsc% HighMidHighsc%
    ## LowMidHighsc%  1.3e-06    -             -             
    ## HighMidHighsc% 1.1e-15    0.00014       -             
    ## HighHighsc%    < 2e-16    1.6e-08       0.02883       
    ## 
    ## P value adjustment method: holm

``` r
oneway.test(target_deathrate ~ f.pcths18_24, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.pcths18_24
    ## F = 36.623, num df = 3.0, denom df = 1009.7, p-value < 2.2e-16

``` r
pairwise.wilcox.test(df$target_deathrate, df$f.pctpubliccoverage)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.pctpubliccoverage 
    ## 
    ##                   LowGovHealth% LowMidGovHealth% HighMidGovHealth%
    ## LowMidGovHealth%  1.1e-13       -                -                
    ## HighMidGovHealth% < 2e-16       3.8e-08          -                
    ## HighGovHealth%    < 2e-16       < 2e-16          3.8e-08          
    ## 
    ## P value adjustment method: holm

``` r
oneway.test(target_deathrate ~ f.pctpubliccoverage, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.pctpubliccoverage
    ## F = 104.8, num df = 3.0, denom df = 1009.4, p-value < 2.2e-16

``` r
pairwise.wilcox.test(df$target_deathrate, df$f.pctpubliccoverage)
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.pctpubliccoverage 
    ## 
    ##                   LowGovHealth% LowMidGovHealth% HighMidGovHealth%
    ## LowMidGovHealth%  1.1e-13       -                -                
    ## HighMidGovHealth% < 2e-16       3.8e-08          -                
    ## HighGovHealth%    < 2e-16       < 2e-16          3.8e-08          
    ## 
    ## P value adjustment method: holm

``` r
oneway.test(target_deathrate ~ f.pctpubliccoverage, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.pctpubliccoverage
    ## F = 104.8, num df = 3.0, denom df = 1009.4, p-value < 2.2e-16

## No-interaction models

``` r
# Linear model: target_deathrate ~ f.percentmarried --> 5.47%
lm_percentmarried <- lm(target_deathrate ~ f.percentmarried, data = df)
summary(lm_percentmarried)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ f.percentmarried, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -126.034  -17.104   -0.734   15.323  177.066 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      185.734      1.268 146.441  < 2e-16 ***
    ## f.percentmarriedLowMidMarried%    -2.867      1.795  -1.597     0.11    
    ## f.percentmarriedHighMidMarried%   -7.949      1.799  -4.419 1.05e-05 ***
    ## f.percentmarriedHighMarried%     -16.911      1.797  -9.413  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.2 on 1827 degrees of freedom
    ## Multiple R-squared:  0.053,  Adjusted R-squared:  0.05145 
    ## F-statistic: 34.09 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
# Linear model: target_deathrate ~ f.incidencerate --> 15%
lm_incidencerate <- lm(target_deathrate ~ f.incidencerate, data = df)
summary(lm_incidencerate)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ f.incidencerate, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -102.869  -16.363   -1.569   15.444  169.131 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        162.569      1.185 137.155  < 2e-16 ***
    ## f.incidencerateLowMidDiagnPerCap    12.687      1.616   7.849 7.09e-15 ***
    ## f.incidencerateHighMidDiagnPerCap   23.078      1.765  13.077  < 2e-16 ***
    ## f.incidencerateHighDiagnPerCap      31.100      1.678  18.533  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.42 on 1827 degrees of freedom
    ## Multiple R-squared:  0.1729, Adjusted R-squared:  0.1716 
    ## F-statistic: 127.3 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
# Linear model: target_deathrate ~ f.medincome --> 19%
lm_medincome <- lm(target_deathrate ~ f.medincome, data = df)
summary(lm_medincome)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ f.medincome, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -129.607  -13.704    0.399   14.741  179.853 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  195.907      1.177 166.383  < 2e-16 ***
    ## f.medincomeLowMidMedianInc   -12.960      1.665  -7.783 1.17e-14 ***
    ## f.medincomeHighMidMedianInc  -22.706      1.666 -13.628  < 2e-16 ***
    ## f.medincomeHighMedianInc     -32.695      1.665 -19.635  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.2 on 1827 degrees of freedom
    ## Multiple R-squared:  0.1874, Adjusted R-squared:  0.1861 
    ## F-statistic: 140.4 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
# Linear model: target_deathrate ~ f.pcths18_24 --> 5.1%
lm_pcths18_24 <- lm(target_deathrate ~ f.pcths18_24, data = df)
summary(lm_pcths18_24)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ f.pcths18_24, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -109.408  -16.152   -0.265   15.215  175.495 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 169.108      1.262 134.015  < 2e-16 ***
    ## f.pcths18_24LowMidHighsc%     7.131      1.783   4.000 6.58e-05 ***
    ## f.pcths18_24HighMidHighsc%   13.757      1.789   7.688 2.43e-14 ***
    ## f.pcths18_24HighHighsc%      18.196      1.794  10.141  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.09 on 1827 degrees of freedom
    ## Multiple R-squared:  0.06059,    Adjusted R-squared:  0.05904 
    ## F-statistic: 39.28 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
# Linear model: target_deathrate ~ f.pctpubliccoverage --> 15%
lm_pctpubliccoverage <- lm(target_deathrate ~ f.pctpubliccoverage, data = df) 
summary(lm_pctpubliccoverage)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ f.pctpubliccoverage, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.889  -14.282    0.811   15.105  187.611 
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                           163.674      1.195 136.937  < 2e-16 ***
    ## f.pctpubliccoverageLowMidGovHealth%    11.515      1.694   6.797 1.44e-11 ***
    ## f.pctpubliccoverageHighMidGovHealth%   19.549      1.699  11.508  < 2e-16 ***
    ## f.pctpubliccoverageHighGovHealth%      29.825      1.698  17.568  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.72 on 1827 degrees of freedom
    ## Multiple R-squared:  0.1535, Adjusted R-squared:  0.1521 
    ## F-statistic: 110.4 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
# Linear model: target_deathrate ~ f.pctpubliccoveragealone --> 18.85%
lm_pctpubliccoveragealone <- lm(target_deathrate ~ f.pctpubliccoveragealone, data = df) 
summary(lm_pctpubliccoveragealone)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ f.pctpubliccoveragealone, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -117.517  -14.276    0.483   14.724  178.983 
    ## 
    ## Coefficients:
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                     162.735      1.174 138.580
    ## f.pctpubliccoveragealoneLowMidGovHealthAlone%    11.401      1.661   6.865
    ## f.pctpubliccoveragealoneHighMidGovHealthAlone%   21.082      1.668  12.639
    ## f.pctpubliccoveragealoneHighGovHealthAlone%      32.401      1.673  19.371
    ##                                                Pr(>|t|)    
    ## (Intercept)                                     < 2e-16 ***
    ## f.pctpubliccoveragealoneLowMidGovHealthAlone%  9.08e-12 ***
    ## f.pctpubliccoveragealoneHighMidGovHealthAlone%  < 2e-16 ***
    ## f.pctpubliccoveragealoneHighGovHealthAlone%     < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.27 on 1827 degrees of freedom
    ## Multiple R-squared:  0.1829, Adjusted R-squared:  0.1816 
    ## F-statistic: 136.3 on 3 and 1827 DF,  p-value: < 2.2e-16

## Interaction models

``` r
m1 <- lm(target_deathrate ~ medincome, data = df)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ medincome, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -125.59  -14.34    0.74   14.73  176.88 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.263e+02  2.317e+00   97.66   <2e-16 ***
    ## medincome   -1.004e-03  4.742e-05  -21.17   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.04 on 1829 degrees of freedom
    ## Multiple R-squared:  0.1969, Adjusted R-squared:  0.1964 
    ## F-statistic: 448.3 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
par(mar = c(5, 5, 4, 2)) 
boxcox(target_deathrate ~ medincome , data = df) #No transformation of the target needed
```

![](images/unnamed-chunk-92-1.png)

``` r
m2 <- lm(target_deathrate ~ medincome + incidencerate, data=df)
boxcox(target_deathrate ~ medincome + incidencerate, data=df) #No transformation of the target needed. Log del target descartat
```

![](images/unnamed-chunk-92-2.png)

``` r
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ medincome + incidencerate, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -141.750  -12.714   -0.495   12.530  130.239 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.263e+02  4.602e+00   27.45   <2e-16 ***
    ## medincome     -9.848e-04  4.130e-05  -23.84   <2e-16 ***
    ## incidencerate  2.206e-01  9.129e-03   24.17   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.8 on 1828 degrees of freedom
    ## Multiple R-squared:  0.3914, Adjusted R-squared:  0.3907 
    ## F-statistic: 587.7 on 2 and 1828 DF,  p-value: < 2.2e-16

``` r
t <- summary(m2)
vif(m2)
```

    ##     medincome incidencerate 
    ##      1.000375      1.000375

``` r
1/(1-t$r.squared)
```

    ## [1] 1.643051

``` r
anova(m1, m2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ medincome
    ## Model 2: target_deathrate ~ medincome + incidencerate
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1829 1146569                                  
    ## 2   1828  868870  1    277699 584.25 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#The residuals indicate a fairly balanced distribution around zero, though the maximum residual of 140.048 suggests some outliers. The coefficients show that all predictors are statistically significant with p-values < 0.001. 
m3 <- lm(target_deathrate ~ medincome + incidencerate + pcths, data=df)
summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ medincome + incidencerate + pcths, 
    ##     data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -118.786  -12.049   -0.291   12.233  140.335 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    9.140e+01  5.493e+00   16.64   <2e-16 ***
    ## medincome     -8.115e-04  4.308e-05  -18.84   <2e-16 ***
    ## incidencerate  2.136e-01  8.872e-03   24.08   <2e-16 ***
    ## pcths          4.283e-01  3.931e-02   10.89   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.13 on 1827 degrees of freedom
    ## Multiple R-squared:  0.4285, Adjusted R-squared:  0.4276 
    ## F-statistic: 456.6 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
#The Breusch-Pagan test was conducted to check for heteroscedasticity (non-constant variance of residuals). With a p-value of 0.0086, the test indicates evidence of heteroscedasticity in the model. This suggests that the residuals’ variance is not constant, potentially violating one of the assumptions of linear regression
bptest(m3)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  m3
    ## BP = 4.5799, df = 3, p-value = 0.2053

``` r
#The reduction in residual sum of squares (RSS) from 1,008,488 in m1 to 724,598 in m3 is significant (p-value < 2.2e-16). The 
#F-statistic of 342.62 confirms that adding incidencerate and pcths significantly improves the model.
anova(m1, m3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ medincome
    ## Model 2: target_deathrate ~ medincome + incidencerate + pcths
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1829 1146569                                  
    ## 2   1827  815868  2    330701 370.28 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#The VIF values for all predictors in the model (medincome, incidencerate, and pcths) are below 1.2, far below the common threshold of 5 or 10 for multicollinearity concerns. This indicates that the predictors are not highly correlated and do not pose a multicollinearity issue in the regression
vif(m3)
```

    ##     medincome incidencerate         pcths 
    ##      1.158362      1.005671      1.164403

``` r
t <- summary(m3)
1/(1-t$r.squared)
```

    ## [1] 1.749791

``` r
lm_stepwise <- step(m3, direction = "both", trace = 0)
plot(lm_stepwise)
```

![](images/unnamed-chunk-92-3.png)![](images/unnamed-chunk-92-4.png)![](images/unnamed-chunk-92-5.png)![](images/unnamed-chunk-92-6.png)

``` r
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                  Estimate  Std. Error  t value  Pr(>|t|)    
    ## (Intercept)    9.1399e+01  7.1680e+00  12.7510 < 2.2e-16 ***
    ## medincome     -8.1146e-04  5.0227e-05 -16.1559 < 2.2e-16 ***
    ## incidencerate  2.1363e-01  1.5952e-02  13.3922 < 2.2e-16 ***
    ## pcths          4.2828e-01  5.0331e-02   8.5092 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model <- m3
```

# Model Validation

All the validation steps will be performed using the final model on the
test dataset. Let’s start by loading the test dataset and applying the
same preprocessing steps as we did for the training dataset.

``` r
test <- read_csv("data/test.csv")
```

    ## Rows: 1216 Columns: 33
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): binnedinc, geography
    ## dbl (31): avganncount, avgdeathsperyear, target_deathrate, incidencerate, me...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(test)
```

    ##   avganncount       avgdeathsperyear target_deathrate incidencerate  
    ##  Min.   :    6.00   Min.   :   3.0   Min.   : 87.6    Min.   :214.8  
    ##  1st Qu.:   70.75   1st Qu.:  26.0   1st Qu.:161.1    1st Qu.:419.6  
    ##  Median :  168.00   Median :  60.0   Median :177.8    Median :453.5  
    ##  Mean   :  580.91   Mean   : 177.4   Mean   :178.4    Mean   :447.2  
    ##  3rd Qu.:  534.50   3rd Qu.: 156.0   3rd Qu.:195.0    3rd Qu.:480.0  
    ##  Max.   :13294.00   Max.   :4895.0   Max.   :293.9    Max.   :718.9  
    ##                                                                      
    ##    medincome        popest2015      povertypercent   studypercap     
    ##  Min.   : 23047   Min.   :    827   Min.   : 3.20   Min.   :   0.00  
    ##  1st Qu.: 38492   1st Qu.:  11170   1st Qu.:12.18   1st Qu.:   0.00  
    ##  Median : 44785   Median :  25962   Median :16.20   Median :   0.00  
    ##  Mean   : 46740   Mean   :  96307   Mean   :17.01   Mean   : 166.24  
    ##  3rd Qu.: 52387   3rd Qu.:  71517   3rd Qu.:20.40   3rd Qu.:  96.16  
    ##  Max.   :125635   Max.   :3299521   Max.   :47.40   Max.   :9439.20  
    ##                                                                      
    ##   binnedinc           medianage      medianagemale   medianagefemale
    ##  Length:1216        Min.   : 22.30   Min.   :22.40   Min.   :22.30  
    ##  Class :character   1st Qu.: 37.60   1st Qu.:36.30   1st Qu.:38.77  
    ##  Mode  :character   Median : 41.00   Median :39.70   Median :42.30  
    ##                     Mean   : 45.31   Mean   :39.55   Mean   :42.11  
    ##                     3rd Qu.: 43.92   3rd Qu.:42.50   3rd Qu.:45.40  
    ##                     Max.   :619.20   Max.   :64.70   Max.   :65.70  
    ##                                                                     
    ##   geography         percentmarried   pctnohs18_24     pcths18_24   
    ##  Length:1216        Min.   :25.30   Min.   : 0.00   Min.   : 7.10  
    ##  Class :character   1st Qu.:47.67   1st Qu.:12.50   1st Qu.:29.00  
    ##  Mode  :character   Median :52.30   Median :17.10   Median :34.85  
    ##                     Mean   :51.58   Mean   :18.13   Mean   :35.01  
    ##                     3rd Qu.:56.30   3rd Qu.:22.62   3rd Qu.:40.90  
    ##                     Max.   :72.50   Max.   :64.10   Max.   :66.20  
    ##                                                                    
    ##  pctsomecol18_24 pctbachdeg18_24   pcths25_over   pctbachdeg25_over
    ##  Min.   : 7.10   Min.   : 0.000   Min.   : 7.50   Min.   : 3.20    
    ##  1st Qu.:34.65   1st Qu.: 2.900   1st Qu.:30.50   1st Qu.: 9.40    
    ##  Median :40.70   Median : 5.300   Median :35.30   Median :12.30    
    ##  Mean   :41.72   Mean   : 6.071   Mean   :34.91   Mean   :13.26    
    ##  3rd Qu.:47.05   3rd Qu.: 8.300   3rd Qu.:39.62   3rd Qu.:16.20    
    ##  Max.   :79.00   Max.   :43.400   Max.   :54.80   Max.   :40.40    
    ##  NA's   :909                                                       
    ##  pctemployed16_over pctunemployed16_over pctprivatecoverage
    ##  Min.   :17.60      Min.   : 0.40        Min.   :22.30     
    ##  1st Qu.:48.40      1st Qu.: 5.60        1st Qu.:56.80     
    ##  Median :54.50      Median : 7.60        Median :65.00     
    ##  Mean   :54.06      Mean   : 7.84        Mean   :64.17     
    ##  3rd Qu.:60.40      3rd Qu.: 9.70        3rd Qu.:72.00     
    ##  Max.   :76.50      Max.   :27.00        Max.   :92.30     
    ##  NA's   :70                                                
    ##  pctprivatecoveragealone pctempprivcoverage pctpubliccoverage
    ##  Min.   :15.70           Min.   :13.50      Min.   :13.50    
    ##  1st Qu.:40.60           1st Qu.:34.20      1st Qu.:30.88    
    ##  Median :48.30           Median :41.15      Median :36.50    
    ##  Mean   :48.16           Mean   :41.05      Mean   :36.40    
    ##  3rd Qu.:55.80           3rd Qu.:47.52      3rd Qu.:41.80    
    ##  Max.   :78.20           Max.   :70.70      Max.   :65.10    
    ##  NA's   :253                                                 
    ##  pctpubliccoveragealone    pctwhite         pctblack          pctasian      
    ##  Min.   : 2.70          Min.   : 10.20   Min.   : 0.0000   Min.   : 0.0000  
    ##  1st Qu.:14.78          1st Qu.: 77.31   1st Qu.: 0.6108   1st Qu.: 0.2489  
    ##  Median :19.00          Median : 90.12   Median : 2.1584   Median : 0.5503  
    ##  Mean   :19.38          Mean   : 83.34   Mean   : 9.1478   Mean   : 1.2233  
    ##  3rd Qu.:23.30          3rd Qu.: 95.13   3rd Qu.:10.0581   3rd Qu.: 1.1626  
    ##  Max.   :43.30          Max.   :100.00   Max.   :81.2819   Max.   :42.6194  
    ##                                                                             
    ##   pctotherrace     pctmarriedhouseholds   birthrate     
    ##  Min.   : 0.0000   Min.   :23.89        Min.   : 0.000  
    ##  1st Qu.: 0.3074   1st Qu.:47.56        1st Qu.: 4.515  
    ##  Median : 0.8908   Median :51.55        Median : 5.451  
    ##  Mean   : 1.9540   Mean   :51.01        Mean   : 5.705  
    ##  3rd Qu.: 2.2318   3rd Qu.:55.10        3rd Qu.: 6.602  
    ##  Max.   :38.7437   Max.   :78.08        Max.   :18.557  
    ## 
