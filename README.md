<!-- README.md is generated from CancerMortality.Rmd. Please edit that file. -->

# Data Preparation

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
    ## 23.9 43.2 47.3 50.0 52.4 54.4 57.1 59.2 61.5 64.5 80.1

``` r
quantile(df$pctprivatecoveragealone, na.rm = TRUE, probs = seq(0, 1, 0.1))
```

    ##   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
    ## 16.8 35.4 39.8 42.9 45.8 48.9 51.6 54.3 57.2 61.6 78.9

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
    ## Column pctprivatecoveragealone has 3 mild outliers and 0 severe outliers
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
discretize_quartiles <- function(column, level_name) {
  res <- cut(column, breaks = quantile(column, probs = seq(0, 1, 0.25)), 
    include.lowest = T,
    labels=c(
      sprintf("Low%s", level_name),
      sprintf("LowMid%s", level_name),
      sprintf("HighMid%s", level_name),
      sprintf("High%s", level_name)
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
non_zero_studypercap_median <- median(df$studypercap[df$studypercap > 0])

df$f.studypercap <- cut(df$studypercap, breaks = c(-Inf, 0, non_zero_studypercap_median, Inf),
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

    ##  [1] "Miner County, South Dakota"    "Douglas County, Washington"   
    ##  [3] "Mason County, Washington"      "Calhoun County, Michigan"     
    ##  [5] "Trousdale County, Tennessee"   "Malheur County, Oregon"       
    ##  [7] "Lincoln County, Washington"    "Rock County, Minnesota"       
    ##  [9] "Shelby County, Kentucky"       "Carroll County, New Hampshire"

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
    ##   23.90   48.60   54.40   54.21   60.30   80.10

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
    ## W = 0.99195, p-value = 1.729e-08

An additional factor `f.pctemployed16_over` is created to discretize the
data according to the quartiles.

``` r
df$f.pctemployed16_over <- discretize_quartiles(df$pctemployed16_over, "Employ%")
```

    ## res
    ##     LowEmploy%  LowMidEmploy% HighMidEmploy%    HighEmploy% 
    ##            463            453            465            450

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
    ##   16.80   41.45   48.90   48.60   55.60   78.90

``` r
cor.test(df$pctprivatecoverage, df$pctprivatecoveragealone)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$pctprivatecoverage and df$pctprivatecoveragealone
    ## t = 110.58, df = 1829, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.9264558 0.9383977
    ## sample estimates:
    ##       cor 
    ## 0.9326819

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
    ## pctemployed16_over      -0.4405446  8.511183e-88
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
    ## -98.548 -10.971  -0.367  10.568 133.206 
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.019e+02  2.663e+01   7.580 5.48e-14 ***
    ## avganncount            -3.085e-03  9.760e-04  -3.161 0.001598 ** 
    ## avgdeathsperyear        1.471e-02  4.682e-03   3.141 0.001711 ** 
    ## incidencerate           1.862e-01  9.204e-03  20.234  < 2e-16 ***
    ## medincome               1.180e-04  1.380e-04   0.855 0.392660    
    ## popest2015             -1.101e-05  6.383e-06  -1.724 0.084858 .  
    ## povertypercent          3.649e-01  2.181e-01   1.673 0.094490 .  
    ## studypercap             1.262e-03  9.364e-04   1.348 0.177827    
    ## binnedinc              -4.208e-06  7.192e-05  -0.059 0.953350    
    ## medianage              -5.324e-01  2.025e-01  -2.629 0.008628 ** 
    ## percentmarried          1.343e+00  2.244e-01   5.984 2.62e-09 ***
    ## pctnohs18_24           -1.018e-01  7.333e-02  -1.388 0.165233    
    ## pcths18_24              1.526e-01  6.547e-02   2.331 0.019881 *  
    ## pctbachdeg18_24        -4.793e-02  1.436e-01  -0.334 0.738629    
    ## pcths25_over            4.342e-01  1.269e-01   3.422 0.000636 ***
    ## pctbachdeg25_over      -1.086e+00  2.018e-01  -5.381 8.38e-08 ***
    ## pctemployed16_over     -7.358e-01  1.463e-01  -5.030 5.40e-07 ***
    ## pctunemployed16_over    1.008e-01  2.180e-01   0.463 0.643766    
    ## pctprivatecoverage     -3.600e-01  1.727e-01  -2.085 0.037225 *  
    ## pctempprivcoverage      3.390e-01  1.331e-01   2.546 0.010968 *  
    ## pctpubliccoverage      -4.161e-01  2.954e-01  -1.409 0.159104    
    ## pctpubliccoveragealone  5.779e-01  3.648e-01   1.584 0.113343    
    ## pctwhite               -1.308e-01  8.162e-02  -1.603 0.109186    
    ## pctblack               -8.049e-04  8.298e-02  -0.010 0.992261    
    ## pctasian               -1.956e-01  2.928e-01  -0.668 0.504207    
    ## pctotherrace           -7.314e-01  1.600e-01  -4.571 5.20e-06 ***
    ## pctmarriedhouseholds   -1.243e+00  2.120e-01  -5.866 5.31e-09 ***
    ## birthrate              -8.032e-01  2.590e-01  -3.102 0.001953 ** 
    ## pcths                          NA         NA      NA       NA    
    ## pctbach                        NA         NA      NA       NA    
    ## racindex                       NA         NA      NA       NA    
    ## social_welfare                 NA         NA      NA       NA    
    ## f.raceBlack            -3.940e+01  1.752e+01  -2.249 0.024635 *  
    ## f.raceWhite            -3.637e+01  1.712e+01  -2.125 0.033699 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.5 on 1801 degrees of freedom
    ## Multiple R-squared:  0.5205, Adjusted R-squared:  0.5127 
    ## F-statistic:  67.4 on 29 and 1801 DF,  p-value: < 2.2e-16

According to this first model, the following variables seem to be very
significant (p-value \< 0.01):

``` r
coefs <- summary(model)$coefficients
significant_vars <- coefs[coefs[,'Pr(>|t|)'] < 0.01,]
significant_vars
```

    ##                          Estimate   Std. Error   t value     Pr(>|t|)
    ## (Intercept)          201.85824104 2.662897e+01  7.580401 5.481049e-14
    ## avganncount           -0.00308513 9.759978e-04 -3.161001 1.598438e-03
    ## avgdeathsperyear       0.01470602 4.681994e-03  3.140973 1.711239e-03
    ## incidencerate          0.18623815 9.204163e-03 20.234122 3.387013e-82
    ## medianage             -0.53238179 2.024771e-01 -2.629343 8.627808e-03
    ## percentmarried         1.34277603 2.244009e-01  5.983827 2.623449e-09
    ## pcths25_over           0.43420213 1.268923e-01  3.421817 6.359459e-04
    ## pctbachdeg25_over     -1.08582355 2.017886e-01 -5.380996 8.377124e-08
    ## pctemployed16_over    -0.73581106 1.462961e-01 -5.029600 5.404200e-07
    ## pctotherrace          -0.73142904 1.600322e-01 -4.570511 5.196152e-06
    ## pctmarriedhouseholds  -1.24348706 2.119973e-01 -5.865579 5.313525e-09
    ## birthrate             -0.80322080 2.589503e-01 -3.101834 1.953098e-03

## Analyzing the behaviour of the main predictors

The analysis found significant relationships between target death rates
and socioeconomic factors. Marital status, incidence rates, and median
income all showed strong associations, with higher marriage and income
levels linked to lower death rates, while higher incidence rates
correlated with higher death rates. These results were supported by both
pairwise Wilcoxon tests and ANOVA.

``` r
# Boxplot for most relevant predictors with appropriate labels
boxplot(target_deathrate ~ f.incidencerate, data = df, main = "Death Rate vs. Incidence Rate", 
        xlab = "Incidence Rate (Factor)", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-1.png)

``` r
boxplot(target_deathrate ~ f.medincome, data = df, main = "Death Rate vs. Median Income", 
        xlab = "Median Income (Factor)", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-2.png)

``` r
boxplot(target_deathrate ~ f.percentmarried, data = df, main = "Death Rate vs. Percent Married", 
        xlab = "Percent Married (Factor)", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-3.png)

``` r
boxplot(target_deathrate ~ pcths, data = df, main = "Death Rate vs. Percent High School", 
        xlab = "Percent High School", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-4.png)

``` r
boxplot(target_deathrate ~ f.povertypercent, data = df, main = "Death Rate vs. Poverty Percent", 
        xlab = "Poverty Percent (Factor)", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-5.png)

``` r
boxplot(target_deathrate ~ f.pctpubliccoverage, data = df, main = "Death Rate vs. Public Coverage", 
        xlab = "Public Coverage (Factor)", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-6.png)

``` r
boxplot(target_deathrate ~ f.pctpubliccoveragealone, data = df, main = "Death Rate vs. Public Coverage Alone", 
        xlab = "Public Coverage Alone (Factor)", ylab = "Death Rate")
```

![](images/unnamed-chunk-90-7.png)

``` r
# Pairwise tests and ANOVA for corresponding variables
# Percent Married
pairwise.wilcox.test(df$target_deathrate, df$f.percentmarried, p.adjust.method = "bonferroni")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.percentmarried 
    ## 
    ##                 LowMarried% LowMidMarried% HighMidMarried%
    ## LowMidMarried%  0.9924      -              -              
    ## HighMidMarried% 0.0003      0.0122         -              
    ## HighMarried%    < 2e-16     < 2e-16        3.5e-08        
    ## 
    ## P value adjustment method: bonferroni

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
pairwise.wilcox.test(df$target_deathrate, df$f.incidencerate, p.adjust.method = "bonferroni")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.incidencerate 
    ## 
    ##                    LowDiagnPerCap LowMidDiagnPerCap HighMidDiagnPerCap
    ## LowMidDiagnPerCap  2.8e-13        -                 -                 
    ## HighMidDiagnPerCap < 2e-16        2.3e-09           -                 
    ## HighDiagnPerCap    < 2e-16        < 2e-16           7.0e-05           
    ## 
    ## P value adjustment method: bonferroni

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
pairwise.wilcox.test(df$target_deathrate, df$f.medincome, p.adjust.method = "bonferroni")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.medincome 
    ## 
    ##                  LowMedianInc LowMidMedianInc HighMidMedianInc
    ## LowMidMedianInc  3.5e-13      -               -               
    ## HighMidMedianInc < 2e-16      3.3e-11         -               
    ## HighMedianInc    < 2e-16      < 2e-16         3.0e-10         
    ## 
    ## P value adjustment method: bonferroni

``` r
oneway.test(target_deathrate ~ f.medincome, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.medincome
    ## F = 133.32, num df = 3.0, denom df = 1009.9, p-value < 2.2e-16

``` r
# Poverty Percent
pairwise.wilcox.test(df$target_deathrate, df$f.povertypercent, p.adjust.method = "bonferroni")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.povertypercent 
    ## 
    ##             LowPov% LowMidPov% HighMidPov%
    ## LowMidPov%  1.1e-10 -          -          
    ## HighMidPov% < 2e-16 3.3e-12    -          
    ## HighPov%    < 2e-16 < 2e-16    2.0e-11    
    ## 
    ## P value adjustment method: bonferroni

``` r
oneway.test(target_deathrate ~ f.povertypercent, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.povertypercent
    ## F = 131.91, num df = 3.0, denom df = 1007.1, p-value < 2.2e-16

``` r
# Public Coverage
pairwise.wilcox.test(df$target_deathrate, df$f.pctpubliccoverage, p.adjust.method = "bonferroni")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.pctpubliccoverage 
    ## 
    ##                   LowGovHealth% LowMidGovHealth% HighMidGovHealth%
    ## LowMidGovHealth%  2.3e-13       -                -                
    ## HighMidGovHealth% < 2e-16       2.0e-07          -                
    ## HighGovHealth%    < 2e-16       < 2e-16          1.2e-07          
    ## 
    ## P value adjustment method: bonferroni

``` r
oneway.test(target_deathrate ~ f.pctpubliccoverage, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.pctpubliccoverage
    ## F = 104.8, num df = 3.0, denom df = 1009.4, p-value < 2.2e-16

``` r
# Public Coverage Alone
pairwise.wilcox.test(df$target_deathrate, df$f.pctpubliccoveragealone, p.adjust.method = "bonferroni")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
    ## 
    ## data:  df$target_deathrate and df$f.pctpubliccoveragealone 
    ## 
    ##                        LowGovHealthAlone% LowMidGovHealthAlone%
    ## LowMidGovHealthAlone%  2.7e-14            -                    
    ## HighMidGovHealthAlone% < 2e-16            1.1e-11              
    ## HighGovHealthAlone%    < 2e-16            < 2e-16              
    ##                        HighMidGovHealthAlone%
    ## LowMidGovHealthAlone%  -                     
    ## HighMidGovHealthAlone% -                     
    ## HighGovHealthAlone%    3.2e-11               
    ## 
    ## P value adjustment method: bonferroni

``` r
oneway.test(target_deathrate ~ f.pctpubliccoveragealone, data = df)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  target_deathrate and f.pctpubliccoveragealone
    ## F = 120.39, num df = 3, denom df = 1006, p-value < 2.2e-16

## Models without variable interactions

This preliminary analysis involves building multiple linear regression
models to explore how different predictors (e.g., poverty rate, marriage
rate, incidence rate, median income, public coverage) relate to a target
variable, target_deathrate. Each model tests the relationship between
target_deathrate and one predictor, while controlling for other factors.

The analysis of various socio-economic and health-related factors
reveals significant relationships with target_deathrate. Poverty
percentage has a strong positive association with death rates, where a
1% increase in poverty corresponds to a 23.42 increase in death rates.
Marriage rates show a negative relationship, with higher marriage
percentages linked to lower death rates, particularly in the linear and
quadratic terms. Incidence rates exhibit a positive effect, suggesting
that higher disease rates contribute to higher death rates. Median
income also has a negative relationship, with higher income associated
with lower death rates, though the effect becomes more complex with
higher-order terms. Interestingly, high school graduation rates are
positively correlated with death rates, which may reflect underlying
socio-economic factors not captured in the model. Public health
coverage, both overall and alone, shows a positive association with
death rates, indicating that more coverage could be linked to higher
mortality, possibly due to disparities in healthcare access or
reporting. Overall, these variables highlight key socio-economic and
health influences on mortality, providing insights for further analysis
and potential intervention strategies.

In the analysis, the decision to modify only the pcths variable is based
on the optimal lambda value obtained from the Box-Cox transformation.
For most variables, the lambda values were either close to 1, indicating
that no transformation is needed, or close to values suggesting the
relationship with the target variable is already approximately linear.
However, for the pcths variable, the optimal lambda value was 0.66,
which suggests that a moderate transformation would be beneficial. A
lambda value of 0.66 indicates that a mild transformation, such as
squaring or taking the square root, could help linearize the
relationship between pcths and target_deathrate.

``` r
# Linear model: target_deathrate ~ percentmarried 
lm_percentmarried <- lm(target_deathrate ~ percentmarried, data = df)
summary(lm_percentmarried)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ percentmarried, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -134.819  -17.033   -0.159   15.258  166.474 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    237.43481    4.87919   48.66   <2e-16 ***
    ## percentmarried  -1.12937    0.09323  -12.11   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.88 on 1829 degrees of freedom
    ## Multiple R-squared:  0.07428,    Adjusted R-squared:  0.07377 
    ## F-statistic: 146.8 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
lambda_percentmarried <- boxcox(target_deathrate ~ percentmarried, data = df)
```

![](images/unnamed-chunk-91-1.png)

``` r
optimal_lambda_percentmarried <- lambda_percentmarried$x[which.max(lambda_percentmarried$y)]
optimal_lambda_percentmarried
```

    ## [1] 0.7070707

``` r
#df$percentmarried <- df$percentmarried^optimal_lambda_percentmarried

# Linear model: target_deathrate ~ incidencerate
lm_incidencerate <- lm(target_deathrate ~ incidencerate, data = df)
summary(lm_incidencerate)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ incidencerate, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -143.812  -16.221   -1.731   15.129  111.093 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   77.85985    4.72685   16.47   <2e-16 ***
    ## incidencerate  0.22486    0.01045   21.52   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.96 on 1829 degrees of freedom
    ## Multiple R-squared:  0.2021, Adjusted R-squared:  0.2017 
    ## F-statistic: 463.2 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
lambda_incidencerate <- boxcox(target_deathrate ~ incidencerate, data = df)
```

![](images/unnamed-chunk-91-2.png)

``` r
optimal_lambda_incidencerate <- lambda_incidencerate$x[which.max(lambda_incidencerate$y)]
optimal_lambda_incidencerate
```

    ## [1] 0.7070707

``` r
#df$incidencerate <- df$incidencerate^optimal_lambda_incidencerate

# Linear model: target_deathrate ~ medincome
lm_medincome <- lm(target_deathrate ~ medincome, data = df)
summary(lm_medincome)
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
lambda_medincome <- boxcox(target_deathrate ~ medincome, data = df)
```

![](images/unnamed-chunk-91-3.png)

``` r
optimal_lambda_medincome <- lambda_medincome$x[which.max(lambda_medincome$y)]
optimal_lambda_medincome
```

    ## [1] 0.7070707

``` r
#df$medincome <- df$medincome^optimal_lambda_medincome

# Linear model: target_deathrate ~ pcths
lm_pcths <- lm(target_deathrate ~ pcths, data = df)
summary(lm_pcths)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ pcths, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -93.016 -15.177  -0.017  14.581 174.189 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 125.3421     3.1755   39.47   <2e-16 ***
    ## pcths         0.7669     0.0447   17.16   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.93 on 1829 degrees of freedom
    ## Multiple R-squared:  0.1386, Adjusted R-squared:  0.1381 
    ## F-statistic: 294.3 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
lambda_pcths <- boxcox(target_deathrate ~ pcths, data = df)
```

![](images/unnamed-chunk-91-4.png)

``` r
optimal_lambda_pcths <- lambda_pcths$x[which.max(lambda_pcths$y)]
optimal_lambda_pcths
```

    ## [1] 0.6666667

``` r
df$pcths_raised <- df$pcths^optimal_lambda_pcths

# Linear model: target_deathrate ~ pctpubliccoverage
lm_pctpubliccoverage <- lm(target_deathrate ~ pctpubliccoverage, data = df)
summary(lm_pctpubliccoverage)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ pctpubliccoverage, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -112.439  -14.555    0.835   14.675  184.514 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       124.12792    2.79201   44.46   <2e-16 ***
    ## pctpubliccoverage   1.51278    0.07548   20.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.3 on 1829 degrees of freedom
    ## Multiple R-squared:  0.1801, Adjusted R-squared:  0.1796 
    ## F-statistic: 401.7 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
lambda_pctpubliccoverage <- boxcox(target_deathrate ~ pctpubliccoverage, data = df)
```

![](images/unnamed-chunk-91-5.png)

``` r
optimal_lambda_pctpubliccoverage <- lambda_pctpubliccoverage$x[which.max(lambda_pctpubliccoverage$y)]
optimal_lambda_pctpubliccoverage
```

    ## [1] 0.7474747

``` r
#df$pctpubliccoverage <- df$pctpubliccoverage^optimal_lambda_pctpubliccoverage

# Linear model: target_deathrate ~ pctpubliccoveragealone
lm_pctpubliccoveragealone <- lm(target_deathrate ~ pctpubliccoveragealone, data = df)
summary(lm_pctpubliccoveragealone)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ pctpubliccoveragealone, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -120.15  -13.06    1.20   14.15  176.78 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            137.64068    1.91584   71.84   <2e-16 ***
    ## pctpubliccoveragealone   2.15035    0.09539   22.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.71 on 1829 degrees of freedom
    ## Multiple R-squared:  0.2174, Adjusted R-squared:  0.217 
    ## F-statistic: 508.2 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
lambda_pctpubliccoveragealone <- boxcox(target_deathrate ~ pctpubliccoveragealone, data = df)
```

![](images/unnamed-chunk-91-6.png)

``` r
optimal_lambda_pctpubliccoveragealone <- lambda_pctpubliccoveragealone$x[which.max(lambda_pctpubliccoveragealone$y)]
optimal_lambda_pctpubliccoveragealone
```

    ## [1] 0.8282828

``` r
#df$pctpubliccoveragealone <- df$pctpubliccoveragealone^optimal_lambda_pctpubliccoveragealone

# Linear model: target_deathrate ~ povertypercent
lm_povertypercent <- lm(target_deathrate ~ povertypercent, data = df)
summary(lm_povertypercent)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ povertypercent, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -122.340  -13.854    1.648   14.863  169.258 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    145.89427    1.63497   89.23   <2e-16 ***
    ## povertypercent   1.96081    0.09097   21.55   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.95 on 1829 degrees of freedom
    ## Multiple R-squared:  0.2026, Adjusted R-squared:  0.2021 
    ## F-statistic: 464.6 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
lambda_povertypercent <- boxcox(target_deathrate ~ povertypercent, data = df)
```

![](images/unnamed-chunk-91-7.png)

``` r
optimal_lambda_povertypercent <- lambda_povertypercent$x[which.max(lambda_povertypercent$y)]
optimal_lambda_povertypercent
```

    ## [1] 0.8686869

``` r
#df$povertypercent <- df$povertypercent^optimal_lambda_povertypercent
```

## Models with variable interactions

We began by building a comprehensive linear model to predict the target
death rate using a set of selected variables, including percentmarried,
incidencerate, medincome, pcths, pctpubliccoverage,
pctpubliccoveragealone, and povertypercent. The idea behind this is to
examine the influence of various socioeconomic factors and health
coverage on the target variable. Given the complexity of this model, we
then applied stepwise regression (in both directions) to simplify it,
seeking to retain only the most influential variables while removing any
that may be redundant or contribute little explanatory power. Stepwise
selection is a common approach to model simplification as it
systematically evaluates each predictor’s contribution, helping to
ensure that the final model is parsimonious without losing predictive
accuracy. The stepwise procedure both adds and removes terms based on
the Akaike Information Criterion (AIC), optimizing the model by reducing
unnecessary complexity while preserving the key predictors. By analyzing
the results, we aim to identify a simpler model that still provides a
meaningful and effective prediction of the death rate, with the
potential for clearer interpretations and easier implementation.

Then, we repeat the process adding more variables and accounting for
interactions to obtain a more completed model. We obtain a model with
56,7% r-square value, which is quite high. However,

``` r
# Create a new model with all selected variables
full_selected_model <- lm(target_deathrate ~ percentmarried + incidencerate + medincome + pcths + pctpubliccoverage + pctpubliccoveragealone + povertypercent, 
                          data = df)
summary(full_selected_model)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ percentmarried + incidencerate + 
    ##     medincome + pcths + pctpubliccoverage + pctpubliccoveragealone + 
    ##     povertypercent, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -110.388  -11.799    0.308   11.132  137.015 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             4.125e+01  1.101e+01   3.746 0.000185 ***
    ## percentmarried         -3.743e-02  1.022e-01  -0.366 0.714144    
    ## incidencerate           2.059e-01  8.716e-03  23.621  < 2e-16 ***
    ## medincome              -2.202e-04  7.969e-05  -2.764 0.005771 ** 
    ## pcths                   5.066e-01  4.073e-02  12.438  < 2e-16 ***
    ## pctpubliccoverage      -2.391e-01  1.484e-01  -1.611 0.107350    
    ## pctpubliccoveragealone  7.687e-01  2.118e-01   3.629 0.000292 ***
    ## povertypercent          9.583e-01  1.829e-01   5.241 1.79e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.36 on 1823 degrees of freedom
    ## Multiple R-squared:  0.4705, Adjusted R-squared:  0.4685 
    ## F-statistic: 231.4 on 7 and 1823 DF,  p-value: < 2.2e-16

``` r
# Perform stepwise selection (both directions)
lm_stepwise <- step(full_selected_model, direction = "both", k=log(nrow(df))); lm_stepwise
```

    ## Start:  AIC=11088.32
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + pctpubliccoverage + pctpubliccoveragealone + povertypercent
    ## 
    ##                          Df Sum of Sq    RSS   AIC
    ## - percentmarried          1        56 755965 11081
    ## - pctpubliccoverage       1      1076 756985 11083
    ## <none>                                755909 11088
    ## - medincome               1      3167 759076 11088
    ## - pctpubliccoveragealone  1      5462 761371 11094
    ## - povertypercent          1     11388 767297 11108
    ## - pcths                   1     64145 820054 11230
    ## - incidencerate           1    231357 987266 11570
    ## 
    ## Step:  AIC=11080.94
    ## target_deathrate ~ incidencerate + medincome + pcths + pctpubliccoverage + 
    ##     pctpubliccoveragealone + povertypercent
    ## 
    ##                          Df Sum of Sq    RSS   AIC
    ## - pctpubliccoverage       1      1227 757192 11076
    ## <none>                                755965 11081
    ## - medincome               1      3119 759084 11081
    ## - pctpubliccoveragealone  1      5774 761739 11087
    ## + percentmarried          1        56 755909 11088
    ## - povertypercent          1     15045 771010 11110
    ## - pcths                   1     66288 822252 11227
    ## - incidencerate           1    239913 995877 11578
    ## 
    ## Step:  AIC=11076.4
    ## target_deathrate ~ incidencerate + medincome + pcths + pctpubliccoveragealone + 
    ##     povertypercent
    ## 
    ##                          Df Sum of Sq    RSS   AIC
    ## - medincome               1      1977 759169 11074
    ## <none>                                757192 11076
    ## + pctpubliccoverage       1      1227 755965 11081
    ## - pctpubliccoveragealone  1      5503 762695 11082
    ## + percentmarried          1       207 756985 11083
    ## - povertypercent          1     20969 778161 11119
    ## - pcths                   1     65171 822362 11220
    ## - incidencerate           1    239499 996691 11572
    ## 
    ## Step:  AIC=11073.66
    ## target_deathrate ~ incidencerate + pcths + pctpubliccoveragealone + 
    ##     povertypercent
    ## 
    ##                          Df Sum of Sq    RSS   AIC
    ## <none>                                759169 11074
    ## + medincome               1      1977 757192 11076
    ## + pctpubliccoverage       1        85 759084 11081
    ## + percentmarried          1        26 759143 11081
    ## - pctpubliccoveragealone  1      6742 765911 11082
    ## - povertypercent          1     41152 800321 11163
    ## - pcths                   1     83632 842801 11258
    ## - incidencerate           1    237775 996944 11565

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ incidencerate + pcths + pctpubliccoveragealone + 
    ##     povertypercent, data = df)
    ## 
    ## Coefficients:
    ##            (Intercept)           incidencerate                   pcths  
    ##                17.7706                  0.2051                  0.5272  
    ## pctpubliccoveragealone          povertypercent  
    ##                 0.5638                  1.2732

``` r
## BEST MODEL PREDICTED BY FUNCTION STEP
#Step:  AIC=11068.72
#target_deathrate ~ incidencerate + pcths + pctpubliccoveragealone + 
#    povertypercent

# Plot the stepwise regression result
plot(lm_stepwise)
```

![](images/unnamed-chunk-92-1.png)![](images/unnamed-chunk-92-2.png)![](images/unnamed-chunk-92-3.png)![](images/unnamed-chunk-92-4.png)

``` r
#Repeat the process adding more variables and accounting for interactions

full_selected_model <- lm(target_deathrate ~ (percentmarried + incidencerate + medincome + pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone +  pctbach)^2, data = df)
full_selected_model_step <- step(full_selected_model, k=log(nrow(df)))
```

    ## Start:  AIC=11121.91
    ## target_deathrate ~ (percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach)^2
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pcths:povertypercent                      1       1.0 683536 11114
    ## - medincome:povertypercent                  1      52.8 683587 11114
    ## - percentmarried:pcths                      1      56.5 683591 11114
    ## - incidencerate:pcths                       1      73.4 683608 11115
    ## - pcths:pctbach                             1     106.4 683641 11115
    ## - percentmarried:povertypercent             1     154.7 683689 11115
    ## - percentmarried:pctpubliccoverage          1     184.1 683719 11115
    ## - percentmarried:pctpubliccoveragealone     1     258.8 683794 11115
    ## - incidencerate:medincome                   1     296.2 683831 11115
    ## - pcths:pctpubliccoverage                   1     499.6 684034 11116
    ## - incidencerate:povertypercent              1     735.6 684270 11116
    ## - medincome:pcths                           1     748.4 684283 11116
    ## - pctpubliccoveragealone:pctbach            1     749.2 684284 11116
    ## - pctpubliccoverage:pctbach                 1     753.6 684288 11116
    ## - incidencerate:pctbach                     1     818.0 684353 11117
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1067.2 684602 11117
    ## - pcths:pctpubliccoveragealone              1    1443.6 684978 11118
    ## - medincome:pctpubliccoverage               1    1766.5 685301 11119
    ## - medincome:pctbach                         1    1795.5 685330 11119
    ## - incidencerate:pctpubliccoverage           1    1821.2 685356 11119
    ## - percentmarried:medincome                  1    1981.4 685516 11120
    ## - percentmarried:pctbach                    1    2497.3 686032 11121
    ## <none>                                                  683535 11122
    ## - percentmarried:incidencerate              1    2934.5 686469 11122
    ## - povertypercent:pctpubliccoveragealone     1    3332.2 686867 11123
    ## - povertypercent:pctbach                    1    3416.2 686951 11124
    ## - medincome:pctpubliccoveragealone          1    4091.9 687627 11125
    ## - incidencerate:pctpubliccoveragealone      1    4959.6 688494 11128
    ## - povertypercent:pctpubliccoverage          1    5355.9 688891 11129
    ## 
    ## Step:  AIC=11114.4
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pcths + percentmarried:povertypercent + percentmarried:pctpubliccoverage + 
    ##     percentmarried:pctpubliccoveragealone + percentmarried:pctbach + 
    ##     incidencerate:medincome + incidencerate:pcths + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:povertypercent + 
    ##     medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoverage + pcths:pctpubliccoveragealone + 
    ##     pcths:pctbach + povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone + 
    ##     povertypercent:pctbach + pctpubliccoverage:pctpubliccoveragealone + 
    ##     pctpubliccoverage:pctbach + pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - medincome:povertypercent                  1      51.9 683588 11107
    ## - incidencerate:pcths                       1      72.8 683608 11107
    ## - percentmarried:pcths                      1      75.7 683611 11107
    ## - pcths:pctbach                             1     106.0 683642 11107
    ## - percentmarried:povertypercent             1     158.8 683695 11107
    ## - percentmarried:pctpubliccoverage          1     185.3 683721 11107
    ## - percentmarried:pctpubliccoveragealone     1     265.9 683802 11108
    ## - incidencerate:medincome                   1     301.7 683837 11108
    ## - pcths:pctpubliccoverage                   1     540.6 684076 11108
    ## - incidencerate:povertypercent              1     757.7 684293 11109
    ## - pctpubliccoverage:pctbach                 1     758.2 684294 11109
    ## - pctpubliccoveragealone:pctbach            1     794.1 684330 11109
    ## - incidencerate:pctbach                     1     817.6 684353 11109
    ## - medincome:pcths                           1    1039.8 684575 11110
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1070.0 684606 11110
    ## - medincome:pctpubliccoverage               1    1765.5 685301 11112
    ## - incidencerate:pctpubliccoverage           1    1820.9 685357 11112
    ## - pcths:pctpubliccoveragealone              1    1909.1 685445 11112
    ## - percentmarried:medincome                  1    1981.9 685518 11112
    ## - medincome:pctbach                         1    2002.4 685538 11112
    ## - percentmarried:pctbach                    1    2581.9 686118 11114
    ## <none>                                                  683536 11114
    ## - percentmarried:incidencerate              1    2953.6 686489 11115
    ## - povertypercent:pctpubliccoveragealone     1    3362.1 686898 11116
    ## - medincome:pctpubliccoveragealone          1    4093.4 687629 11118
    ## - povertypercent:pctbach                    1    4735.0 688271 11120
    ## - incidencerate:pctpubliccoveragealone      1    4979.2 688515 11120
    ## - povertypercent:pctpubliccoverage          1    5434.4 688970 11121
    ## 
    ## Step:  AIC=11107.03
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pcths + percentmarried:povertypercent + percentmarried:pctpubliccoverage + 
    ##     percentmarried:pctpubliccoveragealone + percentmarried:pctbach + 
    ##     incidencerate:medincome + incidencerate:pcths + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + pcths:pctbach + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - percentmarried:pcths                      1      67.6 683655 11100
    ## - incidencerate:pcths                       1      78.2 683666 11100
    ## - percentmarried:povertypercent             1     117.4 683705 11100
    ## - pcths:pctbach                             1     120.3 683708 11100
    ## - percentmarried:pctpubliccoverage          1     222.9 683810 11100
    ## - percentmarried:pctpubliccoveragealone     1     226.1 683814 11100
    ## - incidencerate:medincome                   1     365.7 683953 11100
    ## - pcths:pctpubliccoverage                   1     524.9 684112 11101
    ## - pctpubliccoverage:pctbach                 1     709.3 684297 11101
    ## - incidencerate:povertypercent              1     759.2 684347 11102
    ## - pctpubliccoveragealone:pctbach            1     821.4 684409 11102
    ## - incidencerate:pctbach                     1     824.1 684412 11102
    ## - medincome:pcths                           1    1097.6 684685 11102
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1339.4 684927 11103
    ## - medincome:pctpubliccoverage               1    1713.8 685301 11104
    ## - incidencerate:pctpubliccoverage           1    1797.3 685385 11104
    ## - pcths:pctpubliccoveragealone              1    1946.1 685534 11105
    ## - percentmarried:medincome                  1    2001.3 685589 11105
    ## - medincome:pctbach                         1    2208.7 685796 11105
    ## - percentmarried:pctbach                    1    2531.4 686119 11106
    ## <none>                                                  683588 11107
    ## - percentmarried:incidencerate              1    2960.5 686548 11107
    ## - povertypercent:pctpubliccoveragealone     1    3313.5 686901 11108
    ## - medincome:pctpubliccoveragealone          1    4754.0 688342 11112
    ## - povertypercent:pctbach                    1    4940.3 688528 11113
    ## - incidencerate:pctpubliccoveragealone      1    5082.6 688670 11113
    ## - povertypercent:pctpubliccoverage          1    5787.9 689375 11115
    ## 
    ## Step:  AIC=11099.69
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:povertypercent + percentmarried:pctpubliccoverage + 
    ##     percentmarried:pctpubliccoveragealone + percentmarried:pctbach + 
    ##     incidencerate:medincome + incidencerate:pcths + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + pcths:pctbach + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - incidencerate:pcths                       1      90.0 683745 11092
    ## - pcths:pctbach                             1     132.7 683788 11092
    ## - percentmarried:povertypercent             1     144.5 683800 11093
    ## - percentmarried:pctpubliccoverage          1     204.5 683860 11093
    ## - percentmarried:pctpubliccoveragealone     1     221.0 683876 11093
    ## - incidencerate:medincome                   1     380.4 684035 11093
    ## - pcths:pctpubliccoverage                   1     726.0 684381 11094
    ## - pctpubliccoverage:pctbach                 1     765.4 684420 11094
    ## - incidencerate:povertypercent              1     765.6 684421 11094
    ## - incidencerate:pctbach                     1     816.4 684471 11094
    ## - pctpubliccoveragealone:pctbach            1     962.9 684618 11095
    ## - medincome:pcths                           1    1039.1 684694 11095
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1355.3 685010 11096
    ## - medincome:pctpubliccoverage               1    1676.4 685331 11097
    ## - incidencerate:pctpubliccoverage           1    1765.6 685421 11097
    ## - percentmarried:medincome                  1    2035.5 685691 11098
    ## - medincome:pctbach                         1    2182.2 685837 11098
    ## <none>                                                  683655 11100
    ## - pcths:pctpubliccoveragealone              1    2862.1 686517 11100
    ## - percentmarried:incidencerate              1    2972.4 686628 11100
    ## - povertypercent:pctpubliccoveragealone     1    3249.2 686904 11101
    ## - percentmarried:pctbach                    1    3692.9 687348 11102
    ## - medincome:pctpubliccoveragealone          1    4701.4 688356 11105
    ## - povertypercent:pctbach                    1    4884.1 688539 11105
    ## - incidencerate:pctpubliccoveragealone      1    5046.6 688702 11106
    ## - povertypercent:pctpubliccoverage          1    5729.1 689384 11108
    ## 
    ## Step:  AIC=11092.42
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:povertypercent + percentmarried:pctpubliccoverage + 
    ##     percentmarried:pctpubliccoveragealone + percentmarried:pctbach + 
    ##     incidencerate:medincome + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + pcths:pctbach + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pcths:pctbach                             1     139.7 683885 11085
    ## - percentmarried:povertypercent             1     145.3 683890 11085
    ## - percentmarried:pctpubliccoverage          1     196.6 683942 11085
    ## - percentmarried:pctpubliccoveragealone     1     219.1 683964 11086
    ## - incidencerate:medincome                   1     445.3 684190 11086
    ## - pcths:pctpubliccoverage                   1     696.9 684442 11087
    ## - incidencerate:povertypercent              1     706.6 684452 11087
    ## - pctpubliccoverage:pctbach                 1     754.6 684500 11087
    ## - incidencerate:pctbach                     1     904.0 684649 11087
    ## - pctpubliccoveragealone:pctbach            1     967.3 684712 11088
    ## - medincome:pcths                           1    1032.4 684778 11088
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1392.0 685137 11089
    ## - medincome:pctpubliccoverage               1    1681.7 685427 11089
    ## - incidencerate:pctpubliccoverage           1    1701.5 685447 11090
    ## - percentmarried:medincome                  1    2047.6 685793 11090
    ## - medincome:pctbach                         1    2176.5 685922 11091
    ## <none>                                                  683745 11092
    ## - pcths:pctpubliccoveragealone              1    2846.9 686592 11092
    ## - percentmarried:incidencerate              1    3059.6 686805 11093
    ## - povertypercent:pctpubliccoveragealone     1    3228.5 686974 11094
    ## - percentmarried:pctbach                    1    3709.3 687454 11095
    ## - medincome:pctpubliccoveragealone          1    4769.4 688514 11098
    ## - povertypercent:pctbach                    1    4906.7 688652 11098
    ## - incidencerate:pctpubliccoveragealone      1    5129.2 688874 11099
    ## - povertypercent:pctpubliccoverage          1    5692.5 689438 11100
    ## 
    ## Step:  AIC=11085.28
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:povertypercent + percentmarried:pctpubliccoverage + 
    ##     percentmarried:pctpubliccoveragealone + percentmarried:pctbach + 
    ##     incidencerate:medincome + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - percentmarried:povertypercent             1     165.0 684050 11078
    ## - percentmarried:pctpubliccoverage          1     175.6 684060 11078
    ## - percentmarried:pctpubliccoveragealone     1     181.7 684066 11078
    ## - incidencerate:medincome                   1     439.7 684325 11079
    ## - pcths:pctpubliccoverage                   1     657.5 684542 11080
    ## - incidencerate:povertypercent              1     673.6 684558 11080
    ## - pctpubliccoverage:pctbach                 1     702.6 684587 11080
    ## - pctpubliccoveragealone:pctbach            1    1006.6 684891 11080
    ## - incidencerate:pctbach                     1    1035.5 684920 11080
    ## - medincome:pcths                           1    1292.7 685177 11081
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1626.6 685511 11082
    ## - medincome:pctpubliccoverage               1    1678.3 685563 11082
    ## - incidencerate:pctpubliccoverage           1    1712.4 685597 11082
    ## - percentmarried:medincome                  1    2041.7 685926 11083
    ## - medincome:pctbach                         1    2118.8 686004 11083
    ## - pcths:pctpubliccoveragealone              1    2716.0 686601 11085
    ## <none>                                                  683885 11085
    ## - percentmarried:incidencerate              1    3005.7 686890 11086
    ## - povertypercent:pctpubliccoveragealone     1    3152.6 687037 11086
    ## - percentmarried:pctbach                    1    4468.4 688353 11090
    ## - medincome:pctpubliccoveragealone          1    4931.0 688816 11091
    ## - povertypercent:pctbach                    1    5116.9 689002 11091
    ## - incidencerate:pctpubliccoveragealone      1    5318.6 689203 11092
    ## - povertypercent:pctpubliccoverage          1    5793.8 689679 11093
    ## 
    ## Step:  AIC=11078.21
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctpubliccoverage + percentmarried:pctpubliccoveragealone + 
    ##     percentmarried:pctbach + incidencerate:medincome + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - percentmarried:pctpubliccoveragealone     1      75.1 684125 11071
    ## - percentmarried:pctpubliccoverage          1      96.3 684146 11071
    ## - incidencerate:medincome                   1     435.8 684486 11072
    ## - incidencerate:povertypercent              1     621.0 684671 11072
    ## - pcths:pctpubliccoverage                   1     707.2 684757 11073
    ## - pctpubliccoverage:pctbach                 1     762.3 684812 11073
    ## - incidencerate:pctbach                     1    1046.9 685097 11074
    ## - pctpubliccoveragealone:pctbach            1    1072.9 685123 11074
    ## - medincome:pcths                           1    1191.2 685241 11074
    ## - medincome:pctpubliccoverage               1    1648.6 685698 11075
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1654.2 685704 11075
    ## - incidencerate:pctpubliccoverage           1    1823.2 685873 11076
    ## - medincome:pctbach                         1    1964.8 686015 11076
    ## - pcths:pctpubliccoveragealone              1    2692.1 686742 11078
    ## <none>                                                  684050 11078
    ## - percentmarried:incidencerate              1    3016.1 687066 11079
    ## - povertypercent:pctpubliccoveragealone     1    3388.6 687438 11080
    ## - percentmarried:pctbach                    1    4879.8 688930 11084
    ## - povertypercent:pctbach                    1    5006.6 689056 11084
    ## - percentmarried:medincome                  1    5175.7 689225 11084
    ## - povertypercent:pctpubliccoverage          1    5651.3 689701 11086
    ## - incidencerate:pctpubliccoveragealone      1    5671.6 689721 11086
    ## - medincome:pctpubliccoveragealone          1    5888.2 689938 11086
    ## 
    ## Step:  AIC=11070.9
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctpubliccoverage + percentmarried:pctbach + 
    ##     incidencerate:medincome + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - percentmarried:pctpubliccoverage          1      25.8 684151 11064
    ## - incidencerate:medincome                   1     429.8 684555 11064
    ## - incidencerate:povertypercent              1     605.1 684730 11065
    ## - pcths:pctpubliccoverage                   1     795.9 684921 11066
    ## - pctpubliccoverage:pctbach                 1     799.6 684924 11066
    ## - incidencerate:pctbach                     1    1021.9 685147 11066
    ## - pctpubliccoveragealone:pctbach            1    1098.7 685224 11066
    ## - medincome:pcths                           1    1152.9 685278 11066
    ## - medincome:pctpubliccoverage               1    1576.3 685701 11068
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1665.1 685790 11068
    ## - incidencerate:pctpubliccoverage           1    1942.4 686067 11069
    ## - medincome:pctbach                         1    1942.6 686067 11069
    ## - pcths:pctpubliccoveragealone              1    2805.4 686930 11071
    ## <none>                                                  684125 11071
    ## - percentmarried:incidencerate              1    2982.4 687107 11071
    ## - povertypercent:pctpubliccoveragealone     1    4610.9 688736 11076
    ## - percentmarried:pctbach                    1    4916.0 689041 11076
    ## - percentmarried:medincome                  1    5105.2 689230 11077
    ## - povertypercent:pctbach                    1    5190.2 689315 11077
    ## - incidencerate:pctpubliccoveragealone      1    5915.7 690041 11079
    ## - medincome:pctpubliccoveragealone          1    5957.4 690082 11079
    ## - povertypercent:pctpubliccoverage          1    6380.3 690505 11080
    ## 
    ## Step:  AIC=11063.46
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:medincome + incidencerate:povertypercent + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     incidencerate:pctbach + medincome:pcths + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + pcths:pctpubliccoverage + 
    ##     pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - incidencerate:medincome                   1     423.6 684574 11057
    ## - incidencerate:povertypercent              1     625.0 684776 11058
    ## - pcths:pctpubliccoverage                   1     785.4 684936 11058
    ## - pctpubliccoverage:pctbach                 1     837.7 684988 11058
    ## - incidencerate:pctbach                     1    1090.8 685241 11059
    ## - pctpubliccoveragealone:pctbach            1    1101.2 685252 11059
    ## - medincome:pcths                           1    1199.3 685350 11059
    ## - medincome:pctpubliccoverage               1    1550.5 685701 11060
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1684.1 685835 11060
    ## - medincome:pctbach                         1    1933.8 686084 11061
    ## - incidencerate:pctpubliccoverage           1    2004.8 686155 11061
    ## <none>                                                  684151 11064
    ## - pcths:pctpubliccoveragealone              1    2836.9 686987 11064
    ## - percentmarried:incidencerate              1    3033.7 687184 11064
    ## - povertypercent:pctpubliccoveragealone     1    4597.9 688749 11068
    ## - percentmarried:pctbach                    1    4964.1 689115 11069
    ## - povertypercent:pctbach                    1    5218.9 689369 11070
    ## - incidencerate:pctpubliccoveragealone      1    5992.6 690143 11072
    ## - medincome:pctpubliccoveragealone          1    6193.5 690344 11072
    ## - povertypercent:pctpubliccoverage          1    7239.5 691390 11075
    ## - percentmarried:medincome                  1    8554.8 692705 11079
    ## 
    ## Step:  AIC=11057.08
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:povertypercent + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + incidencerate:pctbach + 
    ##     medincome:pcths + medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoverage + pcths:pctpubliccoveragealone + 
    ##     povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone + 
    ##     povertypercent:pctbach + pctpubliccoverage:pctpubliccoveragealone + 
    ##     pctpubliccoverage:pctbach + pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - incidencerate:povertypercent              1     258.2 684832 11050
    ## - pcths:pctpubliccoverage                   1     665.9 685240 11051
    ## - pctpubliccoverage:pctbach                 1     825.5 685400 11052
    ## - pctpubliccoveragealone:pctbach            1    1253.9 685828 11053
    ## - medincome:pcths                           1    1451.6 686026 11053
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1587.2 686161 11054
    ## - incidencerate:pctbach                     1    1616.1 686190 11054
    ## - medincome:pctpubliccoverage               1    1913.6 686488 11055
    ## - medincome:pctbach                         1    2129.5 686704 11055
    ## - pcths:pctpubliccoveragealone              1    2748.5 687323 11057
    ## <none>                                                  684574 11057
    ## - percentmarried:incidencerate              1    3043.2 687617 11058
    ## - incidencerate:pctpubliccoverage           1    4531.0 689105 11062
    ## - povertypercent:pctpubliccoveragealone     1    4889.0 689463 11063
    ## - povertypercent:pctbach                    1    4987.5 689562 11063
    ## - percentmarried:pctbach                    1    5028.4 689603 11063
    ## - medincome:pctpubliccoveragealone          1    6504.4 691079 11067
    ## - povertypercent:pctpubliccoverage          1    7729.6 692304 11070
    ## - incidencerate:pctpubliccoveragealone      1    8475.7 693050 11072
    ## - percentmarried:medincome                  1    8614.6 693189 11072
    ## 
    ## Step:  AIC=11050.26
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + incidencerate:pctbach + 
    ##     medincome:pcths + medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoverage + pcths:pctpubliccoveragealone + 
    ##     povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone + 
    ##     povertypercent:pctbach + pctpubliccoverage:pctpubliccoveragealone + 
    ##     pctpubliccoverage:pctbach + pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pcths:pctpubliccoverage                   1     720.6 685553 11045
    ## - pctpubliccoverage:pctbach                 1     875.5 685708 11045
    ## - pctpubliccoveragealone:pctbach            1    1295.3 686128 11046
    ## - medincome:pcths                           1    1378.0 686210 11046
    ## - incidencerate:pctbach                     1    1389.0 686221 11046
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1524.0 686356 11047
    ## - medincome:pctpubliccoverage               1    1910.1 686743 11048
    ## - medincome:pctbach                         1    2015.3 686848 11048
    ## - pcths:pctpubliccoveragealone              1    2807.6 687640 11050
    ## <none>                                                  684832 11050
    ## - percentmarried:incidencerate              1    3665.2 688498 11052
    ## - incidencerate:pctpubliccoverage           1    4459.7 689292 11055
    ## - percentmarried:pctbach                    1    4904.6 689737 11056
    ## - povertypercent:pctbach                    1    4919.8 689752 11056
    ## - povertypercent:pctpubliccoveragealone     1    4929.7 689762 11056
    ## - medincome:pctpubliccoveragealone          1    6562.4 691395 11060
    ## - povertypercent:pctpubliccoverage          1    7792.9 692625 11064
    ## - percentmarried:medincome                  1    8542.8 693375 11065
    ## - incidencerate:pctpubliccoveragealone      1   12486.5 697319 11076
    ## 
    ## Step:  AIC=11044.67
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + incidencerate:pctbach + 
    ##     medincome:pcths + medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoverage:pctbach + 
    ##     pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pctpubliccoverage:pctbach                 1     397.5 685951 11038
    ## - pctpubliccoveragealone:pctbach            1     977.0 686530 11040
    ## - incidencerate:pctbach                     1    1322.4 686875 11041
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1624.1 687177 11042
    ## - medincome:pctpubliccoverage               1    2175.3 687728 11043
    ## - pcths:pctpubliccoveragealone              1    2265.8 687819 11043
    ## - medincome:pcths                           1    2626.1 688179 11044
    ## - medincome:pctbach                         1    2772.6 688326 11044
    ## <none>                                                  685553 11045
    ## - percentmarried:incidencerate              1    3770.0 689323 11047
    ## - povertypercent:pctbach                    1    4749.6 690303 11050
    ## - incidencerate:pctpubliccoverage           1    4839.5 690393 11050
    ## - percentmarried:pctbach                    1    5431.4 690984 11052
    ## - povertypercent:pctpubliccoveragealone     1    5555.3 691108 11052
    ## - medincome:pctpubliccoveragealone          1    7043.5 692597 11056
    ## - povertypercent:pctpubliccoverage          1    8776.6 694330 11060
    ## - percentmarried:medincome                  1    9118.7 694672 11061
    ## - incidencerate:pctpubliccoveragealone      1   13274.7 698828 11072
    ## 
    ## Step:  AIC=11038.22
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + incidencerate:pctbach + 
    ##     medincome:pcths + medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone + pctpubliccoveragealone:pctbach
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pctpubliccoveragealone:pctbach            1     617.5 686568 11032
    ## - incidencerate:pctbach                     1    1193.3 687144 11034
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1363.3 687314 11034
    ## - medincome:pctpubliccoverage               1    1830.0 687781 11036
    ## - pcths:pctpubliccoveragealone              1    2159.2 688110 11036
    ## - medincome:pcths                           1    2525.9 688477 11037
    ## <none>                                                  685951 11038
    ## - medincome:pctbach                         1    3418.9 689369 11040
    ## - percentmarried:incidencerate              1    3916.5 689867 11041
    ## - incidencerate:pctpubliccoverage           1    4664.7 690615 11043
    ## - povertypercent:pctbach                    1    4685.1 690636 11043
    ## - percentmarried:pctbach                    1    5258.7 691209 11045
    ## - povertypercent:pctpubliccoveragealone     1    6293.6 692244 11047
    ## - medincome:pctpubliccoveragealone          1    6821.9 692773 11049
    ## - percentmarried:medincome                  1    8910.4 694861 11054
    ## - povertypercent:pctpubliccoverage          1    9603.6 695554 11056
    ## - incidencerate:pctpubliccoveragealone      1   12965.2 698916 11065
    ## 
    ## Step:  AIC=11032.35
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + incidencerate:pctbach + 
    ##     medincome:pcths + medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - incidencerate:pctbach                     1    1275.4 687843 11028
    ## - pcths:pctpubliccoveragealone              1    1563.2 688131 11029
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1835.2 688403 11030
    ## - medincome:pctpubliccoverage               1    1851.4 688420 11030
    ## - medincome:pcths                           1    1918.8 688487 11030
    ## - medincome:pctbach                         1    2815.5 689384 11032
    ## <none>                                                  686568 11032
    ## - percentmarried:incidencerate              1    3982.8 690551 11035
    ## - incidencerate:pctpubliccoverage           1    4664.4 691232 11037
    ## - percentmarried:pctbach                    1    4894.9 691463 11038
    ## - povertypercent:pctbach                    1    5771.7 692340 11040
    ## - medincome:pctpubliccoveragealone          1    6734.6 693303 11043
    ## - povertypercent:pctpubliccoveragealone     1    6956.4 693524 11043
    ## - percentmarried:medincome                  1    8383.1 694951 11047
    ## - povertypercent:pctpubliccoverage          1   10315.8 696884 11052
    ## - incidencerate:pctpubliccoveragealone      1   12996.2 699564 11059
    ## 
    ## Step:  AIC=11028.24
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pcths + 
    ##     medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + pcths:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pcths:pctpubliccoveragealone              1    1765.4 689609 11025
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1939.9 689783 11026
    ## - medincome:pcths                           1    1962.3 689806 11026
    ## - medincome:pctpubliccoverage               1    2040.0 689883 11026
    ## - medincome:pctbach                         1    2809.5 690653 11028
    ## <none>                                                  687843 11028
    ## - percentmarried:incidencerate              1    3620.3 691464 11030
    ## - incidencerate:pctpubliccoverage           1    4579.5 692423 11033
    ## - percentmarried:pctbach                    1    4830.0 692673 11034
    ## - povertypercent:pctbach                    1    6028.8 693872 11037
    ## - medincome:pctpubliccoveragealone          1    6731.8 694575 11039
    ## - povertypercent:pctpubliccoveragealone     1    7060.9 694904 11039
    ## - percentmarried:medincome                  1    8341.5 696185 11043
    ## - povertypercent:pctpubliccoverage          1   10969.9 698813 11050
    ## - incidencerate:pctpubliccoveragealone      1   11721.0 699564 11052
    ## 
    ## Step:  AIC=11025.42
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pcths + 
    ##     medincome:pctpubliccoverage + medincome:pctpubliccoveragealone + 
    ##     medincome:pctbach + povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone + 
    ##     povertypercent:pctbach + pctpubliccoverage:pctpubliccoveragealone
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - medincome:pcths                           1     549.5 690158 11019
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1031.5 690640 11021
    ## - medincome:pctbach                         1    1838.7 691448 11023
    ## - medincome:pctpubliccoverage               1    1986.7 691596 11023
    ## <none>                                                  689609 11025
    ## - percentmarried:incidencerate              1    3558.8 693168 11027
    ## - percentmarried:pctbach                    1    4646.4 694255 11030
    ## - povertypercent:pctbach                    1    4857.9 694467 11031
    ## - incidencerate:pctpubliccoverage           1    4927.1 694536 11031
    ## - medincome:pctpubliccoveragealone          1    6580.7 696190 11035
    ## - povertypercent:pctpubliccoveragealone     1    7249.6 696858 11037
    ## - percentmarried:medincome                  1    8430.9 698040 11040
    ## - povertypercent:pctpubliccoverage          1    9991.6 699600 11044
    ## - incidencerate:pctpubliccoveragealone      1   12527.9 702137 11051
    ## 
    ## Step:  AIC=11019.36
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach + 
    ##     pctpubliccoverage:pctpubliccoveragealone
    ## 
    ##                                            Df Sum of Sq    RSS   AIC
    ## - pctpubliccoverage:pctpubliccoveragealone  1    1001.5 691160 11014
    ## - medincome:pctbach                         1    1331.6 691490 11015
    ## - medincome:pctpubliccoverage               1    2016.3 692175 11017
    ## <none>                                                  690158 11019
    ## - percentmarried:incidencerate              1    3583.4 693742 11021
    ## - percentmarried:pctbach                    1    4623.6 694782 11024
    ## - incidencerate:pctpubliccoverage           1    4788.5 694947 11024
    ## - povertypercent:pctbach                    1    4926.2 695085 11025
    ## - medincome:pctpubliccoveragealone          1    6381.1 696539 11029
    ## - povertypercent:pctpubliccoveragealone     1    6932.5 697091 11030
    ## - percentmarried:medincome                  1    8094.1 698252 11033
    ## - povertypercent:pctpubliccoverage          1    9821.7 699980 11038
    ## - incidencerate:pctpubliccoveragealone      1   12217.4 702376 11044
    ## - pcths                                     1   17584.4 707743 11058
    ## 
    ## Step:  AIC=11014.51
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + medincome:pctbach + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach
    ## 
    ##                                         Df Sum of Sq    RSS   AIC
    ## - medincome:pctbach                      1    1739.9 692900 11012
    ## - medincome:pctpubliccoverage            1    2293.8 693454 11013
    ## <none>                                               691160 11014
    ## - percentmarried:incidencerate           1    3564.5 694724 11016
    ## - povertypercent:pctbach                 1    4500.8 695661 11019
    ## - incidencerate:pctpubliccoverage        1    4864.5 696024 11020
    ## - percentmarried:pctbach                 1    5002.8 696163 11020
    ## - medincome:pctpubliccoveragealone       1    5526.9 696687 11022
    ## - povertypercent:pctpubliccoveragealone  1    7246.9 698407 11026
    ## - percentmarried:medincome               1    7562.7 698723 11027
    ## - povertypercent:pctpubliccoverage       1    8969.9 700130 11031
    ## - incidencerate:pctpubliccoveragealone   1   12214.7 703374 11039
    ## - pcths                                  1   18540.3 709700 11056
    ## 
    ## Step:  AIC=11011.6
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pctpubliccoverage + 
    ##     medincome:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone + povertypercent:pctbach
    ## 
    ##                                         Df Sum of Sq    RSS   AIC
    ## - medincome:pctpubliccoverage            1    1288.4 694188 11008
    ## <none>                                               692900 11012
    ## - povertypercent:pctbach                 1    2900.8 695800 11012
    ## - percentmarried:incidencerate           1    3665.1 696565 11014
    ## - percentmarried:pctbach                 1    3758.5 696658 11014
    ## - incidencerate:pctpubliccoverage        1    5100.4 698000 11018
    ## - medincome:pctpubliccoveragealone       1    5953.8 698854 11020
    ## - percentmarried:medincome               1    7284.7 700184 11023
    ## - povertypercent:pctpubliccoverage       1    7311.7 700211 11023
    ## - povertypercent:pctpubliccoveragealone  1    7427.0 700327 11024
    ## - incidencerate:pctpubliccoveragealone   1   12842.3 705742 11038
    ## - pcths                                  1   18863.5 711763 11053
    ## 
    ## Step:  AIC=11007.49
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pctpubliccoveragealone + 
    ##     povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone + 
    ##     povertypercent:pctbach
    ## 
    ##                                         Df Sum of Sq    RSS   AIC
    ## - povertypercent:pctbach                 1    2683.0 696871 11007
    ## <none>                                               694188 11008
    ## - percentmarried:pctbach                 1    3416.5 697605 11009
    ## - percentmarried:incidencerate           1    3917.1 698105 11010
    ## - incidencerate:pctpubliccoverage        1    5309.5 699498 11014
    ## - percentmarried:medincome               1    6673.2 700861 11018
    ## - povertypercent:pctpubliccoverage       1    7063.9 701252 11018
    ## - povertypercent:pctpubliccoveragealone  1    8028.3 702216 11021
    ## - medincome:pctpubliccoveragealone       1    9373.7 703562 11024
    ## - incidencerate:pctpubliccoveragealone   1   13021.9 707210 11034
    ## - pcths                                  1   18457.5 712646 11048
    ## 
    ## Step:  AIC=11007.04
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     percentmarried:pctbach + incidencerate:pctpubliccoverage + 
    ##     incidencerate:pctpubliccoveragealone + medincome:pctpubliccoveragealone + 
    ##     povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone
    ## 
    ##                                         Df Sum of Sq    RSS   AIC
    ## - percentmarried:pctbach                 1    1280.5 698152 11003
    ## <none>                                               696871 11007
    ## - percentmarried:incidencerate           1    4018.9 700890 11010
    ## - percentmarried:medincome               1    4507.1 701378 11011
    ## - incidencerate:pctpubliccoverage        1    5442.6 702314 11014
    ## - povertypercent:pctpubliccoverage       1    5507.1 702378 11014
    ## - medincome:pctpubliccoveragealone       1    7164.2 704035 11018
    ## - povertypercent:pctpubliccoveragealone  1    7430.3 704301 11019
    ## - incidencerate:pctpubliccoveragealone   1   13512.3 710383 11035
    ## - pcths                                  1   17055.1 713926 11044
    ## 
    ## Step:  AIC=11002.89
    ## target_deathrate ~ percentmarried + incidencerate + medincome + 
    ##     pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
    ##     pctbach + percentmarried:incidencerate + percentmarried:medincome + 
    ##     incidencerate:pctpubliccoverage + incidencerate:pctpubliccoveragealone + 
    ##     medincome:pctpubliccoveragealone + povertypercent:pctpubliccoverage + 
    ##     povertypercent:pctpubliccoveragealone
    ## 
    ##                                         Df Sum of Sq    RSS   AIC
    ## <none>                                               698152 11003
    ## - percentmarried:medincome               1    3248.4 701400 11004
    ## - percentmarried:incidencerate           1    4317.9 702470 11007
    ## - incidencerate:pctpubliccoverage        1    5494.4 703646 11010
    ## - povertypercent:pctpubliccoverage       1    6290.3 704442 11012
    ## - medincome:pctpubliccoveragealone       1    6316.9 704468 11012
    ## - povertypercent:pctpubliccoveragealone  1    7413.3 705565 11015
    ## - incidencerate:pctpubliccoveragealone   1   13808.0 711960 11031
    ## - pctbach                                1   14679.5 712831 11034
    ## - pcths                                  1   17773.9 715926 11041

``` r
#Last model: 

#Step:  AIC=10996.07
#target_deathrate ~ percentmarried + incidencerate + medincome + 
#    pcths + povertypercent + pctpubliccoverage + pctpubliccoveragealone + 
#    pctbach + percentmarried:medincome + incidencerate:pctpubliccoverage + 
#    incidencerate:pctpubliccoveragealone + medincome:pctpubliccoveragealone + 
#    povertypercent:pctpubliccoverage + povertypercent:pctpubliccoveragealone

summary(full_selected_model)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ (percentmarried + incidencerate + 
    ##     medincome + pcths + povertypercent + pctpubliccoverage + 
    ##     pctpubliccoveragealone + pctbach)^2, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -88.668 -11.052  -0.135  10.745 138.769 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                               3.581e+02  1.576e+02   2.272 0.023219
    ## percentmarried                           -1.080e+00  1.720e+00  -0.628 0.529870
    ## incidencerate                            -2.303e-01  1.698e-01  -1.357 0.175003
    ## medincome                                 2.000e-04  1.622e-03   0.123 0.901852
    ## pcths                                    -3.868e-01  1.025e+00  -0.377 0.706032
    ## povertypercent                           -7.048e+00  2.761e+00  -2.553 0.010774
    ## pctpubliccoverage                        -1.207e+00  3.285e+00  -0.367 0.713398
    ## pctpubliccoveragealone                    8.764e-01  3.942e+00   0.222 0.824093
    ## pctbach                                  -5.946e+00  1.594e+00  -3.731 0.000197
    ## percentmarried:incidencerate              3.674e-03  1.324e-03   2.775 0.005574
    ## percentmarried:medincome                 -4.065e-05  1.783e-05  -2.280 0.022697
    ## percentmarried:pcths                     -3.324e-03  8.633e-03  -0.385 0.700267
    ## percentmarried:povertypercent             1.440e-02  2.259e-02   0.637 0.524067
    ## percentmarried:pctpubliccoverage          1.928e-02  2.773e-02   0.695 0.487036
    ## percentmarried:pctpubliccoveragealone    -2.769e-02  3.360e-02  -0.824 0.409930
    ## percentmarried:pctbach                    4.208e-02  1.644e-02   2.560 0.010543
    ## incidencerate:medincome                   1.453e-06  1.648e-06   0.882 0.378074
    ## incidencerate:pcths                       3.398e-04  7.742e-04   0.439 0.660758
    ## incidencerate:povertypercent              4.697e-03  3.380e-03   1.389 0.164860
    ## incidencerate:pctpubliccoverage          -5.899e-03  2.698e-03  -2.186 0.028924
    ## incidencerate:pctpubliccoveragealone      1.315e-02  3.645e-03   3.608 0.000317
    ## incidencerate:pctbach                     2.143e-03  1.463e-03   1.465 0.143024
    ## medincome:pcths                           1.193e-05  8.514e-06   1.402 0.161222
    ## medincome:povertypercent                 -8.423e-06  2.264e-05  -0.372 0.709866
    ## medincome:pctpubliccoverage               4.287e-05  1.991e-05   2.153 0.031437
    ## medincome:pctpubliccoveragealone         -9.970e-05  3.042e-05  -3.277 0.001069
    ## medincome:pctbach                         2.280e-05  1.051e-05   2.171 0.030078
    ## pcths:povertypercent                      8.380e-04  1.663e-02   0.050 0.959809
    ## pcths:pctpubliccoverage                  -1.602e-02  1.399e-02  -1.145 0.252311
    ## pcths:pctpubliccoveragealone              3.687e-02  1.894e-02   1.947 0.051749
    ## pcths:pctbach                             3.101e-03  5.869e-03   0.528 0.597297
    ## povertypercent:pctpubliccoverage          1.701e-01  4.538e-02   3.749 0.000183
    ## povertypercent:pctpubliccoveragealone    -1.420e-01  4.802e-02  -2.957 0.003144
    ## povertypercent:pctbach                    8.804e-02  2.940e-02   2.994 0.002788
    ## pctpubliccoverage:pctpubliccoveragealone -3.262e-02  1.949e-02  -1.674 0.094375
    ## pctpubliccoverage:pctbach                -3.349e-02  2.381e-02  -1.406 0.159787
    ## pctpubliccoveragealone:pctbach            4.675e-02  3.334e-02   1.402 0.161000
    ##                                             
    ## (Intercept)                              *  
    ## percentmarried                              
    ## incidencerate                               
    ## medincome                                   
    ## pcths                                       
    ## povertypercent                           *  
    ## pctpubliccoverage                           
    ## pctpubliccoveragealone                      
    ## pctbach                                  ***
    ## percentmarried:incidencerate             ** 
    ## percentmarried:medincome                 *  
    ## percentmarried:pcths                        
    ## percentmarried:povertypercent               
    ## percentmarried:pctpubliccoverage            
    ## percentmarried:pctpubliccoveragealone       
    ## percentmarried:pctbach                   *  
    ## incidencerate:medincome                     
    ## incidencerate:pcths                         
    ## incidencerate:povertypercent                
    ## incidencerate:pctpubliccoverage          *  
    ## incidencerate:pctpubliccoveragealone     ***
    ## incidencerate:pctbach                       
    ## medincome:pcths                             
    ## medincome:povertypercent                    
    ## medincome:pctpubliccoverage              *  
    ## medincome:pctpubliccoveragealone         ** 
    ## medincome:pctbach                        *  
    ## pcths:povertypercent                        
    ## pcths:pctpubliccoverage                     
    ## pcths:pctpubliccoveragealone             .  
    ## pcths:pctbach                               
    ## povertypercent:pctpubliccoverage         ***
    ## povertypercent:pctpubliccoveragealone    ** 
    ## povertypercent:pctbach                   ** 
    ## pctpubliccoverage:pctpubliccoveragealone .  
    ## pctpubliccoverage:pctbach                   
    ## pctpubliccoveragealone:pctbach              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.52 on 1794 degrees of freedom
    ## Multiple R-squared:  0.5212, Adjusted R-squared:  0.5116 
    ## F-statistic: 54.25 on 36 and 1794 DF,  p-value: < 2.2e-16

In this section, we are building a series of linear regression models to
predict the target death rate. We started with a simple model (m1) that
used only median income as a predictor, which provided some insight into
its relationship with the death rate. Next, we expanded the model (m2)
to include incidence rate as an additional predictor, and confirmed that
no transformation of the target variable was needed through the Box-Cox
transformation. The inclusion of incidence rate significantly improved
the model, as evidenced by the higher R-squared and F-statistic.

We continued to refine the model by adding poverty percentage
(f.povertypercent) in model (m3), which further improved the fit,
showing a significant reduction in residual sum of squares (RSS). The
inclusion of poverty percentage, along with median income and incidence
rate, captured more variance in the target death rate, as confirmed by
an ANOVA comparison between m2 and m3. In model (m4), we added high
school graduation rate (pcths), which was also found to be a significant
predictor. The ANOVA results indicated that this addition further
improved the model’s fit, with a significant reduction in RSS and an
increased F-statistic.

Throughout this process, we monitored potential issues such as
multicollinearity and heteroscedasticity. The Variance Inflation Factor
(VIF) values for all predictors were low (well below 5), indicating that
multicollinearity was not a concern. However, the Breusch-Pagan test for
heteroscedasticity showed evidence of non-constant variance in the
residuals in m4, which could violate one of the assumptions of linear
regression.

Overall, the model selection process allowed us to identify the most
relevant socio-economic and health-related variables influencing the
target death rate. The final model (m4) included median income,
incidence rate, poverty percentage, and high school graduation rate.
Although the model fits the data well, we noted the presence of
heteroscedasticity, which we may need to address in future iterations.
This approach has ensured that we retain the most statistically
significant variables while minimizing the risk of over fitting.Note the
fact prior to this code we tested other possible arrangements and used
other variables in order to find the best model. For sake of simplicity,
this code only shows the last results.

``` r
# target_deathrate ~ incidencerate
m1 <- lm(target_deathrate ~ incidencerate, data = df)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ incidencerate, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -143.812  -16.221   -1.731   15.129  111.093 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   77.85985    4.72685   16.47   <2e-16 ***
    ## incidencerate  0.22486    0.01045   21.52   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.96 on 1829 degrees of freedom
    ## Multiple R-squared:  0.2021, Adjusted R-squared:  0.2017 
    ## F-statistic: 463.2 on 1 and 1829 DF,  p-value: < 2.2e-16

``` r
# plot(m1)
boxcox(target_deathrate ~ incidencerate , data=df) #No transformation of the target needed
```

![](images/unnamed-chunk-93-1.png)

``` r
# target_deathrate ~ povertypercent + incidencerate
m2 <- lm(target_deathrate ~ povertypercent + incidencerate, data= df)
boxcox(target_deathrate ~ povertypercent + incidencerate, data=df) #No transformation of the target needed.
```

![](images/unnamed-chunk-93-2.png)

``` r
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ povertypercent + incidencerate, 
    ##     data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -145.429  -13.272   -0.285   13.208  118.163 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    49.812623   4.300017   11.58   <2e-16 ***
    ## povertypercent  1.889498   0.079644   23.72   <2e-16 ***
    ## incidencerate   0.216661   0.009144   23.69   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.83 on 1828 degrees of freedom
    ## Multiple R-squared:  0.3899, Adjusted R-squared:  0.3893 
    ## F-statistic: 584.2 on 2 and 1828 DF,  p-value: < 2.2e-16

``` r
# plot(m2)
t <- summary(m2)
vif(m2)
```

    ## povertypercent  incidencerate 
    ##        1.00143        1.00143

``` r
1/(1-t$r.squared)
```

    ## [1] 1.63917

``` r
anova(m1,m2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ incidencerate
    ## Model 2: target_deathrate ~ povertypercent + incidencerate
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1829 1139089                                  
    ## 2   1828  870928  1    268162 562.85 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# target_deathrate ~ pcths + incidencerate + povertypercent
m3 <- lm(target_deathrate ~  pcths + incidencerate + povertypercent, data= df)
summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ pcths + incidencerate + povertypercent, 
    ##     data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -114.310  -11.810    0.294   11.810  135.278 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    17.781265   4.512788    3.94 8.45e-05 ***
    ## pcths           0.568193   0.035899   15.83  < 2e-16 ***
    ## incidencerate   0.207240   0.008598   24.10  < 2e-16 ***
    ## povertypercent  1.689400   0.075770   22.30  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.47 on 1827 degrees of freedom
    ## Multiple R-squared:  0.4635, Adjusted R-squared:  0.4626 
    ## F-statistic: 526.1 on 3 and 1827 DF,  p-value: < 2.2e-16

``` r
plot(m3)
```

![](images/unnamed-chunk-93-3.png)![](images/unnamed-chunk-93-4.png)![](images/unnamed-chunk-93-5.png)![](images/unnamed-chunk-93-6.png)

``` r
anova(m1,m3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ incidencerate
    ## Model 2: target_deathrate ~ pcths + incidencerate + povertypercent
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1829 1139089                                  
    ## 2   1827  765911  2    373178 445.09 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(m2,m3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ povertypercent + incidencerate
    ## Model 2: target_deathrate ~ pcths + incidencerate + povertypercent
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1828 870928                                  
    ## 2   1827 765911  1    105016 250.51 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# target_deathrate ~ incidencerate + pcths + pctpubliccoveragealone + povertypercent
m4 <- lm(target_deathrate ~ incidencerate + pcths_raised + pctpubliccoveragealone + povertypercent, data= df)
summary(m4)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ incidencerate + pcths_raised + 
    ##     pctpubliccoveragealone + povertypercent, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -104.820  -11.905    0.517   11.494  136.932 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -0.312697   5.142539  -0.061 0.951520    
    ## incidencerate           0.205297   0.008566  23.967  < 2e-16 ***
    ## pcths_raised            3.258983   0.226714  14.375  < 2e-16 ***
    ## pctpubliccoveragealone  0.534798   0.140264   3.813 0.000142 ***
    ## povertypercent          1.294877   0.127972  10.118  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 20.36 on 1826 degrees of freedom
    ## Multiple R-squared:  0.4697, Adjusted R-squared:  0.4685 
    ## F-statistic: 404.3 on 4 and 1826 DF,  p-value: < 2.2e-16

``` r
plot(m4)
```

![](images/unnamed-chunk-93-7.png)![](images/unnamed-chunk-93-8.png)![](images/unnamed-chunk-93-9.png)![](images/unnamed-chunk-93-10.png)

``` r
#The ANOVA results show that adding the variable pcths (percentage of high school graduates) significantly improves the model in all #three comparisons. In each case, the p-values are extremely low (< 2.2e-16), indicating that the inclusion of pcths leads to a #statistically significant reduction in the residual sum of squares, improving the model's fit. This suggests that pcths provides #valuable explanatory power in predicting target_deathrate.
anova(m1,m4)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ incidencerate
    ## Model 2: target_deathrate ~ incidencerate + pcths_raised + pctpubliccoveragealone + 
    ##     povertypercent
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1829 1139089                                  
    ## 2   1826  757123  3    381966 307.07 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(m2,m4)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ povertypercent + incidencerate
    ## Model 2: target_deathrate ~ incidencerate + pcths_raised + pctpubliccoveragealone + 
    ##     povertypercent
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1828 870928                                  
    ## 2   1826 757123  2    113805 137.24 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(m3,m4)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: target_deathrate ~ pcths + incidencerate + povertypercent
    ## Model 2: target_deathrate ~ incidencerate + pcths_raised + pctpubliccoveragealone + 
    ##     povertypercent
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1   1827 765911                                  
    ## 2   1826 757123  1    8788.4 21.195 4.434e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# The Breusch-Pagan test was conducted to check for heteroscedasticity 
# (non-constant variance of residuals). With a p-value of 0.0086, the test
# indicates evidence of heteroscedasticity in the model. This suggests that the
# residuals' variance is not constant, potentially violating one of the
# assumptions of linear regression
bptest(m4)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  m4
    ## BP = 29.569, df = 4, p-value = 5.989e-06

``` r
# The VIF values for all predictors in the model (medincome, incidencerate, and
# pcths) are below 1.2, far below the common threshold of 5 or 10 for
# multicollinearity concerns. This indicates that the predictors are not highly
# correlated and do not pose a multicollinearity issue in the regression
vif(m4)
```

    ##          incidencerate           pcths_raised pctpubliccoveragealone 
    ##               1.009780               1.124244               3.185215 
    ##         povertypercent 
    ##               2.970894

``` r
t <- summary(m4)
1/(1-t$r.squared)
```

    ## [1] 1.885557

``` r
model <- m4
```

## Visualization and model diagnostics of the chosen model

``` r
lmBest <- model
plot(model)
```

![](images/unnamed-chunk-94-1.png)![](images/unnamed-chunk-94-2.png)![](images/unnamed-chunk-94-3.png)![](images/unnamed-chunk-94-4.png)

``` r
crPlots(model)
```

![](images/unnamed-chunk-94-5.png)

``` r
marginalModelPlots(model)
```

![](images/unnamed-chunk-94-6.png)

## Influential data

Before proceeding with the current model, let us determine whether there
are any data points that are particularly influential on the regression
coefficients.

### A-priori influential data

These are data points that are considerably far from the rest of the
cloud of points. They tend to have a high leverage and can significantly
affect the regression coefficients.

A common measure of leverage is the hat value, which is the diagonal
element of the hat matrix. Observations with a hat value greater than
2p/n, where p is the number of predictors and n is the number of
observations, are considered influential as a rule of thumb.

``` r
hat_values = hatvalues(model)
hat_threshold = 2 * length(coefficients(model)) / nrow(df)
influential_data = which(hat_values > hat_threshold)
length(influential_data)
```

    ## [1] 144

144 data points are found to be highly influential according to the hat
value criterion. We can see them visually via a simple Multidimensional
Scaling (MDS) plot.

``` r
par(mfrow = c(1, 1))
used_variables = attr(model$terms, "term.labels")
mds <- cmdscale(daisy(df[, used_variables]), k = 2) # Use dasy for mixed data types
plot(mds, col = ifelse(1:nrow(df) %in% influential_data, "red", "black"))
```

![](images/unnamed-chunk-96-1.png)

As we can see, the influential data points are scattered throughout the
plot, indicating that they are not clustered in any particular region of
the feature space. However, as expected, most of them are far from the
center of the cloud of points.

Let us remove these influential data points and re-fit the model to see
if the results change significantly.

``` r
model_no_priori = update(model, data = df[-influential_data, ])
summary(model_no_priori)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ incidencerate + pcths_raised + 
    ##     pctpubliccoveragealone + povertypercent, data = df[-influential_data, 
    ##     ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -91.094 -11.313   0.158  10.840 135.615 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              4.4789     5.8759   0.762   0.4460    
    ## incidencerate            0.1988     0.0103  19.302   <2e-16 ***
    ## pcths_raised             3.0393     0.2426  12.528   <2e-16 ***
    ## pctpubliccoveragealone   0.3920     0.1704   2.301   0.0215 *  
    ## povertypercent           1.5973     0.1581  10.100   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.28 on 1682 degrees of freedom
    ## Multiple R-squared:  0.4359, Adjusted R-squared:  0.4346 
    ## F-statistic:   325 on 4 and 1682 DF,  p-value: < 2.2e-16

Surprisingly, the model’s *R*<sup>2</sup> value has decreased slightly
after removing the influential data points. This suggests that the
influential data points were actually contributing to the model’s fit.
We will keep the original model for now.

### A-posteriori influential data

Having already a model defined, we can now search data points that have
actually significantly altered the regression coefficients.The Cook’s
distance is a measure of the influence of each observation on the
regression coefficients. Observations with a high Cook’s distance are
considered influential and can significantly affect the regression
coefficients.

``` r
cooks_distance = cooks.distance(model)
Boxplot(cooks_distance, id=list(labels=df$geography))
```

![](images/unnamed-chunk-98-1.png)

    ##  [1] "Williamsburg city, Virginia"        "Aleutians West Census Area, Alaska"
    ##  [3] "Madison County, Mississippi"        "Calhoun County, Georgia"           
    ##  [5] "Mora County, New Mexico"            "Presidio County, Texas"            
    ##  [7] "Baker County, Georgia"              "Randolph County, Georgia"          
    ##  [9] "Dooly County, Georgia"              "Coahoma County, Mississippi"

Only one data point seems to have a Cook’s distance significantly higher
than the rest: the county of “Williamsburg city, Viriginia”. This can be
further visualized using an influence plot.

``` r
influencePlot(model, id=list(labels=df$geography),
  main="Influence Plot", sub="Circle size is proportional to Cook's distance")
```

![](images/unnamed-chunk-99-1.png)

    ##                                       StudRes         Hat       CookD
    ## Williamsburg city, Virginia        -5.3888229 0.073476012 0.453616182
    ## Madison County, Mississippi         6.8215240 0.003949612 0.036005524
    ## Union County, Florida               0.5226007 0.103141212 0.006284216
    ## Aleutians West Census Area, Alaska  4.0358247 0.017023824 0.055948415

This plot shows as well the points that were determined as a-priori
influential in the previous step.

As a rule of thumb, observations with a Cook’s distance greater than 4/n
are considered influential. Let us remove the a-posteriori influential
points and re-fit the model.

``` r
influential_data = which(cooks_distance > 4/nrow(df)); length(influential_data)
```

    ## [1] 135

``` r
model_no_posteriori = update(model, data = df[-influential_data, ])
summary(model_no_posteriori)
```

    ## 
    ## Call:
    ## lm(formula = target_deathrate ~ incidencerate + pcths_raised + 
    ##     pctpubliccoveragealone + povertypercent, data = df[-influential_data, 
    ##     ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -50.068 -10.430   0.045  10.416  68.838 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -6.524208   4.769723  -1.368    0.172    
    ## incidencerate           0.203940   0.008557  23.834   <2e-16 ***
    ## pcths_raised            3.754005   0.197616  18.996   <2e-16 ***
    ## pctpubliccoveragealone  0.199678   0.125993   1.585    0.113    
    ## povertypercent          1.611601   0.114149  14.118   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.22 on 1691 degrees of freedom
    ## Multiple R-squared:  0.5575, Adjusted R-squared:  0.5565 
    ## F-statistic: 532.6 on 4 and 1691 DF,  p-value: < 2.2e-16

135 data points are found to be highly influential according to the
Cook’s distance criterion. We can see them visually via a simple
Multidimensional Scaling (MDS) plot.

``` r
par(mfrow = c(1, 1))
used_variables = attr(model$terms, "term.labels")
mds <- cmdscale(daisy(df[, used_variables]), k = 2) # Use dasy for mixed data types
plot(mds, col = ifelse(1:nrow(df) %in% influential_data, "red", "black"))
```

![](images/unnamed-chunk-101-1.png)

As before, the influential data points are scattered throughout the
plot, not clustered, and most of them are far from the center of the
cloud of points. When removing these influential data points, the
model’s *R*<sup>2</sup> value has increased significantly (To a 0.56),
suggesting that the influential data points were indeed distorting the
model’s fit. We will keep the model without the a-posteriori influential
data points.

``` r
model <- model_no_posteriori
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
# Type conversion
test$f.binnedinc <- as.factor(test$binnedinc)
test$binnedinc <- sapply(
  strsplit(gsub("[\\[\\]()]", "", test$binnedinc, perl = T), ","),
  function(x) mean(as.numeric(x))
)
test$state <- sub(".*,\\s*", "", test$geography)
test$f.race <- as.factor(apply(test, 1, getRace))

# Missing values
test <- subset(test, select = -c(pctprivatecoveragealone, pctsomecol18_24))
test$pctemployed16_over <- complete(mice(test), action = 1)$pctemployed16_over
```

    ## 
    ##  iter imp variable
    ##   1   1  pctemployed16_over
    ##   1   2  pctemployed16_over
    ##   1   3  pctemployed16_over
    ##   1   4  pctemployed16_over
    ##   1   5  pctemployed16_over
    ##   2   1  pctemployed16_over
    ##   2   2  pctemployed16_over
    ##   2   3  pctemployed16_over
    ##   2   4  pctemployed16_over
    ##   2   5  pctemployed16_over
    ##   3   1  pctemployed16_over
    ##   3   2  pctemployed16_over
    ##   3   3  pctemployed16_over
    ##   3   4  pctemployed16_over
    ##   3   5  pctemployed16_over
    ##   4   1  pctemployed16_over
    ##   4   2  pctemployed16_over
    ##   4   3  pctemployed16_over
    ##   4   4  pctemployed16_over
    ##   4   5  pctemployed16_over
    ##   5   1  pctemployed16_over
    ##   5   2  pctemployed16_over
    ##   5   3  pctemployed16_over
    ##   5   4  pctemployed16_over
    ##   5   5  pctemployed16_over

    ## Warning: Number of logged events: 27

``` r
# Outliers
test_out = which(test$medianage > 100)
test$medianage[test_out] <- (test$medianagemale[test_out] + test$medianagefemale[test_out]) / 2
test <- subset(test, select = -c(medianagemale, medianagefemale))

# Variable discretization
discretize_based_on <- function(col, base_col, level_name) {
  # Discretize the column based on the quartiles of another column
  res <- cut(col, breaks = quantile(base_col, probs = seq(0, 1, 0.25)),
    include.lowest = T,
    labels=c(
      sprintf("Low%s", level_name),
      sprintf("LowMid%s", level_name),
      sprintf("HighMid%s", level_name),
      sprintf("High%s", level_name)
    )
  )
  return(res)
}

test$f.avganncount <- discretize_based_on(test$avganncount, df$avganncount, "CaseCount")
test$f.avgdeathsperyear <- discretize_based_on(test$avgdeathsperyear, df$avgdeathsperyear, "MortCount")
test$f.target_deathrate <- discretize_based_on(test$target_deathrate, df$target_deathrate, "DeathRate")
test$f.incidencerate <- discretize_based_on(test$incidencerate, df$incidencerate, "DiagnPerCap")
test$f.medincome <- discretize_based_on(test$medincome, df$medincome, "MedianInc")
test$f.popest2015 <- discretize_based_on(test$popest2015, df$popest2015, "MidPop")
test$f.povertypercent <- discretize_based_on(test$povertypercent, df$povertypercent, "Pov%")
test$f.studypercap <- cut(
  test$studypercap, breaks = c(-Inf, 0, non_zero_studypercap_median, Inf), # Use the breakpoints from training data
  include.lowest = T,
  labels=c("NoTrials", "MidTrials", "HighTrials")
)
test$f.medianage <- discretize_based_on(test$medianage, df$medianage, "Age")
test$f.percentmarried <- discretize_based_on(test$percentmarried, df$percentmarried, "Married%")
test$f.pctnohs18_24 <- discretize_based_on(test$pctnohs18_24, df$pctnohs18_24, "NoHighsc%")
test$f.pcths18_24 <- discretize_based_on(test$pcths18_24, df$pcths18_24, "Highsc%")
test$f.pcths25_over <- discretize_based_on(test$pcths25_over, df$pcths25_over, "25Highsc%")
test$f.pctbachdeg25_over <- discretize_based_on(test$pctbachdeg25_over, df$pctbachdeg25_over, "Bach%")
test$f.pctemployed16_over <- discretize_based_on(test$pctemployed16_over, df$pctemployed16_over, "Employ%")
test$f.pctunemployed16_over <- discretize_based_on(test$pctunemployed16_over, df$pctunemployed16_over, "Unemploy%")
test$f.pctprivatecoverage <- discretize_based_on(test$pctprivatecoverage, df$pctprivatecoverage, "Private%")
test$f.pctempprivcoverage <- discretize_based_on(test$pctempprivcoverage, df$pctempprivcoverage, "EmployeeHealth%")
test$f.pctpubliccoverage <- discretize_based_on(test$pctpubliccoverage, df$pctpubliccoverage, "GovHealth%")
test$f.pctwhite <- discretize_based_on(test$pctwhite, df$pctwhite, "White%")
test$f.pctblack <- discretize_based_on(test$pctblack, df$pctblack, "Black%")
test$f.pctasian <- discretize_based_on(test$pctasian, df$pctasian, "Asian%")
test$f.pctotherrace <- discretize_based_on(test$pctotherrace, df$pctotherrace, "OtherRace%")
test$f.pctmarriedhouseholds <- discretize_based_on(test$pctmarriedhouseholds, df$pctmarriedhouseholds, "Married%")
test$f.birthrate <- discretize_based_on(test$birthrate, df$birthrate, "Birth%")

# Combining variables
test$pcths <- test$pcths18_24 + test$pcths25_over
test$pctbach <- test$pctbachdeg18_24 + test$pctbachdeg25_over
test$racindex <- test$pctblack + test$pctasian + test$pctotherrace
test$social_welfare <- test$pctpubliccoverage + test$povertypercent

# Raising to the optimal lambda
test$pcths_raised <- test$pcths^optimal_lambda_pcths
```

## Model Evaluation

Let’s evaluate the model on the test dataset. We will start by
predicting the target variable using the model and calculating the mean
squared error (MSE) and the R-squared value.

``` r
test$predicted_deathrate <- predict(model, newdata = test)

test$predicted_residuals <- test$target_deathrate - test$predicted_deathrate
mse <- mean((test$predicted_residuals)^2)
r_squared <- 1 - mse / var(test$target_deathrate)
cat("Mean Squared Error:", mse, "\n")
```

    ## Mean Squared Error: 406.2475

``` r
cat("R-squared:", r_squared, "\n")
```

    ## R-squared: 0.4624113

The model has an R-squared value of 0.46 on the test dataset, indicating
that it can account for 46% of the variance in the target death rate.

Another interesting metric to look at for determining the model’s
performance is the mean absolute error (MAE), which gives a better sense
of the model’s accuracy in predicting the target variable.

``` r
mae <- mean(abs(test$predicted_residuals))
cat("Mean Absolute Error:", mae, "\n")
```

    ## Mean Absolute Error: 15.25958

The model has a mean absolute error of 15.2 on the test dataset, which
means that, on average, the model’s predictions are off by 15.2 units
from the actual death rate.

This becomes more evident when plotting the predicted death rate against
the actual death rate, as well as the residuals against the actual death
rate.

``` r
par(mfrow = c(2, 2))
plot(test$target_deathrate, test$predicted_deathrate,
  main = "Predicted vs. Actual Death Rate",
  xlab = "Actual Death Rate", ylab = "Predicted Death Rate"
)
abline(0, 1, col = "red")

plot(test$predicted_residuals, test$predicted_deathrate,
  main = "Predicted Death Rate vs. Residuals",
  ylab = "Actual Death Rate", xlab = "Residuals"
)
abline(v = 0, col = "red")

plot(test$target_deathrate, test$predicted_residuals,
  main = "Residuals vs. Actual Death Rate",
  xlab = "Actual Death Rate", ylab = "Residuals"
)
abline(h = 0, col = "red")

hist_data = hist(test$predicted_residuals, plot = FALSE)
barplot(hist_data$counts,
  names.arg = hist_data$breaks[-length(hist_data$breaks)],
  axes= TRUE,
  space = 0,
  xlab = "Residuals", main = "Histogram of Residuals"
)
```

![](images/unnamed-chunk-106-1.png)

``` r
# Check whether the residuals are centered around 0 (if p>0.01, we can reject
# the null hypothesis that the residuals are centered around 0)
t.test(test$predicted_residuals)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  test$predicted_residuals
    ## t = -1.6352, df = 1215, p-value = 0.1023
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.0777285  0.1886936
    ## sample estimates:
    ##  mean of x 
    ## -0.9445175

As expected from an Ordinary Least Squares (OLS) model, the residuals
are centered around 0, and the predicted death rate is close to the
actual death rate. However, there is a clear trend in the residuals when
plotting them against the actual death rate. The residuals are higher
for higher death rates, indicating that the model is not capturing all
the variance in the target variable and may need further refinement.

# A word from the authors

In this analysis, we have explored the relationship between various
socio-economic and health-related factors and the rate of death related
to cancer of US counties. We have built a linear regression model that
predicts the death rate based on these factors and evaluated its
performance on a test dataset.

This work was an interesting exercise in data analysis and modeling, and
possibly our first glance at the complexity of finding the best
techniques to model a real-world problem. We have learned a lot about
the importance of data preprocessing, feature selection, and model
evaluation in building a predictive model.

Pretty much the entirety of the analysis was done in collaboration
between the three of us, although some parts were more heavily
influenced by one of us. For instance, Dani Reverter lead the way with
the preliminary data analysis, whilst Albert Puiggròs centered their
efforts on model discovery and fitting, and Marc Parcerisa focused on
model validation and influence analysis.
