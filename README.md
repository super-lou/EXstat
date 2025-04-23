# EXstat [<img src="figures/flower_hex.png" align="right" width=160 height=160 alt=""/>](https://github.com/super-lou/EXstat.CARD)

<!-- badges: start -->
[![R-CMD-check](https://github.com/super-lou/EXstat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/super-lou/EXstat/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green)](https://lifecycle.r-lib.org/articles/stages.html)
![](https://img.shields.io/github/last-commit/super-lou/EXstat)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md) 
<!-- badges: end -->

**EXstat** is a R package which provide an efficient and simple solution to aggregate and analyze the stationarity of time series.

EXstat is highly customizable, but the **EXstat.CARD** extension provides a simpler solution for performing common hydroclimatic aggregations. See the  [EXstat.CARD documentation](#extraction-process-with-card) or the [EXstat.CARD package](https://github.com/super-lou/EXstat.CARD) for advance understanding.

This project was carried out for National Research Institute for Agriculture, Food and the Environment (Institut National de Recherche pour l’Agriculture, l’Alimentation et l’Environnement, [INRAE](https://agriculture.gouv.fr/inrae-linstitut-national-de-recherche-pour-lagriculture-lalimentation-et-lenvironnement) in french) and is at the core of [MAKAHO](https://github.com/super-lou/MAKAHO)


## Installation
For latest development version
``` R
remotes::install_github("super-lou/EXstat")
```

And for [EXstat.CARD](https://github.com/super-lou/EXstat.CARD) extension latest development version
``` R
remotes::install_github("super-lou/EXstat.CARD")
```
(which will take care of EXstat installation also)


## Documentation
### Extraction process
Based on [dplyr](https://dplyr.tidyverse.org/), **input data** format is a `tibble` of at least a column of **date**, some columns of **numeric value** and one or more **character** columns for names of time series in order to identify them uniquely. Thus it is possible to have a `tibble` with multiple time series which can be grouped by their names.

For example, we can use the following `tibble` : 

``` R
library(dplyr)

# Date
Start = as.Date("1972-01-01")
End = as.Date("2020-12-31")
Date = seq.Date(Start, End, by="day")

# Value to analyse
set.seed(100)
X = seq(1, length(Date))/1e4 + runif(length(Date), -100, 100)
X[as.Date("2000-03-01") <= Date & Date <= as.Date("2000-09-30")] = NA

# Creation of tibble
data = tibble(Date=Date, ID="serie A", X=X)
```

Which looks like that :
``` R
> data
# A tibble: 17,898 × 3
   Date       ID           X
   <date>     <chr>    <dbl>
 1 1972-01-01 serie A -38.4 
 2 1972-01-02 serie A -48.5 
 3 1972-01-03 serie A  10.5 
 4 1972-01-04 serie A -88.7 
 5 1972-01-05 serie A  -6.29
 6 1972-01-06 serie A  -3.25
 7 1972-01-07 serie A  62.5 
 8 1972-01-08 serie A -25.9 
 9 1972-01-09 serie A   9.31
10 1972-01-10 serie A -65.9 
# ℹ 17,888 more rows
# ℹ Use `print(n = ...)` to see more rows
```

The process of **variable extraction** (for example the yearly mean of time series) is realised with the `process_extraction()` function.
Minimum arguments are :
* Input `data` described above
* The function `funct` (for example `mean`) you want to use. Arguments of the chosen function can be passed to this extraction process and the function can be previously defined.

Some of the optional arguments are :
* `period` A vector of two dates (or two unambiguous character strings that can be coerced to dates) to restrict the period of analysis. As an example, it can be `c("1950-01-01", "2020-12-31")` to select data from the 1st January of 1950 to the end of December of 2020. The default option is `period=NULL`, which considers all available data for each time serie.
* `time_step` A character string specifying the time step of the variable extraction process. Possible values are :
  - "year" for a value per year
  - "month" for a value for each month of the year (so 12 values if at least a full year is given)
  - "year-month" for a value for each month of each year (so 12 times the number of given year values at the end)
  - "season" for a value for each season of th year (so by default 4 values)
  - "year-season" for a value for each season of each year (so by default 4 times the number of given year values at the end)
  - "yearday" for one value per day of the year (so 365 values at the end if at least a full year is given... but more than one year seems obviously more interesting)
  - "none" if you want to extract a unique value for the whole time serie
* `sampling_period` A character string or a vector of two character strings that will indicate how to sample the data for each time step defined by `time_step`. Hence, the choice of this argument needs to be link with the choice of the time step. For example, for a yearly extraction so if `time_step` is set to `"year"`, `sampling_period` needs to be formated as `%m-%d` (a month - a day of the year) in order to indicate the start of the sampling of data for the current year. More precisly, if `time_step="year"` and `sampling_period="03-19"`, `funct` will be apply on every data from the 3rd march of each year to the 2nd march of the following one. In this way, it is possible to create a sub-year sampling with a vector of two character strings as `sampling_period=c("02-01", "07-31")` in order to process data only if the date is between the 1st february and the 31th jully of each year.

More parameters are available, for example, to :
* handle missing values,
* use suffixes to simplify expressions, and
* manage variables related to seasonality.

In this way
``` R
dataEX = process_extraction(data=data,
                            funct=max,
                            funct_args=list("X", na.rm=TRUE),
                            time_step="year",
                            sampling_period=c("05-01",
                                              "11-30"),
                            period=c(as.Date("1990-01-01"),
                                     as.Date("2020-12-31")))
```

will perform a yearly extraction of the maximum value between may and november, from the 1th march of 1990 to the 31th october of 2020, ignoring `NA` values.

The output is also a `tibble` with a column of **date**, of **character** for the name of time series and a **numerical** column with the extracted variable from the time series.

``` R
> dataEX
# A tibble: 31 × 3
   ID      Date           X
   <chr>   <date>     <dbl>
 1 serie A 1990-05-01 100. 
 2 serie A 1991-05-01 101. 
 3 serie A 1992-05-01 100. 
 4 serie A 1993-05-01  99.9
 5 serie A 1994-05-01  99.0
 6 serie A 1995-05-01 100. 
 7 serie A 1996-05-01 100. 
 8 serie A 1997-05-01 101. 
 9 serie A 1998-05-01  99.6
10 serie A 1999-05-01 101. 
# ℹ 21 more rows
# ℹ Use `print(n = ...)` to see more rows
```

Other examples of more complex cases are available in the package documentation. Try starting with 
``` R
library(EXstat)
?EXstat
```


### Extraction process with [CARD](https://github.com/super-lou/EXstat.CARD/)
For a more user-friendly interaction, this package has been developed in symbiosis with predefined parameterisation files called CARD.

So you don't have to define complex parameters yourself to extract hydroclimatological variables. What's more, if the CARD you want doesn't exist, it's easy to create one based on the others.

#### Basic workflow
For example, in hydrology, if you want to extract the annual mean daily discharge QA from hydrometric data
``` R
install.packages("airGRdatasets")
library(dplyr)

data = tibble(airGRdatasets::A273011002$TS) %>%
    mutate(code="A273011002",
           Date=as.Date(Date)) %>%
    rename(Q=Qls)
```		     

you can simply run
``` R
res = CARD_extraction(data, CARD_name="QA")
```

which will return
``` R
> res
$metaEX
# A tibble: 1 × 19
  variable_en unit_en      name_en description_en method_en sampling_period_en
  <chr>       <chr>        <chr>   <chr>          <chr>     <chr>             
1 QA          m^{3}.s^{-1} Annual… ""             1. annua… 09-01, 08-31      
# ℹ 13 more variables: topic_en <chr>, variable_fr <chr>, unit_fr <chr>,
#   name_fr <chr>, description_fr <chr>, method_fr <chr>,
#   sampling_period_fr <chr>, topic_fr <chr>,
#   preferred_hydrological_month <dbl>, is_date <lgl>, to_normalise <lgl>,
#   palette <chr>, script_path <chr>

$dataEX
$dataEX$QA
# A tibble: 21 × 3
   code       Date          QA
   <chr>      <date>     <dbl>
 1 A273011002 1998-09-01   NA 
 2 A273011002 1999-09-01 7048.
 3 A273011002 2000-09-01 6409.
 4 A273011002 2001-09-01 6403.
 5 A273011002 2002-09-01 4850.
 6 A273011002 2003-09-01 3768.
 7 A273011002 2004-09-01 5044.
 8 A273011002 2005-09-01 4805.
 9 A273011002 2006-09-01 7095.
10 A273011002 2007-09-01 5575.
# ℹ 11 more rows
# ℹ Use `print(n = ...)` to see more rows
```

So the result is a list of the metadata of the extraction in the `metaEX` tibble and the result of the extraction in the `dataEX` tibble.

In a similar, more complex way, you can extract multiple variables at a time with more than one discharge series,
``` R
# For one station
data1 = tibble(airGRdatasets::A273011002$TS) %>%
    mutate(code="A273011002",
           Date=as.Date(Date)) %>%
    rename(Q_obs=Qls)

# and an other
data2 = tibble(airGRdatasets::H622101001$TS) %>%
    mutate(code="H622101001",
           Date=as.Date(Date)) %>%
    rename(Q_obs=Qls)
# make one tibble
data = bind_rows(data1, data2)

# add some noise for mock simulation data
data$Q_sim = data$Q_obs + rnorm(nrow(data), mean=0, sd=100)

# and perfom an extraction
res = CARD_extraction(data,
                      CARD_name=c("QA", "QMNA", "VCN10-5"),
                      suffix=c("obs", "sim"))
```

which will return
``` R
> res
$metaEX
# A tibble: 3 × 19
  variable_en unit_en      name_en description_en method_en sampling_period_en
  <chr>       <chr>        <chr>   <chr>          <chr>     <chr>             
1 VCN10-5     m^{3}.s^{-1} Annual… ""             "1. no t…" "Month of maximum …"
2 QMNA        m^{3}.s^{-1} Annual… ""             "1. mont…" "Month of maximum …"
3 QA          m^{3}.s^{-1} Annual… ""             "1. annu…" 09-01, 08-31      
# ℹ 13 more variables: topic_en <chr>, variable_fr <chr>, unit_fr <chr>,
#   name_fr <chr>, description_fr <chr>, method_fr <chr>,
#   sampling_period_fr <chr>, topic_fr <chr>,
#   preferred_hydrological_month <dbl>, is_date <lgl>, to_normalise <lgl>,
#   script_path <chr>, palette <chr>

$dataEX
$dataEX$`VCN10-5`
# A tibble: 2 × 3
  code       `VCN10-5_obs` `VCN10-5_sim`
  <chr>              <dbl>         <dbl>
1 A273011002          914.          901.
2 H622101001         2770.         2769.

$dataEX$QMNA
# A tibble: 40 × 4
   code       Date       QMNA_obs QMNA_sim
   <chr>      <date>        <dbl>    <dbl>
 1 A273011002 1999-01-01    1050.    1070.
 2 A273011002 2000-01-01    2586.    2569.
 3 A273011002 2001-01-01    1401.    1408.
 4 A273011002 2002-01-01    1463.    1454.
 5 A273011002 2003-01-01    1182.    1167.
 6 A273011002 2004-01-01    1362.    1354.
 7 A273011002 2005-01-01    1245.    1264.
 8 A273011002 2006-01-01    1770     1778.
 9 A273011002 2007-01-01    1889.    1875.
10 A273011002 2008-01-01    1669.    1659.
# ℹ 30 more rows
# ℹ Use `print(n = ...)` to see more rows

$dataEX$QA
# A tibble: 42 × 4
   code       Date       QA_obs QA_sim
   <chr>      <date>      <dbl>  <dbl>
 1 A273011002 1998-09-01    NA     NA 
 2 A273011002 1999-09-01  7048.  7049.
 3 A273011002 2000-09-01  6409.  6419.
 4 A273011002 2001-09-01  6403.  6407.
 5 A273011002 2002-09-01  4850.  4842.
 6 A273011002 2003-09-01  3768.  3763.
 7 A273011002 2004-09-01  5044.  5045.
 8 A273011002 2005-09-01  4805.  4805.
 9 A273011002 2006-09-01  7095.  7093.
10 A273011002 2007-09-01  5575.  5562.
# ℹ 32 more rows
# ℹ Use `print(n = ...)` to see more rows
```


#### Custom workflow
Maybe you can't find the CARD that you want so you want to try to customize one or even create one based on other example. To do so, you need to first get the CARD example that you want in a local directory with for example
``` R
CARD_management(CARD_name=c("VCN10-5"), CARD_path="CARD-WIP")
```
that will create the `VCN10-5.R` CARD in the directory `"CARD-WIP"` of your working directory.

From that point, you can open this R file and for example change the metadata and the return period parameter from `5` to `10` in order to get the `VCN10-10` so the annual minimum of 10-day mean daily discharge with a return period of 10 years instead of 5.

And now for the extraction, just run
``` R
res = CARD_extraction(data, CARD_name=NULL,
                      CARD_path="CARD-WIP")
```
in order to perform the extraction of all the CARD in the `CARD_path` directory.

If you want to make a selection of the variable to extract in your custom CARD directory, simply use the variable `CARD_name` like previously seen in the [basic workflow](#basic-workflow) section.

Take a look at the [EXstat.CARD documentation](https://github.com/super-lou/EXstat.CARD?tab=readme-ov-file#exstatcard-) to better understand the CARD formatting.


### Trend analyse
The stationarity analyse is computed with the `process_trend()` function on the extracted data `dataEX`. The **statistical test** used here is the **Mann-Kendall test**[^mann][^kendall].

Hence, the following expression

``` R
trendEX = process_trend(data=dataEX)
```

produces the result below

```
# A tibble: 1 × 12
  ID      variable_en level H          p      a     b period_trend
  <chr>   <chr>       <dbl> <lgl>  <dbl>  <dbl> <dbl> <list>      
1 serie A X             0.1 TRUE  0.0958 0.0260  99.3 <date [2]>

  mean_period_trend a_normalise a_normalise_min a_normalise_max
  <lgl>                   <dbl>           <dbl>           <dbl>
1 NA                     0.0260          0.0260          0.0260
```

It is a `tibble` which precises, among other information, the name of the time serie the **p value** and `a` the **Theil-Sen's slope**[^theil][^sen], for each row.

Finaly, as the **p value** is below 0.1, the previous time serie shows an **increasing linear trend** which can be represented by the equation `Y = 0.0260*X + b` with a **type I error** of 10 % or a **trust** of 90 %. 


## FAQ
*I have a question.*

-   **Solution**: Search existing issue list and if no one has a similar question create a new issue.

*I found a bug.*

-   **Good Solution**: Search existing issue list and if no one has reported it create a new issue.
-   **Better Solution**: Along with issue submission provide a minimal reproducible example of the bug.
-   **Best Solution**: Fix the issue and submit a pull request. This is the fastest way to get a bug fixed.


## Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[^mann]: [Mann, H. B. (1945). Nonparametric tests against trend. Econometrica: Journal of the econometric society, 245-259.](https://www.jstor.org/stable/1907187)
[^kendall]: [Kendall, M.G. (1975) Rank Correlation Methods. 4th Edition, Charles Grifin, London.](https://www.scirp.org/reference/ReferencesPapers.aspx?ReferenceID=2223266)
[^theil]: [Theil, H. (1950). A rank-invariant method of linear and polynomial regression analysis. Indagationes mathematicae, 12(85), 173.](https://ir.cwi.nl/pub/8270/8270D.pdf)
[^sen]: [Sen, P. K. (1968). Estimates of the regression coefficient based on Kendall's tau. Journal of the American statistical association, 63(324), 1379-1389.](https://www.tandfonline.com/doi/abs/10.1080/01621459.1968.10480934)
