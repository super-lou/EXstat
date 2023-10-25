# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#           2023 Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of EXstat R package.
#
# EXstat R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# EXstat R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EXstat R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title process_extraction
#' @description This process extracts a variable from time series (for example the yearly mean of time series). Extraction can have a specific time step and sampled differently along this time step.
#'
#' @param data Input data format is a [tibble][tibble::tibble()] from the tibble package. It needs to have :
#' * Only one column of [Date][base::Date] that are regularly spaced and unique for each time serie.
#' * If there is more than one time serie, at least one column needs to be of [character][base::character] for names of time series in order to identify them. If more than one column of identifier is given, they will all be used in order to identify a unique time serie.
#' * At least one column of [numeric][base::numeric] (or [logical][base::logical]) on which the process of variable extraction will be perform. More numerical column can be leave but if they are useless, they will be suppressed.
#'
#' e.g.
#' ```
#' > data
#' A tibble: 201 × 4
#'    time         Q_obs  Q_sim  ID     
#'    <date>       <dbl>  <dbl>  <chr>  
#' 1   2000-02-10   10     97.8  serie 1
#' 2   2000-02-11   19    -20.5  serie 1
#' 3   2000-02-12   13    -76.9  serie 1
#' 4   2000-02-13   15    -86.0  serie 1
#'     ...
#' 103 2001-01-01  1.3     1988  serie 2
#' 104 2001-01-02  1.2      109  serie 2
#' 105 2001-01-03  1.0       90  serie 2
#' 106 2001-01-04  1.1       91  serie 2
#'     ...
#' ```
#' 
#' @param funct The function that you want to use for the process of variable extraction. More specificaly, it is possible to give a [list][base::list()] with several functions as element of that [list][base::list()] and the name that will be used for the extracted column as the names element of each function of that previously defined [list][base::list()]. A simple case will be `funct=mean` and a more complicated one `funct=list(QA=mean, QJXA=max)`.  
#' @param funct_args A [list][base::list()] of [list][base::list()] of named arguments needed for each functions provided through [funct]. This [list][base::list()] can be a simple [list][base::list()] if there is only one function given by [funct]. The argument can relate to a column name in order to specify on which numerical column the extraction will be perfom. For the simple example, `funct_args=list("Q_obs", na.rm=TRUE)` and for the more complex case `funct_args=list(list("Q_obs", na.rm=TRUE), list("Q_sim", na.rm=FALSE))`.
#' @param timeStep A [character][base::character] specifying the time step of the variable extraction process. Possible values are :
#' - "year" for a value per year
#' - "month" for a value for each month of the year (so 12 values if at least a full year is given)
#' - "year-month" for a value for each month of each year (so 12 times the number of given year values at the end)
#' - "season" for a value for each season of th year (so by default 4 values)
#' - "year-season" for a value for each season of each year (so by default 4 times the number of given year values at the end)
#' - "yearday" for one value per day of the year (so 365 values at the end if at least a full year is given... but more than one year seems obviously more interesting)
#' "none" if you want to extract a unique value for the whole time serie
#' @param samplePeriod A [character][base::character] or a [vector][base::c()] of two [characters][base::character] that will indicate how to sample the data for each time step defined by [timeStep]. Hence, the choice of this argument needs to be link with the choice of the time step. For example, for a yearly extraction so if [timeStep] is set to `"year"`, [samplePeriod] needs to be formated as `%m-%d` (a month - a day of the year) in order to indicate the start of the sampling of data for the current year. More precisly, if `timeStep="year"` and `samplePeriod="03-19"`, [funct] will be apply on every data from the 3rd march of each year to the 2nd march of the following one. In this way, it is possible to create a sub-year sampling with a [vector][base::c()] of two [characters][base::character] as `samplePeriod=c("02-01", "07-31")` in order to process data only if the date is between the 1st february and the 31th jully of each year.
#' **in developpement** For a monthly (or seasonal) extraction, [samplePeriod] needs to give only day in each month, so for example `samplePeriod="10"` to extract data from the 10th of each month to the 9th of each following month. 
#' @param period A [vector][base::c()] of two [dates][base::Date] (or two unambiguous [character][base::character] that can be coerced to [dates][base::Date]) to restrict the period of analysis. As an example, it can be `c("1950-01-01", "2020-12-31")` to select data from the 1st January of 1950 to the end of December of 2020. The default option is `samplePeriod=NULL`, which considers all available data for each time serie.
#' @param isDate [logical][base::logical]. If TRUE, [process_extration()] will convert the result of the application of [funct] to a day of the year. The aim is for example to give `funct=which.min` and if `isDate=TRUE`, the result will not be the indice of the minimum of the sample but the associated day of the year given by an [integer][base::integer] (1 is the 1st of january).
#' @param NApct_lim [numeric][base::numeric]. The maximum percentage of missing values in each sample allowed. If this threshold is exceeded, the value associated to the current sample will be convert to NA.
#' @param NAyear_lim [numeric][base::numeric].The maximum number of continuous missing years allowed. If this threshold is exceeded, the time serie will be split in half around the problematic period and only the longest part will be used for the extraction process.
#' @param Seasons A [vector][base::c()] of [characters][base::character] that indicates the seasonal pattern of a year. All months of the year needs to be contain in the [Seasons] variable. Give months circulary in a vector in which each element is a character chain of several months identify by the first letter of their names. The default is `Seasons=c("DJF", "MAM", "JJA", "SON")` but it can be set for example to `Seasons=c("MAMJJA", "SONDJF")`.
#' @param nameEX A [character][base::character] specifying the name of the column of the extracted variable if no name is given in [funct]. Default is `"X"`.
#' @param suffix A [character][base::character] [vector][base::c()] representing suffixes to be appended to the column names of the extracted variables. This parameter allows handling multiple extraction scenarios. For example, a cumbersome case can be to have a unique function to apply to a multiple list of column. It is possible to give `funct=list(QA_obs=mean, QA_sim=mean)` and `funct_args=list(list("Q_obs", na.rm=TRUE), list("Q_sim", na.rm=TRUE))` or simply `funct=list(QA=mean)` and `funct_args=list("Q", na.rm=TRUE)` with `suffix=c("_obs", "_sim")`. The two approach give the same result.
#' @param keep *in developpement* A [character][base::character] [vector][base::c()] of column names to keep in the output [tibble][tibble::tibble()]. In the current state, [keep] can only be set to `NULL` if you don't want to keep anythings in the output besides the usefull column, or `"all"` if you want to conserve all the initial column in the output column.
#' Warning : the number of row of the output with `keep="all"` is by consequences the same as the input. So, for example, the extracted value for a year of a daily time serie will be repeated each day of that year in the output.   
#' @param compress [logical][base::logical]. If [timeStep] is set to `"month"` or `"season"`, should the function return a classical [tibble][tibble::tibble()] or a compressed [tibble][tibble::tibble()] ? Hence for these two [timeStep] a small number of time chunk will be present in the output (12 for `timeStep="month"` and by default 4 for `timeStep="season"`), it is possible to make a [pivot_wider][tidyr::pivot_wider()] in order to have the time chunk indication in column and not anymore in line.
#'
#' e.g.
#' ```
#' # for timeStep="season" and compress=FALSE
#' # A tibble: 8 × 4
#'   ID       <time    Q    NApct
#'   <chr>    <chr>  <dbl>  <dbl>
#' 1 serie 1  DJF    1464    8.6
#' 2 serie 1  JJA    1447    0  
#' 3 serie 1  MAM    1395    0  
#' 4 serie 1  SON    1458    0  
#' 5 serie 2  DJF      11    8.6
#' 6 serie 2  JJA       2    0  
#' 7 serie 2  MAM       1    0  
#' 8 serie 2  SON       4    0
#'
#' # for timeStep="season" and compress=TRUE
#' # A tibble: 2 × 5
#'  ID       Q_DJF  Q_JJA  Q_MAM  Q_SON
#'  <chr>    <dbl>  <dbl>  <dbl>  <dbl>
#' 1 serie 1  1464  1447   1395   1458
#' 2 serie 2    11     2      1      4
#' ```
#' 
#' @param expand [logical][base::logical]. If `TRUE`, expand the output [tibble][tibble::tibble()] as a [list][base::list()] of [tibble][tibble::tibble()] for each extracted variable.
#' @param rmNApct [logical][base::logical]. Should the `NApct` column in the output that show the percentage of missing value should be remove ?
#' @param verbose [logical][base::logical]. Should intermediate messages be printed during the execution of the function ?
#' 
#' @examples
#' # Date
#' Start = as.Date("2000-02-01")
#' End = as.Date("2010-04-04")
#' Date = seq.Date(Start, End, by="day")
#' 
#' # Creation of random data set
#' set.seed(99)
#' data_1 = dplyr::tibble(time=Date,
#'                        X_state1=as.numeric(Date) +
#'                            rnorm(length(Date), 1e4, 1e3),
#'                        X_state2=seq(1, length(Date))/1e2 +
#'                            rnorm(length(Date), 0, 1),
#'                        id="serie 1")
#' data_1$X_state2[round(runif(500, 1, nrow(data_1)))] = NA
#' 
#' data_2 = dplyr::tibble(time=Date,
#'                        X_state1=as.numeric(Date) +
#'                            rnorm(length(Date), 1e4, 1e3),
#'                        X_state2=seq(1, length(Date))/1e2 +
#'                            rnorm(length(Date), 0, 1),
#'                        id="serie 2")
#' data_2$X_state2[round(runif(1000, 1, nrow(data_2)))] = NA
#' 
#' data = dplyr::bind_rows(data_1, data_2)
#'
#' # Extraction
#' process_extraction(data=data,
#'                    funct=list(XA_state1=mean),
#'                    funct_args=list("X_state1", na.rm=TRUE),
#'                    timeStep="year")
#'
#' dataEX_tmp =
#'     process_extraction(data=data,
#'                        funct=list(X_month_state2=mean),
#'                        funct_args=list("X_state2", na.rm=TRUE),
#'                        timeStep="year-month",
#'                        NApct_lim=20)
#' 
#' process_extraction(data=dataEX_tmp,
#'                    funct=list(XX_state2=max),
#'                    funct_args=list("X_month_state2", na.rm=TRUE),
#'                    samplePeriod=c("05-01", "11-30"),
#'                    timeStep="year",
#'                    rmNApct=TRUE)
#' 
#' process_extraction(data=data,
#'                    funct=list(XA_state1=mean,
#'                               XX_state2=max),
#'                    funct_args=list(list("X_state1", na.rm=TRUE),
#'                                    list("X_state2", na.rm=TRUE)),
#'                    timeStep="month")
#' 
#' process_extraction(data=data,
#'                    funct=list(XA_state1=mean,
#'                               XX_state2=max),
#'                    funct_args=list(list("X_state1", na.rm=TRUE),
#'                                    list("X_state2", na.rm=TRUE)),
#'                    timeStep="month",
#'                    compress=TRUE)
#' 
#' process_extraction(data=data,
#'                    funct=list(XA=mean),
#'                    funct_args=list(list("X", na.rm=TRUE)),
#'                    suffix=c("_state1", "_state2"),
#'                    timeStep="season",
#'                    expand=TRUE)
#' 
#' process_extraction(data=data,
#'                    funct=list(XA=mean),
#'                    funct_args=list(list("X", na.rm=TRUE)),
#'                    suffix=c("_state1", "_state2"),
#'                    timeStep="season",
#'                    compress=TRUE,
#'                    expand=TRUE)
#' 
#' @importFrom rlang .data
#' @importFrom rlang data_syms
#' @export
#' @md
process_extraction = function(data,
                              funct=max,
                              funct_args=list(),
                              timeStep="year",
                              samplePeriod=NULL,
                              period=NULL,
                              isDate=FALSE,
                              NApct_lim=NULL,
                              NAyear_lim=NULL,
                              Seasons=c("DJF", "MAM", "JJA", "SON"),
                              nameEX="X",
                              suffix=NULL,
                              suffix_delimiter="_",
                              keep=NULL,
                              compress=FALSE,
                              expand=FALSE,
                              rmNApct=FALSE,
                              rm_duplicates=FALSE,
                              dev=FALSE,
                              verbose=FALSE) {

    # print(data)
    # print(tail(data, n=20))
    
    # check data
    if (!tibble::is_tibble(data)) {
        stop ("'data' is not a tibble from the tibble package. This tibble needs a unique column of objects of class 'Date'")
    }
    
    # check Date column
    if (sum(sapply(data, lubridate::is.Date)) == 0 & timeStep != "none" & !dev) {
        stop ("There needs to be at least one column of objects of class 'Date'.")
    }
    if (sum(sapply(data, lubridate::is.Date)) > 1) {
        stop ("There is more than one column of objects of class 'Date'. There needs to be only one column of objects of class 'Date'.")
    }

    # check numerical columns
    if (sum(sapply(data, is.numeric) |
            sapply(data, is.logical)) < 1) {
        stop ("There needs to be at least one column of class 'numeric' or 'logical'.")
    }
    
    # check character columns
    ID_colnames = names(dplyr::select(data,
                                      dplyr::where(is.character)))
    if (sum(sapply(data, is.character)) == 0 & timeStep != "none" & !dev) {
        if (any(duplicated(
            data[[which(sapply(data,
                               lubridate::is.Date))]]))) {
            stop ("There is at least one date value that repeat. It seems that either there is more than one time serie (so they need to be identify by a repeted character column for each serie) or there is an error in the format of the date column.")
        } else {
            warning ("There is no character column in order to identify uniquely each time serie. But hence it seems that there is only one time serie, a generic identifier will be add.")
            data$id = "time serie"
        }
    } else if (sum(sapply(data, is.character)) > 1) {
        message ("There is more than one character column. Thus, all the columns will be use to identify uniquely each time serie.")
        data = tidyr::unite(data, "ID",
                            dplyr::where(is.character),
                            sep="_")
    }

    # DATE NA

    if (timeStep != "none" & !dev) {
        # check unicity of Date column for each character identifier
        Date_unicity =
            dplyr::summarise(dplyr::group_by(data,
                                             get(names(data)[sapply(data, is.character)])),
                             n=sum(duplicated(get(names(data)[sapply(data, lubridate::is.Date)]))))
        if (any(Date_unicity$n > 0)) {


            # print(dplyr::filter(dplyr::group_by(data,
                                                # get(names(data)[sapply(data, is.character)])),
                                # duplicated(get(names(data)[sapply(data, lubridate::is.Date)]))), n=Inf)
            
            # print(data, n=100)
            # print(Date_unicity, n=Inf)

            # print(data[data$ID == "O0174027_observee",], n=Inf)

            if (rm_duplicates) {
                warning (paste0("There is at least one duplicated date in time serie(s) named '",
                             paste0(Date_unicity[[1]][Date_unicity$n > 0],
                                    collapse=", "),
                             "'. 'rm_duplicates' is set to TRUE, so duplicated time serie(s) will be remove."))
                data =
                    dplyr::filter(dplyr::group_by(data,
                                                  get(names(data)[sapply(data, is.character)])),
                                  !duplicated(get(names(data)[sapply(data, lubridate::is.Date)])))
            } else {
                stop (paste0("There is at least one duplicated date in time serie(s) named '",
                             paste0(Date_unicity[[1]][Date_unicity$n > 0],
                                    collapse=", "),
                             "'. Set 'rm_duplicates' to TRUE if you automatically want to remove duplicated time serie(s)."))
            }
        }
    }
    
    # # check continuity of Date column for each character identifier
    # Date_continuity =
    #     dplyr::summarise(dplyr::group_by(dplyr::arrange(data, get(names(data)[sapply(data, lubridate::is.Date)])),
    #                                      get(names(data)[sapply(data, is.character)])),
    #                      n=length(unique(diff(get(names(data)[sapply(data, lubridate::is.Date)])))))

    # print(Date_continuity)
    # print(data)
    
    # if (any(Date_continuity$n > 1)) {
    #     stop (paste0("There is at least one date discontinuity in time serie(s) named '",
    #                  paste0(Date_continuity[[1]][Date_continuity$n > 1],
    #                         collapse=", "), "'. Please, make time serie(s) continuous by adding NA value in numerical column(s) where there is a missing value."))
    # }


    # check funct
    if (is.function(funct) | all(sapply(funct, is.function))) {
        if (is.function(funct)){
            funct = list(funct)
        }
        if (!is.list(funct_args[[1]])) {
            funct_args = list(funct_args)
        }
    } else {
        stop (paste0("'funct' is set to ", funct,
                     ". Please, use an existing function or define it before."))
    }
    
    # check funct_args
    if (!is.list(funct_args)) {
        stop ("'funct_args' needs to be an object of class 'list'.")
    }
    
    # check timeStep
    if (!(timeStep %in% c('none', 'year', 'yearday',
                          'month', 'year-month',
                          'season', 'year-season'))) {
        stop (paste0("'timeStep' is set to '", timeStep,
                     "'. Please select one of : 'none', 'year', 'yearday', 'month', 'year-month', 'season' and 'year-season'"))
    }

    # check samplePeriod
    if (!is.null(samplePeriod)) {        
        if (!all(is.na(samplePeriod))) {

            check_samplePeriod = function (samplePeriod,
                                           timeStep) {
                if (is.character(samplePeriod)) {
                    if (timeStep %in% c("year", "yearday", "none")) {
                        test = try(as.Date(paste0("1972-",
                                                  samplePeriod)),
                                   silent=TRUE)
                        if (any("try-error" %in% class(test)) ||
                            any(is.na(test))) {
                            stop (paste0("'samplePeriod' is not a 'character' element of the right format with a 'timeStep' : ",
                                         timeStep, "."))
                        }
                    } else if (timeStep %in% c("month", "year-month",
                                               "season", "year-season")) {
                        test = try(as.Date(paste0("1972-01-",
                                                  samplePeriod)),
                                   silent=TRUE)
                        if (any("try-error" %in% class(test)) ||
                            any(is.na(test))) {
                            stop (paste0("'samplePeriod' is not a 'character' element of the right format with a 'timeStep' : ",
                                         timeStep, "."))
                        }
                    }
                } else if (is.list(samplePeriod)) {
                    if (!is.function(samplePeriod[[1]]) |
                        !(timeStep %in% c("year", "yearday", "none"))) {
                        stop ("'samplePeriod' can be an object of class 'list' when 'timeStep' is set to 'year'. Moreover, the first argument needs to be an object of class 'function' to apply to the montly aggregated column of 'data' specify by the second character element of the list in order to select the starting month of each time serie.")
                    }
                }
            }

            if (tibble::is_tibble(samplePeriod)) {
                if ("sp" %in% names(samplePeriod) &
                    1 < ncol(samplePeriod) &
                    ncol(samplePeriod) <= 3 &
                    nrow(samplePeriod) ==
                    length(unique(data[[names(data)[
                                           sapply(data,
                                                  is.character)]]]))) {
                    dplyr::mutate(samplePeriod,
                                  check_samplePeriod(sp, timeStep))
                }
            } else if (is.character(samplePeriod) |
                       is.list(samplePeriod)) {
                check_samplePeriod(samplePeriod, timeStep)
            } else {
                stop ("'samplePeriod' needs to be an object of class 'character' or 'list', or, a tibble for which each row has a character identifier for a specified time serie and an object of class 'character' or 'list' in a column named 'sp'.")
            }
        }
    }

    # check period
    if (!is.null(period)) {
        test = try(as.Date(period), silent=TRUE)
        if (any("try-error" %in% class(test)) || any(is.na(test))) {
            stop ("'period' is not in a format able to be coerced to a 'Date' object")
        }
        if (length(period) == 1) {
            stop ("There is only one date in 'period'. Please, select a time period in your time serie(s) with two objects of class 'Date' or set 'period' to NULL in order to use the entire available time serie(s).")
        }
        if (length(period) > 2) {
            stop ("There is more than two date in 'period'. Please, select a time period in your time serie(s) with two objects of class 'Date' or set 'period' to NULL in order to use the entire available time serie(s).")
        }
        if (all(order(period) == c(2, 1))) {
            message ("'period' seems to have two date not in the increasing order. Thus, 'period' will be re-ordered.")
            period = sort(period)
        }
    }

    # check isDate
    if (!is.logical(isDate)) {
        stop ("'isDate' needs to be an object of class 'logical'.")
    }
    if (any(isDate) & !(timeStep %in% c("year", "year-month", "year-season"))) {
        warning ("'isDate' is coherced to FALSE. 'isDate' can be TRUE only if 'timeStep' is 'year', 'year-month' or 'year-season'.")
        isDate = FALSE
    }

    # check NApct_lim
    if (!is.null(NApct_lim)) {
        if (!is.numeric(NApct_lim) |
            !all(0 <= NApct_lim & NApct_lim <= 100) |
            length(NApct_lim) > 1) {
            stop ("'NApct_lim' needs to be an object of class 'numeric' between 0 and 100 of length 1.")
        }
    }

    # check NAyear_lim
    if (!is.null(NAyear_lim)) {
        if (!is.numeric(NAyear_lim) |
            all(NAyear_lim <= 0) |
            length(NAyear_lim) > 1) {
            stop ("'NAyear_lim' needs to be an object of class 'numeric', strictly positif, of length 1.")
        }
    }

    # check Seasons
    Seasons_split = unlist(strsplit(Seasons, "*"))
    if ("D" %in% Seasons_split) {
        n = which(Seasons_split == "D") - 1
        if (n > 0) {
            Seasons_split = c(tail(Seasons_split, -n),
                              head(Seasons_split, n))
        }
        if (paste0(Seasons_split, collapse="") != "DJFMAMJJASON") {
            stop ("All months are not correctly specified in the seasonal patern contain in the 'Season' variable. Please use all the month circulary without missing month in a vector which each element is a character chain of several month identify by the first letter of their names - ex. Seasons=c('DJF', 'MAM', 'JJA', 'SON').")
        }
    } else {
        stop ("At least december ('D') is not present in the seasonal patern contain in the 'Season' variable. Please use all the month circulary without missing month in a vector which each element is a character chain of several month identify by the first letter of their names - ex. Seasons=c('DJF', 'MAM', 'JJA', 'SON').")
    }

    # check nameEX
    if (!is.character(nameEX)) {
        stop ("'nameEX' needs to be an object of class 'character'.")
    }

    # check suffix
    if (!is.null(suffix)) {
        if (!is.character(suffix)) {
            stop ("'suffix' needs to be an object of class 'character'.")
        }
        if (!is.null(suffix_delimiter)) {
            if (is.character(suffix) & length(suffix_delimiter) == 1) {
                suffix = paste0(suffix_delimiter, suffix)
            } else {
                stop ("'suffix_delimiter' needs to be an object of class 'character' of length 1.")
            }
        } else {
            stop ("'suffix_delimiter' needs to be an object of class 'character' of length 1.")
        }
    }

    # check keep
    if (!is.null(keep)) {
        if (!is.character(keep)) {
            stop ("'keep' needs to be an object of class 'character'.")
        }
        if (timeStep %in% c("month", "season", "yearday")) {
            warning ("'keep' is coherced to NULL. 'keep' can be non NULL only if 'timeStep' is not 'month', 'season' or 'yearday'.")
            keep = NULL  
        }
    }

    # check compress
    if (!is.logical(compress)) {
        stop ("'compress' needs to be an object of class 'logical'.")
    }
    # check compress and timeStep
    if (compress & !(timeStep %in% c("year-month", "month",
                                     "year-season", "season"))) {
        warning ("'compress' is coherced to FALSE. 'compress' can be TRUE only if 'timeStep' is 'month', 'year-month', 'season' or 'year-season'.")
        compress = FALSE
    }

    # check expand
    if (!is.logical(expand)) {
        stop ("'expand' needs to be an object of class 'logical'.")
    }

    # check rmNApct
    if (!is.logical(rmNApct)) {
        stop ("'rmNApct' needs to be an object of class 'logical'.")
    }

    # check verbose
    if (!is.logical(verbose)) {
        stop ("'verbose' needs to be an object of class 'logical'.")
    }



    tree("EXTRACTION PROCESS", 0, verbose=verbose)
    
    names_save = names(data)
    idCode_save = NULL
    idDate_save = NULL
    idValue_save = c()
    
    for (id in 1:ncol(data)) {
        x = data[[id]]

        if (is.character(x)) {
            idCode_save = id
        } else if (lubridate::is.Date(x)) {
            idDate_save = id
        } else if (is.numeric(x) | is.logical(x)) {
            idValue_save = c(idValue_save, id)
        }
    }

    if (is.character(period)) {
        period = as.Date(period)
        if (is.na(period[1])) {
            period[1] = min(data[[idDate_save]], na.rm=TRUE)
        }
        if (is.na(period[2])) {
            period[2] = max(data[[idDate_save]], na.rm=TRUE)
        }
    }
    
    if (!is.null(idDate_save)) {
        data = dplyr::relocate(data,
                               names_save[idDate_save],
                               .before=dplyr::everything())
    }
    data = dplyr::relocate(data,
                           names_save[idCode_save],
                           .before=dplyr::everything())

    names_save = names(data)
    idValue_save = c()
    for (id in 1:ncol(data)) {
        x = data[[id]]

        if (is.character(x)) {
            idCode_save = id
        } else if (lubridate::is.Date(x)) {
            idDate_save = id
        } else if (is.numeric(x) | is.logical(x)) {
            idValue_save = c(idValue_save, id)
        }
    }

    
    if (!exists("idDate_save")) {
        idDate_save = NULL
    }

    idValue_keepSave = idValue_save
    names_keepSave = names_save
    
    nValue = length(idValue_save)
    colName = paste0("Value", 1:nValue)

    nfunct = length(funct)

    if (length(isDate) != nfunct) {
        isDate = rep(isDate[1], nfunct)
    }
    
    if (!is.null(suffix)) {
        where_no_suffix = c()
        for (i in 1:nfunct) {
            arg = funct_args[[i]]            
            arg_suffix =
                unlist(lapply(unlist(arg),
                              paste0, suffix))

            # print(arg_suffix)
            # print(names_save)
            # print("")
            
            where_no_suffix = c(where_no_suffix,
                                !any(arg_suffix %in%
                                    names_save))
        }
    } else {
        where_no_suffix = rep(TRUE, nfunct)
    }


    # print("")
    # print("suffix")
    # print(suffix)

    # print("nfunct")
    # print(nfunct)
    # print("where_no_suffix")
    # print(where_no_suffix)
    
    if (!is.null(suffix)) {
        nfunct_tmp = length(funct)
        nsuffix_tmp = length(suffix)
        if (nsuffix_tmp > 1) {
            funct_args = rep(funct_args, nsuffix_tmp)
            funct = rep(funct, nsuffix_tmp)
            isDate = rep(isDate, nsuffix_tmp)
            where_no_suffix = rep(where_no_suffix,
                                  nsuffix_tmp)
        }
        suffix = rep(suffix, each=nfunct_tmp)
        funct2keep =
            !duplicated(as.numeric(where_no_suffix),
                        incomparables=0)
        
        funct = funct[funct2keep]
        funct_args = funct_args[funct2keep]
        isDate = isDate[funct2keep]
        suffix = suffix[funct2keep]
        where_no_suffix = where_no_suffix[funct2keep]
    }

    get_colarg = function (arg_match, colName) {
        colName[arg_match]
    }
    
    colArgs_save = list()
    colArgs = list()
    otherArgs = list()
    colArgs_order = list()
    
    nfunct = length(funct)
    
    for (i in 1:nfunct) {
        arg = funct_args[[i]]

        isDateColArgs = any(arg %in% names_save[idDate_save])
        if (isDateColArgs) {
            names_save = c(names_save, "ValueDate")
            idValue_save = c(idValue_save, max(idValue_save)+1)
            colName = c(colName, paste0("Value",
                                        length(idValue_save)))
            data["ValueDate"] = data[idDate_save]
            funct_args[[i]][arg %in% names_save[idDate_save]] =
                "ValueDate"
            arg = funct_args[[i]]
        }
        
        if (!is.null(suffix) & !where_no_suffix[i]) {
            arg_match = lapply(lapply(arg, paste0, suffix[i]),
                               match,
                               table=names_save[idValue_save])
        } else {
            arg_match = lapply(arg,
                               match,
                               table=names_save[idValue_save])

            if (!is.null(suffix)) {
                suffix[i] = ""
            }
        }
        
        okNA = sapply(lapply(arg_match, is.na), any)
        
        colarg = lapply(arg_match[!okNA], get_colarg, colName)
        otherarg = arg[okNA]

        colArgs = append(colArgs,
                         list(colarg))
        
        otherArgs = append(otherArgs,
                           list(otherarg))
    }

    if (length(colArgs) == 0) {
        stop (paste0("Are the given parameters that refer to column names spelled correctly ? ",
                     funct_args, " is given but only names in ",
                     paste0(names_keepSave[idValue_save], collapse=", "),
                     " are possible."))
    }    

    if (!is.null(idDate_save)) {
        names(data)[c(idCode_save, idDate_save, idValue_save)] =
            c("Code", "Date", unlist(colName))
    } else {
        names(data)[c(idCode_save, idValue_save)] =
            c("Code", unlist(colName))
    }
    
    if (!is.null(names(funct)) & all(names(funct) != "")) {
        nameEX = names(funct)
    } else {
        if (length(nameEX) != nfunct) {
            nameEX = paste0(nameEX[1], 1:nfunct)
        }
    }

    # nameEX_noSuffix = nameEX
    
    if (!is.null(suffix)) {
        nameEX = paste0(nameEX, suffix)
    }
    
    if (nValue > nfunct) {
        names_save = names_save[-c(idValue_save[(nfunct+1):nValue])]
        idValue_save = idValue_save[1:nfunct]
        
    } else if (nValue < nfunct) {
        idValue_save = idValue_save[1:nfunct]
        isNA = is.na(idValue_save)
        nNA = sum(isNA)
        maxId = max(idValue_save, na.rm=TRUE)
        idValue_save[isNA] = (maxId + 1):(maxId + nNA)
    }
    
    names_save[idValue_save[1:nfunct]] = nameEX

    if (!is.null(samplePeriod) &
        dplyr::is.tbl(samplePeriod)) {
        
        if ("args" %in% names(samplePeriod)) {
            apply_name = function (X, table, name) {
                Xres = name[match(X, table)]
                Xres[is.na(Xres)] = X[is.na(Xres)]
                names(Xres) = names(X)
                return (Xres)
            }

            if (!is.null(suffix)) {
                arg_match = unlist(lapply(lapply(samplePeriod$args,
                                                 paste0, suffix[1]),
                                          match,
                                          table=names_keepSave))
                okNA = sapply(lapply(arg_match, is.na), any)
                colarg = unlist(lapply(arg_match[!okNA], get_colarg, names_keepSave))
                samplePeriod$args[[1]][!okNA][[1]] = colarg
            }
            
            samplePeriod$args = lapply(samplePeriod$args,
                                       apply_name,
                                       table=names_keepSave,
                                       name=names(data))
        } else {
            samplePeriop$args = NA
        }
    }

    if (!is.null(NAyear_lim) & !is.null(idDate_save)) {
        data = missing_year(data, nValue, NAyear_lim,
                            verbose=verbose)
    }


    tree("Period", 1, verbose=verbose)
    if (is.null(period) | is.null(idDate_save)) {
        tree("Selecting all the data",
             2, TRUE, verbose=verbose)
    } else {
        tree(paste0("Selecting data between ",
                    format(period[1], "%d %b %Y"),
                    " and ",
                    format(period[2], "%d %b %Y")),
             2, TRUE, verbose=verbose)
        data = dplyr::filter(data, period[1] <= Date & Date <= period[2])
    }
    
    if (timeStep %in% c("year", "yearday", "none")) {
        refDate = "1972"
        sampleFormat = "%m-%d"
        
    } else if (timeStep %in% c("month", "year-month",
                               "season", "year-season")) {
        refDate = "1972-07"
        sampleFormat = "%d"
    }
    Code = names(table(data$Code))
    
    tree("Sample period", 1, verbose=verbose)

    if (is.null(samplePeriod) | all(is.na(samplePeriod))) {
        tree("Default sample period used", 2,
             verbose=verbose)
        if (timeStep %in% c("year", "yearday", "none")) {
            samplePeriod = "01-01"
        } else if (timeStep %in% c("month", "year-month",
                                   "season", "year-season")) {
            samplePeriod = "01"
        }
    }
    
    if (dplyr::is.tbl(samplePeriod)) {

        if (nrow(samplePeriod) == 1) {
            samplePeriod = dplyr::tibble(Code=Code,
                                         samplePeriod)
            
        } else {
            idCode = which((names(samplePeriod) %in% names_save))        
            names(samplePeriod)[idCode] = c("Code")
            samplePeriod = samplePeriod[samplePeriod$Code %in% Code,]
        }

        tree("Fixing sample period for each time series", 2,
             verbose=verbose)

        samplePeriod =
            dplyr::summarise(
                       dplyr::group_by(samplePeriod,
                                       Code),
                       sp=list(
                           fix_samplePeriod(
                               sp,
                               data_code=
                                   data[data$Code ==
                                        dplyr::cur_group()$Code,],
                               args=args,
                               suffix=suffix[1],
                               refDate=refDate,
                               sampleFormat=sampleFormat,
                               verbose=FALSE)),
                       .groups="drop")
        
    } else {

        d = try(as.Date(paste0(refDate, "-", samplePeriod),
                        format="%Y-%m-%d"))        
        if ("try-error" %in% class(d) || any(is.na(d))) {
            if (timeStep %in% c("year", "yearday", "none")) {
                stop ("'samplePeriod' given is not in the right format. You should use two valid number to select a month and a day of the year separated by a '-' for the starting (and/or ending) of the sample period (i.e. '02-10' for the 10th of february). See documentation.")
            } else {
                stop ("'samplePeriod' given is not in the right format. You should use one valid number to select a day of a month for the starting (and/or ending) of the sample period (i.e. '10' for the 10th day of the month). See documentation.")
            }
        }

        if (is.character(samplePeriod)) {
            tree("Fixing sample period", 2, end=TRUE,
                 verbose=verbose)
            samplePeriod = fix_samplePeriod(samplePeriod,
                                            refDate=refDate,
                                            sampleFormat=sampleFormat,
                                            verbose=verbose)
            
        } else {
            tree("Sample period fully given", 2, end=TRUE,
                 verbose=verbose)
        }
        samplePeriod = dplyr::tibble(Code=Code,
                                     sp=rep(list(samplePeriod),
                                            length(Code)))
    }

    samplePeriod$spStart = sapply(samplePeriod$sp, "[[", 1)
    samplePeriod$spEnd = sapply(samplePeriod$sp, "[[", 2)
    
    samplePeriod$refStart = as.Date(paste0(refDate,
                                           '-',
                                           samplePeriod$spStart))
    samplePeriod$refEnd = as.Date(paste0(refDate,
                                         '-',
                                         samplePeriod$spEnd))
    
    if (length(samplePeriod$spStart[!duplicated(samplePeriod$spStart)]) == 1 &
        length(samplePeriod$spEnd[!duplicated(samplePeriod$spEnd)]) == 1) {
        
        tree("Every time series have the same sample period", 3, inEnd=2, verbose=verbose)
        tree(paste0("All : ", samplePeriod$spStart[1],
                    " / ", samplePeriod$spEnd[1]),
             3, end=TRUE, inEnd=2, verbose=verbose)
        
    } else {
        toTree = paste0(samplePeriod$Code, " : ",
                        samplePeriod$spStart,
                        " / ",
                        samplePeriod$spEnd)
        
        nToTree = length(toTree)
        for (t in 1:nToTree) {
            if (t == nToTree) {
                end = TRUE
            } else {
                end = FALSE
            }
            tree(toTree[t], 3, end=end, inEnd=2, verbose=verbose)
        }
    }

    
    
    if (timeStep == "none") {
        tree("None extraction", 1, verbose=verbose)

        tree("Preparing date data for the extraction",
             end=TRUE, 2, verbose=verbose)
        tree("Get general sample info", 3, inEnd=2, verbose=verbose)
        
        samplePeriod$dt2add = 0
        samplePeriod$dt2add[samplePeriod$refStart >
                            samplePeriod$refEnd] = 1

        samplePeriod$isStartAfter29Feb =
            samplePeriod$refStart > as.Date(paste0(refDate, "-02-29"))

        samplePeriod =
            dplyr::mutate(samplePeriod,
                          refStart=
                              dplyr::if_else(
                                         isStartAfter29Feb,
                                         lubridate::add_with_rollback(
                                                        refStart,
                                                        -lubridate::years(dt2add)),
                                         refStart),
                          refEnd=
                              dplyr::if_else(
                                         isStartAfter29Feb,
                                         refEnd,
                                         lubridate::add_with_rollback(
                                                        refEnd,
                                                        lubridate::years(dt2add))),
                          is29FebIn=
                              refStart <= as.Date("1972-02-29") &
                              as.Date("1972-02-29") <= refEnd,
                          interval=as.numeric(refEnd - refStart + 1))

        data = dplyr::full_join(data,
                                samplePeriod[c("Code",
                                               "spStart",
                                               "spEnd",
                                               "dt2add")],
                                by="Code")
        
        
        if (any(samplePeriod$interval != 366)) {
            tree("Sampling of the data", 3, inEnd=2, verbose=verbose)
            
            samplePeriod$mStart =
                as.numeric(substr(samplePeriod$spStart, 1, 2))
            samplePeriod$dStart =
                as.numeric(substr(samplePeriod$spStart, 4, 5))
            samplePeriod$mEnd =
                as.numeric(substr(samplePeriod$spEnd, 1, 2))
            samplePeriod$dEnd =
                as.numeric(substr(samplePeriod$spEnd, 4, 5))
            
            data = dplyr::full_join(data,
                                    samplePeriod[c("Code",
                                                   "mStart",
                                                   "dStart",
                                                   "mEnd",
                                                   "dEnd")],
                                    by="Code")
            
            data = dplyr::filter(data,
                                 
            dt2add == 0
            & (mStart < lubridate::month(Date)
                | (mStart == lubridate::month(Date)
                    & dStart <= lubridate::day(Date)))
            & (lubridate::month(Date) < mEnd
                | (lubridate::month(Date) == mEnd
                    & lubridate::day(Date) <= dEnd))
            
            |
            
            dt2add == 1
            & ((lubridate::month(Date) < mEnd
                | (lubridate::month(Date) == mEnd
                    & lubridate::day(Date) <= dEnd))
                | (mStart < lubridate::month(Date)
                    | (mStart == lubridate::month(Date)
                        & dStart <= lubridate::day(Date))))
            )
            
            data = dplyr::select(data, -c(mStart,
                                          dStart,
                                          mEnd,
                                          dEnd))
        }
        
        data = dplyr::select(data, -c(spStart,
                                      spEnd,
                                      dt2add))

    } else if (timeStep == "year") {
        tree("Yearly extraction", 1, verbose=verbose)

        tree("Preparing date data for the extraction",
             end=TRUE, 2, verbose=verbose)
        tree("Get general sample info", 3, inEnd=2, verbose=verbose)
        
        samplePeriod$dt2add = 0
        samplePeriod$dt2add[samplePeriod$refStart >
                            samplePeriod$refEnd] = 1

        samplePeriod$isStartAfter29Feb =
            samplePeriod$refStart > as.Date(paste0(refDate, "-02-29"))

        samplePeriod =
            dplyr::mutate(samplePeriod,
                          refStart=
                              dplyr::if_else(
                                         isStartAfter29Feb,
                                         lubridate::add_with_rollback(
                                                        refStart,
                                                        -lubridate::years(dt2add)),
                                         refStart),
                          refEnd=
                              dplyr::if_else(
                                         isStartAfter29Feb,
                                         refEnd,
                                         lubridate::add_with_rollback(
                                                        refEnd,
                                                        lubridate::years(dt2add))),
                          is29FebIn=
                              refStart <= as.Date("1972-02-29") &
                              as.Date("1972-02-29") <= refEnd,
                          interval=as.numeric(refEnd - refStart + 1))


        data = dplyr::full_join(data,
                                samplePeriod[c("Code",
                                               "spStart",
                                               "spEnd",
                                               "dt2add")],
                                by="Code")
        
        
        if (any(samplePeriod$interval != 366)) {
            tree("Sampling of the data", 3, inEnd=2, verbose=verbose)
            
            samplePeriod$mStart =
                as.numeric(substr(samplePeriod$spStart, 1, 2))
            samplePeriod$dStart =
                as.numeric(substr(samplePeriod$spStart, 4, 5))
            samplePeriod$mEnd =
                as.numeric(substr(samplePeriod$spEnd, 1, 2))
            samplePeriod$dEnd =
                as.numeric(substr(samplePeriod$spEnd, 4, 5))
            
            data = dplyr::full_join(data,
                                    samplePeriod[c("Code",
                                                   "mStart",
                                                   "dStart",
                                                   "mEnd",
                                                   "dEnd")],
                                    by="Code")

            data = dplyr::filter(data,
                                 
            dt2add == 0
            & (mStart < lubridate::month(Date)
                | (mStart == lubridate::month(Date)
                    & dStart <= lubridate::day(Date)))
            & (lubridate::month(Date) < mEnd
                | (lubridate::month(Date) == mEnd
                    & lubridate::day(Date) <= dEnd))
            
            |
            
            dt2add == 1
            & ((lubridate::month(Date) < mEnd
                | (lubridate::month(Date) == mEnd
                    & lubridate::day(Date) <= dEnd))
                | (mStart < lubridate::month(Date)
                    | (mStart == lubridate::month(Date)
                        & dStart <= lubridate::day(Date))))
            )
            
            data = dplyr::select(data, -c(mStart,
                                          dStart,
                                          mEnd,
                                          dEnd))
        }
        
        tree("Computing of time indicators for each time serie",
             3, inEnd=2, verbose=verbose)
        
        sampleInfo =
            dplyr::summarise(dplyr::group_by(data, Code),
                             
                             minDate=min(Date),
                             minYear=lubridate::year(minDate),
                             minSpStart=dplyr::if_else(spStart[1] == "02-29",
                                                       "02-28",
                                                       spStart[1]),
                             maxDate=max(Date),
                             maxYear=lubridate::year(maxDate),
                             maxSpEnd=dplyr::if_else(spEnd[1] == "02-29",
                                                     "02-28",
                                                     spEnd[1]),

                             rm2start=
                                 dplyr::if_else(
                                            dt2add[1] == 1 &
                                            minDate < 
                                            as.Date(paste0(minYear,
                                                           "-",
                                                           minSpStart)),
                                            1, 0),

                             rm2end=
                                 dplyr::if_else(
                                            dt2add[1] == 1 &
                                            maxDate < 
                                            as.Date(paste0(maxYear,
                                                           "-",
                                                           minSpStart)),
                                            1, 0),

                             add2end=
                                 dplyr::if_else(
                                            dt2add[1] == 1 &
                                            maxDate >
                                            as.Date(paste0(maxYear - rm2end,
                                                           "-",
                                                           maxSpEnd)),
                                            1, 0),
                             

                             minSampleStart=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(minYear,
                                                               "-",
                                                               minSpStart)),
                                                -lubridate::years(rm2start)),
                             minSampleStart=
                                 dplyr::if_else(
                                            check_leapYear(
                                                lubridate::year(minSampleStart)) &
                                            spStart[1] == "02-29",
                                            minSampleStart+1,
                                            minSampleStart),

                             maxSampleStart= 
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(maxYear,
                                                               "-",
                                                               minSpStart)),
                                                -lubridate::years(rm2end)),
                             maxSampleStart=
                                 dplyr::if_else(
                                            check_leapYear(
                                                lubridate::year(maxSampleStart)) &
                                            spStart[1] == "02-29",
                                            maxSampleStart+1,
                                            maxSampleStart),

                             maxSampleEnd=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(
                                                    lubridate::year(maxSampleStart),
                                                    "-",
                                                    maxSpEnd)),
                                                lubridate::years(add2end)),
                             maxSampleEnd=
                                 dplyr::if_else(
                                            check_leapYear(
                                                lubridate::year(maxSampleEnd)) &
                                            spEnd[1] == "02-29",
                                            maxSampleEnd+1,
                                            maxSampleEnd),
                             
                             .groups="drop")

        data = dplyr::select(data, -c(spStart,
                                      spEnd,
                                      dt2add))
        
        sampleInfo = dplyr::full_join(sampleInfo,
                                      samplePeriod,
                                      by="Code")
        samplePeriod = dplyr::select(samplePeriod, Code, sp)

        tree("Get number of missing data for start and end",
             3, inEnd=2, verbose=verbose)

        
        sampleInfo$dNAstart =
            pmax(0, as.numeric(sampleInfo$minDate -
                               sampleInfo$minSampleStart))
        sampleInfo$dNAend =
            pmax(0, as.numeric(sampleInfo$maxSampleEnd -
                               sampleInfo$maxDate))

        if (is.null(keep)) {
            minRef = "minSampleStart"
            maxRef = "maxSampleStart"
        } else {
            if (any(names_keepSave[idValue_keepSave] %in% keep) | keep == "all") {
                minRef = "minDate"
                maxRef = "maxDate"
            } else {
                minRef = "minSampleStart"
                maxRef = "maxSampleStart"
            }
        }
        
        sampleStartInfo =
            sampleInfo[c("Code", minRef, "dNAstart")]
        names(sampleStartInfo) = c("Code", "Date", "dNA")
        sampleEndInfo =
            sampleInfo[c("Code", maxRef, "dNAend")]
        names(sampleEndInfo) = c("Code", "Date", "dNA")
        
        sampleInfoCompress = dplyr::bind_rows(sampleStartInfo,
                                              sampleEndInfo)

        sampleInfoCompress$Year =
            lubridate::year(sampleInfoCompress$Date)


        tree("Create each group",
             3, end=TRUE, inEnd=2, verbose=verbose)
        
        Group = dplyr::reframe(dplyr::group_by(sampleInfo,
                                               Code),
                               Year =
                                   lubridate::year(minSampleStart):
                                   lubridate::year(maxSampleStart))

        Group = dplyr::full_join(Group,
                                 dplyr::select(sampleInfoCompress,
                                               Code,
                                               Year,
                                               dNA),
                                 by=c("Code", "Year"))
        Group = tidyr::replace_na(Group, list(dNA=0)) 
        Group = dplyr::full_join(Group,
                                 dplyr::select(sampleInfo,
                                               Code,
                                               interval,
                                               isStartAfter29Feb,
                                               is29FebIn),
                                 by=c("Code"))

        Group = dplyr::mutate(
                           Group, 
                           isLeapYear=
                               dplyr::if_else(
                                          isStartAfter29Feb,
                                          check_leapYear(Year+1),
                                          check_leapYear(Year)))
        
        Group = dplyr::mutate(Group,
                              leapYear=
                                  dplyr::if_else(isLeapYear,
                                                 0, 1),
                              leapYear=
                                  dplyr::if_else(is29FebIn,
                                                 leapYear, 0),
                              size=interval-leapYear-dNA)

        # print(sampleInfo, width=Inf)
        # print(Group, n=Inf)
        
        Group = dplyr::filter(Group,
                              size > 0)
        Group = dplyr::reframe(Group,
                               group=rep(Year, size))
        
        # print(data, n=Inf)
        # print(data)
        # print(Group)
        
        data = dplyr::bind_cols(data, Group)
        

### Yearday __________________________________________________________
    } else if (timeStep == "yearday") {
        
        tree("Yearday extraction", 1, verbose=verbose)

        tree("Preparing date data for the extraction",
             end=TRUE, 2, verbose=verbose)
        tree("Get general sample info", 3, inEnd=2, verbose=verbose)
        
        samplePeriod$dt2add = 0
        samplePeriod$dt2add[samplePeriod$refStart >
                            samplePeriod$refEnd] = 1

        samplePeriod$isStartAfter29Feb =
            samplePeriod$refStart > as.Date(paste0(refDate, "-02-29"))

        samplePeriod =
            dplyr::mutate(samplePeriod,
                          refStart=
                              dplyr::if_else(
                                         isStartAfter29Feb,
                                         lubridate::add_with_rollback(
                                                        refStart,
                                                        -lubridate::years(dt2add)),
                                         refStart),
                          refEnd=
                              dplyr::if_else(
                                         isStartAfter29Feb,
                                         refEnd,
                                         lubridate::add_with_rollback(
                                                        refEnd,
                                                        lubridate::years(dt2add))),
                          is29FebIn=
                              refStart <= as.Date("1972-02-29") &
                              as.Date("1972-02-29") <= refEnd,
                          interval=as.numeric(refEnd - refStart + 1))


        data = dplyr::full_join(data,
                                samplePeriod[c("Code",
                                               "spStart",
                                               "spEnd",
                                               "dt2add")],
                                by="Code")
        
        
        if (any(samplePeriod$interval != 366)) {
            
            samplePeriod$mStart =
                as.numeric(substr(samplePeriod$spStart, 1, 2))
            samplePeriod$dStart =
                as.numeric(substr(samplePeriod$spStart, 4, 5))
            samplePeriod$mEnd =
                as.numeric(substr(samplePeriod$spEnd, 1, 2))
            samplePeriod$dEnd =
                as.numeric(substr(samplePeriod$spEnd, 4, 5))
            
            data = dplyr::full_join(data,
                                    samplePeriod[c("Code",
                                                   "mStart",
                                                   "dStart",
                                                   "mEnd",
                                                   "dEnd")],
                                    by="Code")

            data = dplyr::filter(data,
                                 
            dt2add == 0
            & (mStart < lubridate::month(Date)
                | (mStart == lubridate::month(Date)
                    & dStart <= lubridate::day(Date)))
            & (lubridate::month(Date) < mEnd
                | (lubridate::month(Date) == mEnd
                    & lubridate::day(Date) <= dEnd))
            
            |
            
            dt2add == 1
            & ((lubridate::month(Date) < mEnd
                | (lubridate::month(Date) == mEnd
                    & lubridate::day(Date) <= dEnd))
                | (mStart < lubridate::month(Date)
                    | (mStart == lubridate::month(Date)
                        & dStart <= lubridate::day(Date))))
            )
            
            data = dplyr::select(data, -c(mStart,
                                          dStart,
                                          mEnd,
                                          dEnd))
        }

        tree("Computing of time indicators for each time serie",
             3, inEnd=2, verbose=verbose)

        sampleInfo =
            dplyr::summarise(dplyr::group_by(data, Code),
                             
                             minDate=min(Date),
                             minYear=lubridate::year(minDate),
                             minSpStart=dplyr::if_else(spStart[1] == "02-29",
                                                       "02-28",
                                                       spStart[1]),
                             maxDate=max(Date),
                             maxYear=lubridate::year(maxDate),
                             maxSpEnd=dplyr::if_else(spEnd[1] == "02-29",
                                                     "02-28",
                                                     spEnd[1]),

                             rm2start=
                                 dplyr::if_else(
                                            dt2add[1] == 1 &
                                            minDate < 
                                            as.Date(paste0(minYear,
                                                           "-",
                                                           minSpStart)),
                                            1, 0),
                             add2end=
                                 dplyr::if_else(
                                            dt2add[1] == 1 &
                                            maxDate < 
                                            as.Date(paste0(maxYear,
                                                           "-",
                                                           maxSpEnd)),
                                            1, 0),

                             rm2end=
                                 dplyr::if_else(
                                            dt2add[1] == 1 &
                                            maxDate < 
                                            as.Date(paste0(maxYear,
                                                           "-",
                                                           minSpStart)),
                                            1, 0),
                             

                             minSampleStart=
                                            lubridate::add_with_rollback(
                                                           as.Date(paste0(minYear,
                                                                          "-",
                                                                          minSpStart)),
                                                           -lubridate::years(rm2start)),
                             minSampleStart=
                                 dplyr::if_else(
                                            check_leapYear(
                                                lubridate::year(minSampleStart)) &
                                            spStart[1] == "02-29",
                                            minSampleStart+1,
                                            minSampleStart),

                             maxSampleStart=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(maxYear,
                                                               "-",
                                                               minSpStart)),
                                                -lubridate::years(rm2end)),
                             maxSampleStart=
                                 dplyr::if_else(
                                            check_leapYear(
                                                lubridate::year(maxSampleStart)) &
                                            spStart[1] == "02-29",
                                            maxSampleStart+1,
                                            maxSampleStart),

                             maxSampleEnd=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(
                                                    lubridate::year(maxSampleStart),
                                                    "-",
                                                    maxSpEnd)),
                                                lubridate::years(add2end)),
                             maxSampleEnd=
                                 dplyr::if_else(
                                            check_leapYear(
                                                lubridate::year(maxSampleEnd)) &
                                            spEnd[1] == "02-29",
                                            maxSampleEnd+1,
                                            maxSampleEnd),

                             nYear=
                                 lubridate::year(maxSampleStart) -
                                 lubridate::year(minSampleStart),
                             
                             .groups="drop")

        data = dplyr::select(data, -c(spStart,
                                      spEnd,
                                      dt2add))
        
        sampleInfo = dplyr::full_join(sampleInfo,
                                      samplePeriod,
                                      by="Code")
        samplePeriod = dplyr::select(samplePeriod, Code, sp)

        tree("Get number of missing data for start and end",
             3, inEnd=2, verbose=verbose)
        
        sampleInfo$dNAstart =
            pmax(0, as.numeric(sampleInfo$minDate -
                               sampleInfo$minSampleStart))
        sampleInfo$dNAend =
            pmax(0, as.numeric(sampleInfo$maxSampleEnd -
                               sampleInfo$maxDate))

        sampleStartInfo =
            sampleInfo[c("Code", "minSampleStart",
                         "minSampleStart", "dNAstart")]
        names(sampleStartInfo) = c("Code", "Date", "Date_end", "dNA")
        sampleEndInfo =
            sampleInfo[c("Code", "maxSampleStart",
                         "maxSampleEnd", "dNAend")]
        names(sampleEndInfo) = c("Code", "Date", "Date_end", "dNA")
        
        sampleInfoCompress = dplyr::bind_rows(sampleStartInfo,
                                              sampleEndInfo)

        sampleInfoCompress$Year =
            lubridate::year(sampleInfoCompress$Date)

        
        tree("Create each group",
             3, end=TRUE, inEnd=2, verbose=verbose)
        
        Group = dplyr::reframe(dplyr::group_by(sampleInfo,
                                               Code),
                               Year =
                                   lubridate::year(minSampleStart):
                                   lubridate::year(maxSampleStart))

        Group = dplyr::full_join(Group,
                                 dplyr::select(sampleInfoCompress,
                                               Code,
                                               Year,
                                               dNA),
                                 by=c("Code", "Year"))
        Group = tidyr::replace_na(Group, list(dNA=0)) 
        Group = dplyr::full_join(Group,
                                 dplyr::select(sampleInfo,
                                               Code,
                                               interval,
                                               isStartAfter29Feb,
                                               is29FebIn,
                                               spStart),
                                 by=c("Code"))

        Group = dplyr::mutate(
                           Group, 
                           isLeapYear=
                               dplyr::if_else(
                                          isStartAfter29Feb,
                                          check_leapYear(Year+1),
                                          check_leapYear(Year)))
        
        Group = dplyr::mutate(Group,
                              leapYear=
                                  dplyr::if_else(isLeapYear,
                                                 0, 1),
                              leapYear=
                                  dplyr::if_else(is29FebIn,
                                                 leapYear, 0),
                              size=interval-leapYear-dNA)

        Group = dplyr::filter(Group,
                              size > 0)

        Group = dplyr::reframe(
                           dplyr::group_by(Group, Code, Year),
                           Date=as.Date(paste0(Year,
                                               "-",
                                               spStart)),
                           Date=seq.Date(
                               Date+dNA,
                               Date+dNA+size-1,
                               "days"),
                           group=
                               lubridate::yday(Date))
        Group = dplyr::select(Group, group)
        
        data = dplyr::bind_cols(data, Group)

        
        sampleInfoCompress =
            dplyr::reframe(
                       dplyr::group_by(sampleInfoCompress,
                                       Code, Date, Date_end),
                       Yearday=lubridate::yday(Date + 0:(dNA-1)),
                       Yearday_end=lubridate::yday(Date_end - 0:(dNA-1)),
                       dNA=1)
        sampleInfoCompress =
            dplyr::mutate(sampleInfoCompress,
                          Yearday=dplyr::if_else(
                                             Date == Date_end,
                                             Yearday,
                                             Yearday_end))
        sampleInfoCompress =
            dplyr::summarise(
                       dplyr::group_by(sampleInfoCompress,
                                       Code, Yearday),
                       dNA=sum(dNA),
                       .groups="drop")
        
        
### Month ____________________________________________________________
    } else if (timeStep %in% c("month", "year-month")) {

        if (timeStep == "month") {
            tree("Monthly extraction", 1, verbose=verbose)
        } else if (timeStep == "year-month") {
            tree("Monthly extraction along years", 1, verbose=verbose)
        }
        
        tree("Preparing date data for the extraction",
             end=TRUE, 2, verbose=verbose)
        tree("Get general sample info", 3, inEnd=2, verbose=verbose)
        
        samplePeriod$dt2add = 0
        samplePeriod$dt2add[samplePeriod$refStart >
                            samplePeriod$refEnd] = 1

        # samplePeriod$isStartAfter29Feb =
        #     samplePeriod$refStart > as.Date(paste0(refDate, "-02-29"))

        samplePeriod =
            dplyr::mutate(samplePeriod,
                          refEnd=
                              dplyr::if_else(
                                         dt2add == 0,
                                         refEnd,
                                         lubridate::add_with_rollback(
                                                        refEnd,
                                                        months(1))),
                          is29In=
                              dplyr::if_else(
                                         dt2add == 0,
                                         as.numeric(spStart) <= 29 &
                                         29 <= as.numeric(spEnd),
                                         as.numeric(spStart) <= 29 &
                                         as.numeric(spEnd) <= 29),

                          is31In=
                              dplyr::if_else(
                                         dt2add == 0,
                                         as.numeric(spEnd) == 31,
                                         TRUE),
                          
                          interval=as.numeric(refEnd - refStart + 1))

        data = dplyr::full_join(data,
                                samplePeriod[c("Code",
                                               "spStart",
                                               "spEnd",
                                               "dt2add"
                                               # "interval"
                                               )],
                                by="Code")

        # if (any(samplePeriod$interval != 31)) {

        # tree("Sampling of the data", 3, inEnd=2, verbose=verbose)
        
        #     samplePeriod$dStart = as.numeric(substr(samplePeriod$spStart, 1, 2))
        #     samplePeriod$dEnd = as.numeric(substr(samplePeriod$spEnd, 1, 2))
            
        #     data = dplyr::full_join(data,
        #                             samplePeriod[c("Code",
        #                                            "dStart",
        #                                            "dEnd")],
        #                             by="Code")
            
        #     data = dplyr::filter(data,
                                 
        #     dt2add == 0
        #     & dStart <= lubridate::day(Date)
        #     & lubridate::day(Date) <= dEnd
            
        #     |
            
        #     dt2add == 1
        #     & (lubridate::day(Date) <= dEnd
        #         | dStart <= lubridate::day(Date))
            
        #     )
            
        #     data = dplyr::select(data, -c(dStart,
        #                                   dEnd))
        # }

        tree("Computing of time indicators for each time serie",
             3, inEnd=2, verbose=verbose)

        sampleInfo =
            dplyr::summarise(dplyr::group_by(data, Code),
                             
                             minDate=min(Date),
                             minYear=lubridate::year(minDate),
                             minMonth=lubridate::month(minDate),
                             
                             maxDate=max(Date),
                             maxYear=lubridate::year(maxDate),
                             maxMonth=lubridate::month(maxDate),
                             
                             minSampleStart=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(minYear,
                                                               "-01-",
                                                               spStart[1])),
                                                months(minMonth-1)),

                             maxSampleStart=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(maxYear,
                                                               "-01-",
                                                               spStart[1])),
                                                months(maxMonth - 1)),
                             
                             maxSampleEnd=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(maxYear,
                                                               "-01-",
                                                               spEnd[1])),
                                                months(maxMonth + dt2add[1] - 1)),

                             nYear=
                                 lubridate::year(maxSampleStart) -
                                 lubridate::year(minSampleStart),
                             .groups="drop")

        data = dplyr::select(data, -c(spStart,
                                      spEnd,
                                      dt2add))
        
        sampleInfo = dplyr::full_join(sampleInfo,
                                      samplePeriod,
                                      by="Code")
        samplePeriod = dplyr::select(samplePeriod, Code, sp)

        
        tree("Get number of missing data for start and end",
             3, inEnd=2, verbose=verbose)
        
        sampleInfo$dNAstart =
            pmax(0, as.numeric(sampleInfo$minDate -
                               sampleInfo$minSampleStart))
        sampleInfo$dNAend =
            pmax(0, as.numeric(sampleInfo$maxSampleEnd -
                               sampleInfo$maxDate))

        if (timeStep == "year-month") {
            if (is.null(keep)) {
                minRef = "minSampleStart"
                maxRef = "maxSampleStart"
            } else {
                if (any(names_keepSave[idValue_keepSave] %in% keep) | keep == "all") {
                    minRef = "minDate"
                    maxRef = "maxDate"
                } else {
                    minRef = "minSampleStart"
                    maxRef = "maxSampleStart"
                }
            }
            
            sampleStartInfo =
                sampleInfo[c("Code", minRef, "dNAstart")]
            names(sampleStartInfo) = c("Code", "Date", "dNA")
            sampleEndInfo =
                sampleInfo[c("Code", maxRef, "dNAend")]
            names(sampleEndInfo) = c("Code", "Date", "dNA")

        } else if (timeStep == "month") {
            sampleStartInfo =
                sampleInfo[c("Code", "minSampleStart", "dNAstart")]
            names(sampleStartInfo) = c("Code", "Date", "dNA")
            sampleEndInfo =
                sampleInfo[c("Code", "maxSampleStart", "dNAend")]
            names(sampleEndInfo) = c("Code", "Date", "dNA")
        }

        sampleInfoCompress = dplyr::bind_rows(sampleStartInfo,
                                              sampleEndInfo)
        sampleInfoCompress$Year =
            lubridate::year(sampleInfoCompress$Date)
        sampleInfoCompress$Month =
            lubridate::month(sampleInfoCompress$Date)

        tree("Create each group",
             3, end=TRUE, inEnd=2, verbose=verbose)
        
        # Group = dplyr::reframe(dplyr::group_by(sampleInfo,
        #                                        Code),
        #                        Year =
        #                            lubridate::year(minSampleStart):
        #                            lubridate::year(maxSampleStart))
        # Group =
        #     dplyr::reframe(
        #                dplyr::group_by(Group,
        #                                Code, Year),
        #                Month = 1:12)

        # Group = dplyr::full_join(Group,
        #                          dplyr::select(sampleInfo,
        #                                        Code,
        #                                        minDate,
        #                                        maxDate),
        #                          by="Code")

        # firstYear = dplyr::filter(Group, Year == min(Year))
        # Group = dplyr::filter(Group, Year != min(Year))
        
        # firstYear = dplyr::mutate(dplyr::group_by(firstYear, Code),
        #                           lim = which(Month ==
        #                                       lubridate::month(minDate)))
        # firstYear = dplyr::filter(dplyr::group_by(firstYear, Code),
        #                           Month >= lim)
        # firstYear = dplyr::select(firstYear, -lim)
        
        # Group = dplyr::bind_rows(Group, firstYear)
        # Group = dplyr::select(Group, -minDate)

        # lastYear = dplyr::filter(Group, Year == max(Year))
        # Group = dplyr::filter(Group, Year != max(Year))
        
        # lastYear = dplyr::mutate(dplyr::group_by(lastYear, Code),
        #                          lim = which(Month ==
        #                                      lubridate::month(maxDate)))
        # lastYear = dplyr::filter(dplyr::group_by(lastYear, Code),
        #                          Month <= lim)
        # lastYear = dplyr::select(lastYear, -lim)
        
        # Group = dplyr::bind_rows(Group, lastYear)
        # Group = dplyr::arrange(Group, Code, Year)
        # Group = dplyr::select(Group, -maxDate)

        # Group = dplyr::full_join(Group,
        #                          dplyr::select(sampleInfoCompress,
        #                                        Code,
        #                                        Year,
        #                                        Month,
        #                                        dNA),
        #                          by=c("Code", "Year", "Month"))
        # Group = tidyr::replace_na(Group, list(dNA=0)) 
        # Group = dplyr::full_join(Group,
        #                          dplyr::select(sampleInfo,
        #                                        Code,
        #                                        interval,
        #                                        is29In,
        #                                        is31In),
        #                          by=c("Code"))

        
        # Group = dplyr::mutate(Group, 
        #                       isLeapYear=
        #                           check_leapYear(Year))


        # # c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        # d29Day = c( 0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0)
        # d31Day = c( 0,  1,  0,  1,  0,  1,  0,  0,  1,  0,  1,  0)

        
        # Group = dplyr::mutate(Group,
        #                       leapYear=
        #                           dplyr::if_else(isLeapYear,
        #                                          0, 1),
        #                       leapYear=
        #                           dplyr::if_else(is29In & Month == 2,
        #                                          leapYear, 0),
        #                       dDay=
        #                           dplyr::if_else(is29In,
        #                                          d29Day[Month],
        #                                          0),
        #                       dDay=
        #                           dplyr::if_else(is31In,
        #                                          dDay+d31Day[Month],
        #                                          dDay),
        #                       size=interval-leapYear-dNA-dDay)
        # Group = dplyr::filter(Group,
        #                       size > 0)

        if (timeStep == "year-month") {
            data$group = format(data$Date, "%Y-%m")
            # Group$YearMonth = paste0(Group$Year, "-",
            #                          formatC(Group$Month, width=2, flag="0"))
            # Group = dplyr::reframe(Group,
            #                        group=rep(YearMonth, size))
            
        } else if (timeStep == "month") {
            # Group = dplyr::reframe(Group,
            #                        group=rep(Month, size))
            data$group = format(data$Date, "%m")
            sampleInfoCompress$Month = lubridate::month(sampleInfoCompress$Date)
            sampleInfoCompress =
                dplyr::summarise(
                           dplyr::group_by(sampleInfoCompress,
                                           Code, Month),
                           dNA=sum(dNA),
                           .groups="drop")
        }

        # data = dplyr::bind_cols(data, Group)

        
### Season ___________________________________________________________
    } else if (timeStep %in% c("season", "year-season")) {

        if (timeStep == "season") {
            tree("Seasonly extraction", 1, verbose=verbose)
        } else if (timeStep == "year-season") {
            tree("Seasonly extraction along years", 1, verbose=verbose)
        }
        tree(paste0("Creating of the conversion process from months ",
                    "to seasons based on seasons vector ",
                    paste0(Seasons, collapse=" - ")),
             2, verbose=verbose)
        nMonthSeasons = sapply(Seasons, nchar)
        
        get_Season = rep(Seasons, nMonthSeasons)
        
        idJan = (which(unlist(strsplit(paste0(Seasons,
                                              collapse=""),
                                       "")) == "D") + 1) %% 12
        
        get_Season = c(get_Season[idJan:12], get_Season[1:(idJan-1)])

        subSeasons = unlist(lapply(nMonthSeasons, seq, from=1)) - 1
        names(subSeasons) = NULL
        subSeasons = c(subSeasons[idJan:12],
                       subSeasons[1:(idJan-1)])

        addSeasons = unlist(lapply(nMonthSeasons, seq, to=1)) - 1
        names(addSeasons) = NULL
        addSeasons = c(addSeasons[idJan:12],
                       addSeasons[1:(idJan-1)])

        orderSeasons = get_Season[!duplicated(get_Season)]
        orderSeasons = orderSeasons[!is.na(orderSeasons)]
        nMonthOrderSeasons = sapply(orderSeasons, nchar)
        endSeasonsMonth = cumsum(nMonthOrderSeasons)
        startSeasonsMonth = endSeasonsMonth - nMonthOrderSeasons + 1
        idOrderJan = (which(unlist(strsplit(paste0(orderSeasons,
                                                   collapse=""),
                                            "")) == "D") + 1) %% 12
        startSeasonsMonth = (startSeasonsMonth - idOrderJan) %% 12 + 1
        
        SeasonsMonth = rep(startSeasonsMonth, nMonthOrderSeasons)
        SeasonsMonth = c(SeasonsMonth[idJan:12],
                         SeasonsMonth[1:(idJan-1)])


        tree("Preparing date data for the extraction",
             end=TRUE, 2, verbose=verbose)
        tree("Get general sample info", 3, inEnd=2, verbose=verbose)
        
        samplePeriod$dt2add = 0
        samplePeriod$dt2add[samplePeriod$refStart >
                            samplePeriod$refEnd] = 1

        # samplePeriod$isStartAfter29Feb =
        #     samplePeriod$refStart > as.Date(paste0(refDate, "-02-29"))

        samplePeriod =
            dplyr::mutate(samplePeriod,
                          refEnd=
                              dplyr::if_else(
                                         dt2add == 0,
                                         refEnd,
                                         lubridate::add_with_rollback(
                                                        refEnd,
                                                        months(1))),
                          is29In=
                              dplyr::if_else(
                                         dt2add == 0,
                                         as.numeric(spStart) <= 29 &
                                         29 <= as.numeric(spEnd),
                                         as.numeric(spStart) <= 29 &
                                         as.numeric(spEnd) <= 29),

                          is31In=
                              dplyr::if_else(
                                         dt2add == 0,
                                         as.numeric(spEnd) == 31,
                                         TRUE),
                          
                          interval=as.numeric(refEnd - refStart + 1))

        data = dplyr::full_join(data,
                                samplePeriod[c("Code",
                                               "spStart",
                                               "spEnd",
                                               "dt2add"
                                               # "interval"
                                               )],
                                by="Code")

        # if (any(samplePeriod$interval != 31)) {

        #     tree("Sampling of the data", 3, inEnd=2, verbose=verbose)
            
        #     samplePeriod$dStart = as.numeric(substr(samplePeriod$spStart, 1, 2))
        #     samplePeriod$dEnd = as.numeric(substr(samplePeriod$spEnd, 1, 2))
            
        #     data = dplyr::full_join(data,
        #                             samplePeriod[c("Code",
        #                                            "dStart",
        #                                            "dEnd")],
        #                             by="Code")
            
        #     data = dplyr::filter(data,
                                 
        #     dt2add == 0
        #     & dStart <= lubridate::day(Date)
        #     & lubridate::day(Date) <= dEnd
            
        #     |
            
        #     dt2add == 1
        #     & (lubridate::day(Date) <= dEnd
        #         | dStart <= lubridate::day(Date))
            
        #     )
            
        #     data = dplyr::select(data, -c(dStart,
        #                                   dEnd))
        # }

        tree("Computing of time indicators for each time serie",
             3, inEnd=2, verbose=verbose)

        sampleInfo =
            dplyr::summarise(dplyr::group_by(data, Code),
                             
                             minDate=min(Date),
                             minYear=lubridate::year(minDate),
                             minMonth=lubridate::month(minDate),
                             
                             maxDate=max(Date),
                             maxYear=lubridate::year(maxDate),
                             maxMonth=lubridate::month(maxDate),

                             minSampleStart=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(
                                                    minYear,
                                                    "-01-",
                                                    spStart[1])),
                                                months(minMonth - 1 -
                                                       subSeasons[minMonth])),
                             maxSampleStart=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(
                                                    maxYear,
                                                    "-01-",
                                                    spStart[1])),
                                                months(maxMonth - 1 -
                                                       subSeasons[maxMonth])),
                             maxSampleEnd=
                                 lubridate::add_with_rollback(
                                                as.Date(paste0(
                                                    maxYear,
                                                    "-01-",
                                                    spEnd[1])),
                                                months(maxMonth +
                                                       dt2add[1] +
                                                       addSeasons[maxMonth] - 1)),

                             nYear=
                                 lubridate::year(maxSampleStart) -
                                 lubridate::year(minSampleStart),
                             .groups="drop")

        data = dplyr::select(data, -c(spStart,
                                      spEnd,
                                      dt2add))
        
        sampleInfo = dplyr::full_join(sampleInfo,
                                      samplePeriod,
                                      by="Code")
        samplePeriod = dplyr::select(samplePeriod, Code, sp)

        tree("Get number of missing data for start and end",
             3, inEnd=2, verbose=verbose)
        
        sampleInfo$dNAstart =
            pmax(0, as.numeric(sampleInfo$minDate -
                               sampleInfo$minSampleStart))
        sampleInfo$dNAend =
            pmax(0, as.numeric(sampleInfo$maxSampleEnd -
                               sampleInfo$maxDate))


        if (timeStep == "year-season") {
            if (is.null(keep)) {
                minRef = "minSampleStart"
                maxRef = "maxSampleStart"
            } else {
                if (any(names_keepSave[idValue_keepSave] %in% keep) | keep == "all") {
                    minRef = "minDate"
                    maxRef = "maxDate"
                } else {
                    minRef = "minSampleStart"
                    maxRef = "maxSampleStart"
                }
            }
            
            sampleStartInfo =
                sampleInfo[c("Code", minRef, "dNAstart")]
            names(sampleStartInfo) = c("Code", "Date", "dNA")
            sampleEndInfo =
                sampleInfo[c("Code", maxRef, "dNAend")]
            names(sampleEndInfo) = c("Code", "Date", "dNA")

        } else if (timeStep == "season") {
            sampleStartInfo =
                sampleInfo[c("Code", "minSampleStart", "dNAstart")]
            names(sampleStartInfo) = c("Code", "Date", "dNA")
            sampleEndInfo =
                sampleInfo[c("Code", "maxSampleStart", "dNAend")]
            names(sampleEndInfo) = c("Code", "Date", "dNA")
        }
        
        sampleInfoCompress = dplyr::bind_rows(sampleStartInfo,
                                              sampleEndInfo)

        sampleInfoCompress$Year =
            lubridate::year(sampleInfoCompress$Date)
        sampleInfoCompress$Month =
            lubridate::month(sampleInfoCompress$Date)

        tree("Create each group",
             3, end=TRUE, inEnd=2, verbose=verbose)

        if (timeStep == "season") {
            data$Month = lubridate::month(data$Date)
            # data$group = get_Season[data$Month]

            data$SeasonDate =
                lubridate::add_with_rollback(data$Date,
                                             - months(subSeasons[data$Month]))

            data$group = format(data$SeasonDate, "%m")
            
            # data$YearSeason =
                # paste0(lubridate::year(data$SeasonDate), "-",
                       # get_Season[lubridate::month(data$SeasonDate)])


            ###
            # seasonInfo =
                # dplyr::summarise(dplyr::group_by(data,
                                                 # Code,
                                                 # YearSeason),
                                 # Date=Date[!duplicated(YearSeason)],
                                 # .groups="drop")
            # seasonInfo =
            #     dplyr::summarise(
            #                dplyr::group_by(seasonInfo,
            #                                Code,
            #                                Date=get_Season[Month]),
            #                nYear=dplyr::n(),
            #                .groups="drop")
            ###
            

            sampleInfoCompress$Season =
                get_Season[lubridate::month(sampleInfoCompress$Date)]
            sampleInfoCompress =
                dplyr::summarise(dplyr::group_by(sampleInfoCompress,
                                                 Code, Season),
                                 dNA=sum(dNA),
                                 # addYear=dplyr::n() - 1,
                                 .groups="drop")

            
        } else if (timeStep == "year-season") {
            data$SeasonDate =
                lubridate::add_with_rollback(
                               data$Date,
                               - months(subSeasons[lubridate::month(data$Date)]))
            # data$YearSeason =
            #     paste0(lubridate::year(data$SeasonDate),
            #            "-",
            #            get_Season[lubridate::month(data$SeasonDate)])

            data$group = format(data$SeasonDate, "%Y-%m")

            sampleInfoCompress$YearSeason =
                paste0(lubridate::year(sampleInfoCompress$Date),
                       "-",
                       get_Season[lubridate::month(sampleInfoCompress$Date)])
        }


        # stop()
        
    }


### Pre Extraction ___________________________________________________
    tree("Grouping data", 1, verbose=verbose)

    if (timeStep == "none") {
        colGroup = "Code"
        data = dplyr::group_by(data, Code)
        
    } else {
        colGroup = c("Code", "group")
        data = dplyr::group_by(data, Code, group)
    }

    if (!is.null(keep)) {
        if (timeStep == "none") {
            keepDate = list(Date="Date")
        } else {
            keepDate = NULL
        }

        colGroup = c(colGroup, "id")
        
    } else {
        keepDate = NULL
        if (timeStep == "none") {
            colGroup = c("Code", "id")
        }
    }


    if (!is.null(keep)) {
        if (!any(names_keepSave[idValue_keepSave] %in% keep) & keep != "all") {
            keep_tmp = NULL
        } else {
            keep_tmp = keep
        }
    } else {
        keep_tmp = keep
    }

    tree("Application of the function",
         1, verbose=verbose)

    data = purrr::reduce(.x=lapply(1:nfunct,
                                   apply_extraction,
                                   data=data,
                                   colArgs=colArgs,
                                   otherArgs=otherArgs,
                                   funct=funct,
                                   keepDate=keepDate,
                                   timeStep=timeStep,
                                   keep=keep_tmp,
                                   colGroup=colGroup),
                         .f=dplyr::full_join, by=colGroup)

    tree("Cleaning extracted tibble", 1, verbose=verbose)
    tree("Manage possible infinite values", 2, verbose=verbose)

    data = dplyr::select(data, -dplyr::starts_with("id"))
    if (!is.null(keep) & !(timeStep %in% c("month",
                                           "season",
                                           "yearday"))) {
        data = dplyr::select(data, -"isNA")
    }

    nValue = nfunct
    
    infinite2NA = function (X) {
        if (tibble::is_tibble(X)) {
            X = dplyr::mutate(X, dplyr::across(.fns=infinite2NA))
        } else {
            X[is.infinite(X)] = NA
        }
        return (X)
    }

    data = dplyr::mutate(data,
                         dplyr::across(.cols=
                                           dplyr::starts_with(
                                                      paste0("ValueEX",
                                                             1:nValue)),
                                       .fns=infinite2NA),
                         .keep="all")
    data = dplyr::ungroup(data)


### Reformat data ____________________________________________________
    tree("Recreate a date vector and add value for NApct computing",
         2, end=TRUE, verbose=verbose)

    if (timeStep == "year") {
        if (is.null(keep) | !is.null(keep) & any(isDate)) {
            data = dplyr::full_join(data,
                                    dplyr::select(sampleInfo,
                                                  Code,
                                                  spStart),
                                    by="Code")
            
            if (is.null(keep)) {
                data = dplyr::mutate(data,
                                     Date=as.Date(
                                         paste0(
                                             group,
                                             "-",
                                             spStart)))
            } else {
                data = dplyr::mutate(data,
                                     Date_group=as.Date(
                                         paste0(group,
                                                "-",
                                                spStart)))
            }
            data = dplyr::select(data, -spStart)
        }
        
        data = dplyr::select(data, -group)
        data = dplyr::relocate(data, Date, .after=Code)

        data = dplyr::full_join(data,
                                dplyr::select(sampleInfoCompress,
                                              Code,
                                              Date,
                                              dNA),
                                by=c("Code", "Date"))
        data = tidyr::replace_na(data, list(dNA=0))
        data$nDay = 365.25
        
        
    } else if (timeStep == "yearday") {
        data = dplyr::rename(data, Yearday=group)
        data = dplyr::mutate(data,
                             Date=
                                 as.Date("1972-01-01") +
                                 Yearday - 1)
        data = dplyr::relocate(data, Date, .after=Code)
        
        data = dplyr::full_join(data,
                                dplyr::select(sampleInfoCompress,
                                              Code,
                                              Yearday,
                                              dNA),
                                by=c("Code", "Yearday"))
        data = tidyr::replace_na(data, list(dNA=0))

        data = dplyr::full_join(data,
                                dplyr::select(sampleInfo,
                                              Code,
                                              nYear),
                                by="Code")
        data = dplyr::rename(data, nDay=nYear)

        
    } else if (timeStep == "year-month") {
        if (is.null(keep) | !is.null(keep) & any(isDate)) {
            data = dplyr::full_join(data,
                                    dplyr::select(sampleInfo,
                                                  Code,
                                                  spStart),
                                    by="Code")
            
            if (is.null(keep)) {
                data = dplyr::mutate(data,
                                     Date=as.Date(
                                         paste0(
                                             group,
                                             "-",
                                             spStart)))
            } else {
                data = dplyr::mutate(data,
                                     Date_group=as.Date(
                                         paste0(group,
                                                "-",
                                                spStart)))
            }
            data = dplyr::select(data, -spStart)
        }
        
        data = dplyr::select(data, -group)
        data = dplyr::relocate(data, Date, .after=Code)

        data = dplyr::full_join(data,
                                dplyr::select(sampleInfoCompress,
                                              Code,
                                              Date,
                                              dNA),
                                by=c("Code", "Date"))
        data = tidyr::replace_na(data, list(dNA=0))
        data$nDay = 30.4375

        
    } else if (timeStep == "month") {        
        data = dplyr::rename(data, Month=group)
        data$Month = as.numeric(data$Month)
        data = dplyr::full_join(data,
                                dplyr::select(sampleInfo,
                                              Code,
                                              spStart),
                                by="Code")
        data = dplyr::mutate(data,
                             Date=
                                 as.Date(paste0("1972-",
                                                Month,
                                                "-",
                                                spStart)))
        data = dplyr::select(data, -spStart)
        data = dplyr::relocate(data, Date, .after=Code)
        
        data = dplyr::full_join(data,
                                dplyr::select(sampleInfoCompress,
                                              Code,
                                              Month,
                                              dNA),
                                by=c("Code", "Month"))
        data = tidyr::replace_na(data, list(dNA=0))

        data = dplyr::full_join(data,
                                dplyr::select(sampleInfo,
                                              Code,
                                              nYear),
                                by="Code")
        data$nDay = 30.4375 * data$nYear
        data = dplyr::select(data, -nYear)

        
    } else if (timeStep == "year-season") {
        if (is.null(keep) | !is.null(keep) & any(isDate)) {
            data = dplyr::full_join(data,
                                    dplyr::select(sampleInfo,
                                                  Code,
                                                  spStart),
                                    by="Code")
            
            if (is.null(keep)) {
                data = dplyr::mutate(data,
                                     Date=as.Date(
                                         paste0(
                                             group,
                                             "-",
                                             spStart)))
            } else {
                data = dplyr::mutate(data,
                                     Date_group=as.Date(
                                         paste0(group,
                                                "-",
                                                spStart)))
            }
            data = dplyr::select(data, -spStart)
        }
        
        data = dplyr::select(data, -group)
        data = dplyr::relocate(data, Date, .after=Code)

        data = dplyr::full_join(data,
                                dplyr::select(sampleInfoCompress,
                                              Code,
                                              Date,
                                              dNA),
                                by=c("Code", "Date"))
        data = tidyr::replace_na(data, list(dNA=0))

        data$YearSeason =
            paste0(lubridate::year(data$Date),
                   "-",
                   get_Season[lubridate::month(data$Date)])
        data$nDay = nchar(gsub("^.*-", "", data$YearSeason)) * 30.4375

        
    } else if (timeStep == "season") {
        data$group = as.numeric(data$group)
        data$Season = get_Season[data$group]
        
        data = dplyr::full_join(data,
                                dplyr::select(sampleInfo,
                                              Code,
                                              spStart),
                                by="Code")
        data = dplyr::mutate(data,
                             Date=
                                 as.Date(paste0("1972-",
                                                group,
                                                "-",
                                                spStart)))
        data = dplyr::select(data, -spStart)
        data = dplyr::select(data, -group)
        data = dplyr::relocate(data, Date, .after=Code)
        data = dplyr::relocate(data, Season, .after=Date)
        
        data = dplyr::full_join(data,
                                dplyr::select(sampleInfoCompress,
                                              Code,
                                              Season,
                                              dNA),
                                by=c("Code", "Season"))
        data = tidyr::replace_na(data, list(dNA=0))

        data = dplyr::full_join(data,
                                dplyr::select(sampleInfo,
                                              Code,
                                              nYear),
                                by="Code")

        data$nDay = nchar(data$Season) * 30.4375 * data$nYear
        data = dplyr::select(data, -nYear)
    } 

### isDate ___________________________________________________________
    if (any(isDate)) {
        tree('Converting index to date', 1, verbose=verbose)
        data = convert_dateEX(data, sampleInfo,
                              isDate, nValue=nValue,
                              keep=keep,
                              verbose=verbose)
    }

    
### Compute NApct ____________________________________________________
    tree("NA management", 1, verbose=verbose)
    tree("Compute NA percentage", 2, verbose=verbose)
    
    if (timeStep == "none") {
        compute_NApct = function (nNA, n) {
            NApct = round(nNA/n * 100, 1)
            return (NApct)
        }
        data = dplyr::mutate(data,
                             dplyr::across(.cols=
                                               dplyr::starts_with(
                                                          paste0("nNA",
                                                                 1:nValue)),
                                           .fns=~compute_NApct(.x, n=n),
                                           .names=paste0("NApct{1:",
                                                         nValue, "}")),
                             .keep="all")
        data = dplyr::ungroup(data)

        data = dplyr::select(data, -c(paste0("nNA",
                                             1:nValue),
                                      n))

    } else {
        compute_NApct = function (nNA, dNA, nDay) {
            NApct = round((nNA + dNA)/nDay * 100, 1)
            return (NApct)
        }
        data = dplyr::mutate(data,
                             dplyr::across(.cols=
                                               dplyr::starts_with(
                                                          paste0("nNA",
                                                                 1:nValue)),
                                           .fns=~compute_NApct(.x,
                                                               dNA=dNA,
                                                               nDay=nDay),
                                           .names=paste0("NApct{1:",
                                                         nValue, "}")),
                             .keep="all")

        data = dplyr::select(data, -c(paste0("nNA",
                                             1:nValue),
                                      n,
                                      dNA,
                                      nDay))
    }
    
    if (!is.null(NApct_lim)) {
        tree(paste0('Removing data if NA percentage is strictly above ',
                    NApct_lim, " %"), 2, verbose=verbose)
        data = NA_filter(data, timeStep=timeStep,
                         nValue=nValue,
                         NApct_lim=NApct_lim,
                         mod=NULL, verbose=verbose)
    }

    tree("Cleaning NA percentage info", 2, end=TRUE, verbose=verbose)
    if (!rmNApct & is.null(keep) & !compress) {
        if (nfunct == 1) {
            data = dplyr::rename(data, NApct=NApct1)
            
        } else {
            for (i in 1:nfunct) {
                data=
                    dplyr::rename(data,
                                  !!paste0("NApct",
                                           "_",
                                           nameEX[i]) :=
                                      !!paste0("NApct", i))
            }
        }
    }
    if (rmNApct | compress) {
        data = dplyr::select(data, -c(paste0("NApct",
                                             1:nValue)))
    }


### Reshape __________________________________________________________
    tree("Last cleaning and formating for output", 1, end=TRUE,
         verbose=verbose)
    tree("Rename column", 2, inEnd=1, verbose=verbose)

    idCode = which(names(data) == "Code")
    if (!(timeStep == "none" & is.null(keep))) {
        idDate = which(names(data) == "Date")
    }
    
    idValue = which(grepl("ValueEX[[:digit:]]", names(data)))

    if (isDateColArgs) {
        names_save = names_save[-length(names_save)]
        idValue_save = idValue_save[-length(idValue_save)]
    }

    if (timeStep == "none" & is.null(keep)) {

        data = dplyr::relocate(data, )
        
        names(data)[c(idCode, idValue)] =
            names_save[c(idCode_save, idValue_save)]
    } else {
        names(data)[c(idCode, idDate, idValue)] =
            names_save[c(idCode_save, idDate_save, idValue_save)] 
    }

    if (!is.null(keep)) {
        test = grepl("Value[[:digit:]]", names(data))
        if (any(test)) {
            
            idValue_keep = which(test)
            idIn =
                which(names_keepSave[idValue_keepSave] %in%
                      names(data))

            if (length(idIn) > 0) {
                idRm = idValue_keep[idIn]
                data = data[-idRm] 
                idValue_keep = idValue_keep[-idIn]
                idValue_keepSave = idValue_keepSave[-idIn]
            }
            names(data)[idValue_keep] =
                names_keepSave[idValue_keepSave]

            
        }
    }

    dateName = names_save[idDate_save]
    valueName = names_save[idValue_save]
    if (timeStep == "season") {
        Ref = Seasons
        shiftRef = startSeasonsMonth
        
    } else if (timeStep == "year-season") {
        Ref = get_Season
        shiftRef = startSeasonsMonth
        
    } else if (timeStep %in% c("year-month", "month")) {
        Ref = format(seq.Date(as.Date("1970-01-01"),
                              as.Date("1970-12-01"),
                              "month"), "%b")
        Ref = gsub("û", "u", Ref)
        Ref = gsub("é", "e", Ref)
        Ref = gsub("[.]", "", Ref)
        shiftRef = 1:12
    }

    if (compress) {
        tree("Compress line to column", 2, inEnd=1, verbose=verbose)
        if (timeStep %in% c("year-month", "year-season")) {
            data = dplyr::mutate(
                              data,
                              Ref=
                                  Ref[lubridate::month(get(dateName))],
                              !!dateName:=
                                  lubridate::year(get(dateName)))

            if (timeStep == "year-season") {
                data = dplyr::select(data, -YearSeason)
            }
            
        } else if (timeStep == "season") {
            data = dplyr::rename(data, Ref=Season)
            data = dplyr::select(data,
                                 -dplyr::all_of(dateName))
            
        } else if (timeStep == "month") {
            data$Ref = Ref[data$Month]
            data = dplyr::select(data,
                                 -dplyr::all_of(c(dateName,
                                                  "Month")))
        }
        data =
            tidyr::pivot_wider(
                       data,
                       names_from=Ref,
                       values_from=valueName,
                       names_glue="{.value}_{Ref}")
    }

    
    if (!is.null(suffix)) {
        valueName = c()
        for (i in 1:length(suffix)) {
            ok = grepl(suffix[i], names(data), fixed=TRUE)
            valueName_no_suffix = gsub(suffix[i], "",
                                       names(data)[ok],
                                       fixed=TRUE)
            valueName = c(valueName, valueName_no_suffix)
            valueName_suffix = paste0(valueName_no_suffix,
                                      suffix[i])
            names(data)[ok] = valueName_suffix
        }
        valueName = valueName[!duplicated(valueName)]

    } else {
        valueName = nameEX
    }
    

    if (compress) {
        if (timeStep %in% c("year-month", "year-season")) {
            data[[dateName]] = as.Date(paste0(data[[dateName]],
                                              "-01-01"))
        }
    }


    
### Keep _____________________________________________________________
    if (!is.null(keep)) {
        tree(paste0("Keeping only the needed data : " ,
                    paste0(keep, collapse=", ")), 2, inEnd=1,
             verbose=verbose)
        if (!any(keep == "all")) {
            data = dplyr::select(data, dplyr::all_of(keep))
        }
    }

    data = tidyr::unnest(data,
                         dplyr::everything(),
                         names_sep="_")
    
    if (length(ID_colnames) > 1) {
        data = tidyr::separate(data, col="ID",
                               into=ID_colnames, sep="_")
    }


    if (expand) {
        tree("Expand the tibble in a list of tibble for each extracted variable",
             2, inEnd=1, verbose=verbose)
        is.character_or_date = function (x) {
            is.character(x) | lubridate::is.Date(x)
        }
        
        if (!is.null(suffix)) {
            valueName_suffix = lapply(valueName, paste0, suffix)
        } else {
            valueName_suffix = valueName
        }

        valueName_select = lapply(
            valueName_suffix,
            append,
            x=names(data)[sapply(data,
                                 is.character_or_date)])

        data = lapply(dplyr::all_of(valueName_select), dplyr::select,
                      .data=data)
        names(data) = valueName

        if (timeStep %in% c("year-month", "year-season")) {
            for (i in 1:length(valueName)) {
                dateName =
                    names(data[[i]])[sapply(data[[i]],
                                            lubridate::is.Date)]
                data[[i]] =
                    dplyr::mutate(
                               data[[i]],
                               !!dateName:=
                                   lubridate::add_with_rollback(
                                                  get(dateName),
                                                  months(shiftRef[i]
                                                         -1)))
            }
        }
    }
    
    tree("Return data", 2, end=TRUE, inEnd=1, verbose=verbose)
    if (verbose) {
        print(data)
    }
    
    return (data)
}



apply_extraction = function (i, data, colArgs, otherArgs,
                             funct, keepDate, timeStep,
                             keep, colGroup) {
    
    colArg = colArgs[[i]]
    otherArg = otherArgs[[i]]
    f = funct[[i]]

    data$isNA =
        is.na(rowSums(
            dplyr::mutate_all(data[unlist(colArg)],
                              as.numeric)))

    if (!is.null(keep) & !(timeStep %in% c("month",
                                           "season",
                                           "yearday"))) {

        if (i == 1) {
            data = dplyr::mutate(
                              data,
                              !!!data_syms(keepDate),
                              !!paste0("ValueEX", i) :=
                                  f(!!!data_syms(colArg),
                                    !!!otherArg),
                              !!paste0("nNA", i) :=
                                  sum(isNA),
                              n=dplyr::n(),
                              id=1:max(c(length(get(paste0("ValueEX",
                                                           i))), 1)))
        } else {
            data = dplyr::mutate(
                              data,
                              !!paste0("ValueEX", i) :=
                                  f(!!!data_syms(colArg),
                                    !!!otherArg),
                              !!paste0("nNA", i) :=
                                  sum(isNA),
                              id=1:max(c(length(get(paste0("ValueEX",
                                                           i))), 1)),
                              .keep="none"
                          )
        }

    } else {
        if (i == 1) {
            data = dplyr::summarise(
                              data,
                              !!!data_syms(keepDate),
                              !!paste0("ValueEX", i) :=
                                  f(!!!data_syms(colArg),
                                    !!!otherArg),
                              !!paste0("nNA", i) :=
                                  sum(isNA),
                              n=dplyr::n(),
                              id=1:max(c(length(get(paste0("ValueEX", i))), 1)),
                              .groups='drop')
        } else {
            data = dplyr::summarise(
                              data,
                              !!!data_syms(keepDate),
                              !!paste0("ValueEX", i) :=
                                  f(!!!data_syms(colArg),
                                    !!!otherArg),
                              !!paste0("nNA", i) :=
                                  sum(isNA),
                              id=1:max(c(length(get(paste0("ValueEX", i))), 1)),
                              .groups='drop')
        }
    }

    data = dplyr::ungroup(data)
        
    return (data)
}






fix_samplePeriod = function (samplePeriod, data_code=NULL, args=NA,
                             suffix=NULL,
                             refDate="1972", sampleFormat="%m-%d",
                             verbose=FALSE) {

    if (is.function(samplePeriod[[1]]) & sampleFormat == "%m-%d") {
        data_code = process_extraction(data=data_code,
                                       funct=list("XM"=mean),
                                       funct_args=args,
                                       timeStep="month",
                                       rmNApct=TRUE,
                                       verbose=FALSE)
        
        data_code_month = data_code
        names(data_code)[names(data_code) == "XM"] = args[[1]][1]
        data_code = process_extraction(data=data_code,
                                       funct=list("fXM"=
                                                      samplePeriod[[1]]),
                                       funct_args=args,
                                       timeStep="none",
                                       rmNApct=TRUE,
                                       verbose=FALSE)

        samplePeriod =
            data_code_month$Month[data_code_month$XM == data_code$fXM]
        if (any(is.na(samplePeriod))) {
            samplePeriod = "09"
        }
        samplePeriod = paste0(samplePeriod, "-01")
    }

    samplePeriod = unlist(samplePeriod)
    
    if (length(samplePeriod) == 1 | any(is.na(samplePeriod))) {
        if (length(samplePeriod) == 1) {
            
            tree("Only start of the sample period was given", 3,
                 inEnd=2,
                 verbose=verbose)
            samplePeriod =
                c(samplePeriod,
                  format(as.Date(paste0(refDate,
                                        '-',
                                        samplePeriod)) - 1,
                         sampleFormat))
            
        } else if (any(is.na(samplePeriod))) {
            idNA = which(is.na(samplePeriod))
            if (idNA == 1) {
                tree("Only end of the sample period was given", 3,
                     inEnd=2,
                     verbose=verbose)
                samplePeriod[1] =
                    format(as.Date(paste0(refDate,
                                          '-',
                                          samplePeriod[2])) + 1,
                           sampleFormat)
                
            } else if (idNA == 2) {
                tree("Only start of the sample period was given", 3,
                     inEnd=2,
                     verbose=verbose)
                samplePeriod[2] =
                    format(as.Date(paste0(refDate,
                                          '-',
                                          samplePeriod[1])) - 1,
                           sampleFormat)
            }
        }
    }

    return (samplePeriod)
}



fix_samplePeriod_FUCKING29FEB = function (samplePeriod,
                                          sampleFormat) {
    samplePeriod = unlist(samplePeriod)
    if (sampleFormat == "%m-%d") {
        samplePeriod[samplePeriod == "02-29"] = "02-28"
    }
    return (samplePeriod)
}

add = function (x, y) {x+y}

reduce_convert_data_hide = function (data, i, isDate) {
    if (isDate[i]) {
        Value = paste0("ValueEX", i)
        data = dplyr::mutate(dplyr::group_by(data, Code),
                             dplyr::across(.cols=dplyr::starts_with(
                                                            Value),
                                           .fns=~add(.x, y=Shift)),
                             dplyr::across(.cols=dplyr::starts_with(
                                                            Value),
                                           .fns=~convert_data_hide(
                                                    .x,
                                                    Date=Date)),
                             .keep="all")
    }
    data = dplyr::ungroup(data)
    return (data)
}

convert_data_hide = function (Value, Date) {
    Month = Value / (365.25/12)        
    MonthNoNA = Month[!is.na(Month)]    
    fact = 2*pi/12
    monthMean_raw = CircStats::circ.mean(fact * MonthNoNA) / fact
    monthMean = (monthMean_raw + 12) %% 12

    upLim = round(monthMean + 6, 2)
    lowLim = round(monthMean - 6, 2)

    above = Month > upLim
    above[is.na(above)] = FALSE
    below = Month < lowLim
    below[is.na(below)] = FALSE

    Value = Value - 1
    
    nDay = rep(365, length(Date))
    nDay[check_leapYear(lubridate::year(Date))] = 366

    # print(Value)
    # print(nDay)
    # print(above)
    # print(below)
    
    Value[above] = Value[above] - nDay[above]
    Value[below] = Value[below] + nDay[below]

    # print(Value)
    
    # Value = round((Value + nDay/2) %% nDay - nDay/2)  
    
    # print(Value)
    # print("")
    
    return (Value)
}

#' @title convert_dateEX
#' @description Convert the index to date format for the extracted variables specified in the isDate argument. The date format is based on the date column of the input tibble and the function "convert_data_hide" is used for the conversion.
#' @param data A tibble containing a column named "Date" which corresponds to the index of each sample and one or several columns named "ValueEXx" (x being a number) that correspond to the extracted variables.
#' @param isDate A logical vector indicating whether the columns named "ValueEXx" should be converted to date format or not
#' @param nValue Number of columns named "ValueEXx"
#' @param isColArgs A logical vector indicating whether the columns named "ValueEXx" corresponds to column arguments of function used in the extraction process
#' @param verbose A logical, if TRUE output to the console will be provided
#' @return A tibble containing a column named "Date" which corresponds to the date of each sample and one or several columns named "ValueEXx" (x being a number) that correspond to the extracted variables.
#' @note documentation generated by chatGPT
#' @export
convert_dateEX = function(data, sampleInfo, isDate, nValue,
                          keep=keep,
                          verbose=TRUE) {

    # print(data, n=Inf)
    
    data = dplyr::full_join(data,
                            dplyr::select(sampleInfo,
                                          Code, minDate),
                            by="Code")
    if (is.null(keep)) {
        data$reelSampleStart = pmax(data$Date,
                                    data$minDate)
    } else {
        data$reelSampleStart = pmax(data$Date_group,
                                    data$minDate)
        data = dplyr::select(data, -Date_group)
    }
    
    data$Shift = lubridate::yday(data$reelSampleStart) - 1
    
    nfunct = length(isDate)
    data = purrr::reduce(.x=1:nfunct,
                         .f=reduce_convert_data_hide,
                         isDate=isDate,
                         .init=data)
    
    data = dplyr::select(data, -c(minDate,
                                  reelSampleStart,
                                  Shift))

    # print(data, n=Inf)
    
    return (data)
}


## 3. NA FILTER AFTER EXTRACTION _____________________________________
#' @title NA filter
#' @description Filters the rows with a NA percentage above the threshold given in parameter.
#' @param data Data after extraction that should be filtered.
#' @param timeStep Character string indicating the time step of the extraction, could be 'none' if the time step doesn't matter, 'year' or 'year-month'.
#' @param nValue Number of values extracted.
#' @param NApct_lim Percentage limit of NA above which rows will be removed. Default is 1.
#' @param mod A list of modification to store comments of filtering applied.
#' @param verbose Logical, when true prints print indicating the removal of data due to high NA percentage
#' @return data filtered by the percentage of NA allowed in input.
#' @importFrom lubridate is.Date
#' @note documentation generated by chatGPT
#' @export
NA_filter = function (data, timeStep, nValue, NApct_lim=1,
                      mod=NULL, verbose=TRUE) {

    NApct2filter = function (X, NApct_lim) {
        filter = X > NApct_lim
        filter[is.na(filter)] = FALSE
        return (filter)
    }
    
    data = dplyr::mutate(data,
                         dplyr::across(.cols=
                                           dplyr::starts_with(
                                                      paste0("NApct",
                                                             1:nValue)),
                                       # .fns=NApct2filter,
                                       # NApct_lim=NApct_lim,
                                       .fns=~NApct2filter(.x,
                                                          NApct_lim=
                                                              NApct_lim),
                                       .names=paste0("filter{1:",
                                                     nValue, "}")),
                         .keep="all")
    data = dplyr::ungroup(data)
    
    filter2NA = function (X, filter) {
        X[filter] = NA
        return (X)
    }
    
    data[paste0("ValueEX", 1:nValue)] =
        Map(filter2NA,
            X=data[paste0("ValueEX", 1:nValue)],
            filter=data[paste0("filter", 1:nValue)])

    # codeFilter = data$Code[data$filter]
    # codeFilter = codeFilter[!duplicated(codeFilter)]
    # if (timeStep == "none") {
    #     dateFilter = ""
    # } else {
    #     if (lubridate::is.Date(data$Date[1])) {
    #         dateFilter = format(data$Date[data$filter], "%Y")
    #     } else {
    #         dateFilter = data$Date[data$filter]
    #     }
    # }
    # Nmod = length(codeFilter)

    # if (!is.null(mod) & !identical(codeFilter, character(0))) {
    #     for (i in 1:Nmod) {
    #         mod =
    #             add_mod(mod, codeFilter[i],
    #                     type='Filtering of NA percentage after EXtraction',
    #                     fun_name='NA assignment',
    #                     comment=paste0('Removal of year ', dateFilter[i]))
    #     }
    # }

    data = dplyr::select(data, -paste0("filter", 1:nValue))
    
    if (!is.null(mod)) {
        res = list(data=data, mod=mod)
        return (res)
    } else {
        return (data)
    }
}



## 2. MANAGES MISSING DATA ___________________________________________
### 2.1. Long missing period over several years ______________________
missing_year_hide = function (Value, Date, NAyear_lim) {

    DateNA = Date[is.na(Value)]
    dDateNA = diff(DateNA)
    if (length(dDateNA) == 0) {
        return (Value)
    }
    dDateNA = c(10, dDateNA)

    idJump = which(dDateNA != 1)
    NJump = length(idJump)

    if (NJump > 0) {
        
        for (i in 1:NJump) {
            idStartNA = idJump[i]
            
            if (i < NJump) {
                idEndNA = idJump[i+1] - 1
            } else {
                idEndNA = length(DateNA)
            }

            StartNA = DateNA[idStartNA]
            EndNA = DateNA[idEndNA]
            
            duration = as.numeric((EndNA - StartNA)/365.25)
            
            if (duration >= NAyear_lim) {
                
                Start = min(Date, na.rm=TRUE)
                End = max(Date, na.rm=TRUE)
                
                Before = StartNA - Start
                After = End - EndNA
                if (Before < After) {
                    Value[Date <= StartNA] = NA
                    start = Start
                    end = StartNA
                } else {
                    Value[Date >= EndNA] = NA
                    start = EndNA
                    end = End
                }
            }
        }
    }
    return (Value)
}

#' @title Missing year
#' @description This function will check for any continuous missing period longer than a specified threshold of years in each time series and assign them as missing values.
#' @param data A data.frame containing at least a date column, a numeric column and a column indicating a name of time series, in order to identify them.
#' @param nValue number of numeric column
#' @param NAyear_lim threshold of year duration for missing period.
#' @param mod An object of class modification, it will store all modification made to the data
#' @param verbose logical for print prints
#' @return return the dataframe with identified missing period as NA
#' @note documentation generated by chatGPT
#' @export
#' @note documentation generated by chatGPT
#' @export
missing_year = function (data, nValue, NAyear_lim=10,
                         mod=NULL, verbose=TRUE) {

    tree("Missing year", 1, verbose=verbose)
    tree(paste0('Checking missing continuous periods longer than ',
                NAyear_lim, ' years'), 2, is.null(mod), verbose=verbose)

    dataMOD = dplyr::mutate(
                         dplyr::group_by(data, Code),
                         dplyr::across(.cols=paste0("Value",
                                              1:nValue),
                                       # .fns=missing_year_hide,
                                       .fns=~missing_year_hide(
                                                .x,
                                                Date=Date,
                                                NAyear_lim=NAyear_lim)
                                       # Date=Date,
                                       # NAyear_lim=NAyear_lim
                                       ),
                         .keep="all")
    dataMOD = dplyr::ungroup(dataMOD)

    # if (!is.null(mod)) {
    #     tree('Wrinting modifications', 2, !is.null(mod),
    #          verbose=verbose)        
    #     isCorr = is.na(df_Value$Value) != is.na(data$Value)
    #     CodeCorr = df_Value$Code[isCorr]
    #     CodeCorr = CodeCorr[!duplicated(CodeCorr)]

    #     for (code in CodeCorr) {

    #         df_Value_code = df_Value[df_Value$Code == code,]
    #         data_code = data[data$Code == code,]

    #         isCorr_code = is.na(df_Value_code$Value) != is.na(data_code$Value)

    #         DateCorr_code = df_Value_code$Date[isCorr_code]

    #         start = min(DateCorr_code)
    #         end = max(DateCorr_code)
    
    #         mod =
    #             add_mod(mod, code,
    #                     type='Missing data management',
    #                     fun_name='NA assignment',
    #                     comment=paste('From ', start,
    #                                   ' of measurements',
    #                                   ' to ', end, sep=''))
    #     }
    # }

    # replace2NA = function (X) {
    #     X = replace(X, list=which(is.na(X)), value=NA)
    #     return (X)
    # }
    # data = dplyr::mutate(data,
    #                      dplyr::across(unlist(colArgs),
    #                                    replace2NA))


    data[paste0("Value", 1:nValue)] = dataMOD[paste0("Value", 1:nValue)]

    if (!is.null(mod)) {
        res = list(data=data, mod=mod)
        return (res)
    } else {
        return (data)
    }
}
