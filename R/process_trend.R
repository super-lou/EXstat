# Copyright 2021-2024 Louis Héraut (louis.heraut@inrae.fr)*1
#           2023      Éric Sauquet (eric.sauquet@inrae.fr)*1
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
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
# EXstat R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EXstat R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title process_trend
#' @description Process a trend analyze on time series data. The Mann-Kendall statistical test is applied to detect trends and some additional statistics are computed like the Sen-Theil slope estimator.
#'
#' @param dataEX Input data format is a [tibble][tibble::tibble()] from the tibble package. It needs to have :
#' * Only one column of [Date][base::Date] that are regularly spaced and unique for each time serie.
#' * If there is more than one time serie, at least one column needs to be of [character][base::character] for names of time series in order to identify them. If more than one column of identifier is given, they will all be used in order to identify a unique time serie.
#' * At least one column of [numeric][base::numeric] (or [logical][base::logical]) on which the process of variable extraction will be perform. More numerical column can be leave but if they are useless, they will be suppressed.
#' The [tibble][tibble::tibble()] output of the [process_extraction()] function is the kind of wanted input data.
#' @param MK_level [numeric][base::numeric] Mann-Kendall test significance level between `0` and `1`. Default is `0.1`.
#' @param time_dependency_option [character][base::character] for handling temporal dependence for the Mann-Kendall test. Possible values are :
#' * `"INDE"`, assume independence (i.e. the standard MK test)
#' * `"AR1"`, assumes AR1 short-term dependence structure (i.e. Hamed and Rao's version of the MK test)
#' * `"LTP"`, assume long-term persistence (i.e. Hamed's version of the MK test)
#' @param suffix A [character][base::character] [vector][base::c()] representing suffixes to be appended to the column names of the extracted variables. See [process_extraction()] for more info. Default `NULL`.
#' @param suffix_delimiter [character][base::character] specifies the delimiter to use between the variable name and the suffix if not `NULL`. The default is `"_"`.
#' @param to_normalise A named [logical][base::logical] [vector][base::c()] indicating whether each variable's trend should be normalised. `TRUE` performs normalisation, while `FALSE` does nothing. This vector must be of length one or have the same length as the number of [numeric][base::numeric] (or [logical][base::logical]) variables in `dataEX`, with names specifying which value corresponds to which variable. Default 'FALSE'.
#' @param metaEX One of the outputs of the [CARD_extraction] function that contains metadata for the normalisation process. Default is `NULL`. If supplied, this normalisation information will be used instead of the settings provided in the `to_normalise` variable.
#' @param extreme_take_not_signif_into_account [logical][base::logical] Whether to consider non-significant trends in the computation of extreme trends. Default is `TRUE`.
#' @param extreme_take_only_series [character][base::character] A [vector][base::c()] of the names of time series to be used for computing extreme trends. Default is `NULL`, which includes all available series.
#' @param extreme_by_suffix [logical][base::logical] If `TRUE`, extreme trends will be computed across separate sets of trend values of the same variable and the same suffix. If `FALSE`, all extreme trends of a variable will be used without considering suffixes. Default is `TRUE`.
#' @param period_trend A [vector][base::c()] of two [dates][base::Date] (or two unambiguous [character][base::character] that can be coerced to [dates][base::Date]) to restrict the period of analysis. As an example, it can be `c("1950-01-01", "2020-12-31")` to select data from the 1st January of 1950 to the end of December of 2020. The default option is `period_trend=NULL`, which considers all available data for each time serie.
#' @param period_change *in developpement* A [list][base::list()] of two [vectors][base::c()] of two [dates][base::Date] (or two unambiguous [character][base::character] that can be coerced to [dates][base::Date]) to restrict two period for the change analysis. As an example, it can be `list(c("1950-01-01", "1999-12-31"), c("2000-01-01", "2020-12-31"))`. The default option is `period_change=NULL`, which does not do any analysis.
#' @param extreme_prob [numeric][base::numeric] The probability for identifying extreme trends using quantiles. Default is `0.01`, so the computed extremes will be based on the [quantile][stats::quantile()] at `0.01` and `0.99`.
#' @param show_advance_stat [logical][base::logical] Whether to display advanced statistical details. Default is `FALSE`.
#' @param dev [logical][base::logical] If `TRUE`, development mode is enabled. Default is `FALSE`.
#' @param verbose [logical][base::logical] Whether to print intermediate messages. Default is `FALSE`.
#' @param verbose_stat [logical][base::logical] Whether to print detailed statistical messages. Default is `FALSE`.
#'
#' @return A [tibble][tibble::tibble()] with trend analysis results, including trend coefficients and statistical significance for each variables.
#'
#' More precisely, the returned `trendEX` [tibble][tibble::tibble()] contains the following columns :
#' * `*` : The idenfiant of time series
#' * `variable_en` : The name in english of variables
#' * `level` : see `MK_level`
#' * `H` : The result of the Mann-Kendall trend test. If `TRUE` a trend is detected at a risk level of `MK_level` (and a confidance of 1-`MK_level`). If `FALSE`, a trend is NOT detected at a risk level of `MK_level`.
#' * `p`: The p-value indicating the statistical significance of the Mann-Kendall trend test.
#' * `a` : The Then-Seil slope estimator that gives an approximation of the trend coefficient. WARNING : A value is always return even if the Mann-Kendall trend test is not significant.
#' * `b` : The ordinate at the origin in sort that you can trace `Y = a * X + b
#' * `period_trend` : An information about the period of analyse for the trend
#' * `mean_period_trend` : The average value of the variable along the `period_trend` that is usefull for normalisation (it is `NA` if `to_normalise` is `FALSE` for this variable).
#' * `a_normalise` : The `a` value normalised with `mean_period_trend`.
#' * `a_normalise_min` : The minimum of `a_normalise` values according to the selection made with `extreme_take_not_signif_into_account`, `extreme_take_only_series` and `extreme_by_suffix`.
#' * `a_normalise_max` : Same as `a_normalise_min` but for maximum values.
#' 
#' If the the `suffix` argument is used, a column is added :
#' * `variable_no_suffix_en` :  The name in english of variables without suffixes
#'
#' And also, if the `period_change` argument is filled up, more columns are added :
#' * `period_change` : An information about the period of analyse for the change break
#' * `mean_period_change` :
#' * `change` :
#' * `change_min` :
#' * `change_max` :
#'
#' @seealso
#' [process_extraction()] for extracting variables.
#' [CARD_extraction()] for extracting variables using CARD parameterization files.
#' [CARD_management()] for managing CARD parameterization files.
#'
#' @examples
#' ## Creation of random data set
#' set.seed(99)
#' Start = as.Date("2000-02-01")
#' End = as.Date("2010-04-04")
#' Date = seq.Date(Start, End, by="day")
#' 
#' # First time serie
#' data_1 = dplyr::tibble(time=Date,
#'                        X_state1=as.numeric(Date) +
#'                            rnorm(length(Date), 1e4, 1e3),
#'                        X_state2=seq(1, length(Date))/1e2 +
#'                            rnorm(length(Date), 0, 1),
#'                        id="serie 1")
#' data_1$X_state2[round(runif(500, 1, nrow(data_1)))] = NA
#' 
#' # Second time serie
#' data_2 = dplyr::tibble(time=Date,
#'                        X_state1=as.numeric(Date) +
#'                            rnorm(length(Date), 1e4, 1e3),
#'                        X_state2=seq(1, length(Date))/1e2 +
#'                            rnorm(length(Date), 0, 1),
#'                        id="serie 2")
#' data_2$X_state2[round(runif(1000, 1, nrow(data_2)))] = NA
#' 
#' # Final data for testing
#' data = dplyr::bind_rows(data_1, data_2)
#' 
#' ## Extraction of the yearly average of daily value.
#' dataEX = process_extraction(data=data,
#'                    funct=list(XA_state1=mean),
#'                    funct_args=list("X_state1", na.rm=TRUE),
#'                    time_step="year")
#' 
#' dataEX = process_extraction(data=data,
#'                             funct=list(XA=mean,
#'                                        XX=max),
#'                             funct_args=list(list("X", na.rm=TRUE),
#'                                             list("X", na.rm=TRUE)),
#'                             suffix=c("state1", "state2"),
#'                             time_step="year")
#' 
#' ## Trend test
#' # The direct application does not take care of possible grouped
#' # variables by suffix for extremes trends values.
#' trendEX1 = process_trend(dataEX,
#'                          to_normalise=TRUE)
#' print(trendEX1, width=Inf)
#' 
#' # More complicated case with the use of 'period_change' and customs
#' # normalisation info
#' trendEX2 =
#'     process_trend(dataEX,
#'                   suffix=c("state1", "state2"),
#'                   suffix_delimiter="_",
#'                   to_normalise=c("XA_state1"=FALSE,
#'                                  "XA_state2"=FALSE,
#'                                  "XX_state1"=TRUE,
#'                                  "XX_state2"=TRUE),
#'                   extreme_take_only_series=NULL,
#'                   extreme_by_suffix=FALSE,
#'                   period_change=list(c(as.Date("2000-01-01"),
#'                                        as.Date("2005-01-01")),
#'                                      c(as.Date("2006-01-01"),
#'                                        as.Date("2010-01-01"))))
#' print(trendEX2, width=Inf)
#' 
#' @export
#' @md
process_trend = function (dataEX,
                          MK_level=0.1,
                          time_dependency_option="INDE",
                          suffix=NULL,
                          suffix_delimiter="_",
                          to_normalise=FALSE,
                          metaEX=NULL,
                          extreme_take_not_signif_into_account=TRUE,
                          extreme_take_only_series=NULL,
                          extreme_by_suffix=TRUE,
                          period_trend=NULL,
                          period_change=NULL,
                          extreme_prob=0.01,
                          show_advance_stat=FALSE,
                          dev=FALSE,
                          verbose=FALSE,
                          verbose_stat=FALSE) {

    # check dataEX
    if (!tibble::is_tibble(dataEX)) {
        stop ("'dataEX' is not a tibble from the tibble package. This tibble needs a unique column of objects of class 'Date'")
    }
    
    # check Date column
    if (sum(sapply(dataEX, lubridate::is.Date)) == 0 & !dev) {
        stop ("There needs to be at least one column of objects of class 'Date'.")
    }
    if (sum(sapply(dataEX, lubridate::is.Date)) > 1) {
        stop ("There is more than one column of objects of class 'Date'. There needs to be only one column of objects of class 'Date'.")
    }

    # check numerical columns
    if (sum(sapply(dataEX, is.numeric) |
            sapply(dataEX, is.logical)) < 1) {
        stop ("There needs to be at least one column of class 'numeric' or 'logical'.")
    }
    
    # check character columns
    ID_colnames = names(dplyr::select(dataEX,
                                      dplyr::where(is.character)))
    if (sum(sapply(dataEX, is.character)) == 0 & !dev) {
        if (any(duplicated(
            dataEX[[which(sapply(dataEX,
                                 lubridate::is.Date))]]))) {
            stop ("There is at least one date value that repeat. It seems that either there is more than one time serie (so they need to be identify by a repeted character column for each serie) or there is an error in the format of the date column.")
        } else {
            warning ("There is no character column in order to identify uniquely each time serie. But hence it seems that there is only one time serie, a generic identifier will be add.")
            dataEX$id = "time serie"
        }
    } else if (sum(sapply(dataEX, is.character)) > 1) {
        message ("There is more than one character column. Thus, all the columns will be use to identify uniquely each time serie.")
        dataEX = tidyr::unite(dataEX, "ID",
                              dplyr::where(is.character),
                              sep="_")
    }

    # DATE NA

    if (!dev) {
        # check unicity of Date column for each character identifier
        Date_unicity =
            dplyr::summarise(dplyr::group_by(dataEX,
                                             get(names(dataEX)[sapply(dataEX, is.character)])),
                             n=sum(duplicated(get(names(dataEX)[sapply(dataEX, lubridate::is.Date)]))))
        if (any(Date_unicity$n > 0)) {
            stop (paste0("There is at least one duplicated date in time serie(s) named '",
                         paste0(Date_unicity[[1]][Date_unicity$n > 0],
                                collapse=", "), "'."))
        }
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

    check_date = function (x) {try(as.Date(x), silent=TRUE)}
    check_order = function (x) {all(order(x) == c(2, 1))}
    
    # check period
    if (!is.null(period_trend)) {
        if (!is.list(period_trend)) {
            period_trend = list(period_trend)
        }
        nPeriod_trend = length(period_trend)
        test = unlist(lapply(period_trend, check_date))
        if (any("try-error" %in% class(test)) || any(is.na(test))) {
            stop ("'period_trend' is not in a format able to be coerced to a 'Date' object")
        }
        if (any(unlist(lapply(period_trend, length)) == 1)) {
            stop ("There is only one date in 'period_trend'. Please, select a time period in your time serie(s) with two objects of class 'Date' or set 'period_trend' to NULL in order to use the entire available time serie(s).")
        }
        if (any(unlist(lapply(period_trend, length)) > 2)) {
            stop ("There is more than two date in 'period_trend'. Please, select a time period in your time serie(s) with two objects of class 'Date' or set 'period_trend' to NULL in order to use the entire available time serie(s).")
        }
        if (any(unlist(lapply(period_trend, check_order)))) {
            message ("'period_trend' seems to have two date not in the increasing order. Thus, 'period_trend' will be re-ordered.")
            period_trend = lapply(period_trend, sort)
        }
    } else {
        nPeriod_trend = 1
    }

    
    if (!is.null(period_change)) {
        if (!is.list(period_change)) {
            period_change = list(period_change)
        }
        nPeriod_change = length(period_change)
        test = unlist(lapply(period_change, check_date))
        if (any("try-error" %in% class(test)) || any(is.na(test))) {
            stop ("'period_change' is not in a format able to be coerced to a 'Date' object")
        }
        if (any(unlist(lapply(period_trend, length)) == 1)) {
            stop ("There is only one date in 'period_change'. Please, select a time period in your time serie(s) with two objects of class 'Date' or set 'period_change' to NULL in order to use the entire available time serie(s).")
        }
        if (any(unlist(lapply(period_trend, length)) > 2)) {
            stop ("There is more than two date in 'period_change'. Please, select a time period in your time serie(s) with two objects of class 'Date' or set 'period_change' to NULL in order to use the entire available time serie(s).")
        }
        if (any(unlist(lapply(period_change, check_order)))) {
            message ("'period_change' seems to have two date not in the increasing order. Thus, 'period_change' will be re-ordered.")
            period_change = lapply(period_change, sort)
        }
        period_change = lapply(period_change, as.Date)
        
    } else {
        nPeriod_change = 1
    }

    # check verbose
    if (!is.logical(verbose)) {
        stop ("'verbose' needs to be an object of class 'logical'.")
    }
    
    tree("TREND ANALYSE", 0, verbose=verbose)

    names_save = names(dataEX)
    idCode_save = NULL
    idDate_save = NULL
    idVariable = c()
    names_save = names(dataEX)
    
    for (id in 1:ncol(dataEX)) {
        x = dataEX[[id]]

        if (is.character(x)) {
            idCode_save = id
        } else if (lubridate::is.Date(x)) {
            idDate_save = id
        } else if (is.numeric(x) | is.logical(x)) {
            idVariable = c(idVariable, id)
        }
    }

    nVariable = length(idVariable)
    Variable = names(dataEX)[idVariable]
    names(dataEX)[c(idCode_save, idDate_save)] =
        c("code", "date")


    if (is.null(metaEX)) {
        if (length(to_normalise) == 1 & nVariable == 1) {
            names(to_normalise) = Variable
        } else if (length(to_normalise) == 1 & nVariable >= 1) {
            warning (paste0("'to_normalise' is a unique value so it will be repeated for all the variables : ",
                            paste0(Variable, collapse=", ")))
            to_normalise = rep(to_normalise, nVariable)
            names(to_normalise) = Variable   
        } else if (!all(names(to_normalise) %in% Variable)) {
            stop (paste0("'to_normalise' does not contain normalisation info for all the variables : ",
                         paste0(Variable, collapse=", ")))       
        }
    } else {
        message ("'metaEX' normalisation's info will be used instead of 'to_normalise' setting.")
    }
    
    trendEX = dplyr::tibble()
    
    for (j in 1:nPeriod_trend) {
        if (is.null(period_trend)) {
            dataEX_period = dataEX
        } else {
            period = as.Date(period_trend[[j]])
            if (is.na(period[1])) {
                period[1] = min(dataEX$date, na.rm=TRUE)
            }
            if (is.na(period[2])) {
                period[2] = max(dataEX$date, na.rm=TRUE)
            }
            
            dataEX_period = dplyr::filter(dataEX,
                                          min(period) <= date &
                                          date <= max(period))
        }

        tree(paste0("For period ", paste0(period, collapse=" ")),
             1, end=j==nPeriod_trend, verbose=verbose)
        if (j==nPeriod_trend) {
            inEnd_period = 1
        } else {
            inEnd_period = NULL
        }
        
        dataEX_period = dplyr::group_by(dataEX_period, code)
        trendEX_period = dplyr::tibble()
        
        for (k in 1:nVariable) {
            variable = Variable[k]

            tree(paste0("For variable ", variable),
                 2, end=k==nVariable,
                 inEnd=inEnd_period, verbose=verbose)
            if (k == nVariable) {
                inEnd = c(inEnd_period, 2)
            } else {
                inEnd = inEnd_period
            }
            
            dataEX_period_Variable =
                dplyr::select(dataEX_period,
                              dplyr::all_of(c("code", "date",
                                              variable)))

            tree(paste0("Application of the Mann-Kendall statistical test ",
                        "with a level of ",
                        round(MK_level*100), " % and ",
                        time_dependency_option, " time dependency option"),
                 3, inEnd=inEnd, verbose=verbose)
            
            trendEX_period_Variable =
                dplyr::summarise(dataEX_period_Variable,
                                 GeneralMannKendall_WRAP(
                                     get(variable),
                                     level=MK_level,
                                     time_dependency_option=
                                         time_dependency_option,
                                     DoDetrending=TRUE,
                                     show_advance_stat=show_advance_stat,
                                     verbose=verbose_stat))

            trendEX_period_Variable$variable_en = variable
            trendEX_period_Variable = dplyr::relocate(trendEX_period_Variable,
                                                      variable_en,
                                                      .after=code)
            if (!is.null(suffix)) {
                variable_no_suffix = variable
                for (i in 1:length(suffix)) {
                    variable_no_suffix = gsub(suffix[i], "",
                                              variable_no_suffix,
                                              fixed=TRUE)
                }
                trendEX_period_Variable$variable_no_suffix_en = variable_no_suffix
                trendEX_period_Variable = dplyr::relocate(trendEX_period_Variable,
                                                          variable_no_suffix_en,
                                                          .after=variable_en)
            } else {
                variable_no_suffix = variable
            }
            
            to_normalise_variable =
                to_normalise[names(to_normalise) == variable]

            
            tree(paste0("Estimation of other variable"),
                 3, end=TRUE, inEnd=inEnd, verbose=verbose)
            
            tree("Computing of the intercept of trend",
                 4, inEnd=c(inEnd, 3), verbose=verbose)
            trendEX_period_Variable = get_intercept(dataEX_period_Variable,
                                                    trendEX_period_Variable,
                                                    verbose=verbose)

            tree("Computing of the optimal period",
                 4, inEnd=c(inEnd, 3), verbose=verbose)
            trendEX_period_Variable = get_period(dataEX_period_Variable,
                                                 trendEX_period_Variable,
                                                 verbose=verbose)
            
            tree("Normalise trend value",
                 4, end=is.null(period_change), inEnd=c(inEnd, 3), verbose=verbose)
            trendEX_period_Variable =
                get_normalise(dataEX_period_Variable,
                              trendEX_period_Variable,
                              to_normalise=to_normalise_variable,
                              metaEX=metaEX,
                              suffix=suffix,
                              verbose=verbose)

            if (!is.null(period_change)) {
                tree("Get period change",
                     4, end=TRUE, inEnd=c(inEnd, 3), verbose=verbose)
                trendEX_period_Variable =
                    get_change(dataEX_period_Variable, 
                               trendEX_period_Variable,
                               period_change=period_change,
                               to_normalise=to_normalise_variable,
                               metaEX=metaEX,
                               suffix=suffix,
                               verbose=verbose)
            }
            
            trendEX_period = dplyr::bind_rows(trendEX_period,
                                              trendEX_period_Variable)
        }
        
        tree(paste0("Computing extreme trend values"),
             2, end=is.null(period_change), inEnd=inEnd_period, verbose=verbose)
        trendEX_period =
            get_extreme_trend(trendEX_period,
                              suffix=suffix,
                              extreme_take_not_signif_into_account=
                                  extreme_take_not_signif_into_account,
                              extreme_take_only_series=extreme_take_only_series,
                              extreme_by_suffix=extreme_by_suffix, 
                              extreme_prob=extreme_prob,
                              verbose=verbose)
        
        if (!is.null(period_change)) {
            tree(paste0("Computing extreme change values"),
                 2, end=TRUE, inEnd=inEnd_period, verbose=verbose)
            trendEX_period =
                get_extreme_change(trendEX_period,
                                   suffix=suffix,
                                   extreme_take_only_series=extreme_take_only_series,
                                   extreme_by_suffix=extreme_by_suffix, 
                                   extreme_prob=extreme_prob,
                                   verbose=verbose)
        }

        trendEX = dplyr::bind_rows(trendEX, trendEX_period)
    }

    # if (isFDR) { ### /!\ pas ok
    #     dataEX.final$p.FDR =
    #         fieldSignificance_FDR(dataEX.final$p,
    #                               level=FDR_level)
    # }

    trendEX = dplyr::arrange(trendEX, code, variable_en)

    idCode = which(names(trendEX) == "code")

    names(trendEX)[c(idCode)] =
        names_save[c(idCode_save)]

    if (length(ID_colnames) > 1) {
        trendEX = tidyr::separate(trendEX, col="ID",
                                  into=ID_colnames, sep="_")
    }

    return (trendEX)
}



#### 2.3.2. Period of trend analysis _________________________________
get_period = function (dataEX, trendEX, verbose=TRUE) {

    Period = dplyr::summarise(dplyr::group_by(dataEX, code),
                              start=min(date, na.rm=TRUE),
                              end=max(date, na.rm=TRUE),
                              period_trend=list(c(start, end)))
    
    trendEX = dplyr::full_join(trendEX,
                               dplyr::select(Period,
                                             c("code",
                                               "period_trend")),
                               by="code")
    return (trendEX)
}

#### 2.3.3. Intercept of trend _______________________________________
get_intercept = function (dataEX, trendEX,
                          verbose=TRUE) {

    variable =  levels(factor(trendEX$variable_en))
    
    MU_X = dplyr::summarise(dplyr::group_by(dataEX, code),
                            mu_X=mean(get(variable),
                                      na.rm=TRUE))

    MU_t = dplyr::summarise(dplyr::group_by(dataEX, code),
                            mu_t=as.numeric(mean(date,
                                                 na.rm=TRUE)) /
                                mean(as.numeric(diff(date)), na.rm=TRUE))

    analyse = dplyr::tibble(code=trendEX$code,
                            a=trendEX$a,
                            mu_X=MU_X$mu_X,
                            mu_t=MU_t$mu_t)
    
    B = dplyr::summarise(dplyr::group_by(analyse, code),
                         b=mu_X - mu_t * a)
    
    trendEX = dplyr::full_join(trendEX, B, by="code")

    trendEX$b[!is.finite(trendEX$b)] = NA

    return (trendEX)
}


get_normalise = function (dataEX,
                          trendEX,
                          to_normalise,
                          metaEX=NULL,
                          suffix=NULL,
                          verbose=FALSE) {
    
    variable =  levels(factor(trendEX$variable_en))

    if (!is.null(suffix)) {
        variable_no_suffix = variable
        for (i in 1:length(suffix)) {
            variable_no_suffix = gsub(suffix[i], "",
                                      variable_no_suffix,
                                      fixed=TRUE)
        }
        if (!is.null(metaEX)) {
            to_normalise = metaEX$to_normalise[metaEX$variable_en == variable_no_suffix]
        }
    } else {
        if (!is.null(metaEX)) {
            to_normalise = metaEX$to_normalise[metaEX$variable_en == variable]
        }
    }

    if (to_normalise) {
        dataEX_mean =
            dplyr::summarise(dplyr::group_by(dataEX, code),
                             mean=mean(get(variable),
                                       na.rm=TRUE))
        trendEX = dplyr::full_join(trendEX,
                            dataEX_mean,
                            by="code")
        trendEX$a_normalise = trendEX$a / trendEX$mean * 100
        trendEX$mean[!is.finite(trendEX$mean)] = NA
        trendEX = dplyr::rename(trendEX, mean_period_trend=mean)
        
    } else {
        trendEX$mean_period_trend = NA
        trendEX$a_normalise = trendEX$a
    }
    return (trendEX)
}


get_change = function (dataEX, trendEX,
                       period_change,
                       to_normalise,
                       metaEX=NULL,
                       suffix=NULL,
                       verbose=FALSE) {
    
    variable =  levels(factor(trendEX$variable_en))

    if (!is.null(suffix)) {
        variable_no_suffix = variable
        for (i in 1:length(suffix)) {
            variable_no_suffix = gsub(suffix[i], "",
                                      variable_no_suffix,
                                      fixed=TRUE)
        }
        if (!is.null(metaEX)) {
            to_normalise = metaEX$to_normalise[metaEX$variable_en == variable_no_suffix]
        }
    } else {
        if (!is.null(metaEX)) {
            to_normalise = metaEX$to_normalise[metaEX$variable_en == variable]
        }
    }
    
    nPeriod_change = length(period_change)
    if (nPeriod_change != 2) {
        return (trendEX)
    }

    dataEX_change = 
        dplyr::summarise(dplyr::group_by(dataEX, code),
                         
                         start_1=max(c(period_change[[1]][1],
                                       min(date, na.rm=TRUE)), na.rm=TRUE),
                         end_1=min(c(period_change[[1]][2],
                                     max(date, na.rm=TRUE)), na.rm=TRUE),
                         
                         start_2=max(c(period_change[[2]][1],
                                       min(date, na.rm=TRUE)), na.rm=TRUE),
                         end_2=min(c(period_change[[2]][2],
                                     max(date, na.rm=TRUE)), na.rm=TRUE),
                         
                         .groups="drop")

    dataEX_change = 
        dplyr::mutate(dplyr::group_by(dataEX_change, code),
                      period_change=list(list(c(start_1, end_1),
                                              c(start_2, end_2))))
    
    dataEX_change_1 =
        dplyr::summarise(
                   dplyr::group_by(
                              dplyr::filter(dataEX,
                                            period_change[[1]][1] <= date &
                                            date <= period_change[[1]][2]),
                              code),
                   mean_1=mean(get(variable), na.rm=TRUE))

    dataEX_change_2 =
        dplyr::summarise(
                   dplyr::group_by(
                              dplyr::filter(dataEX,
                                            period_change[[2]][1] <= date &
                                            date <= period_change[[2]][2]),
                              code),
                   mean_2=mean(get(variable), na.rm=TRUE))

    dataEX_change_tmp = dplyr::full_join(dataEX_change_1, dataEX_change_2,
                                         by="code")
    dataEX_change_tmp =
        dplyr::mutate(dplyr::group_by(dataEX_change_tmp, code),
                      mean_period_change=list(c(mean_1, mean_2)))
    
    dataEX_change = dplyr::full_join(dataEX_change, dataEX_change_tmp,
                                     by="code")
    
    if (to_normalise) {
        dataEX_change$change =
            (dataEX_change$mean_2 - dataEX_change$mean_1) / dataEX_change$mean_1 * 100
    } else {
        dataEX_change$change =
            dataEX_change$mean_2 - dataEX_change$mean_1
    }

    dataEX_change$change[!is.finite(dataEX_change$change)] = NA
    
    trendEX = dplyr::full_join(trendEX,
                        dplyr::select(dataEX_change,
                               c("code",
                                 "period_change",
                                 "mean_period_change",
                                 "change")),
                        by="code")
    return (trendEX)
}



get_extreme_trend = function (trendEX,
                              suffix=NULL,
                              extreme_take_not_signif_into_account=TRUE,
                              extreme_take_only_series=NULL,
                              extreme_by_suffix=TRUE,
                              extreme_prob=0.01,
                              verbose=FALSE) {

    if (!extreme_take_not_signif_into_account) {
        trendEX_a_normalise = trendEX$a_normalise
        trendEX$a_normalise[!trendEX$H] = NA
    }

    if (is.null(extreme_take_only_series)) {
        extreme_take_only_series = trendEX$code
    }

    if (extreme_by_suffix | is.null(suffix)) {
        variable_tmp = "variable_en" 
    } else {
        variable_tmp = "variable_no_suffix_en" 
    }
    
    trendEX = dplyr::mutate(dplyr::group_by(trendEX,
                                            !!!rlang::data_syms(variable_tmp)),
                            a_normalise_min=
                                stats::quantile(a_normalise[code %in% extreme_take_only_series],
                                         extreme_prob,
                                         na.rm=TRUE),
                            a_normalise_max=
                                stats::quantile(a_normalise[code %in% extreme_take_only_series],
                                         1-extreme_prob,
                                         na.rm=TRUE),
                            .keep="all")

    if (!extreme_take_not_signif_into_account) {
        trendEX$a_normalise = trendEX_a_normalise
    }
    
    return (trendEX)
}




get_extreme_change = function (trendEX,
                               suffix=NULL,
                               extreme_take_only_series=NULL,
                               extreme_by_suffix=TRUE,
                               extreme_prob=0.01,
                               verbose=FALSE) {

    if (is.null(extreme_take_only_series)) {
        extreme_take_only_series = trendEX$code
    }

    if (extreme_by_suffix | is.null(suffix)) {
        variable_tmp = "variable_en" 
    } else {
        variable_tmp = "variable_no_suffix_en" 
    }
    
    trendEX = dplyr::mutate(dplyr::group_by(trendEX,
                                            !!!rlang::data_syms(variable_tmp)),
                            change_min=
                                stats::quantile(change[code %in% extreme_take_only_series],
                                         extreme_prob,
                                         na.rm=TRUE),
                            change_max=
                                stats::quantile(change[code %in% extreme_take_only_series],
                                         1-extreme_prob,
                                         na.rm=TRUE),
                            .keep="all")

    trendEX = dplyr::relocate(trendEX,
                              a_normalise_min,
                              a_normalise_max,
                              .after=a_normalise)
    
    return (trendEX)
}

