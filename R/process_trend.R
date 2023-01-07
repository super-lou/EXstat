# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1
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


#' @title process_trend
#' @description Applies Mann-Kendall statistical test to an extracted
#' variable in order to determine the existence or not of a trend in
#' the data.
#' @param data Input data format is a tibble of at least a column of
#' date, a UNIQUE column of numeric value and a character column for
#' names of time series in order to identify them. Thus it is possible
#' to have a tibble with multiple time series which can be grouped by
#' their names. The trend analysis will be performed on the numeric
#' column.
#' @param MK_level Numeric value between 0 and 1 which is the level used in the Mann-Kendall statistical test.
#' @param ... other arguments passed to function
#' \code{GeneralMannKendall_WRAP}.
#' @return A table which specifies per line the name of the time series the value 'p', the value 'stat' and 'a' the slope of Theil-Sen. For example, if the 'p' value is less than 0.1 and the slope 'a' is positive, the associated time serie shows an increasing linear trend which can be represented by the equation 'Y = a*X + b ' with 'a' the Type I error of 10% or 90% confidence.
#' @examples
#' # Date
#' Start = as.Date("1972-01-01")
#' End = as.Date("2020-12-31")
#' Date = seq.Date(Start, End, by="day")
#' 
#' # Value to analyse
#' set.seed(100)
#' X = seq(1, length(Date))/1e4 + runif(length(Date), -100, 100)
#' X[as.Date("2000-03-01") <= Date & Date <= as.Date("2000-09-30")] = NA
#'
#' # Creation of tibble
#' data = dplyr::tibble(Date=Date, ID="serie A", X=X)
#'
#' # Extraction
#' dataEX = process_extraction(data=data,
#'                             samplePeriod=c("05-01",
#'                                            "11-30"),
#'                             funct=max,
#'                             na.rm=TRUE,
#'                             period=c(as.Date("1990-01-01"),
#'                                      as.Date("2020-12-31")),
#'                             timeStep="year")
#'
#' trendEX = process_trend(data=dataEX)
#' trendEX
#' @importFrom rlang .data
#' @export
process_trend = function (data,
                          MK_level=0.1,
                          timeDep_option="INDE",
                          # isFDR=FALSE,
                          # FDR_level=0.1,
                          verbose=FALSE,
                          ...) {

    tree("TREND ANALYSE", 0, verbose=verbose)
    
    names_save = names(data)
    idValue_save = c()
    for (id in 1:ncol(data)) {
        x = data[[id]]

        if (is.character(x)) {
            idCode_save = id
        } else if (lubridate::is.Date(x)) {
            idDate_save = id
        } else if (is.numeric(x)) {
            if (names_save[id] == "NApct") {
                idNA_save = id
            } else {
                idValue_save = c(idValue_save, id)
            }
        }
    }
    
    names(data)[c(idCode_save, idDate_save, idValue_save)] =
        c("Code", "Date", "Value")

    tree("Grouping data by code", 1, verbose=verbose)
    # Group data accordingly to group.names
    data = dplyr::group_by(data, Code)

    tree("Statistical test",
         1, verbose=verbose)

    tree(paste0("Application of the Mann-Kendall statistical test ",
                "with a level of ",
                round(MK_level*100), " % and ",
                timeDep_option, " time dependency option"),
         2, verbose=verbose)
    
    # 1pply function on values accounting for grouping variables
    # 'group.names"
    trendEX =
        dplyr::summarise(data,
                         GeneralMannKendall_WRAP(
                             Value,
                             level=MK_level,
                             timeDep_option=timeDep_option,
                             ...))
    
    tree("Estimation of other variable",
         1, end=TRUE, verbose=verbose)
    trendEX = get_intercept(data, trendEX, verbose=verbose)
    trendEX = get_period(data, trendEX, verbose=verbose)

    # if (isFDR) { ### /!\ pas ok
    #     data.final$p.FDR =
    #         fieldSignificance_FDR(data.final$p,
    #                               level=FDR_level)
    # }

    idCode = which(names(trendEX) == "Code")

    names(trendEX)[c(idCode)] =
        names_save[c(idCode_save)]

    return (trendEX)
}



#### 2.3.2. Period of trend analysis _________________________________
# Compute the start and the end of the period for a trend analysis
# according to the accessible data 
#' @title Period of trend analysis
#' @export
get_period = function (data, trendEX, verbose=TRUE) {

    tree("Computing of the optimal periods of trend analysis",
         2, end=TRUE, inEnd=1, verbose=verbose)

    Start = dplyr::summarise(dplyr::group_by(data, Code),
                             start=min(Date, na.rm=TRUE))
    
    End = dplyr::summarise(dplyr::group_by(data, Code),
                           end=max(Date, na.rm=TRUE))

    Period = dplyr::full_join(Start, End, by="Code")
    trendEX = dplyr::full_join(trendEX, Period, by="Code")
    # trendEX$start = Start$Start
    # trendEX$end = End$End
    return (trendEX)
}

#### 2.3.3. Intercept of trend _______________________________________
# Compute intercept values of linear trends with first order values
# of trends and the data on which analysis is performed.
#' @title Intercept of trend
#' @export
get_intercept = function (data, trendEX,
                          verbose=TRUE) {

    tree("Computing of the intercept of trend",
         2, inEnd=1, verbose=verbose)
    
    MU_X = dplyr::summarise(dplyr::group_by(data, Code),
                            mu_X=mean(Value, na.rm=TRUE))

    MU_t = dplyr::summarise(dplyr::group_by(data, Code),
                            mu_t=as.numeric(mean(Date,
                                                 na.rm=TRUE)) /
                            mean(as.numeric(diff(Date)), na.rm=TRUE))

    analyse = dplyr::tibble(Code=trendEX$Code,
                            a=trendEX$a,
                            mu_X=MU_X$mu_X,
                            mu_t=MU_t$mu_t)
    
    B = dplyr::summarise(dplyr::group_by(analyse, Code),
                         b=mu_X - mu_t * a)
    
    trendEX = dplyr::full_join(trendEX, B, by="Code")

    return (trendEX)
}
