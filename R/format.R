# \\\
# Copyright 2021-2022 Louis Héraut*1,
#                     Éric Sauquet*2,
#                     Valentin Mansanarez
#
# *1   INRAE, France
#      louis.heraut@inrae.fr
# *2   INRAE, France
#      eric.sauquet@inrae.fr
#
# This file is part of ash R toolbox.
#
# Ash R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Ash R toolbox is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ash R toolbox.
# If not, see <https://www.gnu.org/licenses/>.
# ///
#
#
# R/processing/format.R
#
# Manages all the format problem of data and info. Mainly problem of
# input and output of the 'StatsAnalysisTrend' package. It also allows
# to join different selections of station and to gets exact period of
# trend analysis.


## 1. FORMATTING OF DATA _____________________________________________
### 1.1. Joining selection ___________________________________________
# Joins tibbles of different selection of station as a unique one
#' @title Join selection
#' @export
join_selection = function (list_data, list_meta, list_from) {

    nb_selec = length(list_data)
    Blank = TRUE

    Code = c()
    for (i in 1:nb_selec) {
        df_datatmp = list_data[[i]]
        df_metatmp = list_meta[[i]]
        from = list_from[[i]]
        
        if (!is.null(df_datatmp)) {
            
            df_datatmp = df_datatmp[!(df_datatmp$Code %in% Code),]
            df_metatmp = df_metatmp[!(df_metatmp$Code %in% Code),]
            Code = c(Code,
                     df_metatmp$Code[!(df_metatmp$Code %in% Code)])
            
            df_metatmp$source = from
            
            if (Blank) {
                df_data = df_datatmp
                df_meta = df_metatmp
                Blank = FALSE
            } else {
                # Joins tibble
                df_data = full_join(df_data, df_datatmp)
                df_meta = full_join(df_meta, df_metatmp)
            }
        }
    }
    # If there is no data
    if (Blank) {
        stop('No data')
    }
    return (list(data=df_data, meta=df_meta))
}


## 2. WRAP OF TREND ANALYSE __________________________________________
### 2.1. extract.Var _________________________________________________
#' @title extract_Var WRAP
#' @export
extract_Var_WRAP = function (df_data, funct, period,
                             hydroPeriod="01-01", timestep="year",
                             isDate=FALSE, verbose=TRUE, ...) {

    if (verbose) {
        print('... Extraction of data')
    }
    
    # Groups the data by code column
    df_data = group_by(df_data, Code)

    if ("Na.percent" %in% names(df_data)) {
        # Creates a new tibble of data with a group column
        data = tibble(Date=df_data$Date, 
                      group=group_indices(df_data),
                      Value=df_data$Value,
                      Na.percent=df_data$Na.percent)
    } else {
        # Creates a new tibble of data with a group column
        data = tibble(Date=df_data$Date, 
                      group=group_indices(df_data),
                      Value=df_data$Value)
    }
    
    # Gets the different value of the group
    Gkey = group_keys(df_data)
    # Creates a new tibble of info of the group
    info = bind_cols(group=seq(1:nrow(Gkey)),
                     Gkey)

    # Stores data and info tibble as a list that match the entry of
    # the 'extract.Var' function
    df_Xlist = list(data=data, info=info)

    if (timestep == "year-month" | timestep == "month") {
        per.start = substr(hydroPeriod[1], 4, 5)
    } else {
        per.start = hydroPeriod[1]
    }
    
    df_XEx = StatsAnalysisTrend::extract.Var(data.station=df_Xlist,
                                             funct=funct,
                                             period=period,
                                             per.start=per.start,
                                             timestep=timestep,
                                             pos.datetime=1,
                                             ...)

    colnames(df_XEx) = c('Date', 'group', 'Value', 'NA_pct')
    df_XEx$Date = as.Date(paste0(df_XEx$Date, '-', per.start))
    # Recreates the outing of the 'extract.Var' function nicer
    df_XEx = tibble(Date=df_XEx$Date,
                    Value=df_XEx$Value,
                    Code=df_Xlist$info$Code[df_XEx$group],
                    NA_pct=df_XEx$NA_pct*100)

    if (isDate) {
        # Converts index of the tCEN to the julian date associated
        df_XEx = convert_dateEx(df_XEx, df_data, hydroYear=hydroPeriod[1],
                                verbose=verbose)
    }
    return (df_XEx)
}

### 2.2. Estimate.stats ______________________________________________
#' @title Estimate_stats_WRAP
#' @export
Estimate_stats_WRAP = function (df_XEx, period=NULL, dep_option='AR1',
                                verbose=TRUE) {

    if (verbose) {
        print('... Estimation of trend')
    }
    
    df_XEx = group_by(df_XEx, Code)
    df_XEx_RAW = tibble(datetime=as.numeric(format(df_XEx$Date, "%Y")),
                        group1=group_indices(df_XEx),
                        values=df_XEx$Value,
                        Na.percent=df_XEx$NA_pct/100)
    # Gets the different value of the group
    Gkey = group_keys(df_XEx)
    # Creates a new tibble of info of the group
    info = bind_cols(group=seq(1:nrow(Gkey)),
                     Gkey)

    df_Xtrend = StatsAnalysisTrend::Estimate.stats(data.extract=df_XEx_RAW,
                                                   dep.option=dep_option)

    
    # Converts results of trend to tibble
    df_Xtrend = tibble(df_Xtrend)
    colnames(df_Xtrend)[1] = 'group'
    df_Xtrend = tibble(Code=info$Code[df_Xtrend$group],
                       df_Xtrend[-1])
    
    df_Xtrend = get_intercept(df_XEx, df_Xtrend, verbose=verbose)
    df_Xtrend = get_period(df_XEx, df_Xtrend, verbose=verbose)
    if (!is.null(period)) {
        df_Xtrend$input_period = paste(period, collapse='/')
    }
    return (df_Xtrend)
}

### 2.3. Wrap tools __________________________________________________
#### 2.3.1. Convert index to  date ___________________________________
#' @title Convert index to  date
#' @export
convert_dateEx = function(df_XEx, df_data, hydroYear="01-01", verbose=TRUE) {

    if (verbose) {
        print('.... Conversion to number of day')
    }
    
    Shift_hydroYear = as.integer(df_XEx$Date - as.Date(paste0(format(df_XEx$Date, "%Y"), "-01-01")))
    df_XEx$Value = df_XEx$Value + Shift_hydroYear
    
    df_Date = summarise(group_by(df_data, Code),
                                Start=min(Date, na.rm=TRUE))
    df_Date$Julian = NA
    df_Date$origin = as.Date(paste0(format(df_Date$Start, "%Y"),
                                    '-', hydroYear))
    for (i in 1:nrow(df_Date)) {
        df_Date$Julian[i] = julian(df_Date$Start[i], origin=df_Date$origin[i])
    }

    df_Date$Year = format(df_Date$Start, "%Y")

    for (code in df_Date$Code) {
        Ok_Start = df_Date$Code == code
        Shift = df_Date$Julian[Ok_Start]
        year = df_Date$Year[Ok_Start]
        OkXEx_code_year =
            df_XEx$Code == code & format(df_XEx$Date, "%Y") == year        
        df_XEx$Value[OkXEx_code_year] =
            df_XEx$Value[OkXEx_code_year] + Shift

        OkXEx_code = df_XEx$Code == code
        Month = df_XEx$Value[OkXEx_code] / (365.25/12)        
        MonthNoNA = Month[!is.na(Month)]

        fact = 2*pi/12
        monthMean_raw = CircStats::circ.mean(fact * MonthNoNA) / fact
        monthMean = (monthMean_raw + 12) %% 12

        upLim = monthMean + 6
        lowLim = monthMean - 6

        above = Month > upLim
        above[is.na(above)] = FALSE
        below = Month < lowLim
        below[is.na(below)] = FALSE
        
        df_XEx$Value[OkXEx_code][above] = df_XEx$Value[OkXEx_code][above] - 365
        df_XEx$Value[OkXEx_code][below] = df_XEx$Value[OkXEx_code][below] + 365
    }

    Year = format(df_XEx$Date, "%Y")
    Start = as.Date(paste0(Year, '-', hydroYear))
    End = Start + lubridate::years(1) - lubridate::days(1)
    nbDate = as.numeric(difftime(End, Start,
                                 units="days"))

    # Issue for negative value in the y axis
    # df_XEx$Value = df_XEx$Value + 365 
    
    return (df_XEx)
}

#### 2.3.2. Period of trend analysis _________________________________
# Compute the start and the end of the period for a trend analysis
# according to the accessible data 
#' @title Period of trend analysis
#' @export
get_period = function (df_XEx, df_Xtrend=NULL, verbose=TRUE) {

    if (verbose) {
        print('.... Computes the optimal period of trend analysis')
    }

    df_Start = summarise(group_by(df_XEx, Code),
                         Start=min(Date, na.rm=TRUE))
    
    df_End = summarise(group_by(df_XEx, Code),
                       End=max(Date, na.rm=TRUE))

    if (!is.null(df_Xtrend)) {
        df_Xtrend$period_start = df_Start$Start
        df_Xtrend$period_end = df_End$End
        return (df_Xtrend)
    } else {
        res = list(Start=df_Start, End=df_End)
        return (res)
    }
}

#### 2.3.3. Intercept of trend _______________________________________
# Compute intercept values of linear trends with first order values
# of trends and the data on which analysis is performed.
#' @title Intercept of trend
#' @export
get_intercept = function (df_XEx, df_Xtrend, unit2day=365.25,
                          verbose=TRUE) {

    if (verbose) {
        print('.... Computes intercept of trend')
    }
    
    df_mu_X = summarise(group_by(df_XEx, Code),
                        mu_X=mean(Value, na.rm=TRUE))

    df_mu_t = summarise(group_by(df_XEx, Code),
                        mu_t=as.numeric(mean(Date, na.rm=TRUE)) / unit2day)

    df_Xtrendtmp = tibble(Code=df_Xtrend$Code,
                          trend=df_Xtrend$trend,
                          mu_X=df_mu_X$mu_X,
                          mu_t=df_mu_t$mu_t)
    
    df_b = summarise(group_by(df_Xtrendtmp, Code),
                     b=mu_X - mu_t * trend)
    
    df_Xtrend$intercept = df_b$b
    
    return (df_Xtrend)
}


## 3. FOLLOWING OF DATA MODIFICATIONS ________________________________
#' @title Add modification info
#' @export
add_mod = function (df_mod, Code, type, fun_name, comment, df_meta=NULL) {
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = rle(df_data$Code)$value
    }
    
    for (code in Code) {
        df_modtmp = tibble(Code=code, type=type,
                           fun_name=fun_name,
                           comment=comment)
        df_mod = bind_rows(df_mod, df_modtmp)
    }
    return (df_mod)
}
