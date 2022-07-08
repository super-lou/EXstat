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
# R/plotting/shortcut.R


## 1. EXTREMES OF VALUE FOR ALL STATION ______________________________
### 1.1. Trend _______________________________________________________
#' @title Trend Extremes
#' @export
short_trendExtremes = function (list_df2plot, Code, nPeriod_trend,
                                nbVar, nCode, colorForce=FALSE) {
    
    # Blank array to store mean of the trend for each
    # station, perdiod and variable
    TrendValue_code = array(rep(1, nPeriod_trend*nbVar*nCode),
                            dim=c(nPeriod_trend, nbVar, nCode))

    # For all the period
    for (j in 1:nPeriod_trend) {
        # For all the code
        for (k in 1:nCode) {
            # Gets the code
            code = Code[k]
            
            for (i in 1:nbVar) {
                # Extracts the data corresponding to the
                # current variable
                df_data = list_df2plot[[i]]$data
                # Extracts the trend corresponding to the
                # current variable
                df_trend = list_df2plot[[i]]$trend
                # Extracts the type of the variable
                type = list_df2plot[[i]]$type
                # Extracts the data corresponding to the code
                df_data_code = df_data[df_data$code == code,] 
                df_trend_code = df_trend[df_trend$code == code,]

                # Extract start and end of trend periods
                Start = df_trend_code$period_start[j]
                End = df_trend_code$period_end[j]
                
                # Extracts the corresponding data for the period
                df_data_code_per =
                    df_data_code[df_data_code$Date >= Start 
                                 & df_data_code$Date <= End,]
                
                # Same for trend
                df_trend_code_per = 
                    df_trend_code[df_trend_code$period_start == Start 
                                  & df_trend_code$period_end == End,]
                
                # Computes the number of trend analysis selected
                Ntrend = nrow(df_trend_code_per)
                # If there is more than one trend on the same period
                if (Ntrend > 1) {
                    # Takes only the first because they are similar
                    df_trend_code_per = df_trend_code_per[1,]
                }
                
                # If it is a flow variable
                if (type == 'sévérité') {
                    # Computes the mean of the data on the period
                    dataMean = mean(df_data_code_per$Value, na.rm=TRUE)
                    # Normalises the trend value by the mean of the data
                    trendValue = df_trend_code_per$trend / dataMean
                    # If it is a date variable
                } else if (type == 'saisonnalité' | type == 'pluviométrie' | type == 'température' | type == 'évapotranspiration') {
                    trendValue = df_trend_code_per$trend
                }
                
                # If the p value is under the threshold
                if (df_trend_code_per$p <= alpha | colorForce) {
                    # Stores the mean trend
                    TrendValue_code[j, i, k] = trendValue
                    # Otherwise
                } else {
                    # Do not stocks it
                    TrendValue_code[j, i, k] = NA
                }                
            }
        }
    }

    # Compute the min and the max of the mean trend for all the station
    minTrendValue = apply(TrendValue_code, c(1, 2), min, na.rm=TRUE)
    maxTrendValue = apply(TrendValue_code, c(1, 2), max, na.rm=TRUE)

    res = list(min=minTrendValue, max=maxTrendValue)
    return (res)
}

### 1.2. Mean ________________________________________________________
#' @title Mean trend Extremes
#' @export
short_meanExtremes = function (list_df2plot, Code, nPeriod_mean, nbVar, nCode) {
    # Blank array to store difference of mean between two periods
    breakValue_code = array(rep(1, nPeriod_mean*nbVar*nCode),
                            dim=c(nPeriod_mean, nbVar, nCode))
    # Blank array to store mean for a temporary period in order
    # to compute the difference of mean with a second period
    dataMeantmp = array(rep(NA, nbVar*nCode),
                        dim=c(nbVar, nCode))

    # For all period of breaking analysis
    for (j in 1:nPeriod_mean) {
        # For all the code
        for (k in 1:nCode) {
            # Gets the code
            code = Code[k]
            # For all variable
            for (i in 1:nbVar) {
                # Extracts the data corresponding to
                # the current variable
                df_data = list_df2plot[[i]]$data
                # Extract the variable of the plot
                var = list_df2plot[[i]]$var
                # Extract the type of the variable to plot
                type = list_df2plot[[i]]$type
                # Extracts the data corresponding to the code
                df_data_code = df_data[df_data$code == code,] 
                
                # Get the current start and end of the sub period
                Start_mean = mean_period[[j]][1]
                End_mean = mean_period[[j]][2]
                
                # Extract the data corresponding to this sub period
                df_data_code_per =
                    df_data_code[df_data_code$Date >= Start_mean 
                                 & df_data_code$Date <= End_mean,]
                
                # Min max for the sub period
                Datemin = min(df_data_code_per$Date)
                Datemax = max(df_data_code_per$Date)

                # Mean of the flow over the sub period
                dataMean = mean(df_data_code_per$Value,
                                na.rm=TRUE)

                # If this in not the first period
                if (j > 1) {
                    # Compute the difference of mean
                    Break = dataMean - dataMeantmp[i, k]
                    # Otherwise for the first period
                } else {
                    # Stocks NA
                    Break = NA
                }

                # If it is a flow variable
                if (type == 'sévérité') {
                    # Normalises the break by the mean of the
                    # initial period
                    breakValue = Break / dataMeantmp[i, k]
                    # If it is a date variable
                } else if (type == 'saisonnalité' | type == 'climatique') {
                    # Just stocks the break value
                    breakValue = Break
                }
                
                # Stores the result
                breakValue_code[j, i, k] = breakValue
                # Stores temporarily the mean of the current period
                dataMeantmp[i, k] = dataMean
            }
        }
    }
    # Computes the min and the max of the averaged trend for
    # all the station
    minBreakValue = apply(breakValue_code, c(1, 2),
                          min, na.rm=TRUE)
    maxBreakValue = apply(breakValue_code, c(1, 2),
                          max, na.rm=TRUE)

    res = list(min=minBreakValue, max=maxBreakValue, value=breakValue_code)
    return (res)
}
