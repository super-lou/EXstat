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
#' @title Extremes
#' @export
get_valueExtremes = function (list_df2plot, Code, nPeriod,
                              nbVar, nCode, valueType="trend",
                              colorForce=FALSE,
                              minQprob=0, maxQprob=1) {
    
    # Blank array to store mean of the trend for each
    # station, perdiod and variable
    Value_code = array(rep(1, nPeriod*nbVar*nCode),
                       dim=c(nPeriod, nbVar, nCode))

    if (valueType == "break") {
        dataMeantmp = array(rep(NA, nbVar*nCode),
                            dim=c(nbVar, nCode))
    }

    # For all the period
    for (j in 1:nPeriod) {
        # For all the code
        for (k in 1:nCode) {
            # Gets the code
            code = Code[k]
            
            for (i in 1:nbVar) {
                # Extracts the type of the variable
                type = list_df2plot[[i]]$type
                # Extracts the data corresponding to the
                # current variable
                df_data = list_df2plot[[i]]$data
                # Extracts the data corresponding to the code
                df_data_code = df_data[df_data$code == code,]

                if (valueType == "break") {
                    # Get the current start and end of the sub period
                    Start = mean_period[[j]][1]
                    End = mean_period[[j]][2]

                }
                
                if (valueType == "trend") {
                    # Extracts the trend corresponding to the
                    # current variable
                    df_trend = list_df2plot[[i]]$trend
                    df_trend_code = df_trend[df_trend$code == code,]
                                    # Extract start and end of trend periods
                    Start = df_trend_code$period_start[j]
                    End = df_trend_code$period_end[j]
                    # Same for trend
                    df_trend_code_per = 
                        df_trend_code[df_trend_code$period_start == Start 
                                      & df_trend_code$period_end == End,]
                }
                
                # Extracts the corresponding data for the period
                df_data_code_per =
                    df_data_code[df_data_code$Date >= Start 
                                 & df_data_code$Date <= End,]

                if (valueType == "break") {
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
                    if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Normalises the break by the mean of the
                        # initial period
                        value = Break / dataMeantmp[i, k]
                        # If it is a date variable
                    } else if (unit == "jour" | unit == "jour de l'année") {
                        # Just stocks the break value
                        value = Break
                    }
                    
                    # Stores the result
                    Value_code[j, i, k] = value
                    # Stores temporarily the mean of the current period
                    dataMeantmp[i, k] = dataMean
                }

                if (valueType == "trend") {
                    # Computes the number of trend analysis selected
                    Ntrend = nrow(df_trend_code_per)
                    # If there is more than one trend on the same period
                    if (Ntrend > 1) {
                        # Takes only the first because they are similar
                        df_trend_code_per = df_trend_code_per[1,]
                    }
                    
                    # If it is a flow variable
                    if (unit == 'm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Computes the mean of the data on the period
                        dataMean = mean(df_data_code_per$Value, na.rm=TRUE)
                        # Normalises the trend value by the mean of the data
                        value = df_trend_code_per$trend / dataMean
                        # If it is a date variable
                    } else if (unit == "jour" | unit == "jour de l'année") {
                        value = df_trend_code_per$trend
                    }
                    
                    # If the p value is under the threshold
                    if (df_trend_code_per$p <= alpha | colorForce) {
                        # Stores the mean trend
                        Value_code[j, i, k] = value
                        # Otherwise
                    } else {
                        # Do not stocks it
                        Value_code[j, i, k] = NA
                    }
                }
            }
        }
    }

    # Computes the min and the max of the averaged trend for
    # all the station
    minValue = apply(Value_code, c(1, 2),
                          quantile, probs=minQprob, na.rm=TRUE)
    maxValue = apply(Value_code, c(1, 2),
                          quantile, probs=maxQprob, na.rm=TRUE)
    res = list(value=Value_code, min=minValue, max=maxValue)
    return (res)
}
