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
                              minXprob=0, maxXprob=1) {
    
    # Blank array to store mean of the trend for each
    # station, perdiod and variable
    X_code = array(rep(1, nPeriod*nbVar*nCode),
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
                unit = list_df2plot[[i]]$unit
                level = list_df2plot[[i]]$level
                # Extracts the data corresponding to the
                # current variable
                data = list_df2plot[[i]]$data
                # Extracts the data corresponding to the code
                data_code = data[data$Code == code,]

                if (valueType == "break") {
                    # Get the current start and end of the sub period
                    Start = mean_period[[j]][1]
                    End = mean_period[[j]][2]

                }
                
                if (valueType == "trend") {
                    # Extracts the trend corresponding to the
                    # current variable
                    df_trend = list_df2plot[[i]]$trend
                    df_trend_code = df_trend[df_trend$Code == code,]
                                    # Extract start and end of trend periods
                    Start = df_trend_code$start[j]
                    End = df_trend_code$end[j]
                    # Same for trend
                    df_trend_code_per = 
                        df_trend_code[df_trend_code$start == Start 
                                      & df_trend_code$end == End,]
                }
                
                # Extracts the corresponding data for the period
                data_code_per =
                    data_code[data_code$Date >= Start 
                                 & data_code$Date <= End,]

                if (valueType == "break") {
                    # Min max for the sub period
                    Datemin = min(data_code_per$Date)
                    Datemax = max(data_code_per$Date)

                    # Mean of the flow over the sub period
                    dataMean = mean(data_code_per$X,
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
                    if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Normalises the break by the mean of the
                        # initial period
                        value = Break / dataMeantmp[i, k]
                        # If it is a date variable
                    } else if (unit == "jour" | unit == "jour de l'année" | unit == 'jour.an^{-1}') {
                        # Just stocks the break value
                        value = Break
                    }
                    
                    # Stores the result
                    X_code[j, i, k] = value
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
                    if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Computes the mean of the data on the period
                        dataMean = mean(data_code_per$X, na.rm=TRUE)
                        # Normalises the trend value by the mean of the data
                        value = df_trend_code_per$a / dataMean
                        # If it is a date variable
                    } else if (unit == "jour" | unit == "jour de l'année" | unit == 'jour.an^{-1}') {
                        value = df_trend_code_per$a
                    }

                    # If the p value is under the threshold
                    if (df_trend_code_per$p <= level | colorForce) {
                        # Stores the mean trend
                        X_code[j, i, k] = value
                        # Otherwise
                    } else {
                        # Do not stocks it
                        X_code[j, i, k] = NA
                    }
                }
            }
        }
    }

    # Computes the min and the max of the averaged trend for
    # all the station
    minX = apply(X_code, c(1, 2),
                     quantile, probs=minXprob, na.rm=TRUE)
    maxX = apply(X_code, c(1, 2),
                     quantile, probs=maxXprob, na.rm=TRUE)
    res = list(value=X_code, min=minX, max=maxX)
    return (res)
}


get_Nspace = function (data_code, unit, lim_pct, NspaceMax=NULL) {
    
    # If variable unit is date 
    if (unit == "jour de l'année") {
        # The number of digit is 6 because months are display
        # with 3 characters
        Nspace = 6
        if (!is.null(NspaceMax)) {
            accuracy = NULL
        }
        
    # If it is a flow variable
    } else if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}' | unit == 'm^{3/2}.s^{-1/2}' | unit == 'jour' | unit == 'jour.an^{-1}') {
        # Gets the max number of digit on the label
        maxtmp = max(data_code$X, na.rm=TRUE)

        # If the max is greater than 10
        if (get_power(maxtmp) >= 4) {
            Nspace = 12
            if (!is.null(NspaceMax)) {
                accuracy = NULL
            }
            
        } else if (maxtmp >= 10) {
            # The number of digit is the magnitude plus
            # the first number times 2
            Nspace = (get_power(maxtmp) + 1)*2
            # Plus spaces between thousands hence every 8 digits
            Nspace = Nspace + as.integer(Nspace/8)
            if (!is.null(NspaceMax)) {
                # The accuracy is 1
                accuracy = 1
            }

        # If the max is less than 10 and greater than 1
        } else if (maxtmp < 10 & maxtmp >= 1) {
            # The number of digit is the magnitude plus
            # the first number times 2 plus 1 for the dot
            # and 2 for the first decimal
            Nspace = (get_power(maxtmp) + 1)*2 + 3
            if (!is.null(NspaceMax)) {
                # The accuracy is 0.1
                accuracy = 0.1
            }
            
        # If the max is less than 1 (and obviously more than 0)
        } else if (maxtmp < 1) {
            # Fixes the number of significant decimals to 3
            maxtmp = signif(maxtmp, 3)
            # The number of digit is the number of character
            # of the max times 2 minus 1 for the dots that
            # count just 1 space
            Nspace = nchar(as.character(maxtmp))*2 - 3
            if (!is.null(NspaceMax)) {
                # Computes the accuracy
                accuracy = 10^(-nchar(as.character(maxtmp))+3)
            }
        }
        if (unit == 'm^{3/2}.s^{-1/2}') {
            Nspace = Nspace + 1
        }
    }
    
    if (!is.null(NspaceMax)) {
        # Gets the associated number of white space
        prefix = strrep(' ', times=NspaceMax - Nspace)
        res = list(Nspace=Nspace, prefix=prefix, accuracy=accuracy)
        return (res)
        
    } else {
        return (Nspace)
    }
}
