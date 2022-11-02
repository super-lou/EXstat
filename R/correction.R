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
# R/processing/correction.R
#
# Manages all possible corrections that can be performed on the data
# before trend analysis.


## 1. LOCAL CORRECTION OF DATA _______________________________________
#' @title Flag data
#' @export
flag_data = function (df_data, df_flag, df_mod=NULL,
                      verbose=TRUE) {

    if (verbose) {
        print('.. Checking of flags')
    }

    Code = rle(df_data$Code)$value

    for (code in Code) {
        if (code %in% df_flag$Code) {

            df_flag_code = df_flag[df_flag$Code == code,]
            nbFlag = nrow(df_flag_code)

            for (i in 1:nbFlag) {
                newValue = df_flag_code$newValue[i]
                flagDate = as.Date(df_flag_code$Date[i])
                OKcor = df_data$Code == code & df_data$Date == flagDate
                oldValue = df_data$Value[OKcor]
                df_data$Value[OKcor] = newValue

                if (!is.null(df_mod)) {
                    df_mod =
                        add_mod(df_mod, code,
                                type='Value correction',
                                fun_name='Manual new value assignment',
                                comment=paste('At ', flagDate,
                                              ' the value ', oldValue,
                                              ' becomes ', newValue,
                                              sep=''))
                }
            }  
        }
    }
    
    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data)
    }
}

