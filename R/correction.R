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
flag_data = function (data, flag, mod=NULL,
                      verbose=TRUE) {

    if (verbose) {
        print('Checking of flags')
    }

    Code = rle(data$Code)$value

    for (code in Code) {
        if (code %in% flag$Code) {

            flag_code = flag[flag$Code == code,]
            nbFlag = nrow(flag_code)

            for (i in 1:nbFlag) {
                newQ = flag_code$newQ[i]
                flagDate = as.Date(flag_code$Date[i])
                OKcor = data$Code == code & data$Date == flagDate
                oldQ = data$Q[OKcor]
                data$Q[OKcor] = newQ

                if (!is.null(mod)) {
                    mod =
                        add_mod(mod, code,
                                type='Q correction',
                                fun_name='Manual new value assignment',
                                comment=paste('At ', flagDate,
                                              ' the value ', oldQ,
                                              ' becomes ', newQ,
                                              sep=''))
                }
            }  
        }
    }
    
    if (!is.null(mod)) {
        res = list(data=data, mod=mod)
        return (res)
    } else {
        return (data)
    }
}

