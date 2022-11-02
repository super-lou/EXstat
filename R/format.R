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
