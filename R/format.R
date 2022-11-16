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
        datatmp = list_data[[i]]
        df_metatmp = list_meta[[i]]
        from = list_from[[i]]
        
        if (!is.null(datatmp)) {
            
            datatmp = datatmp[!(datatmp$Code %in% Code),]
            df_metatmp = df_metatmp[!(df_metatmp$Code %in% Code),]
            Code = c(Code,
                     df_metatmp$Code[!(df_metatmp$Code %in% Code)])
            
            df_metatmp$source = from
            
            if (Blank) {
                data = datatmp
                df_meta = df_metatmp
                Blank = FALSE
            } else {
                # Joins tibble
                data = full_join(data, datatmp)
                df_meta = full_join(df_meta, df_metatmp)
            }
        }
    }
    # If there is no data
    if (Blank) {
        stop('No data')
    }
    return (list(data=data, meta=df_meta))
}



## 3. FOLLOWING OF DATA MODIFICATIONS ________________________________
#' @title Add modification info
#' @export
add_mod = function (df_mod, Code, type, fun_name, comment, df_meta=NULL) {
    
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = rle(data$Code)$value
    }
    
    for (code in Code) {
        df_modtmp = tibble(Code=code, type=type,
                           fun_name=fun_name,
                           comment=comment)
        df_mod = bind_rows(df_mod, df_modtmp)
    }
    return (df_mod)
}


sourceProcess = function (path, default=NULL) {
    assign("ASHES", new.env(), envir=.GlobalEnv)
    source(path, encoding='UTF-8')
    lsASHES = ls(envir=ASHES)
    
    Process_def = lsASHES[grepl("P[.]", lsASHES)]
    Process = lapply(Process_def, get, envir=ASHES)
    names(Process) = gsub("P[.]", "", Process_def)
    Process = list(Process)
    names(Process) = "P"
    
    if (!is.null(default)) {
        nOK = !(names(default$P) %in% names(Process$P))
        Process$P = append(Process$P, default$P[nOK])
    }
    
    process_allAtt = lsASHES[grepl("P[[:digit:]][.]", lsASHES)]
    process_allNames = str_extract(process_allAtt, "P[[:digit:]]")
    process_names = process_allNames[!duplicated(process_allNames)]
    Nprocess = length(process_names)

    for (i in 1:Nprocess) {
        process_name = paste0("P", i)
        IDprocess = grepl(paste0(process_name, "[.]"),
                          process_allAtt)

        process_att = process_allAtt[IDprocess]
        process = lapply(process_att, get, envir=ASHES)
        
        names(process) = gsub("P[[:digit:]][.]", "",
                              process_att)
        process = list(process)
        names(process) = process_name

        if (!is.null(default)) {
            nOK = !(names(default$P1) %in%
                    names(process[[process_name]]))
            process[[process_name]] =
                append(process[[process_name]], default$P1[nOK])
        }
        Process = append(Process, process)
    }

    rm (list=ls(envir=ASHES), envir=ASHES)
    return (Process)
}
