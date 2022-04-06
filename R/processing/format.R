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
# Rcode/processing/format.R
#
# Manages all the format problem of data and info. Mainly problem of
# input and output of the 'StatsAnalysisTrend' package. It also allows
# to join different selections of station and to gets exact period of
# trend analysis.


# Usefull library
library(dplyr)
library(Hmisc)


## 1. BEFORE TREND ANALYSE ___________________________________________
### 1.1. Joining selection ___________________________________________
# Joins tibbles of different selection of station as a unique one
join_selection = function (list_data, list_meta, list_from) {

    nb_selec = length(list_data)
    Blank = TRUE

    Code = c()
    for (i in 1:nb_selec) {
        df_datatmp = list_data[[i]]
        df_metatmp = list_meta[[i]]
        from = list_from[[i]]
        
        if (!is.null(df_datatmp)) {
            
            df_datatmp = df_datatmp[!(df_datatmp$code %in% Code),]
            df_metatmp = df_metatmp[!(df_metatmp$code %in% Code),]
            Code = c(Code,
                     df_metatmp$code[!(df_metatmp$code %in% Code)])
            
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


### 1.2. Local correction of data ____________________________________
flag_data = function (df_data, df_meta, df_flag, Code=NULL, df_mod=NULL) {

    if (is.null(Code)) {
        # Get all different stations code
        Code = levels(factor(df_meta$code))
        nCode = length(Code)
    } else {
        nCode = length(Code)
    }
 
    for (code in Code) {
        if (code %in% df_flag$code) {

            df_flag_code = df_flag[df_flag$code == code,]
            nbFlag = nrow(df_flag_code)

            for (i in 1:nbFlag) {
                newValue = df_flag_code$newValue[i]
                date = df_flag_code$Date[i]
                OKcor = df_data$code == code & df_data$Date == date
                oldValue = df_data$Value[OKcor]
                df_data$Value[OKcor] = newValue

                if (!is.null(df_mod)) {
                    df_mod =
                        add_mod(df_mod, code,
                                type='Value correction',
                                fun_name='Manual new value assignment',
                                comment=paste('At ', date,
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

### 1.3. Manages missing data ________________________________________
missing_data = function (df_data, df_meta, dayLac_lim=3, yearNA_lim=10, perStart='01-01', Code=NULL, df_mod=NULL) {

    if (is.null(Code)) {
        # Get all different stations code
        Code = levels(factor(df_meta$code))
        nCode = length(Code)
    } else {
        nCode = length(Code)
    }

    for (code in Code) {        
        # Extracts the data corresponding to the code
        df_data_code = df_data[df_data$code == code,]

        DateNA = df_data_code$Date[is.na(df_data_code$Value)]

        dDateNA = diff(DateNA)
        if (any(dDateNA != 1)) {
            
            dDateNA = c(10, dDateNA)
            idJump = which(dDateNA != 1)
            NJump = length(idJump)

            for (i in 1:NJump) {
                idStart = idJump[i]
                
                if (i < NJump) {
                    idEnd = idJump[i+1] - 1
                } else {
                    idEnd = length(DateNA)
                }

                Start = DateNA[idStart]
                End = DateNA[idEnd]

                duration = (End - Start)/365.25
                if (duration >= yearNA_lim) {
                    df_data_code$Value[df_data_code$Date <= End] = NA
                    
                    if (!is.null(df_mod)) {
                        df_mod =
                            add_mod(df_mod, code,
                                    type='Missing data management',
                                    fun_name='NA assignment',
                                    comment=paste('From the start of measurements',
                                                  ' to ', End, sep=''))
                    }
                }
            }
        }

        DateMD = format(df_data_code$Date, "%m-%d")
        idperStart = which(DateMD == perStart)

        if (DateMD[1] != perStart) {
            idperStart = c(1, idperStart)
        }
        NidperStart = length(idperStart)

        for (i in 1:NidperStart) {
            Start = df_data_code$Date[idperStart[i]]
            if (i < NidperStart) {
                End = df_data_code$Date[idperStart[i+1] - 1]
            } else {
                End = df_data_code$Date[length(df_data_code$Date)]
            }
            
            OkYear = df_data_code$Date >= Start & df_data_code$Date <= End
            df_data_code_year = df_data_code[OkYear,]

            StartReal = as.Date(paste(substr(Start, 1, 4),
                                      perStart, sep='-'))
            EndReal = as.Date(paste(as.numeric(substr(Start, 1, 4)) + 1,
                                    perStart, sep='-'))
            
            nbDate = as.numeric(difftime(EndReal, StartReal,
                                         units="days"))
                        
            nbNA = sum(as.numeric(is.na(df_data_code_year$Value)))
            nbNA = nbNA + abs(as.numeric(difftime(StartReal, Start,
                                                  units="days")))
            nbNA = nbNA + abs(as.numeric(difftime(EndReal, End+1,
                                                  units="days")))

            yearLacMiss_pct = nbNA/nbDate * 100

            if (nbNA > dayLac_lim) {
                df_data_code_year$Value = NA
                df_data_code[OkYear,] = df_data_code_year

                if (!is.null(df_mod)) {
                    df_mod = add_mod(df_mod, code,
                                     type='Missing data management',
                                     fun_name='NA assignment',
                                     comment=paste('From ', Start,
                                                   ' to ', End, sep=''))
                }
                
            } else if (nbNA <= dayLac_lim & nbNA > 1) {
                DateJ = as.numeric(df_data_code_year$Date)
                Value = df_data_code_year$Value
               
                Value = approxExtrap(x=DateJ,
                                     y=Value,
                                     xout=DateJ,
                                     method="linear",
                                     na.rm=TRUE)$y                
                df_data_code$Value[OkYear] = Value

                if (!is.null(df_mod)) {
                    df_mod = add_mod(df_mod, code,
                                     type='Missing data management',
                                     fun_name='approxExtrap',
                                     comment=paste(
                                         'Linear extrapolation of NA from ',
                                         Start, ' to ', End, sep=''))
                }
            }
        }
        df_data[df_data$code == code,] = df_data_code        
    }
    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data)
    }
}

### 1.4. Sampling of the data ________________________________________
sampling_data = function (df_data, df_meta, sampleSpan=c('05-01', '11-30'), Code=NULL, df_mod=NULL) {

    if (is.null(Code)) {
        # Get all different stations code
        Code = levels(factor(df_meta$code))
        nCode = length(Code)
    } else {
        nCode = length(Code)
    }

    # 1972 is leap year reference is case of leap year comparison
    sampleStart = as.Date(paste('1972', sampleSpan[1], sep='-'))
    sampleEnd = as.Date(paste('1972', sampleSpan[2], sep='-'))

    DateMD = format(df_data$Date, "%m-%d")
    Date = paste('1972', DateMD, sep='-')
    
    df_data$Value[Date < sampleStart | Date > sampleEnd] = NA

    if (!is.null(df_mod)) {
        for (code in Code) {
            df_mod = add_mod(df_mod, code,
                             type='Seasonal sampling ',
                             fun_name='NA assignment',
                             comment=paste('Before ', sampleStart,
                                           ' and after ', sampleEnd,
                                           sep=''))
        }
    }
  
    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data)
    }
}


## 2. DURING TREND ANALYSE ___________________________________________
date_correction = function (df_XEx, per) {

    # Takes the first date as example
    exDate = df_XEx$datetime[1]
    # Finds the number of dash in the date
    nbt = lengths(regmatches(exDate, gregexpr('-', exDate)))

    # If there is only one dash
    if (nbt == 1) {
        # Converts it to date from a year and a month
        Date = paste(df_XEx$datetime, '01', sep='-')
    # If there is no dash
    } else if (nbt == 0) {
        # Converts it to date from only a year
        Date = paste(df_XEx$datetime, '01', '01', sep='-')
    # If there is more than 2 dashes
    } else if (nbt != 2) {
        # This is not a classical date
        stop('erreur of date format')
    }

    Start = per[1]
    End = per[2]
    
    df_XEx = df_XEx[Date >= Start & Date <= End,]

    return (df_XEx)
}


### 2.1. Preparation _________________________________________________
# Prepares the data in order to have a list of a data tibble with
# date, group and flow column and a info tibble with the station code
# and group column to fit the entry of the 'extract.Var' function in
# the 'StatsAnalysisTrend' package
prepare = function(df_data, colnamegroup=NULL) {
    
    # Forces the column name to group to be a vector 
    colnamegroup = c(colnamegroup)
    # Converts it to index of the column to group
    colindgroup = which(colnames(df_data) == colnamegroup)
    # Groups the data by those indexes
    df_data = group_by_at(df_data, colindgroup)

    # Creates a new tibble of data with a group column
    data = tibble(Date=df_data$Date, 
                  group=group_indices(df_data),
                  Value=df_data$Value)
    
    # Gets the different value of the group
    Gkey = group_keys(df_data)
    # Creates a new tibble of info of the group
    info = bind_cols(group=seq(1:nrow(Gkey)),
                     Gkey)

    # Stores data and info tibble as a list that match the entry of
    # the 'extract.Var' function
    res = list(data=data, info=info)
    return (res)
}

### 2.2. Re-preparation ______________________________________________
# Re-prepares the data in outing of the 'extract.Var' function in
# the 'StatsAnalysisTrend' package in order to fit again to the
# entry of the same function
reprepare = function(df_XEx, df_Xlist, colnamegroup=NULL) {
    
    # Changes the column name of the results of the
    # 'extract.Var' function
    colnames(df_XEx) = c('Date', 'group', 'Value')

    # Converts Date column as character
    df_XEx$Date = as.character(df_XEx$Date)
    # Takes the first date as example
    exDate = df_XEx$Date[1]
    # Finds the number of dash in the date
    nbt = lengths(regmatches(exDate, gregexpr('-', exDate)))

    # If there is only one dash
    if (nbt == 1) {
        # Converts it to date from a year and a month
        df_XEx$Date = paste(df_XEx$Date, '01', sep='-')
    # If there is no dash
    } else if (nbt == 0) {
        # Converts it to date from only a year
        df_XEx$Date = paste(df_XEx$Date, '01', '01', sep='-')
    # If there is more than 2 dashes
    } else if (nbt != 2) {
        # This is not a classical date
        stop('erreur of date format')
    }
    
    # Recreates the outing of the 'extract.Var' function nicer
    df_XEx = bind_cols(Date=as.Date(df_XEx$Date,
                                       format="%Y-%m-%d"),
                       df_XEx[-1],
                       df_Xlist$info[df_XEx$group,
                                     2:ncol(df_Xlist$info)])
    
    # Prepares the nicer outing
    df_XlistEx = prepare(df_XEx, colnamegroup=colnamegroup)
    return (df_XlistEx)
}

### 2.3. Prepare date ________________________________________________
prepare_date = function(df_XEx, df_Xlist, per.start="01-01") {

    df_dateStart = summarise(group_by(df_Xlist$data, group),
                                Date=min(Date))
    # filter(group_by(df_Xlist$data, group), Date == min(Date))
    
    df_dateStart$Date_julian = NA
    df_dateStart$DateHydro_julian = NA
    
    date = as.Date(df_dateStart$Date)
    
    date_per.start = as.Date(paste(substr(date, 1, 4),
                                   '-', per.start, sep=''))
    
    date[date < date_per.start] = date_per.start[date < date_per.start]
    df_dateStart$Date = date
    
    origin = as.Date(paste(format(df_dateStart$Date, "%Y"),
                           '-', per.start, sep=''))

    # originHydro = as.Date(paste(format(df_dateStart$Date, "%Y"),
                                # '-01-01', sep=''))

    for (i in 1:nrow(df_dateStart)) {
        dateJultmp = julian(date[i], origin=origin[i])
        df_dateStart$Date_julian[i] = dateJultmp

        # print(date[i])
        # dateJulHydrotmp = julian(date[i], origin=originHydro[i])
        # df_dateStart$DateHydro_julian[i] = dateJulHydrotmp
    }

    df_dateStart$Year = format(df_dateStart$Date, "%Y")
    
    for (group in df_dateStart$group) {
        
        Ok_dateStart = df_dateStart$group == group
        Shift = df_dateStart$Date_julian[Ok_dateStart]
        year = df_dateStart$Year[Ok_dateStart]
        OkXEx_code_year = df_XEx$group1 == group & df_XEx$datetime == year
        df_XEx$values[OkXEx_code_year] =
            df_XEx$values[OkXEx_code_year] + Shift

        # OkXEx_code = df_XEx$group1 == group

        # ShiftHydro = df_dateStart$DateHydro_julian[Ok_dateStart]
        # df_XEx$values[OkXEx_code] = df_XEx$values[OkXEx_code] + ShiftHydro
        
        ## Add 365 when the point is too remote
        # XEx_code = df_XEx$values[OkXEx_code]
        # meanXEx_code = mean(XEx_code, na.rm=TRUE)
        # dXEx_code = meanXEx_code - XEx_code
        # stdXEx_code = sd(XEx_code, na.rm=TRUE)
        # OkOverStd = dXEx_code >= stdXEx_code*3
        # OkOverStd[is.na(OkOverStd)] = FALSE
        # XEx_code[OkOverStd] = XEx_code[OkOverStd] + 365
        # df_XEx$values[OkXEx_code] = XEx_code

        # print(group)
        # print(df_XEx$datetime[df_XEx$group1 == group][dXEx_code >= stdXEx_code*3])
        
    }

    df_XEx$datetime = as.double(df_XEx$datetime)
    
    return (df_XEx)
}


## 3. AFTER TREND ANALYSE ____________________________________________
### 3.1. Period of trend _____________________________________________
# Compute the start and the end of the period for a trend analysis
# according to the accessible data 
get_period = function (per, df_Xtrend, df_XEx, df_Xlist, per.start='01-01') {

    # Converts results of trend to tibble
    df_Xtrend = tibble(df_Xtrend)
    # Fix the period start and end of the accessible period to a
    # default date
    df_Xtrend$period_start = as.Date("1970-01-01")
    df_Xtrend$period_end = as.Date("1970-01-01")

    # print(df_XEx)
    # print(head(df_XEx))
    # print(tail(df_XEx))
    # print('')

    df_data = df_Xlist$data
    df_info = df_Xlist$info
    
    # For all the different group
    for (g in df_info$group) {
        # Gets the analyse data associated to the group
        df_data_code = df_data[df_data$group == g,]
        # Gets the id in the trend result associated to the group
        id = which(df_Xtrend$group1 == g)
        
        Date = df_data_code$Date
        Date = df_data_code$Date
        
        iStart = which.min(abs(Date - as.Date(per[1])))
        iEnd = which.min(abs(Date - as.Date(per[2])))

        # Stores the start and end of the trend analysis
        df_Xtrend$period_start[id] = as.Date(Date[iStart])
        df_Xtrend$period_end[id] = as.Date(Date[iEnd])
    }
    return (df_Xtrend)
}

### 3.2. Cleaning ____________________________________________________
# Cleans the trend results of the function 'Estimate.stats' in the
# 'StatsAnalysisTrend' package. It adds the station code and the
# intercept of the trend to the trend results. Also makes the data
# more presentable.
clean = function (df_Xtrend, df_XEx, df_Xlist) {

    # Reprepares the list of data and info in order to be presentable
    df_Xlist = reprepare(df_XEx, df_Xlist, colnamegroup=c('code'))
    
    # Adds a column of station code
    df_Xlist$data$code = NA
    # For all the group
    for (g in df_Xlist$info$group) {
        # Adds the station code corresponding to each group info
        df_Xlist$data$code[which(df_Xlist$data$group == g)] = df_Xlist$info$code[df_Xlist$info$group == g]
    }

    # Adds the info to trend tibble
    df_Xtrend = bind_cols(df_Xtrend,
                          df_Xlist$info[df_Xtrend$group1,
                                        2:ncol(df_Xlist$info)])

    # Renames the column of group of trend results
    colnames(df_Xtrend)[1] = 'group'
    # Adds the intercept value of trend
    df_Xtrend = get_intercept(df_Xtrend, df_Xlist, unit2day=365.25)

    # Changes the position of the intercept column
    df_Xtrend = relocate(df_Xtrend, intercept, .after=trend)

    # Creates a list of results to return
    res = list(trend=df_Xtrend, data=df_Xlist$data)
    return (res)
}

## 4. OTHER __________________________________________________________
add_mod = function (df_mod, Code, type, fun_name, comment, df_meta=NULL) {
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = levels(factor(df_meta$code))
    }
    
    for (code in Code) {
        df_modtmp = tibble(code=code, type=type,
                           fun_name=fun_name,
                           comment=comment)
        df_mod = bind_rows(df_mod, df_modtmp)
    }
    return (df_mod)
}
