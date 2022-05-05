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

    print('Checking of flags')
    
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
missing_year = function (df_data, df_meta, yearNA_lim=10, Code=NULL, df_mod=NULL) {

    print('Checking for missing years')
    
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
        df_data[df_data$code == code,] = df_data_code        
    }
    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data)
    }
}


missing_day = function (df_data, df_meta, dayLac_lim=3, perStart='01-01', Code=NULL, df_mod=NULL) {

    print('Checking for missing days')

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


NA_filter = function (df_XEx, NA_pct_lim=1, df_mod=NULL) {

    filter = df_XEx$NA_pct > NA_pct_lim
    
    df_XEx$Value[filter] = NA
    codeFilter = df_XEx$code[filter]
    dateFilter = format(df_XEx$Date[filter], "%Y")
    Nmod = length(codeFilter)
    
    if (!is.null(df_mod)) {
        for (i in 1:Nmod) {
            df_mod =
                add_mod(df_mod, codeFilter[i],
                        type='Filtering of NA percentage after Extraction',
                        fun_name='NA assignment',
                        comment=paste0('Removal of year ', dateFilter[i]))
        }
    }    
    if (!is.null(df_mod)) {
        res = list(data=df_XEx, mod=df_mod)
        return (res)
    } else {
        return (df_XEx)
    }
}

### 1.4. Sampling of the data ________________________________________
sampling_data = function (df_data, df_meta, sampleSpan=c('05-01', '11-30'), Code=NULL, df_mod=NULL) {

    print('Sampling of the data')
    
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
    DateRef = paste('1972', DateMD, sep='-')
    
    df_data$Value[DateRef < sampleStart | DateRef > sampleEnd] = NA

    # df_data$DateRef = DateRef
    # df_data = mutate(.data=df_data,
    #                  Value=
    #                      replace(Value,
    #                              DateRef < sampleStart | DateRef > sampleEnd,
    #                              NA))
    # df_data = select(df_data, -DateRef)
    
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
extract_Var_WRAP = function (df_data, funct, period, perStart,
                             timestep, isDate=FALSE, ...) {

    print('Extraction of data')
    
    # Groups the data by code column
    df_data = group_by(df_data, code)

    # Creates a new tibble of data with a group column
    data = tibble(Date=df_data$Date, 
                  group=group_indices(df_data),
                  Value=df_data$Value,
                  Na.percent=df_data$Na.percent)
    
    # Gets the different value of the group
    Gkey = group_keys(df_data)
    # Creates a new tibble of info of the group
    info = bind_cols(group=seq(1:nrow(Gkey)),
                     Gkey)

    # Stores data and info tibble as a list that match the entry of
    # the 'extract.Var' function
    df_Xlist = list(data=data, info=info)

    # print(df_Xlist)
    # print(tail(data))
    
    df_XEx = extract.Var(data.station=df_Xlist,
                         funct=funct,
                         period=period,
                         per.start=perStart,
                         timestep=timestep,
                         pos.datetime=1,
                         ...)

    # print(df_XEx)
    # print(sum(!is.na(df_XEx$values)))
    # print(tail(df_XEx))
    
    colnames(df_XEx) = c('Date', 'group', 'Value', 'NA_pct')
    df_XEx$Date = as.Date(paste0(df_XEx$Date, '-', perStart))

    # print(df_XEx)
    
    # Recreates the outing of the 'extract.Var' function nicer
    df_XEx = tibble(Date=df_XEx$Date,
                    Value=df_XEx$Value,
                    code=df_Xlist$info$code[df_XEx$group],
                    NA_pct=df_XEx$NA_pct*100)

    # print(df_XEx)
    # print(sum(!is.na(df_XEx$Value)))
    

    if (isDate) {
        # Converts index of the tCEN to the julian date associated
        df_XEx = convert_dateEx(df_XEx, df_data, perStart=perStart)
    }
    
    # print(df_XEx)
    
    return (df_XEx)
}


Estimate_stats_WRAP = function (df_XEx, alpha, period, dep_option='AR1') {

    print('Estimation of trend')
    
    df_XEx = group_by(df_XEx, code)

    # print(df_XEx)
    
    df_XEx_RAW = tibble(datetime=as.numeric(format(df_XEx$Date, "%Y")),
                        group1=group_indices(df_XEx),
                        values=df_XEx$Value,
                        Na.percent=df_XEx$NA_pct/100)

    # print(df_XEx_RAW)
    
    # Gets the different value of the group
    Gkey = group_keys(df_XEx)
    # Creates a new tibble of info of the group
    info = bind_cols(group=seq(1:nrow(Gkey)),
                     Gkey)

    df_Xtrend = Estimate.stats(data.extract=df_XEx_RAW,
                               level=alpha,
                               dep.option=dep_option)

    
    # Converts results of trend to tibble
    df_Xtrend = tibble(df_Xtrend)

    colnames(df_Xtrend)[1] = 'group'

    df_Xtrend = tibble(code=info$code[df_Xtrend$group],
                       df_Xtrend[-1])
        
    df_Xtrend = get_intercept(df_Xtrend, df_XEx)
    
    # Specify the period of analyse
    df_Xtrend = get_period(df_Xtrend, df_XEx)
    
    return (df_Xtrend)
}


### 2.3. Prepare date ________________________________________________
convert_dateEx = function(df_XEx, df_data, perStart="01-01") {

    Shift_perStart = as.integer(df_XEx$Date - as.Date(paste0(format(df_XEx$Date, "%Y"), "-01-01")))
    df_XEx$Value = df_XEx$Value + Shift_perStart
    
    df_Date = summarise(group_by(df_data, code),
                                Start=min(Date, na.rm=TRUE))
    df_Date$Julian = NA
    df_Date$origin = as.Date(paste0(format(df_Date$Start, "%Y"),
                                    '-', perStart))
    for (i in 1:nrow(df_Date)) {
        df_Date$Julian[i] = julian(df_Date$Start[i], origin=df_Date$origin[i])
    }

    df_Date$Year = format(df_Date$Start, "%Y")

    for (code in df_Date$code) {
        Ok_Start = df_Date$code == code
        Shift = df_Date$Julian[Ok_Start]
        year = df_Date$Year[Ok_Start]
        OkXEx_code_year =
            df_XEx$code == code & format(df_XEx$Date, "%Y") == year        
        df_XEx$Value[OkXEx_code_year] =
            df_XEx$Value[OkXEx_code_year] + Shift

        
        # df_XEx_code = df_XEx[df_XEx$code == code,]

        # print(df_XEx_code)
        
        # mean_code = mean(df_XEx_code$Value, na.rm=TRUE)

        # print(mean_code*4/3)
        
        # Out = df_XEx_code$Value >= mean_code*4/3
        # Out[is.na(Out)] = FALSE
        # df_XEx_code$Value[Out] = df_XEx_code$Value[Out] - 365
        # df_XEx[df_XEx$code == code,] = df_XEx_code

        # print(df_XEx_code)
    }

    Year = format(df_XEx$Date, "%Y")
    Start = as.Date(paste0(Year, '-', perStart))
    End = Start + years(1) - days(1)
    nbDate = as.numeric(difftime(End, Start,
                                 units="days"))
    print(nbDate)

    print(df_XEx)
    
    df_XEx$Value = df_XEx$Value %% nbDate

    print(df_XEx)
    
    return (df_XEx)
}


## 3. AFTER TREND ANALYSE ____________________________________________
### 3.1. Period of trend _____________________________________________
# Compute the start and the end of the period for a trend analysis
# according to the accessible data 
get_period = function (df_Xtrend, df_XEx) {

    df_Start = summarise(group_by(df_XEx, code),
                         Start=min(Date, na.rm=TRUE))
    
    df_End = summarise(group_by(df_XEx, code),
                       End=max(Date, na.rm=TRUE))
    
    df_Xtrend$period_start = df_Start$Start
    df_Xtrend$period_end = df_End$End
    
    return (df_Xtrend)
}


### 3.2. Intercept of trend __________________________________________
# Compute intercept values of linear trends with first order values
# of trends and the data on which analysis is performed.
get_intercept = function (df_Xtrend, df_XEx, unit2day=365.25) {

    df_mu_X = summarise(group_by(df_XEx, code),
                        mu_X=mean(Value, na.rm=TRUE))

    df_mu_t = summarise(group_by(df_XEx, code),
                        mu_t=as.numeric(mean(Date, na.rm=TRUE)) / unit2day)

    df_Xtrendtmp = tibble(code=df_Xtrend$code,
                          trend=df_Xtrend$trend,
                          mu_X=df_mu_X$mu_X,
                          mu_t=df_mu_t$mu_t)
    
    df_b = summarise(group_by(df_Xtrendtmp, code),
                     b=mu_X - mu_t * trend)
    
    df_Xtrend$intercept = df_b$b
    
    return (df_Xtrend)
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
