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
flag_data = function (df_data, df_meta, df_flag, Code=NULL, df_mod=NULL,
                      verbose=TRUE) {

    if (verbose) {
        print('.. Checking of flags')
    }
    
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
                flagDate = as.Date(df_flag_code$Date[i])
                OKcor = df_data$code == code & df_data$Date == flagDate
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


## 2. MANAGES MISSING DATA ___________________________________________
### 2.1. Long missing period over several years ______________________
#' @title Long missing period over several years
#' @export
apply_missing_year = function (Date, Value, Code, yearNA_lim) {

    DateNA = Date[is.na(Value)]
    dDateNA = diff(DateNA)
    if (any(dDateNA != 1)) {
        
        dDateNA = c(10, dDateNA)
        idJump = which(dDateNA != 1)
        NJump = length(idJump)

        for (i in 1:NJump) {
            idStartNA = idJump[i]
            
            if (i < NJump) {
                idEndNA = idJump[i+1] - 1
            } else {
                idEndNA = length(DateNA)
            }

            StartNA = DateNA[idStartNA]
            EndNA = DateNA[idEndNA]

            duration = (EndNA - StartNA)/365.25
            if (duration >= yearNA_lim) {
                
                Start = min(Date, na.rm=TRUE)
                End = max(Date, na.rm=TRUE)
                
                Before = StartNA - Start
                After = End - EndNA
                if (Before < After) {
                    Value[Date <= StartNA] = NA
                    start = Start
                    end = StartNA
                } else {
                    Value[Date >= EndNA] = NA
                    start = EndNA
                    end = End
                }
            }
        }
    }
    res = tibble(Date=Date, Value=Value, code=Code)
    return (res)
}

#' @title Missing year
#' @export
missing_year = function (df_data, df_meta, yearNA_lim=10,
                         Code=NULL, df_mod=NULL,
                         verbose=TRUE) {

    if (verbose) {
        print('.. Checking for missing years')
    }
    
    df_Value = summarise(group_by(df_data, code),
                         apply_missing_year(Date,
                                            Value,
                                            Code,
                                            yearNA_lim),
                        .groups="drop")
    if (!is.null(df_mod)) {
        
        isCorr = is.na(df_Value$Value) != is.na(df_data$Value)
        CodeCorr = df_Value$code[isCorr]
        CodeCorr = CodeCorr[!duplicated(CodeCorr)]

        for (code in CodeCorr) {

            df_Value_code = df_Value[df_Value$code == code,]
            df_data_code = df_data[df_data$code == code,]

            isCorr_code = is.na(df_Value_code$Value) != is.na(df_data_code$Value)

            DateCorr_code = df_Value_code$Date[isCorr_code]

            start = min(DateCorr_code)
            end = max(DateCorr_code)
            
            df_mod =
                add_mod(df_mod, code,
                        type='Missing data management',
                        fun_name='NA assignment',
                        comment=paste('From ', start,
                                      ' of measurements',
                                      ' to ', end, sep=''))
        }
    }
    
    df_data$Value = df_Value$Value
    
    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data)
    }
}


### 2.2. Missing period over several days ____________________________
#' @title Missing period over several days
#' @export
missing_day = function (df_data, df_meta, dayLac_lim=3,
                        hydroYear='01-01', Code=NULL, df_mod=NULL,
                        verbose=TRUE) {
    if (verbose) {
        print('.. Checking for missing days')
    }
    
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
        idhydroYear = which(DateMD == hydroYear)

        if (DateMD[1] != hydroYear) {
            idhydroYear = c(1, idhydroYear)
        }
        NidhydroYear = length(idhydroYear)

        for (i in 1:NidhydroYear) {
            Start = df_data_code$Date[idhydroYear[i]]
            if (i < NidhydroYear) {
                End = df_data_code$Date[idhydroYear[i+1] - 1]
            } else {
                End = df_data_code$Date[length(df_data_code$Date)]
            }
            
            OkYear = df_data_code$Date >= Start & df_data_code$Date <= End
            df_data_code_year = df_data_code[OkYear,]

            StartReal = as.Date(paste(substr(Start, 1, 4),
                                      hydroYear, sep='-'))
            EndReal = as.Date(paste(as.numeric(substr(Start, 1, 4)) + 1,
                                    hydroYear, sep='-'))
            
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


## 3. NA FILTER AFTER EXTRACTION _____________________________________
#' @title NA filter
#' @export
NA_filter = function (df_data, df_XEx, dayNA_lim, timestep="year",
                      df_mod=NULL, verbose=TRUE) {

    if (timestep == "year-month" | timestep == "month") {
        dStep = months(1)
    } else if (timestep == "year") {
        dStep = lubridate::years(1)
    }
    
    startStep = df_XEx$Date
    endStep = startStep + dStep
    dayStep = endStep - startStep

    dayNA = df_XEx$NA_pct/100 * dayStep
    filter = dayNA > dayNA_lim

    df_start = summarise(group_by(df_data, code),
                      start=min(Date))
    df_end = summarise(group_by(df_data, code),
                    end=max(Date))

    df_XEx_lim = left_join(df_XEx, df_start, by=c("code"="code"))
    df_XEx_lim = left_join(df_XEx_lim, df_end, by=c("code"="code"))

    filter_start =
        df_XEx_lim$start - df_XEx_lim$Date + dayNA > dayNA_lim
    
    filter_end =
        df_XEx_lim$Date+lubridate::years(1)-1 - df_XEx_lim$end + dayNA > dayNA_lim
    
    filter = filter | filter_start | filter_end

    df_XEx$Value[filter] = NA
    codeFilter = df_XEx$code[filter]
    codeFilter = codeFilter[!duplicated(codeFilter)]
    dateFilter = format(df_XEx$Date[filter], "%Y")
    Nmod = length(codeFilter)

    if (!is.null(df_mod) & !identical(codeFilter, character(0))) {
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


## 4. SAMPLING OF THE DATA ___________________________________________
#' @title Sampling data
#' @export
sampling_data = function (df_data, df_meta,
                          hydroPeriod=c('05-01', '11-30'), Code=NULL,
                          df_mod=NULL, verbose=TRUE) {

    if (is.tbl(hydroPeriod)) {
        if (verbose) {
            print('.. Sampling of the data not possible')
        }
        if (!is.null(df_mod)) {
            res = list(data=df_data, mod=df_mod)
            return (res)
        } else {
            return (df_data)
        }
    }
    
    if (length(hydroPeriod) == 2) {
        sampleStart = as.Date(paste('1972', hydroPeriod[1], sep='-'))
        sampleEnd = as.Date(paste('1972', hydroPeriod[2], sep='-'))
    } else {
        sampleStart = as.Date(paste('1972', hydroPeriod, sep='-'))
        sampleEnd = sampleStart - 1
    }

    if (abs(sampleStart - sampleEnd) == 1) {
        if (verbose) {
            print('.. No sampling of the data needed')
        }
        if (!is.null(df_mod)) {
            res = list(data=df_data, mod=df_mod)
            return (res)
        } else {
            return (df_data)
        }
    }

    if (verbose) {
        print('.. Sampling of the data')
    }
    
    if (is.null(Code)) {
        # Get all different stations code
        Code = levels(factor(df_meta$code))
        nCode = length(Code)
    } else {
        nCode = length(Code)
    }

    mStart = as.numeric(substr(hydroPeriod[1], 1, 2))
    dStart = as.numeric(substr(hydroPeriod[1], 4, 5))
    mEnd = as.numeric(substr(hydroPeriod[2], 1, 2))
    dEnd = as.numeric(substr(hydroPeriod[2], 4, 5))
    
    if (sampleStart < sampleEnd) {
        df_data = filter(df_data,
        (mStart < lubridate::month(Date) |
         (mStart == lubridate::month(Date) &
          dStart <= lubridate::day(Date)))
        &
        (lubridate::month(Date) < mEnd |
         (lubridate::month(Date) == mEnd &
          lubridate::day(Date) <= dEnd))
        )

    } else {
        df_data = filter(df_data,
        (lubridate::month(Date) < mEnd |
         (lubridate::month(Date) == mEnd &
          lubridate::day(Date) <= dEnd))
        |
        (mStart < lubridate::month(Date) |
         (mStart == lubridate::month(Date) &
          dStart <= lubridate::day(Date)))
        )
    }

    if (!is.null(df_mod)) {
        for (code in Code) {
            df_mod = add_mod(df_mod, code,
                             type='Seasonal sampling ',
                             fun_name='NA assignment',
                             comment=paste('Between ', hydroPeriod[1],
                                           ' and ', hydroPeriod[2],
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
