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


# Usefull library
library(dplyr)
library(Hmisc)


## 1. LOCAL CORRECTION OF DATA _______________________________________
flag_data = function (df_data, df_meta, df_flag, Code=NULL, df_mod=NULL) {

    print('.. Checking of flags')
    
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


## 2. MANAGES MISSING DATA ___________________________________________
### 2.1. Long missing period over several years ______________________
missing_year = function (df_data, df_meta, yearNA_lim=10, Code=NULL, df_mod=NULL) {

    print('.. Checking for missing years')
    
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

### 2.2. Missing period over several days ____________________________
missing_day = function (df_data, df_meta, dayLac_lim=3, hydroYear='01-01', Code=NULL, df_mod=NULL) {

    print('.. Checking for missing days')

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


## 4. SAMPLING OF THE DATA ___________________________________________
sampling_data = function (df_data, df_meta, sampleSpan=c('05-01', '11-30'), Code=NULL, df_mod=NULL) {

    print('.. Sampling of the data')
    
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
