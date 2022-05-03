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
# R/processing/analyse.R
#
# File that realise all the possible analysis of data.
# This file regroup mainly the functions use to compute the trend
# analysis of hydrologic variables thanks to the Mann-Kendall Test.
# Functions needed for break or gap analysis are also present.


# Usefull library
library(dplyr)
library(zoo)                 # rollmean
library(StatsAnalysisTrend)
library(lubridate)
library(trend)

# Sourcing R file
source(file.path('R', 'processing', 'format.R'), encoding='UTF-8')


## 1. TREND ANALYSIS _________________________________________________
### 1.1. XA __________________________________________________________
# Realise the trend analysis of the average annual flow (QA)
# hydrological variable
get_XAtrend = function (df_data, df_meta, period, perStart, alpha, dayLac_lim, yearNA_lim, funct=mean, df_flag=NULL, df_mod=tibble(), ...) {

    print(paste0('Computes XA trend of ',
                 as.character(substitute(funct)),
                 ' function with hydrological month start ',
                 substr(perStart, 1, 2)))
    
    # Local corrections if needed
    print('Checking of flags')
    res = flag_data(df_data, df_meta,
                    df_flag=df_flag,
                    df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Removes incomplete data from time series
    print('Checking missing data')
    res = missing_data(df_data, df_meta,
                       dayLac_lim=dayLac_lim,
                       yearNA_lim=yearNA_lim,
                       perStart=perStart,
                       df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Make sure to convert the period to a list
    period = as.list(period)

    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    df_XAtrendB = tibble()

    # For all periods
    for (per in period) {
        
        # Prepare the data to fit the entry of extract.Var
        df_XAlist = prepare(df_data, colnamegroup=c('code'))

        # Compute the yearly mean over the data
        print(paste0('Extraction of data for period ',
                     paste0(per, collapse=' / ')))
        df_XAEx = extract.Var(data.station=df_XAlist,
                              funct=funct,
                              timestep='year',
                              period=per,
                              per.start=perStart,
                              pos.datetime=1,
                              ...)
        
        # Compute the trend analysis
        print(paste0('Estimation of trend for period ',
                     paste0(per, collapse=' / ')))
        df_XAtrend = Estimate.stats(data.extract=df_XAEx,
                                    level=alpha,
                                    dep.option='AR1')

        # Get the associated time interval
        I = interval(per[1], per[2])
        # If it is the largest interval
        if (I > Imax) {
            # Store it and the associated data and info
            Imax = I
            df_XAlistB = df_XAlist
            df_XAExB = df_XAEx
        }

        # Specify the period of analyse
        df_XAtrend = get_period(per, df_XAtrend, df_XAEx, df_XAlist)
        # Store the trend
        df_XAtrendB = bind_rows(df_XAtrendB, df_XAtrend)   
    } 
    # Clean results of trend analyse
    res_XAtrend = clean(df_XAtrendB, df_XAExB, df_XAlistB)

    res = list(data=df_data, mod=df_mod, analyse=res_XAtrend)
    return (res)
}

### 1.2. QMNA ________________________________________________________
# Realise the trend analysis of the monthly minimum flow in the
# year (QMNA) hydrological variable
get_QMNAtrend = function (df_data, df_meta, period, perStart, alpha, sampleSpan, dayLac_lim, yearNA_lim, df_flag, df_mod=tibble()) {

    print(paste0('Computes QMNA trend with hydrological month start ',
                 substr(perStart, 1, 2)))
    
    # Local corrections if needed
    print('Checking of flags')
    res = flag_data(df_data, df_meta,
                    df_flag=df_flag,
                    df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Removes incomplete data from time series
    print('Checking missing data')
    res = missing_data(df_data, df_meta,
                       dayLac_lim=dayLac_lim,
                       yearNA_lim=yearNA_lim,
                       perStart=perStart,
                       df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Samples the data
    print('Sampling of the data')
    res = sampling_data(df_data, df_meta,
                        sampleSpan=sampleSpan,
                        df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Make sure to convert the period to a list
    period = as.list(period)
    
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    df_QMNAtrendB = tibble()
    
    # For all periods
    for (per in period) {
        
        # Prepare the data to fit the entry of extract.Var
        df_QMNAlist = prepare(df_data, colnamegroup=c('code'))
        
        # Compute the montly mean over the data
        print(paste0('Extraction of data for period ',
                     paste0(per, collapse=' / ')))
        df_QMNAEx = extract.Var(data.station=df_QMNAlist,
                                funct=mean,
                                period=per,
                                timestep='year-month',
                                per.start="01",
                                pos.datetime=1,
                                na.rm=TRUE)
        
        # Rerepare the data to fit the entry of extract.Var
        df_QMNAlist = reprepare(df_QMNAEx,
                                df_QMNAlist,
                                colnamegroup=c('code'))
        
        # Compute the yearly min over the data
        print(paste0('Extraction of data for period ',
                     paste0(per, collapse=' / ')))
        df_QMNAEx = extract.Var(data.station=df_QMNAlist,
                                funct=min,
                                period=per,
                                per.start=perStart,
                                timestep='year',
                                pos.datetime=1,
                                na.rm=TRUE)
        # Compute the trend analysis        
        df_QMNAtrend = Estimate.stats(data.extract=df_QMNAEx,
                                      level=alpha,
                                      dep.option='AR1')

        # Get the associated time interval
        I = interval(per[1], per[2])
        # If it is the largest interval
        if (I > Imax) {
            # Store it and the associated data and info
            Imax = I
            df_QMNAlistB = df_QMNAlist
            df_QMNAExB = df_QMNAEx
        }
        
        # Specify the period of analyse
        df_QMNAtrend = get_period(per, df_QMNAtrend,
                                  df_QMNAEx,
                                  df_QMNAlist)
        # Store the trend
        df_QMNAtrendB = bind_rows(df_QMNAtrendB, df_QMNAtrend)
    }
    # Clean results of trend analyse
    res_QMNAtrend = clean(df_QMNAtrendB, df_QMNAExB, df_QMNAlistB)
        
    res = list(data=df_data, mod=df_mod, analyse=res_QMNAtrend)
    return (res)
}

### 1.3. VCN10 _______________________________________________________
rollmean_code = function (df_data, Code, nroll=10, df_mod=NULL) {
    # Blank tibble to store the data averaged
    df_data_roll = tibble()
    # For all the code
    for (code in Code) {
        # Get the data associated to the code
        df_data_code = df_data[df_data$code == code,]
        # Perform the roll mean of the flow over 10 days
        df_data_roll_code = tibble(Date=df_data_code$Date,
                                   Value=rollmean(df_data_code$Value, 
                                                  k=10,
                                                  fill=NA),
                                   code=code)
        # Store the results
        df_data_roll = bind_rows(df_data_roll, df_data_roll_code)

        if (!is.null(df_mod)) {
            df_mod = add_mod(df_mod, code,
                             type='Rolling average',
                             fun_name='rollmean',
                             comment='Rolling average of 10 day over all the data')
        }
    }

    # df_roll = summarise(group_by(df_data, code),
                        # Value=rollmean(Value, k=10, fill=NA))

    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data_roll)
    }
}

# Realises the trend analysis of the minimum 10 day average flow
# over the year (VCN10) hydrological variable
get_VCN10trend = function (df_data, df_meta, period, perStart, alpha, sampleSpan, dayLac_lim, yearNA_lim, df_flag, df_mod=tibble()) {

    print(paste0('Computes VCN10 trend with hydrological month start at ',
                 substr(perStart, 1, 2)))
    
    # Get all different stations code
    Code = levels(factor(df_meta$code))

    # Local corrections if needed
    print('Checking of flags')
    res = flag_data(df_data, df_meta,
                    df_flag=df_flag,
                    df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Computes the rolling average by 10 days over the data
    print('Computes roll mean')
    res = rollmean_code(df_data, Code, 10, df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod

    # Removes incomplete data from time series
    print('Checking missing data')
    res = missing_data(df_data_roll, df_meta,
                       dayLac_lim=dayLac_lim,
                       yearNA_lim=yearNA_lim,
                       perStart=perStart,
                       df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod

    # Samples the data
    print('Sampling of the data')
    res = sampling_data(df_data_roll, df_meta,
                        sampleSpan=sampleSpan,
                        df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod
    
    # Make sure to convert the period to a list
    period = as.list(period)
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    df_VCN10trendB = tibble()
    
    # For all periods
    for (per in period) {        
        # Prepare the data to fit the entry of extract.Var
        df_VCN10list = prepare(df_data_roll, colnamegroup=c('code'))

        # print(df_VCN10list$data$Value)
        
        # Compute the yearly min over the averaged data
        print(paste0('Extraction of data for period ',
                     paste0(per, collapse=' / ')))
        df_VCN10Ex = extract.Var(data.station=df_VCN10list,
                                 funct=min,
                                 period=per,
                                 per.start=perStart,
                                 timestep='year',
                                 pos.datetime=1,
                                 na.rm=TRUE)

        # print(df_VCN10Ex)

        # Compute the trend analysis
        print(paste0('Estimation of trend for period ',
                     paste0(per, collapse=' / ')))
        df_VCN10trend = Estimate.stats(data.extract=df_VCN10Ex,
                                       level=alpha,
                                       dep.option='AR1')

        # print(df_VCN10trend)
        print('ok')

        # Get the associated time interval
        I = interval(per[1], per[2])
        # If it is the largest interval       
        if (I > Imax) {
            # Store it and the associated data and info           
            Imax = I
            df_VCN10listB = df_VCN10list
            df_VCN10ExB = df_VCN10Ex
        }

        # Specify the period of analyse
        df_VCN10trend = get_period(per, df_VCN10trend, df_VCN10Ex,
                                   df_VCN10list)

        # Store the trend
        df_VCN10trendB = bind_rows(df_VCN10trendB, df_VCN10trend)
    }
    
    # Clean results of trend analyse
    res_VCN10trend = clean(df_VCN10trendB, df_VCN10ExB, df_VCN10listB)
    
    res = list(data=df_data_roll, mod=df_mod,
               analyse=res_VCN10trend)
    return (res)
}

### 1.4. tDEB date ___________________________________________________
which_underfirst = function (L, UpLim, select_longest=TRUE) {
    
    ID = which(L <= UpLim)

    if (select_longest) {
        dID = diff(ID)
        dID = c(10, dID)
        
        IDjump = which(dID != 1)
        Njump = length(IDjump)
        
        Periods = vector(mode='list', length=Njump)
        Nperiod = c()
        
        for (i in 1:Njump) {
            idStart = IDjump[i]
            
            if (i < Njump) {
                idEnd = IDjump[i+1] - 1
            } else {
                idEnd = length(ID)
            }
            
            period = ID[idStart:idEnd]
            Periods[[i]] = period
            Nperiod = c(Nperiod, length(period))
        }
        period_max = Periods[[which.max(Nperiod)]]
        id = period_max[1]
    } else {
        id = ID[1]
    }
    return (id)
}

get_tDEBtrend = function (df_data, df_meta, period, perStart, alpha, sampleSpan, dayLac_lim, yearNA_lim, df_flag, thresold_type='VCN10', select_longest=TRUE, df_mod=tibble()) {

    print(paste0('Computes tDEB trend with hydrological month start ',
                 substr(perStart, 1, 2)))
    
    # Get all different stations code
    Code = levels(factor(df_meta$code))
    # Gets the number of station
    nCode = length(Code)

    # Local corrections if needed
    print('Checking of flags')
    res = flag_data(df_data, df_meta,
                    df_flag=df_flag,
                    df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Computes the rolling average by 10 days over the data
    print('Computes roll mean')
    res = rollmean_code(df_data, Code, 10, df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod

    # Removes incomplete data from time series
    print('Checking missing data')
    df_data = missing_data(df_data,
                           df_meta=df_meta,
                           dayLac_lim=dayLac_lim,
                           yearNA_lim=yearNA_lim,
                           perStart=perStart)
    
    # Samples the data
    print('Sampling of the data')
    df_data = sampling_data(df_data,
                            df_meta=df_meta,
                            sampleSpan=sampleSpan)
    
    # Removes incomplete data from the averaged time series
    print('Checking missing data')
    res = missing_data(df_data_roll,
                       df_meta=df_meta,
                       dayLac_lim=dayLac_lim,
                       yearNA_lim=yearNA_lim,
                       perStart=perStart,
                       df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod
    
    # Samples the data
    print('Sampling of the data')
    res = sampling_data(df_data_roll,
                        df_meta=df_meta,
                        sampleSpan=sampleSpan,
                        df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod

    # Make sure to convert the period to a list
    period = as.list(period)
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    df_tDEBtrendB = tibble()

    # For all periods
    for (per in period) {

        if (thresold_type == 'QNj') {
            # Prepare the data to fit the entry of extract.Var
            df_QTlist = prepare(df_data,
                                colnamegroup=c('code'))
        } else if (thresold_type == 'VCN10') {
            # Prepare the data to fit the entry of extract.Var
            df_QTlist = prepare(df_data_roll,
                                colnamegroup=c('code'))
        }
        
        # Compute the yearly mean over the data
        df_QTEx = extract.Var(data.station=df_QTlist,
                              funct=min,
                              timestep='year',
                              period=per,
                              per.start=perStart,
                              pos.datetime=1,
                              na.rm=TRUE)
        
        df_QT = summarise(group_by(df_QTEx, group1),
                          values=max(values, na.rm=TRUE))
        
        # Renames the column of group of trend results
        colnames(df_QT) = c('group', 'Thresold')
        df_QT = full_join(df_QT, df_QTlist$info, by='group')
        df_QT = df_QT[-1]
        
        # print(df_QT)
        
        df_tDEBEx = tibble()
        df_tDEBlist = list(data=tibble(), info=tibble())
        
        # For all the code
        for (k in 1:nCode) {
            # Gets the code
            code = Code[k]
            
            # Get the data associated to the code
            df_data_code = df_data[df_data$code == code,]
            # Get the averaged data associated to the code
            df_data_roll_code = df_data_roll[df_data_roll$code == code,]
            
            # Prepare the data to fit the entry of extract.Var
            df_tDEBlist_code = prepare(df_data_roll_code,
                                      colnamegroup=c('code'))

            QT_code = df_QT$Thresold[df_QT$code == code]
            
            # Compute the yearly min over the averaged data
            print(paste0('Extraction of data for period ',
                         paste0(per, collapse=' / ')))
            df_tDEBEx_code = extract.Var(data.station=df_tDEBlist_code,
                                        funct=which_underfirst,
                                        period=per,
                                        per.start=perStart,
                                        timestep='year',
                                        pos.datetime=1,
                                        UpLim=QT_code,
                                        select_longest=select_longest)

            df_tDEBEx_code$group1 = k
            df_tDEBlist_code$data$group = k
            df_tDEBlist_code$info$group = k
            
            # Converts index of the tDEB to the julian date associated
            df_tDEBEx_code = prepare_date(df_tDEBEx_code,
                                          df_tDEBlist_code)

            # Store the results
            df_tDEBEx = bind_rows(df_tDEBEx, df_tDEBEx_code)
            
            df_tDEBlist$data = bind_rows(df_tDEBlist$data,
                                         df_tDEBlist_code$data)
            
            df_tDEBlist$info = bind_rows(df_tDEBlist$info,
                                         df_tDEBlist_code$info)
        }
        
        # Compute the trend analysis
                print(paste0('Estimation of trend for period ',
                     paste0(per, collapse=' / ')))
        df_tDEBtrend = Estimate.stats(data.extract=df_tDEBEx,
                                      level=alpha,
                                      dep.option='AR1')

        # Get the associated time interval
        I = interval(per[1], per[2])
        # If it is the largest interval       
        if (I > Imax) {
            # Store it and the associated data and info           
            Imax = I
            df_tDEBlistB = df_tDEBlist
            df_tDEBExB = df_tDEBEx
        }
        
        # Specify the period of analyse
        df_tDEBtrend = get_period(per, df_tDEBtrend, df_tDEBEx,
                                   df_tDEBlist)
        # Store the trend
        df_tDEBtrendB = bind_rows(df_tDEBtrendB, df_tDEBtrend)
    }
    # Clean results of trend analyse
    res_tDEBtrend = clean(df_tDEBtrendB, df_tDEBExB, df_tDEBlistB)
    
    res = list(data=df_data_roll, mod=df_mod,
               analyse=res_tDEBtrend)
    return (res)
}

### 1.5. tCEN date ___________________________________________________
# Realises the trend analysis of the date of the minimum 10 day
# average flow over the year (VCN10) hydrological variable
get_tCENtrend = function (df_data, df_meta, period, perStart, alpha, sampleSpan, dayLac_lim, yearNA_lim, df_flag, df_mod=tibble()) {

    print(paste0('Computes tCEN trend with hydrological month start ',
                 substr(perStart, 1, 2)))
    
    # Get all different stations code
    Code = levels(factor(df_meta$code))
    # Blank tibble to store the data averaged
    df_data_roll = tibble() 

    # Local corrections if needed
    print('Checking of flags')
    res = flag_data(df_data, df_meta,
                    df_flag=df_flag,
                    df_mod=df_mod)
    df_data = res$data
    df_mod = res$mod
    
    # Computes the rolling average by 10 days over the data
    print('Computes roll mean')
    res = rollmean_code(df_data, Code, 10, df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod
        
    # Removes incomplete data from time series
    print('Checking missing data')
    res = missing_data(df_data_roll, df_meta,
                       dayLac_lim=dayLac_lim,
                       yearNA_lim=yearNA_lim,
                       perStart=perStart,
                       df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod
    
    # Samples the data
    print('Sampling of the data')
    res = sampling_data(df_data_roll, df_meta,
                                 sampleSpan=sampleSpan,
                                 df_mod=df_mod)
    df_data_roll = res$data
    df_mod = res$mod

    # Make sure to convert the period to a list
    period = as.list(period)
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    df_tCENtrendB = tibble()

    # For all periods
    for (per in period) {
        # Prepare the data to fit the entry of extract.Var
        df_tCENlist = prepare(df_data_roll, colnamegroup=c('code'))
        
        # Compute the yearly min over the averaged data
        print(paste0('Extraction of data for period ',
                     paste0(per, collapse=' / ')))
        df_tCENEx = extract.Var(data.station=df_tCENlist,
                               funct=which.min,
                               period=per,
                               per.start=perStart,
                               timestep='year',
                               pos.datetime=1)

        # Converts index of the tCEN to the julian date associated
        df_tCENEx = prepare_date(df_tCENEx, df_tCENlist)
        
        # Compute the trend analysis
        print(paste0('Estimation of trend for period ',
                     paste0(per, collapse=' / ')))
        df_tCENtrend = Estimate.stats(data.extract=df_tCENEx,
                                      level=alpha,
                                      dep.option='AR1')

        # Get the associated time interval
        I = interval(per[1], per[2])
        # If it is the largest interval       
        if (I > Imax) {
            # Store it and the associated data and info           
            Imax = I
            df_tCENlistB = df_tCENlist
            df_tCENExB = df_tCENEx
        }

        # Specify the period of analyse
        df_tCENtrend = get_period(per, df_tCENtrend, df_tCENEx,
                                   df_tCENlist)
        # Store the trend
        df_tCENtrendB = bind_rows(df_tCENtrendB, df_tCENtrend)
    }
    # Clean results of trend analyse
    res_tCENtrend = clean(df_tCENtrendB, df_tCENExB, df_tCENlistB)

    res = list(data=df_data_roll, mod=df_mod,
               analyse=res_tCENtrend)
    return (res)
}


## 2. OTHER ANALYSES _________________________________________________
### 2.1. Hydrograph __________________________________________________
xref = matrix(
    c(0.099, 0.100, 0.101, 0.099, 0.088, 0.078, 0.072,
      0.064, 0.064, 0.069, 0.076, 0.089,
      0.133, 0.126, 0.111, 0.110, 0.081, 0.056, 0.038,
      0.027, 0.042, 0.063, 0.098, 0.117,
      0.128, 0.142, 0.122, 0.128, 0.105, 0.065, 0.035,
      0.024, 0.031, 0.044, 0.074, 0.101,
      0.157, 0.130, 0.119, 0.094, 0.062, 0.042, 0.028,
      0.021, 0.035, 0.062, 0.099, 0.150,
      0.204, 0.163, 0.118, 0.102, 0.060, 0.030, 0.018,
      0.012, 0.023, 0.041, 0.087, 0.143,
      0.156, 0.154, 0.117, 0.119, 0.086, 0.044, 0.025,
      0.015, 0.025, 0.044, 0.089, 0.127,
      0.139, 0.092, 0.082, 0.099, 0.087, 0.039, 0.015,
      0.012, 0.036, 0.108, 0.159, 0.131,
      0.112, 0.098, 0.101, 0.125, 0.122, 0.072, 0.036,
      0.024, 0.039, 0.067, 0.102, 0.102,
      0.058, 0.050, 0.100, 0.142, 0.158, 0.092, 0.067,
      0.050, 0.042, 0.058, 0.083, 0.100,
      0.050, 0.050, 0.058, 0.083, 0.150, 0.167, 0.117,
      0.083, 0.058, 0.058, 0.067, 0.058,
      0.033, 0.025, 0.033, 0.075, 0.167, 0.217, 0.142,
      0.092, 0.067, 0.058, 0.050, 0.042,
      0.017, 0.008, 0.017, 0.042, 0.108, 0.183, 0.200,
      0.175, 0.117, 0.067, 0.042, 0.025),
    ncol=12, byrow=TRUE)

colnames(xref) = seq(1, 12, 1)
row.names(xref) = c('GROUP1', 'GROUP2', 'GROUP3', 'GROUP4',
                    'GROUP5', 'GROUP6', 'GROUP7', 'GROUP8',
                    'GROUP9', 'GROUP10', 'GROUP11', 'GROUP12')    

# Computes the hydrograph of a station
get_hydrograph = function (df_data, period=NULL, df_meta=NULL) {

    # If there is a specified period
    if (!is.null(period)) {
        # Extracts only the data of this period
        df_data = df_data[df_data$Date >= as.Date(period[1])
                          & df_data$Date <= as.Date(period[2]),]
    }
    
    # If there is the metadata
    if (!is.null(df_meta)) {
        # New column in metadata for hydrological regime
        df_meta$regime_hydro = NA
        # New column in metadata for the start of the hydrological year
        df_meta$start_year = NA

        # Get all different stations code
        Code = levels(factor(df_meta$code))
        # Number of stations
        nCode = length(Code)
        
    # Otherwise it is just a list of flow from one station
    } else {
        # Only one code is present
        nCode = 1
    }

    # Blank tibble to store data
    df_QM = tibble()
    # For all accessible code
    for (k in 1:nCode) {
        # If there is the metadata
        if (!is.null(df_meta)) {
            # Gets the code
            code = Code[k]
            # Get the associated data
            df_data_code = df_data[df_data$code == code,]
        } else {
            # The data are the date for the current code
            df_data_code = df_data
        }
        
        # Gets a list of the month of the data as numeric
        monthData = as.numeric(format(df_data_code$Date, "%m"))
        # Blank list to stock month mean
        QM_code = c()
        # For all months
        for (i in 1:12) {
            # Gets all the flow data associated to the current month
            data = df_data_code$Value[monthData == i]
            # Averages the data
            QM_code[i] = mean(data, na.rm=TRUE)
        }

        regime = 0
        classRegime = ""
        distance = rep(0, length(xref[,1]))
        distancemin = 0
        for (j in 1:length(xref[,1])) {
            distance[j] = sum((QM_code / mean(QM_code) - xref[j,])^2)
        }
        regime = which.min(distance)
        distancemin = distance[which.min(distance)]
        
        if (regime < 7) {
            classRegime = "Pluvial"
        } else if (regime >= 7 & regime < 10) {
            classRegime = "Transition"
        } else if (regime >= 10) {
            classRegime = "Nival Glaciaire"
        } 
        
        # If there is the metadata
        if (!is.null(df_meta)) {
            # Creates a temporary tibble to store hydrograph results
            df_QMtmp = tibble(QM=QM_code, code=code)
            # Stores it
            df_QM = bind_rows(df_QM, df_QMtmp)
            # Stores result of the hydrological regime
            df_meta$regime_hydro[df_meta$code == code] = classRegime
            # Computes the month of the max QM
            maxMonth = which.max(QM_code)
            
            # Stores it as the start of the hydrological year
            df_meta$start_year[df_meta$code == code] = maxMonth
        # Otherwise
        } else {
            # No tibble needed
            df_QM = QM_code
            df_meta = classRegime
        }
    }
    # Returns the hydrograph and meta data
    return (list(QM=df_QM, meta=df_meta))
}
    
### 2.2. Break date __________________________________________________
# Compute the break date of the flow data by station 
get_break = function (df_data, df_meta, alpha=0.1) {
    
    # Get all different stations code
    Code = levels(factor(df_meta$code))
    # Number of stations
    nCode = length(Code)

    # Blank date break list and associated station code vector
    Date_break = list()
    Code_break = c()
    Signif_break = c()

    # For all accessible code
    for (code in Code) {
        # Get the associated data
        df_data_code = df_data[df_data$code == code,] 
        # Remove NA data
        df_data_codeNoNA = df_data_code[!is.na(df_data_code$Value),]

        # Perform the break analysis thanks to the Pettitt test
        res_break = pettitt.test(df_data_codeNoNA$Value)

        # Extract p value
        p_value = res_break$p
        # The length of the data analysed
        nbreak = res_break$nobs
        # Index of the break date
        ibreak = res_break$estimate

        # Get the mean of the index break if there is several
        ibreak = round(mean(ibreak), 0)
        # Store the date break with its associated code
        Date_break = append(Date_break, 
                            df_data_codeNoNA$Date[ibreak])
        Code_break = append(Code_break, code)
        Signif_break = append(Signif_break, p_value <= alpha)

        # step1 = mean(df_data_codeNoNA$Value[1:ibreak])
        # step2 = mean(df_data_codeNoNA$Value[(ibreak+1):nbreak])
    }
    # Create a tibble with the break analysis results
    df_break = tibble(code=Code_break, Date=as.Date(Date_break),
                      significant=Signif_break)
    return (df_break)
}

### 2.3. Time gap ____________________________________________________
# Compute the time gap by station
get_lacune = function (df_data, df_meta) {
    
    # Get all different stations code
    Code = levels(factor(df_meta$code))
    
    # Create new vector to stock results for cumulative and mean
    # time gap by station
    tLac = c()
    meanLac = c()

    # Get rows where there is no NA
    NoNA = complete.cases(df_data)
    # Get data where there is no NA
    df_data_NoNA = df_data[NoNA,]

    # For every station
    for (code in Code) {   
        # Get only the data rows for the selected station
        df_data_code = df_data[df_data$code==code,]
        # Get date for the selected station
        Date = df_data_code$Date
        # Get time span for the selection station
        span = as.numeric(Date[length(Date)] - Date[1])
        
        # Get only the data rows with no NA for the selected station
        df_data_NoNA_code = df_data_NoNA[df_data_NoNA$code==code,]
        # Get date for the selected station
        Date_NoNA = df_data_NoNA_code$Date
        
        # Compute the time gap
        lac = as.numeric(diff(Date_NoNA) - 1)

        # Compute the cumulative gap
        lac_sum = sum(lac)
        # Store the cumulative gap rate
        tLac = c(tLac, lac_sum/span)

        # Compute the mean gap
        lac_mean = mean(lac[lac != 0])
        # Store the mean gap
        meanLac = c(meanLac, lac_mean) 
    }
    
    # Compute the cumulative gap rate in pourcent
    tLac100 = tLac * 100
    # Create tibble for lacune
    df_lac = tibble(code=Code, tLac100=tLac100, meanLac=meanLac)
    # Join a tibble
    df_meta = full_join(df_meta, df_lac)
    return (df_meta)
}

### 2.4. Compute square root of data _________________________________
compute_sqrt = function (df_data) {

    df_sqrt = tibble(Date=df_data$Date,
                     Value=sqrt(df_data$Value),
                     code=df_data$code)
        
    return (df_sqrt)
}

### 2.5. Criticism of data ___________________________________________
add_critique = function (df_critique, Code, author, level, start_date, variable, type, comment='', end_date=NULL, df_meta=NULL, resdir=NULL) {
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = levels(factor(df_meta$code))
    }

    if (is.null(end_date)) {
        end_date = start_date
    }
    
    df_tmp = tibble(code=Code, author=author, level=level,
                    start_date=start_date, end_date=end_date,
                    variable=variable, type=type,
                    comment=comment)
    df_critique = bind_rows(df_critique, df_tmp)

    nc = nrow(df_critique)
    print('Criticism registered')
    print(df_critique[(nc-2):nc,])

    if (!is.null(resdir)) {   
        write_critique(df_critique, resdir)
    }
    
    return (df_critique)
}

# df_critique = add_critique(df_critique, resdir=resdir, Code='', author='louis', level=, start_date=, end_date=NA, variable='', type='', comment='')
