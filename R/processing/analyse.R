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
library(Hmisc)

# Sourcing R file
source(file.path('R', 'processing', 'format.R'), encoding='UTF-8')
source(file.path('R', 'processing', 'correction.R'), encoding='UTF-8')


## 1. TREND ANALYSIS _________________________________________________
### 1.0. X ___________________________________________________________
get_Xtrend = function (var, df_data, df_meta, period, hydroYear,
                       hydroPeriod, alpha,
                       df_flag=NULL, sampleSpan=NULL, yearNA_lim=NULL,
                       dayLac_lim=NULL, NA_pct_lim=NULL,
                       day_to_roll=NULL,
                       functM=NULL, functM_args=NULL, isDateM=FALSE,
                       functY=NULL, functY_args=NULL, isDateY=FALSE,
                       functYT_ext=NULL, functYT_ext_args=NULL,
                       isDateYT_ext=FALSE, functYT_sum=NULL,
                       functYT_sum_args=NULL,
                       df_mod=tibble()) {

    print(paste0('. Computes ', var, ' trend for hydrological month start ',
                 substr(hydroYear, 1, 2)))

    # Get all different stations code
    Code = levels(factor(df_meta$code))
    
    if (!is.null(df_flag)) {
        # Local corrections if needed
        res = flag_data(df_data, df_meta,
                        df_flag=df_flag,
                        df_mod=df_mod)
        df_data = res$data
        df_mod = res$mod
    }

    if (!is.null(day_to_roll)) {
        # Computes the rolling average by day_to_roll days over the data
        res = rollmean_code(df_data, Code, day_to_roll, df_mod=df_mod)
        df_data = res$data
        df_mod = res$mod
    }
    
    if (!is.null(yearNA_lim)) {
        # Removes older data if there are a too long missing period
        res = missing_year(df_data, df_meta,
                           yearNA_lim=yearNA_lim,
                           df_mod=df_mod)
        df_data = res$data
        df_mod = res$mod
    }

    if (!is.null(dayLac_lim)) {
        # Removes incomplete years if there are too long missing
        # consecutive days
        res = missing_day(df_data, df_meta,
                          dayLac_lim=dayLac_lim,
                          hydroYear=hydroYear,
                          df_mod=df_mod)
        df_data = res$data
        df_mod = res$mod
    }

    if (!is.null(sampleSpan)) {
        # Samples the data
        res = sampling_data(df_data, df_meta,
                            sampleSpan=sampleSpan,
                            df_mod=df_mod)
        df_data = res$data
        df_mod = res$mod
    }
    
    # Make sure to convert the period to a list
    period = as.list(period)
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    df_Xtrend_all = tibble()

    # For all periods
    for (per in period) {

        print(paste0('.. For period : ', paste0(per, collapse=' / ')))

        # Monthly extraction
        if (!is.null(functM)) {
            df_XEx = do.call(
                what=extract_Var_WRAP,
                args=c(list(df_data=df_data,
                            funct=functM,
                            period=per,
                            hydroYear="01",
                            hydroPeriod=hydroPeriod,
                            timestep='year-month',
                            isDate=isDateM),
                       functM_args))
            df_data = df_XEx
        }

        # Yearly extraction
        if (!is.null(functYT_ext) & !is.null(functYT_sum)) {
            df_YTEx = do.call(
                what=extract_Var_WRAP,
                args=c(list(df_data=df_data,
                            funct=functYT_ext,
                            period=per,
                            hydroYear=hydroYear,
                            hydroPeriod=hydroPeriod,
                            timestep='year',
                            isDate=isDateYT_ext),
                       functYT_ext_args))
            
            df_YT = summarise(group_by(df_YTEx, code),
                              threshold=functYT_sum(Value,
                                                    !!!functYT_sum_args))
            
            idT = which(functY_args == '*threshold*')
            
            df_XEx = tibble()
            # For all the code
            for (code in Code) {
                # Get the averaged data associated to the code
                df_data_code = df_data[df_data$code == code,]
                YT_code = df_YT$threshold[df_YT$code == code]
                
                functY_args[idT] = YT_code
                
                df_XEx_code = do.call(
                    what=extract_Var_WRAP,
                    args=c(list(df_data=df_data_code,
                                funct=functY,
                                period=per,
                                hydroYear=hydroYear,
                                hydroPeriod=hydroPeriod,
                                timestep='year',
                                isDate=isDateY),
                           functY_args))
                # Store the results
                df_XEx = bind_rows(df_XEx, df_XEx_code)
            }
        } else {            
            df_XEx = do.call(
                what=extract_Var_WRAP,
                args=c(list(df_data=df_data,
                            funct=functY,
                            period=per,
                            hydroYear=hydroYear,
                            hydroPeriod=hydroPeriod,
                            timestep='year',
                            isDate=isDateY),
                       functY_args))
        }
        
        if (!is.null(NA_pct_lim)) {
            # NA filtering
            res = NA_filter(df_XEx,
                            NA_pct_lim=NA_pct_lim,
                            df_mod=df_mod)
            df_XEx = res$data
            df_mod = res$mod
        }

        # Compute the trend analysis
        df_Xtrend = Estimate_stats_WRAP(df_XEx=df_XEx,
                                        alpha=alpha,
                                        period=per,
                                        dep_option='AR1')
        
        # Get the associated time interval
        I = interval(per[1], per[2])
        # If it is the largest interval       
        if (I > Imax) {
            # Store it and the associated data
            Imax = I
            df_XEx_all = df_XEx
        }
        # Store the trend
        df_Xtrend_all = bind_rows(df_Xtrend_all, df_Xtrend)
    }

    # Creates a list of results to return
    res_analyse = list(extract=df_XEx_all, estimate=df_Xtrend_all)
    res = list(data=df_data, mod=df_mod,
               analyse=res_analyse)
    return (res)
}


## 2. USEFUL FUNCTIONS _______________________________________________
### 2.1. Rolling average over stations _______________________________
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

    if (!is.null(df_mod)) {
        res = list(data=df_data, mod=df_mod)
        return (res)
    } else {
        return (df_data_roll)
    }
}

### 2.2. Which with NA management ____________________________________
which.maxNA = function (x) {
    idMax = which.max(x)
    if (identical(idMax, integer(0))) {
        idMax = NA
    }
    return (idMax)
}

which.minNA = function (x) {
    idMin = which.min(x)
    if (identical(idMin, integer(0))) {
        idMin = NA
    }
    return (idMin)
}

### 2.3. Which under threshold _______________________________________
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

### 2.4. Base flow separation ________________________________________
BFS = function (Q, d=5, w=0.9) {

    N = length(Q)
    Slices = split(Q, ceiling(seq_along(Q)/d))
    
    idMinSlices = unlist(lapply(Slices, which.min),
                         use.names=FALSE)
    idShift = c(0, cumsum(unlist(lapply(Slices, length),
                                 use.names=FALSE)))
    idShift = idShift[-length(idShift)]
    idMin = idMinSlices + idShift
    Qmin_k = Q[idMin]

    n = length(Qmin_k)
    Qmin_kp1 = c(Qmin_k[2:n], NA)
    Qmin_km1 = c(NA, Qmin_k[1:(n-1)])
    test = w * Qmin_k < pmin(Qmin_km1, Qmin_kp1)
    test[is.na(test)] = FALSE
    idPivots = idMin[which(test)]
    Pivots = Qmin_k[test]

    # BF = approx(idPivots, Pivots, xout=1:N)$y
    BF = approxExtrap(idPivots, Pivots, xout=1:N,
                      method="linear", na.rm=TRUE)$y  
    
    BF[is.na(Q)] = NA
    BF[BF < 0] = 0
    test = BF > Q
    test[is.na(test)] = FALSE
    BF[test] = Q[test]

    return (BF)
}

### 2.5. Compute square root of data _________________________________
compute_sqrt = function (df_data) {
    df_sqrt = tibble(Date=df_data$Date,
                     Value=sqrt(df_data$Value),
                     code=df_data$code)
    return (df_sqrt)
}


## 3. OTHER ANALYSES _________________________________________________
### 3.1. Hydrograph __________________________________________________
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
    
### 3.2. Break date __________________________________________________
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

### 3.3. Time gap ____________________________________________________
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


## 4. CRITICISM OF DATA ______________________________________________
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
