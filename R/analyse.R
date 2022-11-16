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


## 1. TREND ANALYSIS _________________________________________________
### 1.0. X ___________________________________________________________
#' @title dataEx
#' @export
get_dataEx = function (data, Process, period=NULL, df_flag=NULL,
                       df_mod=tibble(), verbose=TRUE) {

    if (verbose) {
        print(paste0('. Computes ', Process$P$var))
    }
    
    # Get all different stations code
    Code = rle(data$Code)$value

    if (!is.null(df_flag)) {
        # Local corrections if needed
        res = flag_data(data,
                        df_flag=df_flag,
                        df_mod=df_mod,
                        verbose=verbose)
        data = res$data
        df_mod = res$mod
    }


    dataEx = data

    nProcess = length(Process) - 1

    for (i in 1:nProcess) {

        process = Process[[paste0("P", i)]]
        process_names = names(process)
        for (i in 1:length(process)) {
            assign(process_names[i], process[[i]])
        }

        # Extraction
        dataEx = do.call(
            what=extraction_process,
            args=c(list(data=dataEx,
                        funct=funct,
                        funct_args=funct_args,
                        period=period,
                        samplePeriod=samplePeriod,
                        timeStep=timeStep,
                        isDate=isDate,
                        NApct_lim=NApct_lim,
                        NAyear_lim=NAyear_lim,
                        verbose=verbose),
                   functY_args))
    }

    return (dataEx)
}

#' @title X trend
#' @export
get_Xtrend = function (data, Process, period=NULL, level=0.1,
                       df_flag=NULL,
                       df_mod=tibble(),
                       verbose=TRUE) {

    if (verbose) {
        print(paste0('. Computes ', ' trend'))
    }

    # Make sure to convert the period to a list
    period = as.list(period)
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    Xtrend_all = tibble()

    # For all periods
    for (per in period) {

        if (verbose) {
            print(paste0('.. For period : ', paste0(per, collapse=' / ')))
        }

        dataEx = get_dataEx(data,
                            Process,
                            period=per,
                            df_flag=df_flag)
        
        # Compute the trend analysis
        Xtrend = trend_analyse(data=dataEx,
                               MK_level=level,
                               timeDep_option="AR1",
                               verbose=verbose)
        
        # Get the associated time interval
        I = lubridate::interval(per[1], per[2])
        # If it is the largest interval       
        if (I > Imax) {
            # Store it and the associated data
            Imax = I
            dataEx_all = dataEx
        }
        # Store the trend
        Xtrend_all = bind_rows(Xtrend_all, Xtrend)
    }

    # Creates a list of results to return
    res_analyse = list(extract=dataEx_all, estimate=Xtrend_all)
    res = list(data=data, mod=df_mod,
               analyse=res_analyse)
    return (res)
}


## 2. USEFUL FUNCTIONS _______________________________________________
### 2.1. Rolling average over stations _______________________________
expandNA = function (X, nNAdown, nNAup) {
    nX = length(X)
    IdNA = which(is.na(X))
    for (i in 1:nNAdown) {
        Id = IdNA - i
        Id = Id[Id > 0]
        X[Id] = NA
    }
    for (i in 1:nNAup) {
        Id = IdNA + i
        Id = Id[Id < nX]
        X[Id] = NA
    }    
    return (X)
}

#' @title Rolling average
#' @export
rollmean_center = function (X, k) {

    N = length(X)
    nNAdown = floor((k-1)/2)
    nNAup = ceiling((k-1)/2)
    isNA = is.na(X)

    res = rle(isNA)
    lenNA = res$lengths
    valNA = res$values
    idNAend = cumsum(lenNA)
    idNAstart = idNAend+1
    idNAstart = c(1, idNAstart[1:(length(idNAstart)-1)])
    toNA = (lenNA < k) & !valNA
    isNA[unlist(Map(':', idNAstart[toNA], idNAend[toNA]))] = TRUE

    IdNA = which(isNA)
    res = rle(isNA)
    lenNA = res$lengths
    valNA = res$values
    dNA = length(lenNA)
    dNAstart = lenNA[1] * valNA[1]
    dNAend = lenNA[dNA] * valNA[dNA]
    
    XNoNA = X[!isNA]

    if (length(IdNA) > 1) {
        start = 1 + dNAstart
        end = length(IdNA) - dNAend
        if (start < end) {
            IdNA = IdNA[start:end]
        } else {
            IdNA = NULL
        }
    }

    IdNA = IdNA - nNAdown - dNAstart
    Xroll = accelerometry::movingaves(XNoNA, k)

    if (!identical(IdNA, numeric(0)) & !all(is.na(IdNA))) {

        IdNA = IdNA - 1:length(IdNA)
        Xroll = R.utils::insert(Xroll, IdNA, NA)
    }
    Xroll = expandNA(Xroll, nNAdown, nNAup)
    Xroll = c(rep(NA, dNAstart), Xroll)
    Xroll = c(Xroll, rep(NA, dNAend))
    Xroll = c(rep(NA, nNAdown), Xroll)
    Xroll = c(Xroll, rep(NA, nNAup))
    return (Xroll)
}

#' @title Rolling average station
#' @export
rollmean_code = function (data, nroll=10, df_mod=NULL, verbose=TRUE) {

    if (verbose) {
        print(paste0('.. Rolling average over ', nroll, " days"))
    }

    library(accelerometry)
    
    df_roll = summarise(group_by(data, Code),
                        Value=rollmean_center(Value,
                                              k=nroll),
                        .groups="drop")
    
    # df_roll = summarise(group_by(data, Code),
    #                     Value=zoo::rollmean(Value,
    #                                         k=nroll,
    #                                         na.pad=TRUE),
    #                     .groups="drop")
    
    data = tibble(Date=data$Date,
                     Value=df_roll$Value,
                     Code=df_roll$Code)

    if (!is.null(df_mod)) {
        Code = rle(data$Code)$value
        # For all the code
        for (code in Code) {
            df_mod = add_mod(df_mod, code,
                             type='Rolling average',
                             fun_name='rollmean',
                             comment=paste0('Rolling average of',
                                            nroll,
                                            'days over all the data'))
        }
    }
    
    if (!is.null(df_mod)) {
        res = list(data=data, mod=df_mod)
        return (res)
    } else {
        return (data)
    }
}

### 2.2. Compute square root of data _________________________________
#' @title Square root
#' @export
compute_sqrt = function (data) {
    df_sqrt = tibble(Date=data$Date,
                     Value=sqrt(data$Value),
                     Code=data$Code)
    return (df_sqrt)
}


## 3. OTHER ANALYSES _________________________________________________
### 3.1. Hydrograph __________________________________________________
# Computes the hydrograph of a station
#' @title Hydrograph
#' @export
get_hydrograph = function (data, period=NULL, df_meta=NULL) {
    
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
    
    # If there is a specified period
    if (!is.null(period)) {
        # Extracts only the data of this period
        data = data[data$Date >= as.Date(period[1])
                          & data$Date <= as.Date(period[2]),]
    }
    
    # If there is the metadata
    if (!is.null(df_meta)) {
        # New column in metadata for hydrological regime
        df_meta$regime_hydro = NA
        # New column in metadata for the start of the hydrological year
        df_meta$maxQM = NA
        df_meta$minQM = NA
        
        # Get all different stations code
        Code = rle(data$Code)$value
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
            data_code = data[data$Code == code,]
        } else {
            # The data are the date for the current code
            data_code = data
        }
        
        # Gets a list of the month of the data as numeric
        monthData = as.numeric(format(data_code$Date, "%m"))
        # Blank list to stock month mean
        QM_code = c()
        # For all months
        for (i in 1:12) {
            # Gets all the flow data associated to the current month
            data = data_code$Value[monthData == i]
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
            df_QMtmp = tibble(QM=QM_code, Code=code)
            # Stores it
            df_QM = bind_rows(df_QM, df_QMtmp)
            # Stores result of the hydrological regime
            df_meta$regime_hydro[df_meta$Code == code] = classRegime
            
            # Computes the month of the max QM
            maxQM = which.max(QM_code)
            # Computes the month of the max QM
            minQM = which.min(QM_code)
            # Stores it as the start of the hydrological year
            df_meta$maxQM[df_meta$Code == code] = maxQM
            df_meta$minQM[df_meta$Code == code] = minQM
            
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
#' @title Break
#' @export
get_break = function (data, df_meta, level=0.1) {
    
    # Get all different stations code
    Code = rle(data$Code)$value
    # Number of stations
    nCode = length(Code)

    # Blank date break list and associated station code vector
    Date_break = list()
    Code_break = c()
    Signif_break = c()

    # For all accessible code
    for (code in Code) {
        # Get the associated data
        data_code = data[data$Code == code,] 
        # Remove NA data
        data_codeNoNA = data_code[!is.na(data_code$Value),]

        # Perform the break analysis thanks to the Pettitt test
        res_break = pettitt.test(data_codeNoNA$Value)

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
                            data_codeNoNA$Date[ibreak])
        Code_break = append(Code_break, code)
        Signif_break = append(Signif_break, p_value <= level)

        # step1 = mean(data_codeNoNA$Value[1:ibreak])
        # step2 = mean(data_codeNoNA$Value[(ibreak+1):nbreak])
    }
    # Create a tibble with the break analysis results
    df_break = tibble(Code=Code_break, Date=as.Date(Date_break),
                      significant=Signif_break)
    return (df_break)
}

### 3.3. Time gap ____________________________________________________
# Compute the time gap by station
#' @title Time gap
#' @export
get_lacune = function (data, df_meta) {
    
    # Get all different stations code
    Code = rle(data$Code)$value
    
    # Create new vector to stock results for cumulative and mean
    # time gap by station
    tLac = c()
    meanLac = c()

    # Get rows where there is no NA
    NoNA = complete.cases(data)
    # Get data where there is no NA
    data_NoNA = data[NoNA,]

    # For every station
    for (code in Code) {   
        # Get only the data rows for the selected station
        data_code = data[data$Code==code,]
        # Get date for the selected station
        Date = data_code$Date
        # Get time span for the selection station
        span = as.numeric(Date[length(Date)] - Date[1])
        
        # Get only the data rows with no NA for the selected station
        data_NoNA_code = data_NoNA[data_NoNA$Code==code,]
        # Get date for the selected station
        Date_NoNA = data_NoNA_code$Date
        
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
    df_lac = tibble(Code=Code, tLac100=tLac100, meanLac=meanLac)
    # Join a tibble
    df_meta = full_join(df_meta, df_lac)
    return (df_meta)
}


## 4. CRITICISM OF DATA ______________________________________________
#' @title Add criticism
#' @export
add_critique = function (df_critique, Code, author, level, start_date, variable, type, comment='', end_date=NULL, df_meta=NULL, resdir=NULL) {
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = rle(data$Code)$value
    }

    if (is.null(end_date)) {
        end_date = start_date
    }
    
    df_tmp = tibble(Code=Code, author=author, level=level,
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
