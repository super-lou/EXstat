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
# R/processing/script_analyse.R
#
# Script that manages the call to the right process in order to
# realise analyses.


# Sourcing the R file
source(file.path('R', 'processing', 'analyse.R'),
       encoding='UTF-8')


## 1. STATION TREND ANALYSIS _________________________________________
if ('station_trend_analyse' %in% to_do) {
### 1.1. Info about analysis _________________________________________
    if (hydroYear_mode == 'every') {
        hydroYear_all = matrix(rep(paste0(formatC(1:12, width=2,
                                             flag=0),
                                     '-01'),
                              length(var_all)),
                              nrow=length(var_all), byrow=TRUE)
        
    } else if (hydroYear_mode == 'fixed') {
        hydroYear_all = matrix(hydroYear_fixed,
                               byrow=length(var_all))
    }
    
### 1.2. Selection of variables ______________________________________
    var = c()
    type = c()
    glose = c()
    correction = list()
    for (OkVar in to_analyse) {
        Ok = var_all == OkVar
        var = c(var, var_all[Ok])
        type = c(type, type_all[Ok])
        glose = c(glose, glose_all[Ok])
        correction = append(correction,
                            list(correction_all[Ok]))
    }
    hydroYear = matrix(hydroYear_all[var_all %in% to_analyse,],
                      nrow=length(var))
    nbHydroYear = ncol(hydroYear)
    
### 1.3. Trend analyses ______________________________________________
    for (i in 1:nbHydroYear) {

        # QIXA trend
        if ('QIXA' %in% var) {
            res = get_XAtrend(df_data, df_meta,
                              period=trend_period,
                              perStart=hydroYear['QIXA' == var, i],
                              alpha=alpha,
                              df_flag=df_flag,
                              sampleSpan=sampleSpan,
                              yearNA_lim=yearNA_lim,
                              dayLac_lim=dayLac_lim,
                              NA_pct_lim=NA_pct_lim,
                              correction_to_do=unlist(correction['QIXA' == var]),
                              funct=max,
                              na.rm=TRUE)
            df_QIXAdata = res$data
            df_QIXAmod = res$mod
            res_QIXAtrend = res$analyse
        }

        # tQIXA trend
        if ('tQIXA' %in% var) {
            res = get_XAtrend(df_data, df_meta,
                              period=trend_period,
                              perStart=hydroYear['QIXA' == var, i],
                              alpha=alpha,
                              df_flag=df_flag,
                              sampleSpan=sampleSpan,
                              yearNA_lim=yearNA_lim,
                              dayLac_lim=dayLac_lim,
                              NA_pct_lim=NA_pct_lim,
                              correction_to_do=unlist(correction['tQIXA' == var]),
                              funct=which.maxNA,
                              isDate=TRUE)
            df_tQIXAdata = res$data
            df_tQIXAmod = res$mod
            res_tQIXAtrend = res$analyse
        }
        
        # QA trend
        if ('QA' %in% var) {
            res = get_XAtrend(df_data, df_meta,
                              period=trend_period,
                              perStart=hydroYear['QA' == var, i],
                              alpha=alpha,
                              df_flag=df_flag,
                              sampleSpan=sampleSpan,
                              yearNA_lim=yearNA_lim,
                              dayLac_lim=dayLac_lim,
                              NA_pct_lim=NA_pct_lim,
                              correction_to_do=unlist(correction['QA' == var]),
                              funct=mean,
                              na.rm=TRUE)
            df_QAdata = res$data
            df_QAmod = res$mod
            res_QAtrend = res$analyse
        }

        # QMNA tend
        if ('QMNA' %in% var) {            
            res = get_QMNAtrend(df_data, df_meta,
                                period=trend_period,
                                perStart=hydroYear['QMNA' == var, i],
                                alpha=alpha,
                                df_flag=df_flag,
                                sampleSpan=sampleSpan,
                                yearNA_lim=yearNA_lim,
                                dayLac_lim=dayLac_lim,
                                NA_pct_lim=NA_pct_lim,
                                correction_to_do=unlist(correction['QMNA' == var]))
            df_QMNAdata = res$data
            df_QMNAmod = res$mod
            res_QMNAtrend = res$analyse
        }

        # VCN10 trend
        if ('VCN10' %in% var) {            
            res = get_VCN10trend(df_data, df_meta,
                                 period=trend_period,
                                 perStart=hydroYear['VCN10' == var, i],
                                 alpha=alpha,
                                 df_flag=df_flag,
                                 sampleSpan=sampleSpan,
                                 yearNA_lim=yearNA_lim,
                                 dayLac_lim=dayLac_lim,
                                 NA_pct_lim=NA_pct_lim,
                                 correction_to_do=unlist(correction['VCN10' == var]),)
            df_VCN10data = res$data
            df_VCN10mod = res$mod
            res_VCN10trend = res$analyse
        }

        # Start date for low water trend
        if ('tDEB' %in% var) {
            res = get_tDEBtrend(df_data, df_meta, 
                                period=trend_period,
                                perStart=hydroYear['tDEB' == var, i],
                                alpha=alpha,
                                df_flag=df_flag,
                                sampleSpan=sampleSpan,
                                yearNA_lim=yearNA_lim,
                                dayLac_lim=dayLac_lim,
                                NA_pct_lim=NA_pct_lim,
                                correction_to_do=unlist(correction['tDEB' == var]),,
                                thresold_type='VCN10',
                                select_longest=TRUE)
            df_tDEBdata = res$data
            df_tDEBmod = res$mod
            res_tDEBtrend = res$analyse
        }

        # Center date for low water trend
        if ('tCEN' %in% var) {
            res = get_tCENtrend(df_data, df_meta, 
                                period=trend_period,
                                perStart=hydroYear['tCEN' == var, i],
                                alpha=alpha,
                                df_flag=df_flag,
                                sampleSpan=sampleSpan,
                                yearNA_lim=yearNA_lim,
                                dayLac_lim=dayLac_lim,
                                NA_pct_lim=NA_pct_lim,
                                correction_to_do=unlist(correction['tCEN' == var]),)
            df_tCENdata = res$data
            df_tCENmod = res$mod
            res_tCENtrend = res$analyse
        }

        DF_DATA = list()
        DF_TREND = list()
        # For all the variable
        for (v in var) {
            # Gets the extracted data for the variable
            df_datatmp = get(paste('res_', v, 'trend', sep=''))$data
            # Gets the trend results for the variable
            df_trendtmp = get(paste('res_', v, 'trend', sep=''))$trend

            DF_DATA = append(DF_DATA, list(df_datatmp))
            DF_TREND = append(DF_TREND, list(df_trendtmp))
        }

### 1.3. Saving ______________________________________________________
        if ('meta' %in% saving) {
            # Sourcing the R file
            source(file.path('R', 'processing', 'read_write.R'),
                   encoding='UTF-8')
            
            if (fast_format) {
                write_metaFST(df_meta, resdir,
                              filedir=file.path('fst'))
            }
        }

        if ('data' %in% saving) {
            # Sourcing the R file
            source(file.path('R', 'processing', 'read_write.R'),
                   encoding='UTF-8')
            
            # For all the variable
            for (v in var) {
                # Gets the extracted data for the variable
                df_datatmp = get(paste('df_', v, 'data', sep=''))
                # Gets the modification file of the data for the variable
                df_modtmp = get(paste('df_', v, 'mod', sep=''))

                monthStart = substr(hydroYear[v == var, i], 1, 2)
                
                # Writes modified data
                write_data(df_datatmp, df_modtmp, resdir,
                           filedir=file.path('modified_data',
                                             v, monthStart))
                
                if (fast_format) {
                    write_dataFST(df_datatmp, resdir,
                                  filedir='fst',
                                  filename=paste0('data_', v,
                                                  '_', monthStart,
                                                  '.fst'))
                }
            }
        }

        if ('analyse' %in% saving) {
            # Sourcing the R file
            source(file.path('R', 'processing', 'read_write.R'),
                   encoding='UTF-8')
            
            # For all the variable
            for (v in var) {
                # Gets the trend results for the variable
                res_trendtmp = get(paste('res_', v, 'trend', sep=''))

                monthStart = substr(hydroYear[v == var, i], 1, 2)

                # Writes trend analysis results
                write_analyse(res_trendtmp, resdir,
                              filedir=file.path('trend_analyses',
                                                v, monthStart))
                
                if (fast_format) {
                    write_dataFST(res_trendtmp$data,
                                  resdir,
                                  filedir='fst',
                                  filename=paste0(v, 'Ex_',
                                                  monthStart,
                                                  '.fst'))
                }
            }
        }
    }
}


## 2. STATION BREAK ANALYSIS _________________________________________
if ('station_break_analyse' %in% to_do) {
    DF_BREAK = list()
    # For all the variable
    for (v in var) {
        # Gets the trend results for the variable
        res_trend = get(paste('res_', v, 'trend', sep=''))
        # Performs the break analyses for some hydrological variables
        df_break = get_break(res_trend$data, df_meta, alpha=0.1)
        DF_BREAK = append(DF_BREAK, list(df_break))
    }
    names(DF_BREAK) = var
}


## 3. CLIMATE TREND ANALYSIS _________________________________________
if ('climate_trend_analyse' %in% to_do) {
### 3.1. Info about analysis _________________________________________
    var_all_climate = list(
        'PA',
        'TA',
        'ETPA'
    )
    type_all_climate = list(
        'pluviométrie',
        'température',
        'évapotranspiration'
    )
    glose_all_climate = list(
        '',
        '',
        ''
    )

### 3.2. Selection of variables ______________________________________
    var_climate = c()
    type_climate = c()
    glose_climate = c()
    for (OkVar in to_analyse_climate) {
        Ok = var_all_climate == OkVar
        var_climate = c(var_climate, var_all_climate[Ok])
        type_climate = c(type_climate, type_all_climate[Ok])
        glose_climate = c(glose_climate, glose_all_climate[Ok])
    } 
    
### 3.3. Formatting of climate dataframe _____________________________
    # For precipitation
    df_data_P = bind_cols(Date=df_climate_data$Date,
                          Value=df_climate_data$PRCP_mm,
                          code=df_climate_data$code)
    # For temperature
    df_data_T = bind_cols(Date=df_climate_data$Date,
                          Value=df_climate_data$T_degC,
                          code=df_climate_data$code)
    # For evapotranspiration
    df_data_ETP = bind_cols(Date=df_climate_data$Date,
                            Value=df_climate_data$PET_mm,
                            code=df_climate_data$code)
### 3.4. Trend analyses ______________________________________________
    # TA trend
    res = get_XAtrend(df_data_P, df_climate_meta,
                      period=trend_period,
                      perStart='09-01',
                      alpha=alpha,
                      dayLac_lim=dayLac_lim,
                      yearNA_lim=yearNA_lim,
                      df_flag=df_flag,
                      funct=sum)
    df_PAdata = res$data
    df_PAmod = res$mod
    res_PAtrend = res$analyse
    
    # PA trend
    res = get_XAtrend(df_data_T, df_climate_meta,
                      period=trend_period,
                      perStart='09-01',
                      alpha=alpha,
                      dayLac_lim=dayLac_lim,
                      yearNA_lim=yearNA_lim,
                      df_flag=df_flag,
                      funct=mean,
                      na.rm=TRUE)
    df_TAdata = res$data
    df_TAmod = res$mod
    res_TAtrend = res$analyse

    # ETPA trend
    res = get_XAtrend(df_data_ETP, df_climate_meta,
                      period=trend_period,
                      perStart='09-01',
                      alpha=alpha,
                      dayLac_lim=dayLac_lim,
                      yearNA_lim=yearNA_lim,
                      df_flag=df_flag,
                      funct=sum)
    df_ETPAdata = res$data
    df_ETPAmod = res$mod
    res_ETPAtrend = res$analyse
}
