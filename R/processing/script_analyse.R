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
source(file.path('R', 'processing', 'read_write.R'),
       encoding='UTF-8')


## 1. STATION TREND ANALYSIS _________________________________________
if ('station_trend_analyse' %in% to_do) {

    script_to_analyse_dirpath = file.path('R', var_dir, var_to_analyse_dir)
    
    script_to_analyse = list.files(var_to_analyse_dirpath,
                                   pattern="[^default].R$",
                                   recursive=TRUE,
                                   include.dirs=FALSE,
                                   full.names=FALSE)
    
    # scriptNoDir = gsub('.*[/]', '', var_to_analyse_script)
    # scriptNoNum = gsub('.*_', '', scriptNoDir)
    # script_duplicate = !duplicated(scriptNoNum)

    # script_to_analyse = var_to_analyse_script[script_duplicate]
    
    event_to_analyse = list.dirs(var_to_analyse_dirpath,
                                 recursive=TRUE, full.names=FALSE)
    event_to_analyse = event_to_analyse[event_to_analyse != ""]

    structure = replicate(length(event_to_analyse), c())
    names(structure) = event_to_analyse
    
    var_analyse = c()
    type_analyse = c()
    glose_analyse = c()
    df_data_analyse = list()
    df_trend_analyse = list()
    
### 1.3. Trend analyses ______________________________________________
    for (script in script_to_analyse) {

        if (hydroYear_mode == 'every') {
            nHydroYear = 12
        } else {
            nHydroYear = 1
        }
            
        for (iHY in 1:nHydroYear) {

            source(file.path('R', var_dir, init_var_file),
                             encoding='UTF-8')
            source(file.path(var_to_analyse_dirpath, script),
                   encoding='UTF-8')

            script_path_split = split_path(script)

            for (x in script_path_split) {
                if (file_ext(x) == ".R") {
                    
                    
                    
                }


                    
                
            }
            
            if (length(script_path_split) ) {
                
                structure
                
            }

            
            
            if (var %in% var_analyse) {
                next
            }
            
            if (hydroYear_mode == 'every') {
                hydroYear = paste0(formatC(iHY, width=2, flag="0"),
                                   '-01')
            }
            monthHydroYear = substr(hydroYear, 1, 2)

            var_analyse = c(var_analyse, var)
            type_analyse = c(type_analyse, type)
            glose_analyse = c(glose_analyse, glose)
            
            res = get_Xtrend(var,
                             df_data, df_meta,
                             period=trend_period,
                             hydroYear=hydroYear,
                             alpha=alpha,
                             df_flag=df_flag,
                             sampleSpan=sampleSpan,
                             yearNA_lim=yearNA_lim,
                             dayLac_lim=dayLac_lim,
                             NA_pct_lim=NA_pct_lim,
                             day_to_roll=day_to_roll,
                             functM=functM,
                             functM_args=functM_args,
                             isDateM=isDateM,
                             functY=functY,
                             functY_args=functY_args,
                             isDateY=isDateY,
                             functYT_ext=functYT_ext,
                             functYT_ext_args=functYT_ext_args,
                             isDateYT_ext=isDateYT_ext,
                             functYT_sum=functYT_sum,
                             functYT_sum_args=functYT_sum_args)
            
            df_Xdata = res$data
            df_Xmod = res$mod
            res_Xanalyse = res$analyse
            # Gets the extracted data for the variable
            df_XEx = res_Xanalyse$extract
            # Gets the trend results for the variable
            df_Xtrend = res_Xanalyse$estimate

            if ('data' %in% to_assign_out) {
                assign(paste0('df_', var, 'data'), df_Xdata)
                assign(paste0('df_', var, 'mod'), df_Xmod)
            }
            
            if ('analyse' %in% to_assign_out) {
                assign(paste0('res_', var, 'analyse'), res_Xanalyse)
                assign(paste0('df_', var, 'Ex'), df_XEx)
                assign(paste0('df_', var, 'trend'), df_Xtrend)
            }

            if ('station_trend_plot' %in% to_do) {
                df_data_analyse = append(df_data_analyse, list(df_XEx))
                df_trend_analyse = append(df_trend_analyse, list(df_Xtrend))
            }

### 1.3. Saving ______________________________________________________
            if ('data' %in% saving) {
                # Writes modified data
                write_data(df_Xdata, df_Xmod, resdir,
                           filedir=file.path('modified_data',
                                             var, monthHydroYear))
                
                if (fast_format) {
                    write_dataFST(df_Xdata, resdir,
                                  filedir='fst',
                                  filename=paste0('data_', var,
                                                  '_', monthHydroYear,
                                                  '.fst'))
                }
            }

            if ('analyse' %in% saving) {                
                # Writes trend analysis results
                write_analyse(res_Xanalyse, resdir,
                              filedir=file.path('trend_analyses',
                                                var, monthHydroYear))
                
                if (fast_format) {
                    write_dataFST(df_XEx,
                                  resdir,
                                  filedir='fst',
                                  filename=paste0(var, 'Ex_',
                                                  monthHydroYear,
                                                  '.fst'))
                }
            }
        }
    }
}

if ('meta' %in% saving) {
    if (fast_format) {
        write_metaFST(df_meta, resdir,
                      filedir=file.path('fst'))
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
    res = get_Xtrend(df_data_P, df_climate_meta,
                      period=trend_period,
                      hydroYear='09-01',
                      alpha=alpha,
                      dayLac_lim=dayLac_lim,
                      yearNA_lim=yearNA_lim,
                      df_flag=df_flag,
                      funct=sum)
    df_PAdata = res$data
    df_PAmod = res$mod
    res_PAtrend = res$analyse
    
    # PA trend
    res = get_Xtrend(df_data_T, df_climate_meta,
                      period=trend_period,
                      hydroYear='09-01',
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
    res = get_Xtrend(df_data_ETP, df_climate_meta,
                      period=trend_period,
                      hydroYear='09-01',
                      alpha=alpha,
                      dayLac_lim=dayLac_lim,
                      yearNA_lim=yearNA_lim,
                      df_flag=df_flag,
                      funct=sum)
    df_ETPAdata = res$data
    df_ETPAmod = res$mod
    res_ETPAtrend = res$analyse
}



## 4. TOOLS __________________________________________________________
split_path = function (path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}
