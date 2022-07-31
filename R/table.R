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
# R/plotting/table.R
#
# Allows the creation of a summarizing table of trend and break analyses


## 1. TABLE PANEL ___________________________________________________
# Generates a summarizing table of the trend analyses of all station
# for different hydrological variables and periods. Also shows
# difference of means between specific periods.
#' @title Table panel
#' @export
table_panel = function (list_df2plot, df_meta, trend_period,
                        mean_period, colorForce=FALSE, exQprob=0.01,
                        slice=NULL,
                        title=NULL, paper_size='A3',
                        foot_note=FALSE, foot_height=0, resdir=NULL,
                        logo_path=NULL,
                        outdirTmp_pdf='',
                        outdirTmp_png='',
                        df_page=NULL) {

    # Number of variable/plot
    nVar = length(list_df2plot)
    
    # Get all different stations code
    Code = rle(df_data$Code)$value    
    nCode = length(Code)

    # Convert 'trend_period' to list
    trend_period = as.list(trend_period)
    # Number of trend period
    nPeriod_trend = length(trend_period)
    
    # Extracts the min and the max of the mean trend for all the station
    res = get_valueExtremes(list_df2plot, Code,
                            nPeriod_trend, nVar,
                            nCode,
                            valueType="trend",
                            colorForce=colorForce,
                            minQprob=exQprob, maxQprob=1-exQprob)
    minTrendValue = res$min
    maxTrendValue = res$max

    # Blank vectors to store info about trend analyses
    Period_trend = c()
    NPeriod_trend = c()
    Var_trend = c()
    Type_trend = c()
    Code_trend = c()
    Alpha_trend = c()
    TrendValue_trend = c()
    DataMean_trend = c()
    Fill_trend = c()
    Color_trend = c()
    # For all the trend period
    for (j in 1:nPeriod_trend) {
        # For all code
        for (k in 1:nCode) {
            # Gets the code
            code = Code[k]
            # For all variable
            for (i in 1:nVar) {
                # Extracts the data corresponding to the current variable
                df_data = list_df2plot[[i]]$data
                # Extracts the trend corresponding to the
                # current variable
                df_trend = list_df2plot[[i]]$trend
                alpha = list_df2plot[[i]]$alpha
                # Extract the variable of the plot
                var = list_df2plot[[i]]$var
                # Extract the type of the variable to plot
                type = list_df2plot[[i]]$type
                # Extracts the data corresponding to the code
                df_data_code = df_data[df_data$Code == code,]
                # Extracts the trend corresponding to the code
                df_trend_code = df_trend[df_trend$Code == code,]

                # Extract start and end of trend periods
                Start = df_trend_code$period_start[j]
                End = df_trend_code$period_end[j]

                StartY = format(Start, '%Y')
                EndY = format(End, '%Y')
                
                # Creates a period name
                Period = paste(StartY, EndY,
                                sep=' / ')

                # Extracts the corresponding data for the period
                df_data_code_per =
                    df_data_code[df_data_code$Date >= Start 
                                 & df_data_code$Date <= End,]
                # Same for trend
                df_trend_code_per = 
                    df_trend_code[df_trend_code$period_start == Start 
                                  & df_trend_code$period_end == End,]

                # Computes the number of trend analysis selected
                Ntrend = nrow(df_trend_code_per)
                # If there is more than one trend on the same period
                if (Ntrend > 1) {
                    # Takes only the first because they are similar
                    df_trend_code_per = df_trend_code_per[1,]
                }

                # Computes the mean of the data on the period
                dataMean = mean(df_data_code_per$Value, na.rm=TRUE)

                # If it is a flow variable
                if (type == 'sévérité') {
                    # Normalises the trend value by the mean of the data
                    trendValue = df_trend_code_per$trend / dataMean
                # If it is a date variable
                } else if (type == 'saisonnalité') {
                    # Just stocks the trend value
                    trendValue = df_trend_code_per$trend
                }

                # Gets the color associated to the averaged trend
                color_res = get_color(trendValue,
                                      minTrendValue[j, i],
                                      maxTrendValue[j, i],
                                      Palette=Palette_ground(),
                                      colorStep=10,
                                      reverse=FALSE)

                pVal = df_trend_code_per$p
                
                # If the p value is under the threshold
                if (pVal <= alpha){
                    # Specifies the color fill and contour of
                    # table cells
                    fill = color_res
                    color = color_res
                    Alpha = 'TRUE'
                    
                } else if (pVal > alpha & colorForce) {
                    # Specifies the color fill and contour of
                    # table cells
                    fill = 'white'
                    color = color_res
                    Alpha = 'FORCE'
                # Otherwise it is not significative
                } else { 
                    fill = 'white'
                    color = 'grey80'  
                    Alpha = 'FALSE'
                }

                # Stores info needed to plot
                Period_trend = append(Period_trend, Period)
                NPeriod_trend = append(NPeriod_trend, j)
                Var_trend = append(Var_trend, var)
                Type_trend = append(Type_trend, type)
                Code_trend = append(Code_trend, code)
                Alpha_trend = append(Alpha_trend, Alpha)
                TrendValue_trend = append(TrendValue_trend, trendValue)
                DataMean_trend = append(DataMean_trend, dataMean)
                Fill_trend = append(Fill_trend, fill)
                Color_trend = append(Color_trend, color)
            }
        }
    }
    
    # If there is a 'mean_period'
    if (!is.null(mean_period)) {
        # Convert 'mean_period' to list
        mean_period = as.list(mean_period)
        # Number of mean period
        nPeriod_mean = length(mean_period)

        res = get_valueExtremes(list_df2plot, Code,
                                nPeriod_mean, nVar, nCode,
                                valueType="break",
                                minQprob=exQprob, maxQprob=1-exQprob)
        minBreakValue = res$min
        maxBreakValue = res$max
    } else {
        nPeriod_mean = 1
    }

    # If there is a 'mean_period'
    if (!is.null(mean_period)) {
        # Blank vectors to store info about breaking analysis
        Period_mean = c()
        NPeriod_mean = c()
        Var_mean = c()
        Type_mean = c()
        Code_mean = c()
        DataMean_mean = c()
        BreakValue_mean = c()
        
        # Blank array to store mean for a temporary period in order
        # to compute the difference of mean with a second period
        dataMeantmp = array(rep(NA, nVar*nCode),
                            dim=c(nVar, nCode))
        
        # For all period of breaking analysis
        for (j in 1:nPeriod_mean) {
            # For all the code
            for (k in 1:nCode) {
                # Gets the code
                code = Code[k]
                # For all variable
                for (i in 1:nVar) {
                    # Extracts the data corresponding to
                    # the current variable
                    df_data = list_df2plot[[i]]$data
                    # Extract the variable of the plot
                    var = list_df2plot[[i]]$var
                    # Extract the type of the variable to plot
                    type = list_df2plot[[i]]$type
                    # Extracts the data corresponding to the code
                    df_data_code = df_data[df_data$Code == code,] 
                    
                    # Get the current start and end of the sub period
                    Start_mean = mean_period[[j]][1]
                    End_mean = mean_period[[j]][2]
                    
                    # Extract the data corresponding to this sub period
                    df_data_code_per =
                        df_data_code[df_data_code$Date >= Start_mean 
                                     & df_data_code$Date <= End_mean,]
                    
                    # Min max for the sub period
                    Start = min(df_data_code_per$Date)
                    End = max(df_data_code_per$Date)

                    StartY = format(Start, '%Y')
                    EndY = format(End, '%Y')
                    
                    # Creates a period name
                    Period = paste(StartY, EndY,
                                    sep=' / ')

                    # Mean of the flow over the sub period
                    dataMean = mean(df_data_code_per$Value,
                                    na.rm=TRUE)

                    # If this in not the first period
                    if (j > 1) {
                        # Compute the difference of mean
                        Break = dataMean - dataMeantmp[i, k]
                    # Otherwise for the first period
                    } else {
                        # Stocks NA
                        Break = NA
                    }

                    # If it is a flow variable
                    if (type == 'sévérité') {
                        # Normalises the break by the mean of the
                        # initial period
                        breakValue = Break / dataMeantmp[i, k]
                    # If it is a date variable
                    } else if (type == 'saisonnalité') {
                        # Just stocks the break value
                        breakValue = Break
                    }
                    
                    # Stores temporarily the mean of the current period
                    dataMeantmp[i, k] = dataMean
                    
                    # Stores info needed to plot
                    Period_mean = append(Period_mean, Period)
                    NPeriod_mean = append(NPeriod_mean, j)
                    Var_mean = append(Var_mean, var)
                    Type_mean = append(Type_mean, type)
                    Code_mean = append(Code_mean, code)
                    DataMean_mean = append(DataMean_mean, dataMean)
                    BreakValue_mean = append(BreakValue_mean,
                                            breakValue)
                }
            }
        }

        # Blanks vector to store color info
        Fill_mean = c()
        Color_mean = c()
        # Index to count over all break computed
        ii = 1
        for (j in 1:nPeriod_mean) {
            # For all the code
            for (k in 1:nCode) {
                # Gets the code
                code = Code[k]
                # For all variable
                for (i in 1:nVar) {
                    # Extracts averaged breaking
                    breakValue = BreakValue_mean[ii]
                    # Gets the color associated
                    color_res = get_color(breakValue,
                                          minBreakValue[j, i],
                                          maxBreakValue[j, i],
                                          Palette=Palette_ground(),
                                          colorStep=10,
                                          reverse=FALSE)
                    # Gets the fill and contour color
                    fill = color_res
                    color = 'white'
                    # Stores it
                    Fill_mean = append(Fill_mean, fill)
                    Color_mean = append(Color_mean, color)
                    # Passes to the next index
                    ii = ii + 1
                }
            }
        }
    }

    # If the slice option is not specified, the info for all
    # stations will be draw on the same page 
    if (is.null(slice)) {
        slice = nCode
    }

    allType = c()
    for (i in 1:nVar) {
        allType = c(allType, list_df2plot[[i]]$type)
    }
    
    countType = rle(sort(allType))
    df_countType = tibble(type=countType$values, n=countType$lengths)
    nVarMax = max(df_countType$n)
    
    # Gets all the different type of plots
    Type = levels(factor(allType))
    nbType = length(Type)

    # Number of pages
    N_loop = 0
    # For all the type of plots
    for (itype in 1:nbType) {
        # Gets the type
        type = Type[itype]
        # Extracts each possibilities of hydrological region
        RH = rle(sort(df_meta$region_hydro))$values
        twoL = names(df_meta$region_hydro)
        # Number of different first letters
        nRH = length(RH)

        df_table = tibble()
        df_table_S = tibble()
        
        # For all the available first letter
        for (iR in 1:nRH) {

            rh = RH[iR]
            okL = rle(sort(twoL[df_meta$region_hydro == rh]))$values
            nL = nchar(okL[1])
            # Get only station code with the same first letter 
            subCodeRh = Code[substr(Code, 1, nL) %in% okL]
            # Counts the number of station in it
            nsubCodeRh = length(subCodeRh)
            # Computes the number of pages needed to plot
            # all stations
            nMat = as.integer(nsubCodeRh/slice) + 1
            # Counts the number of pages
            N_loop = N_loop + nMat

            df_tableRH = tibble()
            df_tableRH_S = tibble()
            
            for (code in subCodeRh) {

                OK = code == Code_trend & type == Type_trend
                
                df_ligne = tibble(Code=code)
                df_ligne_S = tibble(Code=code)
                df_unit = tibble(Code='')
                
                for (j in 1:nPeriod_trend) {

                    OKper = OK & j == NPeriod_trend

                    period = Period_trend[OKper][1]

                    start = substr(period, 1, 4)
                    end = substr(period, 8, 11)
                    
                    startC = paste('start_', j, sep='')
                    endC = paste('end_', j, sep='')
                    
                    df_ligne = bind_cols(df_ligne,
                                         tibble(start,
                                                end))
                    colnames(df_ligne)[ncol(df_ligne)-1] = startC
                    colnames(df_ligne)[ncol(df_ligne)] = endC

                    df_ligne_S = bind_cols(df_ligne_S,
                                         tibble(start,
                                                end))
                    colnames(df_ligne_S)[ncol(df_ligne_S)-1] = startC
                    colnames(df_ligne_S)[ncol(df_ligne_S)] = endC
                    
                    df_unit =  bind_cols(df_unit,
                                         tibble(a='année', b='année'))
                    colnames(df_unit)[ncol(df_unit)-1] = startC
                    colnames(df_unit)[ncol(df_unit)] = endC

                    Var = Var_trend[OKper]
                    MeanVal = DataMean_trend[OKper]
                    TrendVal = TrendValue_trend[OKper]
                    Alpha = Alpha_trend[OKper]
                    nVar_trend = length(Var)

                    for (i in 1:nVar_trend) {
                        varC = paste(Var[i], 'bar_', j, sep='')
                        meanVal = MeanVal[i]
                        if (type == 'sévérité') {
                            unit = 'm3.s-1'
                        } else if (type == 'saisonnalité') {
                            unit = 'jour'
                        }
                        meanValC = as.character(signif(meanVal, 4))

                        df_ligne = bind_cols(df_ligne,
                                             tibble(meanValC))
                        colnames(df_ligne)[ncol(df_ligne)] = varC

                        df_ligne_S = bind_cols(df_ligne_S,
                                             tibble(meanValC))
                        colnames(df_ligne_S)[ncol(df_ligne_S)] = varC

                        df_unit = bind_cols(df_unit,
                                             tibble(unit))
                        colnames(df_unit)[ncol(df_unit)] = varC
                    }
                    for (i in 1:nVar_trend) {
                        varC = paste(Var[i], '_', j, sep='')
                        trendVal = TrendVal[i]
                        if (type == 'sévérité') {
                            trendVal = trendVal * 100
                            unit = '%.an-1'
                        } else if (type == 'saisonnalité') {
                            unit = 'jour.an-1'
                        }
                        trendValC = as.character(signif(trendVal, 4))

                        if (Alpha[i] == 'TRUE') {
                            trendValC_S = trendValC
                        } else {
                            trendValC_S = ''
                        }
                        
                        df_ligne = bind_cols(df_ligne,
                                             tibble(trendValC))
                        colnames(df_ligne)[ncol(df_ligne)] = varC

                        df_ligne_S = bind_cols(df_ligne_S,
                                               tibble(trendValC_S))
                        colnames(df_ligne_S)[ncol(df_ligne_S)] = varC
                        
                        df_unit = bind_cols(df_unit,
                                            tibble(unit))
                        colnames(df_unit)[ncol(df_unit)] = varC
                    }
                }

                
                for (j in 1:nPeriod_mean) {

                    OKper = OK & j == NPeriod_mean

                    period = Period_mean[OKper][1]

                    start = substr(period, 1, 4)
                    end = substr(period, 8, 11)
                    
                    startC = paste('start_', j+nVar_trend, sep='')
                    endC = paste('end_', j+nVar_trend, sep='')
                    
                    df_ligne = bind_cols(df_ligne,
                                         tibble(start,
                                                end))
                    colnames(df_ligne)[ncol(df_ligne)-1] = startC
                    colnames(df_ligne)[ncol(df_ligne)] = endC

                    df_ligne_S = bind_cols(df_ligne_S,
                                           tibble(start,
                                                  end))
                    colnames(df_ligne_S)[ncol(df_ligne_S)-1] = startC
                    colnames(df_ligne_S)[ncol(df_ligne_S)] = endC
                    
                    df_unit =  bind_cols(df_unit,
                                         tibble(a='année', b='année'))
                    colnames(df_unit)[ncol(df_unit)-1] = startC
                    colnames(df_unit)[ncol(df_unit)] = endC

                    Var = Var_mean[OKper]
                    MeanVal = DataMean_mean[OKper]
                    BreakVal = BreakValue_mean[OKper]
                    nVar_mean = length(Var)
                    
                    for (i in 1:nVar_mean) {
                        varC = paste(Var[i], 'bar_',
                                     j+nVar_trend, sep='')
                        meanVal = MeanVal[i]
                        if (type == 'sévérité') {
                            unit = 'm3.s-1'
                        } else if (type == 'saisonnalité') {
                            unit = 'jour'
                        }
                        meanValC = as.character(signif(meanVal, 4))

                        df_ligne = bind_cols(df_ligne,
                                             tibble(meanValC))
                        colnames(df_ligne)[ncol(df_ligne)] = varC

                        df_ligne_S = bind_cols(df_ligne_S,
                                               tibble(meanValC))
                        colnames(df_ligne_S)[ncol(df_ligne_S)] = varC
                        
                        df_unit = bind_cols(df_unit,
                                            tibble(unit))
                        colnames(df_unit)[ncol(df_unit)] = varC
                    }
                    if (j > 1) {
                        for (i in 1:nVar_mean) {
                            varC = paste('d', Var[i], '_',
                                         j+nVar_trend, sep='')
                            breakVal = BreakVal[i]
                            if (type == 'sévérité') {
                                breakVal = breakVal * 100
                                unit = '%'
                            } else if (type == 'saisonnalité') {
                                unit = 'jour'
                            }
                            breakValC = as.character(signif(breakVal, 4)) 
                            
                            df_ligne = bind_cols(df_ligne,
                                                 tibble(breakValC))
                            colnames(df_ligne)[ncol(df_ligne)] = varC
                            
                            df_ligne_S = bind_cols(df_ligne_S,
                                                   tibble(breakValC))
                            colnames(df_ligne_S)[ncol(df_ligne_S)] = varC
                            
                            df_unit = bind_cols(df_unit,
                                                tibble(unit))
                            colnames(df_unit)[ncol(df_unit)] = varC
                        }
                    }
                }                
                df_tableRH = bind_rows(df_tableRH, df_ligne)
                df_tableRH_S = bind_rows(df_tableRH_S, df_ligne_S)
            }
            df_tableRH = bind_cols(bassin=rh, df_tableRH)
            df_tableRH_S = bind_cols(bassin=rh, df_tableRH_S)
            
            df_table = bind_rows(df_table, df_tableRH)
            df_table_S = bind_rows(df_table_S, df_tableRH_S)
        }
        df_unit$bassin = ''
        df_table = bind_rows(df_unit, df_table)
        df_table_S = bind_rows(df_unit, df_table_S)

        if (!is.null(resdir)) {
            outdir = file.path(resdir, 'summary_tables')
            if (!(file.exists(outdir))) {
                dir.create(outdir, recursive=TRUE)
            }
            
            print(paste('Writing table of ', type, ' in : ', outdir,
                        sep=''))
            
            outfile = paste('table_', type, '.txt', sep='')
            
            write.table(df_table,
                        file=file.path(outdir, outfile),
                        sep=";",
                        quote=FALSE,
                        row.names=FALSE)

            outfile = paste('table_', type, '_signif.txt', sep='')

            write.table(df_table_S,
                        file=file.path(outdir, outfile),
                        sep=";",
                        quote=FALSE,
                        row.names=FALSE)
        }
    }

    # For all the type of plots
    for (itype in 1:nbType) {
        # Gets the type
        type = Type[itype]
        # Extracts each possibilities of hydrological region
        RH = rle(sort(df_meta$region_hydro))$values
        twoL = names(df_meta$region_hydro)
        # Number of different first letters
        nRH = length(RH)
        # For all the available first letter
        for (iR in 1:nRH) {
            # Gets the first letter
            rh = RH[iR]
            okL = rle(sort(twoL[df_meta$region_hydro == rh]))$values
            nL = nchar(okL[1])
            # Get only station code with the same first letter 
            subCodeRh = Code[substr(Code, 1, nL) %in% okL]
            # Counts the number of station in it
            nsubCodeRh = length(subCodeRh)
            # Computes the number of pages needed to
            # plot all stations
            nMat = as.integer(nsubCodeRh/slice) + 1
            # For all the pages
            for (iMat in 1:nMat) {
                n_loop = iR + nRH*(itype-1) + (iMat-1)
                # Print the table name
                print(paste('Table ', iMat, '/', nMat,
                            ' of ', type,
                            ' for region : ', rh,
                            "   (",
                            round(n_loop / N_loop * 100,
                                  0),
                            " %)", 
                            sep=''))
                
                # Extracts the station for the current page
                subCode = subCodeRh[(slice*(iMat-1)+1):(slice*iMat)]
                # Removes NA stations
                subCode = subCode[!is.na(subCode)]
                # Reverses verticale order of stations
                subCode = rev(subCode)
                # Gets the number of station for the page
                nsubCode = length(subCode)

                # Creates logical vector to select only info about
                # stations that will be plot on the page
                OKtrend =
                    Code_trend %in% subCode & Type_trend == type
                # Extracts those info
                subPeriod_trend = Period_trend[OKtrend]
                subNPeriod_trend = NPeriod_trend[OKtrend]
                subVar_trend = Var_trend[OKtrend]
                subType_trend = Type_trend[OKtrend]
                subCode_trend = Code_trend[OKtrend]
                subAlpha_trend = Alpha_trend[OKtrend]
                subTrendValue_trend = TrendValue_trend[OKtrend]
                subDataMean_trend = DataMean_trend[OKtrend]
                subFill_trend = Fill_trend[OKtrend]
                subColor_trend = Color_trend[OKtrend]

                # Same for breaking analysis
                OKmean =
                    Code_mean %in% subCode & Type_mean == type
                # Extracts right info
                subPeriod_mean = Period_mean[OKmean]
                subNPeriod_mean = NPeriod_mean[OKmean]
                subVar_mean = Var_mean[OKmean]
                subType_mean = Type_mean[OKmean]
                subCode_mean = Code_mean[OKmean]
                subDataMean_mean = DataMean_mean[OKmean]
                subBreakValue_mean = BreakValue_mean[OKmean]
                subFill_mean = Fill_mean[OKmean]
                subColor_mean = Color_mean[OKmean]
                
                # Gets the number of variable to plot in
                # function of the current type
                nVarMod =
                    length(levels(factor(subVar_trend)))                
                
                ### Plot ###
                # Fixes the height and width of the table according to
                # the number of station and the number of column to draw
                height = nsubCode
                # width = nVarMod * 2 * nPeriod_trend + nPeriod_trend + nPeriod_mean * nVarMod + nPeriod_mean + nVarMod
                
                width = nVarMax * 2 * nPeriod_trend + nPeriod_trend + nPeriod_mean * nVarMax + nPeriod_mean + nVarMax
                

                # Fixes the size of the plot area to keep proportion right
                options(repr.plot.width=width, repr.plot.height=height)

                # Open a new plot with a personalise theme
                mat = ggplot() + theme_ash() + 
                    # Modification of theme in order to remove axis
                    theme(
                        panel.border=element_blank(),
                        axis.text.x=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.title.y=element_blank(),
                        plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm")
                    )

                colorBack = 'grey94'
                radius = 0.43

                if (nVarMod == 2) {
                    periodSize = 2.8
                } else {
                    periodSize = 3.5
                }

                # Extracts the name of the currently hydrological
                # region plotted
                title = df_meta[df_meta$Code == subCode[1],]$region_hydro

                subtitle = paste(type, ' ', iMat, '/', nMat,
                                 sep='')    
                # Postion and name of the title
                xt = 1 - 6
                yt = height + 2
                Title = bquote(bold(.(title))~'-'~.(subtitle))
                # Writes the title
                mat = mat +
                    annotate("text", x=xt, y=yt,
                             label=Title,
                             hjust=0, vjust=1, 
                             size=6, color="#00A3A8")

                ### Trend ###
                # For all the trend period
                for (j in 1:nPeriod_trend) {
                    # Extracts the info to plot associated to the
                    # right period
                    Period_trend_per =
                        subPeriod_trend[subNPeriod_trend == j]
                    NPeriod_trend_per =
                        subNPeriod_trend[subNPeriod_trend == j]
                    Var_trend_per =
                        subVar_trend[subNPeriod_trend == j]
                    Type_trend_per =
                        subType_trend[subNPeriod_trend == j]
                    Code_trend_per =
                        subCode_trend[subNPeriod_trend == j]
                    Alpha_trend_per =
                        subAlpha_trend[subNPeriod_trend == j]
                    TrendValue_trend_per =
                        subTrendValue_trend[subNPeriod_trend == j]
                    DataMean_trend_per =
                        subDataMean_trend[subNPeriod_trend == j]
                    Fill_trend_per =
                        subFill_trend[subNPeriod_trend == j]
                    Color_trend_per =
                        subColor_trend[subNPeriod_trend == j]

                    # Converts the variable list into levels for factor
                    levels = unlist(subVar_trend[1:nVarMod])
                    
                    # Converts the vector of hydrological variable to
                    # a vector of integer associated to those variable
                    Xtmp = as.integer(factor(as.character(Var_trend_per),
                                             levels=levels))
                    
                    # Computes X position of the column for
                    # the period dates
                    Xc = j + (j - 1)*nVarMod*2
                    # Computes X positions of columns for
                    # the mean of variables
                    Xm = Xtmp + (j - 1)*nVarMod*2 + j
                    # Computes X positions of columns for
                    # the averaged trend
                    X = Xtmp + (j - 1)*nVarMod*2 + nVarMod + j
                    
                    # Computes Y positions of each line for each station
                    Y = as.integer(factor(Code_trend_per))
                    # Reverses vertical order of stations
                    Y = rev(Y)

                    # Position of a line to delimite periods
                    x = Xc - 0.4
                    xend = X[length(X)] + 0.4
                    y = height + 1.13
                    # Drawing of the line
                    mat = mat +
                        annotate("segment",
                                 x=x, xend=xend,
                                 y=y, yend=y, 
                                 color="grey40", size=0.35)

                    # Position of the name of the current period
                    yt = y + 0.03
                    Start = trend_period[[j]][1]
                    End = trend_period[[j]][2]
                    
                    # Name of the period
                    # periodName =
                        # bquote(bold('Période')~bold(.(as.character(j))))
                    if (j == 1) {
                        periodName = bquote(bold("Analyse de tendance sur la série entière"))
                    } else if (j == 2) {
                        periodName = bquote(bold("Analyse de tendance sur la période commune"))
                    }
                    
                    # Naming the period
                    mat = mat +
                        annotate("text", x=x, y=yt,
                                 label=periodName,
                                 hjust=0, vjust=0, 
                                 size=periodSize, color='grey40')
                    
                    # For all the variable
                    for (i in 1:length(X)) {
                        mat = mat +
                            # Plots circles for averaged trends
                            gg_circle(r=radius, xc=X[i], yc=Y[i],
                                      fill=Fill_trend_per[i],
                                      color=Color_trend_per[i],
                                      size=0.75) +
                            # Plots circles for averaged of variables
                            gg_circle(r=radius, xc=Xm[i], yc=Y[i],
                                      fill=colorBack, color=colorBack,
                                      size=0.75) +
                            # Plots circles for the column of period dates
                            gg_circle(r=radius, xc=Xc, yc=Y[i],
                                      fill=colorBack, color=colorBack,
                                      size=0.75) 
                    }

                    # For all averaged trends on this periods
                    for (i in 1:length(TrendValue_trend_per)) {
                        # Extracts the value of the averaged trend
                        trendValue = TrendValue_trend_per[i]
                        type = Type_trend_per[i]

                        # If it is a flow variable
                        if (type == 'sévérité') {
                            Nsign_mean = 2
                            # Converts it to the right format with
                            # two significant figures
                            trendValueC = signif(trendValue*100, 2)
                        # If it is a date variable
                        } else if (type == 'saisonnalité') {
                            # Fixes the significants number for mean to 3
                            Nsign_mean = 3
                            # Converts the trend value with two
                            # significant figures
                            trendValueC = signif(trendValue, 2)
                        }
                        trendValueC = as.character(trendValueC)
                        
                        # If it is significative
                        if (Alpha_trend_per[i] == 'TRUE') {
                            # The text color is white
                            Tcolor = 'white'
                            
                        } else if (Alpha_trend_per[i] == 'FORCE') {
                            Tcolor = Color_trend_per[i]
                        # Otherwise
                        } else if (Alpha_trend_per[i] == 'FALSE') {
                            # The text is grey
                            Tcolor = 'grey80'
                        }
                        
                        # Same for averaged variables over
                        # the current period
                        dataMean = DataMean_trend_per[i]
                        dataMeanC = signif(dataMean, Nsign_mean)

                        mat = mat +
                            # Writes the mean trend
                            annotate('text', x=X[i], y=Y[i],
                                     label=bquote(bold(.(trendValueC))),
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color=Tcolor) + 
                            # Writes the mean of the associated variable
                            annotate('text', x=Xm[i], y=Y[i],
                                     label=dataMeanC,
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color='grey40')
                    }
                    
                    # Writes a name for the period dates column
                    mat = mat +
                        annotate('text', x=Xc, y=max(Y) + 0.9,
                                 label=bquote(bold('Début')),
                                 hjust=0.5, vjust=0.5, 
                                 size=3, color='grey20') + 
                        annotate('text', x=Xc, y=max(Y) + 0.63,
                                 label=bquote(bold('Fin')),
                                 hjust=0.5, vjust=0.5, 
                                 size=3, color='grey20')
                    
                    # For all variable
                    for (i in 1:nVarMod) {
                        # Extract the variable of the plot
                        var = subVar_trend[i]
                        type = subType_trend[i]
                        
                        # If it is a flow variable
                        if (type == 'sévérité') {
                            # Fixes the unit of the mean and the trend
                            # for the flow
                            unit_mean = bquote('['*m^3*'.'*s^{-1}*']')
                            unit_trend = bquote('[%.'*an^{-1}*']')
                        # If it is a date variable
                        } else if (type == 'saisonnalité') {
                            # Fixes the unit of the mean and the trend
                            # for the date
                            unit_mean = bquote('[jour]')
                            unit_trend = bquote('[jour.'*an^{-1}*']')
                        }
                        
                        mat = mat +
                            # Writes the unit of the variable
                            annotate('text', x=X[i], y=max(Y) + 0.63,
                                     label=unit_trend,
                                     hjust=0.5, vjust=0.5, 
                                     size=2, color='grey40') +
                            # Writes the type of the variable
                            annotate('text', x=X[i], y=max(Y) + 0.9,
                                     label=bquote(.(var)),
                                     hjust=0.5, vjust=0.5, 
                                     size=3.25, color='grey20') +
                            # Writes the unit of the averaged variable
                            annotate('text', x=Xm[i], y=max(Y) + 0.63,
                                     label=unit_mean,
                                     hjust=0.5, vjust=0.5, 
                                     size=2, color='grey40') +
                            # Writes the type of the averaged variable
                            annotate('text', x=Xm[i], y=max(Y) + 0.9,
                                     label=expr(bar(!!var)),
                                     hjust=0.5, vjust=0.5, 
                                     size=3.25, color='grey20')
                    }

                    # For all the station on the page
                    for (k in 1:nsubCode) {
                        # Gets the code
                        code = subCode[k]
                        # Extracts label for the period dates
                        label =
                            Period_trend_per[Code_trend_per == code][1]
                        # Gets the start and end of the period
                        # for the station
                        periodStart = substr(label, 1, 4)
                        periodEnd = substr(label, 8, 11)
                        
                        mat = mat +
                            # Writes the starting value
                            annotate('text', x=Xc, y=k + 0.13,
                                     label=bquote(bold(.(periodStart))),
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color='grey40') + 
                            # Writes the ending value
                            annotate('text', x=Xc, y=k - 0.13,
                                     label=bquote(bold(.(periodEnd))),
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color='grey40')
                    }
                }

                ### Mean ###
                # For all the trend period
                for (j in 1:nPeriod_mean) {
                    # Extracts the info to plot associated to the
                    # right period
                    Period_mean_per =
                        subPeriod_mean[subNPeriod_mean == j]
                    NPeriod_mean_per =
                        subNPeriod_mean[subNPeriod_mean == j]
                    Var_mean_per =
                        subVar_mean[subNPeriod_mean == j]
                    Type_mean_per =
                        subType_mean[subNPeriod_mean == j]
                    Code_mean_per =
                        subCode_mean[subNPeriod_mean == j]
                    DataMean_mean_per =
                        subDataMean_mean[subNPeriod_mean == j]
                    BreakValue_mean_per =
                        subBreakValue_mean[subNPeriod_mean == j]
                    Fill_mean_per =
                        subFill_mean[subNPeriod_mean == j]
                    Color_mean_per =
                        subColor_mean[subNPeriod_mean == j]

                    # Converts the variable list into levels for factor
                    levels = unlist(subVar_mean[1:nVarMod])
                    # Converts the vector of hydrological variable to
                    # a vector of integer associated to those variable
                    Xtmp_mean =
                        as.integer(factor(as.character(Var_mean_per),
                                          levels=levels))
                    # Computes X position of the column for
                    # the period dates
                    Xc_mean = j + (j - 1)*nVarMod + X[length(X)]
                    # Computes X positions of columns for
                    # the mean of variables
                    Xm_mean =
                        Xtmp_mean + (j - 1)*nVarMod + j + X[length(X)]
                    # Computes X positions of columns for
                    # the difference of mean between periods (break)
                    Xr_mean =
                        Xtmp_mean + (j - 1)*nVarMod*2 + j + X[length(X)]

                    # Computes Y positions of each line for each station
                    Y_mean = as.integer(factor(Code_mean_per))
                    # Reverses vertical order of stations
                    Y_mean = rev(Y_mean)



                    
                    # # Position of a line to delimite periods
                    # x = Xc_mean - 0.4
                    # xend = Xm_mean[length(Xm_mean)] + 0.25
                    # y = height + 1.1
                    # yend = height + 1.1
                    # # Drawing of the line
                    # mat = mat +
                    #     annotate("segment",
                    #              x=x, xend=xend,
                    #              y=y, yend=yend, 
                    #              color="grey40", size=0.35)

                    # # Position of the name of the current period
                    # yt = y + 0.15
                    # Start = mean_period[[j]][1]
                    # End = mean_period[[j]][2]
                    # # Name of the period
                    # periodName = bquote(bold('Période')~bold(.(as.character(j+nPeriod_trend))))
                    # # Naming the period
                    # mat = mat +
                    #     annotate("text", x=x, y=yt,
                    #              label=periodName,
                    #              hjust=0, vjust=0.5, 
                    #              size=3, color='grey40')

                    # # If this is not the first period
                    # if (j > 1) {
                    #     # Position of a line to delimite results of
                    #     # difference of mean bewteen periods
                    #     x = Xr_mean[1] - 0.4
                    #     xend = Xr_mean[length(Xr_mean)] + 0.25
                    #     # Drawing of the line
                    #     mat = mat +
                    #         annotate("segment",
                    #                  x=x, xend=xend,
                    #                  y=y, yend=yend, 
                    #                  color="grey40", size=0.35)
                    #     # Naming the breaking columns
                    #     breakName =  bquote(bold('Écart')~bold(.(as.character(j-1+nPeriod_trend)))*bold('-')*bold(.(as.character(j+nPeriod_trend))))
                    #     # Writes the name
                    #     mat = mat +
                    #         annotate("text", x=x, y=yt,
                    #                  label=breakName,
                    #                  hjust=0, vjust=0.5, 
                    #                  size=3, color='grey40')
                    # }

                    # Position of a line to delimite periods
                    if (j == 1) {
                        x = Xc_mean - 0.4
                    } else {
                        x = Xc_mean - 0.5
                    }
                    xend = Xm_mean[length(Xm_mean)] + 0.5
                    y = height + 1.13
                    # Drawing of the line
                    mat = mat +
                        annotate("segment",
                                 x=x, xend=xend,
                                 y=y, yend=y, 
                                 color="grey40", size=0.35)
  
                    if (j == 1) {
                        # Position of the name of the current period
                        yt = y + 0.03
                        Start = mean_period[[j]][1]
                        End = mean_period[[j]][2]
                        # Name of the period
                        periodName = bquote(bold('Différence entre les moyennes sur périodes de 20 ans '))
                        # Naming the period
                        mat = mat +
                            annotate("text", x=x, y=yt,
                                     label=periodName,
                                     hjust=0, vjust=0, 
                                     size=periodSize, color='grey40')
                    }

                    # If this is not the first period
                    if (j > 1) {
                        # Position of a line to delimite results of
                        # difference of mean bewteen periods
                        x = Xr_mean[1] - 0.5
                        if (j == nPeriod_mean) {
                            xend = Xr_mean[length(Xr_mean)] + 0.25
                        } else {
                            xend = Xr_mean[length(Xr_mean)] + 0.5
                        }
                        # Drawing of the line
                        mat = mat +
                            annotate("segment",
                                     x=x, xend=xend,
                                     y=y, yend=y, 
                                     color="grey40", size=0.35)
                    }




                    

                    # For all the variable
                    for (i in 1:length(Xm_mean)) {
                        mat = mat +
                            # Plots circles for averaged variables
                            gg_circle(r=radius, xc=Xm_mean[i], yc=Y[i],
                                      fill=colorBack, color=colorBack,
                                      size=0.75) +
                            # Plots circles for the column of period dates
                            gg_circle(r=radius, xc=Xc_mean, yc=Y[i],
                                      fill=colorBack, color=colorBack,
                                      size=0.75)
                        
                        # If this is not the first period
                        if (j > 1) {
                            mat = mat +
                                # Plots circles for breaking results
                                gg_circle(r=radius, xc=Xr_mean[i],
                                          yc=Y[i],
                                          fill=Fill_mean_per[i],
                                          color=Color_mean_per[i])
                        }
                    }
                    
                    # For all averaged variables on this period
                    for (i in 1:length(DataMean_mean_per)) {
                        type = Type_mean_per[i]
                        # If it is a flow variable
                        if (type == 'sévérité') {
                            # The number of significant figures for
                            # flow mean is 2
                            Nsign_mean = 2
                        # If it is a date variable
                        } else if (type == 'saisonnalité') {
                            # The number of significant figures for
                            # date mean is 3
                            Nsign_mean = 3
                        }
                        # Extracts values of averaged variables
                        dataMean = DataMean_mean_per[i]
                                                
                        # Converts it to the right format with two
                        # significant figures
                        dataMeanC = signif(dataMean, Nsign_mean)
                        # Writes averaged variables values
                        mat = mat +
                            annotate('text', x=Xm_mean[i], y=Y[i],
                                     label=dataMeanC,
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color='grey40')
                        # If this is not the first period
                        if (j > 1) {
                            # Extracts values of breaking between periods
                            breakValue = BreakValue_mean_per[i]
                            # If it is a flow variable
                            if (type == 'sévérité') {
                                # Converts it to the right format with two
                                # significant figures
                                breakValueC = signif(breakValue*100, 2)
                            # If it is a date variable
                            } else if (type == 'saisonnalité') {
                                # Converts the break value with two
                                # significant figures
                                breakValueC = signif(breakValue, 2)
                            }
                            breakValueC = as.character(breakValueC)

                            # Writes breaking values
                            mat = mat +
                                annotate('text', x=Xr_mean[i], y=Y[i],
                                    label=bquote(bold(.(breakValueC))),
                                         hjust=0.5, vjust=0.5, 
                                         size=3, color='white')   
                        }
                    }

                    # Writes a name for the period dates column
                    mat = mat +
                        annotate('text', x=Xc_mean, y=max(Y) + 0.9,
                                 label=bquote(bold('Début')),
                                 hjust=0.5, vjust=0.5, 
                                 size=3, color='grey20') + 
                        annotate('text', x=Xc_mean, y=max(Y) + 0.63,
                                 label=bquote(bold('Fin')),
                                 hjust=0.5, vjust=0.5, 
                                 size=3, color='grey20')
                    
                    # For all variables
                    for (i in 1:nVarMod) {
                        # Extract the variable of the plot
                        var = subVar_mean[i]
                        type = subType_mean[i]

                        # If it is a flow variable
                        if (type == 'sévérité') {
                            # Fixes the unit of the mean and the break
                            # for the flow
                            unit_mean = bquote('['*m^3*'.'*s^{-1}*']')
                            unit_break = bquote('[%]')
                            # If it is a date variable
                            # Fixes the unit of the mean and the break
                            # for the date
                        } else if (type == 'saisonnalité') {
                            unit_mean = bquote('[jour]')
                            unit_break = bquote('[jour]')
                        }
                        
                        mat = mat +
                            # Writes the unit of the averaged variable
                            annotate('text',
                                     x=Xm_mean[i], y=max(Y) + 0.63,
                                     label=unit_mean,
                                     hjust=0.5, vjust=0.5, 
                                     size=2, color='grey40') +
                            # Writes the type of the averaged variable
                            annotate('text',
                                     x=Xm_mean[i], y=max(Y) + 0.9,
                                     label=expr(bar(!!var)),
                                     hjust=0.5, vjust=0.5, 
                                     size=3.25, color='grey20')

                        # If this is not the first period
                        if (j > 1) {
                            mat = mat +
                                # Writes the unit of the breaking variable
                                annotate('text', x=Xr_mean[i],
                                         y=max(Y) + 0.63,
                                         label=unit_break,
                                         hjust=0.5, vjust=0.5, 
                                         size=2, color='grey40') +
                                # Writes the type of the breaking variable
                                annotate('text', x=Xr_mean[i],
                                         y=max(Y) + 0.9,
                                         label=paste("d", var, sep=''),
                                         hjust=0.5, vjust=0.5,
                                         size=3.25, color='grey20')
                        }
                    }

                    # For all the station on the page
                    for (k in 1:nsubCode) {
                        # Gets the code
                        code = subCode[k]
                        # Extracts label for the period dates
                        label = Period_mean_per[Code_mean_per == code][1]
                        # Gets the start and end of the period
                        # for the station
                        periodStart = substr(label, 1, 4)
                        periodEnd = substr(label, 8, 11)

                        mat = mat +
                            # # Writes the starting value
                            annotate('text', x=Xc_mean, y=k + 0.13,
                                     label=bquote(bold(.(periodStart))),
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color='grey40') + 
                            # Writes the ending value
                            annotate('text', x=Xc_mean, y=k - 0.13,
                                     label=bquote(bold(.(periodEnd))),
                                     hjust=0.5, vjust=0.5, 
                                     size=3, color='grey40')
                    }            
                }
                
                ### Code ###
                # For all the station
                for (k in 1:nsubCode) {
                    # Gets the code
                    code = subCode[k]
                    # Gets the name of the station
                    name = df_meta[df_meta$Code == code,]$nom
                    # Fixes a limit for the max number
                    # of characters available
                    ncharMax = 38
                    # If the number of character of the name is greater
                    # than the limit
                    if (nchar(name) > ncharMax) {
                        # Cuts the name and add '...'
                        name = paste(substr(name, 1, ncharMax),
                                     '...', sep='')
                    }

                    mat = mat +
                        # Writes the code of the station
                        annotate('text', x=0.3, y=k + 0.14,
                                 label=bquote(bold(.(code))),
                                 hjust=1, vjust=0.5, 
                                 size=3.5, color="#00A3A8") +
                        # Writes the name of the station
                        annotate('text', x=0.3, y=k - 0.14,
                                 label=name,
                                 hjust=1, vjust=0.5, 
                                 size=3.5, color="#00A3A8")
                }

                ### Environment ###
                mat = mat +
                    # Fixed coordinate system
                    coord_fixed() +
                    # X axis
                    scale_x_continuous(limits=c(1 - rel(6), 
                                                width + rel(0.5)),
                                       expand=c(0, 0)) + 
                    # Y axis
                    scale_y_continuous(limits=c(1 - rel(0.5), 
                                                height + rel(2)),
                                       expand=c(0, 0))
                
                # Paper format in A3 if needed
                if (paper_size == 'A3') {
                    width = 42
                    height = 29.7
                    dpi = 300
                    # Otherwise in A4
                } else if (paper_size == 'A4') {
                    width = 29.7
                    height = 21
                    dpi = 100
                }
                
                # If there is a foot note
                if (foot_note) {
                    footName = paste('tableau récapitulatif de ',
                                     type, sep='')
                    
                    if (is.null(df_page)) {
                        n_page = n_loop
                    } else {
                        n_page = df_page$n[nrow(df_page)] + iMat
                    }
                    
                    foot = foot_panel(footName, n_page,
                                      foot_height, logo_path)
                    
                    # Stores the map, the title and the colorbar
                    # in a list
                    P = list(mat, foot)
                    LM = matrix(c(1,
                                  2),
                                nrow=2, byrow=TRUE)
                } else {
                    foot_height = 0
                    # Stores the map, the title and the colorbar
                    # in a list
                    P = list(mat)
                    LM = matrix(c(1),
                                nrow=1, byrow=TRUE)
                }
                id_foot = 2
                
                LMcol = ncol(LM)
                LMrow = nrow(LM)
                
                LM = rbind(rep(99, times=LMcol),
                           LM, rep(99, times=LMcol))
                LMrow = nrow(LM)
                LM = cbind(rep(99, times=LMrow),
                           LM, rep(99, times=LMrow))
                LMcol = ncol(LM)
                
                margin_size = 0.5

                row_height = (height - 2*margin_size - foot_height) / (LMrow - 3)

                Hcut = LM[, 2]
                heightLM = rep(row_height, times=LMrow)
                heightLM[Hcut == id_foot] = foot_height
                heightLM[Hcut == 99] = margin_size

                col_width = (width - 2*margin_size) / (LMcol - 2)
                
                Wcut = LM[(nrow(LM)-1),]
                widthLM = rep(col_width, times=LMcol)
                widthLM[Wcut == 99] = margin_size

                # Arranges the graphical object
                plot = grid.arrange(grobs=P, layout_matrix=LM,
                                    heights=heightLM, widths=widthLM)

                # Saving
                ggsave(plot=plot, 
                       path=outdirTmp_pdf,
                       filename=paste('table',
                                      '_', type,
                                      '_', rh,
                                      iMat, '.pdf', sep=''),
                       width=width, height=height,
                       units='cm', dpi=dpi)

                ggsave(plot=plot, 
                       path=outdirTmp_png,
                       filename=paste('table',
                                      '_', type,
                                      '_', rh,
                                      iMat, '.png', sep=''),
                       width=width, height=height,
                       units='cm', dpi=400)
            }

            if (!is.null(df_page)) {
                section = paste0('Tableau récapitulatif de ',
                                 type)
                subsection = title
                n_page = df_page$n[nrow(df_page)] + 1
                # N_page = df_page$n[nrow(df_page)] + nMat
                df_page = bind_rows(
                    df_page,
                    tibble(section=section,
                           subsection=subsection,
                           n=n_page))
            }
        }           
    }
    return (df_page)
}
