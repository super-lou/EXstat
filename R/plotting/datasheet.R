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
# R/plotting/datasheet.R
#
# Regroups all the graphical tools to generates the datasheets. More precisely, the 'datasheet_panel' function manages all the call for each station of the different graphical functions that generates info header, time serie visualisation and trend analysis graphs for every variable. It also deals with the arranging of all the plots in a single PDF page.


# Sourcing R file
source(file.path('R', 'processing', 'analyse.R'), encoding='UTF-8') # hydrograph
source(file.path('R', 'plotting', 'shortcut.R'), encoding='UTF-8')


## 1. DATASHEET PANEL MANAGER ________________________________________
# Manages datasheets creations for all stations. Makes the call to
# the different headers, trend analysis graphs and realises arranging
# every plots.
datasheet_panel = function (list_df2plot, df_meta, trend_period,
                            mean_period, linetype_per, axis_xlim,
                            colorForce, info_header, time_header,
                            foot_note, layout_matrix, info_height,
                            time_ratio, var_ratio, foot_height,
                            paper_size, resources_path, df_shapefile,
                            logo_dir, PRlogo_file, AEAGlogo_file,
                            INRAElogo_file, FRlogo_file,
                            outdirTmp_pdf, outdirTmp_png,
                            df_page=NULL) {

    # The percentage of augmentation and diminution of the min
    # and max limits for y axis
    lim_pct = 10

    # Number of variable/plot
    nbVar = length(list_df2plot)
    
    # Get all different stations code
    Code = levels(factor(df_meta$code))
    nCode = length(Code)

    if (!is.null(trend_period)) {
        # Convert 'trend_period' to list
        trend_period = as.list(trend_period)
        # Number of trend period
        nPeriod_trend = length(trend_period)

        # Extracts the min and the max of the mean trend for all the station
        res = short_trendExtremes(list_df2plot, Code, nPeriod_trend, nbVar, nCode, colorForce)
        minTrendValue = res$min
        maxTrendValue = res$max
    }
    
    # Blank vector to store the max number of digit of label for
    # each station
    NspaceMax = c()
    # For all the station
    for (code in Code) {

        # Default max digit
        NspaceMax_code = 0

        # If the time header is given it adds one to the number of plot
        nbVarMod = nbVar + as.numeric(!is.null(time_header))

        # For all type of graph
        for (i in 1:nbVarMod) {

            if (i > nbVar) {
                # Extracts the data serie corresponding to the code
                df_data_code = time_header[time_header$code == code,]
                type = 'sévérité'
            } else {
                # Extracts the data corresponding to the current variable
                df_data = list_df2plot[[i]]$data
                # Extracts the type corresponding to the current variable
                type = list_df2plot[[i]]$type
                # Extracts the data corresponding to the code
                df_data_code = df_data[df_data$code == code,]
            }

            # If variable type is date 
            if (type == 'saisonnalité') {
                # The number of digit is 6 because months are display
                # with 3 characters
                Nspace = 6
            # If it is a flow variable
            } else if (type == 'sévérité' | type == 'data' | type == 'pluviométrie' | type == 'température' | type == 'évapotranspiration') {
                # Gets the max number of digit on the label
                maxtmp = max(df_data_code$Value, na.rm=TRUE)
                # Taking into account of the augmentation of
                # max for the window
                maxtmp = maxtmp * (1 + lim_pct/100)
                
                # If the max is greater than 10
                if (maxtmp >= 10) {
                    # The number of digit is the magnitude plus
                    # the first number times 2
                    Nspace = (get_power(maxtmp) + 1)*2
                    # Plus spaces between thousands hence every 8 digits
                    Nspace = Nspace + as.integer(Nspace/8)
                    # If the max is less than 10 and greater than 1
                } else if (maxtmp < 10 & maxtmp >= 1) {
                    # The number of digit is the magnitude plus
                    # the first number times 2 plus 1 for the dot
                    # and 2 for the first decimal
                    Nspace = (get_power(maxtmp) + 1)*2 + 3
                    # If the max is less than 1 (and obviously more than 0)
                } else if (maxtmp < 1) {
                    # Fixes the number of significant decimals to 3
                    maxtmp = signif(maxtmp, 3)
                    # The number of digit is the number of character
                    # of the max times 2 minus 1 for the dots that
                    # count just 1 space
                    Nspace = nchar(as.character(maxtmp))*2 - 1
                }
            }
            
            # If it is the temporary max number
            if (Nspace > NspaceMax_code) {
                # Stores it
                NspaceMax_code = Nspace
            }
        }
        # Stores the max digit number for labels of a station
        NspaceMax = c(NspaceMax, NspaceMax_code)
    }

    # For all the station
    for (k in 1:nCode) {
        # Gets the code
        code = Code[k]
        # Print code of the station for the current plotting
        print(paste("Datasheet for station : ", code,
                    "   (", round(k/nCode*100, 0), " %)", 
                    sep=''))
        
        # Number of header (is info and time serie are needed)
        nbh = as.numeric(!is.null(info_header)) + as.numeric(!is.null(time_header))
        nbp = max(layout_matrix, na.rm=TRUE)
        # Actualises the number of plot
        nbg = nbp + nbh + as.numeric(foot_note)

        # Opens a blank list to store plot
        P = vector(mode='list', length=nbg)
        
        for (id in 1:nbg) {
            P[[id]] = void 
        }
        
        # If the info header is needed
        if (!is.null(info_header)) {
            
            if ("data.frame" %in% class(info_header)) {
                # Extracts the data serie corresponding to the code
                info_header_code = info_header[info_header$code == code,]
                to_do = 'all'
            } else {
                info_header_code = NULL
                to_do = info_header
            }
            # Gets the info plot
            Hinfo = info_panel(list_df2plot, 
                               df_meta,
                               trend_period=trend_period,
                               mean_period=mean_period,
                               periodHyd=mean_period[[1]],
                               df_shapefile=df_shapefile,
                               codeLight=code,
                               df_data_code=info_header_code,
                               to_do=to_do)
            
            # Stores it
            P[[1]] = Hinfo
        }

        # If the time header is given
        if (!is.null(time_header)) {
            # Extracts the data serie corresponding to the code
            time_header_code = time_header[time_header$code == code,]
            
            if (is.null(axis_xlim)) {
                # Gets the limits of the time serie
                axis_xlim_code = c(min(time_header_code$Date),
                                   max(time_header_code$Date))
            } else {
                axis_xlim_code = axis_xlim
            }

            # Gets the time serie plot
            Htime = time_panel(time_header_code, df_trend_code=NULL,
                               trend_period=trend_period,
                               axis_xlim=axis_xlim_code, missRect=TRUE,
                               unit2day=365.25, var='Q', type='sévérité',
                               grid=TRUE, ymin_lim=0,
                               NspaceMax=NspaceMax[k],
                               first=TRUE, lim_pct=lim_pct)
            # Stores it
            P[[2]] = Htime
        } else {
            axis_xlim_code = axis_xlim
        }

        # Computes the number of column of plot asked on the datasheet
        nbcol = ncol(as.matrix(layout_matrix))
        # For all variable
        for (i in 1:nbVar) {
            # Extracts the data corresponding to the current variable
            df_data = list_df2plot[[i]]$data
            # Extracts the trend corresponding to the
            # current variable
            df_trend = list_df2plot[[i]]$trend
            
            unit2day = list_df2plot[[i]]$unit2day
            missRect = list_df2plot[[i]]$missRect
            # Extract the variable of the plot
            var = list_df2plot[[i]]$var
            type = list_df2plot[[i]]$type
            # Extracts the data corresponding to the code
            df_data_code = df_data[df_data$code == code,]
            # Extracts the trend corresponding to the code
            df_trend_code = df_trend[df_trend$code == code,]

            # Blank vector to store color
            color = c()

            if (!is.null(trend_period)) {
                # For all the period
                for (j in 1:nPeriod_trend) {
                    
                    # If the trend is significant
                    # if (df_trend_code$p[j] <= alpha | colorForce){
                    if (df_trend_code$p[j] <= alpha){
                        # Extract start and end of trend periods
                        Start = df_trend_code$period_start[j]
                        End = df_trend_code$period_end[j]

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

                        # If it is a flow variable
                        if (type == 'sévérité') {
                            # Computes the mean of the data on the period
                            dataMean = mean(df_data_code_per$Value,
                                            na.rm=TRUE)
                            # Normalises the trend value by the mean
                            # of the data
                            trendValue = df_trend_code_per$trend / dataMean
                            # If it is a date variable
                        } else if (type == 'saisonnalité' | type == 'pluviométrie' | type == 'température' | type == 'évapotranspiration') {
                            trendValue = df_trend_code_per$trend
                        }
                        
                        # gets the color corresponding to the mean trend
                        color_res = get_color(trendValue, 
                                              minTrendValue[j, i],
                                              maxTrendValue[j, i],
                                              palette_name='perso',
                                              reverse=TRUE)
                        # Stores it temporarily
                        colortmp = color_res

                        # Otherwise
                    } else {
                        # Stores the default grey color
                        colortmp = 'grey85'
                        
                    }
                    # Stores the color
                    color = append(color, colortmp)
                    grid = FALSE
                }
            }

            if (var != 'sqrt(Q)' & var != 'Q') {
                grid = FALSE
                ymin_lim = NULL
            } else {
                grid = TRUE
                ymin_lim = 0
            }

            if (is.null(time_header) & i == 1) {
                first = TRUE
            } else {
                first = FALSE
            }

            # Computes the time panel associated to the current variable
            p = time_panel(df_data_code, df_trend_code, var=var, 
                           type=type, linetype_per=linetype_per,
                           alpha=alpha, colorForce=colorForce,
                           missRect=missRect, trend_period=trend_period,
                           mean_period=mean_period,
                           axis_xlim=axis_xlim_code, 
                           unit2day=unit2day, grid=grid,
                           ymin_lim=ymin_lim, color=color,
                           NspaceMax=NspaceMax[k], first=first,
                           last=(i == nbVar),
                           lim_pct=lim_pct)
            
            # Stores the plot
            P[[i+nbh]] = p
        }

        if (!is.null(df_page)) {
            section = 'Fiche station'
            subsection = code
            n_page = df_page$n[nrow(df_page)] + 1
            df_page = bind_rows(
                df_page,
                tibble(section=section,
                       subsection=subsection,
                       n=n_page))
        }
        
        if (foot_note) {
            footName = 'fiche station'
            if (is.null(df_page)) {
                n_page = k
            }
            
            foot = foot_panel(footName, n_page, resources_path,
                              logo_dir, PRlogo_file,
                              AEAGlogo_file, INRAElogo_file,
                              FRlogo_file, foot_height)
            P[[nbg]] = foot
        }
        
        # Convert the 'layout_matrix' to a matrix if it is not already 
        layout_matrix = as.matrix(layout_matrix)

        # Number of element of the matrix
        nel = nrow(layout_matrix)*ncol(layout_matrix)
        # Gets the place where there is NA value
        idNA = which(is.na(layout_matrix), arr.ind=TRUE)

        LM = layout_matrix
        # Adds non existing plot is where there is NA
        LM[idNA] = seq(max(layout_matrix, na.rm=TRUE) + 1,
                       max(layout_matrix, na.rm=TRUE) + 1 +
                       nel)
        # Shifts all plots to be coherent with the adding of header
        LM = LM + nbh

        if (!is.null(time_header)) {
            id_time = nbh
            LM = rbind(nbh, LM)
        } else {
            id_time = NA
            time_ratio = 0
        }
        if (!is.null(info_header)) {
            id_info = nbh - as.numeric(!is.null(time_header))
            LM = rbind(nbh - as.numeric(!is.null(time_header)), LM)
        } else {
            id_info = NA
            info_ratio = 0
        }
        
        if (foot_note) {
            id_foot = length(LM) + 1
            LM = rbind(LM, id_foot)
        } else {
            id_foot = max(LM) + 1
            foot_height = 0
        }

        # If paper format is A4
        if (paper_size == 'A4') {
            width = 21
            height = 29.7
        } else if (is.vector(paper_size) & length(paper_size) > 1) {
            width = paper_size[1]
            height = paper_size[2]
        }

        LMcol = ncol(LM)
        LMrow = nrow(LM)
        
        LM = rbind(rep(99, times=LMcol), LM, rep(99, times=LMcol))
        LMrow = nrow(LM)
        LM = cbind(rep(99, times=LMrow), LM, rep(99, times=LMrow))
        LMcol = ncol(LM)

        margin_size = 0.5

        Norm_ratio = height * (time_ratio + var_ratio*nbp) / (height - 2*margin_size - foot_height - info_height)

        time_height = height * time_ratio / Norm_ratio
        var_height = height * var_ratio / Norm_ratio

        Hcut = LM[, 2]
        heightLM = rep(0, times=LMrow)        
        
        heightLM[Hcut == id_info] = info_height
        heightLM[Hcut == id_time] = time_height
        heightLM[Hcut > nbh & Hcut < id_foot] = var_height
        heightLM[Hcut == id_foot] = foot_height
        heightLM[Hcut == 99] = margin_size

        col_width = (width - 2*margin_size) / (LMcol - 2)
        
        Wcut = LM[(nrow(LM)-1),]
        widthLM = rep(col_width, times=LMcol)
        widthLM[Wcut == 99] = margin_size

        # Plot the graph as the layout
        plot = grid.arrange(grobs=P, layout_matrix=LM,
                            heights=heightLM, widths=widthLM)
        
        # Saving
        ggsave(plot=plot, 
               path=outdirTmp_pdf,
               filename=paste(as.character(code), '.pdf', sep=''),
               width=width, height=height, units='cm', dpi=100)
        
        # Saving
        ggsave(plot=plot, 
               path=outdirTmp_png,
               filename=paste(as.character(code), '.png', sep=''),
               width=width, height=height, units='cm', dpi=400)
        
    }
    return (df_page)
}


## 2. PANEL FOR THE DATASHEET __________________________________
### 2.1. Time panel __________________________________________________
time_panel = function (df_data_code, df_trend_code, var, type,
                       linetype_per='solid', alpha=0.1,
                       colorForce=FALSE, missRect=FALSE,
                       unit2day=365.25, trend_period=NULL,
                       mean_period=NULL, axis_xlim=NULL, grid=TRUE,
                       ymin_lim=NULL, color=NULL, NspaceMax=NULL,
                       first=FALSE, last=FALSE, lim_pct=10) {
    
    # Compute max and min of flow
    maxQ = max(df_data_code$Value, na.rm=TRUE)
    minQ = min(df_data_code$Value, na.rm=TRUE)

    spread = maxQ - minQ
    
    nTick = 6
    maxQ_win = maxQ + spread*lim_pct/100
    minQ_win = minQ - spread*lim_pct/100

    if (minQ_win < 0) {
        minQtmp_lim = 0
    } else {
        minQtmp_lim = minQ_win
    }

    if (!is.null(ymin_lim)) {
        minQ_win = ymin_lim
    }
    
    spreadtmp = maxQ_win - minQtmp_lim
    
    breakQtmp = spreadtmp / (nTick - 1)

    GradQ_10 = c(0, 1, 1.5, 2, 2.5, 3, 4, 5, 10)

    Grad = GradQ_10 * 10^get_power(breakQtmp)
    dist = abs(Grad - breakQtmp)
    idGrad = which.min(dist)
    breakQ = Grad[idGrad]
    
    if (is.null(ymin_lim)) {
        Grad = GradQ_10 * 10^get_power(minQtmp_lim)      
        Grad[Grad > minQtmp_lim] = NA        
        dist = abs(Grad - minQtmp_lim)        
        idGrad = which.min(dist)        
        minQ_lim = Grad[idGrad]        
    } else {        
        minQ_lim = ymin_lim
    }
    maxQ_list = c()
    i = 1
    maxQtmp = minQ_lim
    
    while (maxQtmp <= maxQ_win) {
        maxQtmp = minQ_lim + i*breakQ
        i = i + 1
    }   
    maxQ_lim = maxQtmp    
        
    # If x axis limits are specified
    if (!is.null(axis_xlim)) {
        axis_xlim = as.Date(axis_xlim)
        minor_minDatetmp_lim = axis_xlim[1]
        minor_maxDatetmp_lim = axis_xlim[2]
    # Otherwise
    } else {
        minor_minDatetmp_lim = as.Date(df_data_code$Date[1]) 
        minor_maxDatetmp_lim =
            as.Date(df_data_code$Date[length(df_data_code$Date)])
    }

    minor_minDatetmp_lim = as.numeric(format(minor_minDatetmp_lim, "%Y"))
    minor_maxDatetmp_lim = as.numeric(format(minor_maxDatetmp_lim, "%Y"))

    minDatetmp_lim = minor_minDatetmp_lim
    maxDatetmp_lim = minor_maxDatetmp_lim

    nTick = 8
    
    spreadtmp = minor_maxDatetmp_lim - minor_minDatetmp_lim
    breakDatetmp = spreadtmp / (nTick - 1)

    GradDate_10 = c(1, 2.5, 5, 10)

    Grad = GradDate_10 * 10^get_power(breakDatetmp)
    dist = abs(Grad - breakDatetmp)
    idGrad = which.min(dist)
    breakDate = Grad[idGrad]

    listDate = seq(round(minDatetmp_lim, -1)-10^(get_power(breakDate)+1),
                   round(maxDatetmp_lim, -1)+10^(get_power(breakDate)+1),
                   by=breakDate)

    minDate_lim = listDate[which.min(abs(listDate - minDatetmp_lim))]
    maxDate_lim = listDate[which.min(abs(listDate - maxDatetmp_lim))]
    minDate_lim = as.Date(paste(minDate_lim, '-01-01', sep=''))
    maxDate_lim = as.Date(paste(maxDate_lim, '-01-01', sep=''))


    minor_breakDatetmp = breakDate / 5
    
    GradMinorDate_10 = c(1, 2, 5, 10)

    Grad = GradMinorDate_10 * 10^get_power(minor_breakDatetmp)
    dist = abs(Grad - minor_breakDatetmp)
    idGrad = which.min(dist)
    minor_breakDate = Grad[idGrad]
    
    listDate = seq(round(minor_minDatetmp_lim,
                         -1) - 10^(get_power(minor_breakDate)+1),
                   round(minor_maxDatetmp_lim,
                         -1) + 10^(get_power(minor_breakDate)+1),
                   by=minor_breakDate)

    minor_minDate_lim =
        listDate[which.min(abs(listDate - minor_minDatetmp_lim))]
    minor_maxDate_lim =
        listDate[which.min(abs(listDate - minor_maxDatetmp_lim))]
    minor_minDate_lim = as.Date(paste(round(minor_minDate_lim),
                                      '-01-01', sep=''))
    minor_maxDate_lim = as.Date(paste(round(minor_maxDate_lim),
                                      '-01-01', sep=''))
    
    # Open new plot
    p = ggplot() + theme_ash

    # Margins
    if (first) {
        p = p + 
            theme(plot.margin=margin(t=2.5, r=0, b=3, l=0, unit="mm"))
    } else if (last) {
        p = p + 
            theme(plot.margin=margin(t=2, r=0, b=0, l=0, unit="mm"))
    } else if (first & last) {
        p = p + 
            theme(plot.margin=margin(t=2.5, r=0, b=0, l=0, unit="mm"))
    } else {
        p = p + 
            theme(plot.margin=margin(t=2, r=0, b=2, l=0, unit="mm"))
    }

    ## Sub period background ##
    if (!is.null(trend_period)) {

        # Convert trend period to list if it is not
        trend_period = as.list(trend_period)
        # Fix a disproportionate minimum for period
        Imin = 10^99
        # For all the sub period of analysis in 'trend_period'
        for (per in trend_period) {
            # Compute time interval of period
            I = interval(per[1], per[2])
            # If it is the smallest interval
            if (I < Imin) {
                # Store it
                Imin = I
                # Fix min period of analysis
                trend_period_min = as.Date(per)
            }
        }
        
        minPer = trend_period_min[1]
        maxPer = trend_period_min[2]
        
        # If there is an 'axis_lim'
        if (!is.null(axis_xlim)) {
            # If the temporary start of period is smaller 
            # than the fix start of x axis limit
            if (minPer < axis_xlim[1]) {
                # Set the start of the period to the start of
                # the x axis limit
                minPer = axis_xlim[1]
            }

            # If the temporary end of period plus one year 
            # is smaller than the fix end of x axis limit
            if (maxPer + years(1) < axis_xlim[2]) {
                # Add one year the the temporary end of period
                maxPer = maxPer + years(1)
            } else {
                # Set the start of the period to the start of
                # the x axis limit
                maxPer = axis_xlim[2]
            }
            
        # If there is no 'axis_lim'
        } else {
            if (minPer < min(df_data_code$Date)) {
                minPer = min(df_data_code$Date)
            }
            if (maxPer > max(df_data_code$Date)) {
                maxPer = max(df_data_code$Date)
            }
        }

        # Draw rectangle to delimiting the sub period
        p = p + 
            geom_rect(aes(xmin=minPer,
                          ymin=minQ_win, 
                          xmax=maxPer,
                          ymax= maxQ_win),
                      linetype=0, fill='grey97')
    }

    ## Mean step ##
    # If there is a 'mean_period'
    if (!is.null(mean_period)) {
        # Convert 'mean_period' to list
        mean_period = as.list(mean_period)
        # Number of mean period
        nPeriod_mean = length(mean_period)

        # Blank tibble to store variable in order to plot
        # rectangle for mean period
        plot_mean = tibble()
        # Blank tibble to store variable in order to plot
        # upper limit of rectangle for mean period
        plot_line = tibble()
        # For all mean period
        for (j in 1:nPeriod_mean) {
            # Get the current start and end of the sub period
            xmin = as.Date(mean_period[[j]][1])
            xmax = as.Date(mean_period[[j]][2])

            # Extract the data corresponding to this sub period
            df_data_code_per =
                df_data_code[df_data_code$Date >= xmin
                             & df_data_code$Date <= xmax,]
            
            # If the min over the sub period is greater
            # than the min of the entier period and
            # it is not the first sub period
            if (xmin > min(df_data_code$Date) & j != 1) {
                # Substract 6 months to be in the middle of
                # the previous year
                xmin = add_months(xmin, -6)
            }
            # If it is not a flow or sqrt of flow time serie and
            # it is the first period
            if (var != 'sqrt(Q)' & var != 'Q' & j == 1) {
                # If there is an x axis limit
                if (!is.null(axis_xlim)) {
                    # If the min of the period is before the x axis min
                    if (xmin < axis_xlim[1]) {
                        # The min for the sub period is the x axis
                        xmin = axis_xlim[1]
                    }
                }
            }

            # If the max over the sub period is smaller
            # than the max of the entier period and
            # it is not the last sub period
            if (xmax < max(df_data_code$Date) & j != nPeriod_mean) {
                # Add 6 months to be in the middle of
                # the following year
                xmax = add_months(xmax, 6)
            }
            # If it is not a flow or sqrt of flow time serie and
            # it is the last period
            if (var != 'sqrt(Q)' & var != 'Q' & j == nPeriod_mean) {
                # If there is an x axis limit
                if (!is.null(axis_xlim)) {
                    # If the max of the period plus 1 year
                    # is smaller thant the max of the x axis limit
                    if (xmax + years(1) < axis_xlim[2]) {
                        # Add one year to the max to include
                        # the entire last year graphically
                        xmax = xmax + years(1)
                    } else {
                        # The max of this sub period is the max
                        # of the x axis limit
                        xmax = axis_xlim[2]
                    }
                   
                }
                # If there is no axis limit
                # } else {
                #     # Add one year to the max to include
                #     # the entire last year graphically
                #     xmax = xmax + years(1)
                # }
            }

            # Mean of the flow over the sub period
            ymax = mean(df_data_code_per$Value, na.rm=TRUE)

            # Create temporary tibble with variable
            # to create rectangle for mean step
            plot_meantmp = tibble(xmin=xmin, xmax=xmax, 
                                  ymin=minQ_win, ymax=ymax, period=j)
            # Bind it to the main tibble to store it with other period
            plot_mean = bind_rows(plot_mean, plot_meantmp)

            # Create vector for the upper limit of the rectangle
            abs = c(xmin, xmax)
            ord = c(ymax, ymax)
            
            # Create temporary tibble with variable
            # to create upper limit for rectangle
            plot_linetmp = tibble(abs=abs, ord=ord, period=j)
            # Bind it to the main tibble to store it with other period
            plot_line =  bind_rows(plot_line, plot_linetmp)
        }
        # Plot rectangles
        p = p + 
            geom_rect(data=plot_mean,
                      aes(xmin=xmin, ymin=ymin, 
                          xmax=xmax, ymax=ymax),
                      linetype=0, fill='grey93')
        # Plot upper line for rectangle
        p = p +
            geom_line(data=plot_line,
                      aes(x=abs, y=ord, group=period),
                      color='grey85',
                      size=0.15)

        
        # for all the sub periods except the last one
        for (i in 1:(nPeriod_mean - 1)) {
            # Computes the time difference in days between periods
            dPeriod = abs(as.Date(mean_period[[i+1]][1]) - as.Date(mean_period[[i]][2]))
                
            if (dPeriod < 10) {
                # The x limit is the x max of the ith rectangle
                xLim = plot_mean$xmax[i]
                # The y limit of rectangle is the max of
                # the two neighboring mean step rectangle
                yLim = max(c(plot_mean$ymax[i], plot_mean$ymax[i+1]))
                # Make a tibble to store data
                plot_lim = tibble(x=c(xLim, xLim), y=c(minQ_win, yLim))
                # Plot the limit of rectangles
                p = p + 
                    geom_line(data=plot_lim, aes(x=x, y=y),
                              linetype='dashed', size=0.15,
                              color='grey85')
                
            } else {
                # Takes the x and y limits for the ith rectangle
                xLim_i = plot_mean$xmax[i]
                yLim_i = plot_mean$ymax[i]
                # Takes the x and y limits for the i+1th rectangle
                xLim_i1 = plot_mean$xmin[i+1]
                yLim_i1 = plot_mean$ymax[i+1]
                
                # Make a tibble to store data
                plot_lim = tibble(x_i=c(xLim_i, xLim_i),
                                  y_i=c(minQ_win, yLim_i),
                                  x_i1=c(xLim_i1, xLim_i1),
                                  y_i1=c(minQ_win, yLim_i1))
                # Plot the limit of rectangles
                p = p + 
                    geom_line(data=plot_lim, aes(x=x_i, y=y_i),
                              linetype='dashed', size=0.15,
                              color='grey85') +
                    geom_line(data=plot_lim, aes(x=x_i1, y=y_i1),
                              linetype='dashed', size=0.15,
                              color='grey85')
            }      
        }
    }

    ### Grid ###
    if (grid) {
        # If there is no axis limit
        if (is.null(axis_xlim)) {
            # The min and the max is set by
            # the min and the max of the date data 
            xmin = min(df_data_code$Date)
            xmax = max(df_data_code$Date)
        } else {
            # Min and max is set with the limit axis parameter
            xmin = axis_xlim[1]
            xmax = axis_xlim[2]
        }
        # Create a vector for all the y grid position
        ygrid = seq(minQ_win, maxQ_win, breakQ)
        # Blank vector to store position
        ord = c() 
        abs = c()
        # For all the grid element
        for (i in 1:length(ygrid)) {
            # Store grid position
            ord = c(ord, rep(ygrid[i], times=2))
            abs = c(abs, xmin, xmax)
        }
        # Create a tibble to store all the position
        plot_grid = tibble(abs=as.Date(abs), ord=ord)
        # Plot the y grid
        p = p +
            geom_line(data=plot_grid, 
                      aes(x=abs, y=ord, group=ord),
                      color='grey85',
                      size=0.15)
    }

    ### Data ###
    # If it is a square root flow or flow
    if (var == 'sqrt(Q)' | var == 'Q') {
        # Plot the data as line
        p = p +
            geom_line(aes(x=df_data_code$Date, y=df_data_code$Value),
                      color='grey20',
                      size=0.3,
                      lineend="round")
    } else {
        # Plot the data as point
        p = p +
            geom_point(aes(x=df_data_code$Date, y=df_data_code$Value),
                       shape=19, color='grey50', alpha=1,
                       stroke=0, size=1)
    }

    ### Missing data ###
    # If the option is TRUE
    if (missRect) {
        # Remove NA data
        NAdate = df_data_code$Date[is.na(df_data_code$Value)]
        # Get the difference between each point of date data without NA
        dNAdate = diff(NAdate)
        # If difference of day is not 1 then
        # it is TRUE for the beginning of each missing data period 
        NAdate_Down = NAdate[append(Inf, dNAdate) != 1]
        # If difference of day is not 1 then
        # it is TRUE for the ending of each missing data period 
        NAdate_Up = NAdate[append(dNAdate, Inf) != 1]

        # Plot the missing data period
        p = p +
            geom_rect(aes(xmin=NAdate_Down, 
                          ymin=minQ_win, 
                          xmax=NAdate_Up, 
                          ymax=maxQ_win),
                      linetype=0, fill='Wheat', alpha=0.4)
    }

    ### Trend ###
    # If there is trends
    if (!is.null(df_trend_code)) {

        # Extract start and end of trend periods
        Start = df_trend_code$period_start
        End = df_trend_code$period_end
        # Get the name of the different period
        UStart = levels(factor(Start))        
        UEnd = levels(factor(End))

        # Compute the max of different start and end
        # so the number of different period
        nPeriod_trend = max(length(UStart), length(UEnd))

        # Blank tibble to store trend data and legend data
        plot_trend = tibble()
        leg_trend = tibble()
        # For all the different period
        for (i in 1:nPeriod_trend) {

            # Extracts the corresponding data for the period
            df_data_code_per =
                df_data_code[df_data_code$Date >= Start[i] 
                             & df_data_code$Date <= End[i],]

            # Computes the mean of the data on the period
            dataMean = mean(df_data_code_per$Value,
                            na.rm=TRUE)
            
            # Get the trend associated to the first period
            df_trend_code_per = 
                df_trend_code[df_trend_code$period_start == Start[i] 
                              & df_trend_code$period_end == End[i],]
            
            # Number of trend selected
            Ntrend = nrow(df_trend_code_per)
            # If the number of trend is greater than a unique one
            if (Ntrend > 1) {
                # Extract only the first hence it is the same period
                df_trend_code_per = df_trend_code_per[1,]
            }            

            # Search for the index of the closest existing date 
            # to the start of the trend period of analysis
            iStart = which.min(abs(df_data_code$Date - Start[i]))
            # Same for the end
            iEnd = which.min(abs(df_data_code$Date - End[i]))

            # Get the start and end date associated
            xmin = df_data_code$Date[iStart]
            xmax = df_data_code$Date[iEnd]

            # If there is a x axis limit
            if (!is.null(axis_xlim)) {
                # If the min of the current period
                # is smaller than the min of the x axis limit
                if (xmin < axis_xlim[1]) {
                    # The min of the period is the min
                    # of the x axis limit
                    xmin = axis_xlim[1]
                }
                # Same for end
                if (xmax > axis_xlim[2]) {
                    xmax = axis_xlim[2]
                } 
            }

            # Create vector to store x data
            abs = c(xmin, xmax)
            # Convert the number of day to the unit of the period
            abs_num = as.numeric(abs) / unit2day
            # Compute the y of the trend
            ord = abs_num * df_trend_code_per$trend +
                df_trend_code_per$intercept

            # Create temporary tibble with variable to plot trend
            # for each period
            plot_trendtmp = tibble(abs=abs, ord=ord, period=i)
            # Bind it to the main tibble to store it with other period
            plot_trend = bind_rows(plot_trend, plot_trendtmp)

            # If there is a x axis limit
            if (!is.null(axis_xlim)) {
                # The x axis limit is selected
                codeDate = axis_xlim
            } else {
                # The entire date data is selected
                codeDate = df_data_code$Date
            }
            # The y limit is stored in a vector
            codeValue = c(minQ_win, maxQ_win)

            # Position of the x beginning and end of the legend symbol
            x = gpct(1.5, codeDate, shift=TRUE)
            xend = x + gpct(3, codeDate)

            # Spacing between legend symbols
            dy = gpct(9, codeValue, min_lim=ymin_lim)
            # Position of the y beginning and end of the legend symbol
            y = gpct(92, codeValue,
                     min_lim=ymin_lim, shift=TRUE) - (i-1)*dy
            yend = y

            # Position of x for the beginning of the associated text
            xt = xend + gpct(1, codeDate)

            # Position of the background rectangle of the legend
            xminR = x - gpct(1, codeDate)
            yminR = y - gpct(5, codeValue, min_lim=ymin_lim)
            # If it is a flow variable
            if (type == 'sévérité') {
                xmaxR = x + gpct(32.5, codeDate)
            # If it is a date variable
            } else if (type == 'saisonnalité' | type == 'pluviométrie' | type == 'température' | type == 'évapotranspiration') {
                xmaxR = x + gpct(20.5, codeDate)
            }
            ymaxR = y + gpct(5, codeValue, min_lim=ymin_lim)

            # Gets the trend
            trend = df_trend_code_per$trend
            # Gets the p value
            pVal = df_trend_code_per$p

            if (pVal <= alpha) {
                colorLine = color[i]
                colorLabel = color[i]
            } else {
                colorLine = 'grey85'
                colorLabel = 'grey85'
            }

            # Computes the mean trend
            trendMean = trend/dataMean
            # Computes the magnitude of the trend
            power = get_power(trend)
            # Converts it to character
            powerC = as.character(power)
            # If the power is positive
            if (powerC >= 0) {
                # Adds a space in order to compensate for the minus
                # sign that sometimes is present for the other periods
                spaceC = '  '
            # Otherwise
            } else {
                # No space is added
                spaceC = ''
            }

            # Gets the power of ten of magnitude
            brk = 10^power
            # Converts trend to character for sientific expression
            trendC = as.character(format(round(trend / brk, 2),
                                         nsmall=2))
            # If the trend is positive
            if (trendC >= 0) {
                # Adds two space in order to compensate for the minus
                # sign that sometimes is present for the other periods
                trendC = paste('  ', trendC, sep='')
            }
            # Converts mean trend to character
            trendMeanC = as.character(format(round(trendMean*100, 2),
                                             nsmall=2))
            if (trendMeanC >= 0) {
                # Adds two space in order to compensate for the minus
                # sign that sometimes is present for the other periods
                trendMeanC = paste('  ', trendMeanC, sep='')
            }

            # Create temporary tibble with variable to plot legend
            leg_trendtmp = tibble(x=x, xend=xend, 
                                  y=y, yend=yend, 
                                  xt=xt,
                                  colorLine=colorLine,
                                  colorLabel=colorLabel,
                                  trendC=trendC,
                                  powerC=powerC,
                                  spaceC=spaceC,
                                  trendMeanC=trendMeanC,
                                  xminR=xminR, yminR=yminR,
                                  xmaxR=xmaxR, ymaxR=ymaxR,
                                  period=i)
            # Bind it to the main tibble to store it with other period
            leg_trend = bind_rows(leg_trend, leg_trendtmp)  
        }

        if (length(linetype_per) < nPeriod_trend) {
            linetype_per = rep(linetype_per, times=nPeriod_trend)
        }

        linetypeLeg_per = linetype_per
        linetypeLeg_per[linetype_per == 'longdash'] = '33'
        linetypeLeg_per[linetype_per == 'dashed'] = '22'
        linetypeLeg_per[linetype_per == 'dotted'] = '11'
        
        # For all periods
        for (i in 1:nPeriod_trend) {
            # Extract the trend of the current sub period
            leg_trend_per = leg_trend[leg_trend$period == i,]

            # Plot the background for legend
            p = p +
                geom_rect(data=leg_trend_per,
                          aes(xmin=xminR, 
                              ymin=yminR, 
                              xmax=xmaxR, 
                              ymax=ymaxR),
                          linetype=0, fill='white', alpha=0.3)

            # Get the character variable for naming the trend
            colorLine = leg_trend_per$colorLine
            colorLabel = leg_trend_per$colorLabel
            trendC = leg_trend_per$trendC
            powerC = leg_trend_per$powerC
            spaceC = leg_trend_per$spaceC
            trendMeanC = leg_trend_per$trendMeanC

            # If it is a flow variable
            if (type == 'sévérité') {
                # Create the name of the trend
                label = bquote(bold(.(trendC)~'x'~'10'^{.(powerC)}*.(spaceC))~'['*m^{3}*'.'*s^{-1}*'.'*an^{-1}*']'~~bold(.(trendMeanC))~'[%.'*an^{-1}*']')
                    
            # If it is a date variable
            } else if (type == 'saisonnalité') {
                # Create the name of the trend
                label = bquote(bold(.(trendC)~'x'~'10'^{.(powerC)}*.(spaceC))~'[jour.'*an^{-1}*']')
            } else if (type == 'pluviométrie' | type == 'évapotranspiration') {
                # Create the name of the trend
                label = bquote(bold(.(trendC)~'x'~'10'^{.(powerC)}*.(spaceC))~'[mm.'*an^{-1}*']')
            } else if (type == 'température') {
                # Create the name of the trend
                label = bquote(bold(.(trendC)~'x'~'10'^{.(powerC)}*.(spaceC))~'[°C.'*an^{-1}*']')
            }

            # Plot the trend symbole and value of the legend
            p = p +
                annotate("segment",
                         x=leg_trend_per$x, xend=leg_trend_per$xend,
                         y=leg_trend_per$y, yend=leg_trend_per$yend,
                         color=colorLine,
                         linetype=linetypeLeg_per[i],
                         lwd=0.8,
                         lineend="round") +

                annotate("text",
                         label=label, size=2.8,
                         x=leg_trend_per$xt, y=leg_trend_per$y, 
                         hjust=0, vjust=0.5,
                         color=colorLabel)
        }

        # For all periods
        for (i in 1:nPeriod_trend) {
            # Extract the trend of the current sub period
            plot_trend_per = plot_trend[plot_trend$period == i,]
            
            # Plot the line of white background of each trend
            p = p + 
                geom_line(data=plot_trend_per, 
                          aes(x=abs, y=ord),
                          color='white',
                          linetype='solid',
                          size=1.5,
                          lineend="round")
        }
        
        # For all periods
        for (i in 1:nPeriod_trend) {
            # Extract the trend of the current sub period
            plot_trend_per = plot_trend[plot_trend$period == i,]

            # Plot the line of trend
            p = p + 
                geom_line(data=plot_trend_per, 
                          aes(x=abs, y=ord),
                          color=color[i],
                          linetype=linetype_per[i],
                          size=0.75,
                          lineend="round")
        }
    }

    # Y axis title
    # If it is a flow variable
    if (type == 'sévérité' | var == 'Q') {
        p = p +
            ylab(bquote(bold(.(var))~~'['*m^{3}*'.'*s^{-1}*']'))
    } else if (var == 'sqrt(Q)') {
        p = p +
            ylab(bquote(bold(.(var))~~'['*m^{3/2}*'.'*s^{-1/2}*']'))
    # If it is a date variable
    } else if (type == 'saisonnalité') {
        p = p +
            ylab(bquote(bold(.(var))~~"[jour de l'année]"))
    } else if (type == 'pluviométrie' | type == 'évapotranspiration') {
        p = p +
            ylab(bquote(bold(.(var))~~'[mm]'))
    } else if (type == 'température') {
        p = p +
            ylab(bquote(bold(.(var))~~"[°C]"))
    }
    
    if (!last & !first) {
        p = p + 
            theme(axis.text.x=element_blank())
    }

    if (first) {
        position = 'top'
    } else {
        position = 'bottom'
    }

    if (is.null(axis_xlim)) {
        limits = c(min(df_data_code$Date), max(df_data_code$Date))
    } else {
        limits = axis_xlim
    }

    if (breakDate < 1) {
        breaks = waiver()
        minor_breaks = waiver()
        date_labels = waiver()
    } else {
        breaks = seq(minDate_lim, maxDate_lim,
                     by=paste(breakDate, 'years'))
        minor_breaks = seq(minor_minDate_lim,
                           minor_maxDate_lim,
                           by=paste(minor_breakDate,
                                    'years'))
        date_labels = "%Y"
    }

    # Parameters of the x axis contain the limit of the date data
    p = p +
        scale_x_date(breaks=breaks,
                     minor_breaks=minor_breaks,
                     guide='axis_minor',
                     date_labels=date_labels,
                     limits=limits,
                     position=position, 
                     expand=c(0, 0))    
    
    # If it is a date variable 
    if (type == 'saisonnalité') {
        # The number of digit is 6 because months are display
        # with 3 characters
        Nspace = 6
        
        prefix = strrep(' ', times=NspaceMax-Nspace)
        accuracy = NULL
        
    # If it is a flow variable
    } else if (type == 'sévérité' | type == 'data' | type == 'pluviométrie' | type == 'température' | type == 'évapotranspiration') {
        # Gets the max number of digit on the label
        maxtmp = max(df_data_code$Value, na.rm=TRUE)
        # Taking into account of the augmentation of
        # max for the window
        maxtmp = maxtmp * (1 + lim_pct/100)

        # If the max is greater than 10
        if (maxtmp >= 10) {
            # The number of digit is the magnitude plus
            # the first number times 2
            Nspace = (get_power(maxtmp) + 1)*2
            # Plus spaces between thousands hence every 8 digits
            Nspace = Nspace + as.integer(Nspace/8)            
            # Gets the associated number of white space
            prefix = strrep(' ', times=NspaceMax-Nspace)
            # The accuracy is 1
            accuracy = 1
            
        # If the max is less than 10 and greater than 1
        } else if (maxtmp < 10 & maxtmp >= 1) {
            # The number of digit is the magnitude plus
            # the first number times 2 plus 1 for the dot
            # and 2 for the first decimal
            Nspace = (get_power(maxtmp) + 1)*2 + 3
            # Gets the associated number of white space
            prefix = strrep(' ', times=NspaceMax-Nspace)
            # The accuracy is 0.1
            accuracy = 0.1
            
        # If the max is less than 1 (and obviously more than 0)
        } else if (maxtmp < 1) {
            # Fixes the number of significant decimals to 3
            maxtmp = signif(maxtmp, 3)
            # The number of digit is the number of character
            # of the max times 2 minus 1 for the dots that
            # count just 1 space
            Nspace = nchar(as.character(maxtmp))*2 - 1
            # Gets the associated number of white space
            prefix = strrep(' ', times=NspaceMax-Nspace)
            # Computes the accuracy
            accuracy = 10^(-nchar(as.character(maxtmp))+2)
        }
    }
    
    # Parameters of the y axis
    # If it is a flow variable
    if (type == 'sévérité' | type == 'data' | type == 'pluviométrie' | type == 'température' | type == 'évapotranspiration') {        
        p = p +
            scale_y_continuous(breaks=seq(minQ_lim, maxQ_lim, breakQ),
                               limits=c(minQ_win, maxQ_win),
                               expand=c(0, 0),
                               labels=number_format(accuracy=accuracy,
                                                    prefix=prefix))
    # If it is a date variable
    } else if (type == 'saisonnalité') {
        # monthNum = as.numeric(format(seq(as.Date(minQ_lim),
                                       # as.Date(maxQ_lim),
                                       # by=paste(breakQ, 'days')),
        # "%m"))

        monthStart = as.Date(paste(substr(as.Date(minQ_lim), 1, 7),
                                   '-01', sep=''))
        monthEnd = as.Date(paste(substr(as.Date(maxQ_lim), 1, 7),
                                 '-01', sep=''))

        byMonth = round(breakQ/30.4, 0)
        if (byMonth == 0) {
            byMonth = 1
        }
        
        breaksDate = seq(monthStart, monthEnd,
                         by=paste(byMonth, 'months'))
        breaksNum = as.numeric(breaksDate)
        breaksMonth = as.numeric(format(breaksDate, "%m"))

        monthName = c('Jan', 'Fév', 'Mar', 'Avr', 'Mai', 'Jui',
                      'Jui', 'Aou', 'Sep', 'Oct', 'Nov', 'Déc')      
        monthName = paste(prefix, monthName, sep='')
        
        labels = monthName[breaksMonth]
        
        p = p +
            scale_y_continuous(breaks=breaksNum,
                               limits=c(minQ_win, maxQ_win),
                               labels=labels,  
                               expand=c(0, 0))
        
    }
    return(p)
}

### 2.2. Info panel __________________________________________________
# Plots the header that regroups all the info on the station
info_panel = function(list_df2plot, df_meta, trend_period=NULL,
                      mean_period=NULL, periodHyd=NULL,
                      df_shapefile=NULL, codeLight=NULL,
                      df_data_code=NULL, to_do='all') {
    
    # If there is a data serie for the given code
    if (!is.null(df_data_code)) {
        # Computes the hydrograph
        hyd = hydrograph_panel(df_data_code, period=periodHyd,
                               margin=margin(t=0, r=0, b=0, l=5,
                                             unit="mm"))
    # Otherwise
    } else {
        # Puts it blank
        hyd = void
    }

    if (!is.null(df_shapefile)) {
        # Computes the map associated to the station
        map =  map_panel(list_df2plot,
                         df_meta,
                         trend_period=trend_period,
                         mean_period=mean_period,
                         df_shapefile=df_shapefile,
                         codeLight=codeLight,
                         mapType='mini',
                         margin=margin(t=0, r=-12, b=0, l=0,
                                       unit="mm"),
                         showSea=FALSE,
                         verbose=FALSE)
    # Otherwise
    } else {
        # Puts it blank
        map = void
    }

    # Gets the metadata about the station
    df_meta_code = df_meta[df_meta$code == codeLight,]

    if ('name' %in% to_do | 'all' %in% to_do) {
        # Extracts the name
        nom = df_meta_code$nom
        # Corrects some errors about the formatting of title with dash
        nom = gsub("-", "-&nbsp;", nom)
        # Name of the datasheet
        text1 = paste(
            "<b>", codeLight, '</b>  -  ', nom,
            sep='')
        # Converts all texts to graphical object in the right position
        gtext1 = richtext_grob(text1,
                               x=0, y=1,
                               margin=unit(c(t=0, r=5, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col="#00A3A8", fontsize=14))
    } else if ('code' %in% to_do) {
        # Name of the datasheet
        text1 = paste(
            "<b>", codeLight, '</b>',
            sep='')
        # Converts all texts to graphical object in the right position
        gtext1 = richtext_grob(text1,
                               x=0, y=1,
                               margin=unit(c(t=0, r=5, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col="#00A3A8", fontsize=14))
    } else {
        gtext1 = void
    }

    # Subitle info
    if ('loc' %in% to_do | 'all' %in% to_do) {
        text2 = paste(
            "<b>",
            "Gestionnaire : ", df_meta_code$gestionnaire, "<br>",
            "Bassin hydrographique : ", df_meta_code$region_hydro,
            "</b>",
            sep='')
        gtext2 = richtext_grob(text2,
                               x=0, y=1.25,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col="grey20", fontsize=8))
    } else {
        gtext2 = void
    }

    # Spatial info about station
    if ('spatial' %in% to_do | 'all' %in% to_do) {
        text3 = paste(
            "<b>",
            "Superficie : ", df_meta_code$surface_km2_BH,
            "  [km<sup>2</sup>] <br>",
            "Altitude : ", df_meta_code$altitude_m_BH, "  [m]<br>",
            "X = ", df_meta_code$L93X_m_BH, "  [m ; Lambert93]<br>",
            "Y = ", df_meta_code$L93Y_m_BH, "  [m ; Lambert93]",
            "</b>",
            sep='')
        gtext3 = richtext_grob(text3,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col="grey20", fontsize=9))
    } else {
        gtext3 = void
    }

    # Time info about station
    if ('temporal' %in% to_do | 'all' %in% to_do) {
        # Computes the time span of data, the start and the end
        duration = as.numeric(format(as.Date(df_meta_code$fin),
                                     "%Y")) -
            as.numeric(format(as.Date(df_meta_code$debut), "%Y"))
        debut = format(as.Date(df_meta_code$debut), "%d/%m/%Y")
        fin = format(as.Date(df_meta_code$fin), "%d/%m/%Y")

        text4 = paste(
            "<b>",
            "Date de début : ", debut, "<br>",
            "Date de fin : ", fin, "<br>",
            "Nombre d'années : ", duration, "  [ans]", "<br>",
            "Taux de lacunes : ", signif(df_meta_code$tLac100, 2),
            "  [%]",
            "</b>",
            sep='')
        gtext4 = richtext_grob(text4,
                               x=0, y=1,
                               margin=unit(c(t=0, r=0, b=0, l=0),
                                           "mm"),
                               hjust=0, vjust=1,
                               gp=gpar(col="grey20", fontsize=9))
    } else {
        gtext4 = void
    }


    # Makes a list of all plots
    P = list(gtext1, gtext2, gtext3, gtext4, hyd, map)
    # P = list(void, void, void, void, void, void, void)
    
    # Creates the matrix layout
    LM = matrix(c(1, 1, 1, 6,
                  2, 2, 5, 6,
                  3, 4, 5, 6,
                  3, 4, 5, 6),
                nrow=4, 
                byrow=TRUE)
    # And sets the relative height of each plot
    heights = rep(1, times=nrow(LM))
    # heights[2] = 0.1
    heights[2] = 0.8

    # Arranges all the graphical objetcs
    plot = grid.arrange(grobs=P,
                        layout_matrix=LM,
                        heights=heights)
    # Return the plot object
    return(plot)
} 

### 2.3. Hydrograph panel ____________________________________________
# Creates a hydrograph for a station with the data serie of flow
hydrograph_panel = function (df_data_code, period, margin=NULL) {

    # Computes the hydrograph
    res_hydrograph = get_hydrograph(df_data_code, period=period)
    # Extracts the results
    monthMean = res_hydrograph$QM
    regime_hydro = res_hydrograph$meta
    
    # Vector of month index
    monthNum = 1:12
    # Vector of month name abbreviation
    monthName = c("J", "F", "M", "A", "M", "J",
                  "J", "A", "S", "O", "N", "D")

    # Open a new plot with the personalise theme
    hyd = ggplot() + theme_ash +
        # Theme modification
        theme(
            # plot.background=element_rect(fill=NA, color="#EC4899"),
            panel.border=element_blank(),
            axis.text.x=element_text(margin=unit(c(0, 0, 0, 0), "mm"),
                                     vjust=1, hjust=0.5),
            axis.ticks.x=element_blank(),
            axis.line.y=element_line(color='grey85', size=0.3),
            plot.title=element_text(size=8, vjust=-0.5, 
                                    hjust=-1E-3, color='grey40'),
            axis.title.y=element_text(size=8, vjust=0, 
                                      hjust=0.5,
                                      color='grey40')) +
        
        # Adds a title to the y axis
        ggtitle(regime_hydro) +
        # Y axis title
        ylab(bquote(bold('QM')~~'['*m^{3}*'.'*s^{-1}*']'))
    
    # If there is no margins specified
    if (is.null(margin)) {
        # Sets all margins to 0
        hyd = hyd + 
            theme(plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"))
    # Otherwise
    } else {
        # Sets margins to the given ones
        hyd = hyd + 
            theme(plot.margin=margin)
    }

    hyd = hyd +
        # Plots the bar
        geom_bar(aes(x=monthNum, y=monthMean), 
                 stat='identity',
                 fill="grey70",
                 width=0.75, size=0.2) +
        # X axis
        scale_x_continuous(breaks=monthNum,
                           labels=monthName,
                           limits=c(0, max(monthNum)+0.5),
                           expand=c(0, 0)) + 
        # Y axis
        scale_y_continuous(limits=c(0, max(monthMean)),
                           n.breaks=4,
                           expand=c(0, 0))
    # Returns the plot
    return (hyd)
}
