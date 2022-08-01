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
# R/plotting/layout.R
#
# Regroups general parameters about plotting like the theme used ang
# color management. It mainly deals with the calling to specific
# plotting functions and the organisation of each plot for the
# generation of the PDF.


## 1. PERSONALISATION ________________________________________________
### 1.1. Personal theme ______________________________________________
#' @title Ggplot2 theme ash
#' @export
theme_ash = function () {
    theme =
        theme(
            # White background
            panel.background=element_rect(fill='white'),
            # Font
            text=element_text(family='sans'),
            # Border of plot
            panel.border = element_rect(color="grey80",
                                        fill=NA,
                                        size=0.7),
            # Grid
            panel.grid.major.x=element_blank(),
            panel.grid.major.y=element_blank(),
            # Ticks marker
            axis.ticks.x=element_line(color='grey75', size=0.3),
            axis.ticks.y=element_line(color='grey75', size=0.3),
            # Ticks label
            axis.text.x=element_text(color='grey40'),
            axis.text.y=element_text(color='grey40'),
            # Ticks length
            axis.ticks.length=unit(1.5, 'mm'),
            # Ticks minor
            ggh4x.axis.ticks.length.minor=rel(0.5),
            # Title
            plot.title=element_blank(),
            # Axis title
            axis.title.x=element_blank(),
            axis.title.y=element_text(size=9, vjust=1.2, 
                                      hjust=0.5, color='grey20'),
            # Axis line
            axis.line.x=element_blank(),
            axis.line.y=element_blank()
            )
    return (theme)
}

### 1.2. Color palette _______________________________________________
#' @title Palette ground
#' @export
Palette_ground = function () {
    palette = c("#543005",
                "#8c510a",
                "#bf812d",
                "#dfc27d",
                "#f6e8c3",
                "#c7eae5",
                "#80cdc1",
                "#35978f",
                "#01665e",
                "#003c30")
    return (palette)
}

#' @title Color event
#' @export
get_colorEvent = function () {
    colorEvent = c("#423089", "#9ed6e3", "#9dc544", "#ed6e6c")
    names(colorEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorEvent)
}

#' @title Text color event
#' @export
get_colorTextEvent = function () {
    colorTextEvent = c("#9687d5", "#d8eff4", "#cee2a2", "#f6b6b5")
    names(colorTextEvent) = c("Crue", "Crue Nivale", "Moyennes Eaux", "Étiage")
    return(colorTextEvent)
}

## 2. USEFUL GENERICAL PLOT __________________________________________
### 2.1. Void plot ___________________________________________________
# A plot completly blank
#' @title Void plot
#' @export
void = function () {
    plot = ggplot() + geom_blank(aes(1,1)) +
        theme(
            plot.background = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
        )
    return (plot)
}


### 2.2. Contour void plot ___________________________________________
# A plot completly blank with a contour
#' @title Contour plot
#' @export
contour = function () {
    plot = ggplot() + geom_blank(aes(1,1)) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            plot.background=element_rect(fill=NA, color="#EC4899"),
            plot.margin=margin(t=0, r=0, b=0, l=0, unit="mm"))
    return (plot)
}

#' @title Switch color label
#' @export
switch_colorLabel = function (color) {
    #switch 12% https://mdigi.tools/darken-color/#f6e8c3
    if (color == "#F6E8C3") {
        newColor = "#efd695"
        
    } else if (color == "#C7EAE5") {
        newColor = "#a1dcd3"
        
    } else {
        newColor = color
    }
    return (newColor)
}

#' @title Get reverse
#' @export
get_reverse = function (var) {
    # gets the color corresponding to the mean trend
    reverse = FALSE
    if (grepl('^tFIN', var) | grepl('^t[_]', var) | grepl('^v', var)) {
        reverse = TRUE
    }
    return (reverse)
}


## 3. LAYOUT _________________________________________________________
# Generates a PDF that gather datasheets, map and summarize table about the trend analyses realised on selected stations
#' @title Layout panel
#' @export
layout_panel = function (df_data, df_meta, structure, layout_matrix,
                         to_plot=c('datasheet', 'table', 'map',
                                   'map_regime', 'map_trend', 'map_mean'),
                         figdir='', filedir_opt='', filename_opt='',
                         variable='', df_trend=NULL,
                         alpha=0.1, unit2day=365.25, var='',
                         type='', event='', unit='', hydroPeriod='',
                         glose=NULL, trend_period=NULL,
                         mean_period=NULL, colorForce=FALSE,
                         exQprob=0.01,
                         linetype_per='solid',
                         axis_xlim=NULL,
                         paper_size='A4',
                         time_header=NULL,
                         info_header=NULL, foot_note=TRUE,
                         info_height=2.8, time_height=3,
                         var_ratio=3, foot_height=1.25,
                         shapefile_list=NULL,
                         resdir=NULL,
                         logo_path=NULL,
                         zone_to_show=NULL,
                         pdf_chunk=c('all'),
                         show_colorEvent=FALSE) {

    dateFile = format(Sys.Date(), "%Y%m%d")

    # Name of the document
    outfile = "ASH"
    # If there is an option to mention in the filename it adds it
    if (filename_opt != '') {
        outfile = paste0(outfile, '_', filename_opt)
    }
    # Add the 'pdf' extensionto the name
    outfile = paste0(outfile, '_', dateFile, '.pdf')

    # If there is not a dedicated figure directory it creats one
    outdir = file.path(figdir, filedir_opt)
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }

    # If there is not a dedicated figure directory it creats one
    outdir_code = file.path(figdir, filedir_opt, paste0('ASH_', dateFile))
    if (!(file.exists(outdir_code))) {
        dir.create(outdir_code)
    }

    # Names of a temporary directory to store all the independent pages
    outdirTmp = file.path(outdir, 'tmp')
    # Creates it if it does not exist
    if (!(file.exists(outdirTmp))) {
        dir.create(outdirTmp)
    # If it already exists it deletes the pre-existent directory
    # and recreates one
    } else {
        if (!is.null(to_plot)) {
            unlink(outdirTmp, recursive=TRUE)
            dir.create(outdirTmp)
        }
    }

    outdirTmp_pdf = file.path(outdirTmp, 'pdf')
    # Creates it if it does not exist
    if (!(file.exists(outdirTmp_pdf))) {
        dir.create(outdirTmp_pdf)
    }

    outdirTmp_png = file.path(outdirTmp, 'png')
    # Creates it if it does not exist
    if (!(file.exists(outdirTmp_png))) {
        dir.create(outdirTmp_png)
    }

    # Number of type/variable
    nbp = length(df_data)

    # Convert data tibble to list of tibble if it is not the case
    if (all(class(df_data) != 'list')) {
        df_data = list(df_data)
    }

    if (all(class(df_trend) != 'list')) {
        df_trend = list(df_trend)
    }

    if (length(alpha) != nbp) {
        alpha = rep(alpha[1], nbp)
    }
    
    if (length(unit2day) != nbp) {
        unit2day = rep(unit2day[1], nbp)
    }

    if (length(var) != nbp) {
        var = rep(var[1], nbp)
    }

    if (length(glose) != nbp) {
        glose = rep(glose[1], nbp)
    }

    if (length(type) != nbp) {
        type = rep(type[1], nbp)
    }

    if (length(event) != nbp) {
        event = rep(event[1], nbp)
    }

    if (length(unit) != nbp) {
        unit = rep(unit[1], nbp)
    }

    if (length(hydroPeriod) != nbp) {
        hydroPeriod = rep(hydroPeriod, nbp)
    }

    # Creates a blank list to store all the data of each type of plot
    list_df2plot = vector(mode='list', length=nbp)

    # For all the type of graph / number of studied variables
    for (i in 1:nbp) {
        # Creates a list that gather all the info for one type of graph
        df2plot = list(data=df_data[[i]], 
                       trend=df_trend[[i]],
                       alpha=alpha[[i]],
                       unit2day=unit2day[[i]],
                       var=var[[i]],
                       type=type[[i]],
                       event=event[[i]],
                       unit=unit[[i]],
                       hydroPeriod=hydroPeriod[[i]],
                       glose=glose[[i]])
        # Stores it
        list_df2plot[[i]] = df2plot
    }

    if ('summary' %in% to_plot) {
        df_page = tibble(section='Sommaire', subsection=NA, n=1)
    } else {
        df_page = tibble()
    }
    
    # If map needs to be plot
    if ('map' %in% to_plot | 'map_regime' %in% to_plot) {
            df_page = map_panel(NULL, 
                                df_meta,
                                idPer_trend=length(trend_period),
                                trend_period=trend_period,
                                mean_period=mean_period,
                                colorForce=colorForce,
                                exQprob=exQprob,
                                mapType='regime',
                                shapefile_list=shapefile_list,
                                foot_note=foot_note,
                                foot_height=foot_height,
                                zone_to_show=zone_to_show,
                                logo_path=logo_path,
                                outdirTmp_pdf=outdirTmp_pdf,
                                outdirTmp_png=outdirTmp_png, 
                                df_page=df_page,
                                verbose=FALSE)
    }
            
    if ('map' %in% to_plot | 'map_trend' %in% to_plot) {
        df_page = map_panel(list_df2plot, 
                            df_meta,
                            idPer_trend=length(trend_period),
                            trend_period=trend_period,
                            mean_period=mean_period,
                            colorForce=colorForce,
                            exQprob=exQprob,
                            mapType='trend',
                            shapefile_list=shapefile_list,
                            foot_note=foot_note,
                            foot_height=foot_height,
                            zone_to_show=zone_to_show,
                            logo_path=logo_path,
                            outdirTmp_pdf=outdirTmp_pdf,
                            outdirTmp_png=outdirTmp_png, 
                            df_page=df_page)
    }
    
    if ('map' %in% to_plot | 'map_mean' %in% to_plot) {     
            df_page = map_panel(list_df2plot, 
                                df_meta,
                                idPer_trend=length(trend_period),
                                trend_period=trend_period,
                                mean_period=mean_period,
                                colorForce=colorForce,
                                exQprob=exQprob,
                                mapType='mean',
                                shapefile_list=shapefile_list,
                                foot_note=foot_note,
                                foot_height=foot_height,
                                zone_to_show=zone_to_show,
                                logo_path=logo_path,
                                outdirTmp_pdf=outdirTmp_pdf,
                                outdirTmp_png=outdirTmp_png, 
                                df_page=df_page)
    }

    # If summarize table needs to be plot
    if ('table' %in% to_plot) {
        df_page = table_panel(list_df2plot,
                              df_meta,
                              trend_period,
                              mean_period,
                              colorForce=colorForce,
                              exQprob=exQprob,
                              slice=19,
                              paper_size='A3',
                              foot_note=foot_note,
                              foot_height=foot_height,
                              resdir=resdir,
                              logo_path=logo_path,
                              outdirTmp_pdf=outdirTmp_pdf,
                              outdirTmp_png=outdirTmp_png, 
                              df_page=df_page)
    }

    # If datasheets needs to be plot
    if ('datasheet' %in% to_plot) {
        df_page = datasheet_panel(list_df2plot,
                                  df_meta,
                                  trend_period=trend_period,
                                  mean_period=mean_period,
                                  linetype_per=linetype_per,
                                  axis_xlim=axis_xlim,
                                  colorForce=colorForce,
                                  exQprob=exQprob,
                                  info_header=info_header,
                                  time_header=time_header,
                                  foot_note=foot_note,
                                  structure=structure,
                                  layout_matrix=layout_matrix,
                                  info_height=info_height,
                                  time_height=time_height,
                                  var_ratio=var_ratio,
                                  foot_height=foot_height,
                                  paper_size=paper_size,
                                  shapefile_list=shapefile_list,
                                  logo_path=logo_path,
                                  zone_to_show=zone_to_show,
                                  show_colorEvent=show_colorEvent,
                                  outdirTmp_pdf=outdirTmp_pdf,
                                  outdirTmp_png=outdirTmp_png, 
                                  df_page=df_page,
                                  pdf_chunk=pdf_chunk)
    }

    if ('summary' %in% to_plot) {
        summary_panel(df_page,
                      foot_note,
                      foot_height,
                      logo_path=logo_path,
                      outdirTmp_pdf=outdirTmp_pdf,
                      outdirTmp_png=outdirTmp_png)
    }

    # Combine independant pages into one PDF
    details = file.info(list.files(outdirTmp_pdf, full.names=TRUE))
    details = details[with(details, order(as.POSIXct(mtime))),]
    listfile_path = rownames(details)

    if ('summary' %in% to_plot) {
        summary_path = listfile_path[length(listfile_path)]
        listfile_path = listfile_path[-length(listfile_path)]
        listfile_path = c(summary_path, listfile_path)
    }

    if (pdf_chunk == 'by_code') {
        # Get all different stations code
        Code = rle(df_data[[1]]$Code)$value
        for (code in Code) {
            listfile_code_path = listfile_path[grepl(code, listfile_path)]
            pdf_combine(input=listfile_code_path,
                        output=file.path(outdir_code, paste0(code, '.pdf')))
        }
    }
    
    if (pdf_chunk == 'all') {
        pdf_combine(input=listfile_path,
                    output=file.path(outdir, outfile))
    }
} 


## 4. PDF ORGANISATION PANEL _________________________________________
### 4.1. Summary _____________________________________________________
#' @title Summary panel
#' @export
summary_panel = function (df_page, foot_note, foot_height,
                          logo_path,
                          outdirTmp_pdf, outdirTmp_png) {
    
    text_title = paste(
        "<b>Analyse de Stationnarité Hydrologique</b>",
        sep='')
    
    text_subtitle = paste(
        "Bassin Adour-Garonnne",
        sep='')

    Sec_name = rle(df_page$section)$values
    nSec = length(Sec_name)
    
    text_sum1 = ''
    text_page1 = ''
    text_sum2 = ''
    text_page2 = ''
    
    nline = 0
    nline_max = 58
    for (idS in 1:nSec) {
        sec_name = Sec_name[idS]
        subSec_name = rle(df_page$subsection[df_page$section == sec_name])$values
        n_page = df_page$n[df_page$section == sec_name][1]

        line = paste("<b>", idS, ". ", sec_name, "</b>", "<br>", sep='')
        page = paste("<b>p.", n_page, "</b><br>", sep='')
        
        if (nline <= nline_max) {
            text_sum1 = paste(text_sum1, line, sep='')
            text_page1 = paste(text_page1, page, sep='')
        } else {
            text_sum2 = paste(text_sum2, line, sep='')
            text_page2 = paste(text_page2, page, sep='')
        }

        nline = nline + 1
        
        nSSec = length(subSec_name)
        for (idSS in 1:nSSec) {
            subsec_name = subSec_name[idSS]
            if (!is.na(subsec_name)) {
                n_page = df_page$n[df_page$section == sec_name &
                                   df_page$subsection == subsec_name][1]

                line = paste("<b>", idS, ".", idSS, ".</b> ",
                             subsec_name, "<br>", sep='')
                page = paste("p.", n_page, "<br>", sep='')
                
                if (nline <= nline_max) {
                    text_sum1 = paste(text_sum1, line, sep='')
                    text_page1 = paste(text_page1, page, sep='')
                } else {
                    text_sum2 = paste(text_sum2, line, sep='')
                    text_page2 = paste(text_page2, page, sep='')
                }

                nline = nline + 1
            }
        }
        if (nline <= nline_max) {
            text_sum1 = paste(text_sum1, "<br>", sep='')
            text_page1 = paste(text_page1, "<br>", sep='')
        } else {
            text_sum2 = paste(text_sum2, "<br>", sep='')
            text_page2 = paste(text_page2, "<br>", sep='')
        }
        nline = nline + 1
    }

    # text_sum1 = gsub(" ", "<span style='color:white'>&#95;</span>",
                     # text_sum1)
    text_sum1 = gsub('[.]', '&#46;', text_sum1)
    text_page1 = gsub('[.]', '&#46;', text_page1)
    
    # text_sum2 = gsub(" ", "<span style='color:white'>&#95;</span>",
                     # text_sum2)
    text_sum2 = gsub('[.]', '&#46;', text_sum2)
    text_page2 = gsub('[.]', '&#46;', text_page2)

    
    # Converts all texts to graphical object in the right position
    gtitle = richtext_grob(text_title,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=20))

    gsubtitle = richtext_grob(text_subtitle,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=15))

    gsum1 = richtext_grob(text_sum1,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col="#00A3A8", fontsize=10))
    
    gpage1 = richtext_grob(text_page1,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=10))

    gsum2 = richtext_grob(text_sum2,
                          x=0, y=1,
                          margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                          hjust=0, vjust=1,
                          gp=gpar(col="#00A3A8", fontsize=10))
    
    gpage2 = richtext_grob(text_page2,
                           x=0, y=1,
                           margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                           hjust=0, vjust=1,
                           gp=gpar(col="#00A3A8", fontsize=10))
    
    
    # If there is a foot note
    if (foot_note) {
        footName = 'sommaire'
        foot = foot_panel(footName,
                          1, foot_height, logo_path)

        P = list(gtitle, gsubtitle, gsum1, gpage1, gsum2, gpage2, foot)
        LM = matrix(c(1, 1, 1, 1,
                      2, 2, 2, 2,
                      3, 4, 5, 6,
                      7, 7, 7, 7),
                    nrow=4, byrow=TRUE)
    } else {
        foot_height = 0
        P = list(gtitle, gsubtitle, gsum1, gpage1, gsum2, gpage2)
        LM = matrix(c(1, 1, 1, 1,
                      2, 2, 2, 2,
                      3, 4, 5, 6),
                    nrow=3, byrow=TRUE)
    }
    id_title = 1
    id_subtitle = 2
    id_page1 = 4
    id_page2 = 6
    id_foot = 7

    LMcol = ncol(LM)
    LMrow = nrow(LM)
    
    LM = rbind(rep(99, times=LMcol), LM, rep(99, times=LMcol))
    LMrow = nrow(LM)
    LM = cbind(rep(99, times=LMrow), LM, rep(99, times=LMrow))
    LMcol = ncol(LM)

    title_height = 0.75
    subtitle_height = 1.25
    margin_size = 0.5
    page_width = 2
    height = 29.7
    width = 21

    row_height = (height - 2*margin_size - foot_height - title_height - subtitle_height) / (LMrow - 5)

    Hcut = LM[, 2]
    heightLM = rep(row_height, times=LMrow)
    heightLM[Hcut == id_title] = title_height
    heightLM[Hcut == id_subtitle] = subtitle_height
    heightLM[Hcut == id_foot] = foot_height
    heightLM[Hcut == 99] = margin_size

    col_width = (width - 2*margin_size - 2*page_width) / (LMcol - 4)
    
    Wcut = LM[4,]
    widthLM = rep(col_width, times=LMcol)
    widthLM[Wcut ==  id_page1 | Wcut ==  id_page2] = page_width
    widthLM[Wcut == 99] = margin_size

    # Arranges the graphical object
    plot = grid.arrange(grobs=P, layout_matrix=LM,
                        heights=heightLM, widths=widthLM)
    
    # Saves the plot
    ggsave(plot=plot,
           path=outdirTmp_pdf,
           filename=paste('sommaire', '.pdf', sep=''),
           width=width, height=height, units='cm', dpi=100)
    
    ggsave(plot=plot,
           path=outdirTmp_png,
           filename=paste('sommaire', '.png', sep=''),
           width=width, height=height, units='cm', dpi=400)
}

### 4.2. Foot note panel______________________________________________
#' @title Foot panel
#' @export
foot_panel = function (name, n_page, foot_height, logo_path) {
    
    nLogo = length(logo_path)
    nbg = nLogo + 3
    P = vector(mode='list', length=nbg)
    P[[1]] = void()
    LM_row = c(1)
    widths = c(1)

    for (i in 1:nLogo) {
        path = logo_path[i]
        logo = names(logo_path)[i]
        img = readPNG(path)
        
        if (logo == 'PR') {
            grob = rasterGrob(img,
                              x=0, hjust=0,
                              width=unit(0.8*foot_height, "cm"))
            width = 0.2
        }
        if (logo == 'FR') { 
            grob = rasterGrob(img,
                              x=0, hjust=0,
                              width=unit(1*foot_height, "cm"))
            width = 0.2
        }
        if (logo == 'INRAE') {
            grob = rasterGrob(img,
                              y=0.565,
                              vjust=0.5,
                              width=unit(1.08*foot_height, "cm"))
            width = 0.25
        }
        if (logo == 'AEAG') {
            grob = rasterGrob(img,
                              y=0.49,
                              vjust=0.5,
                              width=unit(0.7*foot_height, "cm"))
            width = 0.2
        }
        P[[i+1]] = grob
        LM_row = c(LM_row, i+1)
        widths = c(widths, width)
    }

    text_page = paste(
        name, "  <b>p. ", n_page, "</b>",
        sep='')
    
    text_date = paste (
        format(Sys.Date(), "%B %Y"),
        sep='')

    # Converts all texts to graphical object in the right position
    gtext_page = richtext_grob(text_page,
                               x=1, y=0,
                               margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                               hjust=1, vjust=0.5,
                               gp=gpar(col="#00A3A8", fontsize=8))

    gtext_date = richtext_grob(text_date,
                               x=1, y=0.55,
                               margin=unit(c(t=0, r=0, b=0, l=0), "mm"),
                               hjust=1, vjust=0.5,
                               gp=gpar(col="#00A3A8", fontsize=6))

    P[[nLogo+2]] = gtext_page
    LM_row1 = c(LM_row, nLogo+2)
    P[[nLogo+3]] = gtext_date
    LM_row2 = c(LM_row, nLogo+3)
    widths = c(widths, 1)
    
    # Creates the matrix layout
    LM = matrix(c(LM_row1,
                  LM_row2),
                nrow=2, 
                byrow=TRUE)
    
    # Arranges all the graphical objetcs
    plot = grid.arrange(grobs=P,
                        layout_matrix=LM,
                        widths=widths)
    
    # Return the plot object
    return (plot)
}

