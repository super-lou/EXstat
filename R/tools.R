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
# R/plotting/tools.R


## 1. COLOR MANAGEMENT
### 1.1. Color on colorbar ___________________________________________
# Returns a color of a palette corresponding to a value included
# between the min and the max of the variable
#' @title Color on colorbar
#' @export
get_color = function (value, min, max, ncolor=256, palette_name='perso', reverse=FALSE) {

    
    # If the value is a NA return NA color
    if (is.na(value)) {
        return (NA)
    }
    
    # If the palette chosen is the personal ones
    if (palette_name == 'perso') {
        colorList = palette_perso()
    # Else takes the palette corresponding to the name given
    } else {
        colorList = brewer.pal(11, palette_name)
    }
    
    # Gets the number of discrete colors in the palette
    nSample = length(colorList)
    # Recreates a continuous color palette
    palette = colorRampPalette(colorList)(ncolor)
    # Separates it in the middle to have a cold and a hot palette
    Sample_hot = 1:(as.integer(nSample/2)+1)
    Sample_cold = (as.integer(nSample/2)+1):nSample
    palette_hot = colorRampPalette(colorList[Sample_hot])(ncolor)
    palette_cold = colorRampPalette(colorList[Sample_cold])(ncolor)

    # Reverses the palette if it needs to be
    if (reverse) {
        palette = rev(palette)
        palette_hot = rev(palette_hot)
        palette_cold = rev(palette_cold)
    }

    # Computes the absolute max
    maxAbs = max(abs(max), abs(min))

    # If the value is negative
    if (value < 0) {
        if (maxAbs == 0) {
            idNorm = 0
        } else {
            # Gets the relative position of the value in respect
            # to its span
            idNorm = (value + maxAbs) / maxAbs
        }
        # The index corresponding
        id = round(idNorm*(ncolor - 1) + 1, 0)        
        # The associated color
        color = palette_cold[id]
        
    # Same if it is a positive value
    } else {
        if (maxAbs == 0) {
            idNorm = 0
        } else {
            idNorm = value / maxAbs
        }
        id = round(idNorm*(ncolor - 1) + 1, 0)
        color = palette_hot[id]
    }
    return(color)
}

### 1.2. Colorbar ____________________________________________________
# Returns the colorbar but also positions, labels and colors of some
# ticks along it 
#' @title Colorbar
#' @export
get_palette = function (min, max, ncolor=256, palette_name='perso', reverse=FALSE, nbTick=10) {

    # If the value is a NA return NA color
    if (is.null(min) | is.null(max)) {
        return (NA)
    }
    
    # If the palette chosen is the personal ones
    if (palette_name == 'perso') {
        colorList = palette_perso()
    # Else takes the palette corresponding to the name given
    } else {
        colorList = brewer.pal(11, palette_name)
    }
    
    # Gets the number of discrete colors in the palette
    nSample = length(colorList)
    # Recreates a continuous color palette
    palette = colorRampPalette(colorList)(ncolor)
    # Separates it in the middle to have a cold and a hot palette
    Sample_hot = 1:(as.integer(nSample/2)+1)
    Sample_cold = (as.integer(nSample/2)+1):nSample
    palette_hot = colorRampPalette(colorList[Sample_hot])(ncolor)
    palette_cold = colorRampPalette(colorList[Sample_cold])(ncolor)

    # Reverses the palette if it needs to be
    if (reverse) {
        palette = rev(palette)
        palette_hot = rev(palette_hot)
        palette_cold = rev(palette_cold)
    }

    # If the min and the max are below zero
    if (min < 0 & max < 0) {
        # The palette show is only the cold one
        paletteShow = palette_cold
    # If the min and the max are above zero
    } else if (min > 0 & max > 0) {
        # The palette show is only the hot one
        paletteShow = palette_hot
    # Else it is the entire palette that is shown
    } else {
        paletteShow = palette
    }

    # The position of ticks is between 0 and 1
    posTick = seq(0, 1, length.out=nbTick)
    # Blank vector to store corresponding labels and colors
    labTick = c()
    colTick = c()
    # For each tick
    for (i in 1:nbTick) {
        # Computes the graduation between the min and max
        lab = (i-1)/(nbTick-1) * (max - min) + min
        # Gets the associated color
        col = get_color(lab, min=min, max=max,
                        ncolor=ncolor,
                        palette_name=palette_name,
                        reverse=reverse)
        # Stores them
        labTick = c(labTick, lab)
        colTick = c(colTick, col)
    }
    # List of results
    res = list(palette=paletteShow, posTick=posTick,
               labTick=labTick, colTick=colTick)
    return(res)
}

### 1.3. Palette tester ______________________________________________
# Allows to display the current personal palette
#' @title Palette tester
#' @export
palette_tester = function (palette_name='perso', figdir='figures', n=256) {

    outdir = file.path(figdir, 'palette')
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }
    
    # If the palette chosen is the personal ones
    if (palette_name == 'perso') {
        colorList = palette_perso()
    # Else takes the palette corresponding to the name given
    } else {
        colorList = brewer.pal(11, palette_name)
    }
    
    # An arbitrary x vector
    X = 1:n
    # All the same arbitrary y position to create a colorbar
    Y = rep(0, times=n)

    # Recreates a continuous color palette
    palette = colorRampPalette(colorList)(n)

    # Open a plot
    p = ggplot() + 
        # Make the theme blank
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

    for (x in X) {
        # Plot the palette
        # geom_line(aes(x=X, y=Y), color=palette[X], size=60,
        # shape=15)

        p = p +
            annotate("segment",
                     x=x, xend=x,
                     y=0, yend=1, 
                     color=palette[x], size=1)
    }

    p = p +
        scale_x_continuous(limits=c(0, n),
                           expand=c(0, 0)) +
        
        scale_y_continuous(limits=c(0, 1),
                           expand=c(0, 0))

    # Saves the plot
    ggsave(plot=p,
           path=outdir,
           filename=paste(palette_name, '.pdf', sep=''),
           width=10, height=10, units='cm', dpi=100)

    ggsave(plot=p,
           path=outdir,
           filename=paste(palette_name, '.png', sep=''),
           width=10, height=10, units='cm', dpi=300)
}


## 2. PERSONAL PLOT __________________________________________________
### 2.1. Circle ______________________________________________________
# Allow to draw circle in ggplot2 with a radius and a center position
#' @title Circle
#' @export
gg_circle = function(r, xc, yc, color="black", fill=NA, ...) {
    x = xc + r*cos(seq(0, pi, length.out=100))
    ymax = yc + r*sin(seq(0, pi, length.out=100))
    ymin = yc + r*sin(seq(0, -pi, length.out=100))
    annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color,
             fill=fill, ...)
}

### 2.2. Merge _______________________________________________________
#' @title Merge
#' @export
merge_panel = function (add, to, widths=NULL, heights=NULL) {
    # Plot the graph as the layout
    plot = grid.arrange(grobs=list(add, to),
                        heights=heights, widths=widths)
    return (plot)
}


## 3. NUMBER MANAGEMENT ______________________________________________
### 3.1. Number formatting ___________________________________________
# Returns the power of ten of the scientific expression of a value
#' @title Number formatting
#' @export
get_power = function (value) {

    # Do not care about the sign
    value = abs(value)
    
    # If the value is greater than one
    if (value >= 1) {
        # The magnitude is the number of character of integer part
        # of the value minus one
        power = nchar(as.character(as.integer(value))) - 1
    # If value is zero
    } else if (value == 0) {
        # The power is zero
        power = 0
    # If the value is less than one
    } else {
        # Extract the decimal part
        dec = gsub('0.', '', as.character(value), fixed=TRUE)
        # Number of decimal with zero
        ndec = nchar(dec)
        # Number of decimal without zero
        nnum = nchar(as.character(as.numeric(dec)))
        # Compute the power of ten associated
        power = -(ndec - nnum + 1)
    }
    return(power)
}

### 3.2. Pourcentage of variable _____________________________________
# Returns the value corresponding of a certain percentage of a
# data serie
#' @title Pourcentage of variable
#' @export
gpct = function (pct, L, min_lim=NULL, shift=FALSE) {

    # If no reference for the serie is given
    if (is.null(min_lim)) {
        # The minimum of the serie is computed
        minL = min(L, na.rm=TRUE)
    # If a reference is specified
    } else {
        # The reference is the minimum
        minL = min_lim
    }

    # Gets the max
    maxL = max(L, na.rm=TRUE)
    # And the span
    spanL = maxL - minL
    # Computes the value corresponding to the percentage
    xL = pct/100 * as.numeric(spanL)

    # If the value needs to be shift by its reference
    if (shift) {
        xL = xL + minL
    }
    return (xL)
}

### 3.3. Add months __________________________________________________
#' @title Add months
#' @export
add_months = function (date, n) {
    new_date = seq(date, by = paste (n, "months"), length = 2)[2]
    return (new_date)
}


## 4. LOADING ________________________________________________________
### 4.1. Shapefile loading ___________________________________________
#' @title Shapefiles loading
#' @description  Generates a list of shapefiles to draw a hydrological
#' map of the France
#' @param resources_path Path to the resources directory.
#' @param fr_shpdir Directory you want to use in ash\\resources_path\\
#' to get the France shapefile.
#' @param fr_shpname Name of the France shapefile.
#' @param bs_shpdir Directory you want to use in ash\\resources_path\\
#' to get the hydrological basin shapefile.
#' @param bs_shpname Name of the hydrological basin shapefile.
#' @param sbs_shpdir Directory you want to use in
#' ash\\resources_path\\ to get the hydrological sub-basin shapefile.
#' @param sbs_shpname Name of the hydrological sub-basin shapefile.
#' @param rv_shpdir Directory you want to use in ash\\resources_path\\
#' to get the hydrological network shapefile.
#' @param rv_shpname  Name of the hydrological network shapefile.
#' @param show_river Boolean to indicate if the shapefile of the
#' hydrological network will be charge because it is a heavy one and
#' that it slows down the entire process (default : TRUE)
#' @return A list of shapefiles converted as tibbles that can be plot
#' with 'geom_polygon' or 'geom_path'.
#' @export
load_shapefile = function (resources_path, df_meta,
                           fr_shpdir, fr_shpname,
                           bs_shpdir, bs_shpname,
                           sbs_shpdir, sbs_shpname,
                           cbs_shpdir, cbs_shpname, cbs_coord,
                           rv_shpdir, rv_shpname,
                           river_selection=c('all')) {

    Code = levels(factor(df_meta$code))
    
    # Path for shapefile
    fr_shppath = file.path(resources_path, fr_shpdir, fr_shpname)
    bs_shppath = file.path(resources_path, bs_shpdir, bs_shpname)
    sbs_shppath = file.path(resources_path, sbs_shpdir, sbs_shpname)
    cbs_shppath = file.path(resources_path, cbs_shpdir, cbs_shpname)
    rv_shppath = file.path(resources_path, rv_shpdir, rv_shpname)
    
    # France
    fr_spdf = readOGR(dsn=fr_shppath, verbose=FALSE)    
    proj4string(fr_spdf) = CRS("+proj=longlat +ellps=WGS84")
    # Transformation in Lambert93
    france = spTransform(fr_spdf, CRS("+init=epsg:2154"))
    df_france = tibble(fortify(france))

    # Hydrological basin
    basin = readOGR(dsn=bs_shppath, verbose=FALSE)
    df_basin = tibble(fortify(basin))

    # Hydrological sub-basin
    subBasin = readOGR(dsn=sbs_shppath, verbose=FALSE)
    df_subBasin = tibble(fortify(subBasin))

    df_codeBasin = tibble()
    CodeOk = c()
    nShp = length(cbs_shppath)
    # Hydrological stations basins
    for (i in 1:nShp) {
        codeBasin = readOGR(dsn=cbs_shppath[i], verbose=FALSE)
        shpCode = as.character(codeBasin@data$Code)
        df_tmp = tibble(fortify(codeBasin))
        groupSample = rle(as.character(df_tmp$group))$values
        df_tmp$code = shpCode[match(df_tmp$group, groupSample)]
        df_tmp = df_tmp[df_tmp$code %in% Code &
                        !(df_tmp$code %in% CodeOk),]
        CodeOk = c(CodeOk, shpCode[!(shpCode %in% CodeOk)])

        if (cbs_coord[i] == "L2") {
            crs_rgf93 = st_crs(2154)
            crs_l2 = st_crs(27572)
            sf_loca = st_as_sf(df_tmp[c("long", "lat")],
                               coords=c("long", "lat"))
            st_crs(sf_loca) = crs_l2
            sf_loca = st_transform(sf_loca, crs_rgf93)
            sf_loca = st_coordinates(sf_loca$geometry)
            df_tmp$long = sf_loca[, 1]
            df_tmp$lat = sf_loca[, 2]
        }
        df_codeBasin = bind_rows(df_codeBasin, df_tmp)
    }
    df_codeBasin = df_codeBasin[order(df_codeBasin$code),]
    
    # If the river shapefile needs to be load
    if (!("none" %in% river_selection)) {
        # Hydrographic network
        river = readOGR(dsn=rv_shppath, verbose=FALSE) ### trop long ###
        if ('all' %in% river_selection) {
            river = river[river$Classe == 1,]
        } else {
            river = river[grepl(paste(river_selection, collapse='|'),
                                river$NomEntiteH),]
        }
        df_river = tibble(fortify(river))
    } else {
        df_river = NULL   
    }
    return (list(france=df_france,
                 basin=df_basin,
                 subBasin=df_subBasin,
                 codeBasin=df_codeBasin,
                 river=df_river))
}

### 4.2. Logo loading ________________________________________________
#' @title Logo loading
#' @export
load_logo = function (resources_path, logo_dir, PRlogo_file, AEAGlogo_file,
                      INRAElogo_file, FRlogo_file, logo_to_show) {

    logo_path = c()
    if ('PR' %in% logo_to_show) {
        path = file.path(resources_path, logo_dir, PRlogo_file)
        logo_path = c(logo_path, path)
        names(logo_path)[length(logo_path)] = 'PR'
    }
    if ('FR' %in% logo_to_show) {
        path = file.path(resources_path, logo_dir, FRlogo_file)
        logo_path = c(logo_path, path)
        names(logo_path)[length(logo_path)] = 'FR'
    }
    if ('INRAE' %in% logo_to_show) {
        path = file.path(resources_path, logo_dir, INRAElogo_file)
        logo_path = c(logo_path, path)
        names(logo_path)[length(logo_path)] = 'INRAE'
    }
    if ('AEAG' %in% logo_to_show) {
        path = file.path(resources_path, logo_dir, AEAGlogo_file)
        logo_path = c(logo_path, path)
        names(logo_path)[length(logo_path)] = 'AEAG'
    }
    
    return (logo_path)
}



    
