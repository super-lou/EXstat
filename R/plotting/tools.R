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
# Rcode/plotting/tools.R


## 1. COLOR MANAGEMENT
### 1.1. Color on colorbar ___________________________________________
# Returns a color of a palette corresponding to a value included
# between the min and the max of the variable
get_color = function (value, min, max, ncolor=256, palette_name='perso', reverse=FALSE) {

    
    # If the value is a NA return NA color
    if (is.na(value)) {
        return (NA)
    }
    
    # If the palette chosen is the personal ones
    if (palette_name == 'perso') {
        colorList = palette_perso
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
get_palette = function (min, max, ncolor=256, palette_name='perso', reverse=FALSE, nbTick=10) {

    # If the value is a NA return NA color
    if (is.null(min) | is.null(max)) {
        return (NA)
    }
    
    # If the palette chosen is the personal ones
    if (palette_name == 'perso') {
        colorList = palette_perso
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
palette_tester = function (palette_name='perso', figdir='figures', n=256) {

    outdir = file.path(figdir, 'palette')
    if (!(file.exists(outdir))) {
        dir.create(outdir)
    }
    
    # If the palette chosen is the personal ones
    if (palette_name == 'perso') {
        colorList = palette_perso
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
gg_circle = function(r, xc, yc, color="black", fill=NA, ...) {
    x = xc + r*cos(seq(0, pi, length.out=100))
    ymax = yc + r*sin(seq(0, pi, length.out=100))
    ymin = yc + r*sin(seq(0, -pi, length.out=100))
    annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color,
             fill=fill, ...)
}


## 3. NUMBER MANAGEMENT ______________________________________________
### 3.1. Number formatting ___________________________________________
# Returns the power of ten of the scientific expression of a value
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
add_months = function (date, n) {
    new_date = seq(date, by = paste (n, "months"), length = 2)[2]
    return (new_date)
}
