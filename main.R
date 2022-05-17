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
# main.R
#
# Main script that regroups all command lines needed to interact with
# this toolbox. Choose your parameters before executing all the script
# (RStudio : Ctrl+Alt+R) or line by line.


#  ___         __                         _    _                
# |_ _| _ _   / _| ___  _ _  _ __   __ _ | |_ (_) ___  _ _   ___
#  | | | ' \ |  _|/ _ \| '_|| '  \ / _` ||  _|| |/ _ \| ' \ (_-<
# |___||_||_||_|  \___/|_|  |_|_|_|\__,_| \__||_|\___/|_||_|/__/ _____
#
# If you want to contact the author of the code you need to contact
# first Louis Héraut who is the main developer. If it is not possible,
# Éric Sauquet is the main referent at INRAE to contact.
#
# Louis Héraut : <louis.heraut@inrae.fr>
# Éric Sauquet : <eric.sauquet@inrae.fr>
#
# The statistical tools used in this code come from the
# StatsAnalysisTrend package developed by Valentin Mansanarez.
#
# See the 'README.txt' file for more information about the utilisation
# of this toolbox.


#  _   _                  ___              __  _       
# | | | | ___ ___  _ _   / __| ___  _ _   / _|(_) __ _ 
# | |_| |(_-</ -_)| '_| | (__ / _ \| ' \ |  _|| |/ _` |
#  \___/ /__/\___||_|    \___|\___/|_||_||_|  |_|\__, | ______________
## You can modify this part without risk ##      |___/ 

## 1. WORKING DIRECTORY ______________________________________________
# Work path (it normally needs to end with '\\ash' directory)
computer_work_path = 
    "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/ash"
    # "C:\\Users\\louis.heraut\\Documents\\CDD_stationnarite\\ash"

## 2. DATA DIRECTORY _________________________________________________
# Directory of Banque HYDRO data you want to use in ash\\data\\ to
# extract stations flow data. If "" is use, data will be search in
# ash\\data\\.
filedir =
    # ""
    # "AEAG_selection"
    "RRSE"

# Name of the files that will be analysed from the data directory
# (if "all", all the file of the directory will be chosen)
filename =
    # ""
    "all"
    # c(
        # "S2235610_HYDRO_QJM.txt",
        # "O1484320_HYDRO_QJM.txt", 
        # "Q7002910_HYDRO_QJM.txt",
        # "O0362510_HYDRO_QJM.txt"
    # )

## 3. WHAT YOU WANT TO DO ____________________________________________
# This vector regroups all the different step you want to do. For example if you write 'station_extraction', the extraction of the data for the station will be done. If you add also 'station_analyse', the extraction and then the trend analyse will be done. But if you only write, for example, 'station_plot', without having previously execute the code with 'station_extraction' and 'station_analyse', it will results in a failure.
# All the option are :
#    'station_extraction' : Extraction of data and metadata dataframes
#                           about stations
#    'climate_extraction' : Extraction of data and metadata dataframes
#                           about climate data
# 'station_trend_analyse' : Trend analyses of stations data
#  'station_break_analyse' : Brief analysis of break data
# 'climate_trend_analyse' : Trend analyses of the climate data
#    'station_serie_plot' : Plotting of flow series for stations
#    'station_trend_plot' : Plotting of trend analyses of stations
#    'station_break_plot' : Plotting of the break analysis
#    'climate_trend_plot' : Plotting of trend analyses of climate data
to_do =
    c(
        # 'station_extraction',
        # 'station_trend_analyse',
        'station_trend_plot'
    )

## 4. ANALYSIS PARAMETERS ____________________________________________
# Periods of time to perform the trend analyses. More precisely :
# - periodAll tends to represent the maximal accessible period of flow
# data hence the start in 1800
# - periodSub tends to represent the period with the most accessible
# flow data
periodAll = c("1800-01-01", "2020-12-31")
periodSub =
    NULL
    # c("1968-01-01", "2020-12-31")

# Periods of time to average. More precisely :
# - periodRef tends to represent the reference period of the climate
# - periodCur tends to represent the current period
# flow data
periodRef = c("1968-01-01", "1988-12-31")
periodCur = c("2000-01-01", "2020-12-31")


## 5. ADVANCED SETTINGS ______________________________________________
# If you want to open and edit the advanced settings, puts it to TRUE
modify_advanced_settings = FALSE


#  ___               ___              __  _       
# |   \  ___ __ __  / __| ___  _ _   / _|(_) __ _ 
# | |) |/ -_)\ V / | (__ / _ \| ' \ |  _|| |/ _` |
# |___/ \___| \_/   \___|\___/|_||_||_|  |_|\__, | ___________________
## /!\ Do not touch if you are not aware ## |___/

## 0. INITIALISATION _________________________________________________
# Sets working directory
setwd(computer_work_path)

# Creates list of period for trend analysis
trend_period = list()
if (!is.null(periodAll)) {
    trend_period = append(trend_period, list(periodAll))
}
if (!is.null(periodSub)) {
    trend_period = append(trend_period, list(periodSub))
}
# Creates list of period for average analysis
mean_period = list()
if (!is.null(periodRef)) {
    mean_period = append(mean_period, list(periodRef))
}
if (!is.null(periodCur)) {
    mean_period = append(mean_period, list(periodCur))
}

# Gets the path to the advanced settings
advanced_settings_path = file.path('R', 'advanced_settings.R')
# If the user want to modify these settings
if (modify_advanced_settings) {
    # It opens it for him
    file.edit(advanced_settings_path)
}

# Sourcing the R file of advanced settings
source(advanced_settings_path, encoding='UTF-8')

## 1. EXTRACTION _____________________________________________________
if ('station_extraction' %in% to_do | 'climate_extraction' %in% to_do) {
    print('EXTRACTION')
    source(file.path('R', 'processing', 'script_extract.R'),
           encoding='UTF-8')
}

## 2. ANALYSES _______________________________________________________
if ('station_trend_analyse' %in% to_do | 'station_break_analyse' %in% to_do | 'climate_trend_analyse' %in% to_do) {
    print('ANALYSES')
    source(file.path('R', 'processing', 'script_analyse.R'),
           encoding='UTF-8')
}

## 3. PLOTTING _______________________________________________________
if ('station_serie_plot' %in% to_do | 'station_trend_plot' %in% to_do | 'station_break_plot' %in% to_do | 'climate_trend_plot' %in% to_do) {
    print('PLOTTING')
    source(file.path('R', 'plotting', 'script_layout.R'),
           encoding='UTF-8')
}
