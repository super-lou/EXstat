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
# R/advanced_settings.R
#
# Configuration file that regroups different advanced settings about
# file structure, analyses and plotting.


## 1. FILE STRUCTURE _________________________________________________
### 1.1. Input directories ___________________________________________
# Path to the data
computer_data_path = file.path(computer_work_path, 'data')

# Resources directory
resources_path = file.path(computer_work_path, 'resources')
if (!(file.exists(resources_path))) {
  dir.create(resources_path)
}
print(paste('resources_path :', resources_path))

# Logo filename
logo_dir = 'logo'
PRlogo_file = 'logo_Prefet_bassin.png'
AEAGlogo_file = 'agence-de-leau-adour-garonne_logo.png'
INRAElogo_file = 'Logo-INRAE_Transparent.png'
FRlogo_file = 'Republique_Francaise_RVB.png'

shp_dir = 'map'
# Path to the shapefile for france contour from 'computer_data_path' 
fr_shpdir = file.path(shp_dir, 'france')
fr_shpname = 'gadm36_FRA_0.shp'

# Path to the shapefile for basin shape from 'computer_data_path' 
bs_shpdir = file.path(shp_dir, 'bassin')
bs_shpname = 'BassinHydrographique.shp'

# Path to the shapefile for sub-basin shape from 'computer_data_path' 
sbs_shpdir = file.path(shp_dir, 'sous_bassin')
sbs_shpname = 'SousBassinHydrographique.shp'

# Path to the shapefile for station basins shape from 'computer_data_path' 
cbs_shpdir = file.path(shp_dir, 'bassin_station')
cbs_shpname = c('BV_4207_stations.shp', '3BVs_FRANCE_L2E_2018.shp')
cbs_coord = c("L93", "L2")

# Path to the shapefile for river shape from 'computer_data_path' 
rv_shpdir = file.path('map', 'river')
rv_shpname = 'CoursEau_FXX.shp'

### 1.2. Output directories __________________________________________
# Result directory
resdir = file.path(computer_work_path, 'results')
if (!(file.exists(resdir))) {
  dir.create(resdir)
}
print(paste('resdir :', resdir))

# Figure directory
figdir = file.path(computer_work_path, 'figures')
if (!(file.exists(figdir))) {
  dir.create(figdir)
}
print(paste('figdir :', figdir))


modified_data_dir = 'modified_data'
trend_dir = 'trend_analyses'


## 2. STATION SELECTION BY LIST ______________________________________
### 2.1. Selection with '.docx' file _________________________________
# Path to a '.docx' list file of station that will be analysed
DOCXlistdir = 
    ""

DOCXlistname = 
    ""
    # "Liste-station_RRSE.docx" 

### 2.2. Selection with '.txt' file _________________________________
# Path to the '.txt' list file of station that will be analysed
# It can be generated with :
# create_selection(computer_data_path, 'dirname', 'selection.txt')
TXTlistdir =
    ""

TXTlistname = 
    ""
    # "selection.txt"


## 3. ANALYSIS PARAMETERS ____________________________________________
init_var_file = 'default.R'
var_dir = 'variable'
var_to_analyse_dir =
    # ''
    # 'AEAG_selection'
    'shiny'
    # 'wip'

to_assign_out = c(
    # 'modified_data',
    # 'analyse'
)

hydroYear_mode =
    # 'every'
    'fixed'


### 3.2. Climate analysis ____________________________________________
to_analyse_climate = c(
    'PA',
    'TA',
    'ETPA'
)

### 3.3. Data modification ___________________________________________
# Local corrections of the data
df_flag = data.frame(
    code=c('O3141010',
           'O7635010',
           'O7635010',
           'O7635010',
           'O7635010'
           ),
    Date=c('1974-07-04',
           '1948-09-06',
           '1949-02-08',
           '1950-07-20',
           '1953-07-22'
           ),
    newValue=c(9.5,
               4,
               3,
               1,
               3) # /!\ Unit
)


### 3.4. Saving option _______________________________________________
saving = c(
    # 'meta',
    # 'modified_data',
    # 'analyse'
)

fast_format = TRUE

read_results = TRUE


### 3.5. Statistical option __________________________________________
# The risk of the Mann-Kendall trend detection test
alpha = 0.1


## 4. PLOTTING PARAMETERS ____________________________________________
# If the hydrological network needs to be plot
river_selection =
    "none"
    # "all"
    # c(
        # "La Seine$",
        # "'Yonne$",
        # "La Marne$",
        # "La Meuse",
        # "La Moselle$",
        # "^La Loire$", "^la Loire$",
        # "^le cher$",
        # "^La Creuse$", "^la Creuse$",
        # "^La Vienne$", "^la Vienne$",
        # "La Garonne$",
        # "Le Tarn$",
        # "Le Rhône$",
        # "La Saône$"
    # )

# Graphical selection of period
axis_xlim =
    NULL
# c("1982-01-01", "1983-01-01")

# What you want to be plotted for station analyses. For example if 'datasheet' is wrote, datasheet about each stations will be drawn.
# All the option are :
#    'datasheet' : datasheet of trend analyses for each stations
#        'table' : summarizing table about trend analyses
#          'map' : map about trend analyses
to_plot_station =
    c(
        'datasheet'
        # 'table'
        # 'map'
        # 'map_regime'
        # 'map_trend'
        # 'map_mean'
    )

logo_to_show =
    c(
        # 'PR',
        'FR',
        'INRAE'
        # 'AEAG'
    )

zone_to_show =
    'France'
    # 'Adour-Garonne'

pdf_chunk =
    c(
        'all',
        'by_code'
    )

show_colorEvent = TRUE
