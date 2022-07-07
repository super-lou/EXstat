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
# script_install.R
#
# Script that regroups all command lines potentially needed to make
# functional this toolbox.


#  _   _                  ___              __  _       
# | | | | ___ ___  _ _   / __| ___  _ _   / _|(_) __ _ 
# | |_| |(_-</ -_)| '_| | (__ / _ \| ' \ |  _|| |/ _` |
#  \___/ /__/\___||_|    \___|\___/|_||_||_|  |_|\__, | ______________
## You can modify this part without risk ##      |___/ 
# Choose your OS before execute all the script or line by line. This
# toolbox is not tested for OSX user (but normally windows
# installation should be ok) ...
OS =
    'windows'
    # 'linux'


#  ___               ___              __  _       
# |   \  ___ __ __  / __| ___  _ _   / _|(_) __ _ 
# | |) |/ -_)\ V / | (__ / _ \| ' \ |  _|| |/ _` |
# |___/ \___| \_/   \___|\___/|_||_||_|  |_|\__, | ___________________
## /!\ Do not touch if you are not aware ## |___/

## 1. OS COMMON INSTALLATION PACKAGES ________________________________
### 1.1. On CRAN _____________________________________________________
install.packages("devtools")
install.packages("dplyr")
install.packages("officer")
install.packages("lubridate")
install.packages('zoo')
install.packages('tools')
install.packages("gridtext")
install.packages("lubridate")
install.packages('trend')
install.packages("Hmisc")
install.packages("ggplot2")
install.packages('scales')
install.packages('ggh4x')
install.packages("RColorBrewer")
install.packages("png")
install.packages("shadowtext")
install.packages("qpdf")
install.packages("gridExtra")
install.packages("CircStats")
install.packages('latex2exp')
install.packages("accelerometry")

### 1.2. On git ______________________________________________________
library(devtools)
print("")
print('For the following packages you need to press the right key in order to install all.')
print("")
install_github("https://github.com/benRenard/BFunk") 
install_github("https://github.com/vmansanarez/AoTre.git")


# ## 2. OS NOT COMMON INSTALLATION PACKAGES ____________________________
if (OS == 'linux') {
    print('You firstly need to install manually some other packages with :')
    print('sudo apt-get -y update && sudo apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev')
    print('and')
    print('sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev')
    
    if (askYesNo('... is it ok ?')) {
        install.packages("sf")
        install.packages("rgdal")
    }
} else {
    install.packages("sf")
    install.packages("rgdal")
}
