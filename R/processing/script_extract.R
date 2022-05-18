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
# R/processing/script_extract.R
#
# Script that manages the call to the right process in order to
# realise extraction of data.


# Sourcing R files
source(file.path('R', 'processing', 'extract.R'),
       encoding='UTF-8')
source(file.path('R', 'processing', 'format.R'),
       encoding='UTF-8')
source(file.path('R', 'processing', 'analyse.R'),
       encoding='UTF-8')

## 1. EXTRACTION OF HYDROMETRIC STATIONS _____________________________
if ('station_extraction' %in% to_do) {
    # Initialization of null dataframes if there is no data selected
    df_data_DOCX = NULL
    df_data_TXT = NULL
    df_data_MAN = NULL
    df_meta_DOCX = NULL
    df_meta_TXT = NULL
    df_meta_MAN = NULL

### 1.1. Selection of station from a formatted '.docx' file __________
    if (DOCXlistname != "") {
        # Get only the selected station from a list station file
        filename_DOCX = get_selection_DOCX(computer_data_path,
                                           DOCXlistdir,
                                           DOCXlistname,
                                           code_nameCol='code',
                                           choice_nameCol='Choix',
                                           choice_Val=c('A garder',
                                                        'Ajout'), 
                                           optname='_HYDRO_QJM')
        
        # Extract metadata about selected stations
        df_meta_DOCX = extract_meta(computer_data_path, filedir,
                                    filename_DOCX)
        # Extract data about selected stations
        df_data_DOCX = extract_data(computer_data_path, filedir,
                                    filename_DOCX)
    }

### 1.2. Selection from a formatted '.txt' file ______________________
    if (TXTlistname != ""){
        # Get only the selected station from a list station file
        filename_TXT = get_selection_TXT(computer_data_path, 
                                         TXTlistdir,
                                         TXTlistname)

        # Extract metadata about selected stations
        df_meta_TXT = extract_meta(computer_data_path, filedir,
                                   filename_TXT)
        # Extract data about selected stations
        df_data_TXT = extract_data(computer_data_path, filedir,
                                   filename_TXT)
    } 

### 1.3. Manual selection ____________________________________________
    if (all(filename != "")) {
        filename = convert_regexp(computer_data_path, filedir, filename)
        # Extract metadata about selected stations
        df_meta_MAN = extract_meta(computer_data_path, filedir, filename)
        # Extract data about selected stations
        df_data_MAN = extract_data(computer_data_path, filedir, filename)
    }

### 1.4. Joining of data _____________________________________________
    df_join = join_selection(list_data=list(df_data_DOCX,
                                            df_data_TXT,
                                            df_data_MAN),
                             list_meta=list(df_meta_DOCX,
                                            df_meta_TXT,
                                            df_meta_MAN),
                             list_from=list('docx', 'txt', 'manual'))
    df_data = df_join$data
    df_meta = df_join$meta

    # Get all different stations code
    Code = levels(factor(df_meta$code))
    
### 1.5. Add other info about stations _______________________________
    # Time gap
    df_meta = get_lacune(df_data, df_meta)
    # Hydrograph

    if (!is.null(mean_period[[1]])) {
        period = mean_period[[1]]
    } else {
       period = trend_period[[1]] 
    }
    df_meta = get_hydrograph(df_data, df_meta,
                             period=period)$meta
}


## 2. EXTRACTION OF CLIMATE DATA______________________________________
if ('climate_extraction' %in% to_do) {
    res = extract_climate_data(computer_data_path, 'climate',
                              colNames=c('Date', 'PRCP_mm',
                                         'PET_mm', 'T_degC'))
    df_climate_data = res$data
    df_climate_meta = res$meta
}
