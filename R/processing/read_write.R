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
# R/processing/results_manager.R
#
# Manages the writing and reading of results data of the trend analysis.


## 1. WRITING ________________________________________________________
### 1.1. List of dataframe ___________________________________________
write_analyse = function (Ldf, resdir, filedir) {
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }

    print(paste('Writing of list of dataframe in : ', outdir, sep=''))
    
    Lname = names(Ldf)
    Nname = length(Lname)
    for (i in 1:Nname) {
        outfile = paste(Lname[i], '.txt', sep='')
        
        write.table(Ldf[[i]],
                    file=file.path(outdir, outfile),
                    sep=";",
                    quote=TRUE,
                    row.names=FALSE)
    }
}

### 1.2. Dataframe of modified data __________________________________
write_data = function (df_data, df_mod, resdir, filedir) {

    Code = rle(sort(df_mod$code))$values
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }

    print(paste('Writing of modified data in : ', outdir, sep=''))

    for (code in Code) {
        df_data_code = df_data[df_data$code == code,]
        df_mod_code = df_mod[df_mod$code == code,]
    
        outfile1 = paste(code, '.txt', sep='')
        write.table(df_data_code,
                    file=file.path(outdir, outfile1),
                    sep=";",
                    quote=TRUE,
                    row.names=FALSE)

        outfile2 = paste(code, '_modification', '.txt', sep='')
        write.table(df_mod_code,
                    file=file.path(outdir, outfile2),
                    sep=";",
                    quote=TRUE,
                    row.names=FALSE)
    }
}

### 1.3. Dataframe of criticism ______________________________________
write_critique = function (df_critique, resdir, filename='critique') {

    outdir = file.path(resdir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }

    print(paste('Writing criticism in : ', outdir, sep=''))

    outfile = paste(filename, '.txt', sep='')
    write.table(df_critique,
                file=file.path(outdir, outfile),
                sep=";",
                quote=FALSE,
                row.names=FALSE)   
}
# write_critique(df_critique, resdir)

### 1.4. Fast for R __________________________________________________
write_dataFST = function (df_data, resdir, filedir='fst',
                          filename='data.fst') {
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }
    outfile = file.path(outdir, filename)
    fst::write_fst(df_data, outfile, compress=0)
}

write_metaFST = function (df_meta, resdir, filedir='fst',
                          filename='meta.fst') {
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }
    outfile = file.path(outdir, filename)
    fst::write_fst(df_meta, outfile, compress=0)
}


## 2. READING ________________________________________________________
### 2.1. List of dataframe ___________________________________________
read_listofdf = function (resdir, filedir) {

    outdir = file.path(resdir, filedir)
    files = list.files(outdir)
    Nfile = length(files)

    print(paste('Reading of list of dataframe in : ', outdir, sep=''))

    Ldf = list()
    for (i in 1:Nfile) {
        name = splitext(files[i])$name
        
        df =  as_tibble(read.table(file=file.path(outdir, files[i]),
                                   header=TRUE,
                                   sep=";",
                                   quote='"'))

        for (j in 1:ncol(df)) {
            if (is.factor(df[[j]])) {
                d = try(as.Date(df[[1, j]], format="%Y-%m-%d"))
                if("try-error" %in% class(d) || is.na(d)) {
                    df[j] = as.character(df[[j]])
                } else {
                    df[j] = as.Date(df[[j]])
                }
            }
        }
        # OkFact = sapply(df, is.factor)
        # df[OkFact] = lapply(df[OkFact], as.character)
        
        Ldf = append(Ldf, list(df))
        names(Ldf)[length(Ldf)] = name
    }
    return (Ldf)
}

### 2.2. Dataframe of modified data __________________________________
read_data = function (resdir, filedir, filename, verbose=TRUE) {

    # Convert the filename in vector
    filename = c(filename)

    # If the filename is 'all' or regroup more than one filename
    if (all(filename == 'all') | length(filename) > 1) {
        # If the filename is 'all'
        if (all(filename == 'all')) {
            # Create a filelist to store all the filename
            filelist = c()
             # Get all the filename in the data directory selected
            filelist_tmp = list.files(file.path(resdir,
                                                filedir))

            # For all the filename in the directory selected
            for (f in filelist_tmp) {
                # If the filename extention is 'txt'
                if (file_ext(f) == 'txt') {
                    # Store the filename in the filelist
                    filelist = c(filelist, f) 
                }
            }
        # If the filename regroup more than one filename
        } else if (length(filename > 1)) {
            # The filelist correspond to the filename
            filelist = filename
        } 

        # Create a blank data frame
        df_data = data.frame()

        # For all the file in the filelist
        for (f in filelist) {
            # Concatenate by raw data frames created by this function
            # when filename correspond to only one filename
            df_data = rbind(df_data,
                            read_data(resdir, 
                                      filedir, 
                                      f))
        }
        # Set the rownames by default (to avoid strange numbering)
        rownames(df_data) = NULL
        return (df_data)
    }
    
    # Get the filename from the vector
    filename = filename[1]

    # Print metadata if asked
    if (verbose) {
        print(paste("reading of extracted data for file :", filename))
    }

    # Get the file path to the data
    file_path = file.path(resdir, filedir, filename)
    
    if (file.exists(file_path) & substr(file_path, nchar(file_path),
                                        nchar(file_path)) != '/') {
        # Extract the data as a data frame
        df_data = as_tibble(read.table(file_path,
                                       header=TRUE,
                                       na.strings=c('NA'),
                                       sep=';',
                                       quote='"',
                                       skip=0))

        for (j in 1:ncol(df_data)) {
            if (is.factor(df_data[[j]])) {
                d = try(as.Date(df_data[[1, j]], format="%Y-%m-%d"))
                if("try-error" %in% class(d) || is.na(d)) {
                    df_data[j] = as.character(df_data[[j]])
                } else {
                    df_data[j] = as.Date(df_data[[j]])
                }
            }
        }
        
        return (df_data)

    } else {
        print(paste('filename', file_path, 'do not exist'))
        return (NULL)
    }
}

### 2.3. Dataframe of criticism ______________________________________
read_critique = function (resdir, filename='critique') {

    outdir = file.path(resdir)
    outfile = paste(filename, '.txt', sep='')
    file_path = file.path(outdir, outfile)
    
    print(paste('Reading criticism in : ', file_path, sep=''))
    
    df =  as_tibble(read.table(file=file_path,
                               header=TRUE,
                               sep=";"))
    
    for (j in 1:ncol(df)) {
        if (is.factor(df[[j]])) {
            d = try(as.Date(df[[1, j]], format="%Y-%m-%d"))
            if("try-error" %in% class(d) || is.na(d)) {
                df[j] = as.character(df[[j]])
            } else {
                df[j] = as.Date(df[[j]])
            }
        }
    }

    return (df)
}
# df_critique = read_critique(resdir)

### 2.4. Fast for R __________________________________________________
read_FST = function (resdir, filename, filedir='fst') {
    outfile = file.path(resdir, filedir, filename)
    df = tibble(fst::read_fst(outfile))
    return (df)
}


## 3. OTHER __________________________________________________________
splitext = function(file) { 
    ex = strsplit(basename(file), split="\\.")[[1]]
    res = list(name=ex[1], extension=ex[2])
    return (res)
} 
