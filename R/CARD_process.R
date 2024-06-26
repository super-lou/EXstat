# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#           2023 Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1,
#                Nathan Pellerin (nathan.pellerin@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of EXstat R package.
#
# EXstat R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# EXstat R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EXstat R package.
# If not, see <https://www.gnu.org/licenses/>.




reduce_process = function (data, id, Process,
                           period_default=NULL,
                           suffix=NULL,
                           cancel_lim=FALSE,
                           expand_overwrite=NULL,
                           sampling_period_overwrite=NULL,
                           rm_duplicates=FALSE,
                           dev=FALSE,
                           verbose=FALSE) {

    if (verbose) {
        print(paste0("Process ", id, "/", length(Process)-1))
    }
    
    process = Process[[paste0("P", id)]]
    process_names = names(process)
    for (pp in 1:length(process)) {
        assign(process_names[pp], process[[pp]])
    }

    rm ("process")
    gc()
    
    if (!is.null(expand_overwrite) & id == (length(Process)-1)) {
        expand = expand_overwrite
    }
    
    if (is.null(sampling_period_overwrite)) {
        if (is.function(sampling_period[[1]])) {
            sampling_period = dplyr::tibble(sp=list(sampling_period[[1]]),
                                         args=sampling_period[2])
        }
    } else {
        sampling_period = sampling_period_overwrite
    }

    if (is.null(period)) {
        period = period_default
    }

    if (cancel_lim) {
        NApct_lim = NULL
        NAyear_lim = NULL
    }

    # EXtraction
    data = process_extraction(data=data,
                              funct=funct,
                              funct_args=funct_args,
                              time_step=time_step,
                              sampling_period=sampling_period,
                              period=period,
                              is_date=is_date,
                              NApct_lim=NApct_lim,
                              NAyear_lim=NAyear_lim,
                              Seasons=Seasons,
                              nameEX=nameEX,
                              suffix=suffix,
                              keep=keep,
                              compress=compress,
                              expand=expand,
                              rmNApct=rmNApct,
                              rm_duplicates=rm_duplicates,
                              dev=dev,
                              verbose=verbose)
    return (data)
}




get_last_Process = function (Process) {
    nProcess = length(Process) - 1
    for (i in 1:nProcess) {
        process = Process[[paste0("P", i)]]
        process_names = names(process)
        for (pp in 1:length(process)) {
            assign(process_names[pp], process[[pp]])
        }
    }
    res = list(compress=compress, time_step=time_step, Seasons=Seasons)
    return (res)
}



#' @title CARD_extraction
#' @description Extract specified data from a set of script files.
#'
#' @param data Tibble used in the extraction
#' @param CARD_path Directory path where CARD_dir is located
#' @param CARD_dir Subdirectory name where the script files are located (default = "WIP")
#' @param CARD_name Name of the script files to be used (optional)
#' @param CARD_tmp Temporary directory path (optional)
#' @param period_default Period to extract from the data (optional)
#' @param suffix Suffix to append to extracted variables (optional)
#' @param cancel_lim Specify whether to cancel limits (default = FALSE)
#' @param simplify Specify whether to simplify the extracted data by column name (default = FALSE)
#' @param expand_overwrite Overwrite the expand parameter (optional)
#' @param sampling_period_overwrite Overwrite the sampling_period parameter (optional)
#' @param verbose Specify whether to print out the process details (default = FALSE)
#'
#' @return A list of extracted data, along with meta data.
#'
#' @details The function reads the script files from the specified directory and extracts data based on the parameters provided.
#'
#' @note documentation generated by chatGPT
#'
#' @import purrr
#' @import dplyr
#' @export
#' @keywords data extraction CARD
CARD_extraction = function (data, CARD_path, CARD_dir="WIP",
                            CARD_name=NULL, CARD_tmp=NULL,
                            period_default=NULL,
                            suffix=NULL, 
                            cancel_lim=FALSE,
                            simplify=FALSE,
                            expand_overwrite=NULL,
                            sampling_period_overwrite=NULL,
                            rm_duplicates=FALSE,
                            dev=FALSE,
                            verbose=FALSE) {
    
    if (is.null(CARD_tmp)) {
        CARD_tmp = CARD_path
    }
    
    CARD_dirpath = file.path(CARD_tmp, CARD_dir)   
    script_to_analyse = list.files(CARD_dirpath,
                                   pattern=".R$",
                                   recursive=TRUE,
                                   include.dirs=FALSE,
                                   full.names=FALSE)

    if (!is.null(CARD_name)) {
        script_to_analyse =
            script_to_analyse[gsub("^[[:digit:]]+[_]", "",
                                   basename(script_to_analyse)) %in%
                              paste0(CARD_name, ".R")]
    }
    
    script_to_analyse = script_to_analyse[!grepl("__default__.R",
                                                 script_to_analyse)]

    topic_to_analyse = list.dirs(CARD_dirpath,
                                 recursive=TRUE, full.names=FALSE)
    topic_to_analyse = topic_to_analyse[topic_to_analyse != ""]
    topic_to_analyse = gsub('.*_', '', topic_to_analyse)

    structure = replicate(length(topic_to_analyse), c())
    names(structure) = topic_to_analyse

    variable_analyse = c()

    nScript = length(script_to_analyse)
    metaEX = dplyr::tibble()
    dataEX = replicate(nScript, list(NULL))

    for (ss in 1:nScript) {

        script = script_to_analyse[ss]

        list_path = list.files(file.path(CARD_path,
                                         "__tools__"),
                               pattern='*.R$',
                               recursive=TRUE,
                               full.names=TRUE)
        for (path in list_path) {
            source(path, encoding='UTF-8')    
        }

        Process_default = sourceProcess(
            file.path(CARD_path, "__default__.R"))
        
        Process = sourceProcess(
            file.path(CARD_dirpath, script),
            default=Process_default)

        principal = Process$P
        principal_names = names(principal)
        for (pp in 1:length(principal)) {
            assign(principal_names[pp], principal[[pp]])
        }

        variable = variable_en
        split_script = split_path(script)
        
        if (length(split_script) == 1) {
            if (!('None' %in% names(structure))) {
                structure = append(list(None=c()), structure)
            }
            structure[['None']] = c(structure[['None']], variable)
        } else if (length(split_script) == 2) {
            dir = split_script[2]
            dir = gsub('.*_', '', dir)
            structure[[dir]] = c(structure[[dir]], variable)
        }

        if (any(variable %in% variable_analyse)) {
            next
        }
        
        variable_analyse = c(variable_analyse, variable)

        if (verbose) {
            print(paste0('Computes ', Process$P$variable_en))
        }

        nProcess = length(Process) - 1
        
        dataEX[[ss]] =
            purrr::reduce(1:nProcess,
                          reduce_process,
                          Process=Process,
                          period_default=period_default,
                          suffix=suffix,
                          cancel_lim=cancel_lim,
                          expand_overwrite=expand_overwrite,
                          sampling_period_overwrite=sampling_period_overwrite[[ss]],
                          rm_duplicates=rm_duplicates,
                          dev=dev,
                          verbose=verbose,
                          .init=data)
        
        
        if (tibble::is_tibble(dataEX[[ss]])) {
            dataEX[[ss]] = list(dataEX[[ss]])
            if (!simplify) {
                variable = paste0(variable, collapse=" ")
                # glose = paste0(glose, collapse=" ")
                names(dataEX[[ss]]) = variable
            }
        }

        res = get_last_Process(Process)
        rm ("Process")
        gc()

        compress = res$compress
        time_step = res$time_step
        Seasons = res$Seasons

        if (!is.null(sampling_period_overwrite[[ss]])) {
            sampling_period_en = sampling_period_overwrite[[ss]]
            sampling_period_fr = sapply(
                lapply(strsplit(sampling_period_en, "-"), rev),
                paste0, collapse="-")
            sampling_period_en = paste0(sampling_period_en, collapse=", ")
            sampling_period_fr = paste0(sampling_period_fr, collapse=", ")
        }
        
        metaEX =
            dplyr::bind_rows(
                       metaEX,
                       dplyr::tibble(
                                  ### English ___
                                  variable_en=variable_en,
                                  unit_en=unit_en,
                                  name_en=name_en,
                                  description_en=description_en,
                                  method_en=method_en,
                                  sampling_period_en=sampling_period_en,
                                  topic_en=topic_en,
                                  ### French ___
                                  variable_fr=variable_fr,
                                  unit_fr=unit_fr,
                                  name_fr=name_fr,
                                  description_fr=description_fr,
                                  method_fr=method_fr,
                                  sampling_period_fr=sampling_period_fr,
                                  topic_fr=topic_fr,
                                  ### Global ___
                                  source=source,
                                  is_date=is_date, 
                                  to_normalise=to_normalise,
                                  palette=palette))
    }
    rm ("data")
    gc()

    dataEX = unlist(dataEX, recursive=FALSE)
    
    if (simplify) {
        by = names(dplyr::select(dataEX[[1]],
                                 dplyr::where(is.character)))
        dataEX = purrr::reduce(.x=dataEX,
                               .f=dplyr::full_join,
                               by=by)
    }

    return (list(metaEX=metaEX, dataEX=dataEX))
}


#' @title sourceProcess
#' @description Source and process a script file.
#'
#' @param path Path to the script file
#' @param default Default process object to use (optional)
#'
#' @return A list of process objects extracted from the script file.
#'
#' @details The function sources the script file from the specified path and extracts the process objects defined within the file. It returns a list of process objects, where each object represents a different process defined in the script file.
#'
#' @importFrom stringr str_extract
#'
#' @export
sourceProcess = function (path, default=NULL) {
    assign("CARD", new.env(), envir=.GlobalEnv)
    source(path, encoding='UTF-8')
    lsCARD = ls(envir=CARD)

    Process_def = lsCARD[grepl("P[.]", lsCARD)]
    Process = lapply(Process_def, get, envir=CARD)
    names(Process) = gsub("P[.]", "", Process_def)
    Process = list(Process)
    names(Process) = "P"
    
    if (!is.null(default)) {
        nOK = !(names(default$P) %in% names(Process$P))
        Process$P = append(Process$P, default$P[nOK])
    }
    
    process_allAtt = lsCARD[grepl("P[[:digit:]][.]", lsCARD)]
    process_allNames = stringr::str_extract(process_allAtt,
                                            "P[[:digit:]]")
    process_names = process_allNames[!duplicated(process_allNames)]
    Nprocess = length(process_names)

    for (i in 1:Nprocess) {
        process_name = paste0("P", i)
        IDprocess = grepl(paste0(process_name, "[.]"),
                          process_allAtt)

        process_att = process_allAtt[IDprocess]
        process = lapply(process_att, get, envir=CARD)
        
        names(process) = gsub("P[[:digit:]][.]", "",
                              process_att)
        process = list(process)
        names(process) = process_name

        if (!is.null(default)) {
            nOK = !(names(default$P1) %in%
                    names(process[[process_name]]))
            process[[process_name]] =
                append(process[[process_name]], default$P1[nOK])
        }
        Process = append(Process, process)
    }

    rm (list=ls(envir=CARD), envir=CARD)
    return (Process)
}
