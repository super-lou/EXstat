# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#           2023 Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
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


#' @title card_extraction
#' @export
card_extraction = function (data, card_dir, period=NULL,
                            samplePeriod_opti=NULL,
                            verbose=FALSE) {
    
    CARD_dir = gsub("CARD[/].*", "CARD/", card_dir)
    
    print(CARD_dir)
    
    script_to_analyse = list.files(card_dir,
                                   pattern=".R$",
                                   recursive=TRUE,
                                   include.dirs=FALSE,
                                   full.names=FALSE)

    script_to_analyse = script_to_analyse[!grepl("__default__.R",
                                                 script_to_analyse)]

    topic_to_analyse = list.dirs(card_dir,
                                 recursive=TRUE, full.names=FALSE)
    topic_to_analyse = topic_to_analyse[topic_to_analyse != ""]
    topic_to_analyse = gsub('.*_', '', topic_to_analyse)

    structure = replicate(length(topic_to_analyse), c())
    names(structure) = topic_to_analyse
    
    var_analyse = c()
    # topic_analyse = c()
    # unit_analyse = c()
    # samplePeriod_analyse = list()
    # glose_analyse = c()

    metaEx = dplyr::tibble()
    dataEx = list()
    
    for (script in script_to_analyse) {

        list_path = list.files(file.path(CARD_dir,
                                         "__tools__"),
                               pattern='*.R$',
                               recursive=TRUE,
                               # include.dirs=FALSE,
                               full.names=TRUE)
        for (path in list_path) {
            source(path, encoding='UTF-8')    
        }

        Process_default = sourceProcess(
            file.path(CARD_dir, "__default__.R"))
        
        Process = sourceProcess(
            file.path(card_dir, script),
            default=Process_default)

        principal = Process$P
        principal_names = names(principal)
        for (i in 1:length(principal)) {
            assign(principal_names[i], principal[[i]])
        }
        
        split_script = split_path(script)
        
        if (length(split_script) == 1) {
            if (!('None' %in% names(structure))) {
                structure = append(list(None=c()), structure)
            }
            structure[['None']] = c(structure[['None']], var)
        } else if (length(split_script) == 2) {
            dir = split_script[2]
            dir = gsub('.*_', '', dir)
            structure[[dir]] = c(structure[[dir]], var)
        }
        
        if (!is.null(samplePeriod_opti)) {
            if (identical(samplePeriod_opti[[topic[1]]],
                          "min")) {
                minQM = paste0(formatC(meta$minQM,
                                       width=2,
                                       flag="0"),
                               '-01')
                samplePeriodMOD = tibble(Code=meta$Code,
                                         sp=minQM)
            } else if (identical(samplePeriod_opti[[topic[1]]],
                                 "max")) {
                maxQM = paste0(formatC(meta$maxQM,
                                       width=2,
                                       flag="0"),
                               '-01')
                samplePeriodMOD = tibble(Code=meta$Code,
                                         sp=maxQM)
            } else {
                samplePeriodMOD = samplePeriod_opti[[topic[1]]]
            }
            
        } else {
            samplePeriodMOD = NULL
        }

        if (!is.null(samplePeriodMOD)) {
            nProcess = length(Process)
            for (i in 1:nProcess) {
                if (!is.null(Process[[i]]$samplePeriod)) {
                    Process[[i]]$samplePeriod = samplePeriodMOD
                    samplePeriod = Process[[i]]$samplePeriod
                }
            }
        }

        if (var %in% var_analyse) {
            next
        }
        
        var_analyse = c(var_analyse, var)
        # topic_analyse = c(topic_analyse, topic)
        # unit_analyse = c(unit_analyse, unit)
        # samplePeriod_analyse = append(samplePeriod_analyse,
        #                               list(samplePeriod))
        # glose_analyse = c(glose_analyse, glose)

        Xex = get_dataEx(data=data,
                         Process=Process,
                         period=period,
                         verbose=verbose)

        if (verbose) {
            print(paste0("Data extracted for ", var))
            print(Xex)
        }

        # vars = names(Xex)[!(names(Xex) %in% c("ID", "Date"))]
        # vars = gsub("([_]obs)|([_]sim)", "", vars)
        # vars = vars[!duplicated(vars)]

        metaEx = dplyr::bind_rows(
                            metaEx,
                            dplyr::tibble(var=var,
                                          unit=unit,
                                          glose=glose,
                                          topic=
                                              paste0(topic,
                                                     collapse="/"),
                                          samplePeriod=
                                              paste0(samplePeriod,
                                                     collapse="/")))

        # Xex$Model = gsub("[_].*$", "", Xex$ID)
        # Xex$Code = gsub("^.*[_]", "", Xex$ID)
        # Xex = dplyr::select(Xex, -ID)
        # Xex = dplyr::select(Xex, Model, Code, dplyr::everything())

        dataEx = append(dataEx, list(Xex))
        names(dataEx)[length(dataEx)] = var
    }
    
    res = list(dataEx=dataEx, metaEx=metaEx)
    return (res)
}


# card_trend = function (data, card, period=NULL, verbose=TRUE) {




# }






#' @title sourceProcess
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
    process_allNames = str_extract(process_allAtt, "P[[:digit:]]")
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


#' @title dataEx
#' @export
get_dataEx = function (data, Process, period=NULL, flag=NULL,
                       mod=tibble(), verbose=TRUE) {

    if (verbose) {
        print(paste0('Computes ', Process$P$var))
    }
    
    if (!is.null(flag)) {
        # Local corrections if needed
        res = flag_data(data,
                        flag=flag,
                        mod=mod,
                        verbose=verbose)
        data = res$data
        mod = res$mod
    }

    dataEx = data

    nProcess = length(Process) - 1

    for (i in 1:nProcess) {

        print(paste0("Process ", i, "/", nProcess))
        
        process = Process[[paste0("P", i)]]
        process_names = names(process)
        for (i in 1:length(process)) {
            assign(process_names[i], process[[i]])
        }

        # Extraction
        dataEx = do.call(
            what=extraction_process,
            args=list(data=dataEx,
                      funct=funct,
                      funct_args=funct_args,
                      timeStep=timeStep,
                      samplePeriod=samplePeriod,
                      period=period,
                      isDate=isDate,
                      NApct_lim=NApct_lim,
                      NAyear_lim=NAyear_lim,
                      Seasons=Seasons,
                      onlyDate4Season=onlyDate4Season,
                      nameEx=nameEx,
                      keep=keep,
                      compress=compress,
                      rmNApct=rmNApct,
                      verbose=verbose))
    }

    return (dataEx)
}

#' @title X trend
#' @export
get_trend = function (data, Process, period=NULL, level=0.1,
                      flag=NULL, mod=tibble(),
                      verbose=TRUE) {

    if (verbose) {
        print(paste0('Computes ', ' trend'))
    }

    # Make sure to convert the period to a list
    period = as.list(period)
    # Set the max interval period as the minimal possible
    Imax = 0
    # Blank tibble for data to return
    trend_all = tibble()

    # For all periods
    for (per in period) {
        if (verbose) {
            print(paste0('For period : ',
                         paste0(per, collapse=' / ')))
        }

        dataEx = get_dataEx(data,
                            Process,
                            period=per,
                            flag=flag,
                            verbose=verbose)
        
        # Compute the trend analysis
        trend = trend_analyse(data=dataEx,
                              MK_level=level,
                              timeDep_option="AR1",
                              verbose=verbose)
        
        # Get the associated time interval
        I = lubridate::interval(per[1], per[2])
        # If it is the largest interval       
        if (I > Imax) {
            # Store it and the associated data
            Imax = I
            dataEx_all = dataEx
        }
        # Store the trend
        trend_all = bind_rows(trend_all, trend)
    }

    # Creates a list of results to return
    res = list(dataMod=data,
               mod=mod,
               dataEx=dataEx_all,
               trend=trend_all)
    return (res)
}



