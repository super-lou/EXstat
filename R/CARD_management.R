# Copyright 2021-2024 Louis Héraut (louis.heraut@inrae.fr)*1                     
#           2023      Éric Sauquet (eric.sauquet@inrae.fr)*1
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#                     Nathan Pellerin
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
# EXstat R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with EXstat R package.
# If not, see <https://www.gnu.org/licenses/>.


#' @title CARD_management
#' @description Manage the CARD directory structure by performing automatic file operations to copy and paste CARD parameterization files more efficiently.
#'
#' @param CARD_path A [character][base::character] string representing the path to the downloaded CARD directory (it should end with `"CARD"`). In this directory, you can search the CARDs that you want in the `"__all__"` subdirectory that will be use for an analysis (see [layout] to know how to specify which CARD you want).
#' @param CARD_out A [character][base::character] string for a path to a directory where the CARD parameterization files will be copied and pasted for an analysis. Default is `NULL` if you want to use a `tmp` subdirectory in the `CARD_path` directory.
#' @param layout A [character][base::character] [vector][base::c()] specifying the tree structure of files that you want for your analysis. Each element of the vector represents either:
#' * the name of an analysis directory (e.g., `"EX"`)
#' * the beginning and ending of an analysis directory: `"["` for the start and `"]"` for the end
#' * the CARD name (e.g., `"QA"`)
#' For example, if you want to create an `"EX"` analysis directory for the CARDs named `"QA"` and `"QJXA"`, you can provide `c("EX", "[", "QA", "QJXA", "]")` (which is the default value).
#' @param underscore_to_white [logical][base::logical]. If `TRUE`, underscores in directory names will be replaced with spaces. Default is `TRUE`.
#' @param add_id [logical][base::logical]. If `TRUE`, numerical IDs will be added to the start of the copied and pasted CARD names to maintain the input order. Default is `TRUE`.
#' @param overwrite [logical][base::logical]. If `TRUE`, existing CARD files in the `CARD_out` directory will be overwritten. Default is `TRUE`.
#' @param verbose [logical][base::logical]. Should intermediate messages be printed during the execution of the function ? Default `FALSE`.
#' @param args An intermediate form of arguments that is useful if the argparse package is used. If not provided, it will be automatically created using the other function arguments. Default is `NULL`.
#'
#' @return CARD parameterization files will be copied and pasted from `CARD_path` and organized according to the structure given by [layout] into the `CARD_out` directory.
#'
#' @seealso
#' - [process_extraction()] for extracting variables.
#' - [process_trend()] for performing trend analysis on extracted variables.
#' - [CARD_extraction()] for extracting variables using CARD parameterization files.
#' 
#' @examples
#' \dontrun{
#' CARD_management(CARD="path/to/CARD",
#'                 tmp="path/to/copy/CARD",
#'                 layout=c("EX", "[", "QA", "QJXA", "]"),
#'                 overwrite=TRUE,
#'                 verbose=TRUE)
#' }
#' 
#' @export
#' @md
CARD_management = function (CARD_path,
                            CARD_out=NULL,
                            layout=c("EX", "[", "QA", "QJXA", "]"),
                            underscore_to_white=TRUE,
                            add_id=TRUE,
                            overwrite=TRUE,
                            verbose=FALSE,
                            args=NULL) {
    
    if (is.null(args)) {
        args = list(CARD_path=CARD_path, CARD_out=CARD_out, layout=layout,
                    underscore_to_white=underscore_to_white,
                    add_id=add_id, overwrite=overwrite,
                    verbose=verbose)        
    }

    if (is.null(args$CARD_out)) {
        args$CARD_out = file.path(args$CARD_path, "tmp")
    }
        
    if (args$verbose) {
        remind(args)
    }
    if (all(args$layout == "")) {
        print("Error : --layout is void\n", stderr())
        stop ()
    }

    source_dir = file.path(args$CARD_path, "__all__")

    OUT = unlist(args$layout)
    nOUT = length(OUT)
    test1 = "[[]|[(]|[]]|[)]"
    test2 = "[[]|[(]"
    for (i in 1:nOUT) {
        if (i < nOUT & !grepl(test1, OUT[i]) & !grepl(test2, OUT[(i+1)])) {
            OUT[i] = paste0(OUT[i], ".(NA)")
        }
        if (i == nOUT & !grepl(test1, OUT[(i)])) {
            OUT[i] = paste0(OUT[i], ".(NA)")
        }
    }
    OUT = unlist(sapply(OUT, strsplit, split="[.]"),
                 use.names=FALSE)

    OUT = paste0(OUT, collapse="','")
    OUT = gsub("[]]", ")", OUT)
    OUT = gsub("[[]|[(]", "=list(", OUT)
    OUT = gsub("[,]['][=]", "=", OUT)
    OUT = gsub("[(]['][,]", "(", OUT)
    OUT = gsub("[,]['][)]", ")", OUT)
    OUT = gsub("[)][']", ")", OUT)
    OUT = paste0("'", OUT)
    OUT = paste0("list(", OUT, ")")    
    OUT = eval(parse(text=OUT))
    OUT = unlist(OUT)
    OUT = names(OUT)
    OUT = gsub("[.]", "/", OUT)
    OUT = paste0(OUT, ".R")

    n = length(OUT)
    SUB = c()
    save = c()
    IN = c()
    DIR = c()
    for (i in 1:n) {
        path = unlist(strsplit(OUT[i], "/"))
        len = length(path)
        nsd = len - 2

        if (nsd < 0) {
            print("Error : No tmp detect\n", stderr())
            stop ()
            
        } else if (nsd == 0) {
            id = i
            
        } else if (nsd > 0) {

            for (j in 1:nsd) {
                
                if (!(path[(j+1)] %in% save)) {

                    if (length(SUB) >= nsd) {
                        if (any(path %in% save)) {
                            SUB[sum(path %in% save)] =
                                SUB[sum(path %in% save)] + 1
                            SUB[(sum(path %in% save)+1):length(SUB)] = 1
                        } else {
                            SUB[nsd] = SUB[nsd] + 1
                        }
                    } else {
                        SUB = c(SUB, 1)
                    }
                    id = 1
                    save = c(save, path[(j+1)])
                }

                obj = path[(j+1)]
                if (args$underscore_to_white) {
                    obj = gsub("[_]", " ", obj)
                }

                if (args$add_id) {
                    path[(j+1)] = paste0(formatC(SUB[j],
                                                 width=3,
                                                 flag="0"),
                                         "_", obj)
                }
            }
        }
        
        IN = c(IN, path[len])
        DIR = c(DIR, do.call(file.path, as.list(path[-len])))
        
        if (args$add_id) {
            idC = formatC(id, width=3, flag="0")
            path[len] = paste0(idC, "_", path[len])
        }
        
        id = id + 1
        OUT[i] = do.call(file.path, as.list(path))
    }

    DIR = DIR[!duplicated(DIR)]
    DIR = file.path(args$CARD_out, DIR)

    if (any(dir.exists(DIR)) &
        args$overwrite |
        !any(dir.exists(DIR))) {
        if (any(dir.exists(DIR)) &
            args$overwrite) {
            unlink(DIR, recursive=TRUE, force=TRUE)
        }
        for (i in 1:n) {
            dir.create(DIR[i], recursive=TRUE)
        }

        for (i in 1:n) {
            files = list.files(source_dir, recursive=TRUE)
            names(files) = basename(files)
            
            file.copy(file.path(source_dir, files[IN[i]]),
                      file.path(args$CARD_out, OUT[i]))
        }
        
    } else if (any(dir.exists(DIR)) &
               !args$overwrite) {
        warning (paste0("Some directories in ", paste0(DIR, collapse=", "),
                        " already exists. Please use 'overwrite=TRUE' if you want to overwrite current directories."))
    }

    if (args$verbose) {
        print("done")
    }
}


remind = function (args) {
    print("PARAMETERS:")
    n = length(args)
    args_name = names(args)
    for (i in 1:n) {
        print(paste0("    --", args_name[i], " ",
                     paste0(args[[i]], collapse=" ")))
    }
}
