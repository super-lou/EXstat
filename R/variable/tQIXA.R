var = "tQIXA"
type = "saisonnalité"
glose = "Date du maximum annuel du débit journalier"
hydroYear = "01-01"

sampleSpan = NULL
yearNA_lim = 10
dayLac_lim = 3
NA_pct_lim = NULL
day_to_roll = NULL

functM = NULL
functM_args = NULL
isDateM = FALSE

functY = which.maxNA
functY_args = NULL
isDateY = TRUE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL


which.maxNA = function (x) {
    idMax = which.max(x)
    if (identical(idMax, integer(0))) {
        idMax = NA
    }
    return (idMax)
}
