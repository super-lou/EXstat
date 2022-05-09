var = "tCEN"
type = "saisonnalité"
glose = "Centre d'étiage (jour de l'année du VCN10)"
hydroYear = "01-01"

sampleSpan = c('05-01', '11-30')
yearNA_lim = 10
dayLac_lim = 3
NA_pct_lim = NULL
day_to_roll = 10

functM = NULL
functM_args = NULL
isDateM = FALSE

functY = which.minNA
functY_args = NULL
isDateY = TRUE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL


which.minNA = function (x) {
    idMin = which.min(x)
    if (identical(idMin, integer(0))) {
        idMin = NA
    }
    return (idMin)
}
