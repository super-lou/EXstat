var = "VCN10"
type = "sévérité"
glose = "Minimum annuel de la moyenne sur 10 jours du débit journalier"
hydroYear = "01-01"

sampleSpan = c('05-01', '11-30')
yearNA_lim = 10
dayLac_lim = 3
NA_pct_lim = NULL
day_to_roll = 10

functM = NULL
functM_args = NULL
isDateM = FALSE

functY = min
functY_args = list(na.rm=TRUE)
isDateY = FALSE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL
