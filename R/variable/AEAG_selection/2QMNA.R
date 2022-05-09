var = "QMNA"
type = "sévérité"
glose = "Minimum annuel de la moyenne mensuelle du débit journalier"
hydroYear = "01-01"

sampleSpan = c('05-01', '11-30')
yearNA_lim = 10
dayLac_lim = 3
NA_pct_lim = NULL
day_to_roll = NULL

functM = mean
functM_args = list(na.rm=TRUE)
isDateM = FALSE

functY = min
functY_args = list(na.rm=TRUE)
isDateY = FALSE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL
