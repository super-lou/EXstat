var = "Q75"
type = "sévérité"
unit = "m^{3}.s^{-1}"
glose = "Décile Q75 (débits classés)"
event = "Moyennes Eaux"
hydroYear = "09-01"
hydroPeriod = c("09-01", "08-31")

sampleSpan = NULL
yearNA_lim = 10
dayLac_lim = NULL
NA_pct_lim = 1
day_to_roll = NULL

functM = NULL
functM_args = NULL
isDateM = FALSE

compute_Qp = function (Q, p) {
    Qp = quantile(Q[!is.na(Q)], p)
    return (Qp)
}

functY = compute_Qp
functY_args = list(p=0.75)
isDateY = FALSE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL
