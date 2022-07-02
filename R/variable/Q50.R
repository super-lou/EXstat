var = "Q50"
type = "sévérité"
glose = "Décile Q50 (débits classés)"
event = "Moyennes Eaux"
hydroYear = "09-01"

sampleSpan = NULL
yearNA_lim = 10
dayLac_lim = 3
NA_pct_lim = NULL
day_to_roll = NULL

functM = NULL
functM_args = NULL
isDateM = FALSE

compute_Qp = function (Q, p) {
    Qp = quantile(Q[!is.na(Q)], p)
    return (Qp)
}

functY = compute_Qp
functY_args = list(p=0.5)
isDateY = FALSE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL
