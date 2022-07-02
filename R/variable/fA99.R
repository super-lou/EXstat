var = "fA99"
type = "sévérité"
glose = "Fréquence annuelle (Q > Q99)"
event = "Crue"
hydroYear = "09-01"

sampleSpan = NULL
yearNA_lim = 10
dayLac_lim = 3
NA_pct_lim = NULL
day_to_roll = NULL

functM = NULL
functM_args = NULL
isDateM = FALSE

compute_fAp = function (Q, p) {
    Qp = compute_Qp(Q, p)
    n = sum(as.numeric(Q > Qp))
    N = length(Q)
    fA = n/N
    return (fA)
}

compute_Qp = function (Q, p) {
    Qp = quantile(Q[!is.na(Q)], p)
    return (Qp)
}

functY = compute_fAp
functY_args = list(p=0.99)
isDateY = FALSE

functYT_ext = NULL
functYT_ext_args = NULL
isDateYT_ext = FALSE
functYT_sum = NULL
functYT_sum_args = NULL
