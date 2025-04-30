# Copyright xxxx      Benjamin Renard (benjamin.renard@inrae.fr)*1
#           2020-2021 Valentin Mansanarez
#           2021-2024 Louis Héraut (louis.heraut@inrae.fr)*1
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


## 1. GENERAL MANN-KENDALL ___________________________________________
### 1.1. Statistical test ____________________________________________
generalMannKendall_hide = function(X, level=0.1,
                                   time_dependency_option='INDE',
                                   do_detrending=TRUE,
                                   verbose=FALSE) {
    
    #*****************************************************************
    # STEP 0: preliminaries
    #*****************************************************************
    # Create output list and initialize it
    OUT = list(H=NA, P=NA, STAT=NA, TREND=NA, DEP=NA)
    # Check time_dependency_option is valid
    if (!((time_dependency_option == 'INDE') | (time_dependency_option == 'AR1') | (time_dependency_option == 'LTP'))) {
        if (verbose) warning('Unknown time_dependency_option')
        return (OUT)
    }
    # Remove Nas from X to create NA-free vector Z
    Z = X[!is.na(X)]
    n = length(Z)
    # Don't even try if less than 3 non-missing values
    if (n < 3) {
        if (verbose) warning('less than 3 non-missing values')
        return (OUT)
    }
    # Get basic MK stat + Sen's trend estimate
    get.MK.basics = getMKStat(X)
    MK = get.MK.basics$stat
    OUT$TREND = get.MK.basics$trend

    #*****************************************************************
    # CASE 1: 'INDE' or 'AR1'
    #*****************************************************************
    if ((time_dependency_option == 'INDE') | (time_dependency_option == 'AR1')) {
        # Compute basic variance
        var0 = ((n*(n-1)*(2*n+5)))/18
        # Compute ties correction and get ties-corrected variance
        var1 = var0-getTiesCorrection(Z)
        if (is.na(var1)) {
            if (verbose) warning('NA variance')
            return (OUT)
        }
        if (var1 <= 0) {
            if (verbose) warning('negative variance')
            return (OUT)
        }
        # Compute autocorrelation correction if time_dependency_option == 'AR1'
        if (time_dependency_option == 'AR1') {
            AR1.correction=getAR1Correction(X)
            correction=AR1.correction$correction
            OUT$DEP = AR1.correction$lag1
        } else {
            correction = 1
            OUT$DEP = 0
        }
        MKvar = var1*correction
        if (MKvar <= 0) {
            if (verbose) warning('negative variance')
            return (OUT)
        }
    }

    #*****************************************************************
    # CASE 2: 'LTP'
    #*****************************************************************
    if (time_dependency_option == 'LTP') {
        # Estimate Hurst Coeff
        Hu = estimateHurst(X, do_detrending, OUT$TREND)
        OUT$DEP = Hu
        # Get autocorrelation function
        lambda = 0:n
        C = 0.5*(abs(lambda+1)^(2*Hu) - 2*abs(lambda)^(2*Hu) + abs(lambda-1)^(2*Hu))
        # Compute variance of MK using the monstrous 4-level loop...
        var0 = 0
        for (j in 2:n) {
            for (i in 1:(j-1)) {
                for (l in 2:n) {
                    for (k in 1:(l-1)) {
                        num = C[abs(j-l)+1] - C[abs(i-l)+1] - C[abs(j-k)+1] + C[abs(i-k)+1]
                        den = sqrt((2-2*C[abs(i-j)+1]) * (2-2*C[abs(k-l)+1]))
                        var0 = var0 + asin(num/den)
                    }
                }
            }
        }
        var1 = (2/pi)*var0
        if (is.na(var1)) {
            if (verbose) warning('NA variance')
            return (OUT)
        }
        if (var1 <= 0) {
            if (verbose) warning('negative variance')
            return (OUT)
        }
        # bias correction
        a0 = (1.0024*n-2.5681)/(n+18.6693)
        a1 = (-2.2510*n+157.2075)/(n+9.2245)
        a2 = (15.3402*n-188.6140)/(n+5.8917)
        a3 = (-31.4258*n+549.8599)/(n-1.1040)
        a4 = (20.7988*n-419.0402)/(n-1.9248)
        B = a0+a1*Hu+a2*Hu^2+a3*Hu^3+a4*Hu^4
        MKvar = var1*B
        if (MKvar <= 0) {
            if (verbose) warning('negative variance')
            return (OUT)
        }
    }
    #*****************************************************************
    # FINAL STEP: Get test statistics, significance, pval, etc.
    #*****************************************************************
    # Final test statistics
    if (MK > 0) {
        stat = (MK-1)/sqrt(MKvar)
    } else if (MK < 0) {
        stat = (MK+1)/sqrt(MKvar)
    } else {
        stat = MK/sqrt(MKvar)
    }
    OUT$STAT = stat
    # p-val (2-sided test)
    OUT$P = 2*stats::pnorm(-1*abs(stat), mean=0, sd=1)
    # decision
    OUT$H = (OUT$P < level)
    return (OUT)
}


### 1.2. Wrap ________________________________________________________
#' @title General Mann-Kendall
#' @description A general version of the Mann-Kendall test, enabling
#' various dependence assumptions.
#' @param X numeric vector, data. IMPORTANT: X is assumed to be
#' regularly-spaced. It uses NA to fill the gaps rather than removing
#' missing values.
#' @param level numeric in (0,1), level of the test.
#' @param time_dependency_option string, option for handling temporal dependence.
#' Available :
#' \enumerate{
#'     \item 'INDE', assume independence (i.e. the standard MK test)
#'     \item 'AR1', assumes AR1 short-term dependence structure (i.e.
#' Hamed and Rao's version of the MK test)
#'     \item 'LTP', assume long-term persistence (i.e. Hamed's version
#' of the MK test)
#' }
#' @param do_detrending, logical, only used for time_dependency_option == LTP:
#' do detrending before estimating Hurst coefficient (default=TRUE as
#' recommended in Hamed's paper)
#' @param show_advance_stat [logical][base::logical] Whether to display advanced statistical details. Default is `FALSE`.
#' @param verbose [logical][base::logical] Whether to print intermediate messages. Default is `FALSE`.
#' @return A tibble with the following possible fields :
#' \enumerate{
#'     \item level: level of the test
#'     \item H: logical, reject (true) or do not reject (false) H0
#'     \item p: p-value of the test
#'     \item stat: test statistics
#'     \item a: trend estimate (using Sen's slope estimate)
#'     \item time_dependency_option: dependence estimate (= 0 if time_dependency_option='INDE',
#' =lag-1 autocorrelation if time_dependency_option='AR1', =Hurst coefficient if
#' time_dependency_option='LTP')
#' }
#' @details
#' \enumerate{
#'     \item Handling of ties: Specific formula exist for INDE and
#' AR1, but the LTP case is trickier. Hammed's paper is unclear on how
#' to handle ties, especially at the step of Hurst coefficient
#' estimation. There is a normal-score transformation at this step,
#' and one needs to decide how to assign a rank to ties. What is
#' implemented below is the option ties.method = "random", i.e. the
#' rank is randomized for ties. This is not, strictly speaking,
#' correct because this randomization impacts the dependence
#' structure. However synthetic runs suggest it works OK.
#'     \item Computational efficiency: Likely poor for case
#' time_dependency_option='LTP'. There is a 4-level loop which leads to a n^4
#' algorithm. I attempted to vectorize this loop but it didn't improve
#' things => Expect significant running times for time_dependency_option='LTP
#' ' when size(X) > 50... (orders of magnitude: 1s for n=30, 10s for
#' n=50, 2-3 minutes for n=100). On the other hand both options INDE
#' and AR1 are very fast.
#' }
#' @examples
#' \dontrun{
#' data(nhtemp) #Average Yearly Temperatures in New Haven
#' generalMannKendall(X=nhtemp, time_dependency_option='AR1')
#' }
#' @references
#' \enumerate{
#'     \item Hamed, Rao, 1998. A modified Mann-Kendall trend test for
#' autocorrelated data. J. Hydrol., 204(1-4): 182-196.
#'     \item Hamed, 2008. Trend detection in hydrologic data: The
#' Mann-Kendall trend test under the scaling hypothesis. J. Hydrol.,
#' 349(3-4): 350-363.
#'  }
#' @export
GeneralMannKendall = function(X,
                              level=0.1,
                              time_dependency_option='INDE',
                              do_detrending=TRUE,
                              show_advance_stat=FALSE,
                              verbose=FALSE) {
    
    res = generalMannKendall_hide(X=X,
                                  level=level,
                                  time_dependency_option=time_dependency_option,
                                  do_detrending=do_detrending,
                                  verbose=verbose)

    if (show_advance_stat) {
        res = dplyr::tibble(level=level,
                            H=res$H,
                            p=res$P,
                            stat=res$STAT,
                            time_dependency_option=res$DEP,
                            a=res$TREND)
    } else {
        res = dplyr::tibble(level=level,
                            H=res$H,
                            p=res$P,
                            a=res$TREND)
    }
    return (res)
}

### 1.3. Tools _______________________________________________________
#### 1.3.1. Exported _________________________________________________
#' @title  Mann-Kendall statistics
#' @description Compute MK stat and Sen's trend estimate
#' @param X numeric, data
#' @return A list, with components: $stat, MK statistics; $trend,
#' Sen's estimate.
#' @examples
#' \dontrun{
#' data(nhtemp) #Average Yearly Temperatures in New Haven
#' getMKStat(X = nhtemp)
#' }
getMKStat = function(X) {
    n = length(X);count.p = 0;count.m = 0;k = 0
    slope.list = matrix(NA, ((n-1)*n)/2, 1)
    for (j in 2:n) {
        for (i in 1:(j-1)) {
            k = k+1
            if ((!is.na(X[j])) & (!is.na(X[i]))) {
                slope.list[k] = (X[j]-X[i])/(j-i)
                if (X[j] > X[i]) {
                    count.p = count.p+1
                } else if (X[j] < X[i]) {
                    count.m = count.m+1
                }
            }
        }
    }
    stat = count.p-count.m
    trend = stats::median(slope.list[!is.na(slope.list)])
    return (list(stat=stat, trend=trend))
}

#' @title Hurst coefficient
#' @description Estimate the Hurst coefficient of a series
#' @param Z numeric, data (NA-free)
#' @param do_detrending logical, detrend data before estimating Hurst?
#' @param trend numeric, trend value (only used if detrending
#' required)
#' @return The estimated value of the hurst coefficient
#' @examples
#' \dontrun{
#' data(nhtemp) #Average Yearly Temperatures in New Haven
#' estimateHurst(Z=nhtemp)
#' }
estimateHurst = function(Z, do_detrending=TRUE,
                         trend=getMKStat(Z)$trend) {
    #~****************************************************************
    #~* PURPOSE: Get correction for AR(1)-like dependence
    #~****************************************************************
    #~ IN:  1. Z, data vector
    #~      2. do_detrending, detrend data before estimating Hurst?
    #~      3. trend, trend value (only used if detrending required)
    #~ OUT: 1. Estimated value of the hurst coefficient
    #~****************************************************************
    n = length(Z)
    # Detrend if requested
    if (do_detrending) {
        Y = Z-trend*(1:n)
    } else {
        Y = Z
    }
    # Transform to normal-score - Note that ties.method = "random",
    # might affect autocorrelation! but Hamed's paper is unclear on
    # how to treat ties at this step
    W = randomizedNormalScore(Y)
    Max.Lkh = stats::optimize(f=HurstLkh, interval=c(0.5, 1),
                              W, maximum=TRUE)
    H = Max.Lkh$maximum
    return (H)
}

#' @title Randomized Normal Score
#' @description Randomized Normal Score transformation
#' @param x numeric, data
#' @return the normal-score-transformed series
#' @examples
#' \dontrun{
#' data(nhtemp) # Average Yearly Temperatures in New Haven
#' z = randomizedNormalScore(x=nhtemp)
#' par(mfrow = c(1, 2))
#' plot(nhtemp, type='b');plot(z, type='b')
#' }
randomizedNormalScore = function(x) {
    # empirical frequencies
    p = (rank(x,
              ties.method="random",
              na.last="keep")) / (1 + sum(!is.na(x)))
    # Normal quantile
    z = stats::qnorm(p)
    return (z)
}

#### 1.3.2. Not exported _____________________________________________
#' @title Ties correction
#' @description Compute correction to the variance of MK statistics to
#' account for ties
#' @param Z numeric, data (NA-free)
#' @return the correction for the variance of MK stat
#' @examples
#' \dontrun{
#' data(nhtemp) # Average Yearly Temperatures in New Haven
#' getTiesCorrection(Z=nhtemp)
#' getTiesCorrection(rnorm(100))
#' }
getTiesCorrection = function(Z) {
    n = length(Z)
    w = matrix(NA, n, 1)
    tie = matrix(NA, n, 1)
    v = matrix(NA, n, 1)
    for (i in 1:n) {
        w[i] = sum(Z == Z[i])
    } # counts how many times each value is duplicated
    for (i in 1:n) {
        # create a vector containing the number of ties of extent i
        tie[i] = sum(w == i)/i
        # save contribution of i-ties to correction
        v[i] = tie[i]*i*(i-1)*(2*i+5) 
    }
    return (sum(v)/18)
}

#' @title AR(1) correction
#' @description Compute correction to the variance of MK statistics to
#' account for AR(1) autocorrelation
#' @param Z numeric, data (NA-free)
#' @return A list with components: $lag1 (estimated lag-1 correlation
#' coefficient) and $correction (the correction for the variance of MK
#' stat)
#' @examples
#' \dontrun{
#' data(nhtemp) # Average Yearly Temperatures in New Haven
#' getAR1Correction(Z=nhtemp)
#' getAR1Correction(rnorm(100))
#' }
getAR1Correction = function(Z) {
    n = length(Z)
    w = matrix(NA, n-2, 1)
    # Compute lag-1 coefficient
    Z0 = Z[!is.na(Z)]
    m = mean(Z0)
    x = Z[1:(n-1)]
    y = Z[2:n]
    mask = (!is.na(x)) & (!is.na(y))
    lag1 = sum((x[mask]-m) * (y[mask]-m)) / sum((Z0-m)^2)
    #Compute correction
    for (i in 1:(n-2)) {
        w[i] = (n-i) * (n-i-1) * (n-i-2) * ((lag1)^(i))
    } # save contribution of lag i to correction
    correction = 1+(2/(n*(n-1)*(n-2))) * sum(w)
    return (list(lag1=lag1, correction=correction))
}

#' @title Hurst likelihood
#' @description Compute the likelihood function to be maximized for
#' estimating the Hurst coefficient H
#' @param H numeric,  hurst coeff. value
#' @param x, data sample
#' @return log-likelihood value
#' @examples
#' \dontrun{
#' data(nhtemp) # Average Yearly Temperatures in New Haven
#' HurstLkh(H=0.8, x=nhtemp)
#' HurstLkh(H=0.5, x=nhtemp)
#' HurstLkh(H=0.2, x=nhtemp)
#' }
HurstLkh = function(H, x) {
    n = length(x)
    # Compute Cn(H)
    CnH = matrix(NA, n, n)
    for (i in 1:n) {
        for (j in 1:n) {
            l = abs(i-j)
            CnH[i, j] = 0.5*(abs(l+1)^(2*H) - 2*(abs(l)^(2*H)) + abs(l-1)^(2*H))
        }
    }
    mask = !is.na(x)
    m = sum(mask)
    v0 = stats::qnorm((1:m)/(m+1))
    g0 = stats::var(v0)
    L = -0.5*log(det(CnH[mask, mask])) - (t(x[mask]) %*% solve(CnH[mask, mask]) %*% x[mask])/(2*g0)
    return (L)
}


## 2. REGIONAL TEST __________________________________________________
#' @title FDR field significance
#' @description Field significance using the false detection rate
#' approach
#' @param pvals numeric vector, p-values of local tests
#' @param level numeric in (0, 1), level at which field significance
#' is evaluated
#' @return pFDR, the FDR p-value, interpreted as follows: local
#' p-values smaller than pFDR are field-significant.
#' @examples
#' \dontrun{
#' set.seed(123456) # Make example reproducible
#' level = 0.1 # Level of the test
#' data(nhtemp) # Average Yearly Temperatures in New Haven
#' # Add 29 stationary series to nhtemp series
#' X = matrix(c(nhtemp, rnorm(length(nhtemp)*29)),
#'              nrow=length(nhtemp))
#' # Compute local p-values from a MK test
#' pvals = rep(NA, 30)
#' for (i in 1:30) {
#'     pvals[i] = generalMannKendall(X[, i], level=level)$P
#' }
#' # Evaluate field significance
#' pFDR = fieldSignificance_FDR(pvals, level)
#' which(pvals <= level) # Locally-significant sites
#' which(pvals <= pFDR) # FDR-significant sites
#' }
#' @references Benjamini, Y., and Y. Hochberg (1995), Controlling the
#' false discovery rate: A practical and powerful approach to multiple
#' testing, J. R. Stat. Soc., Ser. B., 57, 289–300.
fieldSignificance_FDR = function (pvals, level=0.1) {
    n = length(pvals)
    z = sort(pvals)
    local = (z <= (level/n)*(1:n))
    if (all(local == FALSE)) {
        pFDR = 0
    } else {
        indx = max(which(local))
        pFDR = z[indx]
    }
    return (pFDR)
}


## 3. OTHER __________________________________________________________




tree = function (x, n, end=FALSE, inEnd=NULL, lim=50, verbose=TRUE) {   
    if (verbose) {
        if (n < 1) {
            d = ""
            h = ""
            
        } else {
            if (!end) {
                h = "\u251C\u2500\u2500 " # "├── "
            } else {
                h = "\u2514\u2500\u2500 " # "└── "
            }

            # d = strrep("│   ", n-1)
            d = strrep("\u2502   ", n-1)
            if (!is.null(inEnd)) {
                for (ie in inEnd) {
                    id = (ie-1)*4 + 1
                    substr(d, id, id) = " "
                }
            }
        }
        nh = nchar(h)
        nd = nchar(d)
        Lines = c()
        nextLine = x
        nbNewline = 0
        nbChar = nchar(nextLine) + nh + nd

        while (nbChar > lim) {
            nbNewline = nbNewline + 1
            posSpace = which(strsplit(nextLine, "")[[1]] == " ")
            ok = posSpace + nh + nd < lim
            if (all(!ok)) {
                posNewline = lim - (nh + nd)
            } else {
                posNewline = utils::tail(posSpace[ok], 1)
            }
            line = substring(nextLine, 1, posNewline-1)
            nextLine = substring(nextLine, posNewline+1, nchar(nextLine))
            nbChar = nchar(nextLine) + nh + nd
            Lines = c(Lines, line)
        }
        Lines = c(Lines, nextLine)

        nLine = length(Lines)
        for (i in 1:nLine) {
            line = Lines[i]
            if (i > 1) {
                if (!end) {
                    # h = "│   "
                    h = "\u2502   "
                } else {
                    h = "    "
                }
            }
            X = paste0(d, h, line)
            space = lim - nchar(X)            
            X = paste0(X, strrep(" ", space))
            print(X)
        }
    }
}


check_leapYear_hide = function (year) {
    if ((year %% 4) == 0) {
        if ((year %% 100) == 0) {
            if ((year %% 400) == 0) {
                return (TRUE)
            } else {
                return (FALSE)
            }
        } else {
            return (TRUE)
        }
    } else {
        return (FALSE)
    }  
}


check_leapYear = function (year) {
    sapply(year, check_leapYear_hide)
}


addLeapYear = function (year, month) {
    if (month == 2) {
        if ((year %% 4) == 0) {
            if ((year %% 100) == 0) {
                if ((year %% 400) == 0) {
                    return (1)
                } else {
                    return (0)
                }
            } else {
                return (1)
            }
        } else {
            return (0)
        }
    } else {
        return (0)
    }
}

rmFUCKING29FEB = function (month, day) {
    if (month == 2 & day == 29) {
        return (-1)
    } else {
        return (0)
    }
}




is.character_or_date = function (x) {
    is.character(x) | lubridate::is.Date(x)
}




get_time = function (timer=NULL) {
    if (is.null(timer)) {
         print("start timer")
    } else {
        print(round(Sys.time() - timer, 3))   
    }
    return (Sys.time())
}


check_date_hide = function (x) {
    ok = !is.na(lubridate::ymd(x, quiet=TRUE))
    return (ok)
}
check_date = function (X) {
    Ok = sapply(X, check_date_hide, USE.NAMES=FALSE)
    return (Ok)
}


adjust_date_hide = function (x) {
    date = lubridate::ymd(x, quiet=TRUE)
    if (is.na(date)) {
        year = as.numeric(substr(x, 1, 4))
        month = as.numeric(substr(x, 6, 7))
        day = as.numeric(substr(x, 9, 10))
        
        while (is.na(date)) {
            day = day - 1
            date = lubridate::ymd(sprintf("%04d-%02d-%02d", year, month, day),
                                  quiet=TRUE)
        }
    }
    return (date)
}

adjust_date = function (X) {
    Date = as.Date(sapply(X, adjust_date_hide, USE.NAMES=FALSE))
    return (Date)
}


is.na_not_nan = function(x) {
  is.na(x) & !is.nan(x)
}
