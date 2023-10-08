# [Ch-15 Functions] ----------------------------------------------------------------------------------

# [Ch-15 Function Manual] -----------------------------------------

#' @title Manual for Ch15. Functions
#' @description Ch15. Nonparametric Methods
#' @param fn Function number 0:11, Default: 0
#' @return None.
#'
#' @examples
#' ch15.man()
#' ch15.man(2)
#' ch15.man(11)
#' @rdname ch15.man
#' @export
ch15.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] norm.diag\t\tDiagnosis of the Normality of Data\n")
        cat("[2] signtest.p \t\tSign Test (Binomial Test)\n")
        cat("[3] runs.dist\t\tPlots and Tables of Runs Distribution\n")
        cat("[4] runs.tsp \t\tRuns Test\n")
        cat("[5] corr.tsp\t\tPearson & Spearman Correlation Coefficient\n")
        cat("[6] ranksum.dist \tDistribution of W-M-W Rank-Sum Test Statistic\n")
        cat("[7] ranksum.tsp \tWilcoxon-Mann-Whitney Rank-Sum Test\n")
        cat("[8] signrank.dist \tPlots and Tables of Wilcoxon Signed-Rank Distribution\n")
        cat("[9] signrank.tsp \tWilcoxon Signed-Rank Test\n")
        cat("[10] kruskal.tsp   \tKruskal-Wallis Test\n")
        cat("[11] friedman.tsp  \tFriedman Test\n")
    }
    if (1 %in% fn) {
        cat("[1] Diagnosis of the Normality of Data\n")
        cat("norm.diag(x, xrng, by=1, dig=4, ws=c(8,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Data vector.\n")
        cat("[Optional Input]--------------------------\n")
        cat("xrng\t Range of x-axis, Default: mean \U00B1 3 \U00D7 stdev.\n")
        cat("by\t Histogram class interval, Default: 1.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(8,4).\n")
    }
    if (2 %in% fn) {
        cat("[2] Sign Test (Binomial Test)\n")
        cat("signtest.p(x, mu0=0, alt=\"two\", dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Data vector.\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu0\t Mean value under H0, Default: 0.\n")
        cat("alt\t Type of alternative hypothesis, Default: \"two\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (3 %in% fn) {
        cat("[3] Plots and Tables of Runs Distribution\n")
        cat("runs.dist(n1=2:5, n2=2:5, alp=0.05, ws=c(7,4))\n")
        cat("n1\t Number of data in group 1, Default: 2:5.\n")
        cat("n2\t Number of data in group 2, Default: 2:5.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (4 %in% fn) {
        cat("[4] Runs Test\n")
        cat("runs.tsp(x, n1, n2, alp=0.05, alt=\"two\", dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Data vector (or number of runs).\n")
        cat("[Optional Input]--------------------------\n")
        cat("n1\t Number of data in group 1 (without raw data).\n")
        cat("n2\t Number of data in group 2 (without raw data).\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("alt\t Type alternative hypothesis, Default: \"two\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (5 %in% fn) {
        cat("[5] Pearson & Spearman Correlation Coefficients\n")
        cat("corr.tsp(x, y, r0=0, alp=0.05, alt=\"two\", type=\"s\", dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Data of the first variable.\n")
        cat("y\t Data of the second variable.\n")
        cat("[Optional Input]--------------------------\n")
        cat("r0\t Correlation coefficient value under H0.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("alt\t Type alternative hypothesis, Default: \"two\".\n")
        cat("type\t Calculation method: one of (\"s\", \"p\", \"b\"), Default: \"s\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(8,4).\n")
    }
    if (6 %in% fn) {
        cat("[6] Distribution of W-M-W Rank-Sum Test Statistic\n")
        cat("ranksum.dist(n1=1:2, n2=2, alp=\"n\", dig=4, ws=c(7,4))\n")
        cat("[Optional Input]--------------------------\n")
        cat("n1\t Number of data in group 1, Default: 1:2.\n")
        cat("n2\t Number of data in group 2, Default: 2.\n")
        cat("alp\t Level of significance, Default: \"n\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (7 %in% fn) {
        cat("[7] Wilcoxon-Mann-Whitney Rank-Sum Test\n")
        cat("ranksum.tsp(x, y, alt=\"two\", dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Data of the first group.\n")
        cat("y\t Data of the second group.\n")
        cat("[Optional Input]--------------------------\n")
        cat("alt\t Type of alternative hypothesis, Default: \"two\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (8 %in% fn) {
        cat("[8] Plots and Tables of Wilcoxon Signed-Rank Distribution\n")
        cat("signrank.dist(nv=5:10, alp, ws=c(7,4), sep=FALSE)\n")
        cat("nv\t Number of paired data, Default: 5:10.\n")
        cat("alp\t Level of sinificance.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("sep\t Logical: plot in separate facets? Default: FALSE.\n")
    }
    if (9 %in% fn) {
        cat("[9] Wilcoxon Signed-Rank Test\n")
        cat("signrank.tsp(x, y, mu0=0, alt=\"two\", dig=4, ws=\"n\")\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Data of the first group.\n")
        cat("y\t Data of the second group.\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu0\t Mean difference under the null hypothesis, Default: 0.\n")
        cat("alt\t Type of alternative hypothesis, Default: \"two\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
    }
    if (10 %in% fn) {
        cat("[10] Kruskal-Wallis Test\n")
        cat("kruskal.tsp(x, y, dig=4, ws=\"n\")\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Response data.\n")
        cat("y\t Data of factor levels.\n")
        cat("[Optional Input]--------------------------\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
    }
    if (11 %in% fn) {
        cat("[11] Friedman Test\n")
        cat("friedman.tsp(x, a, b, dig=4, ws=\"n\")\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Response data.\n")
        cat("a\t Data of factor levels.\n")
        cat("b\t Data of block levels.\n")
        cat("[Optional Input]--------------------------\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
    }
}

# Find the relevant alternative (1="less", 2="greater", 3="two sided")
alt.num <- function(alt) {
    nalt <- grep(alt, c("less", "greater", "two.sided"))
    if (length(nalt)>=2) nalt <- max(nalt)
    if (length(nalt)==0) nalt <- grep(alt, c("lower", "upper", "two-sided"))
    invisible(nalt)
}

# [15-1] Diagnosis of the Normality of Data
#' @title Diagnosis of Normality
#' @description Investigate the Normality of Data
#' @param x Data vector.
#' @param xrng Range of x-axis, Default: c(mean-3stdev, mean+3stdev).
#' @param by Histogram class interval, Default: 1.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(8,4).
#' @return None.
#'
#' @examples
#' (x <- c(1,2,5,7, rep(8,7), rep(9,5), rep(10,4)))
#' norm.diag(x)
#'
#' set.seed(1234); y <- rbinom(50, 20, 0.5)
#' norm.diag(y)
#' set.seed(1234); y <- rbinom(50, 20, 0.2)
#' norm.diag(y)
#' @rdname norm.diag
#' @export
norm.diag <- function(x, xrng, by=1, dig=4, ws=c(8,4)) {
    dc <- c("cyan","red","blue")
  # Set the range of x-axis
    xm <- mean(x)
    xs <- sd(x)
    if (missing(xrng)) {
        x1 <- min(x, xm-3*xs)
        x2 <- max(x, xm+3*xs)
        # [Correction]
        xrng <- c(x1-0.5*by, x2+0.5*by)
    }

  # Diagnosis histogram
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        par(mfrow=c(1,2))
        hist(x, prob=T, breaks=seq(xrng[1], xrng[2], by=by), col=dc[1])
        lines(density(x), lwd=2, col=dc[2])
        xax <- seq(xrng[1], xrng[2], length=100)
        lines(xax, dnorm(xax, xm, xs), lwd=2, col=dc[3])

      # Normal probability plot
        qqnorm(x, pch=19, xlab="Theoretical Quantile", ylab="Sample Quantile")
        grid(col="green")
        qqline(x, col=dc[2])
      # Test of normality (Shapiro-Wilk's Test)
        shap <- shapiro.test(x)
        cat("[Normality Test (Shapiro-Wilk's Test)] _________________\n")
        cat("Test Statistic =", round(shap$stat, dig), "\t P-value =", 
            format(shap$p.val, digits=dig), "\n")
    }
}

# [15-2] Sign Test (Binomial Test)
#' @title Sign Test (Binomial Test)
#' @description Sign Test (Binomial Test) with a Plot
#' @param x Data vector.
#' @param mu0 Mean value under H0, Default: 0.
#' @param alt Type of alternative hypothesis, Default: 'two'.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return None.
#'
#' @examples
#' (x <- c(1,2,5,7, rep(8,7), rep(9,5), rep(10,4)))
#' signtest.p(x=x, mu0=7, alt="gr")
#' signtest.p(x=x, mu0=7)
#' signtest.p(x=x, mu0=9, alt="le")
#' @rdname signtest.p
#' @export
signtest.p <- function(x, mu0=0, alt="two", dig=4, ws=c(7,4)) 
{
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    d <- x-mu0
  # Number of data greater than mu0
    nn <- length(x)
    n <- sum(d != 0)
    n1 <- sum(d > 0)
    n2 <- sum(d < 0)
    nmax <- max(n1, n2)

    pv1 <- 1-pbinom(n1-1, n, 0.5)
    pv2 <- 1-pbinom(n2-1, n, 0.5)

  # Normal approximation method (continuity correction)
    mu <- n/2
    va <- n/4
    sig <- sqrt(va)
    apv1 <- pnorm(n1-0.5, mu, sig,lower.tail=F)
    apv2 <- pnorm(n2-0.5, mu, sig,lower.tail=F)
  # Calculate and print p-value
    nalt <- alt.num(alt)
    if (nalt==2) {
        h1 <- "Upper-tailed"
        pv <- pv1
        z0 <- (n1-0.5-mu)/sig
        apv <- 1-pnorm(z0)
        h2 <- paste0("Pr(N \U2265 ", n1, ") = ", myf1(pv))
        h3 <- paste0("Pr(Z > ", round(z0, dig), ") = ", myf1(apv))
        xl <- "Number of Positive Signs"
    } else if (nalt==1) {
        h1 <- "Lower-tailed"
        pv <- pv2
        z0 <- (n2-0.5-mu)/sig
        apv <- 1-pnorm(z0)
        h2 <- paste0("Pr(N \U2265 ", n2, ") = ", myf1(pv))
        h3 <- paste0("Pr(Z > ", round(z0, dig), ") = ", myf1(apv))
        xl <- "Number of Negative Signs"
    } else {
        h1 <- "Two-tailed"
        pv <- 2*min(pv1, pv2)
        z0 <- max(0, (nmax-0.5-mu)/sig)
        apv <- 2*(1-pnorm(z0))
        h2 <- paste0("2\U00D7", "Pr(N \U2265 ", nmax, ") = ", myf1(pv))
        h3 <- paste0("Pr(|Z| > ", round(z0, dig), ") = ", myf1(apv))
        xl <- "Number of P/N Signs"
    }
  # Print test results -------------------------------
    cat(paste0("[Sign Test; ", h1, "]"), "----------------------------------\n")
    cat(paste0("n0=", nn, ",  mu0=", mu0, "   \U21D2   n=", n, 
               ",  n1(+)=", n1, ",  n2(-)=", n2), "\n")
    cat("Exact p-value:", h2, "\n")
    cat("[Normal Approximation (Continuity Correction)]\n")
    cat(paste0("E(N)=", round(mu, dig), ", Var(N)=", round(va, dig), 
        " \U21D2 Z0 = ", myf1(z0), " \U21D2 ", h3), "\n")

  # Plot distribution of the test statistic
    if (is.numeric(ws)) {
        xa <- 0:n
        xca <- (0:(10*n))/10
        pdf <- dbinom(xa, n, 0.5)
        ymax <- max(pdf)*1.05
        ymin <- -0.1*max(pdf)

        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        plot(xa, pdf, type="n", xlab=xl, ylab="f(x)", ylim=c(ymin, ymax),
            main=paste0("Distribution of Sign Test Statistic (n=", n, ")"))
      # Normal approximation
        lines(xca, dnorm(xca, mu, sig), col="blue")
        abline(h=0)
      # Distribution of the test statistic
        lines(xa, dbinom(xa, n, 0.5), type="h", lwd=7, col=grey(0.5))
      # Central location
        segments(mu, 0, mu, dnorm(mu, mu, sig), lty=2, col="red")
        text(mu, ymin/2, labels=mu, col="blue")

      # P-value and Critical value
        if (nalt==2) {
            lines(n1:n, dbinom(n1:n, n, 0.5),  type="h", col="red", lwd=7)
            text(n, ymin/2, labels=paste0("pv=", round(pv, 4)), col="blue", pos=2)
            text(n1, dbinom(n1, n, 0.5), labels=n1, col="red", pos=3)
        } else if (nalt==1) {
            lines(n2:n, dbinom(n2:n, n, 0.5),  type="h", col="red", lwd=7)
            text(n, ymin/2, labels=paste0("pv=", round(pv, 4)), col="blue", pos=2)
            text(n2, dbinom(n2, n, 0.5), labels=n2, col="red", pos=3)
        } else { # [Correction]
            if (n1 < n/2) xbase = c(0:n1, (n-n1):n) else xbase = c(0:(n-n1), n1:n)
            lines(xbase, dbinom(xbase, n, 0.5),  type="h", col="red", lwd=7)
            text(n, ymin/2, labels=paste0("pv1=", round(pv/2, 4)), col="blue", pos=2)
            text(0, ymin/2, labels=paste0("pv2=", round(pv/2, 4)), col="blue", pos=4)
            text(n1, dbinom(n1, n, 0.5), labels=n1, col="red", pos=3)
            text(n2, dbinom(n2, n, 0.5), labels=n2, col="red", pos=3)
        }
    }
}

# [15-3] Functions for the Runs Distribution
# n1 and n2 must be scalars, r can be a vector
# [15-3-A] PMF -------------------------------------------------
#' @title PMF of the Number of Runs
#' @description The Probabilityt Mass Function of the Number of Runs.
#' @param r Number of Runs (vector).
#' @param n1 Number of data in group 1 (scalar).
#' @param n2 Number of data in group 2 (scalar).
#' @return P(R=r).
#'
#' @examples
#' dwruns(2:10, 5, 5)
#' sum(dwruns(2:10, 5, 5))
#' dwruns(1:12, 5, 5)
#' sum(dwruns(1:12, 5, 5))
#'
#' dwruns(2:11, 5, 10)
#' sum(dwruns(2:11, 5, 10))
#' dwruns(1:13, 5, 10)
#' sum(dwruns(1:13, 5, 10))
#' @rdname dwruns
#' @export
dwruns <- function (r, n1, n2) {
      if (any(r != round(r))) stop("r should be an integer...")
      hr <- round(r/2)
      hr1 <- round((r-1)/2)
      hr3 <- round((r-3)/2)
      numer <- ifelse(r%%2==0, 2*choose(n1-1, hr-1)*choose(n2-1, hr-1), 
          choose(n1-1, hr1)*choose(n2-1, hr3) + choose(n1-1, hr3)*choose(n2-1, hr1) )
      pdf <- numer/choose(n1+n2, n1)
      return(pdf)
}

# [15-3-B] CDF: P(R<=r) or P(R>r) ----------------------------------------
#' @title CDF of the Number of Runs
#' @description The Cumulative Distribution Function of the Number of Runs.
#' @param r Number of Runs (vector).
#' @param n1 Number of data in group 1 (scalar).
#' @param n2 Number of data in group 2 (scalar).
#' @param lower Logical value for the lower tail, Default: TRUE.
#' @return P(R<=r) or P(R>r) when lower=FALSE.
#'
#' @examples
#' pwruns(1:12, 5, 5)
#' pwruns(1:12, 5, 5, lower=FALSE)
#' pwruns(1:12, 5, 5) + pwruns(1:12, 5, 5, lower=FALSE)
#'
#' pwruns(1:13, 5, 10)
#' pwruns(1:13, 5, 10, lower=FALSE)
#' pwruns(1:13, 5, 10) + pwruns(1:13, 5, 10, lower=FALSE)
#' @rdname pwruns
#' @export
pwruns <- function (r, n1, n2, lower=TRUE) {
    pmf <- dwruns(1:max(r), n1, n2)

    if (any(r != round(r))) r <- floor(r)
  # Consider r[k]=0
    r <- pmax(1, r)
    if (length(r)==1) {
        cdf <- sum(pmf[1:r])
    } else {
        vcdf <- cumsum(pmf[1:max(r)])
        cdf <-vcdf[r]
    }

    if (lower) {
        return(cdf)
    } else {
        return(1 - cdf)
    }
}

# [15-3-C] Calculate the p-values of runs test ---------------------------------
#' @title P-value of the Two Sample Runs Test
#' @description The P-value of the Two Sample Runs Test
#' @param r Number of Runs (vector).
#' @param n1 Number of data in group 1 (scalar).
#' @param n2 Number of data in group 2 (scalar).
#' @param alt Direction of the alternative hypothesis, Default: 'two'.
#' @return The p-value 
#'
#' @examples
#' runs.pval(1:12, 5, 5)
#' runs.pval(2:5, 5, 5) - runs.pval(10:7, 5, 5)
#' runs.pval(1:12, 5, 5, alt="le")
#' runs.pval(1:12, 5, 5, alt="gr")
#' runs.pval(2:10, 5, 5, alt="le") - runs.pval(10:2, 5, 5, alt="gr")
#'
#' runs.pval(1:13, 5, 10)
#' runs.pval(1:13, 5, 10, alt="le")
#' runs.pval(1:13, 5, 10, alt="gr")
#' @rdname runs.pval
#' @export
runs.pval <- function (r, n1, n2, alt ="two") {
  # Preliminary setting --------------
    if (n1 < 1 | n2 < 1) stop("Both n1 and n2 must be >= 1...")
    nalt <- alt.num(alt)
    rmax <- ifelse(n1==n2, 2*n1, 2*min(n1,n2)+1)
    Er <- 2*n1*n2/(n1+n2)+1

  # Probability mass function -----------
    rv <- 1:rmax
    pmf <- dwruns(rv, n1, n2)
    nn <- length(r)

  # p-value ------------------------------
    pval <- NULL
    if (nalt==1) {
        for (k in 1:nn) {
            if (r[k]<2 | r[k]>rmax) {
                pval <- c(pval, NA)
            } else {
                pval <- c(pval, sum(pmf[rv <= r[k]]))
            }
        }
    } else if (nalt==2) {
        for (k in 1:nn)  {
            if (r[k]<2 | r[k]>rmax) {
                pval <- c(pval, NA)
            } else {
                pval <- c(pval, sum(pmf[rv >= r[k]]))
            }
        }
    } else {
        for (k in 1:nn)   {
            if (r[k]<2 | r[k]>rmax) {
                pval <- c(pval, NA)
            } else {
                pval <- c(pval, sum(pmf[abs(rv - Er) >= abs(r[k] - Er)]))
            }
        }
    }
    return(pval)
}

# [15-3-D] Calculate the lower and upper sided critical values of runs test -----------
#' @title Critical Values of the Two Sample Runs Test
#' @description The Lower and Upper Sided Critical Values of the Two Sample Runs Test.
#' @param n1 Number of data in group 1 (scalar).
#' @param n2 Number of data in group 2 (scalar).
#' @param alpha The level of significance, Default: 0.05.
#' @return The lower and upper sided critical values.
#'
#' @examples
#' cwruns(5, 5)
#' cwruns(5, 5, alpha=0.025)
#'
#' cwruns(5, 10)
#' cwruns(5, 10, alpha=0.025)
#' @rdname cwruns
#' @export
cwruns <- function (n1, n2, alpha=0.05) {
    nmin <- min(n1, n2)
    nmax <- max(n1, n2)
    rmax <- ifelse(n1==n2, 2*n1, 2*min(n1,n2)+1)
    Er <- 2*n1*n2/(n1+n2)+1

  # CDF and Reverse CDF: P(R>=r)
    rv <- 1:rmax
    cdf <- pwruns(rv, n1, n2)
    rcdf <- pwruns(rv-1, n1, n2, lower=FALSE)

  # Lower critical value: Pr(R<=r) <= alpha
    pv1 <- cdf[2]
    if (pv1 > alpha) {
        crv1 <- NA
    } else {
        crv1 <- max(rv[cdf <= alpha])
    }
  # Upper critical value: Pr(R>=r) <= alpha
    pv2 <- rcdf[rmax]
    if (pv2 > alpha) {
        crv2 <- NA
    } else {
        crv2 <- min(rv[rcdf <= alpha])
    }
    return(c(crv1,crv2))
}

# [15-3] Distribution of Runs Test Statistic ------------------------------------
#' @title Plots and Tables of Runs Distribution
#' @description Plots and Tables of the Prbability Mass Function of Runs.
#' @param n1 Number of data in group 1, Default: 2:5.
#' @param n2 Number of data in group 2, Default: 2:5.
#' @return None.
#'
#' @examples
#' runs.dist(n1=c(5,20), n2=c(5,20))
#' runs.dist(n1=5, n2=2:10)
#' runs.dist(n1=1:9*5, n2=9:1*5)
#' runs.dist(n1=1:10*5, n2=10:1*5)
#' @rdname runs.dist
#' @export
runs.dist <- function(n1=2:5, n2=2:5, alp=0.05, ws=c(7,4)) {
    nn1 <- length(n1)
    nn2 <- length(n2)

  # Plot the Distribution of Runs Test Statistic
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(2, 2, 3, 2))

        mm <- max(nn1, nn2)
        if (mm>20) {
            warning("The maximum number of plots may not exceed 20...")
            mm <- 20
        }
      # Apply reuse rule
        if (nn1>nn2) n2 <- rep(n2, ceiling(nn1/nn2))[1:mm]
        if (nn1<nn2) n1 <- rep(n1, ceiling(nn2/nn1))[1:mm]

        wc <- c(1,2,3, 2,3,3, 4,4,3, 5, 4,4,5,5,5, 4,rep(5,4))
        wr <- c(1,1,1, rep(2,5), 3,  2, rep(3,5), rep(4,5))
        ww <- c(7,8,9,7, 9,9,12,12,9, 15, 12,12,15,15,15, 12,rep(15,4))
        wl <- c(5,4,3, rep(6,5), 9, 6, rep(9,5), rep(12,5))
        ##win.graph(ww[mm], wl[mm])
        par(mfrow=c(wr[mm], wc[mm]))

        for (k in 1:mm) {
            rmax <- ifelse(n1[k] == n2[k], 2*n1[k], 2*min(n1[k], n2[k]) + 1)
            x <- 2:rmax
            plot(x, dwruns(x,n1[k],n2[k]), type = "h", lwd=4, col="red", ylab="f(x)",
             main = paste0("Runs PDF (n1 = ", n1[k], ", n2 =", n2[k], ")"))
        }
    }
  # Critical value table of runs test
    if (is.numeric(alp)) {
        lcv <- ucv <- array(NA, dim=c(nn1, nn2))
        rownames(lcv) <- rownames(ucv) <- paste0("n1=",n1)
        colnames(lcv) <- colnames(ucv) <- n2
        colnames(lcv)[1] <- colnames(ucv)[1] <- paste0("n2=",n2[1])
      # Calculate critical values
        for (k1 in 1:nn1) {
            for (k2 in 1:nn2) {
                temp <- cwruns(n1[k1], n2[k2], alp)
                lcv[k1, k2] <- temp[1]
                ucv[k1, k2] <- temp[2]
            }
        }
      # Print the table of critical values
        if (nn1*nn2==1) {
            cat("[Lower Critical Value; alpha =", 
                paste0(100*alp, "%] ="), as.numeric(lcv), "\n")
            cat("[Upper Critical Value (alpha =", 
                paste0(100*alp, "%] ="), as.numeric(ucv), "\n")
        } else {
            cat("[Lower Critical Values (alpha =", 
                paste0(100*alp, "%]"), "__________________\n")
            print(lcv)
            cat("[Upper Critical Values (alpha =", 
                paste0(100*alp, "%]"), "__________________\n")
            print(ucv)
        }
    invisible(list(lcv=lcv, ucv=ucv))
    }
}

# [15-3-2] Critical Values of Runs Test ------------------------------------
#' @title Critical Values of Runs Test
#' @description Lower and Upper Critical Values of Runs Test
#' @param n1 Number of data in group 1, Default: 2:5
#' @param n2 Number of data in group 2, Default: 2:5
#' @param alp Level of significance, Default: 0.05
#' @return None.
#'
#' @examples
#' runs.table(n1=10, n2=10)
#' runs.table(n1=2:10, n2=2:10)
#' runs.table(n1=c(5,20), n2=c(5,20))
#' @rdname runs.table
#' @export
runs.table <- function(n1=2:5, n2=2:5, alp=0.05) {
    # Critical value table of runs test
    nn1 <- length(n1)
    nn2 <- length(n2)
    lcv <- ucv <- array(NA, dim=c(nn1, nn2))
    rownames(lcv) <- rownames(ucv) <- paste0("n1=",n1)
    colnames(lcv) <- colnames(ucv) <- n2
    colnames(lcv)[1] <- colnames(ucv)[1] <- paste0("n2=",n2[1])
    # Calculate critical values
    for (k1 in 1:nn1) {
        for (k2 in 1:nn2) {
            temp <- cwruns(n1[k1], n2[k2], alp)
            lcv[k1, k2] <- temp[1]
            ucv[k1, k2] <- temp[2]
        }
    }

    # Print the table of critical values
    if (nn1*nn2==1) {
        cat("One-sided Lower Critical Value (alpha =", 
            paste0(100*alp, "%) ="), as.numeric(lcv), "\n")
        cat("One-sided Upper Critical Value (alpha =", 
            paste0(100*alp, "%) ="), as.numeric(ucv), "\n")
    } else {
        cat("One-sided Lower Critical Value (alpha =", 
            paste0(100*alp, "%)"), "__________________\n")
        print(lcv)
        cat("One-sided Upper Critical Value (alpha =", 
            paste0(100*alp, "%)"), "__________________\n")
        print(ucv)
    }
}

# [15-4] Runs Test with a Plot
#' @title Runs Test
#' @description Runs Test with a Plot.
#' @param x Data vector (or number of runs).
#' @param n1 Number of data in group 1 (without raw data).
#' @param n2 Number of data in group 2 (without raw data).
#' @param alp Level of significance, Default: 0.05.
#' @param alt Type of alternative hypothesis, Default: 'two'.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return None.
#'
#' @examples
#' x <- c(1,1,0,0,1,0,rep(1,7), rep(0,7))
#' runs.tsp(x)
#' runs.tsp(x, alt="le")
#'
#' set.seed(1234); x <- rbinom(50, 1, 0.5)
#' runs.tsp(x)
#'
#' x <- rep(0, 50)
#' x[c(1:2, 8:10, 17:18, 21, 26:27, 29:31, 36:37, 41:44, 49)] <- 1
#' runs.tsp(x)
#' @rdname runs.tsp
#' @export
runs.tsp <- function(x, n1, n2, alp=0.05, alt="two", dig=4, ws=c(7,4)) {
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Check the data
    nuniq <- length(unique(x))
    if (nuniq > 2) {
        dx <- diff(x)
        dx <- dx[dx != 0]
        x <- as.numeric(dx < 0)
    }
  # Calculate the number of runs
    nn <- length(x)
    if (nn>1) {
        dval <- sort(unique(x))
        n1 <- sum(x==dval[1])
        n2 <- sum(x==dval[2])
        r1 <- length(rle(x)[[1]])
    } else r1 <- x

  # Normal approximation
    mu <- 2*n1*n2/(n1+n2)+1
    vr <- 2*n1*n2*(2*n1*n2-n1-n2)/(n1+n2)^2/(n1+n2-1)
  # Calculate p-value for the type of alternative hypothesis
    pv1 <- pwruns(r1, n1, n2)
    pv2 <- 1-pwruns(r1-1, n1, n2)

  # Runs test
    nalt <- alt.num(alt)
    if (nalt==1) {
        h1 <- "Lower-sided"
        area <- 2:r1
        xpt <- r1
        pos <- 2
        pv <- pv1
        h2 <- paste0("Pr(R\U2264", r1, ") = ", myf1(pv))
        z0 <- (r1+0.5-mu)/sqrt(vr)
        pv2 <- pnorm(z0)
        h3 <- paste0("Pr(Z<", myf1(z0), ") = ", myf1(pv2))
        ppv <- pv
    } else if (nalt==2) {
        h1 <- "Upper-sided"
        area <- r1:(n1+n2)
        xpt <- r1
        pos <- 4
        pv <- pv2
        h2 <- paste0("Pr(R\U2265", r1, ") = ", myf1(pv))
        z0 <- (r1-0.5-mu)/sqrt(vr)
        pv2 <- 1 - pnorm(z0)
        h3 <- paste0("Pr(Z>", myf1(z0), ") = ", myf1(pv2))
        ppv <- pv
    } else {
        h1 <- "Two-sided"
        r2 <- mu + (mu-r1)
        ir1 <- floor(min(r1, r2))
        ir2 <- ceiling(max(r1, r2))
        area <- c(2:ir1, ir2:(n1+n2))
        xpt <- c(ir1, ir2)
        pos <- c(2, 4)
        plow <- pwruns(ir1, n1, n2)
        pupp <- 1-pwruns(ir2-1, n1, n2)
        pv <- plow + pupp
        h2 <- paste0("Pr(R\U2264", ir1, ")+Pr(R\U2265", ir2, ") = ", 
            myf1(plow), "+", myf1(pupp), " = ", myf1(pv))
        z0 <- ifelse(r1<mu, (r1+0.5-mu)/sqrt(vr), (r1-0.5-mu)/sqrt(vr))
        pv2 <- 2*pnorm(-abs(z0))
        h3 <- paste0("Pr(|Z|>", myf1(abs(z0)), ") = ", myf1(pv2))
        ppv <- c(plow, pupp)
    }

  # Print test results -------------------------------
    cat(paste0("[Runs Test;  ", h1, "]"), "------------------------------------\n")
    cat(paste0("n1=", n1, ", n2=", n2, ", R=", r1, "  \U21D2  ", h2), "\n")
    cat("[Normal Approximation (Continuity Correction)]\n")
    cat(paste0("E(R)=", round(mu, dig), ", Var(R)=", round(vr, dig), 
        " \U21D2 Z0 = ", myf1(z0), " \U21D2 ", h3), "\n")

    # Plot distribution
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        xa <- 2:(n1+n2)
        mt <- paste0("Runs Test (n1=", n1, ",  n2=", n2, ") : ", h1)
        ya <- dwruns(xa, n1, n2)
        ymax <- max(ya, dnorm(mu, mu, sqrt(vr)))

        plot(xa, ya, type="h", lwd=5, ylab="f(r)", xlab="Number of Runs",
            ylim=c(0, ymax), main=mt, col=grey(0.5))
        xa2 <- seq(2, n1+n2, length=100)
        lines(xa2, dnorm(xa2, mu, sqrt(vr)), lty=1, col="green4")
        lines(area, dwruns(area, n1, n2), type="h", lwd=5, col="red")
        text(xpt, dwruns(r1,n1,n2), labels=round(ppv, 4), col="blue", pos=pos)
    }
}

# [15-5-A] Test for Correlation with Summary Statistics
cortest.sum <- function(rxy, nn, r0=0, alt="two", prt=TRUE, alp=0.05, dig=4) {
    # Check input
    if (missing(rxy)) stop("Sample correlation coefficient is required.")
    if (missing(nn)) stop("The sample size is required.")
    nalt <- alt.num(alt)

    #cat("[1] Test for Correlation Coefficient _________________________________\n")
    if (r0==0) {
        T0 <- rxy*sqrt((nn-2)/(1-rxy^2))
        if (nalt==3) {
            pv0 <- 2*pt(-abs(T0), nn-2)
        } else if (nalt==1) {
            pv0 <- pt(T0, nn-2)
        } else if (nalt==2) {
            pv0 <- 1 - pt(T0, nn-2)
        }
        otest <- c(r0, rxy, T0, pv0)
        names(otest) <- c("rho0", "r_xy", "T_0", "p-val")
    } else {
        Z0 <- sqrt(nn-3)*0.5*(log((1+rxy)/(1-rxy))-log((1+r0)/(1-r0)))
        if (nalt==3) {
            pv0 <- 2*pnorm(-abs(Z0))
        } else if (nalt==1) {
            pv0 <- pnorm(Z0)
        } else if (nalt==2) {
            pv0 <- 1 - pnorm(Z0)
        }
        otest <- c(r0, rxy, Z0, pv0)
        names(otest) <- c("rho0", "r_xy", "Z_0", "p-val")
    }
    if (prt && r0==0) {
        cat(paste0("T-stat = ", format(rxy, F, dig), " \U00D7 \U221A(", nn-2,
            "/(1-", format(abs(rxy), F, dig), "\U00B2)) = ", round(T0, dig), "\n"))
        if (nalt==3) {
            cat(paste0("p-value = P(|T(", nn-2, ")| > ", round(abs(T0), dig), ") = ",
                format(pv0, F, dig), "\n"))
        } else if (nalt==1) {
            cat(paste0("p-value = P(T(", nn-2, ") < ", round(T0, dig), ") = ",
                format(pv0, F, dig), "\n"))
        } else if (nalt==2) {
            cat(paste0("p-value = P(T(", nn-2, ") > ", round(T0, dig), ") = ",
                format(pv0, F, dig), "\n"))
        }
    } else if (prt && r0!=0) {
        cat(paste0("Z-stat = \U221A", nn-3, "\U00D7", "0.5", "\U00D7[log(",
            round(1+rxy, dig), "/", round(1-rxy, dig),
            ")-log(", 1+r0, "/", 1-r0, ")] = ", round(Z0, dig), "\n"))
        if (nalt==3) {
            cat(paste0("p-value = P(|Z| > ", round(Z0, dig), ") = ",
                format(pv0, F, dig), "\n"))
        } else if (nalt==1) {
            cat(paste0("p-value = P(Z < ", round(Z0, dig), ") = ",
                format(pv0, F, dig), "\n"))
        } else if (nalt==2) {
            cat(paste0("p-value = P(Z > ", round(Z0, dig), ") = ",
                format(pv0, F, dig), "\n"))
        }
    }
    invisible(otest)
}

# [15-5] Pearson Correlation Coefficient & Spearman Correlation Coefficient
#' @title Pearson & Spearman Correlation Coefficient
#' @description Pearson Correlation Coefficient & Spearman Correlation Coefficient.
#' @param x Data of the first variable.
#' @param y Data of the second variable.
#' @param r0 Correlation coefficient value under H0, Default: 0.
#' @param alp Level of significance, Default: 0.05.
#' @param alt Type alternative hypothesis, Default: 'two'.
#' @param dig Number of decimal places, Default: 4.
#' @param type Calculation method: one of ("s", "p", "b"), Default: "s".
#' @param ws Graphic window size, Default: c(8,4).
#' @return Test statistics.
#'
#' @examples
#' x <- c(10,7,0,1,5, 2,8,6,4,9, 3,0,2,4,6, 8)
#' y <- c(75,77,91,64,79, 81,90,86,82,76, 89,93,80,87,83, 78)
#' corr.tsp(x, y)
#' corr.tsp(x, y, alt="le")
#' @rdname corr.tsp
#' @export
corr.tsp <- function(x, y, r0=0, alp=0.05, alt="two", type="s", dig=4, ws=c(8,4)) {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
  # Labels & simple regression
    xl <- deparse(substitute(x))
    yl <- deparse(substitute(y))
    nn <- length(x)
    lm1 <- lm(y ~ x)

  # Pearson correlation coefficient by cor( ) function
    Sxx <- sum(x^2)-sum(x)^2/nn
    Syy <- sum(y^2)-sum(y)^2/nn
    Sxy <- sum(x*y)-sum(x)*sum(y)/nn
    rxy <- Sxy/sqrt(Sxx*Syy)
    pcor <- cortest.sum(rxy, nn, r0, alt, FALSE, alp, dig)

  # Spearman correlation coefficient -----------------
    x2 <- rank(x)
    y2 <- rank(y)
    lm2 <- lm(y2 ~ x2)
    Sxx2 <- sum(x2^2)-sum(x2)^2/nn
    Syy2 <- sum(y2^2)-sum(y2)^2/nn
    Sxy2 <- sum(x2*y2)-sum(x2)*sum(y2)/nn
    rxy2 <- Sxy2/sqrt(Sxx2*Syy2)
    scor <- cortest.sum(rxy2, nn, r0, alt, FALSE, alp, dig)

  # Print
    if (type=="p" || type=="b") {
        cat("[Pearson Correlation Coefficient] ___________________________\n")
        cat(paste0("Sxx = ", myf0(sum(x^2)), " - ", myf0(sum(x)), 
            "\U00B2 / ", nn, " = ", myf1(Sxx)), "\n")
        cat(paste0("Syy = ", myf0(sum(y^2)), " - ", myf0(sum(y)), 
            "\U00B2 / ", nn, " = ", myf1(Syy)), "\n")
        cat(paste0("Sxy = ", myf0(sum(x*y)), " - (", myf0(sum(x)), 
            " \U00D7 ", myf0(sum(y)), ") / ", nn, " = ", myf1(Sxy)), "\n")
        cat(paste0("Corr(x,y) = ", myf0(Sxy), " / \U221A(",
            myf0(Sxx), " \U00D7 ", myf0(Syy), ") = ", myf1(rxy)), "\n")
      # Correlation test by cortest.sum( ) function
        cortest.sum(rxy, nn, r0, alt, TRUE, alp, dig)
        if (type=="p") {
            cat(paste0("[Spearman] Corr = ", myf1(rxy2), ", T0 = ",
                myf1(scor[3]), ", p-value = ", myf1(scor[4])), "\n")
        }
    }


    if (type=="s" || type=="b") {
    cat("[Spearman correlation coefficient] ______________________________\n")
        cat(paste0("Srx.rx = ", myf0(sum(x2^2)), " - ", myf0(sum(x2)), 
            "\U00B2 / ", nn, " = ", myf1(Sxx2)), "\n")
        cat(paste0("Sry.ry = ", myf0(sum(y2^2)), " - ", myf0(sum(y2)), 
            "\U00B2 / ", nn, " = ", myf1(Syy2)), "\n")
        cat(paste0("Srx.ry = ", myf0(sum(x2*y2)), " - (", myf0(sum(x2)), 
            " \U00D7 ", myf0(sum(y2)), ") / ", nn, " = ", myf1(Sxy2)), "\n")
        cat(paste0("Corr(rx,ry) = ", myf0(Sxy2), " / \U221A(",
            myf0(Sxx2), " \U00D7 ", myf0(Syy2), ") = ", myf1(rxy2)), "\n")
      # Correlation test by cortest.sum( ) function
        cortest.sum(rxy2, nn, r0, alt, TRUE, alp, dig)
        if (type=="s") {
            cat(paste0("[Pearson] Corr = ", myf1(rxy), ", T0 = ",
                myf1(pcor[3]), ", p-value = ", myf1(pcor[4])), "\n")
        }
    }

  # Scatter plot -----------------------------------------
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        #cat("[P] Scatter Plots\n")
        mt <- paste(yl, "vs.", xl)
 
        par(mfrow=c(1,2))
        y11 <- floor(min(y, lm1$fit))
        y12 <- ceiling(max(y, lm1$fit))
        plot(x, y, pch=19, main=mt, xlab=xl, ylab=yl, ylim=c(y11, y12))
        grid(col="green")
        abline(lm1, col="red")

        mt2 <- paste0("r(", yl, ") vs. r(", xl, ")")
        y21 <- floor(min(y2, lm2$fit))
        y22 <- ceiling(max(y2, lm2$fit))
        plot(x2, y2, pch=19, main=mt2, xlab=paste0("r(", xl, ")"), 
            ylab=paste0("r(", yl, ")"), ylim=c(y21, y22))
        grid(col="green")
        abline(lm2, col="red")
    }
    invisible(list(pcor=pcor, scor=scor))
}

# [15-6] Distribution of Wilcoxon-Mann-Whitney Rank-Sum Test Statistic
#' @title Distribution of Rank-Sum Test Statistic
#' @description Distribution of Wilcoxon-Mann-Whitney Rank-Sum Test Statistic
#' @param n1 Number of data in group 1, Default: 1:2.
#' @param n2 Number of data in group 2, Default: 2.
#' @param alp Level of significance, Default: "n"
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return None.
#'
#' @examples
#' ranksum.dist(n1=1:3, n2=3:5, ws="n")
#' ranksum.dist(n1=c(2,4,6,8), n2=10, alp=0.05)
#' @rdname ranksum.dist
#' @export
ranksum.dist <- function(n1=1:2, n2=2, alp="n", dig=4, ws=c(7,4)) {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Distribution table of W-M-W rank-sum test statistic
    # [Correct-2] ---------------------------------
    m1 <- length(n1)
    m2 <- length(n2)
    # pwilcox( ) function --> cumulative distribution
    for (k2 in n2) { 
        nu <- k2*max(n1)
        pv <- array(NA, dim=c(nu+1, m1))
        colnames(pv) <- paste0("n1=", n1)
        rownames(pv) <- paste0("U=", 0:nu)
        ii <- 0
        for (k1 in n1) {
            ii <- ii+1
            pv[, ii] <- pwilcox(0:nu, k1, k2)
        }
        ##cat(paste0("[n2 = ", k2, "]"), "______________________________\n")
        ##print(myf0(pv))
    }

  # Quantile table of W-M-W rank-sum test statistic
    if (is.numeric(alp)) {
        qtab <- NULL
      # qwilcox( ) function --> quantiles
        for (k2 in n2) { 
            qtab <- rbind(qtab, qwilcox(alp, n1, k2))
        }
        colnames(qtab) <- paste0("n1=", n1)
        rownames(qtab) <- paste0("n2=", n2)
        cat(paste0("[Critical Values; alp=", alp, "]"), "_________________\n")
        print(qtab)
    } # End of table

    # Plot the distribution
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))

        # if (missing(n1)) stop("Input for n1 is required for plotting...")
        # if (missing(n2)) stop("Input for n2 is required for plotting...")
        nn = max(length(n1), length(n2))
        if (length(n1)==1) n1 <- rep(n1, nn)
        if (length(n2)==1) n2 <- rep(n2, nn)

        if (nn<6) {
            dcol <- c("black", "red", "green4", "blue", "purple", "pink")
        } else {
            dcol <- rainbow(nn)
        }
        lab <- rep("", nn)

        xa <- 0:(max(n1)*max(n2))
        plot(xa, dwilcox(xa, min(n1), min(n2)), type="n", 
            main="Distribution of W-M-W Rank-Sum Statistic",
            lwd=2, xlab="Rank-Sum Statistic (U)", ylab="f(u)")
        for (k in 1:nn) {
            lines(xa, dwilcox(xa, n1[k], n2[k]), type="s", lwd=2, col=dcol[k])
            lab[k] <- paste0("(n1,n2)=(", n1[k], ",", n2[k],")")
        }
        if (nn<=10) legend("topright", lab, lwd=2, col=dcol[1:nn], text.col=dcol[1:nn])
    } # End of plot
    out <- pv
    if (is.numeric(alp)) out <- list(cv=qtab, pv=pv)
    invisible(out)
}

# [15-7] Wilcoxon-Mann-Whitney Rank-Sum Test
#' @title Wilcoxon-Mann-Whitney Rank-Sum Test
#' @description Wilcoxon-Mann-Whitney Rank-Sum Test with a Plot.
#' @param x Data of the first group.
#' @param y Data of the second group.
#' @param alt Type of alternative hypothesis, Default: 'two'.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return None.
#'
#' @examples
#' x <- c(1,5,7,8,8,8,9)
#' y <- c(2,8,8,8,8,9,9,9,9,10,10,10,10)
#' ranksum.tsp(x, y)
#' ranksum.tsp(y, x)
#' @rdname ranksum.tsp
#' @export
ranksum.tsp <- function(x, y, alt="two", dig=4, ws=c(7,4)) {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Test statistic
    n1 <- length(x)
    n2 <- length(y)
    R1 <- sum(rank(c(x,y))[1:n1])
    R2 <- sum(rank(c(x,y))[(n1+1):(n1+n2)])
    U1 <- R1 - n1*(n1+1)/2
    U2 <- R2 - n2*(n2+1)/2
    pv1 <- pwilcox(U2, n1, n2)
    pv2 <- pwilcox(U1, n1, n2)
    U <- min(U1, U2)
    pv <- 2*pwilcox(U, n1, n2)

  # Normal approximation method (without continuity correction)
    mu <- n1*n2/2
    va <- n1*n2*(n1+n2+1)/12
    sig <- sqrt(va)

  # P-value
    nalt <- alt.num(alt)
    if (nalt==2) {
        h1 <- "Upper-sided"
        pv <- pv1
        apv <- pnorm(U2, mu, sig)
        Z0 <- (U2 - mu)/sig
        h2 <- paste0("Pr(U\U2264", U2, ")=", myf1(pv))
        h3 <- paste0("Pr(Z<", round(Z0, dig), ")=", myf1(apv))
    } else if (nalt==1) {
        h1 <- "Lower-sided"
        pv <- pv2
        apv <- pnorm(U1, mu, sig)
        Z0 <- (U1 - mu)/sig
        h2 <- paste0("Pr(R\U2264", U1, ")=", myf1(pv))
        h3 <- paste0("Pr(Z<", round(Z0, dig), ")=", myf1(apv))
    } else {
        h1 <- "Two-sided"
        pv <- 2*min(pv1, pv2)
        apv <- 2*pnorm(U, mu, sig)
        Z0 <- (U - mu)/sig
        h2 <- paste0("2\U00D7", "Pr(U\U2264", U, ")=", myf1(pv))
        h3 <- paste0("Pr(|Z|<", round(Z0, dig), ")=", myf1(apv))
    }

  # Print test results -------------------------------
    cat(paste0("[Wilcoxon-Mann-Whitney Rank-Sum Test; ", h1, "]"), "----------------\n")
    cat(paste0("n1=",n1,", n2=",n2,", R1=",R1,", R2=",R2, " \U21D2 U1=",
               U1,", U2=",U2, " \U21D2 U=", U, " \U21D2 ", h2), "\n")
    cat("[Normal Approximation (Continuity Correction)]\n")
    cat(paste0("E(U)=", round(mu, dig), ", Var(U)=", round(va, dig), 
        " \U21D2 Z0=", myf1(Z0), " \U21D2 ", h3), "\n")

  # Plot the probability distribution of test statistic ----------------------
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
      # [Correction] xmax = ((n1+n2)*(n1+n2+1)/2 - n1*(n1+1)/2)/2
        xmax <- n1*n2
        xa <- 0:xmax
        xca <- (0:(10*xmax))/10
        pdf <- dwilcox(xa, n1, n2)
        ymax <- max(pdf)*1.05
        ymin <- -0.1*max(pdf)
      # Empty plot
        xl <- "Rank-Sum Statistic (U)"

        plot(xa, pdf, type="n", xlab=xl, ylab="f(u)", ylim=c(ymin, ymax),
            main=paste0("W-M-W Rank-Sum Test (n1=", n1, ", n2=", n2,")"))
      # Probability distribution function
        lines(xa, dwilcox(xa, n1, n2), type="h", lwd=3, col=grey(0.5))
      # Central location
        segments(mu, 0, mu, dnorm(mu, mu, sig), lty=2, col="red")
        text(mu, ymin/2, labels=mu, col="blue")

      # Critical region and p-value
        if (nalt==2) {
          # Upper critical region (one-sided)
            text(U1, dwilcox(U1, n1, n2), labels=U1, col="blue", pos=3)
            lines(U1:xmax, dwilcox(U1:xmax, n1, n2),  type="h", col="red", lwd=3)
            text((U1+xmax)/2, ymin/2, labels=round(pv, dig), col="red")
        } else if (nalt==1) {
          # Lower critical region (one-sided)
            text(U1, dwilcox(U1, n1, n2), labels=U1, col="blue", pos=3)
            lines(0:U1, dwilcox(0:U1, n1, n2),  type="h", col="red", lwd=3)
            text(U1/2, ymin/2, labels=round(pv, dig), col="red")
        } else { # [Correction]
            Umin <- min(U1, U2)
            Umax <- max(U1, U2)
            xbase <- c(0:Umin, Umax:xmax)
            lines(xbase, dwilcox(xbase, n1, n2),  type="h", col="red", lwd=3)

          # Lower critical region (two-sided)
            text(Umin, dwilcox(Umin, n1, n2), labels=Umin, col="blue", pos=3)
          # lines(0:U1, dwilcox(0:U1, n1, n2),  type="h", col="red", lwd=3)
            text(Umin/2, ymin/2, labels=round(pv/2, dig), col="red")
          # Upper critical region (two-sided)
            text(Umax, dwilcox(Umax, n1, n2), labels=Umax, col="blue", pos=3)
          # lines(U2:xmax, dwilcox(U2:xmax, n1, n2),  type="h", col="red", lwd=3)
            text((Umax+xmax)/2, ymin/2, labels=round(pv/2, dig), col="red")
        }
        # Normal density
        lines(xca, dnorm(xca, mu, sig), col="green4")
        abline(h=0)
    } # End of plot
}

# [15-8] Plots and Tables of Wilcoxon Signed-Rank Distribution
#' @title Plots and Tables of Wilcoxon Signed-Rank Distribution
#' @description Plots and Tables of the Distribution of Wilcoxon Signed-Rank Test Statistic.
#' @param nv Number of paired samples, Default: 5:10.
#' @param alp Level of sinificance.
#' @param ws Graphic window size, Default: c(7,4).
#' @param sep Logical: plot in separate facets? Default: FALSE.
#' @return None.
#' 
#' @examples
#' signrank.dist(nv=5:15)
#' signrank.dist(nv=c(5,7,10,20))
#' # Quantile tables
#' signrank.dist(nv=5:15, pp=c(0.95, 0.975))
#' signrank.dist(nv=c(5,7,10,20), pp=c(0.95, 0.975, 0.99, 0.995))
#' @rdname signrank.dist
#' @export
signrank.dist <- function(nv=5:10, alp, ws=c(7,4), sep=FALSE, pp) {

    nn <- length(nv)
    nmax <- max(nv)
  # Plot distribution
    if (sep) {
        # Set graphic window
        wc <- c(1,2,3, 2,3,3, 4,4,3, 5, 4,4,5,5,5, 4,rep(5,4))
        wr <- c(1,1,1, rep(2,5), 3,  2, rep(3,5), rep(4,5))
        ww <- c(7,8,9,7, 9,9,12,12,9, 15, 12,12,15,15,15, 12,rep(15,4))
        wl <- c(5,4,3, rep(6,5), 9, 6, rep(9,5), rep(12,5))
        win.graph(ww[nn], wl[nn])
        par(mfrow=c(wr[nn], wc[nn]))
        par(mar=c(4, 4, 4, 2))

        # Separate Plot
        for(n in nv) {
            mu <- n*(n+1)/4
            vw <- n*(n+1)*(2*n+1)/24
            dw <- sqrt(vw)
            xa <- 0:(n*(n+1)/2)
            ymax <- max(dsignrank(xa, n = n), dnorm(mu, mu, dw))
            plot(xa, dsignrank(xa, n = n), type = "h", lwd=2, col="red", ylab="f(w)", xlab="w",
                 ylim=c(0, ymax), main = paste0("W Signed-Rank (n = ", n, ")"))
            xa2 <- seq(0, mu*2, length=100)
            lines(xa2, dnorm(xa2, mu, dw), col="blue")
        }
    } else if (is.numeric(ws)) {
      # Unified Plot
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))

        if (nn<=6) {
            dcol <- c("black", "red", "green4", "blue", "purple", "magenta")
        } else {
            dcol <- rainbow(nn)
        }
        lab <- rep("", nn)

        xa <- 0:0:(nmax*(nmax+1)/2)
        plot(xa, dsignrank(xa, n=nv[1]), type="n", 
            main="Distribution of Wilcoxon Signed-Rank Statistic",
            lwd=2, xlab="Signed-Rank Statistic (W)", ylab="f(w)")
        for (k in 1:nn) {
            lines(xa, dsignrank(xa, n=nv[k]), type="s", lwd=2, col=dcol[k])
            lab[k] = paste0("n=", nv[k])
        }
        if (nn<=10) legend("topright", lab, lwd=2, col=dcol[1:nn], text.col=dcol[1:nn])
    } # End of sep
    if (!missing(pp)) { # pp <- c(0.95, 0.975, 0.99, 0.995)
      # Quantiles of Wilcoxn signed-rank test statistic
        nn <- length(nv)
        npp <- length(pp)
      # qsignrank( ) function --> quantile table
        qv <- array(NA, dim=c(nn, npp))
        colnames(qv) <- pp
        rownames(qv) <- paste0("n=", nv)
        for (i in 1:npp) qv[, i] <- qsignrank(pp[i], nv)
        cat("[Quantiles of Wilcoxon Signed-Rank Test Statistic] _______________\n")
        print(qv)
    }
    if (!missing(alp)) {
      # Quantiles of Wilcoxn signed-rank test statistic
        nn <- length(nv)
        nalp <- length(alp)
      # qsignrank( ) function --> quantile table
        cv <- array(NA, dim=c(nalp, nn))
        colnames(cv) <- paste0("n=", nv)
        rownames(cv) <- paste0("alp=", alp)
        for (i in 1:nalp) {
            cv[i, ] <- qsignrank(alp[i], nv)
            for (j in 1:nn) {
                if (psignrank(cv[i,j], nv[j]) > alp[i]) cv[i,j] <- cv[i,j] - 1
             }
        }
        cat("[Critical Values of Wilcoxon Signed-Rank Test] _______________\n")
        print(cv)
    }
}

# [15-8-2] Quantiles of Wilcoxon Signed-Rank Test Statistic
#' @title Quantiles of Wilcoxon Signed-Rank Test Statistic
#' @description Table of the Quantiles of Wilcoxon Signed-Rank Test Statistic
#' @param nv Number of signed data, Default: 5:10
#' @param pp Probability vector, Default: c(0.95, 0.975, 0.99, 0.995))
#' @return None.
#'
#' @examples
#' signrank.table(nv=5:15)
#' pp <- c(0.005, 0.01, 0.025, 0.05, 0.95, 0.975, 0.99, 0.995)
#' signrank.table(nv=5:15, pp=pp)
#' signrank.table(nv=c(5,7,10,20))
#' @rdname signrank.table
#' @export
signrank.table <- function(nv=5:10, pp) {
    if (missing(pp)) pp <- c(0.95, 0.975, 0.99, 0.995)
    # Quantiles of Wilcoxn signed-rank test statistic
    nn <- length(nv)
    npp <- length(pp)
    # qsignrank( ) function --> quantile table
    cv <- array(NA, dim=c(nn, npp))
    colnames(cv) <- pp
    rownames(cv) <- paste0("n=", nv)
    for (i in 1:npp) cv[, i] <- qsignrank(pp[i], nv)
    cat("Quantiles of Wilcoxon Signed-Rank Test Statistic _______________\n")
    print(cv)
}

# [15-9] Wilcoxon Signed-Rank Test
#' @title Wilcoxon Signed-Rank Test
#' @description Wilcoxon Signed-Rank Test with a Plot
#' @param x Data of the first group.
#' @param y Data of the second group.
#' @param mu0 Mean difference under H0, Default: 0.
#' @param alt Type of alternative hypothesis, Default: 'two'.
#' @param dig Number of decimal places, Default: 4.
#' @param plot Graphic window size, Default: "n".
#' @return None.
#'
#' @examples
#' x <- c(38, 26, 34, 5, 68, 30, 35, 19, 33, 69)
#' y <- c(28, 21, 31, 11, 59, 28, 28, 23, 32, 38)
#' d <- x-y; shapiro.test(d)
#' signrank.tsp(x, y, alt="gr", ws=c(7,4))
#' signrank.tsp(y, x, alt="le", ws=c(7,4))
#' @rdname signrank.tsp
#' @export
signrank.tsp <- function(x, y, mu0=0, alt="two", dig=4, ws="n") {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Test statistic
    d <- x-y-mu0
    d <- d[d !=0]
    n <- length(d)
    rv <- rank(abs(d))
    w1 <- sum(rv[d>0])
    w2 <- sum(rv[d<0])
    pv1 <- psignrank(w2, n)
    pv2 <- psignrank(w1, n)

  # Normal approximation method (continuity correction)
    mu <- n*(n+1)/4
    va <- n*(n+1)*(2*n+1)/24
    sig <- sqrt(va)
    apv1 <- pnorm(w2+0.5, mu, sig)
    apv2 <- pnorm(w1+0.5, mu, sig)

  # P-value
    nalt <- alt.num(alt)
    if (nalt==2) {
        pv <- pv1
        apv <- apv1
        Z0 <- (w2+0.5-mu)/sig
        h1 <- "Upper-sided"
        h2 <- paste0("Pr(W\U2264", w2, ")=", myf1(pv))
        h3 <- paste0("Pr(Z<", round(Z0, dig), ")=", myf1(apv))
    } else if (nalt==1) {
        pv <- pv2
        apv <- apv2
        Z0 <- (w1+0.5-mu)/sig
        h1 <- "Lower-sided"
        h2 <- paste0("Pr(W\U2264", w1, ")=", myf1(pv))
        h3 <- paste0("Pr(Z<", round(Z0, dig), ")=", myf1(apv))
    } else {
        pv <- 2*min(pv1, pv2)
        apv <- 2*min(apv1, apv2)
        cat(paste0("W-stat=",min(w1, w2)), "\t ")
        Z0 <- (min(w1,w2)+0.5-mu)/sig
        h1 <- "Two-sided"
        h2 <- paste0("2\U00D7","Pr(W\U2264", min(w1,w2), ")=", myf1(pv))
        h3 <- paste0("Pr(|Z|>", round(Z0, dig), ")=", myf1(apv))
    }

    # Print test results -------------------------------
    cat(paste0("[Wilcoxon Signed-Rank Test; ", h1, "]"), "----------------\n")
    cat(paste0("n0=", length(x), ", n=", n, "; W(+)=", w1, ", W(-)=", w2, 
               " \U21D2 ", h2), "\n")
    cat("[Normal Approximation (Continuity Correction)]\n")
    cat(paste0("E(W)=", round(mu, dig), ", Var(W)=", round(va, dig), 
        " \U21D2 Z0 =", myf1(Z0), " \U21D2 ", h3), "\n")

    # Plot distribution of test statistic
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))

        xmax <- n*(n+1)/2
        xa <- 0:xmax
        xca <- (0:(10*xmax))/10
        pdf <- dsignrank(xa, n)
        ymax <- max(pdf)*1.05
        ymin <- -0.1*max(pdf)

      # Empty plot
        xlab <- "Signed-Rank Statistic" 

        plot(xa, pdf, type="n", xlab=xlab, ylab="f(u)", ylim=c(ymin, ymax),
            main=paste0("Wilcoxon Signed-Rank Test (n=", n, ")"))
      # Normal approximation
        lines(xca, dnorm(xca, mu, sig), col="green4")
        abline(h=0)
      # Distribution function of test statistic
        lines(xa, pdf, type="h", lwd=3, col=grey(0.6))
      # Central location
        segments(mu, 0, mu, dnorm(mu, mu, sig), lty=2, col="red")
        text(mu, ymin/2, labels=mu, col="blue")
      # Critical region
        if (nalt==2) {
          # upper critical region Display
            text(w1, dsignrank(w1, n), labels=w1, col="blue", pos=3)
            lines(w1:xmax, dsignrank(w1:xmax, n),  type="h", col="red", lwd=3)
            text((w1+xmax)/2, ymin/2, labels=round(pv, dig), col="red")
        } else if (nalt==1) {
          # lower critical region Display
            text(w1, dsignrank(w1, n), labels=w1, col="blue", pos=3)
            lines(0:w1, dsignrank(0:w1, n),  type="h", col="red", lwd=3)
            text(w1/2, ymin/2, labels=myf1(pv), col="red")
        } else {
          # two-sided critical region Display
            wmin <- min(w1,w2)
            wmax <- max(w1,w2)
            text(wmin, dsignrank(wmin, n), labels=wmin, col="blue", pos=3)
            lines(0:wmin, dsignrank(0:wmin, n),  type="h", col="red", lwd=3)
            text(wmin/2, ymin/2, labels=myf1(pv/2), col="red")
            # upper critical region Display
            text(wmax, dsignrank(wmax, n), labels=wmax, col="blue", pos=3)
            lines(wmax:xmax, dsignrank(wmax:xmax, n),  type="h", col="red", lwd=3)
            text((wmax+xmax)/2, ymin/2, labels=myf1(pv/2), col="red")
        }
    } # End of plot
}

# [15-10] Kruskal-Wallis Test
#' @title Kruskal-Wallis Test
#' @description Kruskal-Wallis Test with a Plot
#' @param x Response data.
#' @param y Data of factor levels.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @return None.
#'
#' @examples
#' x <- c(4.6,10.6,8.0,25.0,7.1, 2.9,10.1,3.2,3.8,6.6, 
#'        6.8,9.4,26.5,12.8,8.3, 3.4,3.9,6.0,8.6,5.0)
#' y <- rep(1:4, each=5)
#' kruskal.tsp(x, y, ws=c(7,4))
#' @rdname kruskal.tsp
#' @export
kruskal.tsp <- function(x, y, dig=4, ws="n") {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Test statistic
    nn <- length(x)
    ni <- as.vector(table(y))
    ns <- c(0, cumsum(ni))
    rx <- rank(x)
    rs <- tapply(rx, y, sum)
    ssm <- rs^2/ni
  # [Correction]
    ff <- as.factor(y)
    kk <- nlevels(ff)
    rtab <- matrix(0, kk, max(ni))
    for (k in 1:kk) rtab[k, 1:ni[k]] <- rx[which(ff==k)]
    rtab <- cbind(rtab, rs, ssm)
    rownames(rtab) <- paste0("Group", 1:kk)
    colnames(rtab) <- c(1:max(ni), "Sum", "SS-Avg")

  # Print test results
    cat("[Rank-Sum for each Group] __________________________\n")
    print(round(rtab, dig))
    # for (k in 1:kk) cat(paste0("group",k), rx[(ns[k]+1):ns[k+1]], "\t Sum =", rs[k], "\n")
    df <- kk-1
    H <- 12/nn/(nn+1)*sum(ssm) - 3*(nn+1)
    pv <- pchisq(H, df, lower.tail=FALSE)

    cat("[Kruskal-Wallis Test] ______________________________\n")
    cat(paste0("H = 12/(", nn, "\U00D7", nn+1,") \U00D7 ", myf0(sum(ssm)),
        " - 3 \U00D7 ",nn+1, " = ", myf1(H)), "\n")
    cat(paste0("k=", kk," \U21D2 df=", df, " \U21D2 P(\U03C7\U00B2(", df, ") > ", 
        myf1(H), ") = ", myf1(pv)), "\n")
    # Plot distribution of test statistic
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
      # Utilize chitest.plot2( ) function in chapter 12
        mt <- bquote(bold("Kruskal-Wallis Test: ")~chi^2 ~( .(df)) )
        xl <- "K-W Test Statistic"
        chitest.plot2(stat=H, df=df, ws=ws, main=mt, xlab=xl)
    }
    invisible(list(stat=H, df=df, p.val=pv, rsum=sum(ssm), tab=rtab))
}

# [15-11] Friedman Test
#' @title Friedman Test
#' @description Friedman Test with a Plot
#' @param x Response data.
#' @param a Data of factor levels.
#' @param b Data of block levels.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @return None.
#'
#' @examples
#' x <- c(71,77,75,79,88, 70,94,74,74,83, 72,95,77,80,90, 94,64,76,76,89)
#' a <- rep(1:4, each=5)
#' b <- rep(1:5, 4)
#' friedman.tsp(x, a, b, ws=c(7,4))
#' @rdname friedman.tsp
#' @export
friedman.tsp <- function(x, a, b, dig=4, ws="n") {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Test statistic
    nn <- length(x)
    kk <- length(unique(a))
    rr <- length(unique(b))
    af <- as.factor(a)
    bf <- as.factor(b)
    rx <- tapply(x, b, rank)
    urx <- unlist(rx)
    rxm <- matrix(urx, kk, rr)
    a2 <- rep(1:kk, rr)
    ra <- tapply(urx, a2, sum)
    ssq <- ra^2
    rtab <- cbind(rxm, ra, ssq)
    rownames(rtab) <- paste0("Group", 1:kk)
    colnames(rtab) <- c(paste0("B-", 1:rr), "Sum", "SSq")

  # Print test results
    cat("[Ranks within each Block] __________________________\n")
    print(rtab)
    # for (k in 1:kk) cat(paste0("group",k), "\t ", rxm[k,], "\t Sum =", ra[k], "\n")
    F0 <- 12/kk/(kk+1)/rr*sum(ra^2)-3*rr*(kk+1)
    df <- kk -1
    pv <- pchisq(F0, kk-1, lower.tail=FALSE)
    cat("[Friedman Test", paste0("(k=", kk, ", r=", rr, ")]"), "_________________________\n")
    cat(paste0("F = (12/(", kk, "\U00D7(", kk, "+1)\U00D7", rr, ") \U00D7 ", sum(ssq),
        " - 3\U00D7", rr, "\U00D7(", kk, "+1) = ", myf1(F0)), "\n")
    cat(paste0("df = k-1 = ", df, " \U21D2 P(\U03C7\U00B2(", df, ") > ", 
        myf1(F0), ") = ", myf1(pv)), "\n")

    # Plot distribution of test statistic
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        # Utilize chitest.plot2( ) function in chapter 12
        mt <- bquote(bold("Friedman Test: ")~chi^2 ~( .(df)) )
        xl <- "Friedman Test Statistic"
        chitest.plot2(stat=F0, df=df, ws=ws, main=mt, xlab=xl)
    }
    invisible(list(stat=F0, df=df, p.val=pv, rsum=sum(ra^2), tab=rtab))
}
