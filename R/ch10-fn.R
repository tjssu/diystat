# [Ch-10. Functions] ----------------------------------------------------------
# [Ch-10. Function Manual] -----------------------------------------

#' @title Manual for Ch10. Functions
#' @description Ch10. Inference on a Single Population
#' @param fn Function number, Default: 0
#' @return None.
#'
#' @examples
#' ch10.man()
#' ch10.man(3:4)
#' @rdname ch10.man
#' @export
ch10.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] mean1.test\t\tHypothesis Test and CI for a Mean\n")
        cat("[2] prop1.test\t\tHypothesis Test and CI for a Proportion\n")
        cat("[3] var1.test\t\tHypothesis Test and CI for a Variance\n")
        cat("[4] cimean.sim    \tSimulate the Confidence Interval for a Mean\n")
        cat("[5] civar.sim\t\tSimulate the Confidence Interval for a Variance\n")
        cat("[6] ciprop.sim\t\tSimulate the Confidence Interval for a Proportion\n")
        cat("[7] power.plot\t\tPower Function of the Test for a Mean\n")
    }
    if (1 %in% fn) {
        cat("[1] Hypothesis Test and CI for a Population Mean\n")
        cat("mean1.test(x, mu0, alt=\"two\", alp=0.05, sig, ss, n, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Sample mean or sample data.\n")
        cat("mu0\t Population mean under H0 (if missing, just find the CI).\n")
        cat("[Optional Input]--------------------------\n")
        cat("alt \t Type of the alternative hypothesis (\"two\"=default, \"le\", \"gr\").\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("sig\t Population standard deviation (only when it is known).\n")
        cat("ss\t Sample standard deviation (necessary only if no data are given).\n")
        cat("n\t Sample size (necessary only if no data are given).\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Hypothesis Test and CI for a Proportion\n")
        cat("prop1.test(x, n, p0, alt=\"two\", method=\"binom\", alp=0.05, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Number of successes.\n")
        cat("n\t Sample size.\n")
        cat("p0\t Population proportion under H0 (if missing, just find the CI).\n")
        cat("[Optional Input]--------------------------\n")
        cat("alt \t Type of the alternative hypothesis (\"two\"=default, \"gr\", \"le\").\n")
        cat("method\t One of (\"binom\", \"all\"), Default: \"binom\".\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Hypothesis Test and CI for a Population Variance\n")
        cat("var1.test(x, n, var0, alt=\"two\", alp=0.05, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Sample variance or sample data.\n")
        cat("n\t Sample size (necessary only if no data are given).\n")
        cat("var0\t Population variance under H0 (if missing, just find the CI).\n")
        cat("[Optional Input]--------------------------\n")
        cat("alt \t Type of the alternative hypothesis (\"two\"=default, \"gr\", \"le\")\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] Simulate the Confidence Interval for a Population Mean\n")
        cat("cimean.sim(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("n\t Sample size\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu\t Population mean value, Default: 0.\n")
        cat("sig\t Population standard deviation, Default: 1.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("N\t Number of iterations, Default: 100.\n")
        cat("seed\t Seed value for generating random numbers, Default: 9857.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (5 %in% fn) {
        cat("[5] Simulate the Confidence Interval for a Population Variance\n")
        cat("civar.sim(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("n\t Sample size\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu\t Population mean value, Default: 0.\n")
        cat("sig\t Population standard deviation, Default: 1.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("N\t Number of iterations, Default: 100.\n")
        cat("seed\t Seed value for generating random numbers, Default: 9857.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (6 %in% fn) {
        cat("[6] Simulate the Confidence Interval for a Population Proportion\n")
        cat("ciprop.sim(n, p=0.5, alp=0.05, N=100, seed=9857, ws=c(7,4), method=\"norm\")\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("n\t Sample size\n")
        cat("[Optional Input]--------------------------\n")
        cat("p\t Population proportion value, Default: 0.5.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("N\t Number of iterations, Default: 100.\n")
        cat("seed\t Seed value for generating random numbers, Default: 9857.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("method\t One of (\"norm\"=default, \"wilson\", \"binom\")\n")
    }
    if (7 %in% fn) {
        cat("[7] Power Function of the Test for a Population Mean\n")
        cat("power.plot(mu0, mu1, sig, nv, alt, alp=0.05, pwr=TRUE, dig=4, ws=c(7,4), ...)\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu0\t Population mean under H0, Default: 0.\n")
        cat("mu1\t Population mean under H1.\n")
        cat("sig\t Population standard deviation, Default: 1.\n")
        cat("nv\t Sample size vector, Default: c(10,30,50,100).\n")
        cat("alt \t Type of the alternative hypothesis (\"two\", \"gr\", \"le\").\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("pwr\t Logical: apply power function? (o.w. OC curve) Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
}

# Find the relevant alternative (1="less", 2="greater", 3="two sided")
alt.num <- function(alt) {
    nalt <- grep(paste0("^", alt), c("less", "greater", "two.sided"))
    if (length(nalt)>=2) nalt <- max(nalt)
    if (length(nalt)==0) nalt <- grep(paste0("^", alt), c("lower", "upper", "two-sided"))
    invisible(nalt)
}

# [10-1] Hypothesis Test and CI for a Population Mean
#' @title Hypothesis Test and CI for a Mean
#' @description Hypothesis Test for a Single Population Mean.
#' @param x Sample mean, or sample data.
#' @param mu0 Population mean under H0 (if missing, just find the CI).
#' @param alt Type of the alternative hypothesis ("gr", "le", "two"), Default: "two".
#' @param alp Level of significance, Default: 0.05.
#' @param sig Population standard deviation (only when it is known).
#' @param ss Sample standard deviation (necessary only if no data are given).
#' @param n Sample size (necessary only if no data are given).
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Summary Statistics.
#'
#' @examples
#' 95%-CI, known sigma
#' mean1.test(199.5, sig=5, n=50)
#' 95%-CI, unknown sigma
#' mean1.test(199.5, ss=5, n=50)
#'
#' H0: mu>12.5, known sigma
#' mean1.test(x=12.64, mu0=12.5, alt="gr", sig=0.5, n=40)
#' H0: mu>12.5, unknown sigma
#' mean1.test(x=12.64, mu0=12.5, alt="gr", ss=0.5, n=40)
#'
#' set.seed(1234)
#' x <- rnorm(40, 12.64, 0.5)
#' H0: mu != 12.5, unknown sigma
#' mean1.test(x, mu0=12.5)
#' H0: mu < 12.5, unknown sigma
#' mean1.test(x, mu0=12.5, alt="le")
#'
#' @rdname mean1.test
#' @export
mean1.test <- function(x, mu0, alt="two", alp=0.05, sig, ss, n, dig=4, ws=c(7,4), ...)
{
  # Check Input
    if (missing(x)) stop("Input sample data or sample mean...")
    if (missing(mu0)) Test <- FALSE else Test <- TRUE 
    if (missing(sig)) Norm <- FALSE else Norm <- TRUE 
  # Check if data are given
    if (length(x) > 1) {
        n <- length(x)
        xb <- mean(x)
        ss <- sd(x)
    } else {
        xb <- x
        if (missing(sig) && missing(ss)) stop("Input standard deviation...")
        if (missing(n)) stop("Input sample size...")
    }

  # Format function
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Critical value (or Quantiles)
    nalt <- alt.num(alt)
    if (Norm) {
        cv <- switch(nalt, qnorm(1-alp), qnorm(1-alp), qnorm(1-alp/2))
        tag <- "Z0"
    } else {
        df <- n-1
        cv <- switch(nalt, qt(1-alp, df), qt(1-alp, df), qt(1-alp/2, df))
        tag <- "T0"
    }
    rcv <- myf0(cv)

  # Standard error and Tolerance
    if (!Norm) sig <- ss
    se <- sig/sqrt(n)
    tol <- cv*se
    sstat <- c(xb, sig, se)
    names(sstat) <- c("Xbar", "Stdev", "Sterr")

  # Confidence Interval
    CI <- switch(nalt, c(-Inf, xb+tol), c(xb-tol, Inf), c(xb-tol, xb+tol))

    if (!Test) {
        if (nalt == 1) {
            cat(paste0("[-Inf, ", myf0(xb), "+", myf0(abs(cv)), "\U00D7",
                myf0(sig), "/\U221A", n, "] = [-Inf, ", 
                myf0(xb), "+", myf1(abs(tol)), "] = [-Inf, ",
                myf1(CI[2]),"]"), "\n")
        } else if (nalt == 2) {
            cat(paste0("[", myf0(xb), "-", myf0(abs(cv)), "\U00D7",
                myf0(sig), "/\U221A", n, ", Inf] = [", 
                myf0(xb), "-", myf1(abs(tol)), ", Inf] = [",
                myf1(CI[1]), ", Inf]"), "\n")
        } else if (nalt == 3) {
            cat(paste0("[", myf0(xb), " \U00B1 ", myf0(cv), "\U00D7",
                myf0(sig), "/\U221A", n, "] = [", 
                myf0(xb), " \U00B1 ", myf1(tol), "] = [",
                myf1(CI[1]), ", ", myf1(CI[2]),"]"), "\n")
        }
    }

  # Print Test
    if (Test) {
        stat <- (xb-mu0)/se
        if (nalt == 3) {
            ostat <- paste0("|", tag, "| = |", myf0(xb), " - ", myf0(mu0), 
                "| / (", myf0(sig), "/\U221A", n, ") = ", myf0(abs(stat)))
            if (abs(stat) < cv) sign <- "<" else sign <- ">"
            cat(ostat, sign, rcv, "\n")
        } else {
            ostat <- paste0(tag, " = (", myf0(xb), " - ", myf0(mu0), 
                 ") / (", myf0(sig), "/\U221A", n, ") = ", myf0(stat))
            if (stat < cv) sign <- "<" else sign <- ">"
            cat(ostat, sign, rcv, "\n")
        }

      # P-value 
        if (Norm) {
            pv <- switch(nalt, pnorm(stat),
                      pnorm(stat, lower=FALSE),
                      2*min(pnorm(stat), pnorm(stat, lower=FALSE)))
            switch(nalt, cat("P-v = P(Z < Z0) =", myf1(pv), "\n"),
                cat("P-v = P(Z > Z0) =", myf1(pv), "\n"),
                cat("P-v = 2\U00D7P(Z > |Z0|) =", myf1(pv), "\n"))
        } else {
            pv <- switch(nalt, pt(stat, df),
                      pt(stat, df, lower=FALSE),
                      2*min(pt(stat, df), pt(stat, df, lower=FALSE)))
            switch(nalt, cat("P-v = P(T < T0) =", myf1(pv), "\n"),
            cat("P-v = P(T > T0) =", myf1(pv), "\n"),
            cat("P-v = 2\U00D7P(T > |T0|) =", myf1(pv), "\n"))
        }

      # Plot the PDF
        if (is.numeric(ws)) {
            win.graph(ws[1], ws[2])
            par(mar=c(4.5,4,3,2))

          # Legend
            H1 <- switch(nalt, paste0("mu < ", mu0), paste0("mu > ", mu0),
                            paste0("mu != ", mu0))
            names <- c("Xbar", "Diff", "Serr", "Stat", "Pval")
            sstat <- c(xb, xb-mu0, se, stat, pv)
            leg <- c(paste("H1:", H1), paste(names, myf0(sstat), sep=" = "))

          # Plot
            if (Norm) df <- Inf
            unitest.plot(stat, pv, cv, nalt, dig, leg, df, ...)
        } # End of plot
    } # End of Test
  # Prepare output
    if (Norm) {
        if (Test) {
            out <- list(stat=stat, se=se, pval=pv, CI=CI)
        } else {
            out <- list(xbar=xb, sig=sig, se=se, CI=CI)
        }
    } else {
        if (Test) {
            out <- list(stat=stat, se=se, pval=pv, CI=CI, df=df)
        } else {
            out <- list(xbar=xb, sig=ss, se=se, CI=CI, df=df)
        }
    }
    invisible(out)
}

# Wilson Score CI
wsci <- function(x, n, alp=0.05, prt=TRUE, dig=4) {
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    zv <- qnorm(1-alp/2)
    denom <- n+zv^2
    cp <- (x + zv^2/2)/denom
    tol <- zv/denom*sqrt(x*(n-x)/n + zv^2/4)
    lcl <- pmax(0, cp - tol)
    ucl <- pmin(1, cp + tol)
    nx <- max(length(x), length(alp))
    
    if (prt) {
        rcp <- myf1(cp)
        rtol <- myf1(tol)
        rlcl <- myf1(lcl)
        rucl <- myf1(ucl)
        for (k in 1:nx) cat(paste0("[", rcp[k]," \U00B1 ", rtol[k], "] = [", 
                            rlcl[k], ", " , rucl[k], "]"), "\n")
    }
    if (nx==1) {
        ci <- c(lcl, ucl)
    } else {
        ci <- cbind(lcl, ucl)
        colnames(ci) <- c("LCL", "UCL")
        if (length(unique(x))==1) rownames(ci) <- alp
        if (length(unique(alp))==1) rownames(ci) <- x
    }
    invisible(ci)
}

# Clopper-Pearson Interval
cpi <- function(x, n, alp=0.05, prt=TRUE, dig=4) {
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    lcl <- qbeta(alp/2, x, n-x+1)
    ucl <- qbeta(1-alp/2, x+1, n-x)
    nx <- max(length(x), length(alp))

    if (prt) {
        rlcl <- myf1(lcl)
        rucl <- myf1(ucl)
        eqn <- paste0("[qbeta(", alp/2, ",", x, ",", n-x+1, 
                    "), qbeta(", 1-alp/2, ",", x+1, ",", n-x, ")]")
        for (k in 1:nx) cat(paste0(eqn[k], "= [", rlcl[k], ", " , rucl[k], "]"), "\n")
    }
    if (nx==1) {
        ci <- c(lcl, ucl)
    } else {
        ci <- cbind(lcl, ucl)
        colnames(ci) <- c("LCL", "UCL")
        if (length(unique(x))==1) rownames(ci) <- alp
        if (length(unique(alp))==1) rownames(ci) <- x
    }
    invisible(ci)
}

# [10-2] Hypothesis Test for a Proportion
#' @title Hypothesis Test for a Proportion
#' @description Hypothesis Test for a Proportion by Exact and Approximation Methods
#' @param x Number of successes.
#' @param n Sample size.
#' @param p0 Population proportion under H0 (if missing, just find the CI).
#' @param alt Type of the alternative hypothesis ("gr", "le", "two"), Default: "two".
#' @param method One of ("binom", "all"), Default: "binom".
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Summary Statistics.
#'
#' @examples
#' # Confidence Intervals
#' prop1.test(x=15, n=200)
#' prop1.test(x=15, n=200, method="all")
#' prop1.test(x=11:15, n=200, method="all")
#' prop1.test(x=15, n=200, alp=c(0.1, 0.05, 0.01), method="all")
#'
#' # Investigate the p-values
#' prop1.test(x=2:4, n=10, p0=0.1, alt="gr")
#' prop1.test(x=6:4, n=20, p0=0.5, alt="two")
#'
#' # Check the accuracy of normal approximation 
#' prop1.test(x=15, n=200, p0=0.1, alt="le", method="all")
#' prop1.test(x=15, n=200, p0=0.1,  alt="two", method="all")
#'
#' @rdname bntest.plot
#' @export
prop1.test <- function(x, n, p0, alt="two", method="binom", alp=0.05, dig=4, ws=c(7,4), ...)
{
  # Check Input
    if (missing(x)) stop("Input the number of successes...")
    if (missing(n)) stop("Input the sample size...")
    if (missing(p0)) Test <- FALSE else Test <- TRUE 

  # Format function
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Vectorization criteria
    if (length(n)>1) {n <- n[1]
        cat("Only the first element of sample size,", n, "will be used...\n") }
    if (length(x)>1 & length(alp)>1) {
        alp <- alp[1]
        cat("Only the first element of alpha,", alp, "will be used...\n") 
    }
    if (length(x)>1) {
        alp <- rep(alp, length(x))
    } else if (length(alp)>1) {
        x <- rep(x, length(alp))
    }
    nx <- max(length(x), length(alp))

  # Confidence Intervals ----------------------------------------
    p <- x / n
    if (!Test) prt <- TRUE else prt <- FALSE

  # [1] Normal Approximation CI
    err <- qnorm(1-alp/2)*sqrt(p*(1-p)/n)
    ci.norm <- c(p[1]-err[1], p[1]+err[1])
    if (prt && method == "all") {
        cat("Normal Approximation Interval ---------\n")
        for (k in 1:nx) {
            cat(paste0("[", myf0(p[k]), " \U00B1 ", myf0(qnorm(1-alp[k]/2)), 
                "\U00D7\U221A(", myf0(p[k]), "\U00D7", myf0(1-p[k]), "/", n,
                ")] = [", myf0(p[k]), " \U00B1 ", myf0(err[k]), "] = [",
            myf0(p[k]-err[k]), ", ", myf0(p[k]+err[k]),"]"), "\n")
        }
    }
  # [2] Wilson Score CI
    if (prt && method=="all") {
        cat("Wilson Score Interval ---------\n")
        ci.wsci <- wsci(x, n, alp, prt)
    } else ci.wsci <- wsci(x, n, alp, prt=FALSE)

  # [3] Binomial (Clopper-Pearson) CI
    if (prt) cat("Binomial Interval (Clopper-Pearson) ---------\n")
    ci.binom <- cpi(x, n, alp, prt)

    out <- list(ci.norm, ci.wsci, ci.binom)

  # Hypothesis Tests ------------------------------------------------
    if (Test) {
      # Exact binomial test
        nalt <- alt.num(alt)
        symb1 <- symb2 <- rep("", nx)
        tag1 <- tag2 <- rep("", nx)
        pv1 <- pbinom(x, n, p0)
        pv2 <- 1 - pbinom(pmax(0, x-1), n, p0)
        pv3 <- rep(NA, nx)

        fx0 <- dbinom(0:n, n, p0)
        for (k in 1:nx) pv3[k] <- sum(fx0[fx0 <= dbinom(x[k], n, p0)])
        pval <- switch(nalt, pv1, pv2, pv3)

        for (k in 1:nx) {
          # Two sided case
            if (nalt==3) {
                tag1[k] <- paste0("\U03A3[f(y) \U2264 f(", x[k], ")] = ")
                tag2[k] <- paste0("\U03A3[f(y)\U2264","f(", x[k], ")]=")
            }
            symb1[k] <- switch(nalt, paste0("P(X \U2264 ", x[k], ") = "), 
                            paste0("P(X \U2265 ", x[k], ") = "), tag1[k])
            symb2[k] <- switch(nalt, paste0("P(X\U2264", x[k], ")="), 
                            paste0("P(X\U2265", x[k], ")="), tag2[k])
            cat(paste0("Binomial Test:\t x = ", x[k], "  \t P-v = ", 
                       symb1[k], myf1(pval[k])), "\n")
        }
      # Normal approximation
        ph <- x/n
        se <- sqrt(n*p0*(1-p0))
        tstat <- (x-n*p0)/se
        pva <- switch(nalt, pnorm(tstat), 1-pnorm(tstat), 
                     2*(1-pnorm(abs(tstat))) )
        if (method == "all") {
            symb <- switch(nalt, "P(Z < Z0) = ", "P(Z > Z0) = ", 
                      "2\U00D7P(Z > |Z0|) = ")
            for (k in 1:nx) {
                cat(paste0("Normal Apprxn:\t Z0 = ", myf1(tstat[k]), 
                     "\t P-v = ", symb, myf1(pva[k])), "\n")
            }
        }
      # Normal approximation (continuity correction)
        tstat2 <- switch(nalt, (x+0.5-n*p0)/se, (x-0.5-n*p0)/se,
            ifelse(x<n*p0, (x+0.5-n*p0)/se, (x-0.5-n*p0)/se) )
        pva2 <- switch(nalt, pnorm(tstat2), 1-pnorm(tstat2), 
                      2*(1-pnorm(abs(tstat2))) )
        if (method == "all") {
            for (k in 1:nx) {
                cat(paste0("Cont. Correct:\t Z0 = ", myf1(tstat2[k]), 
                    "\t P-v = ", symb, myf1(pva2[k])), "\n")
            }
        }

        tout <- c(ph, pval, tstat, pva, tstat2, pva2)
        names(tout) <- c("p-hat", "b-pv", "N-apx", "N-pv", "N-cc", "C-pv")
        out <- c(out, tout)

      # Plot ------------------------------------------
        if (method=="binom" && is.numeric(ws)) {
          # Set titles, axes, and graphic parameters
            mt <- paste0("B(", n, ", ", myf0(p0),") Distribution & P-value")
            xrng <- c(0, n)
            xl <- "x"
            yl <- "f(x)"
            if (nx <= 6) {
                col <- c("blue", "red", "green4", "orange", "purple", "magenta")
            } else {
                col <- rainbow(nx)
            }
            text.col <- "blue"
            lwd <- 7
            fill <- "lightcyan"
            pv.col <- "red"
            cex <- 0.8

          # Set the list of arguments
            dots <- list(...)
            if (length(dots) > 0) {
                pars <- names(dots)
                if ("main" %in% pars) mt <- dots$main
                if ("col" %in% pars) col <- dots$col
                if ("text.col" %in% pars) text.col <- dots$text.col
                if ("pv.col" %in% pars) pv.col <- dots$pv.col
                if ("ylab" %in% pars) yl <- dots$ylab
                if ("xlab" %in% pars) xl <- dots$xlab
                if ("xlim" %in% pars) xrng <- dots$xlim
                if ("lwd" %in% pars) lwd <- dots$lwd
                if ("cex" %in% pars) cex <- dots$cex
            }
          # Range of x
            xa <- floor(xrng[1]):ceiling(xrng[2])
            fxa <- dbinom(xa, n, p0)
            ymax <- 1.1*max(fxa)

          # Plot the probability distribution
            win.graph(ws[1], ws[2])
            par(mar=c(4,4,3,2))
            
            plot(xa, fxa, type="h", lwd=lwd, col=grey(0.7), ylim=c(0, ymax),
                 main=mt, xlab=xl, ylab=yl)
            if (n <= 12) text(xa, fxa, myf0(fxa), 
                             col=text.col, pos=3, cex=cex)

            lab <- rep("", nx)
            for (k in 1:nx) {
                xa2 <- switch(nalt, 0:x[k], x[k]:n, 
                             (0:n)[fx0 <= dbinom(x[k], n, p0)])
                lines(xa2, dbinom(xa2, n, p0), type="h", lwd=lwd, col=col[k])
                lab[k] <- paste0(symb2[k], myf0(pval[k]))
            }
            legend("topright", lab, text.col=col[1:nx], xjust=0.5, x.intersp=0)
        } else if (is.numeric(ws)) {
          # Set titles, axes, and graphic parameters
            mt <- paste0("B(", n, ", ", myf0(p0),") Distribution & P-value")
            x1 <- max(0, floor(n*p0 - 4*se))
            x2 <- min(n, ceiling(n*p0 + 4*se))
            xrng <- c(x1, x2)
            xl <- "x"
            yl <- "f(x)"
            lwd <- 5
            pv.col <- "red"
            cex <- 0.8

          # Set the list of arguments
            dots <- list(...)
            if (length(dots) > 0) {
                pars <- names(dots)
                if ("main" %in% pars) mt <- dots$main
                if ("text.col" %in% pars) text.col <- dots$text.col
                if ("pv.col" %in% pars) pv.col <- dots$pv.col
                if ("ylab" %in% pars) yl <- dots$ylab
                if ("xlab" %in% pars) xl <- dots$xlab
                if ("xlim" %in% pars) xrng <- dots$xlim
                if ("lwd" %in% pars) lwd <- dots$lwd
                if ("cex" %in% pars) cex <- dots$cex
            }
          # Range of x
            x1 <- floor(xrng[1])
            x2 <- ceiling(xrng[2])
            xa <- x1:x2
            fxa <- dbinom(xa, n, p0)
            ymax <- 1.1*max(fxa)
          
          # Plot the probability distribution
            win.graph(ws[1], ws[2])
            par(mar=c(4,4,3,2))

            plot(xa, dbinom(xa, n, p0), type="h", lwd=lwd, col=grey(0.7), 
                ylim=c(0, ymax), main=mt, xlab="x", ylab="f(x)" )
            abline(h=0, col=grey(0.4))

            xa2 <- switch(nalt, x1:x, x:x2, (x1:x2)[fxa <= dbinom(x, n, p0)])
            lines(xa2, dbinom(xa2, n, p0), type="h", lwd=lwd, col="red")

          # Normal approximation
            xa3 <- seq(x1, x2, length=100)
            lines(xa3, dnorm(xa3, n*p0, se), col="green2")

            xo <- round(2*n*p0-x, 0)
            if (x<n*p0) {txa4 <- c(x, x+0.5, xo, xo-0.5)
            } else {        txa4 <- c(xo, xo+0.5, x, x-0.5) }
            xa4 <- switch(nalt, c(x, x+0.5), c(x, x-0.5), txa4)
            if (nalt==1) abline(v=xa4, lty=2, col=c("blue", "purple"))
            if (nalt==2) abline(v=xa4, lty=2, col=c("blue", "purple"))
            if (nalt==3) abline(v=xa4, lty=2, col=c("blue", "purple"))

            lab1 <- paste("Binom : ", myf1(pval))
            lab2 <- paste("Normal :", myf1(pva))
            lab3 <- paste("C-Corr :", myf1(pva2))
            legend("topright", c(lab1, lab2, lab3), 
                 text.col=c("red", "blue", "purple"), xjust=0.5, x.intersp=0)
        } # End of Plot
    } # End of Test
    invisible(out)
}


# [10-3] Hypothesis Test for a Population Variance
#' @title Hypothesis Test for a Variance
#' @description Hypothesis Test for a Population Variance
#' @param x Sample variance or sample data
#' @param n Sample size (necessary only if no data are given).
#' @param var0 Population variance under H0 (if missing, just find the CI).
#' @param alt Type of the alternative hypothesis ("gr", "le", "two"), Default: "two"
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Summary Statistics.
#'
#' @examples
#' var1.test(x=1.24, n=25)
#' var1.test(x=1.24, n=25, alt="gr")
#' var1.test(x=1.24, n=25, var0=0.8, alt="gr")
#'
#' x <- c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' var1.test(x=x, alt="two")
#' var1.test(x=x, var0=2, alt="two")
#'
#' set.seed(1234)
#' x <- rnorm(40, 10, 1.5)
#' var1.test(x, var0=2, alt="two")
#'
#' @rdname var1.test
#' @export
var1.test <- function(x, n, var0, alt="two", alp=0.05, dig=4, ws=c(7,4), ...) {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Check Input
    if (missing(x)) stop("Input sample data or sample mean...")
    if (missing(var0)) Test <- FALSE else Test <- TRUE 
    if (length(x)==1 && missing(n)) stop("Input sample size...")

    if (length(x) >1) {
        n <- length(x)
        xb <- mean(x)
        xss <- sum(x^2)-sum(x)^2/n
        xva <- var(x)
        cat(paste0("n = ", n, "\tSxx = ", myf0(xss), "\tVar(X) = ", myf0(xva)), "\n")
    } else {
        xva <- x
        xss <- (n-1)*xva
    }
    df <- n-1

  # Check H1 and find the CI
    nalt <- alt.num(alt)
    cv1 <- qchisq(alp/2, df)
    cv2 <- qchisq(1-alp/2, df)
    cvlo <- qchisq(alp, df)
    cvup <- qchisq(1-alp, df)

    ci <- switch(nalt, c(0, xss/cvlo), c(xss/cvup, Inf), xss/c(cv2, cv1))
    if (!Test) {
        if (nalt == 1) {
            cat(paste0((1-alp)*100, "% CI = [0, ", myf0(xss), "/", 
                myf0(cvlo), "] = [0, ", myf1(ci[2]), "]"), "\n")
        } else if (nalt == 2) {
            cat(paste0((1-alp)*100, "% CI = [", myf0(xss), "/", 
                myf0(cvup), ", Inf] = [", myf1(ci[1]), ", Inf]"), "\n")
        } else if (nalt == 3) {
            cat(paste0((1-alp)*100, "% CI = [", myf0(xss), "/", 
                myf0(cv2), ", ", myf0(xss), "/", myf0(cv1), 
                "] = [", myf1(ci[1]), ", ", myf1(ci[2]), "]"), "\n")
        }
    }
    out <- list(var=xva, df=df, ci=ci)

    chi0 <- pv <- NA
    if (Test) {
      # Calculate the test statistic and p-value
        chi0 <- (n-1)*xva / var0
        pv <- switch(nalt, pchisq(chi0, df), pchisq(chi0, df, lower=F),
            2*min(pchisq(chi0, df), pchisq(chi0, df, lower=F)) )
        fpv <- switch(nalt, paste0("P(X<", myf0(chi0), ")"), 
                  paste0("P(X>", myf0(chi0), ")"),
               paste0("2 \U00D7 [P(X<", myf0(chi0), "), P(X>", myf0(chi0), ")]") )

        cat(paste0("Chi0 = ", df, " \U00D7 ", myf1(xva), " / ", var0, " = ",
            myf1(chi0), "\n=> P-v = ", fpv, " = ", myf1(pv)), "\n")

      # Plot the test result
        if (is.numeric(ws)) {
            win.graph(ws[1], ws[2])
            par(mar=c(4.5, 4, 3, 2))
          # Set titles, axes, and graphic parameters
            prng <- c(0, qchisq(0.999, df))
            mt <- bquote(bold("Chi-Square Test :")~chi^2 ~( .(df) ))
            xl <- "Test Statistic(x)"
            yl <- "f(x)"
            col <- "red"
            text.col <- "blue"
            lwd <- 2
            fill <- "lightcyan"
            pv.col <- "red"
            cex <- 1
            pos <- "topright"

          # Set the list of arguments
            dots <- list(...)
            if (length(dots) > 0) {
                pars <- names(dots)
                if ("main" %in% pars) mt <- dots$main
                if ("col" %in% pars) col <- dots$col
                if ("text.col" %in% pars) text.col <- dots$text.col
                if ("pv.col" %in% pars) pv.col <- dots$pv.col
                if ("ylab" %in% pars) yl <- dots$ylab
                if ("xlab" %in% pars) xl <- dots$xlab
                if ("xlim" %in% pars) prng <- dots$xlim
                if ("lwd" %in% pars) lwd <- dots$lwd
                if ("cex" %in% pars) cex <- dots$cex
                if ("fill" %in% pars) fill <- dots$fill
                if ("pos" %in% pars) pos <- dots$pos
            }

            xa <- seq(prng[1], prng[2], length=100)
            fxa <- dchisq(xa, df)
            fx0 <- dchisq(chi0, df)
            plot(xa, fxa, type="n", xlab=xl, ylab=yl,
                ylim=c(-0.1, 1)*max(fxa), main=mt)

          # P-value and the critical region
            plow <- pchisq(chi0, df)
            if (nalt==2) {
                pv <- 1-plow
                cord.x <- c(chi0, seq(chi0, prng[2], length.out=20), prng[2])
                cord.y <- c(0, dchisq(seq(chi0, prng[2], length.out=20), df), 0)
                polygon(cord.x, cord.y, col=fill)

                segments(chi0, 0, chi0, fx0, lwd=2, col=col)
                text(chi0, fx0*0.9, myf0(pv), pos=4, col=col)
                text(chi0, 0, myf0(chi0), pos=1, col=text.col, cex=cex)
            } else if (nalt==1) {
                pv <- plow
                cord.x <- c(prng[1], seq(prng[1], chi0, length.out=20), chi0)
                cord.y <- c(0, dchisq(seq(prng[1], chi0, length.out=20), df), 0)
                polygon(cord.x, cord.y, col=fill)

                segments(chi0, 0, chi0, fx0, lwd=2, col=col)
                text(chi0, fx0*0.9, myf0(pv), pos=2, col=col)
                text(chi0, 0, myf0(chi0), pos=1, col=text.col, cex=cex)
            } else {
                pv <- 2*min(plow, 1-plow)
                mlow <- qchisq(pv/2, df)
                mup <- qchisq(1-pv/2, df)
                cord.x <- c(mup, seq(mup, prng[2], length.out=20), prng[2])
                cord.y <- c(0, dchisq(seq(mup, prng[2], length.out=20), df), 0)
                polygon(cord.x, cord.y, col=fill)

                cord.x <- c(prng[1], seq(prng[1], mlow, length.out=20), mlow)
                cord.y <- c(0, dchisq(seq(prng[1], mlow, length.out=20), df), 0)
                polygon(cord.x, cord.y, col=fill)

                segments(c(mlow, mup), 0, c(mlow, mup), dchisq(c(mlow, mup), df), 
                        lwd=2, col=col)
                text(c(mlow, mup), dchisq(c(mlow, mup), df)*0.9, myf0(pv/2), 
                       pos=c(2,4), col=col, cex=cex)
                text(c(mlow, mup), 0, myf0(c(mlow, mup)), pos=1, col=text.col, cex=cex)
            }
            abline(h=0)
            abline(v=qchisq(0.5, df), lty=2, lwd=2, col="green3")
            lines(xa, dchisq(xa, df), type="l", lwd=2, col="blue")
          # Legend
            H1 <- switch(nalt, paste0("V(X) < ", var0), paste0("V(X) > ", var0),
                            paste0("V(X) != ", var0))
            names <- c("Svar", "SSq", "Stat", "Pval")
            sstat <- c(xva, xss, chi0, pv)
            leg <- c(paste("H1:", H1), paste(names, myf0(sstat), sep=" = "))
            legend(pos, leg, cex=cex*0.9, xjust=0.5, x.intersp=0,
                   text.col=c("black", rep("blue", 3), "red", "red"), bg="white")
            out <- c(out, stat=chi0, pval=pv)
        } # End of Plot
    } # End of Test

    invisible(out)
}

# [10-4] Simulate the Confidence Interval for a Population Mean
#' @title Simulate the Confidence Interval for a Mean
#' @description Simulate the Confidence Interval for a Population Mean.
#' @param n Sample size.
#' @param mu Population mean value, Default: 0.
#' @param sig Population standard deviation, Default: 1.
#' @param alp Level of significance, Default: 0.05.
#' @param N Number of iterations, Default: 100.
#' @param seed Seed value for generating random numbers, Default: 9857.
#' @param ws Graphic window size, Default: c(7,4).
#' @return Summary Statistics.
#'
#' @examples
#' cimean.sim(n=16, mu=10, sig=2)
#' cimean.sim(n=16, mu=10, sig=2, N=10000, plot=FALSE)
#' @rdname cimean.sim
#' @export
cimean.sim <- function(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, ws=c(7,4)) {
  # Create the matrix of confidence intervals
    ci <- matrix(0, nrow=N, ncol=3)
    ir <- 1:N
  # Tail value of the t-distribution
    tv <- qt(1-alp/2, n-1)
  # Set seed for reproducibility
    set.seed(seed)
  # Generate random numbers and calculate the confidence intervals
    for (i in ir) {
        x <- rnorm(n, mu, sig)
        xm <- mean(x)
        xs <- sd(x)
        lcl <- xm-tv*xs/sqrt(n)
        ucl <- xm+tv*xs/sqrt(n)
        ci[i, ] <- c(lcl, xm, ucl)
    }
    if (is.numeric(ws)) {
      # Display graph
        win.graph(ws[1], ws[2])
        par(mar=c(4.5,4,3,2))
        plot(ir, ci[ ,2], type="n", ylim=c(min(ci), max(ci)),
            main="Confidence Intervals for a Population Mean",
            ylab="Confidence Interval", xlab="Iteration")
        abline(h=mu, col="red")
        #if (N > 100) lwd <- 1 else lwd <- 2
        arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, 
            lwd=ifelse((ci[ , 1]>mu | ci[ , 3]<mu), 2, 1),
            col=ifelse((ci[ , 1]>mu | ci[ , 3]<mu), "red", "blue") )
        points(ir, ci[ ,2], pch=19, cex=0.7)
    }

  # Number of confidence intervals without the population mean
    nup <- sum(ci[ , 1]>mu)
    nlow <- sum(ci[ ,3]<mu)
    cat(paste0("P(LCL > ", mu, ") = ", nup, " / ", N, " = ", nup/N,
        "\t P(UCL < ", mu, ") = ", nlow, " / ", N, " = ", nlow/N), "\n")
    stat <- c(mean(xm), mean(xs), mean(lcl), mean(ucl)) 
    names(stat) <- c("Mean", "Stdv", "LCL", "UCL")
    nout <- c(nlow, nup)
    names(nout) <- c("Low", "Upp")
    invisible(list(stat=stat, nout=nout))
}

# [10-5] Simulate the Confidence Interval for a Population Variance
#' @title Simulate the Confidence Interval for a Variance
#' @description Simulate the Confidence Interval for a Population Variance
#' @param n Sample size, Default: 100.
#' @param mu Population mean value, Default: 0.
#' @param sig Population standard deviation, Default: 1.
#' @param alp Level of significance, Default: 0.05.
#' @param N Number of iterations, Default: 100.
#' @param seed Seed value for generating random numbers, Default: 9857.
#' @param ws Graphic window size, Default: c(7,4).
#' @return Summary Statistics.
#'
#' @examples
#' civar.sim(n=16, mu=10, sig=2)
#' civar.sim(n=16, mu=10, sig=2, N=10000, plot=FALSE)
#' @rdname civar.sim
#' @export
civar.sim <- function(n, mu=0, sig=1, alp=0.05, N=100, seed=9857, ws=c(7,4)) {
  # Create the matrix of confidence intervals
    ci <- matrix(0, nrow=N, ncol=3)
    ir <- 1:N
  # Tail values of the chi-square distribution
    cv1 <- qchisq(alp/2, n-1)
    cv2 <- qchisq(1-alp/2, n-1)
  # Set seed for reproducibility
    set.seed(seed)
  # Generate random numbers and calculate the confidence intervals
    for (i in ir) {
        x <- rnorm(n, mu, sig)
        xm <- var(x)
        xss <- xm*(n-1)
        lcl <- xss/cv2
        ucl <- xss/cv1
        ci[i, ] <- c(lcl, xm, ucl)
    }
    if (is.numeric(ws)) {
      # Display graph
        win.graph(ws[1], ws[2])
        par(mar=c(4.5,4,3,2))

        plot(ir, ci[ ,2], type="n", col=1, ylim=c(min(ci), max(ci)),
            main="Confidence Intervals for a Population Variance",
            ylab="Confidence Interval", xlab="Iteration")
        abline(h=sig^2, col="red")
        arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, 
            lwd=ifelse((ci[ , 1]>sig^2 | ci[ , 3]<sig^2), 2, 1),
            col=ifelse((ci[ , 1]>sig^2 | ci[ , 3]<sig^2), "red", "blue") )
        points(ir, ci[ ,2], pch=19, cex=0.7)
    }
   # Number of confidence intervals without the population variance
    nup <- sum(ci[ , 1]>sig^2)
    nlow <- sum(ci[ ,3]<sig^2)
    cat(paste0("P(LCL > ", sig^2, ") = ", nup, " / ", N, " = ", nup/N,
        "\t P(UCL < ", sig^2, ") = ", nlow, " / ", N, " = ", nlow/N), "\n")

    stat <- c(mean(xm), mean(xss), mean(lcl), mean(ucl)) 
    names(stat) <- c("Var", "SS", "LCL", "UCL")
    nout <- c(nlow, nup)
    names(nout) <- c("Low", "Upp")
    invisible(list(stat=stat, nout=nout))
}

# [10-6] Simulate the Confidence Interval for a Population Proportion
#' @title Simulate the Confidence Interval for a Proportion
#' @description Simulate the Confidence Interval for a Population Proportion
#' @param n Sample size.
#' @param p Population proportion value, Default: 0.5.
#' @param alp Level of significance, Default: 0.05.
#' @param N Number of iterations, Default: 100.
#' @param seed Seed value for generating random numbers, Default: 9857.
#' @param ws Graphic window size, Default: c(7,4).
#' @param method One of ("normal", "wilson", "binom"), Default: "normal".
#' @return Summary Statistics.
#'
#' @examples
#' ciprop.sim(n=16, p=0.6, alp=0.05, N=100)
#' ciprop.sim(n=16, p=0.6, alp=0.05, N=10000, plot=FALSE)
#'
#' ciprop.sim(n=100, p=0.6, alp=0.05, N=10000, plot=FALSE)
#' @rdname ciprop.sim
#' @export
ciprop.sim <- function(n, p=0.5, alp=0.05, N=100, seed=9857, ws=c(7,4), method="norm") {
     ir <- 1:N
  # Tail values of the standard normal distribution
    zv <- qnorm(1-alp/2)
  # Set seed for reproducibility
    set.seed(seed)
  # Generate random numbers and calculate the confidence intervals
    xm <- rbinom(N, n, p)
    xp <- xm/n
    xv <- xp*(1-xp)/n

    if (grepl(method, "normal")) {
        tag <- "(Normal Approximation)"
        lcl <- pmax(0, xp - zv*sqrt(xv))
        ucl <- pmin(1, xp + zv*sqrt(xv))
    } else if (grepl(method, "wilson")) {
        tag <- "(Wilson Score Interval)"
        dd <- wsci(xm, n, alp, prt=FALSE)
        lcl <- dd[, 1]
        ucl <- dd[, 2]
    } else if (grepl(method, "binom")) {
        tag <- "(Binomial Method)"
        dd <- cpi(xm, n, alp, prt=FALSE)
        lcl <- dd[, 1]
        ucl <- dd[, 2]
    }
    ci <- cbind(lcl, xp, ucl)

  # Display graph
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5,4,3,2))

        plot(ir, ci[ ,2], type="n", col=1, ylim=c(min(ci), max(ci)),
            main=paste("CI for a Proportion", tag),
            ylab="Confidence Interval", xlab="Iteration")
        abline(h=p, col="red")
        arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90,
            lwd=ifelse((ci[ , 1]>p | ci[ , 3]<p), 2, 1),
            col=ifelse((ci[ , 1]>p | ci[ , 3]<p), "red", "blue") )
        points(ir, ci[ ,2], pch=19, cex=0.7)
    }
  # Number of confidence intervals without the population proportion
    nup <- sum(ci[ , 1]>p)
    nlow <- sum(ci[ ,3]<p)
    cat(paste0("P(LCL > ", p, ") = ", nup, " / ", N, " = ", nup/N,
        "\t P(UCL < ", p, ") = ", nlow, " / ", N, " = ", nlow/N), "\n")

    stat <- c(mean(xp), mean(xv), mean(lcl), mean(ucl)) 
    names(stat) <- c("Prop", "Svar", "LCL", "UCL")
    nout <- c(nlow, nup)
    names(nout) <- c("Low", "Upp")
    invisible(list(stat=stat, nout=nout))
}

# [10-7] Power Function of the Test for a Population Mean
#' @title Power Function of the Test for a Mean
#' @description Power Function of the Test for a Population Mean.
#' @param mu0 Population mean under H0, Default: 0.
#' @param mu1 Population mean under H1.
#' @param sig Population standard deviation, Default: 1.
#' @param nv Sample size vector, Default: c(10,30,50,100).
#' @param alt Type of the alternative hypothesis ("gr", "le", "two").
#' @param pwr Logical: apply power function? (o.w. OC curve) Default: TRUE.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Power function (or OC function).
#'
#' @examples
#' power.plot()
#' power.plot(pwr=FALSE)
#' power.plot(mu1=0.3)
#' power.plot(mu1=0.3, pwr=FALSE)
#'
#' power.plot(mu0=100, mu1=102, sig=5, alt="gr")
#' power.plot(mu0=100, mu1=102, sig=5, alt="gr", pwr=FALSE)
#' n = c(10, 30, 50, 70, 100)
#' power.plot(mu0=100, mu1=102, sig=5, nv=n)
#' power.plot(mu0=100, mu1=102, sig=5, nv=n, pwr=FALSE)
#'
#' power.plot(nv=50:150)
#' power.plot(nv=50:150, pwr=FALSE)
#'
#' @rdname power.plot
#' @export
power.plot <- function(mu0, mu1, sig, nv, alt, alp=0.05, pwr=TRUE, dig=4, ws=c(7,4), ...) {
    myf0 <- function(x) round(x, dig)
    myf1 <- function(x) format(round(x, dig), nsmall=dig)

  # Type of the alternative hypothesis
    nalt <- alt.num(alt)
  # Set plot variables
    if (missing(mu0)) {mu0 <- 0
        cat("Since mu0 is missing, it is automatically set to 0.\n")}
    if (missing(sig)) {sig <- 1
        cat("Since sig is missing, it is automatically set to 1.\n")}
    if (missing(nv)) nv <- c(10, 30, 50, 100)
    nn <- length(nv)
    se0 <- sig/sqrt(min(nv))

  # Define the power function (or operating characteristic curve)
    pwr1 <- function (n, mu) pnorm(-qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
    pwr2 <- function (n, mu) 1 - pnorm(qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
    pwr3 <- function (n, mu) {pnorm(-qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig)+
                    1-pnorm(qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig) }

    occ1 <- function (n, mu) 1 - pnorm(-qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
    occ2 <- function (n, mu) pnorm(qnorm(1-alp)+n^0.5*(mu0-mu)/sig)
    occ3 <- function (n, mu) {-pnorm(-qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig)+
                        pnorm(qnorm(1-alp/2)+n^0.5*(mu0-mu)/sig) }

    if (pwr) {
        my.pwr <- switch(nalt, pwr1, pwr2, pwr3)
    } else {
        my.pwr <- switch(nalt, occ1, occ2, occ3)
    }
  # Calculate the power (or operating characteristic curve)
    if (!(missing(mu1))) {
        power <- my.pwr(nv, mu1)
        names(power) <- nv
        print(myf0(power))
    }

  # Set titles, axes, and graphic parameters
    prng <- switch(nalt, c(mu0-4*se0, mu0), c(mu0, mu0+4*se0), c(mu0-4*se0, mu0+4*se0))

    if (nn>5) {
        dcol <- rainbow(nn)
    } else {
        dcol <- c("red", "blue", "green4", "purple", "magenta")
    }

    xl <- expression(mu)
    lwd <- 2
    cex <- 1
    text.col <- "red"

    if (pwr) {
        yl <- expression(psi(mu))
        palp <- alp
        lpos <- switch(nalt, "bottom", "right", "bottomright")
        mt <- bquote(bold(Power~Function)~~psi(mu)~~
                            (mu[0]~"="~ .(mu0)~","~sigma~"="~ .(sig)) )
        lalp <- bquote(alpha==.(alp))
    } else {
        yl <- expression(1-psi(mu))
        palp <- 1-alp
        lpos <- switch(nalt, "top", "right", "topright")
        mt <- bquote(bold(OC~Curve)~~1-psi(mu)~~
                               (mu[0]~"="~ .(mu0)~","~sigma~"="~ .(sig)) )
        lalp <- bquote(alpha==.(palp))
    }

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("main" %in% pars) mt <- dots$main
        if ("col" %in% pars) dcol <- dots$col
        if ("pos" %in% pars) lpos <- dots$pos
        if ("text.col" %in% pars) text.col <- dots$text.col
        if ("ylab" %in% pars) yl <- dots$ylab
        if ("xlab" %in% pars) xl <- dots$xlab
        if ("xlim" %in% pars) prng <- dots$xlim
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("cex" %in% pars) cex <- dots$cex
    }

  # Set axis and display graph
    xa <- seq(prng[1], prng[2], length.out=100)
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4,4,3,2))
    }
    if (!(missing(mu1))) {
        x1 <- prng[1]-(prng[2]-prng[1])*0.15
    } else {
        x1 <- prng[1]
    }

  # Power function
    plot(xa, my.pwr(nv[1], xa), type="n", ylim=c(0,1), xlim=c(x1, prng[2]),
            main=mt, ylab=yl, xlab=xl)
    for (i in 1:nn) lines(xa, my.pwr(nv[i], xa), lwd=lwd, col=dcol[i])
    grid(col="green")
    if (pwr) base <- 0 else base <- 1
    abline(h=base, lty=3, col=grey(0.5))
    abline(v=mu0, lty=3, col=grey(0.5))
    segments(prng[1], palp, prng[2]*1.2, lty=2, col="red")

  # Illustration
    if (nalt < 3) {
        if (nn<=10) {
            if (!(missing(mu1))) {
                abline(v=mu1, lty=2, col="blue")
                abline(v=prng[1], lty=3, col=grey(0.5))
                segments(prng[1], power, mu1, power, lty=2, col="blue")
                text(x1, power, myf1(power), pos=4, cex=cex, col=text.col)
                text(prng[1], palp, lalp, cex=cex, col=text.col, pos=2)
            }
        # Display sample size [Correction]
        legend(lpos, paste0("n=", nv), col=dcol[1:nn], lwd=lwd, 
                  bg="white")
        }
    } else if (nalt==3) {
       if (nn<=10) {
            if (!(missing(mu1))) {
                mu2 <- mu0-(mu1-mu0)
                abline(v=c(mu1, mu2), lty=2, col="blue")
                abline(v=prng[1], lty=3, col=grey(0.5))
                segments(prng[1], power, mu1, power, lty=2, col="blue")
                text(x1, power, labels=myf1(power), pos=4, cex=cex, col=text.col)
                text(prng[1], palp, labels=lalp, cex=cex, col=text.col, pos=2)
            }
        # Display sample size [Correction]
        legend(lpos, paste0("n=", nv), col=dcol[1:nn],
               lwd=lwd, cex=cex, bg="white")
        }
    }
    invisible(my.pwr)
}

## ---------------------------------------------------------------------
# [10-A1] Plot the PDF of Test Statistic under the Normal Distribution
#' @title Plot the PDF of Test Statistic under the Normal Distribution
#' @description Plot the PDF of Test Statistic under the Normal Distribution
#' @param z0 Z-test statistic for the population mean
#' @param alt Type of the alternative hypothesis, Default: 'two'
#' @param dig Number of decimal places, Default: 4.
#' @param mt Main title of the plot
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param xlab Label of x-axis, Default: 'Test Statistic (Z0)'
#' @param prt Logical value for printing the p-value, Default: FALSE
#' @return None.
#'
#' @examples
#' win.graph(7, 5)
#' normtest.plot(1.93)
#' normtest.plot(-1.93, alt="le")
#' @rdname normtest2.plot
#' @export
normtest.plot <- function(z0, alt="two", dig=4, mt, prng=c(-4,4), xlab, prt=FALSE) {
    myf0 <- function(v) round(v, dig)

    nalt <- alt.num(alt)
    xa <- seq(prng[1], prng[2], length.out=101)
    if (missing(mt)) mt <- "Distribution of Z0 under H0 ~ N(0,1)"
    if (missing(xlab)) xlab <- "Test Statistic (Z0)"

    plot(xa, dnorm(xa), type="n", xlab=xlab, ylab="pdf",
        ylim=c(-0.1, 1)*max(dnorm(xa)), main=mt)
    # P-value and the critical region
    if (nalt==2) {
        pv <- pnorm(z0, lower.tail=FALSE)
        if (prt) cat("P-v =", pv, "\n")
        cord.x <- c(z0, seq(z0, prng[2], length.out=20), prng[2])
        cord.y <- c(0, dnorm(seq(z0, prng[2], length.out=20)), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(z0, 0, z0, dnorm(z0), lwd=2, col="red")
        text(z0, dnorm(z0)*0.9, myf0(pv), pos=4, col="red")
        text(z0, 0, myf0(z0), pos=1, col="blue")
    } else if (nalt==1) {
        pv <- pnorm(z0)
        if (prt) cat("P-v =", pv, "\n")
        cord.x <- c(prng[1], seq(prng[1], z0, length.out=20), z0)
        cord.y <- c(0, dnorm(seq(prng[1], z0, length.out=20)), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(z0, 0, z0, dnorm(z0), lwd=2, col="red")
        text(z0, dnorm(z0)*0.9, myf0(pv), pos=2, col="red")
        text(z0, 0, myf0(z0), pos=1, col="blue")
    } else if (nalt==3) {
        mlow <- ifelse(z0>0, -z0, z0)
        mup <- ifelse(z0>0, z0, -z0)
        pv <- 2*pnorm(mlow)
        if (prt) cat("P-v =", pv, "\n")
        cord.x <- c(mup, seq(mup, prng[2], length.out=20), prng[2])
        cord.y <- c(0, dnorm(seq(mup, prng[2], length.out=20)), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        cord.x <- c(prng[1], seq(prng[1], mlow, length.out=20), mlow)
        cord.y <- c(0, dnorm(seq(prng[1], mlow, length.out=20)), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(c(mlow, mup), 0, c(mlow, mup), dnorm(z0), lwd=2, col="red")
        text(c(mlow, mup), dnorm(z0)*0.9, myf0(pv/2), pos=c(2,4), col="red")
        text(c(mlow, mup), 0, myf0(c(mlow, mup)), pos=1, col="blue")
    }
    abline(h=0); abline(v=0, lty=2, lwd=2, col="green3")
    lines(xa, dnorm(xa), type="l", lwd=2, col="blue")
}

# [10-A2] Plot the PDF of Test Statistic with the T-distribution
#' @title Plot the PDF of Test Statistic with the T-distribution
#' @description Plot the PDF of Test Statistic with the T-distribution
#' @param t0 T-test statistic for the population mean
#' @param df Degree of freedom
#' @param alt Type of the alternative hypothesis, Default: 'two'
#' @param dig Number of digits below the decimal point, Default: 4
#' @param mt Main title of the plot
#' @param prng Range of x-axis, Default: c(-4, 4)
#' @param xlab Label of x-axis, Default: 'Test Statistic (T0)'
#' @param prt Logical value for printing the p-value, Default: FALSE
#' @return None.
#'
#' @examples
#' win.graph(7, 5)
#' ttest.plot(1.96, df=24)
#'
#' set.seed(3691)
#' x <- rnorm(20, 199, 2)
#' y <- rnorm(25, 200, 4)
#' (res <- t.test(x,y))
#' ttest.plot(res$stat, res$para)
#' @rdname ttest.plot
#' @export
ttest.plot <- function(t0, df, alt="two", dig=4, mt, prng=c(-4,4), xlab, prt=FALSE) {
    myf0 <- function(v) round(v, dig)

    xa <- seq(prng[1], prng[2], length.out=101)
    nalt <- alt.num(alt)
    if (missing(mt)) mt <- paste0("Distribution of T0 under H0 ~ t(", myf0(df),")")
    if (missing(xlab)) xlab <- "Test Statistic (T0)"
    # Plot the PDF
    plot(xa, dt(xa, df), type="n", xlab=xlab, ylab="pdf",
        ylim=c(-0.1, 1)*max(dt(xa, df)), main=mt)
    # P-value and the critical region
    if (nalt==2) {
        pv <- pt(t0, df, lower.tail=FALSE)
        if (prt) cat("P-v =", pv, "\n")
        cord.x <- c(t0, seq(t0, prng[2], length.out=20), prng[2])
        cord.y <- c(0, dt(seq(t0, prng[2], length.out=20), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(t0, 0, t0, dt(t0, df), lwd=2, col="red")
        text(t0, dt(t0, df)*0.9, myf0(pv), pos=4, col="red")
        text(t0, 0, myf0(t0), pos=1, col="blue")
    } else if (nalt==1) {
        pv <- pt(t0, df)
        if (prt) cat("P-v =", pv, "\n")
        cord.x <- c(prng[1], seq(prng[1], t0, length.out=20), t0)
        cord.y <- c(0, dt(seq(prng[1], t0, length.out=20), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(t0, 0, t0, dt(t0, df), lwd=2, col="red")
        text(t0, dt(t0, df)*0.9, myf0(pv), pos=2, col="red")
        text(t0, 0, myf0(t0), pos=1, col="blue")
    } else if (nalt==3) {
        mlow <- ifelse(t0>0, -t0, t0)
        mup <- ifelse(t0>0, t0, -t0)
        pv <- 2*pt(mlow, df)
        if (prt) cat("P-v =", pv, "\n")
        cord.x <- c(mup, seq(mup, prng[2], length.out=20), prng[2])
        cord.y <- c(0, dt(seq(mup, prng[2], length.out=20), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        cord.x <- c(prng[1], seq(prng[1], mlow, length.out=20), mlow)
        cord.y <- c(0, dt(seq(prng[1], mlow, length.out=20), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(c(mlow, mup), 0, c(mlow, mup), dt(t0, df), lwd=2, col="red")
        text(c(mlow, mup), dt(t0, df)*0.9, myf0(pv/2), pos=c(2,4), col="red")
        text(c(mlow, mup), 0, myf0(c(mlow, mup)), pos=1, col="blue")
    }
    abline(h=0)
    abline(v=0, lty=2, lwd=2, col="green3")
    lines(xa, dt(xa, df), type="l", lwd=2, col="blue")
}

# [10-1 OLD] Confidence Interval for a Population Mean
#' @title Confidence Interval for a Mean
#' @description Confidence Interval for a Single Population Mean
#' @param x Sample mean, or sample data
#' @param sig Standard deviation (unnecessary when sample data are given)
#' @param n Sample size (unnecessary when sample data are given)
#' @param pvar Logical value for known population variance, Default: FALSE
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#'
#' @examples
#' mean1.ci(x=199.5, sig=5, n=20, pvar=TRUE)
#' mean1.ci(x=199.5, sig=5, n=20)
#'
#' set.seed(1234)
#' x <- rnorm(20, 199.5, 5)
#' mean1.ci(x, sig=5, pvar=TRUE)
#' mean1.ci(x)
#'
#' @rdname mean1.ci
#' @export
mean1.ci <- function(x, sig, n, pvar=FALSE, alp=0.05, dig=4) {
    myf0 <- function(v) round(v, dig)

    if (missing(x)) stop("Input sample data or sample mean...")
    if (pvar && missing(sig)) stop("Input standard deviation...")
    if (length(x)>1) {
        n <- length(x)
        xb <- mean(x)
        if (!pvar) sig <- sd(x)
    } else {
        xb <- x
        if ((!pvar) && missing(sig)) stop("Input sample standard deviation...")
        if (missing(n)) stop("Input sample size...")
    }

    # Define the quantile function
    if (pvar) {
        quant <- function(p) qnorm(p)
    } else {
        quant <- function(p) qt(p, n-1)
    }

    tol <- quant(1-alp/2)*sig/sqrt(n)

    cat(paste0("[", myf0(xb), " \U00B1 ", myf0(quant(1-alp/2)), "\U00D7",
        myf0(sig), "/\U221A", n,
        "] = [", myf0(xb), " \U00B1 ", myf0(tol), "] = [",
        myf0(xb-tol), ", ", myf0(xb+tol),"]"), "\n")
}

# [10-3] Confidence Interval for a Population Proportion
#' @title Confidence Interval for a Population Proportion
#' @description Confidence Interval for a Population Proportion by Three Methods
#' @param x Number of successes in a sample
#' @param n Sample size
#' @param alp Level of significance, Default: 0.05
#' @param prt Logical value for printing the output, Default: TRUE
#' @param dig Number of digits below the decimal point, Default: 4
#' @param method One of ("all", "normal", "wilson", "binom"), Default: "all"
#' @return c(lcl, ucl)
#'
#' @examples
#' prop1.ci(n=200, x=15)
#' prop1.ci(n=200, x=11:15)
#' prop1.ci(n=200, x=15, alp=c(0.1, 0.05, 0.01))
#' @rdname prob.ci
#' @export
prop1.ci <- function(x, n, alp=0.05, prt=TRUE, dig=4, method="all") {
    # [Correction]
    if (length(n)>1) {n <- n[1]
        cat("Only the first element of sample size,", n, "will be used...\n") }
    if (length(x)>1 & length(alp)>1) {alp <- alp[1]
        cat("Only the first element of alpha,", alp, "will be used...\n") }
    if (length(x)>1) {alp <- rep(alp, length(x))
    } else if (length(alp)>1) {x <- rep(x, length(alp))}

    myf0 <- function(v) round(v, dig)

    p <- x / n
    err <- qnorm(1-alp/2)*sqrt(p*(1-p)/n)
    # [Correction]
    nn <- max(length(x), length(alp))

    if (any(grepl(method, c("all", "normal")))) {
        if (prt) {
            cat("Normal Approximation Interval ---------\n")
            for (k in 1:nn) {
                cat(paste0("[", myf0(p[k]), " \U00B1 ", myf0(qnorm(1-alp[k]/2)), 
                    "\U00D7\U221A(", myf0(p[k]), "\U00D7", myf0(1-p[k]), "/", n,
                    ")] = [", myf0(p[k]), " \U00B1 ", myf0(err[k]), "] = [",
                myf0(p[k]-err[k]), ", ", myf0(p[k]+err[k]),"]"), "\n")
            }
        }
        ci <- c(p[1]-err[1], p[1]+err[1])
    }

    if (any(grepl(method, c("all", "wilson")))) {
        if (prt) cat("Wilson Score Interval ---------\n")
        for (k in 1:nn) ci <- wsci(x[k], n, alp[k], prt)
    }

    if (any(grepl(method, c("all", "binom")))) {
        if (prt) cat("Binomial Interval (Clopper-Pearson) ---------\n")
        for (k in 1:nn) ci <- cpi(x[k], n, alp[k], prt)
    }

    invisible(ci)
}

# [10-5] Confidence Interval for a Population Variance
#' @title Confidence Interval for a Variance
#' @description Confidence Interval for a Population Variance
#' @param x Data vector
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#'
#' @examples
#' x <- c(20.0, 21.5, 20.9, 19.8, 22.5, 20.3, 23.6, 18.0, 23.3, 17.8)
#' var1.ci(x, dig=3)
#'
#' set.seed(1234)
#' x <- rnorm(40, 10, 1.5)
#' var1.ci(x)
#' @rdname var1.ci
#' @export
var1.ci <- function(x, alp=0.05, dig=4) {
    n <- length(x)
    xss <- sum(x^2)-sum(x)^2/n
    xv <- var(x)
    cv1 <- qchisq(alp/2, n-1)
    cv2 <- qchisq(1-alp/2, n-1)
    cat(paste0((1-alp)*100, "% CI = [", myf0(xss), " / ", myf0(cv2), ", ",
        myf0(xss), " / ", myf0(cv1), "] = [",
        myf0(xss/cv2), ", ", myf0(xss/cv1), "]"), "\n")
    invisible(list(var=xv, conf=xss/c(cv2,cv1)))
}

# Plot the Result of the Chi-square Test
chitest.plot <- function(stat, df, pp, alt="two", mt, dig=4, ppt=20) {
    # Plot the PDF
    if (missing(pp)) pp <- 0.999
    prng <- c(0, qchisq(pp, df))
    if (missing(mt)) mt <- bquote(bold("Chi-Square Test :")~chi^2 ~( .(df) ))

    myf0 <- function(v) round(v, dig)

    xa <- seq(prng[1], prng[2], length=100)
    nalt <- alt.num(alt)

    win.graph(7, 5)
    plot(xa, dchisq(xa, df), type="n", xlab="Test Statistic(x)", ylab="f(x)",
        ylim=c(-0.1, 1)*max(dchisq(xa, df)), main=mt)
    # P-value and the critical region
    plow <- pchisq(stat, df)
    if (nalt==2) {
        pv <- 1-plow
        # cat(paste0("Chi0 = ", myf0(stat), "\t P-v = ", myf0(pv)), "\n")
        cord.x <- c(stat, seq(stat, prng[2], length.out=ppt), prng[2])
        cord.y <- c(0, dchisq(seq(stat, prng[2], length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(stat, 0, stat, dchisq(stat, df), lwd=2, col="red")
        text(stat, dchisq(stat, df)*0.9, myf0(pv, 4), pos=4, col="red")
        text(stat, 0, myf0(stat, 4), pos=1, col="blue")
    } else if (nalt==1) {
        pv <- plow
        # cat(paste0("Chi0 = ", myf0(stat), "\t P-v = ", myf0(pv)), "\n")
        cord.x <- c(prng[1], seq(prng[1], stat, length.out=ppt), stat)
        cord.y <- c(0, dchisq(seq(prng[1], stat, length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(stat, 0, stat, dchisq(stat, df), lwd=2, col="red")
        text(stat, dchisq(stat, df)*0.9, myf0(pv, 4), pos=2, col="red")
        text(stat, 0, myf0(stat, 4), pos=1, col="blue")
    } else {
        pv <- 2*min(plow, 1-plow)
        # cat(paste0("Chi0 = ", myf0(stat), "\t P-v = ", myf0(pv)), "\n")
        mlow <- qchisq(pv/2, df)
        mup <- qchisq(1-pv/2, df)
        cord.x <- c(mup, seq(mup, prng[2], length.out=ppt), prng[2])
        cord.y <- c(0, dchisq(seq(mup, prng[2], length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        cord.x <- c(prng[1], seq(prng[1], mlow, length.out=ppt), mlow)
        cord.y <- c(0, dchisq(seq(prng[1], mlow, length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        segments(c(mlow, mup), 0, c(mlow, mup), dchisq(c(mlow, mup), df), lwd=2, col="red")
        text(c(mlow, mup), dchisq(c(mlow, mup), df)*0.9, myf0(pv/2, 4), pos=c(2,4), col="red")
        text(c(mlow, mup), 0, myf0(c(mlow, mup), 4), pos=1, col="blue")
    }
    abline(h=0)
    abline(v=qchisq(0.5, df), lty=2, lwd=2, col="green3")
    lines(xa, dchisq(xa, df), type="l", lwd=2, col="blue")
}
