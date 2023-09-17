# [Ch-12 Functions] ----------------------------------------------------------------------------------
# [Ch-12 Function Manual] -----------------------------------------

#' @title Manual for Ch12. Functions
#' @description Ch12. Analysis of Categorical Data
#' @param fn Function number, Default: 0
#' @return None.
#'
#' @examples
#' ch12.man()
#' ch12.man(1)
#' @rdname ch12.man
#' @export
ch12.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] chigof.test\tGoodness of Fit Test from a Frequency Table\n")
        cat("[2] pbgof.test\tGoodness of Fit Test for Poisson or Binomial Distribution\n")
        cat("[3] cross.test\tChi-square Test for Cross Tabulation Analysis\n")
        cat("[4] mosaic.res\tMosaic Plot with Pearson Residuals for a Cross Table\n")
    }
    if (1 %in% fn) {
        cat("[1] Goodness of Fit Test from a Frequency Table\n")
        cat("chigof.test(x, p, alp=0.05, dtab=2, dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Frequency table.\n")
        cat("[Optional Input]--------------------------\n")
        cat("p\t Probability vector, Default: rep(1/k, k).\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dtab\t Number of decimal places for the table, Default: 2.\n")
        cat("dig\t Number of decimal places for other output, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (2 %in% fn) {
        cat("[2] Goodness of Fit Test for Poisson or Binomial Distribution\n")
        cat("pbgof.test(tab, par, dist=\"p\", mc1, mc2, alp=0.05, dtab=2, dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("tab\t Frequency table.\n")
        cat("[Optional Input]--------------------------\n")
        cat("par\t Parameters (Poisson: mean, Binomial: c(n,p).)\n")
        cat("dist\t Distribution name ('p' or 'b'), Default: 'p'.\n")
        cat("mc1\t Lower category numbers to merge.\n")
        cat("mc2\t Upper category numbers to merge.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dtab\t Number of decimal places for the table, Default: 2.\n")
        cat("dig\t Number of decimal places for other output, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (3 %in% fn) {
        cat("[3] Chi-square Test for Cross Tabulation Analysis\n")
        cat("cross.test(x, v1, v2, alp=0.05, dtab=2, dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Cross table of frequency.\n")
        cat("[Optional Input]--------------------------\n")
        cat("v1\t Row variable name.\n")
        cat("v2\t Column variable name.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dtab\t Number of decimal places for the table, Default: 2.\n")
        cat("dig\t Number of decimal places for other output, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
    if (4 %in% fn) {
        cat("[4] Mosaic Plot with Pearson Residuals for a Cross Table\n")
        cat("require(vcd)\n")
        cat("mosaic.res(tab, main, dig=4, resid=TRUE, ws=c(7,6))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("tab\t Cross table of frequency.\n")
        cat("[Optional Input]--------------------------\n")
        cat("main\t Graph title.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("resid\t Logical: display Pearson residuals? Default: TRUE.\n")
        cat("ws\t Graphic window size, Default: c(7,5).\n")
    }
    if (5 %in% fn) {
        cat("[5] Plot of the Chi-square Test\n")
        cat("chitest.plot2(stat, df, alp=0.05, alt=\"gr\", dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("stat\t Chi-square test statistic.\n")
        cat("df\t Degree of freedom\n")
        cat("[Optional Input]--------------------------\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("alt\t Type of the alternative hypothesis, Default: \"gr\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
}

# [12-1] Goodness of Fit Test from a Frequency Table
#' @title Goodness of Fit Test from a Frequency Table
#' @description Goodness of fit test from a table, when the cell probabilities are given.
#' @param x Frequency table.
#' @param p Probability vector, Default: rep(1/k, k).
#' @param alp Level of significance, Default: 0.05.
#' @param dtab Number of decimal places for the table, Default: 2.
#' @param dig Number of decimal places for other output, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return list(stat, df, cv, pv, tab)
#'
#' @examples
#' # Goodness-of-fit Test
#' x <- c(31,26,22,18,13,10)
#' chigof.test(x)
#'
#' x <- c(32, 65, 47, 38, 18)
#' p <- c(0.15, 0.3, 0.25, 0.2, 0.1)
#' chigof.test(x, p)
#'
#' @rdname chigof.test
#' @export
chigof.test <- function(x, p, alp=0.05, dtab=2, dig=4, ws=c(7,4)) {
    if (is.numeric(dtab)) tabf <- function(v) round(v, dtab)
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    myf2 <- function(v) format(v, FALSE, dig)

  # Number of Classes and Probability
    k <- length(x)
    if (missing(p)) p <- rep(1/k, k)
    if (sum(p) > 1) p <- p/sum(p)
    df <- k-1

  # Test Statistics
    if (is.null(names(x))) names(x) <- 1:k
    n <- sum(x)
    np <- n*p
    ress <- (x-np)^2 / np
    stat <- sum(ress)
    cv <- qchisq(1-alp, df)
    pv <- pchisq(stat, df, lower=F)

  # Summary Table
    tab <- rbind(x, np, ress)
    rownames(tab) <- c("Freq", "n*p", "Resq")

  # Print Results
    if (is.numeric(dtab)) {
        print(tabf(addmargins(tab, 2)))
        if (stat > cv) {
            sign <- " > "
            ans <- "Reject H0"
        } else {
            sign <- " < "
            ans <- "Accept H0"
        }
        ##cat("---------------------------------------------------\n")
        cat(paste0("[GoF] Stat=", myf0(stat), sign, myf1(cv), 
            ";  p-v=", myf2(pv)," \U21D2 ", ans), "\n")
    }

  # Plot the Test Results
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        xrng <- c(0, max(cv*1.1, qchisq(0.999, k-1)))
        mt <- bquote(bold("GoF Test Statistic under H0: ")~chi^2 ~( .(df)) )
        chitest.plot2(stat, df=df, alp=alp, main=mt, xlim=xrng)
    }
    invisible(list(stat=stat, df=k-1, cv=cv, pv=pv, tab=tab))
}

# [12-2] Goodness of Fit Test for Poisson/Binomial from a Frequency Table
#' @title Goodness of Fit Test for Poisson or Binomial Distribution
#' @description Goodness of fit test for Poisson or binomial distribution from a frequency table.
#' @param tab Frequency table.
#' @param par Parameters (Poisson: mean, Binomial: c(n,p).)
#' @param dist Distribution name ('p' or 'b'), Default: 'p'
#' @param mc1 Lower category numbers to merge.
#' @param mc2 Upper category numbers to merge.
#' @param alp Level of significance, Default: 0.05.
#' @param dtab Number of decimal places for the table, Default: 2.
#' @param dig Number of decimal places for other output, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return list(stat, df, cv, pv, tab)
#'
#' @examples
#' # Goodness-of-fit Test (Poisson/Binomial)
#' set.seed(1234)
#' x <- rpois(100,5)
#' mytab <- table(x)
#' pbgof.test(mytab, 5)
#' pbgof.test(mytab)
#' pbgof.test(mytab, mc1=8:9)
#'
#' pbgof.test(mytab, c(10, 0.5), dist="b")
#' pbgof.test(mytab, 10, dist="b")
#' 
#' @rdname pbgof.test
#' @export
pbgof.test <- function(tab, par, dist="p", mc1, mc2, alp=0.05, dtab=2, dig=4, ws=c(7,4)) {
    if (is.numeric(dtab)) tabf <- function(v) round(v, dtab)
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    myf2 <- function(v) format(v, FALSE, dig)

  # Frequency Table
    y <- as.numeric(names(tab))
    f <- as.numeric(tab)
    N <- sum(f)

  # Number of Classes and Probability
    k <- length(tab)
    miss.para <- FALSE
    # Poisson ----
    if (grepl(tolower(dist), "poisson")) {
        if (missing(par)) {
            par <- sum(y*f)/N
            miss.para <- TRUE
        }
        p <- dpois(y, par[1])
        p[1] <- ppois(y[1], par[1])
        p[k] <- 1 - ppois(y[k-1], par[1])
    # Binomial ----
    } else if (grepl(tolower(dist), "binomial")) {
        if (missing(par)) stop("The number of trials (n) is required.")
        if (length(par)==1) {
            par <- c(par, sum(y*f)/(par*N))
            miss.para <- TRUE
        }
        p <- dbinom(y, par[1], par[2])
        p[1] <- pbinom(y[1], par[1], par[2])
        p[k] <- 1 - pbinom(y[k-1], par[1], par[2])
    }
    np <- N*p
    if (y[1]>0) names(tab)[1] <- paste0("~",y[1])
    names(tab)[k] <- paste0(y[k],"~")
    zz <- names(tab)
    tab <- rbind(tab, p, np)
    tab1 <- tab

  # Print the Expected Frequency
    if ((!missing(mc1))|(!missing(mc2))) {
      rownames(tab) <- c("freq", "p", "n*p")
      etab <- addmargins(tab, 2)
      ##print(dtab(etab[3, ]))
      ##cat("---------------------------------------------------\n")
    }

  # Merge Classes ------------------------------------------
    # Lower side ----
    if (!missing(mc1)) {
      mc11 <- min(mc1)
      mc12 <- max(mc1)
      rc1 <- (mc11+1):mc12
      # Merge
      tab1[1,mc11] <- sum(tab[1,mc1])
      tab1[2,mc11] <- sum(tab[2,mc1])
      tab1[3,mc11] <- sum(tab[3,mc1])
      colnames(tab1)[mc11] <- paste(zz[mc11], zz[mc12], sep=":")
    }
    # Upper side ----
    if (!missing(mc2)) {
      mc21 <- min(mc2)
      mc22 <- max(mc2)
      rc2 <- (mc21+1):mc22
      # Merge
      tab1[1,mc21] <- sum(tab[1,mc2])
      tab1[2,mc21] <- sum(tab[2,mc2])
      tab1[3,mc21] <- sum(tab[3,mc2])
      colnames(tab1)[mc21] <- paste(zz[mc21], zz[mc22], sep=":")
    }
    # Remove merged Cells
    if (!missing(mc2))  tab1 <- tab1[, -rc2]
    if (!missing(mc1))  tab1 <- tab1[, -rc1]

  # Test Statistics
    ress <- (tab1[1, ] - tab1[3, ])^2 / tab1[3, ]
    stat <- sum(ress)
    lt <- ncol(tab1)
    df <- lt - 1
    if (miss.para) df <- df - 1
    cv <- qchisq(1-alp, df)
    pv <- pchisq(stat, df, lower=F)

  # Print Results
    if (is.numeric(dtab)) {
        tab1 <- rbind(tab1, ress)
        rownames(tab1) <- c("Freq", "p", "n*p", "Resq")
        print(tabf(addmargins(tab1, 2)))
        if (stat > cv) {
            sign <- " > "
            ans <- "Reject H0"
        } else {
            sign <- " < "
            ans <- "Accept H0"
        }
        ##cat("---------------------------------------------------\n")
        if (grepl(tolower(dist), "poisson")) {
            cat(paste0("[GoF] Mean=", myf0(par[1]), ", Df=", df, "; "))
        } else if (grepl(tolower(dist), "binomial")) {
            cat(paste0("[GoF] n=", par[1], ", p=", myf0(par[2]), ", Df=", df, "; "))
        }
        cat(paste0("Stat=", myf0(stat), sign, myf0(cv), 
           "; p-v =", myf1(pv), " \U21D2 ", ans), "\n")
    }

  # Plot the Test Results
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        xrng <- c(0, max(cv*1.1, qchisq(0.999, k-1)))
        mt <- bquote(bold("GoF Test Statistic under H0: ")~chi^2 ~( .(df)) )
        chitest.plot2(stat, df=df, alp=alp, main=mt, xlim=xrng)
    }
    invisible(list(stat=stat, df=df, cv=cv, pv=pv, tab=tab1))
}

# [12-3] Chi-square Test for Cross Tabulation Analysis
#' @title Chi-square Test for Cross Tabulation Analysis
#' @description Chi-square test of homogeneity or independence for a cross table.
#' @param x Cross Table of frequency.
#' @param v1 Row variable name.
#' @param v2 Column variable name.
#' @param alp Level of significance, Default: 0.05.
#' @param dtab Number of decimal places for the table, Default: 2.
#' @param dig Number of decimal places for other output, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return list(stat, df, cv, pv, exp, ress)
#'
#' @examples
#' x <- c(39,18,12,31,14, 35,23,18,35,13,  27,16,17,24,8,  9,12,8,19,22)
#' x <- matrix(x, nrow=4, ncol=5, byrow=TRUE)
#' rownames(x) <- c("Kor", "Eng", "Math", "Etc")
#' colnames(x) <- c("Sam", "Pub", "Exp", "Sal", "Etc")
#' cross.test(x)
#'
#' data(Hospital, package="vcd")
#' cross.test(Hospital)
#'
#' @rdname cross.test
#' @export
cross.test <- function(x, v1, v2, alp=0.05, dtab=2, dig=4, ws=c(7,4)) {
    if (is.numeric(dtab)) tabf <- function(v) round(v, dtab)
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    myf2 <- function(v) format(v, FALSE, dig)

  # Number of Classes and Expectation
    r <- nrow(x)
    c <- ncol(x)
    if (is.null(rownames(x))) {
        if (missing(v1)) {
            rownames(x) <- 1:r
        } else {
            rownames(x) <- paste0(v1, 1:r)
        }
    }
    if (is.null(colnames(x))) {
        if (missing(v2)) {
            colnames(x) <- 1:c
        } else {
            colnames(x) <- paste0(v2, 1:c)
        }
    }
    tab0 <- addmargins(x)

  # Test Statistic
    ex <- outer(tab0[1:r, c+1], tab0[r+1, 1:c], "*")/tab0[r+1,c+1]
    ress <- (x-ex)^2 / ex
    stat <- sum(ress)
    df <- (r-1)*(c-1)
    cv <- qchisq(1-alp, df)
    pv <- pchisq(stat, df, lower=F)
    rownames(ex) <- rownames(ress) <- rownames(x)
    colnames(ex) <- colnames(ress) <- colnames(x)

  # Print Results
    if (is.numeric(dtab)) {
        cat("[Expected Frequency]  =>  [Residual Square]", 
            "---------------------\n")
        jtab <- cbind(addmargins(ex), rep(NA,r+1), addmargins(ress))
        jdf <- as.data.frame(tabf(jtab))
        jdf[, c+2] <- rep(" | ",r+1)
        names(jdf)[c+2] <- " | "
        print(jdf)
    }
    ## cat("[Test Summary] ---------------------------------\n")
    if (stat > cv) {
        sign <- " > "
        ans <- "Reject H0"
    } else {
        sign <- " < "
        ans <- "Accept H0"
    }
    cat(paste0("[GoF] Stat=", myf0(stat), sign, myf0(cv), 
        "; p-v=", myf2(pv), " \U21D2 ", ans), "\n")

  # Plot the Test Results
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        ##xrng <- c(0, max(cv*1.1, qchisq(0.999, df)))
        mt <- bquote(bold("GoF Test Statistic under H0: ")~chi^2 ~( .(df)) )
        chitest.plot2(stat, df=df, alp=alp, main=mt)
    }
    invisible(list(stat=stat, df=df, cv=cv, pv=pv, exp=ex, res=ress))
}

# [12-4] Mosaic Plot with Pearson Residuals for a Cross Table
#' @title Mosaic Plot with Pearson Residuals for a Cross Table
#' @description Mosaic plot for a cross table displaying Pearson residuals.
#' @param tab Cross table of frequency.
#' @param main Graph title.
#' @param dig Number of decimal places, Default: 4.
#' @param resid Logical: display Pearson residuals? Default: TRUE.
#' @param ws Graphic window size, Default: c(7,6).
#' @return Object from chisq.test()
#'
#' @examples
#' x <- c(39,18,12,31,14, 35,23,18,35,13,  27,16,17,24,8,  9,12,8,19,22)
#' x <- matrix(x, nrow=4, ncol=5, byrow=TRUE)
#' Subject <- c("Kor", "Eng", "Math", "Etc")
#' Hope <- c("Sam", "Pub", "Exp", "Sal", "Etc")
#' t1 <- as.table(x)
#' dimnames(t1) <- list(Subject=Subject, Hope=Hope)
#' # require(vcd)
#' mosaic.res(tab=t1, main="Students' Favorite Subject and Hope")
#'
#' data(Hospital, package="vcd")
#' mosaic.res(Hospital)
#' @rdname mosaic.res
#' @export
mosaic.res <- function(tab, main, dig=4, resid=TRUE, ws=c(7,6)) {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    myf2 <- function(v) format(v, FALSE, dig)

  # [Correction]
    if (missing(main)) main <- paste("Mosaic Plot of", deparse(substitute(tab)))

  # Chi-square Test
    ct <- chisq.test(tab)

  # Mosaic plot
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        vcd::mosaic(tab, shade=T, pop=F, main=main,
            labeling_args=list(offset_varnames=c(top=1), 
                           offset_labels=c(top=0.3)))
      # Display the Pearson residual in each cell
        if (resid) vcd::labeling_cells(text=round(ct$res, 1), clip=FALSE)(tab)
    }

  # Print
    if (is.numeric(dig)) {
      cat(paste0("Test statistic =", myf0(ct$stat), "; df=", ct$para,
          "; p-v=", myf2(ct$p.val)), "\n")
    }
    invisible(ct)
}

# [12-5] Plot of the Chi-square Test
#' @title Plot the Chi-square Test
#' @description Plot the result of a chi-square test.
#' @param stat Chi-square test statistic.
#' @param df Degrees of freedom.
#' @param alp Level of significance, Default: 0.05.
#' @param alt Type of the alternative hypothesis, Default: 'gr'.
#' @param dig Number of decimal places, Default: 4.
#' @param ... Other graphic parameters.
#' @return None.
#'
#' @examples
#' # Goodness-of-fit Test
#' x <- c(31,26,22,18,13,10)
#' (ct <- chisq.test(x))
#' chitest.plot2(stat=ct$stat, df=ct$para)
#'
#' # Test of Homogeneity
#' x <- c(20,16,29,21,14,  14,22,26,25,13,  18,24,32,18, 8,  8,18,33,16,25)
#' x <- matrix(x, nrow=4, ncol=5, byrow=TRUE)
#' (ct <- chisq.test(x))
#' chitest.plot2(stat=ct$stat, df=ct$para)
#'
#' @rdname chitest.plot2
#' @export
chitest.plot2 <- function(stat, df, alp=0.05, alt="gr", dig=4, ...) {
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)
    myf2 <- function(v) format(v, FALSE, dig)

  # Find the relevant alternative (1="less", 2="greater", 3="two sided")
    alt.num <- function(alt) {
        nalt <- grep(paste0("^", alt), c("less", "greater", "two.sided"))
        if (length(nalt)>=2) nalt <- max(nalt)
        if (length(nalt)==0) nalt <- grep(paste0("^", alt), 
                          c("lower", "upper", "two-sided"))
        invisible(nalt)
    }
    nalt <- alt.num(alt)

  # Set the critical value and plot range
    rej <- qchisq(1-alp, df)
    prng <- c(0,qchisq(0.999, df))
    ppt <- 20

  # Plot the PDF
    mt <- bquote(bold("Distribution of the Chi-square Statistic under H0: ")
                 ~chi^2 ~( .(df)) )
    xl <- "Chi-square Statistic"
    yl <- pdf
    xmod <- max(df-2, 0)
    ymax <- dchisq(xmod, df)
    yrng <- c(0, ymax*1.1)
    fill <- "lightcyan"
    lwd <- 2
    col <- text.col <- "blue"
    pv.col <- cv.col <- "red"
    cex <- 1

    dots <- list(...)
    if (length(dots)>0) {
        par <- names(dots)
        if ("main" %in% par) mt <- dots$main
        if ("xlab" %in% par) xl <- dots$xlab
        if ("ylab" %in% par) xl <- dots$ylab
        if ("ylim" %in% par) yrng <- dots$ylim
        if ("xlim" %in% par) prng <- dots$xlim
        if ("fill" %in% par) fill <- dots$fill
        if ("lwd" %in% par) lwd <- dots$lwd
        if ("col" %in% par) col <- dots$col
        if ("pv.col" %in% par) pv.col <- dots$pv.col
        if ("text.col" %in% par) text.col <- dots$text.col
        if ("pv.col" %in% par) pv.col <- dots$pv.col
        if ("cex" %in% par) cex <- dots$cex
    }

    xa <- seq(prng[1], prng[2], length.out=101)
    plot(xa, dchisq(xa, df), type="n", xlab=xl, ylab=xl,
        ylim=yrng, main=mt)
    abline(h=0)

    # P-value and the critical region

    plow <- pchisq(stat, df)
    if (nalt==2) {
        pv <- 1-plow
        if (pv > 0.5) ppt <- 50
        cord.x <- c(stat, seq(stat, prng[2], length.out=ppt), prng[2])
        cord.y <- c(0, dchisq(seq(stat, prng[2], length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col=fill)
      # Display the critical value
        rejy <- dchisq(rej, df)
        rejy <- (rejy+ymax)/2
        staty <- (dchisq(stat, df)+ymax)/2

        segments(stat, 0, stat, staty, lwd=1, col="blue")
        xpv <- ifelse(stat>qchisq(0.5, df), (stat+prng[2])/2, stat)
        ypv <- ifelse(stat>qchisq(0.5, df), dchisq(xpv, df), dchisq(stat, df)/2)
        pospv <- ifelse(stat>qchisq(0.5, df), 3, 4)
        text(xpv, ypv, myf1(pv), pos=pospv, col=pv.col, cex=cex)

        segments(rej, 0, rej, rejy, lwd=1, col="red")
        ry0 <- ifelse (abs(rej-stat)<2, rejy, 0)

        postat <- ifelse(stat>rej, 4, 2)
        if (pv < 0.0001) postat <- 2
        if (pv>0.7) postat <- 3
        text(stat, staty, labels=bquote(chi[0]^2 == .(myf1(stat))),
            pos=postat, col=text.col, cex=cex)
        text(rej, rejy, labels=bquote(chi[.(paste(1-alp,";",df))]^2 == .(myf1(rej))),
            pos=3, col=cv.col, cex=cex)
    } else if (nalt==1) {
        pv <- plow
        if (pv > 0.5) ppt <- 50
        # cat("P-v =", myf1(pv), "\n")
        cord.x <- c(prng[1], seq(prng[1], stat, length.out=ppt), chis)
        cord.y <- c(0, dchisq(seq(prng[1], stat, length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(stat, 0, stat, dchisq(stat, df), lwd=2, col="red")
        text(stat, dchisq(stat, df), myf1(pv), pos=2, col=pv.col, cex=cex)
        text(stat, 0, myf1(stat), pos=1, col=text.col, cex=cex)
    } else if (nalt==3) {
        pv <- 2*min(plow, 1-plow)
        if (pv > 0.5) ppt <- 30
        # cat("P-v =", myf1(pv), "\n")
        mlow <- qchisq(pv/2, df)
        mup <- qchisq(1-pv/2, df)
        cord.x <- c(mup, seq(mup, prng[2], length.out=ppt), prng[2])
        cord.y <- c(0, dchisq(seq(mup, prng[2], length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col=fill)
        cord.x <- c(prng[1], seq(prng[1], mlow, length.out=ppt), mlow)
        cord.y <- c(0, dchisq(seq(prng[1], mlow, length.out=ppt), df), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(c(mlow, mup), 0, c(mlow, mup), dchisq(c(mlow, mup), df), lwd=2, col="red")
        text(c(mlow, mup), dchisq(c(mlow, mup), df), myf1(pv/2), 
               pos=c(2,4), col=pv.col, cex=cex)
        text(c(mlow, mup), 0, myf1(c(mlow, mup)), pos=1, col=text.col, cex=cex)
    }
    abline(v=qchisq(0.5, df), lty=2, lwd=lwd, col="green3")
    lines(xa, dchisq(xa, df), type="l", lwd=lwd, col=col)

}
