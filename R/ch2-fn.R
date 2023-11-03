# [Ch-2 Functions] -------------------------------------------------------
# [Ch-2 Function Manual] -----------------------------------------

#' Chapter 2 Functions Manual
#'
#' This function allows you to review the 7 functions in ch2.
#' @param fn Number 0 to 7 for function. Defaults to 0.
#' @keywords Ch2 Descriptive Statistics
#' @return None
#' @examples
#' ch2.man()
#' ch2.man(3:5)
#' @export
ch2.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] freq.table\t\t Create a Frequency Table\n")
        cat("[2] mult.hist\t\t Draw Multiple Histograms from a Data Frame\n")
        cat("[3] unstable.hist \t Draw Histograms of Unstable Processes\n")
        cat("[4] strat.hist\t\t Draw Stratified Histograms\n")
        cat("[5] strat.hist2\t\t Draw Stratified Histograms from a Data Frame\n")
        cat("[6] corr.plot6\t\t Draw Scatter Plots of Six Cases\n")
        cat("[7] scat.lm\t\t Draw a Scatter Plot with Regression Line\n")
        cat("[8] location.est \t Calculate Measures of Location\n")
        cat("[9] spread.est\t\t Calculate Measures of Dispersion\n")
    }
    if (1 %in% fn) {
        cat("[1] Create a Frequency Table\n")
        cat("freq.table(x, breaks, dig=4, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x  \t Data vector\n")
        cat("[Optional Input]--------------------------\n")
        cat("breaks\t Vector of class limits.\n")
        cat("dig \t The number of decimal places, Default: 4.\n")
        cat("ws \t Graphic window size, Default: \"n\".\n")
        cat("...   \t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Draw Multiple Histograms from a Data Frame\n")
        cat("mult.hist(data, breaks, ws, mfd, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("data \t Data frame of input data.\n")
        cat("[Optional Input]--------------------------\n")
        cat("breaks \t Break method, Default: Sturges.\n")
        cat("ws \t Graphic window size vector.\n")
        cat("mfd \t Multi-frame dimension.\n")
        cat("... \t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Draw Histograms of Unstable Processes\n")
        cat("unstable.hist(N=200, mu=c(10,6), sig=c(1,0.5), miss=c(8,9), cut=9, ws, mfd, ...)\n")
        cat("[Optional Input]--------------------------\n")
        cat("N  \t Number of data for each histogram, Default: 200.\n")
        cat("mu \t Means of two normal distributions, Default: c(10,6).\n")
        cat("sig \t Standard deviation of the distributions, Default: c(1,0.5).\n")
        cat("miss \t Limits of the missing values, Default: c(8,9).\n")
        cat("cut \t Cut-off value, Default: 9.\n")
        cat("ws \t Graphic window size.\n")
        cat("mfd \t Multi-frame dimension vector.\n")
        cat("... \t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] Draw Stratified Histograms\n")
        cat("strat.hist(ng=3, n=200, mu=2+4*1:ng, sig=1, spec, ws, mfd, ...)\n")
        cat("[Optional Input]--------------------------\n")
        cat("ng \t Number of subgroups, Default: 3.\n")
        cat("n  \t Number of data for each subgroup, Default: 200.\n")
        cat("mu \t Means of subgroups, Default: 2+4*1:ng.\n")
        cat("sig \t Standard deviations of subgroups, Default: 1.\n")
        cat("spec \t Specification limits, Default: c(min(mu)-2,max(mu)+2).\n")
        cat("ws \t Graphic window size.\n")
        cat("mfd \t Multi-frame dimension.\n")
        cat("... \t Other graphic parameters.\n")
    }
    if (5 %in% fn) {
        cat("[5] Draw Stratified Histograms from a Data Frame\n")
        cat("strat.hist2(df, cdep, cfac, spec, br, col, ws, mfd, mar, prob=FALSE)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("df \t Data frame of input data.\n")
        cat("cdep \t Column numbers of dependent variables.\n")
        cat("cfac \t Column number of stratifying factor.\n")
        cat("[Optional Input]--------------------------\n")
        cat("spec \t Specification limits.\n")
        cat("col \t Colors of the histograms.\n")
        cat("ws \t Graphic window size.\n")
        cat("mfd \t Multi-frame dimension vector.\n")
        cat("mar \t Vector of plot margins.\n")
        cat("prob\t Logical: plot density instead of frequency? Default: FALSE.\n")
    }
    if (6 %in% fn) {
        cat("[6] Draw Scatter Plots of Six Cases\n")
        cat("corr.plot6(m1 = 60, s1=10, m2=60, s2=10, r=0.7, r2=-0.8, n=50, ws=c(9,6))\n")
        cat("[Optional Input]--------------------------\n")
        cat("m1 \t Mean of X, Default: 60.\n")
        cat("s1  \t Standard deviation of X, Default: 10.\n")
        cat("m2 \t Mean of Y, Default: 60.\n")
        cat("s2 \t Standard deviation of Y, Default: 10.\n")
        cat("r   \t Correlation coefficient of X and Y, Default: 0.7.\n")
        cat("r2  \t Correlation coefficient (stratified case), Default: -0.8.\n")
        cat("n   \t Number of data pairs, Default: 50.\n")
        cat("ws \t Graphic window size, Default: c(9,6).\n")
    }
    if (7 %in% fn) {
        cat("[7] Draw a Scatter Plot with Regression Line\n")
        cat("scat.lm(x, y, data, dig=2, ws, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x  \t Data vector for x-axis.\n")
        cat("y  \t Data vector for y-axis.\n")
        cat("[Optional Input]--------------------------\n")
        cat("data \t Data set containing x and y.\n")
        cat("dig \t Number of decimal places, Default: 2.\n")
        cat("ws \t Graphic window size.\n")
        cat("...  \t Other graphic parameters.\n")
    }
    if (8 %in% fn) {
        cat("[8] Calculate Measures of Location\n")
        cat("location.est(x, tr=0.1, mode=\"near\", detail=FALSE, dig=4)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x  \t Data vector or matrix.\n")
        cat("[Optional Input]--------------------------\n")
        cat("tr \t Trim proportion of the trimmed mean, Default: 0.1.\n")
        cat("mode\t Selection of the mode, c(\"near\", \"first\", \"last\").\n")
        cat("detail\t Logical: print detailed output? Default: FALSE.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
    }
    if (9 %in% fn) {
        cat("[9] Calculate Measures of Dispersion\n")
        cat("spread.est(x, detail=FALSE, dig=4)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x  \t Data vector or matrix.\n")
        cat("[Optional Input]--------------------------\n")
        cat("detail\t Logical: print detailed output? Default: FALSE.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
    }
}

# [2-1] Create a Frequency Table

#' Create a Frequency Table
#'
#' To create a frequency table for a numeric vector (or matrix) x.
#' @param x Data vector (or matrix).
#' @param breaks Breaks (or number) of intervals.
#' @param dig The number of decimal places. Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @keywords Ch2 Descriptive Statistics
#' @return xtab Frequency Table
#' @examples
#' set.seed(1234)
#' freq.table(rnorm(100))
#' freq.table(rnorm(100), ws=c(7,5), col=7)
#' @export
freq.table <- function(x, breaks, dig=4, ws="n", ...) {
    x <- as.vector(x)
    n <- length(x)
  # class limits
    if (missing(breaks)) breaks <- hist(x, plot=FALSE)$breaks

  # statistics
    xh <- hist(x, breaks=breaks, plot=FALSE)
    cuts <- xh$breaks
    xcf <- cumsum(xh$counts)
    xrf <- xh$counts/n
    xrcf <- xcf/n

  # frequency table
    ng <- length(cuts)
    xclass <- paste0(c("[", rep("(", ng-2)), cuts[-ng], ", ", cuts[-1], "]")
    xtab <- cbind(xh$mids, xh$counts, xcf, round(xrf, dig), round(xrcf, dig))
    rownames(xtab) <- xclass
    colnames(xtab) <- c("Center", "Freq", "Cum-Fr", "Rel-Fr", "Rel-CFr")
  # Plot
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        hist(x, breaks=breaks, ...)
    }
    return(xtab)
}

# [2-2] Draw Multiple Histograms from a Data Frame

#' Multiple Histograms
#'
#' To draw multiple histograms from a data frame.
#' @param data Data frame of input data.
#' @param breaks Break method Default: Sturges.
#' @param ws Graphic window size vector.
#' @param mfd Multi-frame dimension vector.
#' @param ... Other graphic parameters.

#' @param mt Vector of main titles.
#' @param xl Vector of x labels.
#' @param col Vector of colors for the histograms.
#' @param mar Margin vector of the plot.
#' @return None.
#' @keywords Ch2. Descriptive Statistics
#' @examples
#' mult.hist(iris[1:4])
#' mult.hist(iris[1:4], col=rainbow(4))
#' mult.hist(iris[1:4], col=rainbow(4), ws=c(12,4), mfd=c(1,4))
#' mult.hist(mtcars[1:7], col=rainbow(7))
#' @export
mult.hist <- function(data, breaks, ws, mfd, ...) {
  # Check Input
    if (missing(data)) stop("Input data as a data frame with numeric data.")
    if (!is.list(data)) stop("Input data as a data frame with numeric data.")
    ng <- length(data)
    if (ng > 16) stop("The number of groups cannot exceed 16.")
    if (missing(breaks)) {
        mybr <- rep("Sturges", ng)
    } else {
        mybr <- br
    }

    mt <- paste("Histogram of", names(data))
    xl <- names(data)
    col <- NULL

    dots <- list(...)
    if (length(dots)>0) {
        pars <- names(dots)
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt) < ng) mt <- rep(mt, ceiling(ng/length(mt)))
        }
        if ("xlab" %in% pars) {
            xl <- dots$xlab
            if (length(xl) < ng) xl <- rep(xl, ceiling(ng/length(xl)))
        }
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col) < ng) col <- rep(col, ceiling(ng/length(col)))
        }
    }

    if (missing(ws)) {
        ww <- c(4,6,9,7,9, 9,9,9,9,11, 11,11,11,11,11, 11,13,13,13,13)
        wl <- c(3,3,3,6,6, 6,6,6,9,9, 9,9,11,11,11, 11,11,11,11,11)
        win.graph(ww[ng], wl[ng])
    } else if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
    }

    if (missing(mfd)) {
        wc <- c(1,2,3,2,3, 3,4,4,3,4, 4,4,4,4,4, 4,5,5,5,5)
        wr <- c(1,1,1,2,2, 2,2,2,3,3, 3,3,4,4,4, 4,4,4,4,4)
        par(mfrow=c(wr[ng], wc[ng]))
    } else par(mfrow=c(mfd[1], mfd[2]))

  # Histograms
    for (k in 1:ng) hist(data[[k]], breaks=mybr[k], 
                    main=mt[k], xlab=xl[k], col=col[k])
}

# [2-3] Draw Histograms of Unstable Processes

#' Unstable Histograms
#'
#' To create four types of unstable histograms.
#' @param N Number of random variates, Default: 200.
#' @param mu Means of two normal distributions, Default: c(10,6).
#' @param sig Standard deviations of two distributions, Default: c(1,0.5).
#' @param miss Limits of the missing values, Default: c(8,9).
#' @param cut Cut-off value, Default: 9.
#' @param ws Graphic window size vector.
#' @param mfd Multi-frame dimension vector.
#' @param ... Other graphic parameters.
#' @keywords Ch2 Descriptive Statistics
#' @return None
#' @examples
#' unstable.hist()
#' unstable.hist(mu=c(8,15), sig=c(1.5,0.6), miss=c(9,10), cut=10, col=2:5)
#' unstable.hist(col=2:5, ws=c(12,4), mfd=c(1,4))
#' @export
unstable.hist <- function(N=200, mu=c(10,6), sig=c(1,0.5), miss=c(8,9), cut=9, 
	ws, mfd, ...) {
  # Create uniform probability vectors (0.9N, 0.5N, 0.1N)
    p1 <- (1:(0.9*N))/(0.9*N+1)
    p2 <- (1:(0.5*N))/(0.5*N+1)
    p3 <- (1:(0.1*N))/(0.1*N+1)
  # Assign parameters
    m1 <- mu[1]
    m2 <- mu[2]
    s1 <- sig[1]
    s2 <- sig[2]
    a <- miss[1]
    b <- miss[2]

  # Create quantiles of N(m1, s1^2) distribution (0.9N, 0.5N)
    x1 <- qnorm(p1, mean=m1, sd=s1)
    x2 <- qnorm(p2, mean=m1, sd=s1)
  # Create quantiles of N(m2, s1^2) and N(m2, s2^2) distribution (0.5N, 0.1N)
    y1 <- qnorm(p2, mean=m2, sd=s1)
    y2 <- qnorm(p3, mean=m2, sd=s2)

  # Create Data
    df <- list()
  # (a) Iland type : 90%(x1) + 10%(y2)
    df[[1]] <- c(x1, y2)
  # (b) Camel type : 50%(x2) +50%(y1)
    df[[2]] <- c(x2, y1)
  # (c) Sink type : remove values of x1 in [a, b]
    df[[3]] <- x1[(x1>b) | (x1<a)]
  # (d) Cliff type : remove values of x1 below or above c
    # [Correction]
    if (cut < m1) df[[4]] <- x1[x1>=cut] else df[[4]] <- x1[x1<=cut]

  # Set titles and labels
    mt <- c("Isolated Island", "Multiple Peaks", "Missing Values", "Cliff Shape")
    xl <- paste0("(", letters[1:4], ")")
    col <- rep("cyan", 4)
    brk <- c(15,15,12,12)

    dots <- list(...)
    if (length(dots)>0) {
        pars <- names(dots)
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt) < 4) mt <- rep(mt, ceiling(4/length(mt)))
        }
        if ("xlab" %in% pars) {
            xl <- dots$xlab
            if (length(xl) < 4) xl <- rep(xl, ceiling(4/length(xl)))
        }
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col) < 4) col <- rep(col, ceiling(4/length(col)))
        }
        if ("breaks" %in% pars) {
            brk <- dots$breaks
            if (length(brk) < 4) brk <- rep(brk, ceiling(4/length(brk)))
        }
    }

    if (!missing(ws)) win.graph(ws[1], ws[2])
    if (missing(mfd)) par(mfrow=c(2,2)) else par(mfrow=mfd)

  # making histograms
    for (k in 1:4) 
        hist(df[[k]], breaks=brk[k], main=mt[k], xlab=xl[k], col=col[k])
}

# [2-4] Draw Stratified Histograms

#' Stratified Histograms
#'
#' To create Stratified Histograms from a mixed data set.
#' @param ng Number of subgroups, Default: 3.
#' @param n Number of data for each subgroup, Default: 200.
#' @param mu Means of subgroups, Default: 2+4*1:ng.
#' @param sig Standard deviations of subgroups, Default: 1.
#' @param spec Specification limits, Default: c(min(m)-2, max(m)+2).
#' @param ws Graphic window size vector.
#' @param mfd      Multi-frame dimension vector
#' @param ... Other graphic parameters.
#' @keywords Ch2 Descriptive Statistics.
#' @return None
#' @examples
#' strat.hist()
#' strat.hist(ng=4, mu=c(6,10,14,18), ws=c(9,6), spec=c(4,20))
#' strat.hist(ng=5, n=400, ws=c(9,6), prob=T)
#' @export
strat.hist <- function(ng=3, n=200, mu=2+4*1:ng, sig=1, spec, ws, mfd, ...)
{
    if (ng >=9) stop("Number of class should be less than 9!")
    if (length(sig)==1) sig <- rep(sig, ng)
    if (missing(spec)) spec <- c(min(mu)-2, max(mu)+2)

  # creating uniform probability vector (n)
    p <- (1:n)/(n+1)
  # creating quantiles of N(mu, sig^2) distribution (n each)
    x <- vector("list", ng)
    for (k in 1:ng) x[[k]] <- qnorm(p, mean=mu[k], sd=sig[k])

  # combining data
    xd <- x[[1]]
    for (k in 2:ng) xd <- c(xd, x[[k]])
    nb1 <- ceiling(sqrt(n*ng))
    nb2 <- ceiling(sqrt(n))

  # adjusting graphic parameters
    nr <- ifelse(ng<=5, 2, 3)
    nc <- ceiling((ng+1)/nr)

  # Breaks and ylim
    xh <- hist(xd, breaks=nb1, plot=F)
    brk <- xh$breaks
    ym <- max(xh$counts)

  # Set titles and axes
    mt <- paste0("group-", LETTERS[1:ng])
    xrng <- c(min(mu[1]-3*sig[1], spec[1]-2*sig[1]), 
	max(mu[ng]+3*sig[ng], spec[2]+2*sig[ng]))
    col <- c("orange", rep("cyan", ng))

    dots <- list(...)
    if (length(dots)>0) {
        pars <- names(dots)
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt) < ng) mt <- rep(mt, ceiling(ng/length(mt)))
        }
        if ("xlim" %in% pars) {
            xrng <- dots$xlim
        }
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col) < ng) col <- rep(col, ceiling(ng/length(col)))
        }
        if ("breaks" %in% pars) {
            brk <- dots$breaks
        }
    }

  # making histograms
    if (!missing(ws)) win.graph(ws[1], ws[2])
    if (missing(mfd)) par(mfrow=c(nr, nc)) else par(mfrow=mfd)
    par(mar=c(3,3,4,1))

  # Plot
    hist(xd, breaks=brk, main="All Data", 
        ylab="", xlab="", xlim=xrng, col=col[1])

    brk <- xh$breaks
    segments(spec, 0, spec, ym/2, lwd=2, col="red")
    text(spec, c(ym/2,ym/2), c("SL", "SU"), col="red", pos=3)

    for (k in 1:ng) {
        hist(x[[k]], breaks=brk, main=mt[k],
        ylab="", xlab="", ylim=c(0, ym), xlim=xrng, col=col[k+1])
        segments(spec, 0, spec, ym/2, lwd=2, col="red")
        text(spec, c(ym,ym)/2, c("SL", "SU"), col="red", pos=3) 
    }
}

# [2-5] Draw Stratified Histograms with a Data Frame

#' Stratified Histograms with a Data Frame
#'
#' To draw stratified histograms using a data frame.
#' @param df Data frame of input data.
#' @param cdep The column of the dependent variable.
#' @param cfac The column of the stratifying factor.
#' @param spec Specification limits.
#' @param br Break method Default: Sturges.
#' @param col Vector of colors for the histograms.
#' @param ws Graphic window size vector.
#' @param mfd Multi-frame dimension vector.
#' @param mar Margin vector of the plot.
#' @param prob Logical value for density Default: FALSE.
#' @return None.
#' @keywords Ch2. Descriptive Statistics
#' @examples
#' strat.hist2(iris, 1, 5)
#' strat.hist2(iris, 1, 5, prob=T)
#' strat.hist2(iris, 1, 5, spec=c(4,7), prob=T)
#' for (k in 1:4) strat.hist2(iris, k, 5)
#' @export
strat.hist2 <- function(df, cdep, cfac, spec, br, col, ws, mfd, mar, prob=FALSE) {
    if (missing(df)) stop("Please input a data frame with numeric data.")
    if (missing(cdep)) stop("Please input the column of the dependent variable.")
    if (missing(cfac)) stop("Please input the column of the factor.")

    # Assign variables
    x <- df[[cdep]]
    n <- length(x)
    fac <- df[[cfac]]
    if (!is.factor(fac)) fac <- as.factor(fac)

    # Number of Groups
    gname <- levels(fac)
    ng <- length(gname)
    if (ng > 8) stop("The number of groups must be 8 or less.")

    # Breaks for the histogram
    if (missing(br)) {
        mybr <- rep("Sturges", ng)
    } else {
        mybr <- br
    }

    # set up parameters
    if (missing(col)) col <- c("orange", rep("cyan", ng))
    nb1 <- ceiling(sqrt(n))
    xl <- range(x)
    if (!missing(spec)) xl <- c(min(xl[1],spec[1]), max(xl[2],spec[2]))
    rng <- xl[2]-xl[1]
    xl <- xl + 0.05*rng*c(-1, 1)

    if (missing(ws)) {
        ww <- c(7,7,7, 9,9, 9,9,9)
        wl <- c(3,6,6, 6,6, 9,9,9)
        win.graph(ww[ng], wl[ng])
    } else win.graph(ws[1], ws[2])

    if (missing(mfd)) {
        wc <- c(2,2,2, 3,3, 3,3,3)
        wr <- c(1,2,2, 2,2, 3,3,3)
        par(mfrow=c(wr[ng], wc[ng]))
    } else par(mfrow=c(mfd[1], mfd[2]))

    if (missing(mar)) par(mar=c(3,3,4,1))

    # modify
    xh <- hist(x, breaks=nb1, plot=F)
    brk <- xh$breaks
    if (prob) {
        ymg <- vector()
        for (k in 1:ng) ymg[k] <- max(hist(x[fac==gname[k]], breaks=brk, plot=F)$density)
        ymax <- max(ymg)
    } else {
        ymax <- max(xh$counts)
    }

    # making the main histogram
    hist(x, breaks=nb1, main=names(df)[cdep], probability=prob,
        ylab="", xlab="", xlim=xl, col=col[1])
    if (!missing(spec)) {
        segments(spec, 0, spec, ymax/2, lwd=2, col="red")
        text(spec, c(ymax/2,ymax/2), c("SL", "SU"), col="red", pos=3)
    }

    # making stratified histograms
    for (k in 1:ng) {hist(x[fac==gname[k]], breaks=brk, main=gname[k], probability=prob,
        ylab="", xlab="", ylim=c(0, ymax), xlim=xl, col=col[k+1])
        if (!missing(spec)) {
          segments(spec, 0, spec, ymax/2, lwd=2, col="red")
          text(spec, c(ymax, ymax)/2, c("SL", "SU"), col="red", pos=3) }
    }
}

# [2-6] Draw Scatter Plots of Six Cases
# creating random variables of the bivariate normal distribution
rbivariate <- function(m1, s1, m2, s2, r, n) {
    z1 <- rnorm(n)
    z2 <- rnorm(n)
    x <- sqrt(1-r^2)*s1*z1 + r*s1*z2 + m1
    y <- s2*z2 + m2
    return(list(x,y))
}

# Draw 6 types of scatter plots

#' @title Six Scatter Plots
#' @description Create 6 types of scatter plots.
#' @param m1 Mean 1, Default: 60.
#' @param s1 Standard deviation 1, Default: 10.
#' @param m2 Mean 2, Default: 60.
#' @param s2 Standard deviation 2, Default: 10.
#' @param r Correlation Coefficient 1, Default: 0.7.
#' @param r2 Correlation Coefficient 2, Default: -0.8.
#' @param n Number of samples, Default: 50.
#' @param ws Graphic window size vector, Default: c(9,6).
#' @return None
#' @keywords Ch2. Descriptive Statistics
#' @examples
#' corr.plot6()
#' corr.plot6(r=0.6, r2=0.9, n=100)
#' @export
corr.plot6 <- function(m1 = 60, s1=10, m2=60, s2=10, r=0.7, r2=-0.8, n=50, ws=c(9,6)) {
  # adjusting graphic parameters
    x1 <- floor(m1-3*s1)
    x2 <- ceiling(m1+3*s1)
    y1 <- floor(m2-3*s2)
    y2 <- ceiling(m2+3*s2)
    xa <- seq(m1-2.5*s1, m1+2.5*s1, length=n)

  # setting seed
    set.seed(9857)
    # positive correlation
    d1 <- rbivariate(m1, s1, m2, s2, r, n)
  # negative correlation
    d2 <- rbivariate(m1, s1, m2, s2, -r, n)
  # quadratic relation
    d3 <- list(xa, (m2+2*s2)-0.05*(xa-m1)^2+rnorm(n, 0, s2*0.6))
  # little correlation
    d4 <- list(rnorm(n, m1, s1), rnorm(n, m2, s2))
    d81 <- rbivariate(m1, s1, m2-1.5*s2, s2, r, n/2)
    d82 <- rbivariate(m1, s1, m2+1.5*s2, s2, r2, n/2)
    d8 <- list(c(d81[[1]], d82[[1]]), c(d81[[2]], d82[[2]]))

  # plot
    if (is.numeric(ws)) win.graph(ws[1], ws[2])
    par(mfrow=c(2,3))
  # positive correlation
    plot(d1[[1]],d1[[2]], pch=19, cex=1.2, main="(a) Positive Correlation", cex.lab=1.5,
        xlab="", ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
    abline(lm(d1[[2]]~d1[[1]]), lwd=2, lty=2, col="red")
    # negative correlation
    plot(d2[[1]],d2[[2]], pch=19, cex=1.2, main="(b) Negative Correlation", cex.lab=1.5,
        xlab="", ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
    abline(lm(d2[[2]]~d2[[1]]), lwd=2, lty=2, col="red")
    # little correlation
    plot(d4[[1]],d4[[2]], pch=19, cex=1.2, main="(c) Little Correlation", cex.lab=1.5,
        xlab="", ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
    abline(lm(d4[[2]]~d4[[1]]), lwd=2, lty=2, col="red")
    # quadratic relation
    plot(d3[[1]],d3[[2]], pch=19, cex=1.2, main="(d) Quadratic Relation", cex.lab=1.5,
        xlab="", ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
    abline(lm(d3[[2]]~d3[[1]]), lwd=2, lty=2, col="red")
    # outlier
    o1 <- c(d1[[1]][1:(n-2)], m1-2*s1, m1+2*s1)
    o2 <- c(d1[[2]][1:(n-2)], m2+2*s2, m2-2*s2)
    plot(o1, o2, pch=19, cex=1.2, main="(e) Outliers", cex.lab=1.5,
        xlab="", ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
    points(c(m1-2*s1, m1+2*s1), c(m2+2*s2, m2-2*s2), pch=0, cex=2, col="red")
    abline(lm(o2~o1), lwd=2, lty=2, col="red")
    # stratification
    plot(d8[[1]],d8[[2]], pch=19, cex=1.2, main="(f) Stratification", cex.lab=1.5,
        xlab="", ylab="", xlim=c(x1, x2), ylim=c(y1, y2))
    abline(lm(d8[[2]]~d8[[1]]), lwd=2, lty=2, col="red")
    abline(lm(d81[[2]]~d81[[1]]), lwd=2, lty=2, col="blue")
    abline(lm(d82[[2]]~d82[[1]]), lwd=2, lty=2, col="blue")
}

# [2-7] Draw a Scatter Plot with a Regression Line
#' Scatter Plot with a Regression Line
#'
#' To create a scatter plot with a linear regression line.
#' @param x Vector of x variable.
#' @param y Vector of y variable.
#' @param dig The number of decimal places, Default: 4.
#' @param ws Graphic window size.
#' @param ... Other graphic parameters.
#' @return Object lm(y~x)
#' @keywords Ch2. Descriptive Statistics
#' @examples
#' with(mtcars, scat.lm(wt, mpg))
#' scat.lm(wt, mpg, mtcars, main="Weight vs. MPG", dig=4, ws=c(7,5))
#'
#' par(mfrow=c(2,2))
#' for (k in 3:6) scat.lm(mtcars[[k]], mtcars$mpg, 
#'                       xlab=names(mtcars)[k], ylab="mpg")
#' @export
scat.lm <- function(x, y, data, dig=2, ws, ...) {

  # Set titles and axes
    xl <- deparse(substitute(x))
    yl <- deparse(substitute(y))

    if (!missing(data)) {
        x <- data[[xl]]
        y <- data[[yl]]
    }

    pch <- 19
    cex <- 1.2
    col <- "red"
    lty <- 2
    lwd <- 2

    dots <- list(...)
    pars <- names(dots)
    if ("xlab" %in% pars) xl <- dots$xlab
    if ("ylab" %in% pars) yl <- dots$ylab
    mt <- paste("Scatter Plot of", yl, "vs.", xl)
    if ("main" %in% pars) mt <- dots$main
    if ("pch" %in% pars) pch <- dots$pch
    if ("cex" %in% pars) cex <- dots$cex
    if ("lty" %in% pars) lty <- dots$lty
    if ("lwd" %in% pars) lwd <- dots$lwd
    if ("col" %in% pars) col <- dots$col
    #}

    if (!missing(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
    }
  # simple scatter plot --> plot(x, y) function
    plot(x, y, main=mt, xlab=xl, ylab=yl, pch=pch, cex=cex)
    grid(col="green")

  # fitted simple regression line --> lm(y ~ x) function & abline( ) function
    sr <- lm(y ~ x)
    ssr <- summary(sr)
    abline(sr, lty=lty, lwd=lwd, col=col)
  # formula of fitted simple regression equation
    b <- sr$coef[[2]]
    a <- sr$coef[[1]]
    sign <- ifelse(b < 0, "-", "+")
  # Legend postion
    xm <- (min(x, na.rm=TRUE) + max(x, na.rm=TRUE))/2
    ym <- (min(y, na.rm=TRUE) + max(y, na.rm=TRUE))/2
    nq11 <- sum(x < xm & y < ym, na.rm=TRUE)
    nq12 <- sum(x < xm & y > ym, na.rm=TRUE)
    nq21 <- sum(x > xm & y < ym, na.rm=TRUE)
    nq22 <- sum(x > xm & y > ym, na.rm=TRUE)
    if (b >= 0) {
        pos <- ifelse (nq12 <= nq21, "topleft", "bottomright")
    } else {
        pos <- ifelse (nq22 <= nq12, "topright", "bottomleft")
    }
    pv <- 1-pf(ssr$f[1], ssr$f[2], ssr$f[3])
    legend(pos, legend=c(paste("Y =", round(a, dig), sign, 
        round(abs(b), dig), "X"),
        paste("R-sq =", round(ssr$r.sq, dig)),
        paste("P-v =", format(pv, digits=3, scientific=T)) ),
        text.col=c("red", "blue", "black"), cex=1, 
        x.intersp = 0, yjust=0.5, bg="white")
    invisible(sr)
}

# [2-8] Calculate Measures of Location
#' Estimation of Location
#'
#' To calculate location estimates such as
#' sample mean, median, mode, geometric mean, harmonic mean, and trimmed mean.
#' @param x Vector of input data.
#' @param tr Treaming ratio. Default: 0.1.
#' @param mode Selection of the mode, c("first", "last", "near").
#' @param detail Logical: print detailed output? Default: FALSE.
#' @param dig The number of decimal places. Default: 4.
#' @return Six measures of location.
#' @keywords Ch2. Descriptive Statistics
#' @examples
#' location.est(mtcars$mpg, detail=T)
#'
#' set.seed(1234)
#' x <- round(rnorm(100, 10, 2), 1)
#' location.est(x, detail=T)
#' @export
location.est <- function(x, tr=0.1, mode="first", detail=FALSE, dig=4) {
    rdd <- function(x) format(round(x, dig), nsmall=dig)
    n <- length(x)
    numNA <- sum(is.na(x))
    if (numNA > 0) {
        whNA <- which(is.na(x))
        n0 <- n
        n <- n0 - numNA      
        cat("Warning: Missing valus are in postion ", whNA, "\n")
	x <- x[!is.na(x)]
    }
  # mean ==> mean( ) function
    xmean <- mean(x)
  # median ==> median( ) function
    xmed <- median(x)
  # frequency ==> table( ) function
    tabx <- table(x)
  # mode ==> value with maximum frequency in table
    allmode <- as.numeric(names(tabx[tabx==max(tabx)]))
    if (max(tabx)==1) {
        xmode <- NA
    } else if (length(allmode)>1) {
        cat("Warning: Multiple modes exist: ", allmode, "\n")
        if (mode=="first") {
            xmode <- allmode[1]
        } else if (mode=="last") {
            xmode <- allmode[length(allmode)]
        } else {
            dist <- abs(allmode - xmean)
            xmode <- allmode[which.min(dist)]
        }
    } else {xmode <- allmode}
  # Check zero values for geometric/harmonic mean
    whZero <- which(x==0)
    if (length(whZero>0)) {
        cat("Warning (Geometric/Harmonic mean): Exclude zero values at\n")
        print(whZero)
        xnz <- x[-whZero]
    } else {xnz <- x}
  # Geometric mean: prod(a)^(1/length(a))
    gm_mean <- function(a) exp(sum(log(a))/length(a))
    gmean <- gm_mean(xnz)
  # Harmonic mean
    hmean <- 1/mean(1/xnz)
  # Trimmed mean ==> mean(x, trim) function
    tmean <- mean(x, trim=tr)

  # Summary result
    res <- c(xmean, xmed, xmode, gmean, hmean, tmean)
    names(res) <- c("Mean", "Median", "Mode", "Geom-M", "Harm-M", "Trim-M")
  # Detailed output
    if (detail==TRUE) {
        nt <- floor(n*tr)
        sumt <- sum(sort(x)[(nt+1):(n-nt)])
        n2 <- n - 2*nt
        cat("Calculation in Detail --------------------------------------------",
        "\n(1) Mean =", paste0(rdd(sum(x)), "/", n), "=", rdd(xmean),
        "\n(2) Median =", paste0("x(", (n+1)/2, ") ="), rdd(xmed),
        "\n(3) Mode =", allmode, paste0("(", max(tabx), " times)"),
        "\n(4) Geom. Mean =", paste0("exp(", rdd(sum(log(xnz))),
            "/", length(xnz), ") ="), rdd(gmean),
        "\n(5) Harm. Mean =", paste0("1/(", rdd(mean(1/xnz)), ") ="), rdd(hmean),
        "\n(6)", paste0("Trim. Mean(",tr,") = ", rdd(sumt), "/", n2), "=", rdd(tmean), "\n")
        invisible(res)
    } else return(res)
}

# [2-9] Calculate Measures of Dispersion

#' Spread Estimation
#'
#' To calculate spread estimates.
#' @param x Vector of input data.
#' @param detail Logical: print detailed output? Default: FALSE.
#' @param dig The number of decimal places. Default: 4.
#' @return Five measures of dispersion.
#' @keywords Ch2. Descriptive Statistics
#' @examples
#' spread.est(mtcars$mpg, detail=T)
#'
#' set.seed(1234)
#' x <- round(rnorm(100, 10, 2), 1)
#' spread.est(x, detail=T)
#' @export
spread.est <- function(x, detail=FALSE, dig=4) {
    rdd <- function(x) format(round(x, dig), nsmall=dig)

    if (is.matrix(x)) x <- as.vector(x)
    n <- length(x)
    numNA <- sum(is.na(x))
    if (numNA > 0) {
        whNA <- which(is.na(x))
        n0 <- n
        n <- n0 - numNA
        cat("Warning: Missing valus are in postion ", whNA, "\n")
	x <- x[!is.na(x)]
    }

  # Sample variance ==> var( ) function
    xvar <- var(x)
  # Standard deviation ==> sd( ) function
    xsd <- sd(x)
  # Range ==> max( )-min( ) function
    xrng <- max(x) - min(x)
  # Inter-quartile range ==> IQR( ) function
    xiqr <- IQR(x)
  # Coefficient of variation ==> sd( ) / mean( ) function
    xcv <- xsd / mean(x)

  # Summary result
    res <- c(xvar, xsd, xrng, xiqr, xcv)
    names(res) <- c("Variance", "Std-Dev", "Range", "IQR", "CoV")

    if (detail==TRUE) {
        cat("Calculation in Detail --------------------------------------------",
        "\n(1) Variance =", paste0("(", rdd(sum(x^2))," - ", rdd(abs(sum(x))),
            "\U00B2/", n, ") /", n-1), "=", rdd(xvar),
        "\n(2) Stand. Dev. =", paste0("\U221A(", rdd(xvar), ") ="), rdd(xsd),
        "\n(3) Range =", rdd(max(x)), "-", paste0("(",rdd(min(x)),")"),
            "=", rdd(xrng),
        "\n(4) IQR =", rdd(quantile(x, 0.75)), "-",
            paste0("(",rdd(quantile(x, 0.25)),")"), "=", rdd(xiqr),
        "\n(5) CoV =", rdd(xsd), "/", paste0("(",rdd(mean(x)),")"),
            "=", rdd(xcv), "\n")
        invisible(res)
    } else return(res)
}

