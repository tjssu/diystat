# [Ch-9 Functions] ----------------------------------------------------------------------------------
# [Ch-9 Function Manual] -----------------------------------------

#' @title Manual for Ch9. Functions
#' @description Ch9. Distributions of Sample Statistics
#' @param fn Function number (0~6), Default: 0
#' @return None.
#'
#' @examples
#' ch9.man()
#' ch9.man(5)
#' @rdname ch9.man
#' @export
ch9.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] norm.sim \tSimulation of Normal Sample Means\n")
        cat("[2] norm.spn \tMinimum Number of Samples to Meet an Error Limit\n")
        cat("[3] tdist.sim\tSimulation of the t-distribution\n")
        cat("[4] chi.sim  \tSimulation of the Chi-square Distribution\n")
        cat("[5] fdist.sim\tSimulation of the F-distribution\n")
        cat("[6] clt.plot  \tDiagnosis of the Central Limit Theorem\n")
    }
    if (1 %in% fn) {
        cat("[1] Simulation of Normal Sample Means\n")
        cat("norm.sim(ns, mu=0, sig=1, N=1000, prt=TRUE, ws=c(10,4), dig=4, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("ns\t Sample size\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu\t Expected value, Default: 0.\n")
        cat("sig\t Standard deviation, Default: 1.\n")
        cat("N\t Number of iterations, Default: 1000.\n")
        cat("prt\t Logical: print statistics? Default: TRUE.\n")
        cat("ws\t Graphic window size, Default: c(10,4).\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Minimum Number of Samples to Meet an Error Limit\n")
        cat("norm.spn(kp, alp, prt=TRUE, ws=c(7,4), log=TRUE, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("kp\t Error limits in multiples of the standard deviation.\n")
        cat("alp\t Levels of significance.\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the sample sizes? Default: TRUE.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("log\t Logical: use log-scale for y-axis? Default: TRUE.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Simulation of the t-distribution\n")
        cat("tdist.sim(ns, mu=0, sig=1, N=1000, prt=TRUE, ws=c(10,4), dig=4, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("ns\t Sample size.\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu\t Expected value, Default: 0.\n")
        cat("sig\t Standard deviation, Default: 1.\n")
        cat("N\t Number of iterations, Default: 1000.\n")
        cat("prt\t Logical: print statistics? Default: TRUE.\n")
        cat("ws\t Graphic window size, Default: c(10,4).\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] Simulation of the Chi-square Distribution\n")
        cat("chi.sim(ns, mu=0, sig=1, N=1000, muknow=TRUE, prt=TRUE, ws=c(10,4), dig=4, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("ns\t Sample size\n")
        cat("[Optional Input]--------------------------\n")
        cat("mu\t Expected value, Default: 0.\n")
        cat("sig\t Standard deviation, Default: 1.\n")
        cat("N\t Number of iterations, Default: 10000.\n")
        cat("muknow\t Logical: is mu known?, Default: TRUE.\n")
        cat("prt\t Logical: print statistics? Default: TRUE.\n")
        cat("ws\t Graphic window size, Default: c(10,4).\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (5 %in% fn) {
        cat("[5] Simulation of the F-distribution from Sample Variances\n")
        cat("fdist.sim(sig1=1, sig2=1, n1, n2, N=1000, prt=TRUE, ws=c(10,4), dig=4, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("sig1\t Standard deviation of the first population.\n")
        cat("sig2\t Standard deviation of the second population.\n")
        cat("n1\t Sample size of the first population.\n")
        cat("n2\t Sample size of the second population.\n")
        cat("[Optional Input]--------------------------\n")
        cat("N\t Number of iterations, Default: 1000.\n")
        cat("prt\t Logical: print statistics? Default: TRUE.\n")
        cat("ws\t Graphic window size, Default: c(10,4).\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (6 %in% fn) {
        cat("[6] Diagnosis of the Central Limit Theorem\n")
        cat("clt.plot(dist, para, para2, ns, N=10000, sigknow=TRUE, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("dist\t Name of population distribution. (one of the below)\n")
        cat("\t (\"exp\",\"gamma\",\"weibull\",\"beta\",\"norm\",
                \"t\",\"chisq\",\"f\",\"pois\",\"binom\")\n")
        cat("para\t First parameter of the distribution.\n")
        cat("para2\t Second parameter of the distribution. (if necessary)\n")
        cat("[Optional Input]--------------------------\n")
        cat("ns\t Sample size, Default: c(10,30,50).\n")
        cat("N\t Number of iterations, Default: 10000.\n")
        cat("sigknow\t Logical: is population variance known? Default: TRUE.\n")
        cat("...\t Other graphic parameters.\n")
    }
}

# [9-1] Simulation of Normal Sample Means
#' @title Simulation of Normal Sample Means
#' @description Simulation for the Distribution of Normal Sample Means.
#' @param ns Sample size.
#' @param mu Expected value, Default: 0.
#' @param sig Standard deviation, Default: 1.
#' @param N Number of iterations, Default: 1000.
#' @param prt Logical: print statistics? Default: TRUE.
#' @param ws Graphic window size, Default: c(10,4).
#' @param dig Number of decimal places, Default: 4.
#' @param ... Other graphic parameters.
#' @return Sample means and standard deviations.
#'
#' @examples
#' norm.sim(ns=4, mu=5, sig=9, lwd=3)
#'
#' norm.sim(ns=25, mu=100, sig=10, N=10000, pos="topright")
#'
#' norm.sim(ns=10, mu=100, sig=10, ng=100, N=100000)
#' @rdname norm.sim
#' @export
norm.sim <- function(ns, mu=0, sig=1, N=1000, prt=TRUE, ws=c(10,4), dig=4, ...) {
  # Check Input
    if (missing(ns)) stop("Input sample size.")
    df <- ns - 1
    dots <- list(...)
    pars <- names(dots)
    type <- "hist"

  # Generate ns sample means, iterate N times
    seed <- 9857
    if ("seed" %in% pars) seed <- pars$seed
    set.seed(seed)
    # Raw X
    xm <- matrix(rnorm(ns*N, mu, sig), ncol=N)
    xd <- sample(xm, N)
    xtb <- mean(xd)
    xts <- sd(xd)
    # X-bar
    xb <- colMeans(xm)
    xbb <- mean(xb)
    xbs <- sd(xb)

  # Standardization
    # Raw X
    zd <- (xd-mu)/sig
    ztb <- mean(zd)
    zts <- sd(zd)
    # X-bar
    zb <- (xb-mu)/sig*sqrt(ns)
    zbb <- mean(zb)
    zbs <- sd(zb)

  # Define the population PDF and sample PDF
    popd <- function(x) dnorm(x, mu, sig)
    smd <- function(x) dnorm(x, mu, sig/sqrt(ns))

  # Expected value & standard deviation
    dig0 <- 3
    Ex0 <- format(round(xtb, dig0), nsmall=dig0)
    Dx0 <- format(round(xts, dig0), nsmall=dig0)
    Ex1 <- format(round(xbb, dig0), nsmall=dig0)
    Dx1 <- format(round(xbs, dig0), nsmall=dig0)
    Ex2 <- format(mu, nsmall=dig0)
    Dx2 <- format(round(sig, dig0), nsmall=dig0)

    Ez0 <- format(round(ztb, dig0), nsmall=dig0)
    Dz0 <- format(round(zts, dig0), nsmall=dig0)
    Ez1 <- format(round(zbb, dig0), nsmall=dig0)
    Dz1 <- format(round(zbs, dig0), nsmall=dig0)
    Ez2 <- format(0, nsmall=dig0)
    Dz2 <- format(1, nsmall=dig0)

  # Print
    if (prt) {
        means <- c(mu, xtb, mu, xbb, 0, ztb, zbb)
        sigs <- c(sig, xts, sig/sqrt(ns), xbs, 1, zts, zbs)
        tab <- rbind(means, sigs)
        colnames(tab) <- c("Input","X-raw","Expect","X-bar","Expect","Z-raw","Z-xb")
        rownames(tab) <- c("Mean", "Stdv")
        print(round(tab, dig))
    }

  # Plot
    win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))
    par(mar=c(4,4,3,2))
    x1 <- mu-3*sig
    x2 <- mu+3*sig

    mt1 <- bquote(bold("Distribution of") ~~bar(X)[.(ns)] ~~ 
           bold("X ~") ~~N( .(mu) , .(sig)^2 ) )
    mt2 <- "Standardized Distributions"
    lwd <- 2
    col0 <- adjustcolor("yellow", 0.4)
    col1 <- adjustcolor("cyan", 0.4)
    col2 <- "cyan"
    ng0 <- 40
    xr1 <- c(x1, x2)
    xr2 <- c(min(-4,zd), max(4,zd))
    yl1 <- "f(x)"
    yl2 <- bquote(phi(z))
    xl1 <- "x"
    xl2 <- "z"
    cex <- 0.8
    pos1 <- pos2 <- "topleft"

  # Set the list of arguments
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("breaks" %in% pars) ng0 <- dots$breaks
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt)>1) {
                mt1 <- mt[1]
                mt2 <- mt[2]
            } else mt1 <- mt2 <- mt
        } 
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col)>1) {
                col0 <- col[1]
                col1 <- col[2]
                col2 <- ifelse(length(col)>2, col[3], col[2]) 
            } else col0 <- col1 <- col2 <- col
        }
        if ("pos" %in% pars) {
            pos <- dots$pos
            if (length(pos)>1) {
                pos1 <- pos[1]
                pos2 <- pos[2]
            } else pos1 <- pos2 <- pos
        } 
        if ("ylab" %in% pars) yl1 <- dots$ylab
        if ("xlab" %in% pars) xl1 <- dots$xlab
        if ("xlim" %in% pars) xr1 <- dots$xlim
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("cex" %in% pars) cex <- dots$cex
    }

  # Plot the population PDF and sample PDF
    H1 <- hist(xb, breaks=ng0/2, plot=FALSE)
    hist(xd, breaks=ng0, prob=T, col=col0, xlim=xr1,
         ylab=yl1, xlab=xl1, main=mt1, ylim=c(0,max(H1$density)*1.05))
    H1$counts <- H1$density
    plot(H1, col=col1, add=TRUE)

    curve(popd, xr1[1], xr1[2], lwd=lwd, col="blue", add=TRUE)
    curve(smd, xr1[1], xr1[2], lwd=lwd, col="red", add=TRUE)

    cc1 <- c(Ex2, Ex0, Ex1)
    cw1 <- nchar(cc1)
    sp1 <- 2*(max(cw1) - cw1) + grepl("^-", cc1)
    for (i in 1:3) {
        if (sp1[i]>0) for (j in 1:sp1[i]) cc1[i] <- paste0(" ", cc1[i])
    }
    cc2 <- c(Dx2, Dx0, Dx1)
    cw2 <- nchar(cc2)
    sp2 <- max(cw2) - cw2
    for (i in 1:3) {
        if (sp2[i]>0) for (j in 1:sp2[i]) cc2[i] <- paste0("  ", cc2[i])
    }

    leg <- vector()
    leg[1] <- paste("Pa    Mean   Stdev")
    leg[2] <- paste("In ", cc1[1], cc2[1])
    leg[3] <- paste("Xi ", cc1[2], cc2[2])
    leg[4] <- paste("Xb", cc1[3], cc2[3])

    legend(pos1, leg, cex=cex, text.col=c(1,"red","blue","blue"),
           x.intersp=-0.5)

  # Plot the standardized PDF
    H1 <- hist(zb, breaks=ng0, plot=FALSE)
    H0 <- hist(zd, breaks=ng0, plot=FALSE)
    hist(zd, breaks=ng0, prob=T, col=col0, xlim=xr2,
         ylab=yl2, xlab=xl2, main=mt2, 
         ylim=c(0,max(H1$density,H0$density)*1.05))
    H1$counts <- H1$density
    plot(H1, col=col1, add=TRUE)

    ##lines(density(zd), lwd=lwd, col="blue")
    ##lines(density(zb), lwd=lwd, col="red")
    curve(dnorm, xr2[1], xr2[2], lwd=lwd, col="red", add=TRUE)

    cc1 <- c(Ez2, Ez0, Ez1)
    cw1 <- nchar(cc1) 
    sp1 <- 2*(max(cw1) - cw1) + grepl("^-", cc1)
    for (i in 1:3) {
        if (sp1[i]>0) for (j in 1:sp1[i]) cc1[i] <- paste0(" ", cc1[i])
    }
    cc2 <- c(Dz2, Dz0, Dz1)
    cw2 <- nchar(cc2)
    sp2 <- max(cw2) - cw2
    for (i in 1:3) {
        if (sp2[i]>0) for (j in 1:sp2[i]) cc2[i] <- paste0("  ", cc2[i])
    }

    leg <- vector()
    leg[1] <- paste("Pa  Mean Stdev")
    leg[2] <- paste("In ", cc1[1], cc2[1])
    leg[3] <- paste("Xi ", cc1[2], cc2[2])
    leg[4] <- paste("Xb", cc1[3], cc2[3])

    legend(pos2, leg, cex=cex, text.col=c(1,"red","blue","blue"),
           x.intersp=-0.5)

  # Plot ECDF of the standardized statistic
    if (type=="qq") {
    qq0 <- qqnorm(zd, plot=F)
        qq1 <- qqnorm(zb, plot=F)
        za <- seq(xr2[1], xr2[2], length=100)
        plot(za, za, type="n", ylab=yl2, xlab=xl2, main=mt2)
        grid(col=3)
        points(qq0, pch=19, cex=0.6, col=1)
        points(qq1, pch=17, cex=0.6, col=4)
        abline(0, 1, col="red")
    }
    invisible(tab)
}

# [9-2] Minimum Number of Samples to Meet an Error Limit
#' @title Minimum Number of Samples to Meet an Error Limit
#' @description Minimum Number of Samples to Meet an Error Limit of Sample Means from Normal Population.
#' @param kp Error limits in multiples of the standard deviation.
#' @param alp Levels of significance.
#' @param prt Logical: print the sample size? Default: TRUE.
#' @param ws Graphic window size, Default: c(7,4).
#' @param log Logical: use log-scale for y-axis? Default: TRUE.
#' @param ... Other graphic parameters.
#' @return Sample sizes.
#'
#' @examples
#' alp <- c(0.01, 0.05, 0.1)
#' dcol <- c("red", "blue", "green4")
#' norm.spn(kp=0.4, alp, col=dcol)
#'
#' norm.spn(kp=0.05, alp, cex=0.8, col=dcol)
#'
#' norm.spn(kp=1:5/10, alp, col=dcol)
#'
#' norm.spn(kp=0.2, alp=1:10/100,cex=0.8,col="cyan",xlim=c(0.15,0.25))
#'
#' @rdname norm.spn
#' @export
norm.spn <- function(kp, alp, prt=TRUE, ws=c(7,4), log=TRUE, ...) {
  # Check Input
    if (missing(kp)) stop("Input error limit in multiples of sigma.")
    if (missing(alp)) stop("Input level of significances.")

  # Function for calculating the minimum number of samples
    spn <- function(k, alp) ceiling((qnorm(1-alp/2)/k)^2)

  # Get the minimum number of samples
    nalp <- length(alp)
    nkp <- length(kp)
    if (min(nalp, nkp)==1) {
        mspn <- spn(kp, alp)
        if (nalp > 1) names(mspn) <- alp
        if (nkp > 1) names(mspn) <- kp
    } else {
        mspn <- outer(kp, alp, "spn")
        colnames(mspn) <- alp
        rownames(mspn) <- kp
    }
    if (prt) print(mspn)

  # Set the title and axis
    # [Correction]
    if (nkp==1) {
        mt <- paste0("Minimum Number of Samples for ",
            kp, "-sigma Error Limit")
    } else {
        mt <- paste0("Minimum Number of Samples for (",
            min(kp), "~", max(kp), ")-sigma Error Limit")
    }

    xlim <- c(min(0.1, kp/2), max(kp)*2)
    ylim <- c(max(1,spn(xlim[2], max(alp))), spn(xlim[1], min(alp)))
    lwd <- 2
    col <- rainbow(nalp)
    yl <- "Number of Samples"
    xl <- "k"
    cex <- 1

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("xlim" %in% ndots) {
            xlim <- dots$xlim
            ylim <- c(max(1,spn(xlim[2], max(alp))), spn(xlim[1], min(alp)))
        }
        if ("ylim" %in% ndots) ylim <- dots$ylim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("col" %in% ndots) {
            col <- dots$col
            if (length(col)==1) {
                rc <- (nalp:1)/nalp
                col <- sapply(rc, function(x) adjustcolor(col, 1, x,x,x))
            }
        }
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("cex" %in% ndots) cex <- dots$cex
    }

  # Plot in log-scale
    xmin <- xlim[1]
    xmax <- xlim[2]
    xa <- seq(xmin, xmax, length=100)
    # [Correction]
    #if (xmax>=1) y1 <- 1 else y1 <- spn(xmax, max(alp))
    #y2 <- spn(xmin, min(alp))
    y1 <- ylim[1]
    y2 <- ylim[2]
    del <- (xmax - xmin)/25
    x1 <- xmin - del*nkp
    x2 <- xmax + del

  if (is.numeric(ws)) {
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

    if (log) {
        plot(xa, spn(xa, alp[1]), type="n", log="y", ylab=yl,
            xlab=xl, ylim=c(y1, y2), xlim=c(x1, x2), main=mt)
    } else {
        plot(xa, spn(xa, alp[1]), type="n", ylab=yl,
            xlab=xl, ylim=c(y1, y2), xlim=c(x1, x2), main=mt)
    }
    grid( )
    for (i in 1:nalp) lines(xa, spn(xa, alp[i]), lwd=lwd, col=col[i])

  # Display legend
    leg <- list()
    for (i in 1:nalp) leg[[i]] <- bquote(alpha==.(alp[i]))
    # [Correction]
    if (nalp<=10) vcex <- cex else vcex <- cex/(nalp-9)^0.1
    legend("topright", sapply(leg, as.expression), lwd=2, 
        cex=vcex, col=col, bg="white")

  # Illustrate specific cases
    # [Correction]
    if (nkp==1) kcex <- vcex else kcex <- vcex/nkp^0.1
    for (k in 1:nkp) segments(kp[k], y1, kp[k], spn(kp[k], min(alp)), 
                        lty=2, col="magenta")
    for (k in 1:nkp) segments(x1+k*del, spn(kp[k], alp), kp[k], spn(kp[k], alp),
                        lty=2, col="magenta")
    for (k in 1:nkp) text(x1+(k-1)*del, spn(kp[k], alp), labels=spn(kp[k], alp), 
                        cex=kcex, col="blue")
  }
    invisible(mspn)
}

# [9-3] Simulation for the t-distribution
#' @title Simulation for the t-distribution
#' @description Simulation for the t-distribution.
#' @param ns Sample size.
#' @param mu Expected value, Default: 0.
#' @param sig Standard deviation, Default: 1.
#' @param N Number of iterations, Default: 1000.
#' @param prt Logical: print statistics? Default: TRUE.
#' @param ws Graphic window size, Default: c(10,4).
#' @param dig Number of decimal places, Default: 4.
#' @param ... Other graphic parameters.
#' @return Sample means and standard deviations.
#'
#' @examples
#' tdist.sim(ns=10, mu=100, sig=10)
#'
#' tdist.sim(ns=5, N=10000, lwd=3)
#'
#' tdist.sim(ns=5, breaks=100)
#'
#' tdist.sim(ns=5, mu=100, sig=10, N=10000, breaks=200, seed=157)
#'
#' @rdname tdist.sim
#' @export
tdist.sim <- function(ns, mu=0, sig=1, N=1000, prt=TRUE, ws=c(10,4), dig=4, ...) {
  # Check Input
    if (missing(ns)) stop("Input sample size.")
    df <- ns - 1
    if (any(ns < 3)) cat("The sample size should be as least three...\n")
    dots <- list(...)
    pars <- names(dots)

  # Generate ns sample means, iterate N times
    seed <- 9857
    if ("seed" %in% pars) seed <- dots$seed
    set.seed(seed)
    # Raw X
    xm <- matrix(rnorm(ns*N, mu, sig), ncol=N)
    xtb <- mean(xm)
    xts <- sd(xm)
    # X-bar
    xb <- colMeans(xm)
    xbb <- mean(xb)
    xbs <- sd(xb)
    ss <- apply(xm, 2, sd)

  # Standardization
    zb <- (xb-mu)/sig*sqrt(ns)
    zbb <- mean(zb)
    zbs <- sd(zb)
  # Studentization (sig -> sts, sig -> ss)
    tb <- (xb-mu)/ss*sqrt(ns)
    tbb <- mean(tb)
    tbs <- sd(tb)

  # Define the sample PDF
    smd <- function(x) dt(x, df)

  # Expected value & standard deviation
    dig0 <- 3
    Ez0 <- format(round(zbb, dig0), nsmall=dig0)
    Dz0 <- format(round(zbs, dig0), nsmall=dig0)
    Ez1 <- format(round(tbb, dig0), nsmall=dig0)
    Dz1 <- format(round(tbs, dig0), nsmall=dig0)
    Ez2 <- format(0, nsmall=dig0)
    Dz2 <- format(1, nsmall=dig0)

    DT <- ifelse(df>2, sqrt(df/(df-2)), Inf)
    Dt <- format(round(DT, dig0), nsmall=dig0)

  # Print
    if (prt) {
        means <- c(mu, xtb, mu, xbb, 0, tbb)
        sigs <- c(sig, xts, sig/sqrt(ns), xbs, DT, tbs)
        tab <- rbind(means, sigs)
        colnames(tab) <- c("Input","X-raw","Expect","X-bar","Expect","T-xb")
        rownames(tab) <- c("Mean", "Stdv")
        print(round(tab, dig))
    }

  # Set titles, axes, and graphic parameters
    mt1 <- paste0("Distribution of T-Statistics (n=", ns, ", N=", N, ")")
    mt2 <- "Quantile-Quantile Plots"
    lwd <- 2
    col0 <- "yellow"
    col1 <- "blue"
    col2 <- "red"
    yl1 <- "f(t)"
    yl2 <- "Sample Quantiles"
    xl1 <- "t"
    xl2 <- "Theoretical Qauntiles"
    cex <- 0.8
    pos1 <- "topright"
    pos2 <- "bottomright"
    ng0 <- 50
    zrng <- c(min(-4,tb), max(4,tb))

  # Set the list of arguments
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("breaks" %in% pars) ng0 <- dots$breaks
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt)>1) {
                mt1 <- mt[1]
                mt2 <- mt[2]
            } else mt1 <- mt
        } 
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col)>1) {
                col0 <- col[1]
                col1 <- col[2]
                col2 <- ifelse(length(col)>2, col[3], col[2]) 
            } else col0 <- col
        }
        if ("pos" %in% pars) {
            pos <- dots$pos
            if (length(pos)>1) {
                pos1 <- pos[1]
                pos2 <- pos[2]
            } else pos1 <- pos2 <- pos
        } 
        if ("ylab" %in% pars) yl1 <- dots$ylab
        if ("xlab" %in% pars) xl1 <- dots$xlab
        if ("xlim" %in% pars) zrng <- dots$xlim
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("cex" %in% pars) cex <- dots$cex
    }

  # Display graph
    pos01 <- pos02 <- "topleft"
    if (pos1=="topleft") pos01 <- "topright"
    if (pos2=="topleft") pos02 <- "bottomright"

    del <- (max(tb)-min(tb))/ng0

    mycut <- seq(min(tb)-del/2, max(tb)+del/2, by=del)
    H0 <- hist(tb, breaks=mycut, plot=FALSE)
    hmax <- max(H0$density)
    if (ns>=10) {
        ymax <- hmax*1.05
    } else if (ns>=5) {
        ymax <- hmax*1.1
    } else {
        ymax <- hmax*1.2
    }

  # Plot-1: Histogram
    if (is.numeric(ws)) win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))
    par(mar=c(4,4,3,2))

    x1 <- max(-5, zrng[1])
    x2 <- min(5, zrng[2])
    hist(tb, breaks=mycut, prob=T, col=col0, ylim=c(0, ymax), 
            xlim=c(x1,x2), ylab=yl1, xlab=xl1, main=mt1)
    curve(dnorm, x1, x2, lwd=lwd, col=col1, add=T)
    curve(smd, x1, x2, lwd=lwd, col=col2, add=T)

    rname <- c("Para","Exact","Tstat","Zstat")
    Mean <- c("Mean", Ez2, Ez1, Ez0)
    Stdev <- c("Stdev", Dt, Dz1, Dz0)
    tab1 <- cbind(rname, Mean, Stdev)

    legend(pos1, tab1, ncol=3, cex=cex, text.col=rep(c(1,"red","blue",1),3),
           xjust=1, x.intersp=0)

    legend(pos01, c(paste0("t(", df,")"), "N(0,1)"), 
            lwd=c(2,2), col=c("red","blue"), cex=cex)

  # Plot-2: ECDF
    ## qq0 <- qqplot(qt(ppoints(N), df=df), tb, plot=F)
    za <- seq(x1, x2, length=100)
    plot(za, za, type="n", ylab=yl2, xlab=xl2, main=mt2)
    grid(col=3)
    ## abline(lm(qq0$y~qq0$x), col=col2, lwd=lwd)
    ## qqline(tb, distribution=function(p) qt(p,df), probs=c(0.1,0.9), col=col2, lwd=lwd)
    ## points(qq0$x, qq0$y, pch=19, cex=0.6, col=1)
    yy <- sort(tb)
    xx <- qt(1:N/(N+1), df)
    xx0 <- qnorm(1:N/(N+1))
    abline(0, 1, col=col2, lwd=lwd)
    points(xx0, yy, pch=19, cex=0.6, col=3)
    points(xx, yy, pch=19, cex=0.6, col=1)
    ##abline(lm(yy ~ xx0), col=col1, lwd=lwd)

    legend(pos2, tab1, ncol=3, cex=cex, text.col=rep(c(1,"red","blue",1),3),
           xjust=1, x.intersp=0, bg="white")

    legend(pos02, c(paste0("t(", df,")"), "N(0,1)"), 
            pch=c(19,19), col=c(1,3), cex=cex, pt.cex=1, bg="white")

  # Return
    invisible(tab)
}

# [9-4] Simulation for the chi-square Distribution
#' @title Simulation for the chi-square Distribution
#' @description Simulation for the chi-square Distribution
#' @param ns Sample size.
#' @param mu Expected value, Default: 0.
#' @param sig Standard deviation, Default: 1.
#' @param N Number of iterations, Default: 1000.
#' @param muknow Logical: is mu known?, Default: TRUE.
#' @param prt Logical: print statistics? Default: TRUE.
#' @param ws Graphic window size, Default: c(10,4).
#' @param dig Number of decimal places, Default: 4.
#' @param ... Other graphic parameters.
#' @return Sample means and standard deviations.
#'
#' @examples
#' chi.sim(ns=5)
#' chi.sim(ns=5, muknow=FALSE)
#'
#' chi.sim(ns=10, mu=100, sig=10)
#' chi.sim(ns=10, mu=100, sig=10, muknow=FALSE)
#'
#' chi.sim(ns=5, N=10000, dig=4, breaks=100, seed=1243)
#' chi.sim(ns=5, N=10000, dig=4, muknow=FALSE, breaks=100, seed=1243)
#'
#' @rdname chi.sim
#' @export
chi.sim <- function(ns, mu=0, sig=1, N=1000, muknow=TRUE, prt=TRUE, ws=c(10,4), dig=4, ...)
{
  # Check Input
    if (missing(ns)) stop("Input sample size.")
    df <- ns - 1
    if (any(ns < 3)) cat("The sample size should be as least three...\n")
    dots <- list(...)
    pars <- names(dots)

  # Generate ns sample means, iterate N times
    seed <- 9857
    if ("seed" %in% pars) seed <- dots$seed
    set.seed(seed)

  # Sample statistic with mu known or unknown
    # Raw X
    xm <- matrix(rnorm(ns*N, mu, sig), ncol=N)
    xtb <- mean(xm)
    xts <- sd(xm)
    # X-bar
    xsm <- colSums(xm)
    xb <- xsm / ns
    xbb <- mean(xb)
    xbs <- sd(xb)
    xsq <- apply(xm, 2, function(x) sum(x^2))

    if (muknow) {
        cs <- (xsq - 2*mu*xsm + ns*mu^2)/sig^2
    } else {
        cs <- (xsq - xsm^2 / ns) / sig^2
    }

  # Degree of freedom (0=Correct)
    nu0 <- ifelse(muknow, ns, ns-1)
    nu1 <- ifelse(muknow, ns-1, ns)
  # Define the chi-square PDF
    svd0 <- function(x) dchisq(x, nu0)
    svd1 <- function(x) dchisq(x, nu1)

  # Expected value & standard deviation
    Ecs <- mean(cs)
    Dcs <- sd(cs)
    dig0 <- 3
    Ec <- format(round(Ecs, dig0), nsmall=dig0)
    Ec0 <- format(round(nu0, dig0), nsmall=dig0)
    Ec1 <- format(round(nu1, dig0), nsmall=dig0)

    Dc <- format(round(Dcs, dig0), nsmall=dig0)
    Dc0 <- format(round(sqrt(2*nu0), dig0), nsmall=dig0)
    Dc1 <- format(round(sqrt(2*nu1), dig0), nsmall=dig0)

    Ex0 <- format(round(xtb, dig0), nsmall=dig0)
    Dx0 <- format(round(xts, dig0), nsmall=dig0)
    Ex1 <- format(round(xbb, dig0), nsmall=dig0)
    Dx1 <- format(round(xbs, dig0), nsmall=dig0)

  # Print
    if (prt) {
        means <- c(xtb, xbb, Ecs, nu0, nu1)
        sigs <- c(xts, xbs, Dcs, sqrt(2*nu0), sqrt(2*nu1))
        tab <- rbind(means, sigs)
        colnames(tab) <- c("X-raw", "X-bar", "Simul", "Exact", "Wrong")
        rownames(tab) <- c("Mean", "Stdv")
        print(round(tab, dig))
    }

  # Set titles, axes, and graphic parameters
    mt1 <- ifelse(muknow, 
                "Dist. of SSq: known mu",
                "Dist. of SSq: Unknown mu")
    mt1 <- paste0(mt1, " (n=", ns, ", N=", N, ")")
    mt2 <- "Quantile-Quantile Plots"
    lwd <- 2
    col0 <- "yellow"
    col1 <- "blue"
    col2 <- "red"
    yl1 <- "f(x)"
    yl2 <- "Sample Quantiles"
    xl1 <- "x"
    xl2 <- "Theoretical Qauntiles"
    cex <- 0.8
    pos1 <- "right"
    pos2 <- "bottomright"
    ng0 <- 50

    x1 <- 0
    x2 <- ceiling(max(cs))
    xrng <- c(x1, x2)

  # Set the list of arguments
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("breaks" %in% pars) ng0 <- dots$breaks
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt)>1) {
                mt1 <- mt[1]
                mt2 <- mt[2]
            } else mt1 <- mt
        } 
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col)>1) {
                col0 <- col[1]
                col1 <- col[2]
                col2 <- ifelse(length(col)>2, col[3], col[2]) 
            } else col0 <- col
        }
        if ("pos" %in% pars) {
            pos <- dots$pos
            if (length(pos)>1) {
                pos1 <- pos[1]
                pos2 <- pos[2]
            } else pos1 <- pos2 <- pos
        } 
        if ("ylab" %in% pars) yl1 <- dots$ylab
        if ("xlab" %in% pars) xl1 <- dots$xlab
        if ("xlim" %in% pars) xrng <- dots$xlim
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("cex" %in% pars) cex <- dots$cex
    }

  # Display graph
    pos01 <- "topright"
    pos02 <- "topleft"
    if (pos1=="topright") pos01 <- "right"
    if (pos2=="topleft") pos02 <- "bottomright"

  # Plot-1: Histogram
    if (is.numeric(ws)) win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))
    par(mar=c(4,4,3,2))
    x1 <- xrng[1]
    x2  <- xrng[2]

    del <- (max(cs)-min(cs))/ng0

    mycut <- seq(min(cs)-del/2, max(cs)+del/2, by=del)
    H0 <- hist(cs, breaks=mycut, plot=FALSE)
    hmax <- max(H0$density)
    if (ns>=10) {
        ymax <- hmax*1.05
    } else if (ns>=5) {
        ymax <- hmax*1.1
    } else {
        ymax <- hmax*1.2
    }

    hist(cs, breaks=mycut, prob=T, col=col0, ylim=c(0, ymax), 
            xlim=c(x1,x2), ylab=yl1, xlab=xl1, main=mt1)
    curve(svd0, x1, x2, lwd=lwd, col=col2, add=T)
    curve(svd1, x1, x2, lwd=lwd, col=col1, add=T)

    rname <- c("Para","Exact","Simul","Wrong")
    Mean <- c("Mean", Ec0, Ec, Ec1)
    Stdev <- c("Stdev", Dc0, Dc, Dc1)
    tab1 <- cbind(rname, Mean, Stdev)

    legend(pos1, tab1, ncol=3, cex=cex, text.col=rep(c(1,"red","blue",1),3),
           xjust=1, x.intersp=0)

    leg <- list()
    leg[[1]] <- bquote(chi^2 ~( .(nu0) ))
    leg[[2]] <- bquote(chi^2 ~( .(nu1) ))
    legend(pos01, sapply(leg, as.expression),
        lwd=c(lwd,lwd), col=c("red","blue"), cex=cex)

  # Plot-2: Q-Q Plot
    za <- seq(x1, x2, length=100)
    plot(za, za, type="n", ylab=yl2, xlab=xl2, main=mt2)
    grid(col=3)
    yy <- sort(cs)
    xx <- qchisq(1:N/(N+1), nu0)
    xx0 <- qchisq(1:N/(N+1), nu1)
    abline(0, 1, col=col2, lwd=lwd)
    points(xx0, yy, pch=19, cex=0.6, col=3)
    points(xx, yy, pch=19, cex=0.6, col=1)
    ##abline(lm(yy ~ xx0), col=col1, lwd=lwd)

    legend(pos02, sapply(leg, as.expression),
        pch=c(19,19), col=c("1","green"), cex=cex, bg="white")

    legend(pos2, tab1, ncol=3, cex=cex, text.col=rep(c(1,"red","blue",1),3),
           xjust=1, x.intersp=0, text.width=NA, bg="white")
  # Return
    invisible(tab)
}

# [9-5] Simulation for the F-distribution
#' @title Simulation for the F-distribution
#' @description Simulation for the F-distribution.
#' @param sig1 Standard deviation of the first population.
#' @param sig2 Standard deviation of the second population.
#' @param n1 Sample size of the first population.
#' @param n2 Sample size of the second population.
#' @param N Number of iterations, Default: 1000.
#' @param prt Logical: print statistics? Default: TRUE.
#' @param ws Graphic window size, Default: c(10,4).
#' @param dig Number of decimal places, Default: 4.
#' @param ... Other graphic parameters.
#' @return Sample means and standard deviations.
#'
#' @examples
#' # Stable
#' fdist.sim(sig1=2, sig2=7, n1=15, n2=20)
#' # Very unstable
#' fdist.sim(n1=5, n2=4, xlim=c(0,20))
#' fdist.sim(n1=5, n2=4, N=10000, breaks=100, xlim=c(0,20))
#'
#' @rdname fdist.sim
#' @export
fdist.sim <- function(sig1=1, sig2=1, n1, n2, N=1000, prt=TRUE, ws=c(10,4), dig=4, ...)
{
  # Check Input
    if (missing(n1)||missing(n2)) stop("Input sample size.")
    dots <- list(...)
    pars <- names(dots)
    df1 <- n1 - 1
    df2 <- n2 - 1

  # Generate N statistics from two independent normal population
    seed <- 9857
    if ("seed" %in% pars) seed <- dots$seed
    set.seed(seed)

  # Sample statistics
    # Raw X1, X2
    xm1 <- matrix(rnorm(n1*N, 0, sig1), ncol=N, byrow=TRUE)
    xtb1 <- mean(xm1)
    xts1 <- sd(xm1)
    xm2 <- matrix(rnorm(n2*N, 0, sig2), ncol=N, byrow=TRUE)
    xtb2 <- mean(xm2)
    xts2 <- sd(xm2)

    # X-bar1, X-bar2
    xsm1 <- colSums(xm1)
    xb1 <- xsm1 / n1
    xbb1 <- mean(xb1)
    xbs1 <- sd(xb1)
    xsv1 <- apply(xm1, 2, var)

    xsm2 <- colSums(xm2)
    xb2 <- xsm2 / n2
    xbb2 <- mean(xb2)
    xbs2 <- sd(xb2)
    xsv2 <- apply(xm2, 2, var)

  # F-Stat
    fs <- (xsv1/xsv2)*(sig2/sig1)^2
    #fs <- NULL
    #vratio <- function(n1, n2, s1, s2) {
    #    var(rnorm(n1, sd=s1))/s1^2/(var(rnorm(n2, sd=s2))/s2^2)}
    #for (k in 1:N) fs <- c(fs, vratio(n1, n2, sig1, sig2))

  # Define F-PDF (0=TRUE, 1=FALSE)
    fd0 <- function(x) df(x, df1, df2)
    fd1 <- function(x) df(x, n1, n2)
    xmax <- qf(0.99, df1, df2)
    xmod <- ifelse(n1>3, (df1-2)/df1*df2/(df2+1), 0)
    ymax <- ifelse(n1>3, max(fd0(xmod), fd1(xmod)), 1)

  # Expected value & standard deviation
    Mfs <- mean(fs)
    Sfs <- sd(fs)
    Mf0 <- ifelse(df2>2, df2/(df2-2), Inf)
    Vf0 <- ifelse(df2>4, 2*df2^2*(df1+df2-2)/df1/(df2-2)^2/(df2-4),
            ifelse(df2>2, Inf, NA) )
    Sf0 <- sqrt(Vf0)
    Mf1 <- ifelse(n2>2, n2/(n2-2), Inf)
    Vf1 <- ifelse(n2>4, 2*n2^2*(n1+n2-2)/n1/(n2-2)^2/(n2-4),
            ifelse(n2>2, Inf, NA) )
    Sf1 <- sqrt(Vf1)
 
    dig0 <- 3
    Efs <- format(round(Mfs, dig0), nsmall=dig0)
    Ef0 <- format(round(Mf0, dig0), nsmall=dig0)
    Ef1 <- format(round(Mf1, dig0), nsmall=dig0)

    Dfs <- format(round(Sfs, dig0), nsmall=dig0)
    Df0 <- format(round(Sf0, dig0), nsmall=dig0)
    Df1 <- format(round(Sf1, dig0), nsmall=dig0)

    Ex1 <- format(round(xbb1, dig0), nsmall=dig0)
    Dx1 <- format(round(xbs1, dig0), nsmall=dig0)
    Ex2 <- format(round(xbb2, dig0), nsmall=dig0)
    Dx2 <- format(round(xbs2, dig0), nsmall=dig0)

  # Print
    if (prt) {
        means <- c(xtb1, xtb2, xbb1, xbb2, Mfs, Mf0, Mf1)
        sigs <- c(xts1, xts2, xbs1, xbs2, Sfs, Sf0, Sf1)
        tab <- rbind(means, sigs)
        colnames(tab) <- c("X-1", "X-2", "X-b1", "X-b2", "Simul", "Exact", "Wrong")
        rownames(tab) <- c("Mean", "Stdv")
        print(round(tab, dig))
    }

  # Set titles, axes, and graphic parameters
    mt1 <- bquote("("~S[1]^2~"/"~sigma[1]^2~ ")/(" ~S[2]^2~"/"~sigma[2]^2~
            ") ~"~ F( .(n1-1) , .(n2-1) ) )

    mt2 <- "Quantile-Quantile Plots"
    lwd <- 2
    col0 <- "yellow"
    col1 <- "blue"
    col2 <- "red"
    yl1 <- "f(x)"
    yl2 <- "Sample Quantiles"
    xl1 <- "x"
    xl2 <- "Theoretical Qauntiles"
    cex <- 0.8
    pos1 <- "right"
    pos2 <- "bottomright"
    ng0 <- 50

    x1 <- 0
    x2 <- ceiling(max(fs))
    xrng <- c(x1, x2)

  # Set the list of arguments
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("breaks" %in% pars) ng0 <- dots$breaks
        if ("main" %in% pars) {
            mt <- dots$main
            if (length(mt)>1) {
                mt1 <- mt[1]
                mt2 <- mt[2]
            } else mt1 <- mt
        } 
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col)>1) {
                col0 <- col[1]
                col1 <- col[2]
                col2 <- ifelse(length(col)>2, col[3], col[2]) 
            } else col0 <- col
        }
        if ("pos" %in% pars) {
            pos <- dots$pos
            if (length(pos)>1) {
                pos1 <- pos[1]
                pos2 <- pos[2]
            } else pos1 <- pos2 <- pos
        } 
        if ("ylab" %in% pars) yl1 <- dots$ylab
        if ("xlab" %in% pars) xl1 <- dots$xlab
        if ("xlim" %in% pars) xrng <- dots$xlim
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("cex" %in% pars) cex <- dots$cex
    }

  # Display graph
    pos01 <- "topright"
    pos02 <- "topleft"
    if (pos1=="topright") pos01 <- "right"
    if (pos2=="topleft") pos02 <- "bottomright"

    if (is.numeric(ws)) win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))
    par(mar=c(4,4,3,2))
    x1 <- xrng[1]
    x2  <- xrng[2]

  # Plot-1: Histogram
    del <- (x2-x1)/ng0

    mycut <- seq(min(fs)-del/2, max(fs)+del, by=del)
    H0 <- hist(fs, breaks=mycut, plot=FALSE)
    hmax <- max(H0$density)
    if (n2>=10) {
        ymax <- hmax*1.05
    } else if (n2>=5) {
        ymax <- hmax*1.1
    } else {
        ymax <- hmax*1.2
    }

    hist(fs, breaks=mycut, prob=T, col=col0, ylim=c(0, ymax), 
            xlim=c(x1,x2), ylab=yl1, xlab=xl1, main=mt1)
    curve(fd0, x1, x2, lwd=lwd, col=col2, add=T)
    curve(fd1, x1, x2, lwd=lwd, col=col1, add=T)

    # Display legends
    rname <- c("Para","Exact","Simul","Wrong")
    Mean <- c("Mean", Ef0, Efs, Ef1)
    Stdev <- c("Stdev", Df0, Dfs, Df1)
    tab1 <- cbind(rname, Mean, Stdev)

    legend(pos1, tab1, ncol=3, cex=cex, text.col=rep(c(1,"red","blue",1),3),
           xjust=1, x.intersp=0)

    leg <- list()
    leg[[1]] <- bquote(F( .(n1-1) , .(n2-1) ))
    leg[[2]] <- bquote(F( .(n1) , .(n2) ))
    legend(pos01, sapply(leg, as.expression),
        lwd=c(lwd,lwd), col=c("red","blue"), cex=cex)

  # Plot-2: Q-Q Plot
    za <- seq(x1, x2, length=100)
    plot(za, za, type="n", ylab=yl2, xlab=xl2, main=mt2)
    grid(col=3)
    yy <- sort(fs)
    xx <- qf(1:N/(N+1), n1-1, n2-1)
    xx0 <- qf(1:N/(N+1), n1, n2)
    abline(0, 1, col=col2, lwd=lwd)
    points(xx0, yy, pch=19, cex=0.6, col=3)
    points(xx, yy, pch=19, cex=0.6, col=1)

    legend(pos02, sapply(leg, as.expression),
        pch=c(19,19), col=c("1","green"), cex=cex, bg="white")

    legend(pos2, tab1, ncol=3, cex=cex, text.col=rep(c(1,"red","blue",1),3),
           xjust=1, x.intersp=0, text.width=NA, bg="white")
  # Return
    invisible(tab)
}

# [9-6] Diagnose the Central Limit Theorem
# Generate random variables and standardized statistics
genstat <- function(dist, para, para2, ns, N, seed, sigknow) {
  # Probability distribution names and serial numbers
    dlist <- c("exp", "gamma", "weibull", "beta", 
              "norm", "t", "chisq", "f", "pois", "binom")
    dnum <- grep(paste0("^", dist), dlist)
    dist <- dlist[dnum]
    if(length(dnum)!=1) stop("Input correct name of distribution.")

  # Function name for generating random variables
    rdist <- paste0("r", dist)

  # Expected value and standard deviation
    Ex <- switch(dnum, 1/para, para*para2, para2*gamma(1+1/para), 
            para/(para+para2), para, 0, para, 
            ifelse(para2>2, para2/(para2-2), Inf), para, ns*para )
    Vx <- switch(dnum, 1/para^2, para*para2^2, 
            para2^2*(gamma(1+2/para)-gamma(1+1/para)^2),
            para*para2/(para+para2)^2/(para+para2+1), para2^2,
            ifelse(para>2, para/(para-2), ifelse(para>1, Inf, NA)), 2*para,
            ifelse(para2>4, 2*para2^2*(para+para2-2)/para/(para2-2)^2/(para2-4), 
            ifelse(para2>2, Inf, NA)), para, ns*para*(1-para) )
    Dx <- sqrt(Vx)

  # Generate random variables standardized statistics
    sgr <- rep(1:N, each=ns)
    set.seed(seed)
    if (dist %in% c("exp", "t", "chisq", "pois")) {
        dat <- do.call(rdist, list(N*ns, para))
    } else if (dist == "gamma") {
        dat <- do.call(rdist, list(N*ns, para, 1/para2))
    } else if (dist == "binom") {
        dat <- do.call(rdist, list(N, ns, para))
    } else {
        dat <- do.call(rdist, list(N*ns, para, para2))
    }

  # Two cases for sigma known or unknown
    if (sigknow) {
        if (dist == "binom") {
            stat <- (dat-Ex)/Dx
        } else {
            stat <- tapply(dat, sgr, function(x) (mean(x)-Ex)/Dx*sqrt(ns))
        }
    } else    {
        if (dist == "binom") {
            stat <- (dat-Ex)/sqrt(dat*(1-dat/ns))
        } else {
            xmean <- tapply(dat, sgr, mean)
            xstd <-  tapply(dat, sgr, sd)
            # Remove cases for standard deviation = 0
            stat <- ((xmean-Ex)/xstd*sqrt(ns))[xstd>0]
        }
    }
    # Return generated statistics
    invisible(stat)
}

# Verify the central limit theorem
#' @title Diagnosis of the Central Limit Theorem
#' @description Diagnosis of the Central Limit Theorem
#' @param dist Name of population distribution ("exp","gamma","weibull","beta","norm", "t","chisq","f","pois","binom")
#' @param para First parameter of the distribution.
#' @param para2 Second parameter of the distribution. (if necessary)
#' @param ns Sample size, Default: c(10, 30, 50).
#' @param d Group width in histogram, Default: rep(0.5, 3).
#' @param N Number of iterations, Default: 10000.
#' @param seed Seed value for generating random numbers, Default: 9857
#' @param sigknow Logica: is population variance known? Default: TRUE.
#' @param ... Other graphic parameters. (seed, col, fill, lwd, pch, pt.col)
#' @return None.
#'
#' @examples
#' # t-distribution
#' clt.plot("t", para=3)
#' clt.plot("t", para=3, breaks=50)
#' clt.plot("t", para=3, N=10000, breaks=50)
#' # Exponential distribution
#' clt.plot("exp", para=5, N=10000)
#' clt.plot("exp", para=5, ns=c(1,3,5,10)*10, breaks=50)
#' # Gamma distribution
#' clt.plot("gam", para=0.5, para2=10)
#' clt.plot("gam", para=0.5, para2=10, ns=c(1,3,5,10)*10, breaks=50)
#' clt.plot("gam", para=0.5, para2=10, ns=c(1,3,5,10)*10, N=10000, breaks=100)
#' @rdname clt.plot
#' @export
clt.plot <- function(dist, para, para2, ns, N=1000, sigknow=TRUE, ...) {
  # Probability distribution name and graph title
    dlist <- c("exp", "gamma", "weibull", "beta", "norm", 
            "t", "chisq", "f", "pois", "binom")
    dname <- c("Exp", "Gam", "Wei", "Beta", "Norm", 
            "T", "Chisq", "F", "Poi", "Binom")
  # Check Input
    if (missing(dist)) {
        cat(paste(dlist, collapse=", "), "\n")
        stop("Input one of the distribution above....")
    }

    fulllist <- c("exponential", "gamma", "weibull", "beta", "normal",
               "t", "chisquared", "f", "poisson", "binomial")
    dist <- tolower(dist)
    dist <- sub(" ", "", dist)
    dist <- sub("-", "", dist)
    dist <- sub("distribution", "", dist)
    dist <- sub("dist", "", dist)
    dnum <- grep(paste0("^", dist), fulllist)
    dist <- dlist[dnum]

    if(length(dnum)!=1) {
        cat(paste(dlist, collapse=", "), "\n")
        stop("Input one of the distribution above....")
    }
  # para2 must be scalar or missing...
    if (!missing(para2) && length(para2)>1) para2 <- para2[1]

  # Calculate standardized statistics
    if (missing(ns)) ns <- c(10,30,50)
    d <- rep(0.5, 3)

  # Generate ns sample means, iterate N times
    seed <- 9857
    dots <- list(...)
    pars <- names(dots)
    if ("seed" %in% pars) seed <- dots$seed
    set.seed(seed)

    Mns <- TRUE
    mm <- length(ns)
    zs <- list()

    if (mm > 1) {
        para <- rep(para[1], mm)
    } else if (length(para)>1) {
        Mns <- FALSE
        mm <- length(para)
        ns <- rep(ns, mm)
    }

    for (k in 1:mm) {
        zs[[k]] <- genstat(dist, para[k], para2, ns[k], N, seed, sigknow)
    }

  # Set titles, axes, and graphic parameters
    if(dnum %in% c(1,6,7,9,10)) {
        mt1 <- paste0(dname[dnum], "(", para, ")  n=", ns)
    } else {
        mt1 <- paste0(dname[dnum], "(", para, ",", para2, ")  n=", ns)
    }
    # Binomial
    if (dnum==10) mt1 <- paste0(dname[dnum], "(n, ", para, ")  n=", ns)

    if(Mns) {
        mt2 <- paste0("Normal Q-Q Plot:  n=", ns)
    } else {
        mt2 <- paste0("Normal Q-Q Plot:  par1=", para)
    }

    lwd <- 2
    fill <- "yellow"
    col1 <- "red"
    col2 <- "red"
    yl1 <- "f(x)"
    xl1 <- NULL
    xl2 <- "Theoretical Quantiles"
    yl2 <- "Sample Quantiles"
    xrng <- c(-4, 4)
    ng0 <- rep(16, mm)
    pch <- 19
    pt.col <- 1
    cex <- 0.6

  # Set the list of arguments
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("breaks" %in% pars) {
            ng0 <- dots$breaks
            if (length(ng0)==1) ng0 <- rep(ng0, mm)
        }
        if ("main" %in% pars) {
            mt1 <- dots$main
            if (length(mt1==1)) mt1 <- rep(mt1, mm)
        } 
        if ("col" %in% pars) {
            col <- dots$col
            if (length(col)>1) {
                col1 <- col[1]
                col2 <- col[2]
            } else col1 <- col
        }
        if ("pos" %in% pars) {
            pos <- dots$pos
            if (length(pos)>1) {
                pos1 <- pos[1]
                pos2 <- pos[2]
            } else pos1 <- pos2 <- pos
        } 
        if ("fill" %in% pars) fill <- dots$fill
        if ("ylab" %in% pars) {
            yl <- dots$ylab
            if (length(yl)>1) {
                yl1 <- yl[1]
                yl2 <- yl[2]
            } else yl1 <- yl
        }
        if ("xlab" %in% pars) {
            xl <- dots$xlab
            if (length(xl)>1) {
                xl1 <- xl[1]
                xl2 <- xl[2]
            } else xl1 <- xl
        }
        if ("xlim" %in% pars) xrng <- dots$xlim
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("cex" %in% pars) cex <- dots$cex
        if ("pch" %in% pars) pch <- dots$pch
        if ("pt.col" %in% pars) pt.col <- dots$pt.col
    }

  # Display graph
    win.graph(3*mm, 5)
    par(mfrow=c(2,mm))
    par(mar=c(4,4,3,2))
    d <- (xrng[2]-xrng[1])/ng0

    for (j in 1:mm) {
      # Set histogram range: centering 0, width d[j], covering all values
        brk <- c(rev(seq(-d[j]/2, min(zs[[j]])-d[j], by=-d[j])), 
                seq(d[j]/2, max(zs[[j]])+d[j], by=d[j]))
        H0 <- hist(zs[[j]], breaks=brk, plot=FALSE)
        ymax <- max(0.5, H0$dens)

        hist(zs[[j]], breaks=brk, prob=T, xlim=xrng, ylim=c(0, ymax), 
            col=fill, ylab=yl1, xlab=xl1, main=mt1[j])
        curve(dnorm, xrng[1], xrng[2], lwd=lwd, col=col1, add=T)
    }
  # Normal probability plot with selected 100 points, using qqnorm( ) function
    set.seed(47)
    for (j in 1:mm) {
        ##zss <- sort(zs[[j]])[(0:99)*100+50]
        ##zss <- sort(zs[[j]])
        ##temp <- qqnorm(zss, main=mt2[j], ylab=yl2, xlab=xl2, pch=pch, cex=cex)
        ##abline(lm(temp$y ~ temp$x), lwd=lwd, col=col2)
        qq <- qqnorm(zs[[j]], plot.it=FALSE)
        plot(xrng, xrng, type="n", main=mt2[j], ylab=yl2, xlab=xl2)
        grid(col=3)
        ##qqline(zs[[j]], lwd=lwd, col=3)
        ##abline(lm(qq$y ~ qq$x), lwd=lwd, col=3)
        abline(0, 1, lwd=lwd, col=col2)
        points(qq$x, qq$y, pch=pch, cex=cex, col=pt.col)
        legend("topleft", c("Normal"), lwd=lwd, col=col2, bg="white")
        legend("bottomright", c("Simulation"), pch=pch, col=pt.col, bg="white")
      # Not Run ...
        GOF <- FALSE
        if (GOF) {
          # R-square
            x.n <- qnorm((1:N)/(N+1))
            y.n <- sort(zs[[j]])
            rsq <- summary(lm(y.n ~ x.n))$r.squared
            legend("bottomright", c("R-square", format(round(rsq,4),nsmall=4)), 
                   text.col="blue", bg="white")

          # GoF Tests
            ks.pv <- ks.test(zs[[j]], "pnorm")$p.val
            cv.pv <- goftest::cvm.test(zs[[j]], "pnorm")$p.val
            ad.pv <- goftest::ad.test(zs[[j]], "pnorm")$p.val
            print(c(ks.pv, cv.pv, ad.pv))
        }
    }
}

# [Misc] Histogram of the generated statistics
testplot <- function(z, d, mt, n) {
    m <- length(n)
    win.graph(3*m, 6)
    par(mfrow=c(2,m))

    for (j in 1:m) {
      # Set histogram range: centering 0, width d[j], covering all values
        br <- c(rev(seq(-d[j]/2, min(z[[j]])-d[j], by=-d[j])), 
                seq(d[j]/2, max(z[[j]])+d[j], by=d[j]))
        dum <- hist(z[[j]], breaks=br, plot=FALSE)
        ymax <- max(0.4, dum$dens)

        hist(z[[j]], breaks=br, prob=T, xlim=c(-4,4), ylim=c(0, ymax), 
            col="yellow", ylab="f(x)",
            main=paste0(mt, "  n=", n[[j]]), xlab=NULL)
        curve(dnorm, -4, 4, lwd=2, col="red", add=T)
    }

  # Normal probability plot with selected 100 points, using qqnorm( ) function
    set.seed(47)
    for (j in 1:m) {
        zss <- sort(z[[j]])[(0:99)*100+50]
        temp <- qqnorm(zss, pch=19, cex=0.8)
        abline(lm(temp$y ~ temp$x), lwd=2, col="red")
    }
}
