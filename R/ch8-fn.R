# [Ch-8 Functions] ----------------------------------------------------------------------------------
# [Ch-8 Function Manual] -----------------------------------------

#' @title Manual for Ch.8
#' @description Ch8. Normal and Related Distributions
#' @param fn Function number (0~11), Default: 0
#' @return None.
#' @examples
#' ch8.man()
#' ch8.man(1)
#' @rdname ch8.man
#' @export
ch8.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] cont.spdf   \tPlot the PDF of Multiple Continuous Random Variables\n")
        cat("[2] norm.trans  \tCheck Probability Conservation of Standardization\n")
        cat("[3] snorm.cdf   \tPlot Standard Normal Cumulative Probability P(Z<z)\n")
        cat("[4] snorm.prob  \tInterval Probability of the Standard Normal Distribution\n")
        cat("[5] snorm.quant \tQuantile Plot of the Standard Normal Distribution\n")
        cat("[6] chi.prob    \tCumulative Probability of the Chi-square Distribution\n")
        cat("[7] chi.quant   \tQuantile Plot of the Chi-square Distribution\n")
        cat("[8] tnorm.comp  \tCompare T-distribution with the Standard Normal\n")
        cat("[9] tdist.prob  \tInterval Probabilities of the T-Distribution\n")
        cat("[10] tdist.quant\tQuantile Plot of the T-Distribution\n")
        cat("[11] f.prob     \tCumulative Probability of the F-distribution\n")
        cat("[12] f.quant    \tQuantile Plot of the F-distribution\n")
    }
    if (1 %in% fn) {
        cat("[1] Plot the PDF of Multiple Continuous Random Variables\n")
        cat("cont.spdf(dist, para, para2, ws=c(7,4), xp, sep=FALSE, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("dist\t Distribution name.")
        cat("(\"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", 
                \"t\", \"chisq\", \"f\")\n")
        cat("para\t First parameter vector of the distribution.\n")
        cat("para2\t Second parameter vector. (except \"exp\", \"t\", \"chisq\")\n")
        cat("[Optional Input]--------------------------\n")
        cat("ws\t Grapic window size, Default: c(7,4).\n")
        cat("xp\t Location vector for vertical lines.\n")
        cat("sep\t Logical: plot the PDF separately? Default: FALSE.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Check the Probability Conservation of Standardization\n")
        cat("norm.trans(mu, sig, a, b, prt=TRUE, dig=4, ws=c(9,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("mu\t Mean of the normal distribution\n")
        cat("sig\t Standard deviation of the normal distribution\n")
        cat("a\t Lower limit of X for P(a<X<b).\n")
        cat("b\t Upper limit of X for P(a<X<b).\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(9,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Plot Standard Normal Cumulative Probability P(Z<z)\n")
        cat("snorm.cdf(zp=-2:2, prt=TRUE, dig=4, ws=c(7,4), ...)\n")
        cat("[Optional Input]--------------------------\n")
        cat("zp\t Values of z for annotation, Default: -2:2.\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,5).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] Interval Probability of the Standard Normal Distribution\n")
        cat("snorm.prob(lb=-(1:4), ub=-lb, prt=TRUE, dig=4, ws=c(7,4), ...)\n")
        cat("[Optional Input]--------------------------\n")
        cat("lb\t Lower bound of z for P(lb<Z<ub), Default: -(1:4).\n")
        cat("ub\t Upper bound of z for P(lb<Z<ub), Default: -lb.\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,5).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (5 %in% fn) {
        cat("[5] Quantile Plot of the Standard Normal Distribution\n")
        cat("snorm.quant(pv, prt=TRUE, dig=4, ws=c(7,5), PDF=TRUE, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("pv\t Probability values to find the quantiles.\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,5).\n")
        cat("PDF\t Logical: plot the PDF? (o.w. CDF) Default: TRUE.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (6 %in% fn) {
        cat("[6] Cumulative Probability of the Chi-square Distribution\n")
        cat("chi.prob(nu, ub, lb=0, prt=TRUE, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("nu\t Degree of freedom of the chi-square distribution.\n")
        cat("ub\t Values of upper limits for F(ub).\n")
        cat("[Optional Input]--------------------------\n")
        cat("lb\t Values of lower limits for P(lb<X<ub), Default: 0.\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,5).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (7 %in% fn) {
        cat("[7] Quantile Plot of the Chi-square Distribution\n")
        cat("chi.quant(nu, pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("nu\t Degree of freedom of the chi-square distribution.\n")
        cat("pv\t Probability values to find the quantiles.\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,4).\n")
        cat("PDF\t Logical: plot the PDF? (o.w. CDF) Default: TRUE.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (8 %in% fn) {
        cat("[8] Compare T-distribution with the Standard Normal\n")
        cat("tnorm.comp(nu, ws=c(9,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("nu\t Degree of freedom for the T-distribution.\n")
        cat("[Optional Input]--------------------------\n")
        cat("ws\t Grapic window size, Default: c(9,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (9 %in% fn) {
        cat("[9] Interval Probabilities of the T-Distribution\n")
        cat("tdist.prob(nu, lb=-(1:4), ub=-lb, prt=TRUE, dig=4, ws=c(7,4), ...)\n")
        cat("nu Degrees of freedom.\n")
        cat("lb Lower bound of x values, Default: -(1:4).\n")
        cat("ub Upper bound of x values, Default: -lb.\n")
        cat("prt Logical: print the output? Default: TRUE.\n")
        cat("dig Number of decimal places, Default: 4.\n")
        cat("ws Grapic window size, Default: c(7,4).\n")
        cat("... Other graphic parameters.\n")
    }
    if (10 %in% fn) {
        cat("[10] Quantile Plot of the T-Distribution\n")
        cat("tdist.quant(nu, pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...)\n")
        cat("nu Degrees of freedom.\n")
        cat("pv Probability values to find the quantiles.\n")
        cat("prt Logical: print the output? Default: TRUE.\n")
        cat("dig Number of decimal places, Default: 4.\n")
        cat("ws Grapic window size, Default: c(7,4).\n")
        cat("PDF Logical: plot the PDF? (o.w. CDF) Default: TRUE.\n")
        cat("... Other graphic parameters.\n")
    }
    if (11 %in% fn) {
        cat("[11] Cumulative Probability of the F-distribution\n")
        cat("f.prob(nu1, nu2, ub, prt=TRUE, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("nu1\t Degree of freedom of the numerator.\n")
        cat("nu2\t Degree of freedom of the denominator.\n")
        cat("ub\t Values of upper limits for F(ub).\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (12 %in% fn) {
        cat("[12] Quantile Plot of the F-distribution\n")
        cat("f.quant(nu1, nu2, pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("nu1\t Degree of freedom of the numerator.\n")
        cat("nu2\t Degree of freedom of the denominator.\n")
        cat("pv\t Probability values to find the quantiles.\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the output? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Grapic window size, Default: c(7,4).\n")
        cat("PDF Logical: plot the PDF? (o.w. CDF) Default: TRUE.\n")
        cat("...\t Other graphic parameters.\n")
    }
}

# [8-1] Plot the PDF of Multiple Continuous Random Variables
# Get the PDF of continuous random variables
getpdf8 <- function(dist, xa, para, para2) {
    np <- length(xa)
    N <- max(length(para), length(para2))

    # PDF name
    dpdf <- paste0("d", dist)
    pdf <- matrix(NA, nrow=np, ncol=N)

    # Vector of the PDF
    if (dist %in% c("exp", "t", "chisq")) {
        for (k in 1:N) {
            pdf[, k] <- do.call(dpdf, list(xa, para[k]))
        }
    } else if (dist == "gamma") {
        for (k in 1:N) {
            pdf[, k] <- do.call(dpdf, list(xa, para[k], 1/para2[k]))
        }
    } else { 
        for (k in 1:N) {
            pdf[, k] <- do.call(dpdf, list(xa, para[k], para2[k]))
        }
    }
    invisible(pdf)
}
# [8-1] Plot the PDF of Multiple Continuous Random Variables
#' @title Plot PDF of Continuous Random Variables
#' @description Plot the PDF of Multiple Continuous Random Variables
#' @param dist Distribution name ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f").
#' @param para First parameter vector of the distribution.
#' @param para2 Second parameter vector (except "exp", "t", "chisq").
#' @param ws Grapic window size, Default: c(7,4).
#' @param xp Location vector for vertical lines.
#' @param sep Logical: plot each PDF separately? Default: FALSE.
#' @param ... Other graphic parameters.

#' @return None.
#' @examples
#' # Four normal PDFs
#' mu <- c(0, 0, 2, 2)
#' sig <- c(1, 2, 1, 2)
#' cont.spdf("norm", mu, sig, xp=mu, xlim=c(-7,7))
#' # 12 normal PDFs
#' mu <- rep(1:4, 3); sig <- rep(1:3, each=4)
#' cont.spdf("norm", mu, sig, xp=mu, sep=TRUE, xlim=c(-5,10))
#' # Four F PDFs
#' cont.spdf("f", 5:8, 8:11, sep=TRUE)
#' @rdname cont.spdf
#' @export
cont.spdf <- function(dist, para, para2, ws=c(7,4), xp, sep=FALSE, ...) {
  # Number of parameters and their names
    N <- length(para)
    pn <- deparse(substitute(para))

  # Require para2 except the exponential distribution
    if (missing(para2)) {
        para2 <- para
    } else {
        pn2 <- deparse(substitute(para2))
        N2 <- length(para2)
        if (N==1 & N2>1) {
            para <- rep(para, N2)
            pn <- deparse(substitute(para2))
        }
        if (N>1 & N2==1) para2 <- rep(para2, N)
        N <- max(N,N2)
    }

  # Probability distribution name
    # ("Exponential", "Gamma", "Weibull", "Beta", "normal", "T-", "Chi-square", "F-")
    dlist <- c("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
    if (missing(dist)) {
        cat(paste(dlist, collapse=", "), "\n")
        stop("In put one of the distribution names above....")
    }
    distnum <- grep(dist, dlist)
    if (length(distnum) != 1) stop("Errors in probability distribution name...")
    dist <- dlist[distnum]

  # Calculate the range of x
    if (missing(para2)) para2 <- NA
    res <- list()
    for (k in 1:N) res[[k]] <- cdist.exp(distnum+1, para[k], para2[k], prt="n")
    low <- min(sapply(res, function(x) x$xrng[1]))
    upp <- max(sapply(res, function(x) x$xrng[2]))
    mu <- sapply(res, function(x) x$Ex)

    xlim <- c(low, upp)
    low2 <- min(sapply(res, function(x) x$Ex - 4*sqrt(x$Vx)))
    upp2 <- max(sapply(res, function(x) x$Ex + 4*sqrt(x$Vx)))

    if (xlim[1]==-Inf) xlim[1] <- low2
    if (xlim[2]== Inf) xlim[2] <- upp2

  # Set xlim
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("xlim" %in% ndots) xlim <- dots$xlim
    }

    xa <- seq(xlim[1], xlim[2], length=100)
    pdf <- getpdf8(dist, xa, para, para2)
    ymax <- max(pdf)*1.1
    ylim <- c(0, ymax)
    lwd <- 2

  # Separate Graphs ---------------------------
    if (sep==TRUE) {
      # [Correction]
        if (N>12) stop("The number of parameter values should not exceed 12...")
      # Main title label for separate graphs
        mt <- list()
        for (k in 1:N) mt[[k]] <- switch(distnum,
            bquote( Exp ( lambda == .(para[k]) ) ),
            bquote( Gamma ( alpha == .(para[k]) , theta == .(para2[k]) ) ),
            bquote( Weib ( alpha == .(para[k]) , theta == .(para2[k]) ) ),
            bquote( Beta ( alpha == .(para[k]) , beta == .(para2[k]) ) ),
            bquote( N ( mu == .(para[k]) , sigma^2 == .(para2[k]^2) ) ),
            bquote( T ( nu == .(para[k]) ) ),
            bquote( chi^2 ~ ( nu == .(para[k]) ) ),
            bquote( F ( nu[1] == .(para[k]) , nu[2] == .(para2[k]) ) )  )

      # Colors and labels
        dcol <- rep("red", N)
        yl  <- rep("f(x)", N)
        xl  <- paste0("(", letters[1:N], ")")

      # Divide graphic window (1~12)
        nr <- switch(N, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3)
        nc <- switch(N, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4)
        ## win.graph(3.5*nc, 3*nr)
        win.graph(ws[1], ws[2])
        par(mfrow=c(nr, nc))
        par(mar=c(4,4,3,2))
      # Set the list of arguments
        dots <- list(...)
        if (length(dots) > 0) {
            ndots <- names(dots)
            if ("main" %in% ndots) {
                mt <- dots$main
                if (length(mt)==1) mt <- rep(mt, N)
                mt <- as.list(mt)
            }
            if ("col" %in% ndots) {
                dcol <- dots$col
                if (length(dcol)==1) mt <- rep(dcol, N)
            }
            if ("ylab" %in% ndots) {
                yl <- dots$ylab
                if (length(yl)==1) yl <- rep(yl, N)
            }
            if ("xlab" %in% ndots) {
                xl <- dots$xlab
                if (length(xl)==1) xl <- rep(xl, N)
            }
            if ("ylim" %in% ndots) ylim <- dots$ylim
            if ("lwd" %in% ndots) lwd <- dots$lwd
        }
      # Plot the PDF
        for (k in 1:N) {
            plot(xa, pdf[ ,k], type="l", main=mt[[k]], xlim=xlim, 
                ylim=ylim, lwd=lwd, col=dcol, ylab=yl[k], xlab=xl[k] )
            if (missing(xp)) {
                abline(v=mu[k], lty=2, col="blue")
            } else {
                abline(v=xp[k], lty=2, col="blue")
            }
        }
     }

  # Simultaneous Graphs --------------------------
    if (sep==FALSE) {
        # Legend label for simultaneous graphs
        leg <- list()
        for (k in 1:N) leg[[k]] <- switch(distnum,
            bquote( Exp ( .(para[k]) ) ),
            bquote( Gamma (.(para[k]) ,  .(para2[k]) ) ),
            bquote( Weib ( .(para[k]) ,  .(para2[k]) ) ),
            bquote( Beta (  .(para[k]) ,  .(para2[k]) ) ),
            bquote( N ( .(para[k]) ,  .(para2[k])^2 ) ),
            bquote( T (  .(para[k]) ) ),
            bquote( chi^2 ~ ( .(para[k]) ) ),
            bquote( F (  .(para[k]) ,  .(para2[k]) ) )  )

      # Title, Colors and labels
        Distlist <- c("Exponetial", "Gamma", "Weibull", "Beta", 
                    "Normal", "T -", "Chi square", "F -")
        mt <- paste("PDF of", Distlist[distnum], "Distributions")

        dcol <- c("red", "blue", "green3", "purple", gray(0.4), "orange",
                  "metalblue", "magenta", "cyan", "gold", "pink", "brown")
        if (N > 12) dcol <- rainbow(N)
        yl  <- "f(x)"
        xl  <- "x"

      # Set the list of arguments
        dots <- list(...)
        if (length(dots) > 0) {
            ndots <- names(dots)
            if ("main" %in% ndots) mt <- dots$main
            if ("col" %in% ndots) {
                dcol <- dots$col
                if (length(dcol)==1) dcol <- rep(dcol, N)
            }
            if ("ylab" %in% ndots) yl <- dots$ylab
            if ("xlab" %in% ndots) xl <- dots$xlab
            if ("xlim" %in% ndots) xlim <- dots$xlim
            if ("ylim" %in% ndots) ylim <- dots$ylim
            if ("lwd" %in% ndots) lwd <- dots$lwd
        }

      # Open graphic window
        win.graph(ws[1], ws[2])
        par(mar=c(4,4,3,2))
      # Plot the PDF
        plot(xa, pdf[ ,1], type="n", main=mt, xlim=xlim, 
            ylim=ylim, ylab=yl, xlab=xl)
        grid(col=3)
        for (k in 1:N) {
            lines(xa, pdf[ ,k], type="l", lwd=lwd, col=dcol[k])
            if (!missing(xp)) abline(v=xp[k], lty=2, col=4)
            if (N <= 12) {
                maxy <- max(pdf[ ,k])
                maxx <- xa[pdf[ ,k]==maxy][1]
                text(maxx, maxy, leg[[k]], pos=3, offset=0.2, cex=0.9)
            }
        }
        # if (N <= 12) legend("topright", legend=unlist(leg), lwd=2, col=dcol)
     }
}

# [8-2] Check Probability Conservation in Standardizing the Normal Distribution
#' @title Standardization of the Normal Distribution
#' @description Check Probability Conservation in Standardizing the Normal Distribution
#' @param mu Mean of the normal distribution.
#' @param sig Standard deviation of the normal distribution.
#' @param a Lower limit of X for P(a<X<b).
#' @param b Upper limit of X for P(a<X<b).
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(9,4).
#' @param ... Other graphic parameters.
#' @return None.
#' @examples
#' # X ~ N(2, 2): Pr(-1 < X < 4)
#' norm.trans(2, 2, -1, 4)
#' # X ~ N(100, 4): Pr(-95 < X < 108)
#' norm.trans(100, 4, 95, 108)
#' @rdname norm.trans
#' @export
norm.trans <- function(mu, sig, a, b, prt=TRUE, dig=4, ws=c(9,4), ...) {
  # Calculate the original probability
    px <- pnorm(b, mu, sig) - pnorm(a, mu, sig)
    cat(paste0("Pr(", a, " < X < ", b, ") = "), round(px, dig), "\n")
    np <- 100
  # Calculate the standardized probability
    c <- (a-mu)/sig
    d <- (b-mu)/sig
    pz <- pnorm(d) - pnorm(c)
    cat(paste0("Pr(", round(c, dig), " < Z < ", round(d, dig), ") = "), 
        round(pz, dig), "\n")

  # Calculate the normal PDF
    span <- 6
    low <- mu - span
    upp <- mu + span
  # [Correction] If sigma >=3, or <=1/3, reset x and y scales.
    x1 <- seq(low, upp, length=np)
    if (sig >=3 | sig <=1/3) {
        x1 <- seq(mu - span*sig, mu + span*sig, length=np)
    }
    x0 <- seq(low-mu, upp-mu, length=np)
    fx <- matrix(c(dnorm(x0, 0, 1), dnorm(x1, mu, sig)), ncol=2, byrow=F)
    ymax <- max(fx)
    ysc1 <- ysc0 <- 1
    if (sig >= 3) ysc1 <- 1/sig
    if (sig <= 1/3) ysc0 <- sig

  # Set the title and the graphic window
    mt1 <- bquote(N( mu == .(mu) ,  sigma^2 == .(sig^2) ) )
    mt0 <- bquote(N( mu == 0 ,  sigma^2 == 1 ) )
    col <- c("red", "red")
    ##yl <- c("f(x)", "\U03D5(z)")
    yl <- c(expression(f(x)), expression(phi(z)))
    xl <- c("x", "z")
    fill <- rep("lightcyan", 2)
    lwd <- 2
    cex <- 1

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) {
            mt <- dots$main
            if (length(mt)==1) {
                mt1 <- mt0 <- mt
            } else {
                mt1 <- mt[1]
                mt0 <- mt[2]
            }
        }
        if ("col" %in% ndots) {
            col <- dots$col
            if (length(col)==1) col <- rep(col, 2)
        }
        if ("fill" %in% ndots) {
            fill <- dots$fill
            if (length(fill)==1) fill <- rep(fill, 2)
        }
        if ("ylab" %in% ndots) {
            yl <- dots$ylab
            if (length(yl)==1) yl <- rep(yl, 2)
        }
        if ("xlab" %in% ndots) {
            xl <- dots$xlab
            if (length(xl)==1) xl <- rep(xl, 2)
        }
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }

  # Plot
    win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))
    par(mar=c(4,4,3,2))

  # Plot the normal PDF
    plot(x1, fx[,2], type="n", main=mt1, ylim=c(0, ymax*ysc1), 
        ylab=yl[1], xlab=xl[1])
    grid()
    cord.x <- c(a, seq(a, b, 0.01), b)
    cord.y <- c(0, dnorm(seq(a, b, 0.01), mu, sig), 0)

  # Plot polygon by polygon() function
    polygon(cord.x, cord.y, col=fill[1])
    ab <- (a+b)/2
    text(ab, 0.4*dnorm(ab, mu, sig), labels=paste0("P(",a,"<X<",b,")\n=",
        round(px, dig)), cex=cex)
    lines(x1, fx[,2], lwd=lwd, col=col[1])

  # Plot the standard normal PDF
    plot(x0, fx[,1], type="n", main=mt0, ylim=c(0, ymax*ysc0), 
        ylab=yl[2], xlab=xl[2])
    grid()
    cord.x <- c(c, seq(c, d, 0.01), d)
    cord.y <- c(0, dnorm(seq(c, d, 0.01)), 0)
    polygon(cord.x, cord.y, col=fill[2])
    cd <- (c+d)/2
    text(cd, 0.4*dnorm(cd), labels=paste0("P(",round(c, dig),"<Z<",
        round(d, dig),")\n=",round(pz, dig)), cex=cex)
    lines(x0, fx[,1], lwd=lwd, col=col[2])
}

# [8-3] Plot Standard Normal Cumulative Probability P(Z<z)
#' @title Plot Standard Normal Cumulative Probability
#' @description Plot Standard Normal Cumulative Probability P(Z<z)
#' @param zp Values of z for annotation, Default: -2:2.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @examples
#' snorm.cdf()
#'
#' zp <- seq(-2, 2, by=0.5)
#' snorm.cdf(zp)
#' @rdname snorm.cdf
#' @export
snorm.cdf <- function(zp=-2:2, prt=TRUE, dig=4, ws=c(7,4), ...) {

    yp <- pnorm(zp)
    if (prt) {
        tab <- yp
        names(tab) <- paste0("Phi(", zp, ")")
        print(round(tab, dig))
    }
  # Set the title and axis
    mt <- "Cumulative Probabilities of the Standard Normal Distribution"
    xlim <- c(-4, 4)
    lwd <- 2
    col <- "red"
    ylim <- c(-0.1, 1)
    yl <- bquote(Phi(z))
    xl <- "z"

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab

        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("ylim" %in% ndots) ylim <- dots$ylim
        if ("lwd" %in% ndots) lwd <- dots$lwd
    }

    xa <- seq(xlim[1], xlim[2], length=100)
    low1 <- xlim[1] - 0.12*(xlim[2]-xlim[1])
    low2 <- (low1*2 + xlim[1])/3

  # Plot the cumulative probability
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

    plot(xa, pnorm(xa), type ="n", main=mt,
        ylim=ylim, xlim=c(low1, xlim[2]), ylab=yl, xlab=xl)
    abline(h=0, col="green2")
    lines(xa, pnorm(xa), type="l", lwd=lwd, col=col)

  # Display the cumulative probabilities up to zp
    segments(zp, 0, zp, yp, lty=2, col="blue")
    segments(zp, yp, xlim[1], yp, lty=2, col="blue")
    text(zp, 0, labels=zp, pos=1, cex=0.8)
    text(low2, yp, labels=round(yp, dig), cex=0.8)
}

# [8-4] Interval Probability of the Standard Normal Distribution
#' @title Interval Probability of the Standard Normal Distribution
#' @description Interval Probability of the Standard Normal Distribution.
#' @param lb Lower bound of z values, Default: -(1:4).
#' @param ub Upper bound of z values, Default: -lb.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Interval probabilities.
#' @examples
#' zp <- 1:3
#' snorm.prob(zp)
#'
#' snorm.prob(-4:0*1.5, 0:4*1.5)
#' @rdname snorm.prob
#' @export
snorm.prob <- function(lb=-(1:4), ub=-lb, prt=TRUE, dig=4, ws=c(7,4), ...) {
  # Find the interval probability P(lb<Z<ub)
    prz <- pnorm(ub)-pnorm(lb)
    tab <- prz
    names(tab) <- paste0("P(", lb, ":", ub, ")")
  # Print
    if (prt) print(round(tab, dig))

  # Set the title and axis
    mt <- "Interval Probability of the Standard Normal Distribution"
    zrng <- c(-4, 4)
    lwd <- 2
    col <- "red"
    yl <- bquote(phi(z))
    xl <- "z"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) zrng <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }
    x1 <- min(zrng[1], lb)
    x2 <- max(zrng[2], ub)
    xa <- seq(x1, x2, length=100)
    ymax <- max(dnorm(xa))
    nzp <- length(lb)
    yd <- 0.8*ymax/(nzp+1)

 # Plot the interval probability
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

    plot(xa, dnorm(xa), type ="n", main=mt,
        ylim=c(0, ymax*1.05), xlim=c(x1, x2), ylab=yl, xlab=xl)
    abline(h=0, col="green4")
    lines(xa, dnorm(xa), type="l", lwd=lwd, col=col)

  # Display the interval probability P(lb<Z<ub)
    abline(v=c(lb, ub), lty=2, col=4)
    yp <- yd*(nzp:1-0.5)
    arrows(lb, yp, ub, yp, length=0.1, code=3, col="blue")
    text((lb+ub)/2, yp, labels=paste0("P(", lb,"<Z<", ub,")=", 
        round(prz, dig)), pos=3, offset=0.2, cex=cex)
    invisible(tab)
}

# [8-5 PDF type] Quantile Plot of the Standard Normal Distribution
#' @title Quantile Plot of the Standard Normal Distribution
#' @description Quantile Plot of the Standard Normal Distribution.
#' @param pv Probability values to find the quantiles.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param PDF Logical: plot the PDF? (o.w. CDF) Default: TRUE.
#' @param ... Other graphic parameters.
#' @return Standard normal quantiles.
#' @examples
#' pv <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' snorm.quant(pv)
#'
#' snorm.quant(pv, col=3, lwd=5, cex=1)
#'
#' @rdname snorm.quant
#' @export
snorm.quant <- function(pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...) {
  # Check Input
    if (missing(pv)) stop("Input probability values to find the quantiles.")
  # Quantiles
    zv <- qnorm(pv)
    npv <- length(pv)
    names(zv) <- pv
    if (prt) print(round(zv, dig))

  # Quantiles Graph
    mt <- "Quantiles of the Standard Normal Distribution"
    xlim <- c(-3, 3)
    yl <- bquote(phi(z))
    xl <- "z"
    lwd <- 2
    col <- "red"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }

  # Set Axes
    x1 <- min(xlim[1], qnorm(min(pv)))
    x2 <- max(xlim[2], qnorm(max(pv)))
    xmin <- x1 - 0.1*(x2-x1)
    xa <- seq(x1, x2, length=100)
    pdf <- dnorm(xa)
    ymax <- max(pdf)
    npv <- length(pv)

 # Plot the Quantiles
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

 # PDF Type
    if (PDF) {
        plot(xa, dnorm(xa), type ="n", main=mt,
            ylim=c(0, ymax*1.05), xlim=c(xmin, x2), 
            ylab=yl, xlab=xl)
        abline(h=0, col="green4")
        yp <- ymax*0.8*(npv:1)/npv
        segments(zv, 0, zv, yp, lty=3, col=4)
        segments(0, 0, 0, dnorm(0), col=4)
        lines(xa, dnorm(xa), type="l", lwd=lwd, col=col)

      # Display Quantiles
        fzv <- format(zv, digits=dig, nsmall=dig)
        fpv <- format(pv, digits=dig, nsmall=3)
        arrows(x1, yp, zv, yp, length=0.07, code=2, angle=15, lty=1, col="blue")
        text(x1, yp, paste0("p=",fpv), cex=cex, pos=2, col="blue")
        text(zv, yp, fzv, pos=4, offset=0.2, cex=cex)
  # CDF Type
    } else {
        xmax <- x2 + 0.1*(x2-x1)
        plot(xa, pnorm(xa), type ="l", main=mt, lwd=lwd, col=col, 
            xlim=c(xmin, xmax), ylab=bquote(Phi(z)), xlab="x")
        abline(h=0, lty=1, col=grey(0.5))

      # Display quantiles
        ymax <- max(pnorm(xa))
        abline(v=c(xlim[1], 0), col=grey(0.5), lty=3)
        yp <- ymax*(1:npv)/(npv+1)
        segments(zv, 0, zv, pmax(yp, pv), lty=2, col=4)
        fpv <- format(pv, nsmall=3)
        arrows(xlim[1], yp, zv, yp, length=0.07, code=2, col="blue")
        text(xlim[1], yp, paste0("p=", fpv), pos=2, col="blue", cex=cex)
        text(zv, yp, round(zv, dig), pos=4, cex=cex)
    }

  # Inverse CDF Type
    ICDF <- FALSE
    if (ICDF) {
        main <- "Quantile Plot of the Standard Normal Distribution"
        p <- 1:399/400
        plot(p, qnorm(p), type ="l", lwd=2, col="red", 
            ylim=c(-4.4, 3), xlim=c(-0.15, 1),
            ylab=bquote(Phi^-1 ~ (p)), xlab="p", main=main)
        abline(h=-3, lty=1, col=grey(0.5))
        segments(0.5, -3, 0.5, 4, lty=3, col=grey(0.5))
        segments(0, 0, 1.5, 0, lty=3, col=grey(0.5))

      # Display quantiles
        yp <- rep(-3-(1:4)/4, 10)
        yp <- yp[1:npv]
        segments(pv, yp, pv, zv, lty=2, col="blue")
        arrows(pv, zv, -0.02, zv, length=0.07, lty=2, col="blue")
        text(-0.02, zv, labels=format(zv, digits=dig, nsmall=dig), 
            pos=2, cex=0.8)
        text(pv, yp, labels=pv, cex=0.8, pos=1, col="red")
    }
    invisible(zv)
}

# [8-5 Step type] Quantile Plot of the Standard Normal Distribution
#' @title Quantile Plot of the Standard Normal Distribution
#' @description Quantile Plot of the Standard Normal Distribution (revised)
#' @param pv Vector of probability values
#' @param pv2 Vector of specific probability values
#' @param main Graph title, Default: 'Quantiles of the Standard Normal Distribution'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' pv <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' # Step type
#' snorm.quant1(pv)
#'
#' @rdname snorm.quant1
#' @export
snorm.quant1 <- function(pv, pv2=pv, dig=4, main) {
    if (missing(main)) main <- "Quantiles of the Standard Normal Distribution"
  # Quantiles
    zv <- qnorm(pv)
    names(zv) <- pv
    print(round(zv, dig))
    zv2 <- qnorm(pv2)
    dv2 <- dnorm(zv2)

  # Set axis
    x1 <- min(-3, qnorm(min(pv)))
    xmin <- x1*1.16
    x2 <- max(3, qnorm(max(pv)))
    x <- seq(x1, x2, length=100)
    pdf <- dnorm(x)
    ymax <- max(pdf)
    npv <- length(pv2)
    y1 <- 0.1*npv*ymax

  # Quantiles Graph
    wc <- ifelse(npv > 5, 6, 5)
    win.graph(7, wc)
    plot(x, pdf, type ="n", ylim=c(-y1, ymax), xlim=c(xmin, x2),
        ylab=bquote(phi(z)), xlab="z", main=main)
    abline(h=0, col="green4")
    lines(x, pdf, type="l", lwd=2, col="red")

  # Display major quantiles
    yp <- -0.1*ymax*(1:npv)
    segments(zv2, yp, zv2, dv2, lty=3, col=4)
    segments(0, 0, 0, dnorm(0), col=4)

    arrows(x1, yp, zv2, yp, length=0.07, code=2, angle=20, col="blue")
    text(x1, yp, paste0("p=", pv2), pos=2, col="blue", cex=0.8)
    text(zv2, yp, round(zv2, dig), pos=4, cex=0.8)
}

# [8-5 CDF type] Quantile Plot of the Standard Normal Distribution
#' @title Quantile Plot of the Standard Normal Distribution
#' @description Quantile Plot of the Standard Normal Distribution (CDF version)
#' @param pv Vector of probability values
#' @param pv2 Vector of specific probability values
#' @param main Graph title, Default: 'Quantiles of the Standard Normal Distribution'
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' pv <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' # CDF type
#' snorm.quant2(pv)
#'
#' @rdname snorm.quant2
#' @export
snorm.quant2 <- function(pv, dig=4, main, ws=c(7,6)) {
    if (missing(main)) main <- "Quantile Plot of the Standard Normal Distribution"
  # Quantiles of the standard normal distribution
    zv <- qnorm(pv)
    npv <- length(pv)
    names(zv) <- pv
    print(round(zv, dig))

  # Plot quantiles
    p <- 1:399/400
    win.graph(ws[1], ws[2])
    plot(p, qnorm(p), type ="l", lwd=2, col="red", 
        ylim=c(-4.4, 3), xlim=c(-0.15, 1),
        ylab=bquote(Phi^-1 ~ (p)), xlab="p", main=main)
    abline(h=-3, lty=1, col=grey(0.5))
    segments(0.5, -3, 0.5, 4, lty=3, col=grey(0.5))
    segments(0, 0, 1.5, 0, lty=3, col=grey(0.5))

  # Display quantiles
    yp <- rep(-3-(1:4)/4, 10)
    yp <- yp[1:npv]
    segments(pv, yp, pv, zv, lty=2, col="blue")
    arrows(pv, zv, -0.02, zv, length=0.07, lty=2, col="blue")
    text(-0.02, zv, labels=format(zv, digits=dig, nsmall=dig), pos=2, cex=0.8)

    text(pv, yp, labels=pv, cex=0.8, pos=1, col="red")
}

# [8-6] Cumulative Probability of the Chi-square Distribution
#' @title Cumulative Probability of the Chi-square Distribution
#' @description Cumulative Probability of the Chi-square Distribution.
#' @param nu Degree of freedom of the chi-square distribution.
#' @param ub Values of upper limits for F(ub).
#' @param lb Values of lower limits for P(lb<X<ub), Default: 0.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Cumulative (or Interval) Probabilities
#' @details DETAILS
#' @examples
#' k <- 1:10; nu <- 5
#' chi.prob(nu, ub=k)
#'
#' nu <- 5; k <- seq(1, 15, by=2)
#' chi.prob(nu, ub=k)
#' @rdname chi.prob
#' @export
chi.prob <- function(nu, ub, lb=0, prt=TRUE, dig=4, ws=c(7,4), ...) {
  # Check Input
    if (missing(nu)) stop("Input the degrees of freedom.")
    if (missing(ub)) stop("Input values ub for F(ub).")
    if (any(ub-lb<0)) stop("ub should be greater than lb.")
    nub <- length(ub)

  # Print the cumulative probabilities
    prx <- pchisq(ub, nu) - pchisq(lb, nu)
    names(prx) <- paste0("P(", lb, ":", ub, ")")
    if (prt) print(round(prx, dig))

  # Set the title and axis
    if (all(lb==0)) {
        mt <- bquote("Cumulative Probabilities " ~ F(x) == P(chi[.(nu)]^2 < x))
    } else {
        mt <- bquote("Interval Probabilities of" ~ chi[.(nu)]^2 )
    }
    xmax <- max(ub, qchisq(0.95, nu))
    xlim <- c(0, xmax) 
    lwd <- 2
    col <- "red"
    yl <- "f(x)"
    xl <- "x"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }
    x1 <- min(xlim[1], lb)
    x2 <- max(xlim[2], ub)
    xmin <- x1 - 0.1*(x2-x1)
    xmax <- x2 + 0.1*(x2-x1)
    xa <- seq(x1, xmax, length=100)
    pdf <- dchisq(xa, nu)
    ymax <- max(pdf)
    ymin <- -0.05*ymax
    nub <- length(ub)
    yp <- 0.9*ymax*(nub:1)/(nub+1)

  # Plot the chi-square PDF
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))
    plot(xa, dchisq(xa, nu), type ="n", main=mt,
        ylim=c(ymin,ymax), xlim=c(xmin,xmax), ylab=yl, xlab=xl)
    abline(h=0, col="green4")
    lines(xa, dchisq(xa, nu), type="l", lwd=lwd, col=col)

  # Display specific cumulative probabilities
    rpr <- round(prx, dig)
    abline(v=c(x1, nu), col=grey(0.5), lty=3)
    segments(ub, 0, ub, pmax(yp,dchisq(ub,nu)), lty=2, col=4)
    if (all(lb==0)) {
        arrows(x1, yp, ub, yp, length=0.07, code=2, col="blue")
        text(x1, yp, paste0("F(", ub, ")"), pos=2, cex=cex)
        text(ub, yp, format(rpr, nsmall=dig), pos=4, offset=0.2, cex=cex)
        text(ub, 0, round(ub, dig), pos=1, offset=0.2, col="blue", cex=cex)
    } else {
        segments(lb, 0, lb, pmax(yp,dchisq(ub,nu)), lty=2, col=4)
        arrows(lb, yp, ub, yp, length=0.07, code=3, col="blue")
        text(x1, yp, paste0("P(", lb, ":", ub, ")"), pos=2, cex=cex)
        text((lb+ub)/2, yp, format(rpr, nsmall=dig), pos=3, offset=0.1, cex=cex)
        ulb <- sort(unique(c(lb, ub)))
        text(ulb, 0, round(ulb, dig), pos=1, offset=0.2, col="blue", cex=cex)
    }
    Ex <- nu
    abline(v=c(x1, Ex), col=grey(0.5), lty=3)
    yp2 <- 0.9*ymax
    text(x1, yp2, paste0("F(mu)"), pos=2, cex=cex, col="blue")
    arrows(x1, yp2, Ex, yp2, length=0.07, code=2, col="blue")
    text(Ex, yp2, format(pchisq(Ex,nu), digits=dig, nsmall=dig), 
        pos=4, offset=0.2, cex=cex, col="blue")

    invisible(prx)
}

# [8-7] Quantile Plot of the Chi-square Distribution
#' @title Quantile Plot of the Chi-square Distribution
#' @description Quantile Plot of the Chi-square Distribution
#' @param nu Degree of freedom of the chi-square distribution
#' @param pv Probability values to find the quantiles.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param PDF Logical: plot the PDF? (o.w. CDF) Default: TRUE.
#' @param ... Other graphic parameters.
#' @return Quantiles of the Chi-square Distribution.
#' @examples
#' nu <- 5
#' pv <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' chi.quant(nu, pv)
#' chi.quant(nu, pv, PDF=FALSE)
#' @rdname chi.quant
#' @export
chi.quant <- function(nu, pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...) {
  # Check Input
    if (missing(nu)) stop("Input the degrees of freedom.")
    if (missing(pv)) stop("Input probability values for the quantiles.")

  # Find Quantiles of the chi-square distribution
    tab <- cv <- qchisq(pv, nu)
    names(tab) <- pv
    if (prt) print(round(tab, dig))

  # Set the title and axis
    mt <- bquote("Quantiles of the Chi-square Distribution"~chi[p~","~.(nu)]^2 )
    pmax <- max(pv, 0.95)
    xlim <- c(0, qchisq(pmax, nu)*1.1)
    lwd <- 2
    col <- "red"
    yl <- "f(x)"
    xl <- "x"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }

  # Set axis
    xa <- seq(xlim[1], xlim[2], length=100)
    xmin <- xlim[1] - 0.1*(xlim[2]-xlim[1])
    pdf <- dchisq(xa, nu)
    ymax <- max(pdf)
    npv <- length(pv)
    y1 <- 0.05*ymax

 # Plot the Quantiles
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

  # PDF Type
    if (PDF) {
        plot(xa, pdf, type ="n", ylim=c(-y1, ymax), xlim=c(xmin, xlim[2]),
            ylab=yl, xlab=xl, main=mt)
        abline(h=0, col="green4")
        lines(xa, pdf, type="l", lwd=lwd, col=col)

      # Display major quantiles
        abline(v=xlim[1], col=grey(0.5), lty=3)
        yp <- 0.9*ymax*(npv:1)/(npv+1)
        segments(cv, 0, cv, yp, lty=2, col=4)
        fpv <- format(pv, nsmall=3)
        arrows(xlim[1], yp, cv, yp, length=0.07, code=2, col="blue")
        text(0, yp, paste0("p=", fpv), pos=2, offset=0.3, col="blue", cex=cex)
        text(cv, yp, round(cv, dig), pos=4, offset=0.2, cex=cex)

  # CDF Type
    } else {
        plot(xa, pchisq(xa, nu), type ="l", lwd=lwd, col=col, 
            xlim=c(xmin, xlim[2]), ylim=c(-0.05,1),
            ylab="F(x)", xlab="x", main=mt)
        abline(h=0, lty=1, col="green4")

      # Display quantiles
        ymax <- max(pchisq(xa, nu))
        abline(v=xlim[1], col=grey(0.5), lty=3)
        yp <- ymax*(1:npv)/(npv+1)
        segments(cv, 0, cv, pmax(yp, pv), lty=2, col=4)
        fpv <- format(pv, nsmall=3)
        arrows(xlim[1], yp, cv, yp, length=0.07, code=2, col="blue")
        text(0, yp, paste0("p=", fpv), pos=2, offset=0.3, col="blue", cex=cex)
        text(cv, yp, round(cv, dig), pos=4, offset=0.2, cex=cex)
    }
    segments(nu, 0, nu, 1.2, col=grey(0.5), lty=3)
    text(nu, 0, "E(X)", pos=1, offset=0.2, cex=cex, col="blue")
    invisible(tab)
}

# [8-8] Compare T-distribution with the Standard Normal
#' @title Compare T-distribution with the Standard Normal
#' @description Compare T-distribution with the Standard Normal
#' @param nu Degree of freedom for the T-distribution.
#' @param ws Grapic window size, Default: c(10,4).
#' @param ... Other graphic parameters.
#' @return None.
#' @examples
#' nu <- c(1, 5, 10, 30)
#' tnorm.comp(nu)
#' tnorm.comp(10:100)
#' @rdname tnorm.comp
#' @export
tnorm.comp <- function(nu, ws=c(9,4), ...) {
  # Check Input
    if (missing(nu)) stop("Input the dgrees of freedom.")
    nnu <- length(nu)

  # Set the title and axis
    mt <- "N(0,1) and T-dist."
    lwd <- c(2, rep(1, nnu))
    yl <- "PDF"
    xl <- c("(a)", "(b)")
    cex <- 0.8
    xlim <- c(-3.5, 3.5)

    if (nnu <= 7) {
        dcol <- c("black", "red", "blue", "green2", 
                  "purple", "pink", "cyan", "orange")
    } else {
        dcol <- c("black", rainbow(nnu))
    }

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) {
            dcol <- dots$col
            if (length(dcol)==1) dcol <- rep(dcol, nnu+1)
        }
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) {
            xl <- dots$xlab
            if (length(xl)==1) xl <- rep(xl, 2)
        }
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) {
            lwd <- dots$lwd
            if (length(lwd)==1) lwd <- rep(lwd, nnu+1)
        }
        if ("cex" %in% ndots) cex <- dots$cex
    }

    xa <- seq(xlim[1], xlim[2], length=100)
    x1 <- xlim[1]-0.1*(xlim[2]-xlim[1])
    x2 <- xlim[2]+0.1*(xlim[2]-xlim[1])

    ymax <- max(dnorm(xa))

  # Set graphic window
    win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))
    par(mar=c(4,4,3,2))

  # Plot PDF => Fig (a)
    plot(xa, dnorm(xa), type="l", main=mt, lwd=lwd[1], 
        xlim=xlim, ylab=yl, xlab=xl[1])
    abline(v=0, lty=3, col=grey(0.5))
    for (i in 1:nnu) lines(xa, dt(xa, nu[i]), lwd=lwd[i+1], col=dcol[i+1])
    if (nnu<=10) legend("topright", c("N(0,1)", paste0("t(", nu, ")")), 
                      lwd=2, cex=cex, col=dcol)

  # PDF in log-scale => Fig (b)
    plot(xa, dnorm(xa), type="l", log="y", main=paste(mt, "(Log scale)"),
        lwd=lwd[1], xlim=xlim, ylab=paste0("log[",yl,"]"), xlab=xl[2])
    abline(v=0, lty=3, col=grey(0.5))
    for (i in 1:nnu) lines(xa, dt(xa, nu[i]), lwd=lwd[i+1], col=dcol[i+1])
}

# [8-9] Interval Probabilities of the T-Distribution
#' @title Interval Probabilities of the T-Distribution
#' @description Interval Probability of the T-Distribution.
#' @param nu Degrees of freedom.
#' @param lb Lower bound of x values, Default: -(1:4).
#' @param ub Upper bound of x values, Default: -lb.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Interval Probabilities.
#' @examples
#' xp <- 1:3
#' tdist.prob(nu=10, lb=-xp)
#'
#' tdist.prob(nu=15, -4:0*1.5, 0:4*0.5)
#' @rdname tdist.prob
#' @export
tdist.prob <- function(nu, lb=-(1:4), ub=-lb, prt=TRUE, dig=4, ws=c(7,4), ...) {
  # Check Input
    if (missing(nu)) stop("Input degrees of freedom.")
    if (any(ub-lb<0)) stop("ub should be greater than lb.")
    Nnu <- length(nu)
    if (Nnu>1) {
       cat("Only the first degrees of freedom will be used.\n")
       nu <- nu[1]
    }
  # Find the interval probability P(lb<T<ub)
    prz <- pt(ub,nu)-pt(lb,nu)
    tab <- prz
    names(tab) <- paste0("P(", lb, ":", ub, ")")
  # Print
    if (prt) print(round(tab, dig))

  # Set the title and axis
    mt <- paste0("Interval Probability of the T(", nu, ") Distribution")
    xrng <- c(min(lb), max(ub))
    lwd <- 2
    col <- "red"
    yl <- "f(x)"
    xl <- "x"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xrng <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }
    x1 <- min(xrng[1], lb)
    x2 <- max(xrng[2], ub)
    xlim <- c(x1, x2) + c(-0.1, 0.1)*(x2 - x1)
    xa <- seq(xlim[1], xlim[2], length=100)
    ymax <- max(dt(xa,nu))
    nzp <- length(lb)
    yd <- 0.8*ymax/(nzp+1)

 # Plot the interval probability
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

    plot(xa, dt(xa,nu), type ="n", main=mt,
        ylim=c(0, ymax*1.05), xlim=c(x1, x2), ylab=yl, xlab=xl)
    abline(h=0, col="green4")
    abline(v=0, col=grey(0.5), lty=3)
    lines(xa, dt(xa,nu), type="l", lwd=lwd, col=col)

  # Display the interval probability P(lb<Z<ub)
    abline(v=c(lb, ub), lty=2, col=4)
    yp <- yd*(nzp:1-0.5)
    arrows(lb, yp, ub, yp, length=0.1, code=3, col="blue")
    text((lb+ub)/2, yp, labels=paste0("P(", lb,"<T<", ub,")=", 
        round(prz, dig)), pos=3, offset=0.2, cex=cex)
    invisible(tab)
}

# [8-10] Quantile Plot of the T-Distribution
#' @title Quantile Plot of the T-Distribution
#' @description Quantile Plot of the T-Distribution.
#' @param nu Degrees of freedom.
#' @param pv Probability values to find the quantiles.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param PDF Logical: plot the PDF? (o.w. CDF) Default: TRUE.
#' @param ... Other graphic parameters.
#' @return T(nu) quantiles.
#' @examples
#' pv <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' tdist.quant(nu=5, pv)
#'
#' tdist.quant(nu=5, pv, ws=c(9,6), col=3, lwd=5, cex=1)
#'
#' @rdname tdist.quant
#' @export
tdist.quant <- function(nu, pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...) {
  # Check Input
    if (missing(nu)) stop("Input degrees of freedom.")
    if (missing(pv)) stop("Input probability values to find the quantiles.")
    if (length(nu)>1) {
        cat("Only the first value of degrees of freedom will be used.\n")
        nu <- nu[1]
    }
  # Quantiles
    zv <- qt(pv,nu)
    npv <- length(pv)
    names(zv) <- pv
    if (prt) print(round(zv, dig))

  # Quantiles Graph
    mt <- paste0("Quantiles of the T(", nu, ") Distribution")
    xlim <- c(-4.5, 4.5)
    xrng <- c(min(zv), max(zv))
    yl <- "f(x)"
    xl <- "x"
    lwd <- 2
    col <- "red"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }

  # Set Axes
    x1 <- min(xlim[1], xrng[1])
    x2 <- max(xlim[2], xrng[2])
    xmin <- x1 - 0.1*(x2-x1)
    xmax <- x2 + 0.1*(x2-x1)
    xa <- seq(x1, x2, length=100)
    pdf <- dt(xa, nu)
    ymax <- max(pdf)
    npv <- length(pv)

 # Plot the Quantiles
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

 # PDF Type
    if (PDF) {
        plot(xa, dt(xa,nu), type ="n", main=mt,
            ylim=c(0, ymax*1.05), xlim=c(xmin, xmax), 
            ylab=yl, xlab=xl)
        abline(h=0, col="green4")
        yp <- ymax*0.8*(npv:1)/npv
        segments(zv, 0, zv, yp, lty=2, col=4)
        abline(v=c(xlim[1], 0), col=grey(0.5), lty=3)
        lines(xa, dt(xa,nu), type="l", lwd=lwd, col=col)

      # Display Quantiles
        fzv <- format(zv, digits=dig, nsmall=dig)
        fpv <- format(pv, digits=dig, nsmall=3)
        arrows(x1, yp, zv, yp, length=0.07, code=2, angle=15, lty=1, col="blue")
        text(x1, yp, paste0("p=",fpv), cex=cex, pos=2, offset=0.2, col="blue")
        text(zv, yp, fzv, pos=4, offset=0.2, cex=cex)
  # CDF Type
    } else {
        plot(xa, pt(xa,nu), type ="l", main=mt, lwd=lwd, col=col, 
            xlim=c(xmin, xmax), ylab="F(x)", xlab="x")
        abline(h=0, lty=1, col=grey(0.5))

      # Display quantiles
        ymax <- max(pt(xa,nu))
        abline(v=c(xlim[1], 0), col=grey(0.5), lty=3)
        yp <- ymax*(1:npv)/(npv+1)
        segments(zv, 0, zv, pmax(yp, pv), lty=2, col=4)
        fpv <- format(pv, nsmall=3)
        arrows(xlim[1], yp, zv, yp, length=0.07, code=2, col="blue")
        text(xlim[1], yp, paste0("p=", fpv), pos=2, offset=0.2, col="blue", cex=cex)
        text(zv, yp, round(zv, dig), pos=4, offset=0.2, cex=cex)
    }
    invisible(zv)
}

# [8-11] Cumulative Probability of the F-distribution
#' @title Cumulative Probability of the F-distribution
#' @description Cumulative Probability of the F-distribution
#' @param nu1 Degree of freedom of the numerator.
#' @param nu2 Degree of freedom of the denominator.
#' @param ub Values of upper limits for F(ub).
#' @param lb Values for F(ub)-F(lb), Default: 0.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Cumulative probabilities.
#' @examples
#' k <- 1:7
#' nu1 <- 8; nu2 <- 5
#' f.prob(nu1, nu2, ub=k)
#'
#' f.prob(nu1, nu2, lb=0:3, ub=7:10)
#'
#' @rdname f.prob
#' @export
f.prob <- function(nu1, nu2, ub, lb=0, prt=TRUE, dig=4, ws=c(7,4), ...) {
  # Check Input
    if (missing(nu1)) stop("Input the degrees of freedom of numerator.")
    if (missing(nu2)) stop("Input the degrees of freedom of denominator.")
    if (missing(ub)) stop("Input ub to find F(ub).")
    if (any(ub-lb<0)) stop("ub should be greater than lb.")
    nub <- length(ub)
  # Prevent weird plots
    if (length(nu1)>1 || length(nu2)>1) ws <- "n"

  # Print the cumulative probability
    prx <- pf(ub, nu1, nu2) - pf(lb, nu1, nu2)
    ub2 <- round(ub, 3)
    for (k in 1:nub) if (abs(ub2[k]-ub[k]) > 1e-7) 
                        ub2[k] <- MASS::fractions(ub2[k])
    if (all(lb==0)) {       
        names(prx) <- paste0("F(", ub2, ")")
    } else {
        lb2 <- round(lb, 3)
        for (k in 1:nub) if (abs(lb2[k]-lb[k]) > 1e-7) 
                            lb2[k] <- MASS::fractions(lb2[k])
        names(prx) <- paste0("P(", lb2, ":", ub2, ")")
    }
    if (prt) print(round(prx, dig))

  # Plot ----------------------------------
  if (is.numeric(ws)) {
  # Set the title and axis
    if (all(lb==0)) {
        mt <- bquote("Cumulative Probabilities" ~ F(x) == P(F[.(nu1)~ ","~ .(nu2)] < x))
    } else {
        mt <- bquote("Interval Probabilities of" ~ F[.(nu1)~ ","~ .(nu2)] )
    }

    xlim <- c(0, max(ub))
    lwd <- 2
    col <- "red"
    yl <- "f(x)"
    xl <- "x"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }
    x1 <- min(xlim[1], lb)
    x2 <- max(xlim[2], ub)
    xmin <- x1 - 0.1*(x2-x1)
    xmax <- x2 + 0.1*(x2-x1)
    xa <- seq(x1, x2, length=100)
    pdf <- df(xa, nu1, nu2)
    ymax <- max(pdf)
    ymin <- -0.05*ymax
    yp <- 0.9*ymax*(nub:1)/(nub+1)

  # Plot the interval probability
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

    plot(xa, pdf, type ="n", main=mt,
        ylim=c(ymin, ymax), xlim=c(xmin, xmax), ylab=yl, xlab=xl)
    abline(h=0, col="green4")
    lines(xa, pdf, type="l", lwd=lwd, col=col)

  # Display specific cumulative probabilities
    rpr <- round(prx, dig)
    abline(v=0, col=grey(0.5), lty=3)
    segments(ub, 0, ub, pmax(yp,df(ub,nu1,nu2)), lty=2, col=4)
    if (all(lb==0)) {
        arrows(x1, yp, ub, yp, length=0.07, code=2, col="blue")
        text(x1, yp, paste0("F(", ub2, ")"), pos=2, cex=cex)
        text(ub, yp, format(rpr, nsmall=dig), 
            pos=4, offset=0.2, cex=cex)
        text(ub, 0, round(ub, dig), pos=1, offset=0.2, col="blue", cex=cex)
    } else {
        segments(lb, 0, lb, pmax(yp,df(ub,nu1,nu2)), lty=2, col=4)
        arrows(lb, yp, ub, yp, length=0.07, code=3, col="blue")
        text(x1, yp, paste0("P(", lb2, ":", ub2, ")"), pos=2, cex=cex)
        text((lb+ub)/2, yp, format(rpr, nsmall=dig), 
            pos=3, offset=0.1, cex=cex)
        ulb <- sort(unique(c(lb, ub)))
        text(ulb, 0, round(ulb, dig), pos=1, offset=0.2, col="blue", cex=cex)
    }

    if (nu2>2) {
        Ex <- nu2/(nu2-2)
        yp2 <- 0.9*ymax
        abline(v=c(0, Ex), col=grey(0.5), lty=3)
        text(x1, yp2, paste0("F(mu)"), pos=2, cex=cex, col="blue")
        arrows(x1, yp2, Ex, yp2, length=0.07, code=2, col="blue")
        text(Ex, yp2, format(pf(Ex,nu1,nu2), digits=dig, nsmall=dig), 
            pos=4, offset=0.2, cex=cex, col="blue")
    }
  }
    invisible(prx)
}

# [8-12] Quantile Plot of the F-distribution
#' @title Quantile Plot of the F-distribution
#' @description Quantile Plot of the F-distribution.
#' @param nu1 Degree of freedom of the numerator.
#' @param nu2 Degree of freedom of the denominator.
#' @param pv Probability values to find the quantiles.
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Grapic window size, Default: c(7,4).
#' @param PDF Logical: plot the PDF? (o.w. CDF) Default: TRUE.
#' @param ... Other graphic parameters.
#' @return Quantiles.
#' @examples
#' nu1 <- 8; nu2 <- 5
#' pv <- c(0.005, 0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99, 0.995)
#' f.quant(nu1, nu2, pv)
#' @rdname f.quant
#' @export
f.quant <- function(nu1, nu2, pv, prt=TRUE, dig=4, ws=c(7,4), PDF=TRUE, ...) {
  # Check Input
    if (missing(nu1)) stop("Input the degrees of freedom of numerator.")
    if (missing(nu2)) stop("Input the degrees of freedom of denominator.")
    if (missing(pv)) stop("Input probability values for the quantiles.")

  # Find Quantiles of the F-distribution
    tab <- cv <- qf(pv, nu1, nu2)
    names(tab) <- pv
    if (prt) print(round(tab, dig))

  # Set the title and axis
    mt <- bquote("Quantiles "~F[p~";("~.(nu1)~","~ .(nu2)~")"] )
    pmax <- max(pv, 0.99)
    xlim <- c(0, qf(pmax, nu1, nu2)*1.1)
    lwd <- 2
    col <- "red"
    yl <- "f(x)"
    xl <- "x"
    cex <- 0.8

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        ndots <- names(dots)
        if ("main" %in% ndots) mt <- dots$main
        if ("col" %in% ndots) col <- dots$col
        if ("ylab" %in% ndots) yl <- dots$ylab
        if ("xlab" %in% ndots) xl <- dots$xlab
        if ("xlim" %in% ndots) xlim <- dots$xlim
        if ("lwd" %in% ndots) lwd <- dots$lwd
        if ("cex" %in% ndots) cex <- dots$cex
    }

  # Set axis
    xa <- seq(xlim[1], xlim[2], length=100)
    xmin <- xlim[1] - 0.1*(xlim[2]-xlim[1])
    pdf <- df(xa, nu1, nu2)
    ymax <- max(pdf)
    npv <- length(pv)
    y1 <- 0.05*ymax

 # Plot the Quantiles
    win.graph(ws[1], ws[2])
    par(mar=c(4,4,3,2))

  # PDF Type
    if (PDF) {
        plot(xa, pdf, type ="n", ylim=c(-y1, ymax), xlim=c(xmin, xlim[2]),
            ylab=yl, xlab=xl, main=mt)
        abline(h=0, col="green4")
        lines(xa, pdf, type="l", lwd=lwd, col=col)

      # Display major quantiles
        rcv <- round(cv, dig)
        abline(v=xlim[1], col=grey(0.5), lty=3)
        yp <- 0.9*ymax*(npv:1)/(npv+1)
        segments(cv, 0, cv, yp, lty=2, col=4)
        fpv <- format(pv, nsmall=3)
        arrows(xlim[1], yp, cv, yp, length=0.07, code=2, col="blue")
        text(0, yp, paste0("p=", fpv), pos=2, offset=0.3, col="blue", cex=cex)
        text(cv, yp, format(rcv, nsmall=dig), pos=4, offset=0.2, cex=cex)
  # CDF Type
    } else {
        cdf <- pf(xa, nu1, nu2)
        plot(xa, cdf, type ="l", lwd=lwd, col=col, 
            xlim=c(xmin, xlim[2]), ylim=c(-0.05,1),
            ylab="F(x)", xlab="x", main=mt)
        abline(h=0, lty=1, col="green4")

      # Display quantiles
        ymax <- max(cdf)
        abline(v=xlim[1], col=grey(0.5), lty=3)
        yp <- ymax*(1:npv)/(npv+1)
        segments(cv, 0, cv, pmax(yp, pv), lty=2, col=4)
        fpv <- format(pv, nsmall=3)
        arrows(xlim[1], yp, cv, yp, length=0.07, code=2, col="blue")
        text(0, yp, paste0("p=", fpv), pos=2, offset=0.3, col="blue", cex=cex)
        text(cv, yp, round(cv, dig), pos=4, offset=0.2, cex=cex)
    }
    if (nu2>2) {
        Ex <- nu2/(nu2-2)
        segments(Ex, 0, Ex, 1.2, col=grey(0.5), lty=3)
        text(Ex, 0, "E(X)", pos=1, offset=0.2, cex=cex, col="blue")
    }
    invisible(tab)
}

# [OLD] ------------------------------------------------------
# [8-9] Simulation of the F-distribution
#' @title Simulation of the F-distribution
#' @description Simulation of the F-distribution
#' @param nu1 Numerator degree of freedom, Default: 5
#' @param nu2 Denominator degree of freedom, Default: 5
#' @param N Number of random values, Default: 10000
#' @param ng Number of classes in histogram, Default: 250
#' @param seed Seed value for random number generator, Default: 9857
#' @param xp Vector of x-axis values, Default: 1:9
#' @param dig Number of digits below the decimal point, Default: 4
#' @return None.
#' @examples
#' fdist.sim(nu1=8, nu2=5)
#'
#' fdist.sim(50, 50, ng=100, xp=1:5/2)
#' @rdname fdist.sim
#' @export
fdist.sim <- function(nu1=5, nu2=5, N=10000, ng=250, seed=9857, xp=1:9, dig=4) {
    # Generate random numbers
    set.seed(seed)
    dat1 <- rchisq(N, nu1)
    dat2 <- rchisq(N, nu2)
    fs1 <- (dat1/nu1)/(dat2/nu2)

    # Define the F-distribution function
    fd1 <- function(x) df(x, nu1, nu2)
    xmax <- max(xp, qf(0.99, nu1, nu2))

    # Expected value and standard deviation
    Ex1 <- round(mean(fs1), dig)
    Dx1 <- round(sd(fs1), dig)
    Ex2 <- ifelse(nu2>2, round(nu2/(nu2-2), dig), Inf)
    Dx2 <- ifelse(nu2>4, 
        round(sqrt(2*nu2^2*(nu1+nu2-2)/nu1/(nu2-2)^2/(nu2-4)), dig),
        ifelse(nu2>2, Inf, NA) )

    # Plot histogram
    win.graph(7, 5)
    hist(fs1, breaks=ng, prob=T,
        xlim=c(0,xmax), col="yellow",
        main=bquote("("~chi[.(nu1)]^2~"/"~.(nu1)~") / ("~chi[.(nu2)]^2~
            "/"~.(nu2)~")  ~  F("~.(nu1)~","~.(nu2)~")" ), ylab="f(x)", xlab="x")
    curve(fd1, 0, xmax, lwd=2, col="red", add=T)
    legend("topright", c("Para.  Exact   Simul.",
        paste("E(X) ", Ex2, Ex1), paste("D(X) ", Dx2, Dx1)),
        text.col=c("black","blue","blue") )

    # Compare the cumulative probabilities F(1), F(2), ...
    Theory <- pf(xp, nu1, nu2)
    Simula <- sapply(xp, function(x) sum(fs1<x))/N
    cdf <- rbind(Theory, Simula)
    colnames(cdf) <- paste0("F(", xp, ")")
    print(round(cdf, dig))
}