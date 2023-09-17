# [Ch-7 Functions] ----------------------------------------------------------------------------------
# [Ch-7 Function Manual] -----------------------------------------

#' @title Manual for Ch.7 Functions
#' @description Ch.7 Continuous Random Variables
#' @param fn Function number (0~2), Default: 0
#' @return None
#' @examples
#' ch7.man()
#'
#' @rdname ch7.man
#' @export
ch7.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] cont.dist\t Expectations, Transformation, and PDF for Continuous Dist.\n")
        cat("[2] cont.mpdf\t PDF and CDF Plots for Continuous Distributions\n")
    }
    if (1 %in% fn) {
        cat("[1] Expectations, Transformation, and PDF for Continuous Distribution\n")
        cat("cont.dist(dist, para1, para2, Tr, prt=\"var\", dig=4, ws=\"n\", ...)\n")
        cat("dist\t Name of continuous probability distribution (one of the follows)\n")
        cat("  \t (\"unif\", \"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", \"t\", \"chisq\", \"f\").\n")
        cat("para1\t First parameter vector of the distribution.\n")
        cat("para2\t Second parameter vector of the distribution (optional).\n")
        cat("Tr\t Transformation function in text string.\n")
        cat("prt\t Print option, one of c(\"exp\", \"var\", \"n\"), Default: \"var\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graph window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] PDF and CDF Plots for Continuous Distributions\n")
        cat("cont.mpdf(dist, para1, para2, xlim, ymax, xp1, xp2, np=100, ws, ...)\n")
        cat("[Mandatory Input]------------------------------\n")
        cat("dist\t Name of continuous probability distribution (one of the follows.)\n")
        cat("\t (\"exp\", \"gamma\", \"weibull\", \"beta\", \"norm\", \"t\", \"chisq\", \"f\")\n")
        cat("para1\t First parameter vector of the distribution.\n")
        cat("para2\t Second parameter vector of the distribution (optional).\n")
        cat("[Optional Input]------------------------------\n")
        cat("xlim\t Limit of x-axis for the plot.\n")
        cat("ymax\t Upper limit of y-axis for the plot.\n")
        cat("xp1\t Specific x values for annotations in the PDF plot.")
        cat("xp2\t Specific x values for annotations in the CDF plot.")
        cat("np\t Number of plot points, Default: 100.\n")
        cat("ws\t Graph window size, Default: c(9,5).\n")
        cat("...\t Other graphic parameters.\n")
    }
}

# [7-1]
cdist.exp <- function(distnum, para1, para2, Tr, prt="var", dig=4) {

  # E(X) and V(X) -----------------------
  # [1] Uniform
    if (distnum == 1) {
        Ex <- (para1 + para2)/2
        Vx <- (para2 - para1)^2/12
        cEx <- paste0("E(X) = (", para1, "+", para2, ")/2 = ", round(Ex, dig))
        cVx <- paste0("V(X) = (", para2, "-", para1, ")^2/12 = ", round(Vx, dig))
        cfun <- paste0("dunif(x,", para1, ",", para2, ")")
        xrng <- c(para1, para2)
  # [2] Exponential
    } else if (distnum == 2) {
        Ex <- 1 / para1
        Vx <- 1 / para1^2
        cEx <- paste0("E(X) = 1/", para1, " = ", round(Ex, dig))
        cVx <- paste0("V(X) = 1/", para1, "^2 = ", round(Vx, dig))
        cfun <- paste0("dexp(x,", para1, ")")
        xrng <- c(0, Inf)
  # [3] Gamma (para1=alpha, para2=theta)
    } else if (distnum == 3) {
        Ex <- para1 * para2
        Vx <- para1 * para2^2
        cEx <- paste0("E(X) = \U03B1*\U03B8 = ",
                 para1, "*", para2, " = ", round(Ex, dig))
        cVx <- paste0("V(X) = \U03B1*\U03B8\U00B2 = ",
                 para1, "*", para2, "^2 = ", round(Vx, dig))
        cfun <- paste0("dgamma(x,", para1, ",", 1/para2, ")")
        xrng <- c(0, Inf)
  # [4] Weibull
    } else if (distnum == 4) {
        Ex <- para2 * gamma(1+1/para1)
        Vx <- para2^2 * (gamma(1+2/para1) - gamma(1+1/para1)^2)
        cEx <- paste0("E(X) = ", para2, " * \U0393(1+1/", para2, 
                      ") = ", round(Ex, dig))
        cVx <- paste0("V(X) = ", para2, "^2 * [\U0393(1+2/", para2, 
                      ")-\U0393(1+1/", para2,")^2] = ", round(Vx, dig))
        cfun <- paste0("dweibull(x,", para1, ",", para2, ")")
        xrng <- c(0, Inf)
  # [5] Beta
    } else if (distnum == 5) {
        Ex <- para1/(para1 + para2)
        Vx <- para1*para2/(para1 + para2)^2/(para1 + para2+1)
        cEx <- paste0("E(X) = ", para1, "/(", para2, "+", para2,
                      ") = ", round(Ex, dig))
        cVx <- paste0("V(X) = ", para1, "*", para2, "/[(", para1, "+", para2,
                      ")^2*(", para1, "+", para2, "+1)] = ", round(Vx, dig))
        cfun <- paste0("dbeta(x,", para1, ",", para2, ")")
        xrng <- c(0, 1)
  # [6] Normal
    } else if (distnum == 6) {
        Ex <- para1
        Vx <- para2^2
        cEx <- paste0("E(X) = \U03BC = ", round(Ex, dig))
        cVx <- paste0("V(X) = \U03C3", "^2 = ", para2, "^2 = ", round(Vx, dig))
        cfun <- paste0("dnorm(x,", para1, ",", para2, ")")
        xrng <- c(-Inf, Inf)
  # [7] T-Dist
    } else if (distnum == 7) {
        Ex <- 0
        Vx <- ifelse(para1<=1, NA, ifelse(para1<=2, Inf, para1/(para1-2)))
        cEx <- paste0("E(X) = ", 0)
        cVx <- paste0("V(X) = ", ifelse(para1<=1, "NA", ifelse(para1<=2, "Inf", 
                    paste0(para1,"/(",para1,"-2) = ", round(Vx, dig)) )))
        cfun <- paste0("dt(x,", para1, ")")
        xrng <- c(-Inf, Inf)
  # [8] Chi-square
    } else if (distnum == 8) {
        Ex <- para1
        Vx <- 2*para1
        cEx <- paste0("E(X) = \U03BD =", round(Ex, dig))
        cVx <- paste0("V(X) = 2*\U03BD = ", round(Vx, dig))
        cfun <- paste0("dchisq(x,", para1, ")")
        xrng <- c(0, Inf)
  # [9] F-Dist
    } else if (distnum == 9) {
        Ex <- ifelse(para2 <= 2, NA, para2/(para2-2))
        Vx <- ifelse(para2 <= 4, NA, 2*para2^2*(para1+para2-2)/
                                     (para1*(para2-2)^2*(para2-4)) )
        cEx <- paste0("E(X) = ", ifelse(para2 <= 2, "NA", 
                      paste0(para2,"/(",para2,"-2) = ", round(Ex, dig)) ))
        cVx <- paste0("V(X) = ", ifelse(para2 <= 4, "NA", 
                      paste0("2*",para2,"^2*(",para1,"+",para2,"-2)/[",para1,
                      "*(",para2,"-2)^2*(",para2,"-4)] = ", round(Vx, dig)) ))
        cfun <- paste0("df(x,", para1, ",", para2, ")")
        xrng <- c(0, Inf)
    }
    if (prt %in% c("exp", "var")) cat(cEx, "\n")
    if (prt == "var") cat(cVx, "\n")

  # Transformation
    Ey <- Vy <- NA
    if (!missing(Tr)) {
        Tr <- tolower(Tr)
        if (!grepl("x", Tr)) stop("Input Tr as a string function of x.")
        cEt <- paste0("(", Tr, ")*", cfun)
        ETf <- str2fn(cEt, "x")
        Ey <- integrate(ETf, xrng[1], xrng[2])[[1]]
        cEt2 <- paste0("(", Tr, ")^2*", cfun)
        ET2f <- str2fn(cEt2, "x")
        Ey2 <- integrate(ET2f, xrng[1], xrng[2])[[1]]
        Vy <- Ey2 - Ey^2

        TR <- toupper(Tr)
        pint <- paste0("\U222B_[", xrng[1], ":", xrng[2], "] {")
        if (prt %in% c("exp", "var")) {
            cat(paste0("E(",TR,") = ", pint, cEt, "}dx"),
                "=", round(Ey, dig), "\n")
        }
        if (prt == "var") {
            cat(paste0("E((",TR,")\U00B2) = ", pint, cEt2,
                "}dx"), "=", round(Ey2, dig), "\n")
            cat(paste0("Var(",TR,") = ", round(Ey2,dig), " - ", 
                round(abs(Ey),dig),"\U00B2 = ", round(Vy,dig)), "\n")
        }
    }
  # Return ---------------
    return(list(cfun=cfun, xrng=xrng, Ex=Ex, Vx=Vx, Ey=Ey, Vy=Vy))
}

# [7-1]
# Expectations, Transformation, and PDF Plots for Continuous Distributions
#' @title Expectations, Transformation, and PDF Plots for Continuous Distributions
#' @description Expectation, Variance, Transformation, and PDF Plots for Continuous Distributions.
#' @param dist Name of continuous probability distribution (one of the follows)
#'             ("unif", "exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f").
#' @param para1 First parameter vector of the distribution.
#' @param para2 Second parameter vector of the distribution (optional).
#' @param Tr Transformation function in text string.
#' @param prt Print option, one of c("exp", "var", "n"), Default: "var".
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graph window size, Default: "n".
#' @param ... Other graphic parameters.
#'
#' @return list(fun, Ex, Vx, Tfun, Ey, Vy)
#' @examples
#' cont.dist("unif", 0, 1, Tr="x^2", prt="var", ws=c(9,4))
#'
#' cont.dist("exp", 5, Tr="2*x+4", prt="var", ws=c(9,4))
#'
#' cont.dist("gamma", 2, 5, Tr="2*(x-5)", prt="var", ws=c(9,4))
#'
#' cont.dist("weib", 2, 5, Tr="x^2", prt="var", ws=c(9,4))
#'
#' cont.dist("beta", 2, 5, Tr="2+4*x", prt="var", ws=c(9,4))
#'
#' @rdname cont.mpdf
#' @export
cont.dist <- function(dist, para1, para2, Tr, prt="var", dig=4, ws="n", ...) {

  # Check input
  # Probability distribution names
    dlist <- c("unif", "exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
    dlist2 <- c("uniform", "exponential", "gamma", "weibull", "beta", "normal", 
                "tdist", "chisquare", "fdist")
    if (missing(dist)) {
        stop("Input the name of a distribution ...")
    }
    if (missing(para1)) stop("Input the first parameter.")

  # Check whether the distribution name is in the list
    dist <- tolower(dist)
    dist <- gsub("-", "", dist)
    dist <- gsub(" ", "", dist)
    distnum <- which(dlist %in% dist)

  # Check also the extended list
    if (length(distnum)==0) distnum <- grep(dist ,dlist2)
    if (length(distnum)==0) {
        stop("Input one of the distribution names below ...")
        cat(paste(dlist, collapse=", "), "\n")
    }

  # Correct the distribution name, if necessary
    if (!(dist %in% dlist)) dist <- dlist[distnum]

  # Find E(X) and Var(X)
    if (!missing(Tr)) {
        TR <- toupper(Tr)
        if (!missing(para2)) {
            res <- cdist.exp(distnum, para1, para2, Tr, prt=prt, dig=dig)
        } else {
            res <- cdist.exp(distnum, para1, Tr=Tr, prt=prt, dig=dig)
        }
    } else {
        if (!missing(para2)) {
            res <- cdist.exp(distnum, para1, para2, prt=prt, dig=dig)
        } else {
            res <- cdist.exp(distnum, para1, prt=prt, dig=dig)
        }
    }

    Ex <- res$Ex
    Vx <- res$Vx
    Dx <- sqrt(Vx)
    Ey <- res$Ey
    Vy <- res$Vy
    Dy <- sqrt(Vy)

  # PDF and TR-PDF
    cfun <- res$cfun
    xrng <- res$xrng
    fun <- str2fn(cfun, "x")
    Tfun <- NA
    if (!missing(Tr)) {
        Tres <- Tr.pdf(cfun, xrng[1], xrng[2], "X","x",Tr,"y")
        Tfun <- Tres$fun
        Trng <- Tres$rng
    }
  # Plot
    if (is.numeric(ws)) {
      # Plot title
        para.p <- ifelse(missing(para2), paste0("(", para1, ")"),
                  paste0("(", para1, ",", para2, ")"))
        dname <- paste0("X~", c("U", "Exp", "G", "W", "Beta", 
                "Norm", "T", "Chis", "F"), para.p)
        distname <- dname[distnum]
        mt1 <- paste0("PDF of ", distname)
        if (!missing(Tr)) mt2 <- paste0("PDF of Y=", TR)

      # Set graphic window
        win.graph(ws[1], ws[2])
        if (!missing(Tr)) par(mfrow=c(1,2))
        par(mar=c(4,4,4,2))
      # Set arguments
        xlim <- xrng
        if (xlim[1]==-Inf) xlim[1] <- Ex - 4*Dx
        if (xlim[2]== Inf) xlim[2] <- Ex + 4*Dx
        xlim[1] <- xlim[1]-0.1*(xlim[2]-xlim[1])
        xlim[2] <- xlim[2]+0.1*(xlim[2]-xlim[1])

        lwd <- 2
        xlab <- c("x", "y")
        ylab <- c("f(x)", "f(y)")
        pos <- 3
        cols <- rep("red", 2)
        col.text <- col.seg <- col.arr <- "blue"
        lty.seg <- 2
        cex.leg <- 1
        pos1 <- pos2 <- pos.leg <- "topright"
        bg.leg <- NA

        dots <- list(...)
        pars <- names(dots)

        if (length(dots) > 0) {
            if ("xlim" %in% pars) xlim <- dots$xlim

            if ("main" %in% pars) {
                main <- dots$main
                if (length(main)==1) main <- rep(main, 2)
                mt1 <- main[1]
                mt2 <- main[2]
            }
            if ("lwd" %in% pars) lwd <- dots$lwd
            if ("col" %in% pars) {
                cols <- dots$col
                if (length(cols)==1) cols <- rep(cols, 2)
            }
            if ("xlab" %in% pars) {
                xlab <- dots$xlab
                if (length(xlab)==1) xlab <- rep(xlab, 2)
            }
            if ("ylab" %in% pars) {
                ylab <- dots$ylab
                if (length(ylab)==1) ylab <- rep(ylab, 2)
            }
            if ("cex" %in% pars) cex <- dots$cex
            if ("pos" %in% pars) pos <- dots$pos
            if ("col.text" %in% pars) col.text <- dots$col.text
            if ("col.seg" %in% pars) col.seg <- dots$col.seg
            if ("col.arr" %in% pars) col.arr <- dots$col.arr
            if ("lty" %in% pars) lty.seg <- dots$lty
            if ("cex.leg" %in% pars) cex.leg <- dots$cex.leg
            if ("pos.leg" %in% pars) {
                pos.leg <- dots$pos.leg
                if (length(pos.leg)==1) {
                    pos1 <- pos2 <- pos.leg
                } else {
                    pos1 <- pos.leg[1]
                    pos2 <- pos.leg[2]
                }
            }
            if ("bg" %in% pars) bg.leg <- dots$bg
        }

      # Plot the probability density function
        xa <- seq(xlim[1], xlim[2], length=200)
        pdf <- fun(xa)
        ymax <- max(pdf)
        if (ymax == Inf) ymax <- 10
        if ("ymax" %in% pars) ymax <- dots$ymax
        plot(xa, pdf, type="l", main=mt1, lwd=lwd, col=cols[1], 
            ylab=ylab[1], xlab=xlab[1], ylim=c(-ymax*0.05, ymax*1.1))
        grid(col = 3)

      # Display the expected value
        ym <- 0.5*ymax
        segments(Ex, 0, Ex, ym, lty=lty.seg, col=col.seg)
        text(Ex, ym, paste0("E(X)"), pos=pos, col=col.text)

      # Display the standard deviation
        x1 <- Ex - Dx
        x2 <- Ex + Dx
        arrows(Ex, ym, x2, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)
        arrows(Ex, ym, x1, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)

      # Display legend
        legend(pos1, cex=cex.leg, c(paste0("E(X) = ",round(Ex,dig)),
            paste0("D(X) = ", round(Dx,dig))), bg=bg.leg)

      # Plot the TR probability density function
        if (!missing(Tr)) {
            Trf <- str2fn(Tr, "x")
            xlim <- Trf(xlim)
            if (xlim[1]==0) xlim[1] <- xlim[1]-0.1*(xlim[2]-xlim[1])
            xa <- seq(xlim[1], xlim[2], length=200)
            Tpdf <- Tfun(xa)
            ymax2 <- max(Tpdf)
            if (ymax2 == Inf) ymax2 <- ymax
            plot(xa, Tpdf, type="l", main=mt2, lwd=lwd, col=cols[2], 
                ylab=ylab[2], xlab=xlab[2], ylim=c(-ymax2*0.05, ymax2*1.1))
            grid(col = 3)

          # Display the expected value
            ym <- 0.5*ymax2
            segments(Ey, 0, Ey, ym, lty=lty.seg, col=col.seg)
            text(Ey, ym, paste0("E(Y)"), pos=pos, col=col.text)

          # Display the standard deviation
            x1 <- Ey - Dy
            x2 <- Ey + Dy
            arrows(Ey, ym, x2, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)
            arrows(Ey, ym, x1, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)

          # Display legend
            legend(pos2, cex=cex.leg, c(paste0("E(Y) = ",round(Ey,dig)),
                paste0("D(Y) = ", round(Dy,dig))), bg=bg.leg)
        }
    }
    invisible(list(fun=fun, Ex=Ex, Vx=Vx, Tfun=Tfun, Ey=Ey, Vy=Vy))
}


# [7-2] Probability Density Function and CDF for Continuous Random Variables
# Get the pdf and the CDF vectors of single x-vector
getdf <- function(dist, xa, para, para2) {
    np <- length(xa)
    N <- max(length(para), length(para2))

    # Probability distribution function name
    dpdf <- paste0("d", dist)
    dcdf <- paste0("p", dist)
    pdf <- cdf <- matrix(NA, nrow=np, ncol=N)

    # Call the pdf by assigning the parameter
    if (dist %in% c("exp", "t", "chisq")) {
        for (k in 1:N) {
            pdf[, k] <- do.call(dpdf, list(xa, para[k]))
            cdf[, k] <- do.call(dcdf, list(xa, para[k]))
        }
    } else if (dist == "gamma") {
        for (k in 1:N) {
            pdf[, k] <- do.call(dpdf, list(xa, para[k], 1/para2[k]))
            cdf[, k] <- do.call(dcdf, list(xa, para[k], 1/para2[k]))
        }
    } else {
        for (k in 1:N) {
            pdf[, k] <- do.call(dpdf, list(xa, para[k], para2[k]))
            cdf[, k] <- do.call(dcdf, list(xa, para[k], para2[k]))
        }
    }
    # Return the pdf and the CDF
    invisible(list(pdf=pdf, cdf=cdf))
}
# Get the pdf and the CDF vectors of multiple x-vector
getdf2 <- function(dist, xa, para, para2) {
    np <- length(xa)
    N <- max(length(para), length(para2))
    if (N != np) stop("Number of x-vectors must be same as the number of parameters!")

    # Probability distribution function name
    dpdf <- paste0("d", dist)
    dcdf <- paste0("p", dist)
    pdf <- cdf <- rep(NA, N)

    # Call the pdf by assigning the parameter
    if (dist %in% c("exp", "t", "chisq")) {
        for (k in 1:N) {
            pdf[k] <- do.call(dpdf, list(xa[k], para[k]))
            cdf[k] <- do.call(dcdf, list(xa[k], para[k]))
        }
    } else if (dist == "gamma") {
        for (k in 1:N) {
            pdf[k] <- do.call(dpdf, list(xa[k], para[k], 1/para2[k]))
            cdf[k] <- do.call(dcdf, list(xa[k], para[k], 1/para2[k]))
        }
    } else {
        for (k in 1:N) {
            pdf[k] <- do.call(dpdf, list(xa[k], para[k], para2[k]))
            cdf[k] <- do.call(dcdf, list(xa[k], para[k], para2[k]))
        }
    }

    # Return the pdf and the CDF
    invisible(list(pdf=pdf, cdf=cdf))
}
# PDF and CDF Plots for Continuous Distributions
#' @title PDF and CDF Plots for Continuous Distributions
#' @description PDF and CDF Plots for Continuous Distributions.
#' @param dist Name of continuous probability distribution (one of the follows)
#'             ("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f").
#' @param para1 First parameter vector of the distribution.
#' @param para2 Second parameter vector of the distribution (optional).
#' @param xlim Limit of x-axis for the plot.
#' @param ymax Upper limit of y-axis for the plot.
#' @param xp1 Specific x values for annotations in the PDF plot.
#' @param xp2 Specific x values for annotations in the CDF plot.
#' @param np Number of plot points, Default: 100.
#' @param ws Graph window size, Default: c(9,5).
#' @param ... Other graphic parameters.
#'
#' @return None.
#' @examples
#' lamb <- 1:5
#' cont.mpdf("exp", para1=lamb, xlim=3, ymax=5)
#'
#' alp <- c(0.5, 1, 2, 3); rate <- 1
#' cont.mpdf("gamma", alp, rate, xlim=8, ymax=1.2)
#'
#' th <- 1; alp <- c(0.5, 1, 2, 3)
#' cont.mpdf("weibull", alp, th, 5, 1.2)
#'
#' alp <- 10:100/10; th <- 1
#' cont.mpdf("gamma", alp, th, 20)
#' cont.mpdf("weibull", alp, th, 5)
#' @rdname cont.mpdf
#' @export
cont.mpdf <- function(dist, para1, para2, xlim, ymax, xp1, xp2, np=100, ws, ...) {

  # Check input
    if (missing(xlim)) stop("Input xlim, the range of x-axis.")

  # Probability distribution names
    dlist <- c("exp", "gamma", "weibull", "beta", "norm", "t", "chisq", "f")
    dlist2 <- c("exponential", "gamma", "weibull", "beta", "normal", 
                "tdist", "chisquare", "fdist")
    if (missing(dist)) {
        cat(paste(dlist, collapse=", "), "\n")
        stop("Input one of the distribution name above ....")
    }

  # Check whether the distribution name is in the list
    dist <- tolower(dist)
    distnum <- which(dlist %in% dist)

  # Check also the extended list
    if (length(distnum)==0) distnum <- grep(dist ,dlist2)
    if (length(distnum)==0) {
        cat(paste(dlist, collapse=", "), "\n")
        stop("Input one of the distribution name above ....")
    }

  # Correct the distribution name, if necessary
    if (!(dist %in% dlist)) dist <- dlist[distnum]

  #  Graph title
    dname <- paste0(c("Exponential", "Gamma", "Weibull", "Beta", 
                "Normal", "T-", "Chi-square", "F-"), " Dist.")
    distname <- dname[which(dlist %in% dist)]
    mt1 <- paste0("PDF of ", distname)
    mt2 <- paste0("CDF of ", distname)

  # Number of parameters and their names
    if (missing(para1)) stop("Input the distribution parameter ....")
    N <- length(para1)
    pn <- deparse(substitute(para1))
    varypn <- 12

  # Require para2 except the exponential, t, chisquare distribution
    if (missing(para2)) {para2 <- para1
        if (!(dist %in% c("exp", "t", "chisq"))) {
            cat("The second parameter is required for the", dist, "distribution.\n",
                 "It was set to the same as the first parameter...\n") 
        }
    } else { 
        pn2 <- deparse(substitute(para2))
        N2 <- length(para2)
        if (N==1 & N2>1) {
            varypn <- 2
            para1 <- rep(para1, N2)
            pn <- deparse(substitute(para2))
        }
        if (N>1 & N2==1) {
            varypn <- 1
            para2 <- rep(para2, N)
        }
        if (N>1 & N2>1) varypn <- 12
        N <- max(N,N2)
    }

  # Check the parameter names
    # exp(rate=lambda), gamma(shape=alpha, rate=1/theta)
    # weibull(shape=alpha, scale=theta), beta(shape1=alpha, shape2=beta)
    # norm(mean=mu, sd= sigma), t(df=nu), chisq(df=nu), f(df1=nu1, df2=nu2)
    # [Correction]
    if (N<=10) {
        # Legend Labels
        lab <- list()
        if (varypn==1) {
            for (k in 1:N) lab[[k]] <- 
                switch(distnum, bquote(lambda == .(para1[k])),
                bquote(alpha == .(para1[k])), bquote(alpha == .(para1[k])),
                bquote(alpha == .(para1[k])), bquote(mu == .(para1[k])),
                bquote(nu == .(para1[k])), bquote(nu == .(para1[k])), 
                bquote(nu[1] == .(para1[k])))
        } else if (varypn==2) {
            for (k in 1:N) lab[[k]] <- 
                switch(distnum, bquote(lambda == .(para1[k])),
                bquote(theta == .(para2[k])), bquote(theta == .(para2[k])),
                bquote(beta == .(para2[k])), bquote(sigma == .(para2[k])),
                bquote(nu == .(para1[k])), bquote(nu == .(para1[k])), 
                bquote(nu[2] == .(para2[k])))
        } else if (varypn==12) {
            for (k in 1:N) lab[[k]] <- 
                switch(distnum, bquote(lambda == .(para1[k])),
                bquote(alpha == .(para1[k]) ~ theta == .(para2[k])),
                bquote(alpha == .(para1[k]) ~ theta == .(para2[k])),
                bquote(alpha == .(para1[k]) ~ beta == .(para2[k])),
                bquote(mu == .(para1[k]) ~ sigma == .(para2[k])),
                bquote(nu == .(para1[k])), bquote(nu == .(para1[k])),
                bquote(nu[1] == .(para1[k]) ~ nu[2] == .(para2[k])))
        }
        leg <- lab[[1]]
        if (N >= 2) for (i in 2:N) leg <- c(leg, lab[[i]])
    }

  # Get the vector of PDF and CDF
    if (length(xlim) == 1) {
        lo <- 0
        up <- xlim
    } else {
        lo <- xlim[1]
        up <- xlim[2]
    }
    xa <- seq(lo, up, length=np)
    dum <- getdf(dist, xa, para1, para2)
    pdf <- dum$pdf
    cdf <- dum$cdf

  # Set colors
    if (N<=6) {
        dcol <- c("red", "blue", "orange2", "green4", "purple", "cyan2")
    } else {
        dcol <- rainbow(N)
    }

  # Set graphic window
    if (missing(ws)) ws <- c(9, 5)
    win.graph(ws[1], ws[2])
    par(mfrow=c(1,2))

  # Set arguments
    lwd <- 2
    xlab <- c("(a)", "(b)")
    ylab <- c("f(x)", "F(x)")
    pos <- 3
    col.text <- col.seg <- col.arr <- "blue"
    lty.seg <- 2
    cex.leg <- 1
    pos1 <- "topright"
    pos2 <- "bottomright"
    bg.leg <- "white"

    dots <- list(...)

    if (length(dots) > 0) {
        pars <- names(dots)
        if ("main" %in% pars) {
            main <- dots$main
            if (length(main)==1) main <- rep(main, 2)
            mt1 <- main[1]
            mt2 <- main[2]
        }
        if ("lwd" %in% pars) lwd <- dots$lwd
        if ("col" %in% pars) {
            cols <- dots$col
            if (length(cols) < N) {
                 dcol <- rep(cols, ceiling(N/length(cols)))
            } else dcol <- cols
        }
        if ("xlab" %in% pars) {
            xlab <- dots$xlab
            if (length(xlab)==1) xlab <- rep(xlab, 2)
        }
        if ("ylab" %in% pars) {
            ylab <- dots$ylab
            if (length(ylab)==1) ylab <- rep(ylab, 2)
        }
        if ("cex" %in% pars) cex <- dots$cex
        if ("pos" %in% pars) pos <- dots$pos
        if ("col.text" %in% pars) col.text <- dots$col.text
        if ("col.seg" %in% pars) col.seg <- dots$col.seg
        if ("col.arr" %in% pars) col.arr <- dots$col.arr
        if ("lty" %in% pars) lty.seg <- dots$lty
        if ("cex.leg" %in% pars) cex.leg <- dots$cex.leg
        if ("pos.leg" %in% pars) {
            pos.leg <- dots$pos.leg
            if (length(pos.leg)==1) {
                pos1 <- pos2 <- pos.leg
            } else {
                pos1 <- pos.leg[1]
                pos2 <- pos.leg[2]
            }
        }
        if ("bg" %in% pars) bg.leg <- dots$bg
        }

  # Plot the probability density function
    if (missing(ymax)) ymax <- max(pdf)
    plot(xa, pdf[ ,1], type="l", main=mt1,
        lwd=lwd, col=dcol[1], ylab=ylab[1], xlab=xlab[1], ylim=c(0, ymax))
    grid(col = 3)
    if (N >= 2) for (i in 2:N) lines(xa, pdf[ ,i], lwd=lwd, col=dcol[i])

    # [Correction]
    if (N<=10) {
        if (!missing(xp1)) {
            yp1 <- getdf2(dist, xp1, para1, para2)$pdf
            text(xp1, yp1, paste0("(",para1, ",", para2, ")"))
        } else if (N>=2) {
            legend(pos1, sapply(leg, as.expression), lwd=lwd, col=dcol[1:N])
        } else {
            legend(pos1, as.expression(leg), lwd=lwd, col=dcol[1]) 
        }
    }

    # Plot the cumulative distribution function
    plot(xa, cdf[ ,1], type="l", main=mt2,
        lwd=lwd, col=dcol[1], ylim=c(0,1), ylab=ylab[2], xlab=xlab[2])
    grid(col = 3)
    if (N >= 2) for (i in 2:N) lines(xa, cdf[ ,i], lwd=lwd, col=dcol[i])
    # [Correction]
    if (N<=10) {
        if (!missing(xp2)) {
            yp2 <- getdf2(dist, xp2, para1, para2)$cdf
            text(xp2, yp2, paste0("(",para1, ",", para2, ")"))
        } else if (N>=2) {
            legend(pos2, sapply(leg, as.expression), lwd=lwd, col=dcol[1:N])
        } else {
            legend(pos2, as.expression(leg), lwd=lwd, col=dcol[1]) 
        }
    }
}

# [OLD] -------------------------------------------------------
# [7-1] PDF Plot of Continuous Random Variables
#' @title PDF Plot of Continuous Random Variables
#' @description PDF Plot of Continuous Random Variables
#' @param FUN Continuous probability density function
#' @param lo Lower limit of x-axis
#' @param up Upper limit of x-axis
#' @param mt Graph title
#' @param dig Number of digits below the decimal point, Default: 3
#' @param xn Random variable name, Default: 'X'
#' @param prt Print expected values and variances? Default: FALSE
#' @param pos Legend location, Default: 'center'
#' @return list(ev=Ex, std=Dx)
#' @examples
#' fx <- function(x) dnorm(x,10,2)
#' cont.plot(fx, 0, 20, prt=T)
#'
#' fx <- function(x) dgamma(x, 3, scale=5)
#' cont.plot(fx, 0, 40, mt="X ~ Gam(3,5)", prt=T)
#' @rdname cont.plot
#' @export
cont.plot <- function(FUN, lo, up, mt, dig=3, xn="X", prt=FALSE, pos="center") {
    Xn <- toupper(xn)
    if (missing(mt)) mt <- paste0("PDF and Expected Value of ", Xn)

    # Define expected values
    ex <- function(x) x*FUN(x)
    ex2 <- function(x) x^2*FUN(x)
    Ex <- integrate(ex, -Inf, Inf)[[1]]
    Ex2 <- integrate(ex2, -Inf, Inf)[[1]]
    Vx <- Ex2 - Ex^2
    Dx <- sqrt(Vx)

    # Print expected values
     if (prt==TRUE) {
        cat(paste0("E(",Xn,") = ",round(Ex, dig)), "\t ")
        cat(paste0("V(",Xn,") = ",round(Vx, dig)), "\t ")
        cat(paste0("D(",Xn,") = ",round(Dx, dig)), "\n")
     }

    # Plot the pdf and the expected values
    # Set the range of x-axis
    xa <- seq(lo, up, length=200)
    # Plot the probability distribution function f(x)
    plot(xa, FUN(xa), type="l", main=mt, ylim=c(0, max(FUN(xa))*1.1),
        xlab="", ylab=paste0("f(", xn,")"), lwd=2, col="red")
    abline(v=Ex, lty=2, col="blue")
    # Display legend
    legend(pos,
        c(paste0("E(",Xn,")=",round(Ex,dig)),
        paste0("D(",Xn,")=", round(Dx,dig))),
        bg="white")

    invisible(list(ev=Ex, std=Dx))
}