# [Ch-6 Functions] ----------------------------------------------------------------------------------
# [Ch-6 Function Manual] -----------------------------------------

#' @title Manual for Ch.6
#' @description Ch.6 Probability Distributions of Discrete Random Variables
#' @param fn Function number, Default: 0
#' @return None.
#' @examples
#' ch6.man()
#' ch6.man(1:2)
#' @rdname ch6.man
#' @export
ch6.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] disc.dist\t\tMoments and Probabilities of Discrete Distributions\n")
        cat("[2] disc.mexp\t\tExpected Values and Plots of Multiple Discrete PDFs\n")
        cat("[3] multinom.plot \tPlots of Multinomial Distributions\n")
    }

    if (1 %in% fn) {
        cat("[1] Moments and Probabilities of Discrete Distributions\n")
        cat("disc.dist(dist, par1, par2, nd, lb, ub, prt=\"exp\", dig=4, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("dist\t Discrete probability distribution.\n")
        cat("par1\t First parameter of the distribution.\n")
        cat("[Optional Input]--------------------------\n")
        cat("par2\t Second parameter of the distribution (if necessary).\n")
        cat("nd\t Sample size for the hypergeometric distribution.\n")
        cat("lb\t Lower bound for findind P(lb <= X).\n")
        cat("ub\t Upper bound for findind P(X <= ub).\n")
        cat("prt\t Print option: one of c(\"exp\", \"var\", \"n\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Expected Values and Plots of Multiple Discrete PDFs\n")
        cat("disc.mexp(xv, fx, fx2, fx3, prt=\"exp\", dig=4, ws=\"n\", del=0.2, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("xv\t Values of the random variable.\n")
        cat("fx\t List of the first PDFs (2~9).\n")
        cat("[Optional Input]--------------------------\n")
        cat("fx2\t List of the second PDFs (same number of fx).\n")
        cat("fx3\t List of the third PDFs (same number of fx).\n")
        cat("prt\t Print option: one of c(\"exp\", \"var\", \"n\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("del\t Distance for overlapping plots, Default: 0.2.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Multinomial PDF Plots, Expectations, and Probabilities\n")
        cat("multinom.plot(ps, size, lb, ub, prt=\"exp\", dig=4, ws=\"n\", ...)\n")
        cat("require(scatterplot3d)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("ps\t Probability matrix with rows for multiple cases.\n")
        cat("size\t Sample size.\n")
        cat("lb\t Lower bound vector for finding a probability.\n")
        cat("ub\t Upper bound vector for finding a probability.\n")
        cat("prt\t Print option: one of c(\"exp\", \"var\", \"n\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
    }
}

# [6-1] Discrete Distributions
#' @title Expected Values and Probabilities of Discrete Distributions
#' @description Expected Values and Probabilities of Discrete Distributions
#' @param dist Discrete probability distribution.
#' @param par1 First parameter of the distribution.
#' @param par2 Second parameter of the distribution (if necessary).
#' @param nd Sample size for the hypergeometric distribution
#' @param lb Lower bound for findind P(lb <= X).
#' @param ub Upper bound for findind P(X <= ub).
#' @param prt Print option: one of c("exp", "var", "n"), Default: "exp".
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return list(EX=expected value, VX=variance, Pr=probability)
#' @examples
#' # Discrete uniform
#' disc.dist("unif", 20, lb=15, prt="var")
#'
#' # Binomial
#' disc.dist("binom", 30, 0.05, prt="var")
#' disc.dist("binom", 30, 0.05, prt="p", lb=3, ws=c(7,4), xlim=c(0,10))
#'
#' @rdname disc.dist
#' @export
disc.dist <- function(dist, par1, par2, nd, lb, ub, prt="exp", dig=4, ws="n", ...) {
  # Check input
    if (missing(dist)) stop("Input the name of discrete distribution.")

  # Check distribution
    dlist <- c("uniform", "bernoulli", "binomial", "poisson", "hypergeometric", 
             "geometric", "nbinomial", "negativebinomial")
    fdn <- paste0("^", tolower(dist))
    if (any(grepl(fdn, dlist))) {
        dnum <- grep(fdn, dlist)[1]
    } else {
        stop("Input valid name of discrete distribution.")
    }

  # Check the bounds and obtain probability
    if (!missing(lb) || !missing(ub)) {
        prob <- TRUE
        if (missing(lb)) {
            side <- "low"
            cPr <- paste0("Pr(X\U2264", ub, ") =")
        } else if (missing(ub)) {
            side <- "upp"
            cPr <- paste0("Pr(X\U2265", lb, ") =")
        } else {
            side <- "two"
            cPr <- paste0("Pr(", lb, "\U2264", "X\U2264", ub,") =")
        }
    } else {
        prob <- FALSE
        Pr <- NA
    }

  # Check parameters, create the PDF, Find E(X) and Var(X)
    # [1] Discrete Uniform (par1=nd, par2=aa)
    if (dnum == 1) {
        if (missing(par1)) {
            stop("Input par1 for the sample size.")
        } else {
            nd <- par1
            if (missing(par2)) {
                aa <- 0
                xd <- 1:nd
            } else {
                aa <- par2 - 1
                xd <- (aa+1):(nd+aa)
            }
        }
        fun <- function(x) rep(1, length(x))/nd
        Ex <- sum(xd*fun(xd))
        Ex2 <- sum(xd^2*fun(xd))
        Vx <- Ex2 - Ex^2
        aa <- min(xd) - 1
        pEx <- ifelse(aa==0, paste0("E(X) = (",nd,"+1)/2 = ", round(Ex, dig)),
               paste0("E(X) = ", aa,"+(",nd,"+1)/2 =", round(Ex, dig)))
        pVx <- paste0("V(X) = (",nd,"+1)(",nd,"-1)/12 = ", round(Vx, dig))
        title <- paste0("Disc-Unif(",min(xd),", ",max(xd),")")
      # Check the bounds and obtain probability
        if (prob) {
            if (missing(lb)) lb <- min(xd)
            if (missing(ub)) ub <- max(xd)
            pxd <- xd[xd>=lb & xd<=ub]
            Pr <- sum(fun(pxd))
            ufx <- MASS::fractions(1/nd)
            mPr <- paste0(length(pxd),"*(", ufx, ") =")
        }

    # [2] Bernoulli  (par1=p)
    } else if (dnum == 2) {
        xd <- c(0, 1)
        if (missing(par1)) {
            stop("Input par1 (p) for the Bernoulli distribution.")
        }
        title <- paste0("Bernoulli(",par1,")")
        fun <- function(x) dbinom(x, 1, par1)
        Ex <- sum(xd*fun(xd))
        Ex2 <- sum(xd^2*fun(xd))
        Vx <- Ex2 - Ex^2
        pp1 <- ifelse(par1==round(par1, dig), par1, MASS::fractions(par1))
        pEx <- paste0("E(X) = ", round(Ex, dig))
        pVx <- paste0("V(X) = ", pp1, "*(1-",pp1,") = ", round(Vx, dig))
      # Check the bounds and obtain probability
        if (prob) {
            if (missing(lb)) lb <- min(xd)
            if (missing(ub)) ub <- max(xd)
            pxd <- xd[xd>=lb & xd<=ub]
            Pr <- sum(fun(pxd))
            mPr <- paste0("sum(dbinom(", lb,":",ub,", 1, ", pp1, ")) =")
        }

    # [3] Binomial (par1=nd, par2=p)
    } else if (dnum == 3) {
        if (missing(par1)) {
            stop("Input par1 for the sample size.")
        } else {
            nd <- par1
            xd <- 0:nd
        }
        if (missing(par2)) stop("Input par2 for the probability of success.")

        fun <- function(x) dbinom(x, nd, par2)
        Ex <- sum(xd*fun(xd))
        Ex2 <- sum(xd^2*fun(xd))
        Vx <- Ex2 - Ex^2
        pp1 <- ifelse(par2==round(par2, dig), par2, MASS::fractions(par2))
        pEx <- paste0("E(X) = ", nd,"*",pp1, " = ", round(Ex, dig))
        pVx <- paste0("V(X) = ", nd,"*",pp1,"*(1-",pp1,") = ", round(Vx, dig))
        title <- paste0("Binomial(",nd,", ",pp1,")")
      # Check the bounds and obtain probability
        if (prob) {
            if (missing(lb)) lb <- min(xd)
            if (missing(ub)) ub <- max(xd)
            pxd <- xd[xd>=lb & xd<=ub]
            Pr <- sum(fun(pxd))
            mPr <- paste0("sum(dbinom(", lb,":",ub,", ",nd,", ", pp1, ")) =")
        }

    # [4] Poisson (par1=lambda)
    } else if (dnum == 4) {
        if (missing(par1)) stop("Input par1 for the Poisson distribution.")
        if (missing(nd)) nd <- par1*5
        xd <- 0:nd
        title <- paste0("Poisson(",par1,")")
        fun <- function(x) dpois(x, par1)
        Ex <- par1		
        Ex2 <- par1 + par1^2	
        Vx <- par1		
        pp1 <- ifelse(par1==round(par1, dig), par1, MASS::fractions(par1))
        pEx <- paste0("E(X) = \U03BB = ", pp1)
        pVx <- paste0("V(X) = \U03BB = ", pp1)
      # Check the bounds and obtain probability
        if (prob) {
            if (side=="two") {
                mPr <- paste0("sum(dpois(", lb,":",ub,", ", pp1, ")) =")
                pxd <- xd[xd>=lb & xd<=ub]
                Pr <- sum(fun(pxd))
            } else if (side=="low") {
                mPr <- paste0("sum(dpois(0:", ub,", ", pp1, ")) =")
                lb <- 0
                pxd <- xd[xd<=ub]
                Pr <- sum(fun(pxd))
            } else if (side=="upp") {
                mPr <- paste0("1 - sum(dpois(0:", lb-1,", ", pp1, ")) =")
                ub <- max(xd)
                pxd <- xd[xd>=lb]
                Pr <- 1 - sum(fun(0:(lb-1)))
            }
        }

    # [5] Hypergeometric (par1=S, par2=F, nd=n)
    } else if (dnum == 5) {
        if (missing(nd)) {
            stop("Input nd for the sample size.")
        } else {
            xd <- 0:nd
        }
        if (missing(par1)) {
            stop("Input par1 for the hypergeometric distribution.")
        }
        if (missing(par2)) {
            stop("Input par2 for the hypergeometric distribution.")
        }
        # Case (par1=N, par2=p) -----
        if (par2 < 1) {
            NN <- par1
            par1 <- round(NN*par2)
            par2 <- round(NN*(1-par2))
        }
        # Case (par1=N, par2=N*p) -----
        if (par2 >= 1 && par1 > par2) {
            NN <- par1
            par1 <- par2
            par2 <- NN - par1
        }
        fun <- function(x) dhyper(x, par1, par2, nd)
        Ex <- sum(xd*fun(xd))
        Ex2 <- sum(xd^2*fun(xd))
        Vx <- Ex2 - Ex^2
        NN <- par1 + par2
        pp <- par1/NN
        rpp <- ifelse(pp==round(pp, dig), pp, MASS::fractions(pp))
        pEx <- paste0("E(X) = ", nd,"*",rpp, " = ", round(Ex, dig))
        pVx <- paste0("V(X) = ", nd,"*",rpp,"*(1-",rpp,")*(", 
                   NN, "-", nd,")/(",NN, "-1) = ", round(Vx, dig))
        title <- paste0("Hypergeometric(",NN,", ",par1,", ",nd,")")
      # Check the bounds and obtain probability
        if (prob) {
            if (missing(lb)) lb <- min(xd)
            if (missing(ub)) ub <- max(xd)
            pxd <- xd[xd>=lb & xd<=ub]
            Pr <- sum(fun(pxd))
            mPr <- paste0("sum(dhyper(", lb,":",ub,", ",
                         par1,", ", par2, ", ", nd,")) =")
        }

    # [6] Geometric (par1=p)
    } else if (dnum == 6) {
        if (missing(par1)) {
            stop("Input par1 for the geometric distribution.")
        }
        if (missing(nd)) nd <- ceiling(10/par1)
        xd <- 1:nd
        fun <- function(x) dgeom(x-1, par1)
        Ex <- 1/par1
        Ex2 <- (2-par1)/par1^2
        Vx <- Ex2 - Ex^2
        pp1 <- par1
        if (nchar(pp1 > 6)) pp1 <- as.character(MASS::fractions(par1))
        pEx <- paste0("E(X) = 1/(", pp1, ") = ", round(Ex, dig))
        pVx <- paste0("V(X) = (1-",pp1,")/(",pp1,")^2 = ", round(Vx, dig))
        title <- paste0("Geometric(",pp1,")")
      # Check the bounds and obtain probability
        if (prob) {
            if (side=="two") {
                mPr <- paste0("sum(dgeom(", lb-1,":",ub-1,", ", pp1, ")) =")
                pxd <- xd[xd>=lb & xd<=ub]
                Pr <- sum(fun(pxd))
            } else if (side=="low") {
                mPr <- paste0("sum(dgeom(0:", ub-1,", ", pp1, ")) =")
                lb <- 1
                pxd <- xd[xd<=ub]
                Pr <- sum(fun(pxd))
            } else if (side=="upp") {
                mPr <- paste0("1 - pgeom(", lb-2,", ", pp1, ")) =")
                ub <- max(xd)
                pxd <- xd[xd>=lb]
                Pr <- 1 - sum(fun(0:(lb-1)))
            }
        }

    # [7] Negative Binomial (par1=r, par2=p)
    } else if (dnum == 7) {
        if (missing(par1) || missing(par2)) {
            stop("Input par1 and par2 for the negativebinomial distribution.")
        }
        if (missing(nd)) nd <- ceiling(20*par1/par2)
        xd <- par1:nd
        # Case (par1=p, par2=r) -----
        if (par1 < 1) {
            temp <- par1
            par1 <- par2
            par2 <- temp
        }

        fun <- function(x) dnbinom(x-par1, par1, par2)
        Ex <- par1/par2
        Ex2 <- Ex^2 + par1*(1-par2)/par2^2
        Vx <- par1*(1-par2)/par2^2	# Ex2 - Ex^2
        pp2 <- par2
        if (nchar(pp2 > 6)) pp2 <- as.character(MASS::fractions(par2))
        pEx <- paste0("E(X) = ",par1,"/(", pp2, ") = ", round(Ex, dig))
        pVx <- paste0("V(X) = ",par1,"*(1-",pp2,")/(",pp2,")^2 = ", round(Vx, dig))
        title <- paste0("Negative-Binomial(",par1,",",pp2,")")
      # Check the bounds and obtain probability
        if (prob) {
            if (side=="two") {
                mPr <- paste0("sum(dnbinom(", lb-par1,":",ub-par1,
                              ", ", par1, ", ", pp2, ")) =")
                pxd <- xd[xd>=lb & xd<=ub]
                Pr <- sum(fun(pxd))
            } else if (side=="low") {
                mPr <- paste0("sum(dnbinom(0:", ub-par1,
                              ", ", par1, ", ", pp2, ")) =")
                lb <- par1
                pxd <- xd[xd<=ub]
                Pr <- sum(fun(pxd))
            } else if (side=="upp") {
                mPr <- paste0("1 - pnbinom(", lb-par1-1,", ",
                              ", ", par1, ", ", pp2, ")) =")
                ub <- max(xd)
                pxd <- xd[xd>=lb]
                Pr <- 1 - sum(fun(0:(lb-1)))
            }
        } else {Pr <- NA}
    }

  # Print output
    if (prt %in% c("exp", "var")) cat(pEx, "\n")
    if (prt == "var") cat(pVx, "\n")
    if (prob) cat(cPr, mPr, round(Pr, dig), "\n")

  # Plot PDF f(x)
    if (is.numeric(ws)) {
        xp <- fun(xd)
        x1 <- min(xd)
        x2 <- max(xd)
        xr <- x2 - x1
        mt <- paste("PDF of", title)

        arg1 <- list(type="h", main=mt, pch=19, lwd=5, ylim=c(0, max(xp)*1.1),
                   xlim=c(x1-0.1*xr, x2+0.1*xr), col="orange", xlab="x", ylab="f(x)")
        dots <- list(...)
        dd1 <- c("type", "main", "pch", "lwd", "ylim", "xlim", "col", "xlab", "ylab")

        cex <- 0.8
        pos <- 3
        col.text <- col.seg <- col.arr <- "black"
        lty.seg <- 2
        pos.leg <- "topright"

        if (length(dots) > 0) {
            if ("cex" %in% names(dots)) cex <- dots$cex
            if ("pos" %in% names(dots)) pos <- dots$pos
            if ("col.text" %in% names(dots)) col.text <- dots$col.text
            if ("col.seg" %in% names(dots)) col.seg <- dots$col.seg
            if ("col.arr" %in% names(dots)) col.arr <- dots$col.arr
            if ("lty.seg" %in% names(dots)) lty.seg <- dots$lty.seg
            if ("pos.leg" %in% names(dots)) pos.leg <- dots$pos.leg

            ck1 <- which(dd1 %in% names(dots))
            for (kk in ck1) arg1[[kk]] <- 
                            dots[[which(names(dots) %in% dd1[kk])]]
            dots <- dots[-which(names(dots) %in% dd1)]
        }

        win.graph(ws[1], ws[2])
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))
        xylist <- list(x=xd, y=xp)
        arg2 <- c(arg1, dots)
        do.call(plot, c(xylist, arg2))
        grid(col="green")
        dcol <- ifelse(arg2$col=="red", "red4", "red")
        if (prob) lines(pxd, fun(pxd), type=arg2$type, lwd=arg2$lwd, col=dcol)
      # Display the expected value
        ym <- 0.5*max(xp)
        segments(Ex, 0, Ex, ym, lty=lty.seg, col=col.seg)
        anotate <- paste0("E(X)")
        text(Ex, ym, anotate, pos=pos, col=1)

      # Display the standard deviation
        Dx <- sqrt(Vx)
        x1 <- Ex - Dx
        x2 <- Ex + Dx
        arrows(Ex, ym, x2, ym, length=0.1, angle=90, lty=2, col=col.arr)
        arrows(Ex, ym, x1, ym, length=0.1, angle=90, lty=2, col=col.arr)

      # Display text
        if (prt=="p") text(xd, fun(xd), round(fun(xd),dig),
                               cex=cex, col="blue", pos=pos)

      # Display legend
        leg.cex <- 1
        legend(pos.leg, cex=leg.cex, c(paste0("E(X) = ",round(Ex,dig)),
            paste0("D(X) = ", round(Dx,dig))), bg="white")
    }
    invisible(list(EX=Ex, VX=Vx, Pr=Pr))
}

# [6-2] Expected Values and Plots of Multiple Discrete PDFs
#' @title Expected Values and Plots of Multiple Discrete PDFs
#' @description Expected Values and Plots of Multiple Discrete PDFs.
#' @param xv Values of the random variable.
#' @param fx List of the first PDFs (2~9).
#' @param fx2 List of the second PDFs (same number of fx).
#' @param fx3 List of the third PDFs (same number of fx).
#' @param prt Print option: one of c("var", "exp", "n"), Default: "var".
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param del Distance for overlapping plots, Default: 0.2.
#' @param ... Other graphic parameters.
#' @return list(Ex, Vx, Dx)
#' @examples
#' # Binomial distributions
#' n <- 10;  p <- 1:9/10; x <- 0:n
#' fx1 <- fx2 <- list()
#' for (i in 1:9) fx1[[i]] <- dbinom(x, n, p[i])
#' mt1 <- paste0("B(10,", p, ")")
#' disc.mexp(x, fx1, prt="n", ws=c(8,6), main=mt1, lwd=5)
#' 
#' # Binomial vs. Hypergeometric
#' N <- 50;  S <- 1:9*5; n <- 10; x <- 0:n
#' for (i in 1:9) fx2[[i]] <- dhyper(x, S[i], N-S[i], n)
#' mt12 <- paste0("HG(10,50,", S,"):B(10,", p,")")
#' disc.mexp(x, fx2, fx1, ws=c(8,6), main=mt12)
#' @rdname disc.mexp
#' @export
disc.mexp <- function(xv, fx, fx2, fx3, prt="var", dig=4, ws="n", del=0.2, ...) {
  # Number of probability distributions
    ng <- length(fx)
    if (ng>9) stop("Number of probability distribution must be in 2~9!")
    Add2 <- ifelse (missing(fx2), FALSE, TRUE)
    Add3 <- ifelse (missing(fx3), FALSE, TRUE)
  # Random variable name
    Xn <- toupper(deparse(substitute(xv)))

  # Calculate expected values, variances, and standard deviations
    Ex <- Exs <- Vx <- Dx <- list()
    for (k in 1:ng) {
        Ex[[k]] <- sum(xv*fx[[k]])
        Exs[[k]] <- sum(xv^2*fx[[k]])
        Vx[[k]] <- Exs[[k]] - Ex[[k]]^2
        Dx[[k]] <- sqrt(Vx[[k]])
    }
    if (Add2) prt <- "n"
    if (Add3) prt <- "n"

  # Print expected values, variances, and standard deviations
    Exv <- unlist(Ex)
    if (prt =="exp") {
        names(Exv) <- paste0("E(X", 1:ng, ")")
        print(round(as.table(Exv), dig))
    }

    Ex2 <- unlist(Exs)
    Vxv <- unlist(Vx)
    Dxv <- unlist(Dx)
    
    tab <- cbind(Exv, Ex2, Vxv, Dxv)
    colnames(tab) <- c("E(X)", "E(X^2)","V(X)","D(X)")
    rownames(tab) <- paste0("X", 1:ng)
    if (prt == "var") {
        if (ng < 5) {
            print(round(tab, dig))
        } else {
            print(t(round(tab, dig)))
        }
    }

  # Plot PDF f(x)
    if (is.numeric(ws)) {
        x1 <- min(xv)
        x2 <- max(xv)
        xr <- x2 - x1

        mt <- paste0("PDF of ", "X", 1:ng)
        arg1 <- list(type="h", xlim=c(x1-0.1*xr, x2+0.1*xr),
                ylab="f(x)", xlab="x", lwd=3, col="red")
        dd1 <- names(arg1)
        dots <- list(...)
        if (length(dots)>0) {
            if ("main" %in% names(dots)) {
                mt <- dots$main
                if (length(mt)==1) mt <- rep(mt, ng)
                dots <- dots[-which(names(dots) %in% "main")]
            }
        }
        if (length(dots)>0) {
            ck1 <- which(dd1 %in% names(dots))
            for (kk in ck1) arg1[[kk]] <- 
                            dots[[which(names(dots) %in% dd1[kk])]]
            dots <- dots[-which(names(dots) %in% dd1)]
        }
        arg2 <- c(arg1, dots)

      # Set up graphic window
        nc <- switch(ng, 1, 2, 3, 2, 3, 3, 3, 3, 3)
        nr <- switch(ng, 1, 1, 1, 2, 2, 2, 3, 3, 3)
        win.graph(ws[1], ws[2])
        par(mfrow=c(nr, nc))
        ##pm <- par()$mar
        ##mrat <- ws[2]/ws[1]
        ##if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))
        if (ng > 3) par(mar=c(4,4,3,2))

      # Plot
        for (k in 1:ng) {
            xylist <- list(x=xv, y=fx[[k]], main=mt[k])
            do.call(plot, c(xylist, arg2))
            ##plot(xv, fx[[k]], type="h", main=mt[k], ylab="f(x)", 
            ##    xlab="x", lwd=3, col="red")
            if (Add2) lines(xv-del, fx2[[k]], 
                         type=arg2$type, lwd=arg2$lwd, col="blue")
            if (Add3) lines(xv+del, fx3[[k]], 
                         type=arg2$type, lwd=arg2$lwd, col="green3")
        }
    }
    # Return the results
    invisible(tab)
}

# [6-3] Multinomial PDF Plots, Expectations, and Probabilities
#' @title Multinomial PDF Plots, Expectations, and Probabilities
#' @description PDF Plots, Expectations, and Probabilities of Multinomial Distribution.
#' @param ps Probability (or proportion) matrix with rows for multiple cases.
#' @param size Sample size.
#' @param lb Lower bound vector for finding a probability.
#' @param ub Upper bound vector for finding a probability.
#' @param prt Print option: one of c("var", "exp", "n"), Default: "exp".
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return Matrix of multinomial PDF
#' @examples
#' library(scatterplot3d)
#' ps <- matrix(c(1,1,8, 1,5,4, 4,4,2, 1,1,1), nrow=4, ncol=3, byrow=T)
#' multinom.plot(ps, 5)
#' @rdname multinom.plot
#' @export
multinom.plot <- function(ps, size, lb, ub, prt="exp", dig=4, ws="n", ...) {

  # Check input
    if (missing(ps)) stop("Input the probability matrix ps.")
    if (missing(size)) stop("Input the sample size.")
  # Number of categories (nc) and Number of distributions (ng)
    if (is.matrix(ps)) {
        nc <- ncol(ps)
        ng <- nrow(ps)
    } else if (is.vector(ps)) {
        nc <- length(ps)
        ng <- 1
        ps <- matrix(ps, nrow=1)
    } else {
        stop("Input valid probability matrix.")
    }

    if (nc > 3 && is.numeric(ws)) {
        ws <- "n"
        cat("Higher than 3-dimensional plots cannot be drawn.\n")
    }

  # Create sample space using urnsample2( ) function
    xr <- urnsample2(1:nc, size = size, replace = TRUE, ordered = FALSE)
    nr <- nrow(xr)
    if (prt=="all") cat("Number of Possible Combinations =", nr, "\n")

  # Sum frequencies corresponding to each of 1, 2, ..., nc
    x <- list()
    for (i in 1:nc) x[[i]] <- apply(xr, 1, function(x) sum(x==i))
    if (prt=="all") {
        cat("Combinations of the Frequency Vectors -------------\n")
        for (i in 1:nc) print(x[[i]])
    }

  # Calculate probability matrix using dmultinom() function
    xm <- x[[1]]
    for (i in 2:nc) xm <- cbind(xm, x[[i]])
    fx6 <- matrix(NA, nr, ng)
    for (j in 1:ng) {
        for (i in 1:nr) {
            fx6[i, j] <- dmultinom(xm[i, ], size=size, prob=ps[j, ])
        }
    }
    colnames(fx6) <- paste0("P",1:ng)

  # Confirm the sum of probabilities
    if (prt=="all") print(apply(fx6, 2, sum))

  # Normalize probabilitiy matrix
    prm <- ps
    for (j in 1:ng) if (sum(ps[j,])>1) prm[j,] <- ps[j,]/sum(ps[j,])

  # Moments
    if (prt %in% c("exp", "var", "all")) {
       Ex <- size*prm
       colnames(Ex) <- paste0("E(X", 1:nc, ")")
       rownames(Ex) <- paste0("Case", 1:ng)
       tab <- Ex

       if (prt %in% c("var", "all")) {
           Vx <- size*prm*(1-prm)
           colnames(Vx) <- paste0("V(X", 1:nc, ")")
           rownames(Vx) <- paste0("Case", 1:ng)
           tab <- cbind(Ex, Vx)
        }
        print(round(tab, dig))
    }

  # Interval probabilities
    if (!missing(lb) || !missing(ub)) {
        pint <- rep("*", nc)
        if (!missing(lb)) {
            if (length(lb) > nc) {
                stop("Input valid lower bound.")
            } else if (length(lb) < nc) {
                lb[(length(lb)+1):nc] <- NA
            }
            islb <- !is.na(lb)
        } else {
            islb <- rep(FALSE, nc)
            lb <- rep(NA, nc)
        }

        if (!missing(ub)) {
            if (length(ub) > nc) {
                stop("Input valid upper bound.")
            } else if (length(ub) < nc) {
                ub[(length(ub)+1):nc] <- NA
            }
            isub <- !is.na(ub)
        } else {
            isub <- rep(FALSE, nc)
            ub <- rep(NA, nc)
        }

        nlb <- which(islb)
        nub <- which(isub)
        islu <- islb & isub
        nlu <- which(islu)

        if (any(islu)) {
            dble <- rep(FALSE, nc)
            sing <- rep(FALSE, nc)
            dble[islu] <- lb[islu] < ub[islu]
            sing[islu] <- lb[islu] == ub[islu]
            
            if (any(dble)) {
	        ndbl <- which(dble)
                pint[dble] <- paste0(lb[dble], "\U2264",
                               "X", ndbl, "\U2264", ub[dble])
            }
            if (any(sing)) {
	        nsin <- which(sing)
                pint[sing] <- paste0("X", nsin, "=", lb[sing])
            }
        } else if (any(islb)) {
             pint[islb] <- paste0("X", nlb, "\U2265", lb[islb])
        } else if (any(isub)) {
             pint[isub] <- paste0("X", nub, "\U2264", ub[isub])
        }

      # Find the probability
        if (any(!islb)) lb[!islb] <- 0
        if (any(!isub)) ub[!isub] <- size

        hit <- apply(xm, 1, function(y) all(y >= lb & y <= ub))
        prs <- apply(fx6, 2, function(y) sum(y[hit]))
        for (kk in 1:ng) cat(paste0("Case", kk, ": P(", 
                         paste(pint, collapse=","), ") = ",
                         round(prs[kk], dig)), "\n")
        if(grepl("deb", prt)) print(cbind(xm[hit,], round(fx6[hit,],dig)))
    }

  # Change probabilities to texts
    probs <- rep("", ng)
    if (sum(ps[1,])>1) {
        for (j in 1:ng) probs[j] <- paste(paste0(ps[j,],"/",sum(ps[j,])), collapse=",")
    } else {
        for (j in 1:ng) probs[j] <- paste(ps[j,], collapse=",")
    }

  # Display probability distribution function
    if (is.numeric(ws)) {
        mt6 <- paste0("Multinom(", probs, ")")
        wc <- c(1,2,3,2,3,3,4,4,3,4)
        wr <- c(1,1,1,2,2,2,2,2,3,3)
        ww <- c(4,6,9,7,9,9,9,9,9,9)
        wl <- c(3,3,3,6,6,6,6,6,9,9)
        ## win.graph(ww[ng], wl[ng])
        win.graph(ws[1], ws[2])
        par(mfrow=c(wr[ng], wc[ng]))

        scol <- "red"
        slwd <- 5
        smain <- mt6

        dots <- list(...)
        if (length(dots) > 0) {
            pars <- names(dots)
            if ("col" %in% pars) scol <- dots$col
            if ("lwd" %in% pars) slwd <- dots$lwd
            if ("main" %in% pars) {
                smain <- dots$main
                if (length(smain)==1) smain <- rep(smain, ng)
            }
        }

        for (k in 1:ng) scatterplot3d::scatterplot3d(x[[1]], x[[2]], fx6[,k], 
            type="h", main=smain[k], zlab="f(x1,x2,x3)", 
            xlab="x1", ylab="x2", pch=16, lwd=slwd, color=scol)
    }
    dum <- matrix(NA, nc, nr)
    for (i in 1:nc) dum[i,] <- x[[i]]
    rownames(fx6) <- paste0("p(", apply(dum, 2, paste, collapse=","), ")")
    invisible(fx6)
}
