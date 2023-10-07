# [Ch-4 Functions] ----------------------------------------------------------------------------------

# [Ch-4 Function Manual] -----------------------------------------
#' @title Manual for Ch4. Functions
#' @description Ch4. Random Variables and Probability Distributions
#' @param fn Function Number (0-14). Default: 0
#' @return None.
#' @examples
#' ch4.man()
#' ch4.man(7:8)
#' @rdname ch4.man
#' @export
ch4.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] rolldie.sum\t  Probability Distribution of the Sum of n Dice\n")
        cat("[2] urnsamp.sum\t  PDF of the Sum of n numbers Urn Sampled\n")
        cat("[3] disc.cdf\t  CDF of a Discrete Random Variable\n")
        cat("[4] cont.cdf\t  CDF of a Continuous Random Variable\n")
        cat("[5] cont.prob\t  Interval Probability of a Continuous PDF\n")
        cat("[6] disc.jpdf\t  Using Joint PDF of Two Discrete Random Variables\n")
        cat("[7] cont.dint\t  Double Integration of a Continuous Function\n")
        cat("[8] cont.jcdf\t  Joint CDF of Two Continuous Random Variables\n")
        cat("[9] cont.jpdf\t  Using Joint PDF of Continuous Random Variables\n")
        cat("[10] cont.trans\t  Transformation of a Continuous Random Variable\n")
    }
    if (1 %in% fn) {
        cat("[1] Probability Distribution of the Sum of Dice \n")
        cat("rolldie.sum(n, nf=6, prt=FALSE, dig=4, ws=c(7,5), cex)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("n\t Number of die rolls.\n")
        cat("[Optional Input]--------------------------\n")
        cat("nf\t Number of the die faces, Default: 6.\n")
        cat("prt\t Logical: print the distribution? Default: FALSE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,5).\n")
        cat("cex\t Text size in the plot.\n")
    }
    if (2 %in% fn) {
        cat("[2] PDF of the Sum of n Numbers from a Finite Population\n")
        cat("urnsamp.sum(x, n, repl=FALSE, prt=FALSE, dig=4, ws=c(7,5), mt, cex)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Numeric vector of the population.\n")
        cat("n\t Sample size.\n")
        cat("[Optional Input]--------------------------\n")
        cat("repl\t Logical: sampling with replacement? Default: FALSE.\n")
        cat("prt\t Logical: print the distribution? Default: FALSE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,5).\n")
        cat("mt\t Main title of the plot.\n")
        cat("cex\t Text size in the plot.\n")
    }
    if (3 %in% fn) {
        cat("[3] CDF of a Discrete Random Variable\n")
        cat("disc.cdf(fx, prt=TRUE, dig=4, ws=c(7,5), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fx\t Frequency table of the discrete random variable.\n")
         cat("[Optional Input]--------------------------\n")
        cat("prt\t Logical: print the distribution? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws \t Graphic window size, Default: c(7,5).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] CDF of a Continuous Random Variable\n")
        cat("cont.cdf(fun, lb, ub, xp, prt=FALSE, dig=4, ws=c(7,5), xlim, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun\t Probability density function f(x) or \"f(x)\".\n")
        cat("[Optional Input]--------------------------\n")
        cat("lb \t Lower bound of f(x).\n")
        cat("ub \t Upper bound of f(x).\n")
        cat("xp\t Specific values of X for marking the probability.\n")
        cat("prt\t Logical: print F(xp)? Default: FALSE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws \t Graphic window size, Default: c(7,5).\n")
        cat("xlim\t Lower and upper limit of x-axis.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (5 %in% fn) {
        cat("[5] Interval Probability of a Continuous Random Variable\n")
        cat("cont.prob(fun, aa, bb, lb, ub, prt=TRUE, dig=4, ws=\"n\", xlim, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun \t Probability density function f(x) or \"f(x)\".\n")
        cat("aa \t Lower bound aa of P(aa<X<bb).\n")
        cat("aa \t Upper bound bb of P(aa<X<bb).\n")
        cat("[Optional Input]--------------------------\n")
        cat("lb \t Lower bound of the PDF.\n")
        cat("ub \t Upper bound of the PDF.\n")
        cat("prt\t Logical: print F(xp)? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws \t Graphic window size, Default: \"n\".\n")
        cat("xlim\t Lower and upper limit of x-axis.\n")
        cat("... \t Other graphic parameters.\n")
    }
    if (6 %in% fn) {
        cat("[6] Using Joint PDF of Two Discrete Random Variables\n")
        cat("disc.jpdf(X, Y, FUN, type=\"j\", xc, yc, prt=TRUE, dig=4, ws=\"n\", cex=0.8)\n")       
        cat("[Mandatory Input]--------------------------\n")
        cat("X \t Sample space (or range) of X, or table(X,Y).\n")
        cat("Y \t Sample space (or range) of Y (optional).\n")
        cat("FUN \t Joint PDF of X and Y (optional).\n")
        cat("[Optional Input]--------------------------\n")
        cat("type\t \"j\"=joint, \"m\"=margin, \"c\"=condition, \"i\"=indep.\n")
        cat("xc \t Given conditioning value of X (type=\"c\").\n")
        cat("yc \t Given conditioning value of Y (type=\"c\").\n")
        cat("prt \t Logical: print frequency table? Default: TRUE.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
        cat("ws \t Graphic window size, Default: \"n\".\n")
        cat("cex \t Text size of f(x,y) in the plot, Default: 0.8.\n")
    }
    if (7 %in% fn) {
        cat("[7] Double Integration of Continuous Joint PDF\n")
        cat("cont.dint(fun, x1, x2, y1, y2, frac=FALSE, Big=100)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun \t Continuous joint PDF as string \"f(x,y)\".\n")
        cat("x1 \t Lower limit (numeric) of X.\n")
        cat("x2 \t Upper limit (numeric) of X.\n")
        cat("y1 \t Lower limit (numeric, function or strings) of Y.\n")
        cat("y2 \t Upper limit (numeric, function or strings) of Y.\n")
        cat("[Optional Input]--------------------------\n")
        cat("frac\t Logical: convert the result to fraction? Default: FLASE.\n")
        cat("Big \t Big number replacing Inf (functional bound case), Default: 100.\n")
    }
    if (8 %in% fn) {
        cat("[8] Joint CDF of Two Continuous Random Variables\n")
        cat("cont.jcdf(fun, x1, y1, y2, xp, yp, ws=\"n\", xrng, yrng, dig=4)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun \t Continuous joint PDF as string \"f(x,y)\".\n")
        cat("x1 \t Lower limit (numeric) of X.\n")
        cat("y1 \t Lower limit (numeric or character) of Y.\n")
        cat("[Optional Input]--------------------------\n")
        cat("y2 \t Upper limit (character) of Y.\n")
        cat("xp \t Values of X for obtaining F(xp,yp).\n")
        cat("yp \t Values of Y for obtaining F(xp,yp).\n")
        cat("ws \t Graphic window size, Default: \"n\".\n")
        cat("xrng\t Plot range of x-axis.\n")
        cat("yrng\t Plot range of y-axis.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
    }
    if (9 %in% fn) {
        cat("[9] Using Joint PDF of Two Continuous Random Variables\n")
        cat("cont.jpdf(fun,lb,ub,y1,y2,type=\"m\",xp,yp,prt=TRUE,dig=4,ws=\"n\",xl,yl)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun \t Continuous joint PDF as string \"f(x,y)\".\n")
        cat("lb \t Lower bounds of X and Y, numeric.\n")
        cat("ub \t Upper bounds of X and Y, numeric.\n")
        cat("[Optional Input]--------------------------\n")
        cat("y1 \t Functional lower limit of Y, as \"f(x)\".\n")
        cat("y2 \t Functional upper limit of Y, as \"f(y)\".\n")
        cat("type\t \"m\"=margin, \"c\"=condition, \"i\"=independ, Default: \"m\".\n")
        cat("xp \t specific (or conditional) value of X.\n")
        cat("yp \t specific (or conditional) value of Y.\n")
        cat("prt \t Logical: print the marginal PDF? Default: TRUE.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
        cat("ws \t Graphic window size, Default: \"n\".\n")
        cat("xl \t Limit of x-axis of the plot.\n")
        cat("yl \t Limit of y-axis of the plot.\n")
    }
    if (10 %in% fn) {
        cat("[10] Transformation of a Continuous PDF\n")
        cat("cont.trans(fx, flim, tf, a, b, prt=TRUE, dig=4, ws=\"n\", xlim, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fx \t PDF of X as string \"f(x)\".\n")
        cat("flim\t range of X for f(x) > 0.\n")
        cat("[Optional Input]--------------------------\n")
        cat("tf \t Character vector of transformation functions (up to 8).\n")
        cat("a  \t Lower limit of X for calculating P(a<X<b).\n")
        cat("b  \t Upper limit of X for calculating P(a<X<b).\n")
        cat("prt \t Logical: print the output? Default: TRUE.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
        cat("ws \t Window size of the plot, Default: \"n\".\n")
        cat("xlim\t range of x-axis of the plot.\n")
        cat("... \t Other graphic parameters for text.\n")
    }
}

# [CH-4] -----------------------------------
# Iterative Combinatorial Calculation (indep)
dsum_itrn2 <- function(ndice, nside=6) {
    NN = nside^ndice
    currNumArrangements = 0
    tempSum = 0
    XX = as.integer(ndice:(nside * ndice))
    NX = length(XX)
    STM = matrix(c(XX, as.integer(rep.int(0, NX))), nrow = NX, ncol = 2)
    for (BV in 1:nside) {
        for (KK in 1:ndice) {
            numBs = KK
            numIs = (ndice - KK)
            Ncomb = (factorial(ndice)/(factorial(numBs) * factorial(numIs)))
            innB = nside
            outB = 1
            innRng = abs(BV - innB)
            posV = ((BV + 1):nside)

            if (KK == ndice) {
              tempSum = (KK * BV)
              STM[tempSum-(ndice-1),2] = STM[tempSum-(ndice-1),2] + Ncomb
            }
            else {
              outRaw = choose((innRng + numIs - 1), numIs)
              outMtx = matrix(nrow = outRaw, ncol = ndice)
              if (dim(outMtx)[1] > 0) {
                outMtx[, 1:KK] = BV
                hCombs = gtools::combinations(n = innRng, r = numIs, 
                    v = posV, repeats.allowed = TRUE)
                hPerms = apply(hCombs, 1, 
                    function(x) factorial(numIs)/prod(factorial(table(x))))
                outMtx[, (KK + 1):ndice] = hCombs
                for (rowNum in 1:nrow(outMtx)) {
                  tempSum = sum(outMtx[rowNum, ])
                  currNumArr = Ncomb * hPerms[rowNum]
                  STM[tempSum -(ndice - 1), 2] = 
                      STM[tempSum - (ndice - 1), 2] + currNumArr
                }
              }
            }
        }
    }
  # Single die --------
    if (STM[NX, 1] <= ndice) {
        STM = matrix(c(ndice, NN), nrow = 1, ncol = 2)
    }
  # Multiple die --------
    else {
        addComb = sum(STM[STM[, 1] < ndice, 2])
        STM = STM[STM[, 1] >= ndice, ]
        STM[1, 2] = STM[1, 2] + addComb
    }

    invisible(STM[,2])
}

# [4-1] Probability Distribution of the Sum of n Dice
#' @title PDF of the Sum of n Dice
#' @description Probability Distribution of the Sum of n Dice.
#' @param n Number of die rolls.
#' @param nf Number of the die faces, Default: 6.
#' @param prt Logical: print the distribution? Default: FALSE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,5).
#' @param cex Text size in the plot.
#' @return List of frequency and probability distribution of the sum.
#' @examples
#' # Rolling four dice
#' rolldie.sum(4)
#' # Rolling ten dice
#' dist <- rolldie.sum(10)
#' # The sample space has 60466176 elements.
#' sum(dist$freq); 6^10
#' @rdname rolldie.sum
#' @export

rolldie.sum <- function(n, nf=6, prt=FALSE, dig=4, ws=c(7,5), cex) {
  # Text size
    if (missing(cex)) {
        cexv = c(1,1,0.95,0.9,0.85, 0.8,0.7,0.6,0.5,0.4) 
        cex = cexv[n]
    }
  # Calculate the frequency distribution of the sum
    X.val <- n:(n*nf)
    X.freq <- dsum_itrn2(n, nf)
    
    N <- sum(X.freq)
    names(X.freq) <- X.val
    X.prob <- X.freq / N
    if (prt) {
        print(c(X.freq, N))
        print(round(c(X.prob, sum(X.prob)), dig))
    }

  # Mean and variance of random variables X
    EX <- sum(X.val * X.prob)
    EX2 <- sum(X.val^2 * X.prob)
    VX <- EX2 - EX^2
    DX <- sqrt(VX)
    Xmin <- min(X.val)
    Xmax <- max(X.val)

    if (is.numeric(ws)) {
      # Distribution graph of random variables X
        win.graph(ws[1], ws[2])
        plot(X.val, X.prob, type="h", col="red",
          main=paste("Probability Distribution of the Sum of",n,"Dice"),
          lwd=4, ylim=c(0, max(X.prob)*1.1), xlab="Sum (X)", ylab="f(x)")
          fitnorm <- function(x) dnorm(x, EX, DX)
        curve(fitnorm, Xmin, Xmax, add=T, col="blue")
      # Display probability(frequency)
        if(n <=10) {
          text(Xmin:Xmax, X.prob, labels=X.freq, pos=3, col="blue", cex=cex)
        }
        legend("topright", c(paste("|S| =", N), 
          paste("E(X) =",round(EX,dig)), paste("D(X) =",round(DX,dig))), 
          bg="white")
    }
    out <- list(freq=X.freq, prob=X.prob)
    return(out)
}

# [4-2] Probability Distribution of the Sum of n Numbers from a Finite Population
#' @title PDF of the Sum of n Numbers from a Finite Population
#' @description Probability Distribution of the Sum of n Numbers Drawn from a Finite Population.
#' @param x Numeric vector representing the population.
#' @param n Sample size.
#' @param repl Logical: sampling with replacement? Default: FALSE.
#' @param prt Logical: print the distribution? Default: FALSE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,5).
#' @param mt Main title of the plot.
#' @param cex Text size in the plot.
#' @return List of frequency and probability distribution.
#' @examples
#' # Sum of three samples out of 1:10
#' urnsamp.sum(1:10, 3)
#'
#' # Rolling four dice
#' mytitle <- "PDF of the sum of Four Dice"
#' urnsamp.sum(1:6, 4, repl=TRUE, mt=mytitle)
#'
#' # Tossing 20 coins, sum of heads
#' mytitle <- "PDF of the sum of Heads in 20 Coin Tosses"
#' out <- urnsamp.sum(0:1, 20, repl=TRUE, mt=mytitle)
#' sum(out$freq); 2^20
#'
#' @rdname urnsamp.sum
#' @export
urnsamp.sum <- function(x, n, repl=FALSE, prt=FALSE, dig=4, ws=c(7,5), mt, cex) {
  # Create the sample space
    S <- urnsample2(x, n, replace=repl, ordered=TRUE)
    N <- nrow(S)
    if(!is.numeric(x)) stop("x must be a numeric vector...")

    if (missing(cex)) {
        cexv = c(1,1,0.95,0.9,0.85, 0.8,0.7,0.6,0.5,0.4) 
        cex = cexv[min(which(N<6^(1:10)))]
    }
  # Calculate the sum
    X <- apply(S, 1, sum)
    freq <- table(X)
    if (prt) print(addmargins(freq))

  # Calculate probability
    fx <- freq / N
    if (prt) print(round(addmargins(fx), dig))

  # Mean and variance of random variable X
    X.val <- as.numeric(names(freq))
    EX <- sum(X.val * fx)
    EX2 <- sum(X.val^2 * fx)
    VX <- EX2 - EX^2
    DX <- sqrt(VX)
    Xmin <- min(X.val)-1
    Xmax <- max(X.val)+1

    if (is.numeric(ws)) {
      # Distribution graph of random variable X
        if(missing(mt)) mt <- paste0("PDF of the Sum of ", n, 
                      " Samples out of ", length(x), " Numbers")
        win.graph(ws[1], ws[2])
        plot(fx, type="h", col="red", lwd=4, xlim=c(Xmin, Xmax), 
            ylim=c(0, max(fx)*1.1),
            main=mt, xlab="Sum (X)", ylab="f(x)")
      # Display probability(frequency)
        if (N<=6^10) text(X.val, fx, labels=freq, pos=3, cex=cex, col="blue")
        legend("topright", c(paste("|S| =",N), paste("E(X) =",EX),
              paste("D(X) =", round(DX,4))), bg="white")
    }
    out <- list(freq=freq, prob=fx)
    invisible(out)
}

# [4-3] CDF of a Discrete Random Variable
#' @title CDF of a Discrete Random Variable
#' @description Cumulative Distribution Function of a Discrete Random Variable.
#' @param fx Frequency table of the discrete random variable.
#' @param prt Logical: print the distribution? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,5).
#' @param ... Other graphic parameters.
#' @return list(CDF=xcdf, mean=EX, sd=DX).
#' @examples
#' # CDF of the sum of heads in five coin tosses
#' coin5 <- choose(5, 0:5)
#' names(coin5) <- 0:5
#' disc.cdf(coin5, main="CDF of the Sum of Five Coins Heads")
#'
#' # CDF of the random variable of B(10, 0.7)
#' B10.7 <- dbinom(0:10, 10, 0.7)
#' names(B10.7) <- 0:10
#' disc.cdf(B10.7, cex=0.8)
#' @rdname disc.cdf
#' @export
disc.cdf <- function(fx, prt=TRUE, dig=4, ws=c(7,5), ...) {

  # Sort
    xv <- as.numeric(names(fx))
    xp <- fx[order(xv)]
    xv <- sort(xv)

  # Normalize the probabilities
    if (sum(xp) > 1) xp <- xp/sum(xp)

  # Define the CDF F(x)
    xcdf <- c(0, cumsum(xp))
    sf <- stepfun(xv, xcdf)

  # Print
    if (prt) {
      lb <- c(-Inf, xv)
      ub <- c(xv, Inf)
      area <- paste0(c("(",rep("[",length(xv))), lb, ",", ub, ")")
      names(xcdf) <- area
      print(round(xcdf, dig))
    }
  # Mean and variance of random variable X
    EX <- sum(xv * xp)
    EX2 <- sum(xv^2 * xp)
    VX <- EX2 - EX^2
    DX <- sqrt(VX)

  # Plot the CDF F(x)
    if (is.numeric(ws)) {
        mt <- "Cumulative distribution function (CDF) of X"
        arg1 <- list(main=mt, pch=19, lwd=2, ylim=c(-0.02,1.02), col="red",
                   xlab="x", ylab="F(x)", cex=1, verticals=FALSE)
        dots <- list(...)
        dd1 <- c("main", "pch", "lwd", "ylim", "col", "xlab", "ylab", "cex")

        if (length(dots) > 0) {
            ck1 <- which(dd1 %in% names(dots))
            for (kk in ck1) arg1[[kk]] <- 
                            dots[[which(names(dots) %in% dd1[kk])]]
            dots <- dots[-which(names(dots) %in% dd1)]
        }

        win.graph(ws[1], ws[2])
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))

        do.call(plot, c(sf, arg1, dots))
        grid(col="green")

        arg2 <- list(xv, xcdf[-length(xcdf)], col=arg1[[5]], cex=arg1[[8]])
        do.call(points, arg2)

      # Display the probability
        arg3 <- list(xv, xcdf[-1], round(xcdf[-1], dig), col="blue", 
                    pos=2, cex=arg1[[8]])
        do.call(text, arg3)
      # Display legend
        legend("bottomright", c(paste("E(X) =",round(EX, dig)),
               paste("D(X) =", round(DX, dig))), bg="white")
    }
    invisible(list(CDF=xcdf, mean=EX, sd=DX))
}

# [4-4] CDF of a Continuous Random Variable
#' @title CDF of a Continuous Random Variable
#' @description Cumulative Distribution Function of a Continuous Random Variable.
#' @param fx Probability density function f(x) of X.
#' @param lb Lower bound of the PDF.
#' @param ub Upper bound of the PDF.
#' @param xp Specific values of X for displaying the probability.
#' @param prt Logical: print F(xp)? Default: FALSE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,5).
#' @param xlim Lower and Upper limit of x-axis.
#' @param ... Other graphic parameters.
#' @return list(CDF=CDF, mean=EX, sd=DX).
#' @examples
#' # f(x) = 2*exp(-2*x), for (x>0)
#' cont.cdf("2*exp(-2*x)", lb=0, xp=c((1:5)*0.2, 2), xlim=c(-1,3))
#' cont.cdf("2*exp(-2*x)", lb=0, xp=c((1:5)*0.2, 2), prt=TRUE, ws="n")
#' cont.cdf("2*exp(-2*x)", lb=0, xp=c((1:5)*0.2, 2), prt=TRUE)
#'
#' # f(x) = 3*x^2, for (0<x<1)
#' cont.cdf("3*x^2", lb=0, ub=1, xp=(2:5)*0.2, prt=TRUE)
#'
#' # f(x) = dnorm(x,10,2), for (-Inf<x<Inf)
#' cont.cdf("dnorm(x,10,2)", xp=3:7*2, prt=TRUE)
#'
#' @rdname cont.cdf
#' @export
cont.cdf <- function(fun, lb, ub, xp, prt=FALSE, dig=4, ws=c(7,5), xlim, ...) {
  # Check input
    if (missing(fun)) stop("Input f(x)...")
  # Function Input
    if (is.function(fun)) {
        cfx <- deparse(body(fun))
        if (grepl("[<>=]", cfx)) {
            add.bound <- FALSE
            fx <- fun
            cfx2 <- cfx
            dfx <- as.character(body(fx))
            cdn <- grep("[<>=]", dfx)
            if (length(dfx)==3) cfx0 <- dfx[setdiff(2:3, cdn)]
            if (length(dfx)==4) cfx0 <- dfx[3]
        } else {
            cfx0 <- cfx
            add.bound <- TRUE
        }
  # String Input
    } else if (is.character(fun)) {
        cfx0 <- fun
        add.bound <- TRUE
    } else {
        stop("Input f(x) as function or strings...")
    }

  # Bound of the PDF
    if (missing(lb)) {
        lb <- -Inf
        if (missing(ub)) {
            if (add.bound) fx <- str2fn(cfx0, "x")
            ub <- Inf
            add.bound <- FALSE
        } else {
            head <- paste0("ifelse(x<", ub, ", ")
            tail <- paste0(", for (x<", ub, ")")
        }
    } else if (missing(ub)) {
        ub <- Inf
        head <- paste0("ifelse(x>", lb, ", ")
        tail <- paste0(", for (x>", lb, ")")
    } else {
        head <- paste0("ifelse(x>", lb, " & x<", ub, ", ")
        tail <- paste0(", for (", lb, "<x<", ub, ")")
    }
  # Add bound to fx
    if (add.bound) {
        cfx <- paste0(head, cfx0, ", ", 0, ")")
        cfx2 <- paste0(cfx0, tail)
        fx <- str2fn(cfx, "x")
    }

  # Mean and variance of random variable X
    if (lb == -Inf) {
        lb2 <- -1
        while (fx(lb2) > 1e-9) lb2 <- lb2*10
    } else {lb2 <- lb}
    if (ub == Inf) {
        ub2 <- 1
        while (fx(ub2) > 1e-9) ub2 <- ub2*10
    } else {ub2 <- ub}

    xfx <- function(x) x*fx(x)
    x2fx <- function(x) x^2*fx(x)
    EX <- integrate(xfx, lb2, ub2)$value
    EX2 <- integrate(x2fx, lb2, ub2)$value
    VX <- EX2 - EX^2
    DX <- sqrt(VX)

  # Define the CDF F(x)
    Fx <- function(x) integrate(fx, lb2, x)$value
  # Vectorize the CDF
    VFx <- Vectorize(Fx, "x")

  # Set the range of the plot
    if (missing(xlim)) {
        low <- ifelse(lb > -Inf, lb-DX, EX-4*DX)
        up <- ifelse(ub < Inf, ub+DX, EX+4*DX)
    } else {
        low <- xlim[1]
        up <- xlim[2]
    }
  # Set the range of X
    xrange <- seq(low, up, length=100)
    CDF <- VFx(xrange)

  # Print the CDF F(x)
    if (prt) {
      # Integrate w.r.t. X [2023.05.01]
        bbx <- Integ.str(cfx0, "x")
        ## print(bbx)
        if (length(bbx) > 1) {
            ccx <- bbx[2]
            if (ccx=="C") {
                ccx <- bbx[3]
                if (bbx[1]=="-") ccx <- paste0("-(", ccx, ")")
            }
            ## print(ccx)
          # Definite Integral [2023.05.01]
            SFx <- str2fn(ccx, "x")
            SFx0 <- -SFx(lb)
            if (bbx[1]=="-") {
                ddx <- paste(SFx0, ccx)
            } else {
                if (SFx0 == 0) {
                    ddx <- ccx
                } else if (SFx0 < 0) {
                    ddx <- paste(ccx, SFx0)
                } else {
                    ddx <- paste(ccx, "+", SFx0)
                }
            }
            ddx <- Ryacas::yac_str(paste0("Simplify(", ddx, ")"))
            cifx <- sub(cfx0, ddx, cfx2, fixed=TRUE)
        } else {
            cifx <- bbx
        }
        
      # Print
        cat(paste0("F(x) = ", cifx), "\n")
        if (!missing(xp)) {
            Fxp <- VFx(xp)
            names(Fxp) <- paste0("F(", xp, ")")
            print(round(Fxp, dig))
        }
    } # End of Print the CDF F(x)

  # Plot the CDF F(x)
    if (is.numeric(ws)) {
      # Set arguments
        mt <- "Continuous Cumulative Distribution Function (CDF)"
        arg <- list(main=mt, lwd=3, ylim=c(-0.02,1.02),
              col="red", xlab="x", ylab="F(x)", cex=0.8, pos=4)
        dots <- list(...)
        dd1 <- names(arg)

        if (length(dots) > 0) {
            ck1 <- which(dd1 %in% names(dots))
            for (kk in ck1) arg[[kk]] <- 
                     dots[[which(names(dots) %in% dd1[kk])]]
            dots <- dots[-which(names(dots) %in% dd1)]
        }
      # Plot
        win.graph(ws[1], ws[2])
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))

        plot(xrange, CDF, type="l", main=arg[[1]], lwd=arg[[2]],
             ylim=arg[[3]], col=arg[[4]], xlab=arg[[5]], ylab=arg[[6]])
        ## do.call(plot, c(xrange, VFx(xrange), arg))

      # Display the CDF F(xp)
        grid(col="green")
        abline(h=0)
        if (!missing(xp)) {
            n <- length(xp)
            lp <- low + (up-low)*0.2
            segments(lp, VFx(xp), xp, VFx(xp), lty=2, col="blue")
            segments(xp, 0, xp, VFx(xp), lty=2, col="blue")
            text(rep(low, n), VFx(xp), labels=paste0("F(", xp, ")=",
                round(VFx(xp),dig)), col=arg[[4]], 
                cex=arg[[7]], pos=arg[[8]])
        }

       # Display legend
        legend("bottomright", c(paste("E(X) =",round(EX, dig)),
            paste("D(X) =", round(DX, dig))), bg="white")
    }
    CDF <- cbind(xrange, CDF)
    invisible(list(Fx=VFx, mean=EX, sd=DX, fx=fx))
}

# [4-5] Interval Probability of a Continuous PDF
#' @title Interval Probability of a Continuous Random Variable
#' @description Interval Probability P(aa<X<bb) of a Continuous Random Variable X.
#' @param fun Probability density function f(x) or "f(x)".
#' @param aa Lower bound aa of P(aa<X<bb).
#' @param aa Upper bound bb of P(aa<X<bb).
#' @param lb Lower bound of f(x)
#' @param ub Upper bound of f(x)
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,5).
#' @param xlim Lower and Upper limit of x-axis.
#' @param ... Other graphic parameters.
#' @return Table of P(X<bb), P(X<aa), and P(aa<X<bb).
#' @examples
#' # f(x) = 2*exp(-2*x) for (x>0)
#' pdf <- function(x) 2*exp(-2*x)*(x>0)
#' cont.prob(pdf, aa=c(0.2,0.7), bb=c(0.5,1))
#' cont.prob(pdf, aa=c(0.2,0.7), bb=c(0.5,1), prt=FALSE, ws=c(7,4))
#' cont.prob(pdf, aa=c(0.2,0.7), bb=c(0.5,1), ws=c(7,4), xlim=c(-0.5,2))
#'
#' # f(x) = 3*x^2, for (0<x<1)
#' cont.prob("3*x^2", aa=0:2*0.3, bb=1:3*0.25, lb=0, ub=1, ws=c(7,4))
#'
#' # Other built-in PDFs
#' cont.prob("dnorm(x,10,2)", aa=1:3*4, bb=1:3*4+3, ws=c(7,4))
#' cont.prob("dexp(x)", aa=1:3*0.4, bb=1:3*0.4+0.3, prt=TRUE, ws=c(7,4))
#'
#' @rdname cont.prob
#' @export
cont.prob <- function(fun, aa, bb, lb, ub, prt=TRUE, dig=4, ws="n", xlim, ...) {
  # Check input
    if (missing(fun)) stop("Input f(x)...")
    if (missing(aa)) stop("Input aa to find P(aa<X<bb)...")
    if (missing(bb)) stop("Input bb to find P(aa<X<bb)...")

  # Obtain the PDF fx
    temp <- cont.cdf(fun, lb, ub, prt=FALSE, ws="n")
    Fx <- temp$Fx
    fx <- temp$fx

  # Obtain the interval probability
    Fupp <- Fx(bb)
    Flow <- Fx(aa)
    Prob <- Fupp - Flow
    ptab <- cbind(Prob, Fupp, Flow)
    colnames(ptab) <- c("Prob", "F(upp)", "F(low)")
    rownames(ptab) <- paste0("[",aa,",",bb,"]")

  # Print P(aa<X<bb) = F(bb)-F(aa)
    if (prt) print(round(ptab, dig))

  # Plot the PDF F(x)
    if (is.numeric(ws)) {
        if (missing(xlim)) xlim <- c(min(aa)-(max(bb)-min(aa))/10,
                               max(bb)+(max(bb)-min(aa))/10)
        low <- xlim[1]
        up <- xlim[2]
      # Set the range of X
        xrange <- seq(low, up, length=100)
        PDF <- fx(xrange)

      # Set arguments
        mt <- "Continuous PDF and Interval Probabilities"
        arg <- list(main=mt, lwd=2, col="red", xlab="x", ylab="f(x)", 
               cex=0.8, fill='lightcyan')
        dots <- list(...)
        dd1 <- names(arg)

        if (length(dots) > 0) {
            ck1 <- which(dd1 %in% names(dots))
            for (kk in ck1) arg[[kk]] <- dots[[which(names(dots) %in% dd1[kk])]]
            dots <- dots[-which(names(dots) %in% dd1)]
        }
      # Plot
        win.graph(ws[1], ws[2])
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))

        y2 <- max(PDF)
        plot(xrange, PDF, type="n", main=arg[[1]],
             ylim=c(-y2/15,y2), xlab=arg[[4]], ylab=arg[[5]])
        grid(col=3)
      # Polygons
        for (kk in 1:length(aa)) {
            xint <- seq(aa[kk],bb[kk],0.01)
            cord.x <- c(aa[kk], xint, bb[kk])
            cord.y <- c(0, fx(xint), 0)

            polygon(cord.x, cord.y, col=arg[[7]])
            mid <- (aa[kk]+bb[kk])/2
            text(mid, 0.4*fx(mid), labels=round(Prob[kk], dig), cex=arg[[6]])
            lines(xint, fx(xint), lwd=2, col=1)
        }
        abline(h=0)
        lines(xrange, PDF, lwd=arg[[2]], col=arg[[3]])
        text(c(aa,bb), 0, c(aa,bb), col="blue", cex=arg[[6]], pos=1)
    }
    invisible(ptab)
}

# [4-6] Joint Probability Distribution of Two Discrete Random variable
#' @title Joint PDF of Two Discrete Random variable
#' @description Joint, Marginal, Conditional Probability Distribution of Two Discrete Random variables.
#' @param X Sample space (or range) of X, or table(X,Y).
#' @param Y Sample space (or range) of Y (optional).
#' @param FUN Joint PDF of X and Y (optional).
#' @param type "j"=joint, "m"=margin, "c"=condition, "i"=independ, Default: "j".
#' @param xc Given conditioning value of X (type="c").
#' @param yc Given conditioning value of Y (type="c").
#' @param prt Logical: print frequency table? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param cex Text size of f(x,y) in the plot, Default: 0.8.
#' @return Joint frequency and probability table.
#' @examples
#' require(scatterplot3d)
#' # [1] Input the elements of X and Y, respectively
#' S = rolldie2(3)
#' X = apply(S, 1, median)
#' Y = apply(S, 1, \(x) max(x)-min(x))
#' # Joint
#' disc.jpdf(X, Y)
#' disc.jpdf(X, Y, prt=FALSE, ws=c(7,5))
#' # Marginal
#' disc.jpdf(X, Y, type="m")
#' disc.jpdf(X, Y, type="m", prt=FALSE, ws=c(7,5))
#' # Conditional
#' disc.jpdf(X, Y, type="c", yc=0:5)
#' disc.jpdf(X, Y, type="c", yc=0:5, prt=FALSE, ws=c(7,5))
#' # Independence
#' disc.jpdf(X, Y, type="i")
#' 
#' # [2] Functional input
#' fxy = function(x, y) x^2+y
#' x=1:3; y=1:5
#' disc.jpdf(x, y, fxy, ws=c(7,5))
#' 
#' # Marginal
#' disc.jpdf(x, y, fxy, type="m", ws=c(7,5))
#' # Conditional
#' disc.jpdf(x, y, fxy, type="c", xc=1:3, ws=c(9,3))
#' # Independence
#' disc.jpdf(x, y, fxy, type="i")
#' 
#' # [3] Tabular input
#' x=1:3; y=1:5
#' tab = outer(x, y, fxy)
#' dimnames(tab) <- list(X=x, Y=y)
#' disc.jpdf(tab, ws=c(7,5))
#'
#' # Marginal
#' disc.jpdf(tab, type="m", ws=c(7,5))
#' # Conditional
#' disc.jpdf(tab, type="c", xc=1:3, ws=c(9,3))
#' # Independence
#' disc.jpdf(tab, type="i")
#' 
#' @rdname disc.jpdf
#' @export
disc.jpdf <- function(X, Y, FUN, type="j", xc, yc, 
                 prt=TRUE, dig=4, ws="n", cex=0.8)
{
  # Check input
  # [1] Functional input
    if (!missing(FUN)) {
        Xn <- toupper(deparse(substitute(X)))
        Yn <- toupper(deparse(substitute(Y)))
        tab <- outer(X, Y, FUN)
        tabXY <- as.table(tab)
        dimnames(tabXY) <- list(X, Y)
        names(attr(tabXY, "dimnames")) <- c(Xn, Yn)
  # [2] Tabular input
    } else if (is.matrix(X)) {
        tabXY <- X
        Xn <- names(attr(tabXY, "dimnames"))[1]
        Yn <- names(attr(tabXY, "dimnames"))[2]
  # [3] Raw input
    } else {
        Xn <- deparse(substitute(X))
        Yn <- deparse(substitute(Y))
        tabXY <- table(X, Y)
    }

    N <- sum(tabXY)

  # Joint and marginal frequency table
    mtabXY <- addmargins(tabXY)
  # Joint and marginal probability
    ptabXY <- tabXY/N
    mptabXY <- addmargins(ptabXY)

  # Print and Plot
    type <- tolower(substr(type, 1, 1))
  # [1] Joint PDF --------------------------------
  if (type=="j") {
    if (prt) {
      if (abs(N-1) < 1e-10) {
        cat(paste("Joint & Marginal Probability Distribution of",
                 Xn, "&", Yn), "\n")
        print(round(mptabXY, dig))
      } else {
        cat(paste("Joint & Marginal Frequency Distribution of",
                 Xn, "&", Yn), "\n")
        print(mtabXY)
      }
    }

   # Plot Joint PDF
    if (is.numeric(ws)) {
      require(scatterplot3d)
      xa <- as.numeric(rownames(tabXY))
      nx <- length(xa)
      ya <- as.numeric(colnames(tabXY))
      ny <- length(ya)
      xa <- rep(xa, ny)
      ya <- rep(ya, each=nx)
      fxy <- as.vector(as.matrix(ptabXY))

     # Set colors
      dc <- rank(fxy)
      dp <- sort(unique(dc))
      nc <- length(dp)
      for (k in 1:nc) dc[dc==dp[k]] = nc+1-k
      dcol <- heat.colors(nc)

     # Open graphic window
      win.graph(ws[1], ws[2])
      pm <- par()$mar
      mrat <- ws[2]/ws[1]
      if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))

      if (abs(N-1) < 1E-10) {
          qxy <- round(tabXY, dig)
      } else {
          qxy <- paste0(tabXY,"/",N)
      }
      ## labels = round(fxy[fxy>0], dig) ##
      s3d <- scatterplot3d::scatterplot3d(xa, ya, fxy, type="h",
            main=paste("Joint Probability Distribution of", Xn, "&", Yn),
            xlab=Xn, ylab=Yn, zlab="f(x, y)", pch=16, lwd=5, color=dcol[dc])
            s3d.coords <- s3d$xyz.convert(xa, ya, fxy)
      text(s3d.coords$x[fxy>0], s3d.coords$y[fxy>0], labels = qxy[fxy>0],
             pos = 3, offset = 0.3, col="blue", cex=cex)
    }
  }

  # [2] Marginal PDF --------------------------------
  if (type=="m") {
   # Marginal frequency table of X and Y
    tabX <- as.table(rowSums(tabXY))
    tabY <- as.table(colSums(tabXY))
   # Marginal probability of X and Y
    ptabX <- tabX/N
    ptabY <- tabY/N

   # Print the marginal frequency of X and Y
    if (prt) {
        cat("Marginal Frequency Distribution of", Xn, "\n")
        print(addmargins(tabX))
        # cat("Marginal probability distribution of", Xn, "\n")
        # print(round(ptabX, dig))
        cat("Marginal Frequency Distribution of", Yn, "\n")
        print(addmargins(tabY))
        # cat("Marginal probability distribution of", Yn, "\n")
        # print(round(ptabY, dig))
    }

   # Plot the marginal probability of X and Y
    if (is.numeric(ws)) {
      xa <- as.numeric(names(tabX))
      nx <- length(xa)
      ya <- as.numeric(names(tabY))
      ny <- length(ya)

     # Display graph
      win.graph(ws[1], ws[2])
      pm <- par()$mar
      par(mar=c(2,4,3,2))
      par(mfrow=c(2,1))

      plot(ptabX, type="h", ylim=c(0, max(ptabX)*1.15), 
	  main=paste("Marginal Probability Distribution of", Xn),
          xlab="", ylab="f(x)", pch=16, lwd=5, col="red")
      text(xa, ptabX, paste0(tabX, "/", N), pos=3, col="blue", cex=0.8)
      plot(ptabY, type="h", ylim=c(0, max(ptabY)*1.15), 
	  main=paste("Marginal Probability Distribution of", Yn),
          xlab="", ylab="f(y)", pch=16, lwd=5, col="red")
      text(ya, ptabY, paste0(tabY, "/", N), pos=3, col="blue", cex=0.8)
    }
  }

  # [3] Conditional PDF --------------------------------
  if (type=="c") {
   # Check Input
    if (missing(xc) && missing(yc)) 
        stop("Input condition values xc or yc...")
   # Marginal frequency table of X and Y
    tabX <- as.table(rowSums(tabXY))
    tabY <- as.table(colSums(tabXY))

   # Extract values of X and Y
    xa <- as.numeric(names(tabX))
    nx <- length(xa)
    ya <- as.numeric(names(tabY))
    ny <- length(ya)

   # List of conditional probabilities
    nc <- ifelse (missing(xc), length(yc), length(xc))
    cpdf <- vector("list", nc)
    cfreq <- vector("list", nc)

   # Case when the conditioning variable = Y
    if (!missing(yc)) {
        Cs <- yc
        Cn <- "Y"
        Dn <- "X"
        da <- xa
        yla <- paste0("f(x|y=",Cs,")")

        for (k in 1:nc) {
            Cv <- as.character(Cs[k])
            cfreq[[k]] <- tabXY[ , Cv]
            cpdf[[k]] <- cfreq[[k]] / sum(cfreq[[k]])
        }
    }
   # Case when the conditioning variable = X
    if (!missing(xc))  {
        Cs <- xc
        Cn <- "X"
        Dn <- "Y"
        da <- ya
        yla <- paste0("f(y|x=",Cs,")")

        for (k in 1:nc) {
            Cv <- as.character(Cs[k])
            cfreq[[k]] <- tabXY[Cv, ]
            cpdf[[k]] <- cfreq[[k]] / sum(cfreq[[k]])
        }
    }
   # Print the conditional PDF
    if (prt) {
	tout <- NULL
        for (k in 1:nc) {
	    tout <- rbind(tout, c(cfreq[[k]],sum(cfreq[[k]])))
	}
        tout <- as.data.frame(matrix(tout, nrow=nc))
        colnames(tout) <- c(da, "Denom")
        rownames(tout) <- yla
        print(tout)
    }

  # Plot conditional PDF
    if (is.numeric(ws)) {
        dr <- switch(nc, 1, 1, 1, 2, 2, 2, 3, 3, 3)
        dc <- switch(nc, 1, 2, 3, 2, 3, 3, 3, 3, 3)
        # ww <- switch(nc, 7, 7, 8, 8, 9, 9, 9, 9, 9)
        # wh <- switch(nc, 3, 6, 6, 6, 6, 6, 9, 9, 9)
        win.graph(ws[1], ws[2])
        par(mfrow=c(dr,dc))
        par(mar=c(3,4,3,2))

        ymax <- max(sapply(cpdf, max))
        for (k in 1:nc) {
            plot(da, cpdf[[k]], type="h",
                main=paste0("Cond. Prob. Dist. of ", Dn, " | ", Cn, "=", Cs[k]),
                ylim=c(0, ymax*1.15), xlim=c(min(da)-0.3,max(da)+0.3), 
		xlab="", ylab=yla[k], pch=16, lwd=5, col="red")
            text(da, cpdf[[k]], paste0(cfreq[[k]], "/", sum(cfreq[[k]])), 
                pos=3, col="blue", cex=0.8)
        }
    }
  }

  # [4] Independence of X & Y ----------------------------
  if (type=="i") {
   # Product of marginal probabilities
    frX <- apply(tabXY, 1, sum)
    frY <- apply(tabXY, 2, sum)
    frXY <- (frX %o% frY)/N
    mfrXY <- addmargins(as.table(frXY))

   # Compare the joint probability and the product of marginal probabilities
    diffXY <- mtabXY - mfrXY
    Expect <- c(as.numeric(names(frX)), NA)
    dfXY <- cbind(mtabXY, Expect, round(mfrXY, 2))

    xn <- tolower(Xn)
    yn <- tolower(Yn)
    if (prt) {
        cat(paste0("Joint PDF: f(", xn, "," , yn, ")\U00D7",N, 
                 "\t <=> f(", xn, ")f(", yn, ")\U00D7",N), "\n")
        print(dfXY)
    }
   # Determine independence
    err <- abs(max(tabXY - frXY))
    if (err == 0) {
        cat(paste0("f(", xn, ",", yn, ") = f(", xn, ")f(", yn, ")"),
           "\U21D2 Independent\n")
    } else {
        cat(paste0("max|f(", xn, ",", yn, ")-f(", xn, ")f(", yn, ")| ="), 
            round(err, dig), "/", N, "\U21D2 Not Independent\n")
    }
  }

  # Return the joint PDF of X & Y
    if (type=="j") {
        invisible(list(freq=tabXY, prob=ptabXY))
    } else if (type=="m") {
        invisible(list(fx=ptabX, fy=ptabY))
    } else if (type=="c") {
        invisible(list(freq=cfreq, pdf=cpdf))
    } else if (type=="i") {
        invisible(list(freq=mtabXY, exp=mfrXY))
    }
}

# Functions for [4-7] Double Integration of Continuous Joint PDF
# Regular Integration (Not implemented...)
dint.reg <- function(fxy, x1, x2, y1, y2) {
  res <- 
    integrate(function(y) {
      sapply(y, function(y) {
        integrate(function(x) {
          sapply(x, function(x) FUN(x, y)) }, x1, x2)$value
      })
     }, y1, y2)[[1]]
  return(res)
}

# Numeric Integration by cubature package (Implemented...)
dint.cub <- function(fun, a1, a2, b1, b2, tol = 1e-07) {
  # require(mosaicCalc)
  # require(cubature)
  # require(MASS)
  # Check Input
    if (missing(fun)) stop("Input the PDF as a string format...")
    if (!is.character(fun)) stop("Input the PDF as a string format...")
    if (any(c(missing(a1),missing(a2),missing(b1),missing(b2)))) 
        stop("Input a1, a2, b1, b2 as numerics...")
    if (any(c(!is.numeric(a1),!is.numeric(a2),!is.numeric(b1),!is.numeric(b2)))) 
        stop("Input a1, a2, b1, b2 as numerics...")

    fxy <- as.formula(paste(fun, "~", "x+y"))
    f <- mosaicCore::makeFun(fxy)
    vf <- function(v) f(v[1], v[2])
    ## Integrate2(fxy, bounds(x=a1:a2, y=b1:b2))
    res <- cubature::hcubature(vf, c(a1, b1), c(a2, b2), tol = tol, 
          maxEval = 1e+06)$integral
    res
}

# Not implemented due to poor performance (dint.pcub)
# Symbolic Method by "Ryacas" package --(polynomial only)----
# Not implemented ...(dint.yak)

# Symbolic by "mosaicCalc" package --(a few problems)----
dint.mc <- function(fun, x1, x2, y1, y2, detail=FALSE, dig=4) {
  # require(mosaicCalc)
  # Check Input
    if (missing(fun)) stop("Input the PDF as a string format...")
    if (!is.character(fun)) stop("Input the PDF as a string format...")
    if (any(c(missing(x1),missing(x2),missing(y1),missing(y2)))) 
        stop("Input x1, x2, y1, y2...")
    if (any(c(!is.numeric(x1),!is.numeric(x2)))) 
        stop("Input x1, x2 as numerics...")

  # Integ(fun(x,y)) w.r.t. y => Fy(y,x)
    int1 <- mosaicCalc::antiD(as.formula(paste(fun, "~", "y")))

  # Integ(Fy(y,x)) w.r.t. x => Fyx(y1,x)
    if(is.character(y1)) {
        ym1 <- mosaicCore::makeFun(as.formula(paste(y1, "~", "x")))
        int21 <- mosaicCalc::antiD(int1(ym1(x), x) ~ x)
    } else {
        int21 <- mosaicCalc::antiD(int1(y, x) ~ x, y=y1)
    }
  # Integ(Fy(y,x)) w.r.t. x => Fyx(y2,x)
    if(is.character(y2)) {
        ym2 <- mosaicCore::makeFun(as.formula(paste(y2, "~", "x")))
        int22 <- mosaicCalc::antiD(int1(ym2(x), x) ~ x)
    } else {
        int22 <- mosaicCalc::antiD(int1(y, x) ~ x, y=y2)
    }
  # Final calculation
    res <- int22(x2) - int21(x2) - (int22(x1) - int21(x1))
  # Print detailed output
    if (detail) {
        cat(paste0("(", round(int22(x2),dig), "-", round(int21(x2),dig), 
           ") - (", round(int22(x1),dig), "-", round(int21(x1),dig), ")"), "\n")
        cat(paste0(" = (", round(int22(x2)-int21(x2), dig), "-", 
            round(int22(x1)-int21(x1), dig), ") = ", round(res, dig)), "\n")
    }
  # Return output
    return(res)
}

# [4-7] Double Integration of Continuous Joint PDF
# [Main] Define function Pr(x1<X<x2, y1<Y<y2) by double integration
# Hybrid Method, New [2023.04.06]
#' @title Double Integration of Joint PDF of Continuous Random Variables
#' @description Double Integration of Joint PDF of Continuous Random Variables.
#' @param fun Continuous joint PDF as string "f(x,y)".
#' @param x1 Lower limit (numeric) of X.
#' @param x2 Upper limit (numeric) of X.
#' @param y1 Lower limit (numeric, function or strings) of Y.
#' @param y2 Upper limit (numeric, function or strings) of Y.
#' @param frac Logical: convert the result to fraction? Default: FLASE.
#' @param Big Big number replacing Inf (functional bound case), Default: 100.
#' @return Double integral.
#' @examples
#' # Integrate f(x,y) = 2*(x+y)*(x<y) over 0<x,y<1
#' cont.dint("2*(x+y)", 0, 1, "x", 1)
#' # P(0<X<0.5, 0<Y<0.5)
#' cont.dint("2*(x+y)", 0, 0.5, "x", 0.5)
#' cont.dint("2*(x+y)", 0, 0.5, "x", 0.5, frac=TRUE)
#'
#' # Integrate f(x,y) = 2*exp(-x-y) over 0<x<y
#' cont.dint("2*exp(-x-y)", 0, Inf, "x", Inf)
#' # P(0<X<0.5, X<Y<0.5)
#' cont.dint("2*exp(-x-y)", 0, 0.5, "x", 0.5)
#'
#' @rdname cont.dint
#' @export

cont.dint <- function(fun, x1, x2, y1, y2, frac=FALSE, Big=100) {
  # require(MASS)
  # require(pracma)
  # require(mosaicCalc)
  # require(cubature)
  # Check Input
    if (missing(fun)) stop("Input the PDF as a string format or a function...")
    if (any(c(missing(x1),missing(x2),missing(y1),missing(y2)))) 
        stop("Input x1, x2, y1, y2...")
    if (any(c(!is.numeric(x1),!is.numeric(x2)))) 
        stop("Input x1, x2 as numerics...")

  # Functional Input => pracma::integral2()
    if (is.function(fun)) {
      # require(pracma)
      # Big number trick
        x1 <- max(x1, -Big)
        x2 <- min(x2, Big)
        if (is.numeric(y1)) y1 <- max(y1, -Big)
        if (is.numeric(y2)) y2 <- min(y2, Big)
        res <- pracma::integral2(fun, x1, x2, y1, y2)[[1]]
  # Character String Input --------------
    } else if (is.character(fun)) {
        if (any(is.character(y1), is.character(y2))) {
            res <- dint.mc(fun, x1, x2, y1, y2)
        } else {
            res <- dint.cub(fun, x1, x2, y1, y2)
        }
    }
    if (frac) res <- MASS::fractions(res)
    return(res)
}

# [4-8] Joint CDF of Two Continuous Random Variables
#' @title Joint CDF of Two Continuous Random Variables
#' @description Joint CDF F(xp,yp) of Two Continuous Random Variables.
#' @param fun Continuous joint PDF as string "f(x,y)".
#' @param xp Values of X for obtaining F(xp,yp)
#' @param yp Values of Y for obtaining F(xp,yp)
#' @param x1 Lower limit (numeric) of X.
#' @param y1 Lower limit (numeric or character) of Y.
#' @param y2 Upper limit (character) of Y.
#' @param ws Graphic window size, Default: "n".
#' @param xrng Plot range of x-axis
#' @param yrng Plot range of y-axis
#' @param dig Number of decimal places, Default: 4.
#' @return data frame(x=xp, y=yp, F=F(xp,yp)).
#' @examples
#' # CDF for f(x,y)=2*(x+y), (0<x<y<1)
#' cont.jcdf("2*(x+y)", 0, "x", xp=-1:6/5, yp=-1:6/5)
#' # Plot the CDF in 3D
#' cont.jcdf("2*(x+y)", 0, "x", ws=c(7,5), xrng=c(0,1), yrng=c(0,1))
#' 
#' @rdname cont.jcdf
#' @export
cont.jcdf <- function(fun, x1, y1, y2, xp, yp, ws="n", xrng, yrng, dig=4) {

    if (missing(fun)) stop("Input the PDF as a string format...")
    if (!is.character(fun)) stop("Input the PDF as a string format...")

    if (missing(x1) || missing(y1)) 
        stop("Input lower bounds: x1 and y1...")
    if (!is.numeric(x1)) 
        stop("Input x1 as numeric...")

  # Functional upper bound case -------
    if (!missing(y2)) {
        if (!is.character(y2)) stop("Input y2 as a string format...")
        upper <- TRUE
    } else upper <- FALSE

  # Define CDF
    Fxy <- function(x, y) {
        if (upper) y <- paste0("pmin(", y, ",", y2, ")")

        if (is.numeric(y1)) {
            res <- ifelse(x <= x1, 0, ifelse(y <= y1, 0, 
                 pmin(1, cont.dint(fun, x1, x, y1, y)) ))
        } else {
            res <- ifelse(x <= x1, 0, 
                 pmin(1, cont.dint(fun, x1, x, y1, y)) )
        }
    }

  # Vectorize CDF
    VFxy <- Vectorize(Fxy, c("x", "y"))

  # Plot the joint CDF F(x,y)
    if (is.numeric(ws)) {
        require(scatterplot3d)
      # Set plot area of X and Y
        xa <- seq(xrng[1], xrng[2], length=20)
        ya <- seq(yrng[1], yrng[2], length=20)
        xa <- rep(xa, 20)
        ya <- rep(ya, each=20)
      # Display graph
        win.graph(ws[1], ws[2])
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))

        s3d <- scatterplot3d::scatterplot3d(xa, ya, VFxy(xa, ya), 
              highlight.3d=TRUE,
              main="Joint Cumulative Distribution of X and Y",
              xlab="x", ylab="y", zlab="F(x,y)", pch=20)   
    }

  # Calculate the CDF
    if (!missing(xp) || !missing(yp)) {
        if (missing(xp) || missing(yp)) 
            stop("Input xp, yp as numerics...")
        if (!is.numeric(xp) || !is.numeric(yp)) 
            stop("Input xp, yp as numerics...")
        if (length(xp) != length(yp)) 
            stop("Length of xp & yp must be equal...")

        prob <- VFxy(xp, yp)
        df <- data.frame(x=xp, y=yp, F=prob) 
        names(prob) <- paste0("F(",xp,",",yp,")")
        print(round(prob, dig))

      # Return cumulative probability F(xp, yp)
        invisible(df)
    }
}

# [4-9] Joint PDF of Two Continuous Random Variables
#' @title Joint PDF of Two Continuous Random Variables
#' @description Obtain Marginal and Conditional PDF and Check Independence from the Joint PDF of Two Continuous Random Variables as Functions.
#' @param fun Continuous joint PDF function "f(x,y)".
#' @param lb Lower bounds of X and Y, numeric.
#' @param ub Upper bounds of X and Y, numeric.
#' @param y1 Functional lower limit of Y, as "f(x)".
#' @param y2 Functional upper limit of Y, as "f(y)".
#' @param type "m"=marginal, "c"=conditional, "i"=independence, Default: "m".
#' @param xp specific (or conditional) value of X.
#' @param yp specific (or conditional) value of Y.
#' @param prt Logical: print the marginal PDF? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param xl Limit of x-axis of the plot.
#' @param yl Limit of y-axis of the plot.
#' @return Marginal("m") or conditional("c") PDF as functions.
#' @examples
#' # [Ex1] f(x,y) = (x+y), (0<x<1, 0<y<1)
#' # Marginal PDF
#' fn <- cont.jpdf("x+y", 0, 1, type="m")
#' integrate(fn$fx, 0, Inf)[[1]]; integrate(fn$fy, 0, Inf)[[1]]
#' fn <- cont.jpdf("x+y", 0, 1, type="m", prt=FALSE, 
#'                 ws=c(7,5), xp=1:4/5, yp=1:4/5)
#' # Conditional PDF
#' fn <- cont.jpdf("x+y", 0, 1, type="c")
#' fn <- cont.jpdf("x+y", 0, 1, xp=1:4/5, type="c")
#' fn <- cont.jpdf("x+y", 0, 1, xp=1:4/5, type="c", prt=FALSE, ws=c(7,5))
#' # Independence
#' fn=cont.jpdf("x+y", 0, 1, type="i")
#'
#' # [Ex2] f(x,y) = 2*(x+y), (0<x<y<1)
#' # Marginal PDF
#' fn <- cont.jpdf("2*(x+y)", 0, 1, y1="x", type="m")
#' integrate(fn$fx, 0, Inf)[[1]]; integrate(fn$fy, 0, Inf)[[1]]
#' fn <- cont.jpdf("2*(x+y)", 0, 1, y1="x", type="m", prt=FALSE,
#'                 ws=c(7,5),xp=0.2*2:4,yp=0.2*2:4)
#' # Conditional PDF
#' fn <- cont.jpdf("2*(x+y)", 0, 1, y1="x", type="c")
#' fn <- cont.jpdf("2*(x+y)", 0, 1, y1="x", xp=1:4/5, type="c")
#' fn <- cont.jpdf("2*(x+y)", 0, 1, y1="x", xp=1:4/5, type="c", 
#'                 prt=FALSE, ws=c(7,5))
#' # Independence
#' fn <- cont.jpdf("2*(x+y)", 0, 1, y1="x", type="i")
#' # [Ex3] fxy = 2*exp(-(x+y)), (0<x<y<Inf)
#' # Marginal PDF
#' fn <- cont.jpdf("2*exp(-(x+y))", 0, Inf, y1="x")
#' integrate(fn$fx, 0, Inf)[[1]]; integrate(fn$fy, 0, Inf)[[1]]
#' # Conditional PDF
#' fn <- cont.jpdf("2*exp(-(x+y))", 0, Inf, y1="x", type="c")
#' fn <- cont.jpdf("2*exp(-(x+y))", 0, Inf, y2="x", xp=1:4, type="c", 
#'                 prt=FALSE, ws=c(7,5), xl=c(0,6))
#' # Independence
#' fn <- cont.jpdf("2*exp(-(x+y))", 0, Inf, y1="x", type="i")
#'
#' # [Ex4] fxy = 24*x*y, (0<x<1, 0<y<1, x+y<1)
#' # Marginal PDF
#' fn <- cont.jpdf("24*x*y", 0, 1, y2="1-x")
#' integrate(fn$fx, 0, Inf)[[1]]; integrate(fn$fy, 0, Inf)[[1]]
#' # Conditional PDF
#' fn <- cont.jpdf("24*x*y", 0, 1, y2="1-x", type="c")
#' fn <- cont.jpdf("24*x*y", 0, 1, y2="1-x", type="c", 
#'                 prt=FALSE, ws=c(7,5), yp=1:4/5)
#' # Independence
#' fn <- cont.jpdf("24*x*y", 0, 1, y2="1-x", type="i")

#' @rdname cont.jpdf
#' @export

cont.jpdf <- function(fun, lb, ub, y1, y2, type="m", xp, yp, 
                      prt=TRUE, dig=4, ws="n", xl, yl) {
  # Check Input
    if (missing(fun)) stop("Input the PDF \"f(x,y)\" as string...")
    if (is.function(fun)) fun <- deparse(body(fun))
    if (!is.character(fun)) stop("Input the PDF \"f(x,y)\" as string...")
    if (missing(lb)) lb <- -Inf
    if (missing(ub)) ub <- Inf
    ## stop("Input numeric lower and upper bounds: lb and ub...")

  # Duplicate "x" and "y" [2023.4.29]
    if (!grepl("xx", fun)) {
        fun <- gsub("x", "xx", fun)
        fun <- gsub("exxp", "exp", fun)
    }
    if (!grepl("yy", fun)) fun <- gsub("y", "yy", fun)
    if (!missing(y1) && !grepl("xx",y1)) y1 <- gsub("x", "xx", y1)
    if (!missing(y2) && !grepl("xx",y2)) y2 <- gsub("x", "xx", y2)

  # Numeric Bounds of X and Y
    if (length(lb)==1) lb <- rep(lb, 2)
    if (length(ub)==1) ub <- rep(ub, 2)
    xrng <- paste0("(", lb[1], "<x<", ub[1], ")")
    yrng <- paste0("(", lb[2], "<y<", ub[2], ")")
    xrf <- paste0("ifelse(", lb[1], "<x & x<", ub[1], ",")
    yrf <- paste0("ifelse(", lb[2], "<y & y<", ub[2], ",")

  # Functional range of X and Y
    xm1 <- x1 <- lb[1]
    xm2 <- x2 <- ub[1]

  # Range of X -------[2023.5.16]
    if (missing(y2)) {
        ym2 <- y2 <- ub[2]
    } else {
        ym2 <- paste0("(", y2, ")")
        x.y2 <- Simp.inv(y2)
        Rev.y2 <- ifelse (grepl("[-/][ (]*x", y2), TRUE, FALSE)
        if (length(x.y2) > 1) {
            if (Rev.y2) {
                if (ub[1] > 0) {
                    x2 <- x.y2[1]
                    xm2 <- paste0("(", x2, ")")
                }
                if (lb[1] < 0) {
                    x1 <- x.y2[2]
                    xm1 <- paste0("(", x1, ")")
                }
            } else {
                if (ub[1] > 0) {
                    x1 <- x.y2[1]
                    xm1 <- paste0("(", x1, ")")
                }
                if (lb[1] < 0) {
                    x2 <- x.y2[2]
                    xm2 <- paste0("(", x2, ")")
                }
            }
        } else {
            ##dx.y2 <- as.character(body(str2fn(x.y2, "yy")))
          # Both upper bound [2023.4.25]
            ##if (dx.y2[1] %in% c("-","/") && grep("yy", dx.y2)==3) {
            if (Rev.y2) {
                x2 <- x.y2
                xm2 <- paste0("(", x2, ")")
            } else {
                x1 <- x.y2
                xm1 <- paste0("(", x1, ")")
            }
        }
    }
    if (missing(y1)) {
        ym1 <- y1 <- lb[2]
    } else {
        ym1 <- paste0("(", y1, ")")
        x.y1 <- Simp.inv(y1)
        Rev.y1 <- ifelse (grepl("[-/][ (]*x", y1), TRUE, FALSE)
        if (length(x.y1) > 1) {
            if (Rev.y1) {
                if (ub[1] > 0) {
                    x1 <- x.y1[1]
                    xm1 <- paste0("(", x1, ")")
                }
                if (lb[1] < 0) {
                    x2 <- x.y1[2]
                    xm2 <- paste0("(", x2, ")")
                }
            } else {
                if (ub[1] > 0) {
                    x2 <- x.y1[1]
                    xm2 <- paste0("(", x2, ")")
                }
                if (lb[1] < 0) {
                    x1 <- x.y1[2]
                    xm1<- paste0("(", x1, ")")
                }
            }
        } else {
            ##dx.y1 <- as.character(body(str2fn(x.y1, "yy")))
          # Both lower bound [2023.4.25]
            ##if (dx.y1[1] %in% c("-","/") && grep("yy", dx.y1)==3) {
            if (Rev.y1) {
                x1 <- x.y1
                xm1 <- paste0("(", x1, ")")
            } else {
                x2 <- x.y1
                xm2 <- paste0("(", x2, ")")
            }
        }
    }

  # Find the marginal PDF of X: f(x) -------------
  # Integrate w.r.t. Y
    bby <- Integ.str(fun, "yy")
    if (length(bby) > 1) {
        ccy <- bby[2]
        if (ccy=="C") {
            ccy <- bby[3]
            if (bby[1]=="-") ccy <- paste0("-1*(", ccy, ")")
        }
    } else {ccy <- bby}
  # Definite Integral
    ddy <- paste(gsub("yy", ym2, ccy), "-(", gsub("yy", ym1, ccy), ")")
    cfx0 <- Ryacas::yac_str(paste0("Simplify(", ddy, ")"))

  # Take care of exp(-Inf) => 0 # [2023.5.3]
    if (grepl("Inf", cfx0)) {
        cfx0 <- exp2_0(cfx0, "xx")
    }

    cfx2 <- paste0(gsub("xx", "x", cfx0), ", for ", xrng)
    cfx <- paste0(gsub("x", "xx", xrf), cfx0, ", 0)")
    fx <- str2fn(cfx, "xx")

  # Find the marginal PDF of Y: f(y) -------------
  # Integrate w.r.t. X
    bbx <- Integ.str(fun, "xx")
    ccx <- bbx[2]
    if (ccx=="C") {
      ccx <- bbx[3]
      if (bbx[1]=="-") ccx <- paste0("-1*(", ccx, ")")
    }
  # Definite Integral
    ddx <- paste(gsub("xx", xm2, ccx), "-(", gsub("xx", xm1, ccx), ")")
    cfy0 <- Ryacas::yac_str(paste0("Simplify(", ddx, ")"))

  # Take care of exp(-Inf) => 0 # [2023.5.3]
    if (grepl("Inf", cfy0)) {
        cfy0 <- exp2_0(cfy0, "yy")
    }

    cfy2 <- paste0(gsub("yy", "y", cfy0), ", for ", yrng)
    cfy <- paste0(gsub("y", "yy", yrf), cfy0, ", 0)")
    fy <- str2fn(cfy, "yy")
    
    if (!missing(xp)) fxv <- fx(xp)
    if (!missing(yp)) fyv <- fy(yp)

  # Set the range of the joint PDF
    ## cxr <- paste0("(xx>", lb[1], ")&(xx<", ub[1], ")")
    # [2023.4.25]
    if (is.character(x1)) {
      xrng <- c(x1, ub[1])
    } else if (is.character(x2)) {
      xrng <- c(lb[1], x2)
    } else {
      xrng <- c(lb[1], ub[1])
    }
    cxr <- paste0("(xx>", xrng[1], ")&(xx<", xrng[2], ")")
    cxr2 <- paste0("(", lb[1], "<x<", ub[1], ")")

    if (!missing(y1)) {
      yrng <- c(y1, ub[2])
    } else if (!missing(y2)) {
      yrng <- c(lb[2], y2)
    } else {
      yrng <- c(lb[2], ub[2])
    }
    cyr <- paste0("(yy>", yrng[1], ")&(yy<", yrng[2], ")")
    cyr2 <- paste0("(", lb[2], "<y<", ub[2], ")")

  # Obtain the joint PDF
    cfxy <- paste0("ifelse(", cxr,"&", cyr, ",", fun, ", 0)")
    fxy <- str2fn(cfxy, "xx+yy")
    ## mosaicCore::makeFun(as.formula(paste(cfxy, "~ xx+yy")))

  # Print the PDF -----------------
    if (prt) {
      # Marginal PDF -------
        if (type=="m") {
            cat("f(x) =", cfx2, "\n")
            cat("f(y) =", cfy2, "\n")
        }
    }

  # Find the Conditional PDF -----------------------------
    if (type=="c") {
      # Obtain the conditional PDF
        fx_y <- function(xx, yy) fxy(xx, yy)/fy(yy)
        fy_x <- function(yy, xx) fxy(xx, yy)/fx(xx)

        if (!missing(xp)) {
            nn1 <- length(xp)
            ##fy_xp <- list()
            ##for (k in 1:nn1) fy_xp[[k]] <- 
            ##               function(yy) fxy(xp[k], yy)/fx(xp[k])
        }
        if (!missing(yp)) {
            nn2 <- length(yp)
            ##fx_yp <- list()
            ##for (k in 1:nn2) fx_yp[[k]] <- 
            ##               function(xx) fxy(xx, yp[k])/fy(yp[k])
        }

      # Print the PDF -----------------------------------
        if (prt) {
        # Conditional PDF --------------------
          cond.prt <- function(vv, cc) {
            vv1 <- vv
            vv2 <- ifelse(vv=="x", "xx", "yy")
            cc2 <- ifelse(vv=="x", "yy", "xx")
            cc1 <- ifelse(vv=="x", "y", "x")
            ## fnd <- ifelse(vv=="x", fy, fx)
            denc <- ifelse(vv=="x", cfy0, cfx0)

            numc <- gsub(vv2, vv1, fun)
            numc <- gsub(cc2, cc, numc)
            numc <- gsub(" ", "", numc)
            ## denc <- as.character(body(fnd))[3]
            denc <- gsub(cc2, cc, denc)
            denc <- gsub(" ", "", denc)

          # xrng: for vv="x", yrng: for vv="y"
            if (vv=="y") {
                head <- ifelse(!is.numeric(cc), paste0("f(y|x) = "),
                             paste0("f(y|x=", cc, ") = "))
                if (is.character(ym1)) {
                    yrng <- c(gsub(cc2,cc,y1), ub[2])
                } else if (is.character(ym2)) {
                    yrng <- c(lb[2], gsub(cc2,cc,y2))
                } else {
                    yrng <- c(lb[2], ub[2])
                }
                tail <- paste0(" for (", yrng[1],"<y<",yrng[2],")")
            }
            if (vv=="x") {
                head <- ifelse(!is.numeric(cc), paste0("f(x|y) = "),
                             paste0("f(x|y=", cc, ") = "))
                if (is.character(xm2)) {
                    xrng <- c(lb[1], gsub(cc2,cc,x2))
                } else if (is.character(xm1)) {
                    xrng <- c(gsub(cc2,cc,x1), ub[1])
                } else {
                    xrng <- c(lb[1], ub[1])
                }
                tail <- paste0(" for (", xrng[1],"<x<",xrng[2],")")
            }
            cat(paste0(head, "[", numc, "]/[", denc, "],", tail), "\n")
          } # End of cond.prt()

            cond.prt("x", "y")
            cond.prt("y", "x")

            if (type=="c" && !missing(xp)) {
                for (k in 1:nn1) cond.prt("y", xp[k])
            }
            if (type=="c" && !missing(yp)) {
                for (k in 1:nn2) cond.prt("x", yp[k])
            }
        } # End of if (prt)

    } # End of if (type=="c")

  # Independence of X and Y -----------------
    if (type=="i") {
      # Check the range of X and Y
        if (is.character(y1) && is.character(y2)) {
          cmt <- "Both limit of X is affected by Y. => X and Y are not independent."
        } else if (is.character(y1)) {
          cmt <- "Upper limit of X is affected by Y. => X and Y are not independent."
        } else if (is.character(y2)) {
          cmt <- "Lower limit of X is affected by Y. => X and Y are not independent."
        }

        xbind <- paste0("(", x1, "<x<", x2, ")")
        xbind <- gsub("yy", "y", xbind)
        ybind <- paste0("(", y1, "<y<", y2, ")")
        ybind <- gsub("xx", "x", ybind)

      # Take care of exp(-Inf) => 0
      #  aax <- as.character(body(fx))
      #  if (aax[1]=="ifelse") aax <- aax[3]
      #  bbx <- Deriv::Simplify(aax)
      #  ## bbx <- gsub(" exp\\(.+Inf.+\\)+", 0, bbx)

      #  aay <- as.character(body(fy))
      #  if (aay[1]=="ifelse") aay <- aay[3]
      #  bby <- Deriv::Simplify(aay)
      #  ## bby <- gsub(" exp\\(.+Inf.+\\)+", 0, bby)

      # Product f(x)*f(y)
      #  bbxy <- paste0("(", bbx, ")*(", bby, ")")
        bbxy <- paste0("(", cfx0, ")*(", cfy0, ")")
        ccxy <- gsub("exp(0)", "1", bbxy, fixed=TRUE)

      # Compare f(x,y) and f(x)*f(y)
        if (is.character(y1) || is.character(y2)) {
            res <- "f(x,y) != f(x)f(y) => X and Y are not Independent."
        } else {
          # Numeric Grid Check
            ## fx0 <- str2fn(aax, "xx")
            ## fy0 <- str2fn(aay, "yy")
            fx0 <- str2fn(cfx0, "x")
            fy0 <- str2fn(cfy0, "y")
            fxy0 <- str2fn(fun, "xx+yy")
            if (lb[1]==-Inf) lb[1] <- -100
            if (lb[2]==-Inf) lb[2] <- -100
            if (ub[1]== Inf) ub[1] <-  100
            if (ub[2]== Inf) ub[2] <-  100
            xa <- seq(lb[1], ub[1], length=100)
            ya <- seq(lb[2], ub[2], length=100)
            xya <- expand.grid(xa, ya, KEEP.OUT.ATTRS = FALSE)
            epps <- 1e-6
            gapfn <- function(rr) fxy(rr[1],rr[2])-fx(rr[1])*fy(rr[2])
            gap <- apply(xya, 1, gapfn)
            gap.max <- max(gap)
            ## cmt <- paste("max|f(x,y)-f(x)f(y)| =", gap.max)
            if (gap.max < epps) {
                res <- "f(x,y) = f(x)f(y) => X and Y are Independent."          
            } else {
                res <- "f(x,y) != f(x)f(y) => X and Y are not independent."
            }
        }

      # Print joint PDF and the difference
        fun2 <- gsub("xx", "x", fun)
        fun2 <- gsub("yy", "y", fun2)
        ccxy2 <- gsub("xx", "x", ccxy)
        ccxy2 <- gsub("yy", "y", ccxy2)
        ccxy2 <- gsub(" ", "", ccxy2)
        cat("----- Check Independence of X and Y -----\n")
        cat(paste0("f(x,y) = ", fun2, ", for ", xbind,"&",ybind), "\n")
        cat(paste0("f(x)f(y) = ", ccxy2), "\n")
        cat(res, "\n")
    } # End of if (type=="i")
 
  # Plot the PDF --------------------------------------
    # Plot marginal PDF f(x) and f(y) ----------------
    if (is.numeric(ws) && type=="m") {
      # Set up the plot
        ## if (missing(xl)) stop("Input xl: the limit of X for plot...")
        ## if (missing(yl)) stop("Input yl: the limit of Y for plot...")
      # Set x-axis (range of X)
        if (missing(xl)) {
            prng <- plot.rng(cfx, "xx", lb[1], ub[1])
            xlb <- prng[1]
            xub <- prng[2]
            ## if (lb[1]==-Inf) xlb <- -3*max(xp) else xlb <- lb[1]
            ## if (ub[1]== Inf) xub <- 3*max(xp) else xub <- ub[1]
        } else {
            xlb <- xl[1]
            xub <- xl[2]
        }
      # Set x-axis (range of Y)
        if (missing(yl)) {
            prng <- plot.rng(cfy, "yy", lb[2], ub[2])
            ylb <- prng[1]
            yub <- prng[2]
            ## if (lb[2]==-Inf) ylb <- -3*max(yp) else ylb <- lb[2]
            ## if (ub[2]== Inf) yub <- 3*max(yp) else yub <- ub[2]
        } else {
            ylb <- yl[1]
            yub <- yub
        }

        xa <- seq(xlb, xub, length=400)
        ya <- seq(ylb, yub, length=400)
 
        win.graph(ws[1], ws[2])
        par(mfrow=c(2, 1))
        par(mar=c(2.2,4,3,2))

      # Plot marginal PDF ----------------
        plot(xa, fx(xa), type="l", ylim=c(0, max(fx(xa))*1.1), 
            main="Marginal PDF of X",
            xlab="", ylab="f(x)", lwd=3, col="red")
        if (!missing(xp)) {
            segments(xlb+0.07*(xub-xlb), fxv, xp, fxv, lty=2, col="blue")
            segments(xp, 0, xp, fxv, lty=2, col="blue")
            text(rep(xlb+0.02*(xub-xlb), length(xp)), fxv, 
                round(fxv, dig), cex=0.8, col="blue")
        }

        plot(ya, fy(ya), type="l", ylim=c(0, max(fy(ya))*1.1), 
             main="Marginal PDF of Y", 
             pch=16, lwd=3, xlab="", ylab="f(y)", col="red")
        if (!missing(yp)) {
            segments(ylb+0.07*(yub-ylb), fyv, yp, fyv, lty=2, col="blue")
            segments(yp, 0, yp, fyv, lty=2, col="blue")
            text(rep(ylb+0.02*(yub-ylb), length(yp)), fyv, 
                round(fyv, dig), cex=0.8, col="blue")
        }
    } # End of if (type=="m")

    # Plot marginal PDF f(x) and f(y) ----------------
    if (is.numeric(ws) && type=="c") {
        if (!missing(xp)) {
          # Set x-axis (range of Y)
            if (missing(xl)) {
                if (lb[2]==-Inf) xlb <- -3*max(xp) else xlb <- lb[2]
                if (ub[2]== Inf) xub <- 3*max(xp) else xub <- ub[2]
            } else {
                xlb <- xl[1]
                xub <- xl[2]
            }
            mt <- paste0("Cond. PDF of Y | X=", xp)
            yll <- paste0("f(y|x=", xp, ")")

            xdel <- (xub - xlb)*0.2
            xlow <- xlb - xdel
            xupp <- xub + xdel
            xa <- c(xlb, seq(xlow, xupp, length=200), xub)

          # Plot conditional PDF f(x|yp) or f(y|xp)
            dr <- switch(nn1, 1, 1, 1, 2, 2, 2, 3, 3, 3)
            dc <- switch(nn1, 1, 2, 3, 2, 3, 3, 3, 3, 3)
            win.graph(ws[1], ws[2])
            par(mfrow=c(dr,dc))
            par(mar=c(2.5, 4, 3, 2))

            ymax <- max(fy_x(xa,xp[1]), fy_x(xa,xp[nn1]))
            for (kk in 1:nn1) {
                plot(xa, fy_x(xa,xp[kk]), type="l", ylim=c(0, ymax*1.1), 
                    main=mt[kk], xlab="", ylab=yll[kk], lwd=3, col="red")
                grid(col=3)
            }
        } # End of if (!missing(xp))

        if (!missing(yp)) {
          # Set x-axis (range of X)
            if (missing(yl)) {
                if (lb[1]==-Inf) xlb <- -3*max(yp) else xlb <- lb[1]
                if (ub[1]== Inf) xub <- 3*max(yp) else xub <- ub[1]
            } else {
                xlb <- yl[1]
                xub <- yl[2]
            }
            mt <- paste0("Cond. PDF of X | Y=", yp)
            yll <- paste0("f(x|y=", yp, ")")
 
            xdel <- (xub - xlb)*0.2
            xlow <- xlb - xdel
            xupp <- xub + xdel
            xa <- c(xlb, seq(xlow, xupp, length=200), xub)

          # Plot conditional PDF f(x|yp) or f(y|xp)
            dr <- switch(nn2, 1, 1, 1, 2, 2, 2, 3, 3, 3)
            dc <- switch(nn2, 1, 2, 3, 2, 3, 3, 3, 3, 3)
            win.graph(ws[1], ws[2])
            par(mfrow=c(dr,dc))
            par(mar=c(2.5, 4, 3, 2))

            ymax <- max(fx_y(xa, yp[1]), fx_y(xa, yp[nn2]))
            for (kk in 1:nn2) {
                plot(xa, fx_y(xa, yp[kk]), type="l", ylim=c(0, ymax*1.1), 
                    main=mt[kk], xlab="", ylab=yll[kk], lwd=3, col="red")
                grid(col=3)
            }
        } # End of if (!missing(yp))
    } # End of if (type=="c")


    # Return PDF and Probabilities ------------------------
    out <- list(fx=fx, fy=fy)
    if (type=="m") {
        if (!missing(xp)) out$fxv <- fxv
        if (!missing(yp)) out$fyv <- fyv
    } else if (type=="c") {
        out$fx_y <- fx_y
        out$fy_x <- fy_x
        ## if (!missing(yp)) out$fx_yp <- fx_yp
        ## if (!missing(xp)) out$fy_xp <- fy_xp
    }
    invisible(out)
}

# [4-10] Transformation of a Continuous Random Variable
#' @title Transformed PDF of a Continuous Random Variable
#' @description Transformed PDF of a Continuous Random Variable.
#' @param fx PDF of X as string "f(x)".
#' @param flim range of X for f(x) > 0.
#' @param tf Character vector of transformation functions (up to 8).
#' @param a Lower limit for calculating P(a<X<b).
#' @param b Upper limit for calculating P(a<X<b).
#' @param prt Logical: print the output? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Window size of the plot, Default: "n".
#' @param xlim range of X for the plot.
#' @param ... Other graphic parameters for text.
#' @return Transformed PDFs.
#' @examples
#' # Linear Transformation
#' TR <- c(y="10*x-4", w="-10*x+4")
#' cont.trans(fx="dnorm(x)", tf=TR, a=-1, b=1)
#' cont.trans(fx="dnorm(x)", tf=TR, a=-1, b=1, prt=FALSE, ws=c(9,3))
#' cont.trans("(x+1)/2", c(-1,1), TR, a=-0.3, b=0.7, ws=c(9,3))
#' # Non-linear Transformation
#' cont.trans(fx="dnorm(x)", tf=c(y="x^2",w="exp(x)"), a=-1, b=1, ws=c(9,3))
#' TR <- c(y="log(x+2)", w="x^2")
#' cont.trans("(x+1)/2", c(-1,1), TR, a=-0.3, b=0.7, ws=c(9,3))
#'
#' @rdname cont.trans
#' @export
# [Main] -----------------------------------------
cont.trans <- function(fx, flim, tf, a, b, prt=TRUE, dig=4, ws="n", xlim, ...) {
  # Check input
    if (missing(fx)) stop("Input the PDF of X as string \"f(x)\"...")
    if (missing(flim)) flim <- c(-Inf, Inf)
    if (missing(tf)) stop("Input the transformation functions...")
    debug10 <- FALSE
    ## debug10 <- TRUE

    if (missing(a) && missing(b)) {
        job <- "F"  ## function only
        a <- flim[1]
        b <- flim[2]
    } else if (missing(a) && !missing(b)) {
        job <- "C"  ## CDF: P(X<b)
        a <- flim[1]
    } else if (!missing(a) && missing(b)) {
        job <- "R"  ## 1-CDF: P(X>a)=1-P(X<a)
        b <- flim[2]
    } else {
        job <- "I"  ## P(a<X<b)
    }

  # Number of transformed variable
    N <- length(tf)
  # Transformed variable names
    if (is.null(names(tf))) {
        vn <- paste0("t", 1:N)
        Vn <- paste0("T", 1:N)
    } else {
        vn <- names(tf)
        vn <- tolower(vn)
        Vn <- toupper(vn)
    }
  # Capitalize "X" [2023.4.19]
    fx <- gsub("x", "X", fx)
    fx <- gsub("eXp", "exp", fx)
    for (k in 1:N) {
        tf[k] <- gsub("x", "X", tf[k])
        tf[k] <- gsub("eXp", "exp", tf[k])
    }

  # Set up the domain of the original PDF
    if (flim[1]==-Inf && flim[2]==Inf) {
        cfx <- fx
    } else if (flim[2]==Inf) {
        cfx <- paste0("ifelse(X>",flim[1],",",fx,",0)")
    } else if (flim[1]==-Inf) {
        cfx <- paste0("ifelse(X<",flim[2],",",fx,",0)")
    } else {
        cfx <- paste0("ifelse(X>",flim[1]," & X<",flim[2],",",fx,",0)")
    }
    if (debug10) cat("cfx:", cfx, "\n") ## [1]
    FnX <- str2fn(cfx, "X")

  # Calculate probability
    px <- integrate(FnX, a, b)[[1]]

  # Create Transformation Functions
    TF <- list()
    for (k in 1:N) {
        TF[[k]] <- str2fn(tf[k], "X")
    }

  # Obtain Transformed PDFs -------------------------

  # Obtain Inverse TR functions
    ftf <- itf <- itf2 <- jac <- Gcdf <- rep(NA, N)

    for (k in 1:N) {
        Gcdf[k] <- TRUE
        ###try.int <- Int0.str(fx, "X")
        ###if (length(try.int) > 1) Gcdf[k] <- FALSE

        temp <- myInv.str(tf[k], vn[k])
        itf[k] <- Sim.str(temp[1])
        ## print(tf[k]); print(vn[k]); print(temp)
        if (debug10) cat(paste0("itf[",k,"]=", itf[k]), "\n") ## [2]

      # Duplicate TR
        if (length(temp) > 1) {
            Gcdf[k] <- FALSE
            ## cat(paste0("temp[1]: ", temp[1], "  temp[2]: ", temp[2]), "\n") ##
            itf2[k] <- sub(temp[2], paste0("-",temp[2]), temp[1], fixed=TRUE)
            if (!is.na(itf2[k])) itf2[k] <- Sim.str(itf2[k])
            if (debug10) cat(paste0("itf2[", k, "]=", itf2[k]), "\n") ## [2b]
        }

      # Jacobian
        jac[k] <- D.str(itf[k], vn[k])
        jac[k] <- sub("^-", "", jac[k])
        if (debug10) cat(paste0("jac[",k,"]=", jac[k]), "\n") ## [3]
        ### jac[k] <- paste0("abs(", jac[k], ")")
    }

  # Transformed PDF
    MFTF1 <- MFTF2 <- FTF <- list()
    cdftf <- cftf <- tlo <- tup <- xcut <- vector()
    mftf1 <- mftf2 <- cftf2 <- clim <- rep(NA, N)

    check <- flim
    trng.prt <- vector()
    mainf <- subf <- maincdf <- subcdf <- vector()

    for (k in 1:N) {
        frng <- find.rng(TF[[k]], flim)
        tlo[k] <- frng[1]
        tup[k] <- frng[2]
        xcut[k] <- frng[3]
        clim[k] <- frng[4]
        if (debug10) cat(paste0("(k=",k, ") frng:"), frng, "\n") ## [4]
        trng.prt[k] <- paste0(", for (", round(frng[1],4), "<", 
                    vn[k], "<", round(frng[2],4), ")")
        if (debug10) cat(paste0("trng.prt[",k,"]:"), trng.prt[k], "\n") ## [5]

       # Functional string
        tr.body <- gsub("X", paste0("(",itf[k],")"), fx)

        cftf[k] <- paste0(tr.body,"*",Sim.str(jac[k]))
        if (debug10) cat(paste0("cftf[",k,"]: ", cftf[k]), "\n") ## [6]
        if (Gcdf[k]) {
            cdftf[k] <- Tr.cdf(fx, flim[1], itf[k], "X", vn[k])
            if (debug10) cat(paste0("cdftf[",k,"]: ", cftf[k]), "\n") ## [6b]
        }

       # Handle Duplicate TR
        if (!is.na(itf2[k]) && flim[1]<xcut[k] && flim[2]>xcut[k]) {
            if (debug10) cat("flim: ", flim, "\n") ## [7]
            if (debug10) cat(paste0("xcut[",k,"] = ", xcut[k]), "\n") ## [7b]
            ## cftf2[k] <- gsub("sqrt\\(", "-sqrt\\(", itf[k])
            tr2.body <- gsub("X", paste0("(",itf2[k],")"), fx)

            cftf2[k] <- paste0(tr2.body,"*",jac[k])
            if (debug10) cat(paste0("cftf2[",k,"]: ", cftf2[k]), "\n") ## [6c]
        } else { cftf2[k] <- NA }

       # Transformed PDF
        if (is.na(cftf2[k])) {
          ftf[k] <- paste0("ifelse(", vn[k],">",round(tlo[k],7),
                  "&", vn[k], "<",round(tup[k],7), "," ,cftf[k], ",0)")
          if (debug10) cat(paste0("ftf[",k,"]:"), ftf[k], "\n") ## [7]
          FTF[[k]] <- str2fn(ftf[k], vn[k])
        } else {
       # Multiple TR Case
          dist1 <- round(xcut[k],4) - flim[1]
          dist2 <- flim[2] - round(xcut[k],4)

          if (dist1 <= dist2) {
              mainf[k] <- paste0(Sim2.str(tr.body,vn[k]), "*", Sim.str(jac[k]))
              subf[k] <- paste0(Sim2.str(tr2.body,vn[k]), "*", Sim.str(jac[k]))
              ## if (grepl("not", mainf[k])) mainf[k] <- cftf[k]
              ## if (grepl("not", subf[k]))subf[k] <- cftf2[k]
          } else {
              mainf[k] <- cftf2[k]
              subf[k] <- cftf[k]  ## correction [23.5.4]
              ##mainf[k] <- paste0(Sim2.str(tr2.body,vn[k]), "*", Sim.str(jac[k]))
              ##subf[k] <- paste0(Sim2.str(tr.body,vn[k]), "*", Sim.str(jac[k]))
          }
          if (debug10) cat(paste0("mainf[",k,"]:"), mainf[k], "\n") ## [8]
          if (debug10) cat(paste0("subf[",k,"]:"), subf[k], "\n") ## [8b]

          mftf1[k] <- paste0("ifelse(", vn[k],">",round(tlo[k],7),
                  "&", vn[k], "<",round(tup[k],7), ", " ,mainf[k], ", 0)")
          if (debug10) cat(paste0("mftf1[",k,"]:"), mftf1[k], "\n") ## [9]
          MFTF1[[k]] <- str2fn(mftf1[k], vn[k])
 
          mftf2[k] <- paste0("ifelse(", vn[k],">",round(tlo[k],7),
                  "&", vn[k], "<",round(clim[k],7), ", " ,subf[k], ", 0)")
          if (debug10) cat(paste0("mftf2[",k,"]:"), mftf2[k], "\n") ## [9b]
          MFTF2[[k]] <- str2fn(mftf2[k], vn[k])

          ftf[k] <- paste0(mftf1[k], "+", mftf2[k])
          if (debug10) cat(paste0("ftf[",k,"]:"), ftf[k], "\n") ## [10]
          ## FTF[[k]] <- mosaicCore::makeFun(
          ##            as.formula(paste(ftf[k], "~", vn[k])))
          FTF[[k]] <- str2fn(ftf[k], vn[k])
        } # End of if
    }
    
  # Calculate Transformed Probabilities
    py <- py1 <- py2 <- a2 <- b2 <- c2<- vector()
    for (k in 1:N) {
        abrng <- find.rng(TF[[k]], c(a,b))
        a2[k] <- abrng[1]
        b2[k] <- abrng[2]
        c2[k] <- abrng[4]
        py[k] <- integrate(FTF[[k]], a2[k], b2[k])[[1]]
        ## cat("abrng:", abrng, "\n") ##

        if (!is.na(cftf2[k])) {
            py1[k] <- integrate(MFTF1[[k]], a2[k], b2[k])[[1]]
            py2[k] <- integrate(MFTF2[[k]], a2[k], c2[k])[[1]]
            py[k] <- py1[k] + py2[k]
        }
    }

   # Print
    if (prt) {
        if (job == "I") {
            pr.x <- paste0("Pr(", a, " < X < ", b, ") =")
        } else if (job=="C") {
            pr.x <- paste0("F(", b, ") = Pr(", "X < ", b, ") =")
        } else if (job=="R") {
            pr.x <- paste0("1-F(", a, ") = Pr(", a, " < X) =")
        } 
        if (job != "F") cat(pr.x, round(px, dig), "\n")

        for (k in 1:N) {
            if (is.na(cftf2[k])) {
                if (Gcdf[k]) {
                    cat(paste0("x=", itf[k], "  =>  ",
                      "f(",vn[k],")=", cdftf[k], trng.prt[k]), "\n")
                } else {
                     cat(paste0("x=", itf[k], "  =>  ",
                      "f(",vn[k],")=", cftf[k], trng.prt[k]), "\n")
                }

                if (job == "I") {
                    cat(paste0("Pr(",round(a2[k],dig)," < ",Vn[k]," < ",
                      round(b2[k],dig),") ="), round(py[k],dig), "\n")
                } else if (job == "C") {
                    cat(paste0("Pr(",Vn[k]," < ",
                      round(b2[k],dig),") ="), round(py[k],dig), "\n")
                } else if (job == "R") {
                    cat(paste0("Pr(",round(a2[k],dig)," < ",Vn[k],
                      ") ="), round(py[k],dig), "\n")
                }
            } else {
                cat(paste0("x=", itf[k], "  =>  ", 
                   "f(",vn[k], ")=", mainf[k], ", for (",
                   round(tlo[k],4),"<",vn[k],"<",round(tup[k],4),")"),"\n")
                cat(paste0("x=", itf2[k], "  =>  ", 
                   "f(",vn[k],")=", subf[k], ", for (",
                   round(tlo[k],4),"<",vn[k],"<",round(clim[k],4),")"),"\n")

                if (job == "I") {
                    cat(paste0("Pr(",round(a2[k],dig)," < ",Vn[k]," < ",
                      round(b2[k],dig),") ="), round(py1[k],dig), "+", 
                      round(py2[k],dig), "=", round(py[k],dig), "\n")
                } else if (job == "C") {
                    cat(paste0("Pr(",Vn[k]," < ",
                      round(b2[k],dig),") ="), round(py1[k],dig), "+", 
                      round(py2[k],dig), "=", round(py[k],dig), "\n")
                } else if (job == "R") {
                    cat(paste0("Pr(",round(a2[k],dig)," < ",Vn[k],
                      ") ="), round(py1[k],dig), "+", 
                      round(py2[k],dig), "=", round(py[k],dig), "\n")
              }
            }
        } # End of for
    } # End of if (prt)

   # Plot ---(N <= 8) --------------------------------------
    if (is.numeric(ws)) {
        dr <- switch(N, 1,1, 2,2,2, 3,3,3, 3,3,3, 4,4,4,4, 4,4,4,4,4)
        dc <- switch(N, 2,3, 2,3,3, 3,3,3, 4,4,4, 4,4,4,4, 5,5,5,5,5)
        win.graph(ws[1], ws[2])
        par(mfrow=c(dr, dc))
        par(mar=c(3,4,3,2))

       # Display the pdf of X and P(a<X<b)
        if (missing(xlim)) {
            lo <- a - (b-a)
            up <- b + (b-a)
        } else {
            lo <- xlim[1]
            up <- xlim[2]
        }
        x1 <- lo - 0.2*(up-lo)
        x2 <- up + 0.2*(up-lo)
        k1 <- x1*200
        k2 <- x2*200
        xm <- (a+b)/2
        ym <- FnX(xm)*0.5
        xa <- k1:k2/200
      # Suppress Warnings (NaN) in Plotting ----------
      suppressWarnings({
        plot(xa, FnX(xa), type="n", las=1, ylab="f(x)", xlab="", main="pdf of X")
        cord.x <- c(a, seq(a, b, 0.01), b)
        cord.y <- c(0, FnX(seq(a, b, 0.01)), 0)
        polygon(cord.x, cord.y, col='lightcyan')
        text(xm, ym, labels=paste0("P(",a,"<X<",b,")\n=",round(px, 4)), ...)
        lines(xa, FnX(xa), lwd=2, col="red")

        # Display the pdf of T1, T2, ..., TN and P(a2<Ti<b2)
        for (k in 1:N) {
            ## a2 <- min(TF[[k]](c(a,b)))
            ## b2 <- max(TF[[k]](c(a,b)))
            ## lo2 <- min(TF[[k]](c(lo,up)))
            ## up2 <- max(TF[[k]](c(lo,up)))
            lo2 <- a2[k] - (b2[k]-a2[k])
            up2 <- b2[k] + (b2[k]-a2[k])
           # Set plot range
            x1 <- lo2 - 0.2*(up2-lo2)
            x2 <- up2 + 0.2*(up2-lo2)
            k1 <- x1*200
            k2 <- x2*200
            xm <- (a2[k]+b2[k])/2
            ym <- FTF[[k]](xm)*0.5
            xa <- k1:k2/200

           # Plot the pdf and the probabilities
            if (is.na(cftf2[k])) {
                plot(xa, FTF[[k]](xa), type="n", las=1, xlab="", 
                  ylab=paste0("f(",vn[k],")"), main=paste0("pdf of ",Vn[k]))
                cord.x <- c(a2[k], seq(a2[k], b2[k], 0.01), b2[k])
                cord.y <- c(0, FTF[[k]](seq(a2[k], b2[k], 0.01)), 0)
                polygon(cord.x, cord.y, col='lightcyan')
                text(xm, ym, labels=paste0("P(",round(a2[k],4),"<", Vn[k],
                      "<",round(b2[k],4),")\n=",round(py[k],4)), ...)
                lines(xa, FTF[[k]](xa), lwd=2, col="red")
             } else {
                plot(xa, FTF[[k]](xa), type="n", las=1, xlab="", ylim=c(0,4*ym),
                  ylab=paste0("f(",vn[k],")"), main=paste0("pdf of ",Vn[k]))
               # main polygon
                cord.x <- c(a2[k], seq(a2[k], b2[k], 0.01), b2[k])
                cord.y <- c(0, MFTF1[[k]](seq(a2[k], b2[k], 0.01)), 0)
                polygon(cord.x, cord.y, col="lightcyan")
               # sub polygon
                cord.x <- c(a2[k], seq(a2[k], c2[k], 0.01), c2[k])
                cord.y <- c(0, FTF[[k]](seq(a2[k], c2[k], 0.01)), 0)
                polygon(cord.x, cord.y, col="yellow")
               # text
                ym1 <- MFTF1[[k]](xm)*0.5
                text(xm, ym1, labels=round(py1[k],4), ...)
                ym2 <- MFTF1[[k]](xm) + MFTF2[[k]](xm)*0.8
                text(xm, ym2, labels=round(py2[k],4), ...)
               # total
                text(xm, ym, labels=paste0("P(",round(a2[k],4),"<", Vn[k],
                      "<",round(b2[k],4),")\n=",round(py[k],4)), ...)
                lines(xa, MFTF1[[k]](xa), lwd=1, col="blue")
                lines(xa, FTF[[k]](xa), lwd=2, col="red")
            }
        } # End of for
      }) # End of suppressWarnings
    } # End of plot
    invisible(FTF)
}

# [Utility functions] -----------------------------------------
# Function to make a function from strings [2023.4.17]
str2fn <- function(body, vars) {
    mosaicCore::makeFun(as.formula(paste(body, "~", vars)))
}

# Function to find range of TF [2023.4.18]
find.rng <- function(fun, rng) {
    if (is.character(fun)) fun <- str2fn(fun, "X")

    tlim <- sort(fun(rng))
    ## print(tlim) ##
    dfun <- D(body(fun), "X")
    ## print(dfun) ##
    ddfun <- D(dfun, "X")
    ## print(ddfun) ##

    if (ddfun==0) {
       # Linear TRF
        res <- c(tlim, Inf)
    } else {
        dfn <- D.fn(deparse(body(fun)), "X")
        ## print(dfn) ##
        dlim <- sort(dfn(rng))
        ## print(dlim) ##

        if (dlim[1]<0 && dlim[2]>0) {
            eqn <- paste0(deparse(dfun), "== 0")
            ## print(eqn) ##
            sol <- round(as.numeric(Sol.str(eqn, "X")), 10)
            ## print(sol) ##
            obj <- fun(sol)
            ## print(obj) ##
            ycut <- min(fun(rng))
            res <- c(min(tlim, obj), max(tlim, obj), sol, ycut)
        } else {
           # Monotone TRF
            ycut <- min(fun(rng))
            res <- c(tlim, Inf, ycut)
        }
    }
    return(res)
}

# Functional integration (string => string)
Integ.str <- function(fun, var) {
    bfn <- c("dnorm", "dt(", "df(", "dexp", "dgamma", "dweibull", "dchisq")
    pfn <- c("pnorm", "pt(", "pf(", "pexp", "pgamma", "pweibull", "pchisq")
    check <- sapply(bfn, function(ss) grepl(ss, fun, fixed=TRUE))
    builtin <- ifelse(any(check), TRUE, FALSE)
    if (builtin) {
        bfn0 <- bfn[check]
        pfn0 <- pfn[check]
        out <- sub(bfn0, pfn0, fun, fixed=TRUE)
    } else {
        intfn <- mosaicCalc::antiD(as.formula(paste(fun, "~", var)))
        out <- as.character(body(intfn))
    }
    return(out)
}

## Modified [2023.5.20]
Int0.str <- function(fun, var) {
    bfn <- c("dnorm", "dt(", "df(", "dexp", "dgamma", "dweibull", "dchisq")
    pfn <- c("pnorm", "pt(", "pf(", "pexp", "pgamma", "pweibull", "pchisq")
    check <- sapply(bfn, function(ss) grepl(ss, fun, fixed=TRUE))
    builtin <- ifelse(any(check), TRUE, FALSE)

    if (builtin) {
        bfn0 <- bfn[check]
        pfn0 <- pfn[check]
        out <- sub(bfn0, pfn0, fun, fixed=TRUE)
    } else {
      # Additional check
        intfn <- mosaicCalc::antiD(as.formula(paste(fun, "~", var)))
        out0 <- deparse(body(intfn))
        if (length(out0)>3 && out0[1] == "{") {
            out <- NA
        } else {
            out <- paste(out0, collapse="")
            out <- gsub("C", 0, out)
            out <- Ryacas::yac(paste0("Simplify(", out, ")"))
            out <- gsub(" ", "", out)
            out <- gsub("\\(-(\\d)\\)", "-\\1", out)
        }
    }
    return(out)
}

# Numerical Integration using String function
Int.str <- function(cfx, vn, a, b) {
   # Create function
    fnx <- str2fn(cfx, vn)
   # Integration
    integrate(fnx, a, b)[[1]]
}

# String function => Derivative (string)
D.str <- function(body, var) {
    expr <- deparse(D(parse(text=body), var))
   # Decompose and Merge Power
    part <- stringr::str_split_1(expr, " \\* ")
    if (length(part)==1) {
        part1 <- part
    } else {
        nn <- grep("[A-z]", part)
        part1 <- part[nn]
        part2 <- ifelse(nn==1, paste0(part[2],"*"), paste(part[1],"*"))
    }
    if (grepl("\\^\\(.+\\)$", part1)) {
        pwr <- sub("^.+\\^", "", part1)
        pwr <- Rmpar.str(pwr)
        pwr <- sub(".*\\^\\((.+)\\).*", "\\1", pwr)
        base <- sub("\\^\\(.+\\)$", "", part1)
        pwr <- eval(parse(text=pwr))
        opwr <- MASS::fractions(pwr)
        expr1 <- paste0(base, "^(", opwr, ")")
        expr <- paste0(part2, expr1)
    }
    ## sub(paste0("(",var,")"), var, expr, fixed=TRUE)
    expr
}
# String function => Derivative (string) [2023.5.20]
D0.fn <- function(fun, vn) {
    if (!is.character(vn)) stop("Input var name as string...")

    if (is.function(fun)) {
        out <- D(body(fun), vn)
    } else if (is.expression(fun)||is.language(fun)) {
        out <- D(fun, vn)
    } else if (is.character(fun)) {
        out <- D(parse(text=fun), vn)
    }
    out <- deparse(out)
    out <- paste(out, collapse="")
    out <- gsub(" ", "", out)
    gsub("\\(-1\\)", "-1", out)
}

# String function => Derivative (function)
D.fn <- function(expr, vn) {
    if (!is.character(vn)) stop("Input var name as string...")

    if (is.function(expr)) {
        res <- D(body(expr), vn)
    } else if (is.expression(expr)||is.language(expr)) {
        res <- D(expr, vn)
    } else if (is.character(expr)) {
        res <- D(parse(text=expr), vn)
    }
    str2fn(deparse(res), vn)
}

# R => Yacas
r2yac <- function(eq) {
    eq <- gsub("log\\(", "Ln\\(", eq)
    eq <- gsub("asin\\(", "ArcSin\\(", eq)
    eq <- gsub("acos\\(", "ArcCos\\(", eq)
    eq <- gsub("atan\\(", "ArcTan\\(", eq)
    eq <- gsub("sqrt\\(", "Sqrt\\(", eq)
    eq <- gsub("exp\\(", "Exp\\(", eq)
    eq <- gsub("abs\\(", "Abs\\(", eq)
    eq <- gsub("sin\\(", "Sin\\(", eq)
    eq <- gsub("cos\\(", "Cos\\(", eq)
    eq <- gsub("tan\\(", "Tan\\(", eq)
}
# Yacas => R
yac2r <- function(eq) {
    eq <- gsub("Ln\\(", "log\\(", eq)
    eq <- gsub("ArcSin\\(", "asin\\(", eq)
    eq <- gsub("ArcCos\\(", "acos\\(", eq)
    eq <- gsub("ArcTan\\(", "atan\\(", eq)
    eq <- tolower(eq)
}

# String equation => Inverse equation (string)
Sol.str <- function(eqn, var, num=FALSE) {
    eqn <- r2yac(eqn)
    res <- Ryacas::yac(paste0("Solve(", eqn, ",", var, ")"))
    res <- sub(".*==", "", res)
    res <- sub("\\}$", "", res)
    if (grepl("[A-z]", res)) {
        res <- yac2r(res)
    }
    if (num) res <- eval(parse(text=res))
    res
}

# String expression => Simple expression (string)
Sim.str <- function(expr) {
    out <- Ryacas::yac(paste0("Simplify(", expr, ")"))
    out <- gsub(" ", "", tolower(out))
    out <- gsub("inf", "Inf", out)
    gsub("\\(-1\\)", "-1", out)
}

# Simplify conditional expressions [2023.5.8]
Sim2.str <- function(expr, var) {
    ## out <- Ryacas::yac(paste0("Simplify(", expr, ")"))
    out <- Deriv::Simplify(expr)
    out <- gsub(" ", "", tolower(out))
    ## cat("out =", out, "\n") ## [1]

    Tn.reg <- c(
        "-sqrt(\\w)<0", "-sqrt(\\w)<=0", "0>-sqrt(\\w)", "0>=-sqrt(\\w)",
        "-\\w^(1/4)<0", "-\\w^(1/4)<=0", "0>-\\w^(1/4)", "0>=-\\w^(1/4)",
        "-exp(\\w)<0", "-exp(\\w)<=0",   "0>-exp(\\w)", "0>=-exp(\\w)")
    Tp.reg <- c(
        "sqrt(\\w)>0", "sqrt(\\w)>=0",   "0<sqrt(\\w)", "0<=sqrt(\\w)",
        "\\w^(1/4)>0", "\\w^(1/4)>=0",   "0<\\w^(1/4)", "0<=\\w^(1/4)",
        "exp(\\w)>0", "exp(\\w)>=0",     "0<exp(\\w)", "0<=exp(\\w)")
    Tn.text <- gsub("\\w", var, Tn.reg, fixed=TRUE)
    Tp.text <- gsub("\\w", var, Tp.reg, fixed=TRUE)

    Fn.reg <- c(
        "-sqrt(\\w)>0", "-sqrt(\\w)>=0", "0<-sqrt(\\w)", "0<=-sqrt(\\w)",
        "-\\w^(1/4)>0", "-\\w^(1/4)>=0", "0<-\\w^(1/4)", "0<=-\\w^(1/4)",
        "-exp(\\w)>0", "-exp(\\w)>=0",   "0<-exp(\\w)", "0<=-exp(\\w)")
    Fp.reg <- c(
        "sqrt(\\w)<0", "sqrt(\\w)<=0",   "0>sqrt(\\w)", "0>=sqrt(\\w)",
        "\\w^(1/4)<0", "\\w^(1/4)<=0",   "0>\\w^(1/4)", "0>=\\w^(1/4)",
        "exp(\\w)<0", "exp(\\w)<=0",     "0>exp(\\w)", "0>=exp(\\w)")
    Fn.text <- gsub("\\w", var, Fn.reg, fixed=TRUE)
    Fp.text <- gsub("\\w", var, Fp.reg, fixed=TRUE)

  # Simplify ifelse(logic, body1, body2)
    if (grepl("^ifelse", out)) {
        dd <- Decomp.str(out, var)
        dd <- gsub(" ", "", dd)
        ## cat("dd =", dd, "\n") ## [2]
        logic <- dd[2]

        if (logic %in% c(Tn.text, Tp.text)) {
            out <- dd[3]
        } else if (logic %in% c(Fn.text, Fp.text)) {
            out <- dd[4]
        }
        ## cat("[3] out =", out, "\n") ## [3]
    } else {
        for (k in 1:length(Tn.text)) {
            out <- gsub(Tn.text[k], 1, out, fixed=TRUE)
        }
        ## cat("[Tn] out =", out, "\n") ## [4]
        for (k in 1:length(Fn.text)) {
            out <- gsub(Fn.text[k], 0, out, fixed=TRUE)
        }
        ## cat("[Fn] out =", out, "\n") ## [5]

        for (k in 1:length(Tp.text)) {
            out <- gsub(Tp.text[k], 1, out, fixed=TRUE)
        }
        ## cat("[Tp] out =", out, "\n") ## [6]
        for (k in 1:length(Fp.text)) {
            out <- gsub(Fp.text[k], 0, out, fixed=TRUE)
        }
        ## cat("[Fp] out =", out, "\n") ## [7]
        out <- Deriv::Simplify(out)
        out <- gsub(" ", "", out)
    }
    ## out <- gsub(" ", "", tolower(out))
    gsub("\\(-1\\)", "-1", out)
}

# String expression => Factor expression (string)
Fac.str <- function(expr) {
    Ryacas::yac(paste0("Factor(", expr, ")"))
}

# String expression => Extract str from (str)
Rmpar.str <- function(expr) {
    if (length(expr)>1) stop(paste("Error: expr =", expr))
    out <- expr
    if (grepl("^\\(", expr) && grepl("\\)$", expr)) {
        out <- sub("^\\(", "", out)
        out <- sub("\\)$", "", out)
        out <- Rmpar.str(out)
    }
    return(out)
}

# String expression of "X" => Decomposed expression (string vector)
Decomp.str <- function(expr, var="X") {
    as.character(body(str2fn(expr, var)))
}

# Merge Power
Merge.pwr <- function(expr, cs) {
    if (grepl("\\^\\d+$", expr)) {
        pwr <- as.numeric(sub("^.+\\^", "", expr))
        base <- sub("\\^\\d+$", "", expr)
        csnum <- as.numeric(cs)
        opwr <- MASS::fractions(pwr/csnum)
        out <- paste0(base, "^(", opwr, ")")
    } else if (grepl("\\^\\(.+\\)$", expr)) {
        pwr <- sub("^.+\\^", "", expr)
        pwr <- Rmpar.str(pwr)
        pwr <- sub(".*\\^\\((.+)\\).*", "\\1", pwr)
        base <- sub("\\^\\(.+\\)$", "", expr)
        pwr <- eval(parse(text=pwr))
        csnum <- as.numeric(cs)
        opwr <- MASS::fractions(pwr/csnum)
        out <- paste0(base, "^(", opwr, ")")
    } else {
        pwr <- 1/as.numeric(cs)
        opwr <- MASS::fractions(pwr)
        out <- paste0("(",expr, ")^(", opwr, ")")
    }
    return(out)
}

# String equation => Inverse equation (string)
Inv.op <- function(expr, op, cs) {
   # cs should be numeric string
    if (grepl("[A-z]", cs)) stop(paste0(expr, op, cs), "cannot be handled...")
    mult <- ""
    cs <- Rmpar.str(cs)
    ncs <- eval(parse(text=cs))
    acs <- ifelse(ncs>0, cs, sub("-", "", cs))

    if (nchar(expr) <= 2) {
        expr2 <- expr
    } else {
        expr2 <- paste0("(", expr, ")")
    }

    if (op=="+") {
        sign <- ifelse(ncs>0, "-", "+")
        res <- paste0(expr, sign, acs)
    } else if (op=="-") {
        sign <- ifelse(ncs>0, "+", "-")
        res <- paste0(expr, sign, acs)
    } else if (op=="*") {
        sign <- ifelse(ncs>0, "", "-")
        res <- paste0(sign, expr2, "/", acs)
    } else if (op=="/") {
        sign <- ifelse(ncs>0, "", "-")
        res <- paste0(sign, expr2, "*", acs)
    } else if (op=="^") {
        expo <- as.numeric(cs)
        res <- Merge.pwr(expr, cs)
        if ((expo %% 2)==0) mult <- "pm"
    }
    return(c(res, mult))
}

Sinv.op <- function(body, op, cs, pos=2) {

    if (grepl("^\\(", cs) && grepl("\\)$", cs)) {
        cs <- sub("^\\(", "", cs)
        cs <- sub("\\)$", "", cs)
    }
    ncs <- eval(parse(text=cs))
    acs <- ifelse(ncs>0, cs, sub("-", "", cs))
  # Core
    core <- ifelse(nchar(body)<=2, body, paste0("(",body,")"))
    if (op=="+") {
        sign <- ifelse(ncs>0, "-", "+")
        out <- paste0(core, sign, acs)
    } else if (op=="-") {
        if (pos==2) {
          sign <- ifelse(ncs>0, "+", "-")
          out <- paste0(core, sign, acs)
        } else {
          out <- paste0(cs, "-", core)
        }
    } else if (op=="*") {
        sign <- ifelse(ncs>0, "", "-")
        out <- paste0(sign, core, "/", acs)
    } else if (op=="/") {
        if (pos==2) {
            sign <- ifelse(ncs>0, "", "-")
            out <- paste0(sign, core, "*", acs)
        } else {
             out <- paste0(cs, "/", core)
        }
    } else if (op=="^") {
        sign <- ifelse(ncs>0, "", "-")
        out <- paste0(core, "^(", sign, "1/", acs, ")")
        if ((ncs %% 2)==0) {
            out <- paste0(c("", "-"), sub("^-", "", out))
        }
    }
    return(out)
}

# Inverse of a simple function (string => string)
Sinv.fn <- function(fn, var) {
    core <- ifelse(nchar(var)<=2, var, paste0("(",var,")"))
    if (fn=="exp") {
        out <- paste0("log(", core, ")")
    } else if (fn=="log") {
        out <- paste0("exp(", core, ")")
    } else if (fn=="sqrt") {
        out <- paste0(core, "^2")
    } else if (fn=="abs") {
        out <- paste0(c("", "-"), core)
    }
    return(out)
}

Inv.fn <- function(fn, expr) {
   # expr should be string function of "X"
    mult <- ""

    if (nchar(expr) <= 2) {
        expr2 <- expr
    } else {
        expr2 <- paste0("(", expr, ")")
    }

    if (fn=="exp") {
        res <- paste0("log(", expr, ")")
    } else if (fn=="log") {
        res <- paste0("exp(", expr, ")")
    } else if (fn=="sqrt") {
        res <- paste0(expr2, "^2")
    } else if (fn=="abs") {
        mult <- c("+", "-")
        res <- expr2
    } else if (fn=="sin") {
        res <- paste0("asin(", expr, ")")
    } else if (fn=="asin") {
        res <- paste0("sin(", expr, ")")
    } else if (fn=="cos") {
        res <- paste0("acos(", expr, ")")
    } else if (fn=="acos") {
        res <- paste0("cos(", expr, ")")
    } else if (fn=="tan") {
        res <- paste0("atan(", expr, ")")
    } else if (fn=="atan") {
        res <- paste0("tan(", expr, ")")
    }
    return(res)
}

# Obtain Inverse Transformation [t(y) == X]
myInv.str <- function(expr, var, dupl=NULL, v0="X") {
    res <- var

   # Remove (...) and decompose
    dd <- Rmpar.str(expr)
    dd <- Decomp.str(dd)
    ## print(dd) ## [1]

    nn <- length(dd)
    if (nn==1) {
        res <- res[1]
    } else if (nn==2) {
        res <- Inv.fn(dd[1], res[1])
        ## cat("res21:", res, "\n") ## [2]
        res <- myInv.str(dd[2], res[1])
        ## cat("res22:", res, "\n") ## [3]
    } else if (nn==3) {
        mm <- grep(v0, dd)
        body <- dd[mm]
        const <- setdiff(1:3, c(1,mm))
        res <- Inv.op(res[1], dd[1], dd[const])
        if (res[2]=="pm") dupl <- res[1]
        ## cat("res31:", res, "\n") ## [4]
        res <- myInv.str(body, res[1], dupl)
        ## cat("res32:", res, "\n") ## [5]
    }
    return(c(res, dupl))
}

Simp.inv <- function(expx, v0="xx", t0="yy") {
  # Check input
    if (!is.character(expx)) stop("Input f(v0) as string...")
  # Decompose expx
    if (grepl("^\\(", expx) && grepl("\\)$", expx)) {
        bare <- sub("^\\(", "", expx)
        bare <- sub("\\)$", "", bare)
    } else bare <- expx
    dd <- as.character(body(str2fn(bare, v0)))
    ## print(dd)
    nn <- length(dd)
  # Initialize
    res <- t0
  # Single variable expx
    if (nn==1) {
        res <- res[1]
  # Functional expx
    } else if (nn==2) {
        ## res <- Sinv.fn(dd[1], var=res[1])
        ## res <- Simp.inv(dd[2], var=res[1]) # [Removed: 2023.09.28]
        ## [Added: 2023.09.28]
        trans <- Sinv.fn(dd[1], var=res[1])
        dd2 <- as.character(body(str2fn(dd[2], v0)))
        if (length(dd2) == 1) {
            res <- trans
        } else {
            res <- Simp.inv(dd[2], t0=trans)
        }
  # Operational expx
    } else if (nn==3) {
        mm <- grep(v0, dd)
        body <- dd[mm]
        const <- setdiff(2:3, mm)
        res <- Sinv.op(res[1], dd[1], dd[const], mm)
        if (length(res)==1) {
            res <- Simp.inv(body, t0=res[1])
        } else {
            res1 <- res[1]
            res1 <- Simp.inv(body, t0=res1[1])
            res2 <- res[2]
            res2 <- Simp.inv(body, t0=res2[1])
            res <- c(res1, res2)
        }
    }
    res <- remove.par(res)  ## [Added: 2023.09.28]
    return(res)
}

# Find plot range of a function [2023.4.24]
plot.rng <- function(FUN, var, lb=-Inf, ub = Inf, del=0.001) {
    if (is.function(FUN)) {
        fun <- deparse(body(FUN))
    } else if (is.character(FUN)) {
        fun <- FUN
    }
    if (!is.character(var)) var <- deparse(substitute(var))

    xfn <- paste0(fun, "*", var)
    x2fn <- paste0(fun, "*", var, "^2")

    FN <- str2fn(fun, var)
    XFN <- str2fn(xfn, var)
    X2FN <- str2fn(x2fn, var)

    ss <- c(lb, ub)
    nyet <- TRUE
    root <- NULL
    inf.lb <- ifelse(lb==-Inf, TRUE, FALSE)
    inf.ub <- ifelse(ub== Inf, TRUE, FALSE)
    Big <- 10

    while (nyet) {
        if (inf.lb) lb <- -Big
        if (inf.ub) ub <- Big
        prob <- integrate(FN, lb, ub)[[1]]
        ## cat("lb =",lb, "ub =", ub, "prob =", prob, "\n")
        if (prob < 1-1e-7) {
            Big <- Big*2
        } else {nyet <- FALSE}
        if (Big > 1e+8) nyet <- FALSE
    }
    EX <- integrate(XFN, lb, ub)[[1]]
    EX2 <- integrate(X2FN, lb, ub)[[1]]
    SD <- sqrt(EX2-EX^2)
    rng <- EX + c(-3,3)*SD
    rng[1] <- max(lb, rng[1])
    rng[2] <- min(ub, rng[2])
    return(rng)
}

exp2_0 <- function(cfn, vv) {
  # Take care of exp(-Inf) => 0
    fn <- str2fn(cfn, vv)
    ## print(fn)
    aax <- deparse(body(fn))
  # Split by " + " sign
    bbx <- stringr::str_split_1(aax, " \\+ ")
    ## print(bbx)
    find <- grepl("exp", bbx) & grepl("Inf", bbx)
    ## print(find)
    if (sum(find) > 0) {
        bbn <- length(bbx)
        if (bbn == 1) {
            bbx <- ""
        } else if (bbn == 2) {
            bbx <- bbx[!find]
        } else {
            bbx <- paste(bbx[!find], collapse=" ")
        }
    }
  # Split by " - " sign
    bbx <- stringr::str_split_1(aax, " - ")
    ## print(bbx)
    find <- grepl("exp", bbx) & grepl("Inf", bbx)
    ## print(find)
    if (sum(find) > 0) {
        bbn <- length(bbx)
        if (bbn == 1) {
            bbx <- ""
        } else if (bbn == 2) {
            bbx <- bbx[!find]
        } else {
            bbx <- paste(bbx[!find], collapse=" ")
        }
    }

    ## bbx <- Deriv::Simplify(bbx)
    ## print(bbx)
    ## cfn <- gsub(" exp\\(.+Inf.+\\)+", 0, bbx)
    cfn <- bbx
    nlp <- stringr::str_count(cfn, stringr::fixed("("))
    nrp <- stringr::str_count(cfn, stringr::fixed(")"))
    if (nlp > nrp) {
        for (k in 1:(nlp-nrp)) cfn <- paste0(cfn, ")")
    }
    cfn <- Deriv::Simplify(cfn)
    cfn <- gsub(" ", "", cfn)
    return(cfn)
}

Tr.cdf <- function(fun, low, itrf, vn="x", v2n="y") {
    if (length(fun) > 1) stop(paste("fun =", fun))
    vn <- tolower(vn)
    Vn <- toupper(vn)
    fun <- gsub(vn, Vn, fun)
    fun <- gsub("eXp", "exp", fun)
    if (length(itrf) > 1) itrf <- itrf[1]

    bfn <- c("dnorm", "dt(", "df(", "dexp", "dgamma", "dweibull", "dchisq")
    check <- sapply(bfn, function(ss) grepl(ss, fun, fixed=TRUE))
    builtin <- ifelse(any(check), TRUE, FALSE)
    if (builtin) {       
        out <- sub(Vn, itrf, fun, fixed=TRUE)
        if (!missing(itrf)) {
            jac <- D.str(itrf, v2n)
            out <- paste0(out, "*", sub("^-", "", jac))  
        }
    } else {
      # Integrate w.r.t. Vn
        ## print(fun) ## [0]
        cfn <- Int0.str(fun, Vn)
        ## print(cfn) ## [0]
      # Definite integral
        cfn0 <- gsub(Vn, low, cfn)
        ## print(cfn0) ## [1]
        if (grepl("[A-z]", cfn0)) {
            cfn2 <- paste(cfn, "-", cfn0)
        } else {
            cfn0 <- eval(str2expression(cfn0))
            if (cfn0 == 0) {
                cfn2 <- cfn
            } else {
                cfn2 <- paste(cfn, "-", cfn0)
            }
        }
      # Substitute
        cfn2 <- gsub(Vn, paste0("(",itrf,")"), cfn2)
        ## print(cfn2) ## [1]
        ## dfn2 <- D.str(cfn2, v2n)
      # Derivative
        dfn2 <- deparse(D(parse(text=cfn2), v2n))
        ## print(dfn2) ## [2]
      # Check the sign (weird in some cases)
        d.itrf <- D.str(itrf, v2n)
        out <- sub("^-", "", dfn2) 
    }
    Sim.str(out)
}

# Remove Redundant Parentheses ## [Added: 2023.09.28] [Revised: 2023.10.5]
rem.paren <- function(ex)
{
  # When ex = variable or numeric item
    if( mode(ex) %in% c("name","numeric") ) return(ex)

  # Check ex[[1]]
    op <- as.character(ex[[1]])
  # When ex[[1]] = (
    if( op == "(" ) return(rem.paren(ex[[2]]))
  # Otherwise
    ## if( op %in% c("+","-","*","/","^") ) {
    if (length(ex) == 3) {
        return(call(op, rem.paren(ex[[2]]), rem.paren(ex[[3]])))
    } else {
        return(call(op, rem.paren(ex[[2]])))
    }
}

remove.par <- function(s) {
    temp <- deparse(rem.paren(parse(text=s)[[1]]))
    gsub(" ", "", temp)
}
