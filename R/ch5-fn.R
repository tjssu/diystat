# [Ch-5 Functions] ----------------------------------------------------------------------------------
# [Ch-5 Function Manual] -----------------------------------------

#' @title Manual for Ch5
#' @description Ch5. Expected Values of Random Variables
#' @param fn Function number, Default: 0
#' @return None.
#' @examples
#' ch5.man()
#' ch5.man(2)
#' @rdname ch5.man
#' @export
ch5.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] disc.exp\tExpected Values of a Discrete Random Variable\n")
        cat("[2] cont.exp\tExpected Values of a Continuous Random Variable\n")
        cat("[3] disc.jexp\tExpected Values of Two Discrete Random Variables\n")
        cat("[4] cont.jexp\tExpected Values of Two Continuous Random Variables\n")
        cat("[5] mult.corr\tCorrelation Coefficients and Scatter Plots\n")
    }
    if (1 %in% fn) {
        cat("[1] Expected Values of a Discrete Random Variable\n")
        cat("disc.exp(xf, xv, Tr, prt=\"exp\", dig=4, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("xf\t Probability (frequency) distribution or random variable.\n")
        cat("[Optional Input]--------------------------\n")
        cat("xv\t Values of X (required if xf is a function or vector).\n")
        cat("Tr\t Transformation formula (string).\n")
        cat("prt\t Print option: one of c(\"exp\", \"var\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
        cat("[Returned Object]--------------------------\n")
        cat("list(EX=E(X), VX=V(X))\n")
    }
    if (2 %in% fn) {
        cat("[2] Expected Values of a Continuous Random Variable\n")
        cat("cont.exp(fun, lb, ub, Tr, prt=\"exp\", dig=4, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun\t Continuous PDF (function or string).\n")
        cat("[Optional Input]--------------------------\n")
        cat("lb\t Lower bound of fun, Default: -Inf.\n")
        cat("ub\t Upper bound of fun, Default: Inf.\n")
        cat("Tr\t Transformation formula (string).\n")
        cat("prt\t Print option: one of c(\"exp\", \"var\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
        cat("[Returned Object]--------------------------\n")
        cat("list(EX=E(X), VX=V(X))\n")
    }
    if (3 %in% fn) {
        cat("[3] Expected Values of Two Discrete Random Variables\n")
        cat("disc.jexp(X, Y, FUN, Tr, prt=\"exp\", dig=4, detail=FALSE)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("X\t Joint frequency (or probability) table (or vector).\n")
        cat("[Optional Input]--------------------------\n")
        cat("Y\t The second random variable (required if X is a vector).\n")
        cat("FUN\t FUN Joint frequency (probability) function (if necessary).\n")
        cat("Tr\t Transformation formula (string).\n")
        cat("prt\t Print option: one of c(\"exp\",\"var\",\"cov\",\"cor\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("detail\t Logical: print the joint & marginal PDF? Default: FALSE.\n")
        cat("[Returned Object]--------------------------\n")
        cat("list(EX=E(X),EY=E(Y),VX=V(X),VY=V(Y),EXY=E(XY),VXY=Cov(X,Y),CXY=Corr(X,Y))\n")
    }
    if (4 %in% fn) {
        cat("[4] Expected Values of Two Continuous Random Variables\n")
        cat("cont.jexp(fun, lb, ub, y1, y2, Tr, prt=\"exp\", dig=4, frac=FALSE)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("fun\t Continuous joint PDF as string \"f(x,y)\".\n")
        cat("[Optional Input]--------------------------\n")
        cat("lb\t Lower bound (numeric) of X and Y.\n")
        cat("ub\t Upper bound (numeric) of X and Y.\n")
        cat("y1\t Lower limit (function or strings) of Y.\n")
        cat("y2\t Upper limit (function or strings) of Y.\n")
        cat("Tr\t Transformation formula (string).\n")
        cat("prt\t Print option: one of c(\"exp\",\"var\",\"cov\",\"cor\"), Default: \"exp\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("frac\t Logical: convert the result to fraction? Default: FALSE.\n")
        cat("[Returned Object]--------------------------\n")
        cat("list(EX=E(X),EY=E(Y),VX=V(X),VY=V(Y),EXY=E(XY),VXY=Cov(X,Y),CXY=Corr(X,Y))\n")
    }
    if (5 %in% fn) {
        cat("[5] Correlation Coefficients and Scatter Plots\n")
        cat("mult.corr(data, item, prt=TRUE, dig=4, detail=FALSE, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("data\t Input data frame (or list or matrix) of variables.\n")
        cat("[Optional Input]--------------------------\n")
        cat("item\t Selected names of the variables.\n")
        cat("prt\t Logical: print the result? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("detail\t Logical: print frequency tables? Default: FALSE.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
        cat("[Returned Object]--------------------------\n")
        cat("Correlation Matrix.\n")
    }
}

# Print long strings on multiple lines
prt.div <- function(str, M=80, H=7) {
    if (nchar(str) > M) {
        Leng <- nchar(str)
        div <- Leng %/% (M-H) + 1
        split <- paste0(strsplit(str, ",")[[1]], ",")
        Last <- length(split)
        split[Last] <- sub(",", "", split[Last])
        len <- cumsum(nchar(split))
        cut <- rep(1, div)
        for (k in 1:div) cut[k] <- max(which(len <= round(Leng/div*k)))
        cut[1] <- cut[1] + 1
        cat(paste(split[1:cut[1]], collapse=""), "\n")
        for (k in 2:div) if (cut[k]>cut[k-1]) cat(paste0("       ", 
            paste(split[(cut[k-1]+1):cut[k]], collapse="")), "\n")
    } else {
        cat(str, "\n")
    }
}

prt.div2 <- function(str, M=80, H=7, spl="\\)", sph="-") {
    splh <- paste0(spl, sph)
    spl0 <- sub("\\", "", spl, fixed=TRUE)
    sph0 <- sub("\\", "", sph, fixed=TRUE)
    if (nchar(str) > M) {
        Leng <- nchar(str)
        div <- Leng %/% (M-H) + 1
        split <- paste0(sph0, strsplit(str, splh)[[1]], spl0)
        Last <- length(split)
        split[Last] <- sub(paste0(spl,"$"), "", split[Last])
        split[1] <- sub(paste0("^", sph), "", split[1])
        len <- cumsum(nchar(split))
        cut <- rep(1, div)
        for (k in 1:div) cut[k] <- max(which(len <= round(Leng/div*k)))
        cut[1] <- cut[1] + 1
        out <- paste(split[1:cut[1]], collapse="")
        for (k in 2:div) {
            if (cut[k]>cut[k-1]) {
                out <- paste(out, "\n", paste0("       ", 
                    paste(split[(cut[k-1]+1):cut[k]], collapse="")))
            }
        }
    }
    return(out)
}

# [5-1] Expected Values of a Discrete Random Variable
#' @title Expected Values of a Discrete Random Variable
#' @description Expected Values of a Discrete Random Variable
#' @param xf Probability (frequency) distribution or random variable.
#' @param xv Values of X (required if xf is a function or vector).
#' @param Tr Transformation formula in string.
#' @param prt Print option: one of c("exp", "var"), Default: "exp".
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return list(EX=expected value, VX=variance)
#' @examples
#' S <- tosscoin2(10)
#' X <- apply(S,1,\(x) sum(x=="H"))
#' # Using the raw random variable
#' disc.exp(X, ws=c(7,4))
#' # Using the frequency table
#' x <- table(X)
#' disc.exp(x, ws=c(7,4))
#' 
#' # Using the PDF: B(20, 0.5)
#' x <- (0:20); y <- x^2
#' fun <- function(v) dbinom(v, 20, 0.5)
#' disc.exp(fun, x, y, prt="var", ws=c(7,4), cex=0.6)
#' @rdname disc.exp
#' @export
disc.exp <- function(xf, xv, Tr, prt="exp", dig=4, ws="n", ...) {
  # Check input
    if (missing(xf)) stop("Input xf as a table, function, or a vector.")

    if (is.table(xf)) {
        Xn <- toupper(deparse(substitute(xf)))
        xv <- as.numeric(names(xf))
        xf <- as.numeric(xf)
    } else if (is.function(xf) || is.character(xf)) {
        if (missing(xv)) stop("Input xv: the values of x for f(x).")
        Xn <- toupper(deparse(substitute(xv)))
        if (is.character(xf)) {
            xf <- str2fn(xf, "x")
            xf <- xf(xv)
        } else if (is.function(xf)) {
            xf <- xf(xv)
        }
    } else if (is.vector(xf)) {
        Xn <- toupper(deparse(substitute(xf)))
        tab <- table(xf)
        xv <- as.numeric(names(tab))
        xf <- as.numeric(tab)
    }

  # Get transformation values Tv
    Do.tr <- FALSE
    if (!missing(Tr)) {
        Do.tr <- TRUE
        xn <- tolower(Xn)
        Tn <- toupper(deparse(substitute(Tr)))
        if (is.character(Tr)) {
            TR <- toupper(Tr)
            Tr <- tolower(Tr)
            Tr <- str2fn(Tr, xn)
            Tv <- Tr(xv)
        } else if (is.function(Tr)) {
            Tv <- Tr(xv)
        } else {
            stop("Input Tr as string.")
        }
    }

  # Calculate the expected value
    N <- sum(xf)
    sm <- sum(xv*xf)
    ex <- sm/N
    rxv <- round(xv, dig)
    rxf <- round(xf, dig)
    rsm <- round(sm, dig)
    rex <- round(ex, dig)
    rN <- round(N, dig)
    xp <- xf/N
    rxp <- round(xp, dig)

  # Calculate variance & standard deviation
    xq <- xv^2
    sq <- sum(xq*xf)
    ex2 <- sq/N
    vx <- ex2 - ex^2
    dx <- sqrt(vx)
    rxq <- round(xq, dig)
    rsq <- round(sq, dig)
    rex2 <- round(ex2, dig)
    rvx <- round(vx, dig)
    rdx <- round(dx, dig)

    cx1 <- paste0("(", paste(rxv, collapse=","),")")
    cx2 <- paste0("(", paste(rxq, collapse=","),")")
    cfr <- paste0("(", paste(rxf, collapse=","),")")
    mult2 <- paste(paste(rxq, rxf, sep="\U00D7"), collapse="+")

    if (Do.tr) {
      # Calculate the expected value
        Ts <- sum(Tv*xf)
        eT <- Ts/N
        rTv <- round(Tv, dig)
        rTs <- round(Ts, dig)
        reT <- round(eT, dig)
      # Calculate variance & standard deviation
        Tq <- Tv^2
        Tsq <- sum(Tq*xf)
        eT2 <- Tsq/N
        vT <- eT2 - eT^2
        dT <- sqrt(vT)
        rTq <- round(Tq, dig)
        rTsq <- round(Tsq, dig)
        reT2 <- round(eT2, dig)
        rvT <- round(vT, dig)
        rdT <- round(dT, dig)

        cT1 <- paste0("(", paste(rTv, collapse=","),")")
        cT2 <- paste0("(", paste(rTq, collapse=","),")")
        mult2 <- paste(paste(rTq, rxf, sep="\U00D7"), collapse="+")
    }

    if (prt %in% c("exp", "var")) {
        out1 <- paste0("E(",Xn, ") = ", cx1)
        if (N > 1.00001) {
            out2 <- paste0("%*%", cfr,
                "/", rN, " = ", rsm, "/", rN, " = ", rex)
            out <- paste0(out1, out2)
        } else {
            out2 <- paste0("%*%", cfr, " = ", rex)
            out <- paste0(out1, out2)
        }
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
    }

    if (prt == "var") {
        out1 <- paste0("E(",Xn, "\U00B2) = ", cx2)
        if (N > 1.00001) {
            out2 <- paste0("%*%", cfr,
                "/", rN, " = ", rsq, "/", rN, " = ", rex2)
            out <- paste0(out1, out2)
        } else {
            out2 <- paste0("%*%", cfr, " = ", rex2)
            out <- paste0(out1, out2)
        }
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
        cat(paste0("V(",Xn,") = ", rex2, " - ", rex, "\U00B2 = ", rvx), "\n")
        cat(paste0("D(",Xn,") = \U221A(", rvx, ") = ", rdx), "\n")
    }

    if (Do.tr && (prt %in% c("exp", "var"))) {
        out1 <- paste0("E(",Tn, ") = ", cT1)
        if (N > 1.00001) {
            out2 <- paste0("%*%", cfr,
                "/", rN, " = ", rTs, "/", rN, " = ", reT)
            out <- paste0(out1, out2)
        } else {
            out2 <- paste0("%*%", cfr, " = ", reT)
            out <- paste0(out1, out2)
        }
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
    }

    if (Do.tr && (prt == "var")) {
        out1 <- paste0("E(",Tn, "\U00B2) = ", cT2)
        if (N > 1.00001) {
            out2 <- paste0("%*%", cfr,
                "/", rN, " = ", rsq, "/", rN, " = ", reT2)
            out <- paste0(out1, out2)
        } else {
            out2 <- paste0("%*%", cfr, " = ", reT2)
            out <- paste0(out1, out2)
        }
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
        cat(paste0("V(",Tn,") = ", reT2, " - ", reT, "\U00B2 = ", rvT), "\n")
        cat(paste0("D(",Tn,") = \U221A(", rvT, ") = ", rdT), "\n")
    }

  # Plot PDF f(x)
    if (is.numeric(ws)) {
        x1 <- min(xv)
        x2 <- max(xv)
        xr <- x2 - x1
        mt <- paste("PDF of", Xn)

        arg1 <- list(type="h", main=mt, pch=19, lwd=5, ylim=c(0, max(xp)*1.1),
                   xlim=c(x1-0.1*xr, x2+0.1*xr), col="red", xlab="x", ylab="f(x)")
        dots <- list(...)
        dd1 <- c("type", "main", "pch", "lwd", "ylim", "xlim", "col", "xlab", "ylab")

        cex <- 0.8
        pos <- 3
        col.text <- col.seg <- col.arr <- "blue"
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
        if (Do.tr) par(mfrow=c(1,2))
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))
        xylist <- list(x=xv, y=xp)
        do.call(plot, c(xylist, arg1, dots))

        grid(col="green")
        abline(h=0, lty=3, col=grey(0.5))

      # Display probability
        text(xv, xp, round(xp, dig), pos=pos, col=col.text, cex=cex)

      # Display the expected value
        ym <- 0.5*max(xp)
        segments(ex, 0, ex, ym, lty=lty.seg, col=col.seg)
        anotate <- paste0("E(", Xn, ")")
        text(ex, ym, anotate, pos=pos, col=col.text)

      # Display the standard deviation
        x1 <- ex - dx
        x2 <- ex + dx
        arrows(ex, ym, x2, ym, length=0.1, angle=90, lty=2, col=col.arr)
        arrows(ex, ym, x1, ym, length=0.1, angle=90, lty=2, col=col.arr)

      # Display legend
        leg.cex <- ifelse(Do.tr, 0.8, 1)
        legend(pos.leg, cex=leg.cex, c(paste0("E(", Xn, ") = ",round(ex,dig)),
            paste0("D(", Xn, ") = ", round(dx,dig))), bg="white")
    }

  # Plot transformed PDF f(tr)
    if (Do.tr && (is.numeric(ws))) {
        x1 <- min(Tv)
        x2 <- max(Tv)
        xr <- x2 - x1
        mt2 <- paste("PDF of", Tn, "=", toupper(TR))
        arg2 <- list(type="h", main=mt2, pch=19, lwd=5, ylim=c(0, max(xp)*1.1),
                   xlim=c(x1-0.1*xr, x2+0.1*xr), col="red", 
                   xlab="x", ylab=paste0("f(",tolower(Tn),")"))
        dd2 <- c("type", "main", "pch", "lwd", "ylim", "xlim", "col", "xlab", "ylab")
        ck2 <- which(dd2 %in% names(arg2))
        for (kk in ck2) arg1[[kk]] <- arg2[[kk]]

        xylist <- list(x=Tv, y=xp)
        do.call(plot, c(xylist, arg2, dots))

        grid(col="green")
        abline(h=0, lty=3, col=grey(0.5))

      # Display probability
        text(Tv, xp, round(xp, dig), pos=pos, col=col.text, cex=cex)

      # Display the expected value
        ym <- 0.5*max(xp)
        segments(eT, 0, eT, ym, lty=lty.seg, col=col.seg)
        anotate <- paste0("E(", Tn, ")")
        text(eT, ym, anotate, pos=pos, col=col.text)

      # Display the standard deviation
        x1 <- eT - dT
        x2 <- eT + dT
        arrows(eT, ym, x2, ym, length=0.1, angle=90, lty=2, col=col.arr)
        arrows(eT, ym, x1, ym, length=0.1, angle=90, lty=2, col=col.arr)

      # Display legend
        legend(pos.leg, cex=leg.cex, c(paste0("E(", Tn, ") = ",round(eT,dig)),
            paste0("D(", Tn, ") = ", round(dT,dig))), bg="white")
    }

    if (Do.tr) {
        invisible(list(ET=eT, VT=vT, EX=ex, VX=vx))
    } else {
        invisible(list(EX=ex, VX=vx))
    }
}

# Find lb and ub of the function with logical boundary condition
find.bound <- function(cfun, var="x") {
    lb <- -Inf
    ub <- Inf
    try11 <- stringr::str_match(cfun, 
            paste0("(\\(|&)", var, ">(.*?)(\\)|&|,)"))[1,3]
    if (!is.na(try11)) lb <- try11
    try12 <- stringr::str_match(cfun, 
            paste0("(\\(|&)(.*?)<", var, "(\\)|&|,)"))[1,3]
    if (!is.na(try12)) lb <- try12

    try21 <- stringr::str_match(cfun, 
            paste0("(\\(|&)",var,"<(.*?)(\\)|&|,)"))[1,3]
    if (!is.na(try21)) ub <- try21
    try22 <- stringr::str_match(cfun, 
            paste0("(\\(|&)(.*?)>", var, "(\\)|&|,)"))[1,3]
    if (!is.na(try22)) ub <- try22

    if (is.na(as.numeric(lb))) {
        lb <- gsub("\\D", "", lb)
        lb <- gsub(ub, "", lb)
    }
    if (is.na(as.numeric(ub))) {
        ub <- gsub("\\D", "", ub)
        ub <- sub(lb, "", ub)
    }
    c(lb, ub)
}

# Find the core of a function: c(var, cfun, lb, ub, Builtin)
core.str <- function(fun) {
  # Check fun => cfun: string function
    if (missing(fun)) stop("Input fx as a string.")

    if (is.function(fun)) {   
        cfun <- gsub(" ", "", deparse(body(fun)))
    } else {
        cfun <- gsub(" ", "", fun)
    }
    lb <- -Inf
    ub <- Inf

  # Check built-in pdf functions
    bfn0 <- c("dnorm", "dt(", "df(", "dexp", "dgamma", 
              "dweibull", "dchisq", "dbeta", "dunif")

    check <- sapply(bfn0, function(ss) grepl(ss, cfun, fixed=TRUE))
    Builtin <- ifelse(any(check), TRUE, FALSE)

  # Find the variable
    if (Builtin) {
        bfx <- bfn0[check]
        var <- gsub("(\\W|\\d)", "", 
               stringr::str_match(cfun, paste0(bfx,"\\((.*?)\\)"))[1,2])
    } else {
        bfn <- c("exp", "log", "abs", "sqrt", "asin", "acos", "atan",
            "sin", "cos", "tan", "ifelse", "if")
        rmf <- gsub("(\\W|\\d)", "", cfun)

        for (k in 1:length(bfn)) rmf <- gsub(bfn[k], "", rmf)
        if (nchar(rmf) == 1) {
            uni.var <- TRUE
            var <- rmf
        } else {
            vv <- NULL
            for (j in 1:nchar(rmf)) vv <- c(vv, substr(rmf, j, j))
            uu <- unique(vv)
            if (length(uu) == 1) {
                uni.var <- TRUE
                var <- uu[1]
            } else {
                uni.var <- FALSE
                tt <- table(vv)
                var <- names(tt)[tt==max(tt)]
            }
        }
    }

  # Remove logical condition
    if (grepl("[<>]", cfun)) {
        dfun <- Decomp.str(cfun, var)
        dfun <- gsub(" ", "", dfun)
      # ifelse => Do nothing...
        if (dfun[1] != "ifelse") {
            ndf <- length(dfun)
            svv <- grep(var, dfun)
            srm <- grep("[<>]", dfun)
            svv <- setdiff(svv, srm)
            if (length(svv) == 1) {
                cfun <- dfun[svv]
            } else {
                cat("[core.str] dfun =", dfun, "\n")
            }

            bb <- gsub("\\D", "", dfun[srm])
            L1 <- grepl(paste0(var,">"), dfun[srm])
            L2 <- grepl(paste0("<",var), dfun[srm])
            U1 <- grepl(paste0(var,"<"), dfun[srm])
            U2 <- grepl(paste0(">",var), dfun[srm])
            if (L1 || L2) lb <- bb
            if (U1 || U2) ub <- bb
        }
    }
    return(c(var, cfun, lb, ub, Builtin))
}

# Integrate the first two moments of f(x) = c*exp(-c*x)
Iex1.str <- function(fun, var="x") {
    ifun <- mosaicCalc::antiD(as.formula(paste(fun, "~", var)))
    Fun <- deparse(body(ifun))
    Fun <- Sim.str(sub("C", "0", Fun))

    iFun <- mosaicCalc::antiD(as.formula(paste(Fun, "~", var)))
    Fun1 <- deparse(body(iFun))
    Fun1 <- Sim.str(sub("C", "0", Fun1))

    i2Fun <- mosaicCalc::antiD(as.formula(paste(Fun1, "~", var)))
    Fun2 <- deparse(body(i2Fun))
    Fun2 <- Sim.str(sub("C", "0", Fun2))

    out1 <- paste0(var, "*", Fun, "-", Fun1)
    out1 <- Sim.str(out1)
    out2 <- paste0(var, "^2*", Fun, "-2*", var, "*", Fun1,
                "+2*", Fun2)
    out2 <- Sim.str(out2)
    return(c(out1, out2))
}

# Evaluate string function fun(val)
Eval.str <- function(fun, val, var="x") {
    sfun <- gsub(var, "X", fun)
    sfun <- gsub("eXp","exp", sfun)
    vfun <- gsub("X", val, sfun)

    if (val != Inf && val != -Inf) {
        out <- eval(parse(text=vfun))
    } else {
        ##vfun0 <- exp2_0(vfun, vv=var)
        ##out <- eval(parse(text=vfun0))
        out <- 0
    }
    out
}

# Obtain the PDF of transformed variable
Tr.pdf <- function(fx, lb, ub, Vn, var, Tr, Tvar, debug=FALSE) {
    fX <- gsub(var, Vn, fx)
    fX <- gsub("eXp", "exp", fX)
    fX <- gsub("Exp", "exp", fX)
    fX <- gsub("exP", "exp", fX)
    Tr <- gsub(var, Vn, Tr)
    Tr <- gsub("eXp", "exp", Tr)
    Tr <- gsub("Exp", "exp", Tr)
    Tr <- gsub("exP", "exp", Tr)
  
  # Obtain Transformed PDFs -------------------------
    iTr2 <- NA
    temp <- myInv.str(Tr, Tvar)
    iTr <- Sim.str(temp[1])
    if (debug) cat(paste0("iTr=", iTr), "\n") ## [1]

  # Duplicate Tr
    if (length(temp) > 1) {
        if (debug) cat(paste0("temp[1]: ", temp[1], "  temp[2]: ", temp[2]), "\n") ##
        iTr2 <- sub(temp[2], paste0("-",temp[2]), temp[1], fixed=TRUE)
        if (!is.na(iTr2)) iTr2 <- Sim.str(iTr2)
        if (debug) cat(paste0("iTr2=", iTr2), "\n") ## [2b]
    }

  # Jacobian
    Jac <- D.str(iTr, Tvar)
    Jac <- sub("^-", "", Jac)
    if (debug) cat(paste0("Jac=", Jac), "\n") ## [3]
    if (debug) Jac <- paste0("abs(", Jac, ")")

  # Create Transformation Function
    TF <- str2fn(Tr, Vn)
  # Transformed PDF
    mftf1 <- mftf2 <- cftf2 <- clim <- NA
    flim <- c(lb, ub)

    frng <- find.rng(TF, flim)
    tlo <- frng[1]
    tup <- frng[2]
    xcut <- frng[3]
    clim <- frng[4]
    if (debug) cat("frng:", frng, "\n") ## [4]
    trng.prt <- paste0(", for (", round(frng[1],4), "<", 
                Tvar, "<", round(frng[2],4), ")")
    if (debug) cat(paste0("trng.prt:"), trng.prt, "\n") ## [5]

  # Functional string
    tr.body <- gsub(Vn, paste0("(",iTr,")"), fX)
    if (debug) cat(paste0("fX: ", fX), "\n") ## [6]
    if (debug) cat(paste0("tr.body: ", tr.body), "\n") ## [6]
    cfTr <- paste0(tr.body, "*", Sim.str(Jac))
    if (debug) cat(paste0("cftf: ", cfTr), "\n") ## [6]

  # Handle Duplicate TR
    if (!is.na(iTr2) && flim[1]<xcut && flim[2]>xcut) {
        if (debug) cat("flim: ", flim, "\n") ## [7]
        if (debug) cat(paste0("xcut = ", xcut), "\n") ## [7b]
        ## cftf2 <- gsub("sqrt\\(", "-sqrt\\(", iTr)
        tr2.body <- gsub(Vn, paste0("(",iTr2,")"), fX)

        cftf2 <- paste0(tr2.body, "*", Jac)
        if (debug) cat(paste0("cftf2: ", cftf2), "\n") ## [6c]
    } else { cftf2 <- NA }

  # Transformed PDF
    if (is.na(cftf2)) {
        fTr <- paste0("ifelse(", Tvar,">",round(tlo,7),
              "&", Tvar, "<",round(tup,7), "," ,cfTr, ",0)")
        if (debug) cat(paste0("ftf:"), fTr, "\n") ## [7]
        FTF <- str2fn(fTr, Tvar)
    } else {
      # Multiple TR Case
        dist1 <- round(xcut,4) - flim[1]
        dist2 <- flim[2] - round(xcut,4)

        if (dist1 <= dist2) {
            mainf <- paste0(Sim2.str(tr.body,Tvar), "*", Sim.str(Jac))
            subf <- paste0(Sim2.str(tr2.body,Tvar), "*", Sim.str(Jac))
        } else {
            mainf <- cftf2
            subf <- cfTr  ## correction [23.5.4]
        }
        if (debug) cat(paste0("mainf:"), mainf, "\n") ## [8]
        if (debug) cat(paste0("subf:"), subf, "\n") ## [8b]

        mftf1 <- paste0("ifelse(", Tvar,">",round(tlo,7),
              "&", Tvar, "<",round(tup,7), ", " ,mainf, ", 0)")
        if (debug) cat(paste0("mftf1:"), mftf1, "\n") ## [9]
        MFTF1 <- str2fn(mftf1, Tvar)
 
        mftf2 <- paste0("ifelse(", Tvar,">",round(tlo,7),
              "&", Tvar, "<",round(clim,7), ", " ,subf, ", 0)")
        if (debug) cat(paste0("mftf2:"), mftf2, "\n") ## [9b]
        MFTF2 <- str2fn(mftf2, Tvar)

        fTr <- paste0(mftf1, "+", mftf2)
        if (debug) cat(paste0("ftf:"), fTr, "\n") ## [10]
        FTF <- str2fn(fTr, Tvar)
    } # End of if
    return (list(fun=FTF, rng=c(tlo, tup)))
}

# [5-2] Expected Values of a Continuous Random Variable

#' @title Expected Values of a Continuous Random Variable
#' @description Calculate E(X) and Var(X) of a Continuous Random Variable X
#' @param fun Continuous PDF (function or string).
#' @param lb Lower bound of fun, Default: -Inf.
#' @param ub Upper bound of fun, Default: Inf.
#' @param Tr Transformation formula in string.
#' @param prt Print option: one of c("exp", "var"), Default: "exp".
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return list(EX=E(X), VX=Var(X))
#' @examples
#' # Using the PDF
#' pdf <- function(x) exp(-2*x)*(x>0)
#' cont.exp(pdf, prt="var")
#' # Using the string PDF with a lower bound
#' cont.exp("2*exp(-2*x)", lb=0, prt="var")
#'
#' # Using the string PDF with a two-sided bound
#' cont.exp("2*y", lb=0, ub=1, prt="var")
#'
#' # Using the string of built-in PDF
#' cont.exp("dnorm(x, 10, 3)", prt="var")
#' cont.exp(\(x) dnorm(x, 10, 3), prt="var")
#'
#' # Applying transformation
#' y="x^2"; cont.exp("dnorm(x)", Tr=y, prt="var")
#'
#' @rdname cont.exp
#' @export

cont.exp <- function(fun, lb, ub, Tr, prt="exp", dig=4, ws="n", ...) {
  # Check fun
    if (missing(fun)) stop("Input fun as a function or string.")

    if (is.character(fun)) {
        cfun <- fun
    } else if (is.function(fun)) {
        cfun <- deparse(body(fun))
    } else stop("Input fun as a function or string.")

    out <- core.str(cfun)
    ## print(out) ## [1]
    var <- out[1]
    Vn <- toupper(var)
    cfx <- out[2]
    fx <- str2fn(cfx, var)
    Builtin <- as.logical(out[5])

    if (missing(lb)) lb <- as.numeric(out[3])
    if (missing(ub)) ub <- as.numeric(out[4])

    if (missing(Tr)) {
        Do.tr <- FALSE
    } else {
        Do.tr <- TRUE
        Tvar <- deparse(substitute(Tr))
        Tn <- toupper(Tvar)
        if (nchar(Tvar) > 2) {
            Tvar <- "tt"
            Tn <- "Tr"
        }
        if (is.function(Tr)) Tr <- deparse(body(Tr))
    }

    coef <- 1
    pint <- paste0("\U222B_[", lb, ":", ub, "] {")
    prob <- integrate(fx, lb, ub)[[1]]

    err <- 1e-7
    if (abs(prob-1) > err) {
        coef <- MASS::fractions(1/prob)
        cat(paste("Coefficient", coef, "must be multiplied."), "\n")
        ##pint <- paste (coef, "\U00D7", pint)
        cfx <- paste0(coef, "*(", cfx, ")")
        fx <- str2fn(cfx, var)
    }

  # Mean and variance of random variable X
    ex1 <- function(x) x*fx(x)
    ex2 <- function(x) x^2*fx(x)
    Ex <- integrate(ex1, lb, ub)[[1]]
    Ex2 <- integrate(ex2, lb, ub)[[1]]
    Vx <- Ex2 - Ex^2
    Dx <- sqrt(Vx)

    if (Do.tr) {
        etr1 <- str2fn(paste0("(", Tr, ")*", cfx), var)
        etr2 <- str2fn(paste0("(", Tr, ")^2*", cfx), var)
        ET <- integrate(etr1, lb, ub)[[1]]
        ET2 <- integrate(etr2, lb, ub)[[1]]
        VT <- ET2 - ET^2
        DT <- sqrt(VT)
    }

  # Display output
    ndsp <- function(val) ifelse(val>0, round(val, dig), 
                paste0("(", round(val, dig), ")"))
    bound <- paste0(":[<", ub, ">-<", lb, ">]")

    if (prt %in% c("exp", "var")) {
        cex1 <- paste0(var, " * ", cfx)
        fint1 <- Int0.str(cex1, var)
        if (all(c(!Builtin, !is.na(fint1), prt=="exp", !Do.tr))) {
            int11 <- Eval.str(fint1, lb, var)
            int12 <- Eval.str(fint1, ub, var)
            midf1 <- paste0("     = ", fint1, bound)
            if (nchar(midf1)>80) midf1 <- prt.div2(midf1,75)
            cat(paste0("E(",Vn,") = ", pint, cex1, "}d",var,
                "\n", midf1,
                "\n     = ", round(int12, dig)), "-", ndsp(int11),
                "=", round(Ex, dig), "\n")
        } else {
            cat(paste0("E(",Vn,") = ", pint, cex1, "}d",var),
                "=", round(Ex, dig), "\n")
        }
    }

    if (prt == "var") {
        cex2 <- paste0(var, "\U00B2 * ", cfx)
        cex2f <- paste0(var, "^2 * ", cfx)

        fint2 <- Int0.str(cex2f, var)
        if (!Builtin && !is.na(fint2) && !Do.tr) {
            int21 <- Eval.str(fint2, lb, var)
            int22 <- Eval.str(fint2, ub, var)
            midf2 <- paste0("     = ", fint2, bound)
            if (nchar(midf2)>80) midf2 <- prt.div2(midf2,75)
            cat(paste0("E(",Vn,"\U00B2) = ", pint, cex2,
                "}d",var,"\n", midf2, "\n     = ", round(int22, dig)),
                "-", ndsp(int21), "=", round(Ex2, dig), "\n")
        } else {
            cat(paste0("E(",Vn,"\U00B2) = ", pint, cex2,
                "}d",var), "=", round(Ex2, dig), "\n")
        }
        cat(paste0("Var(",Vn,") = ", round(Ex2,dig), " - ", 
                round(abs(Ex),dig),"\U00B2 = ", round(Vx,dig)), "\n")
    }

  # Transformation --------------
    if (Do.tr && prt %in% c("exp", "var")) {
        ceT1 <- paste0("(", Tr, ") * ", cfx)
        fint1 <- Int0.str(ceT1, var)
        if (!Builtin && !is.na(fint1)) {
            int11 <- Eval.str(fint1, lb, var)
            int12 <- Eval.str(fint1, ub, var)
            midf1 <- paste0("     = ", fint1, bound)
            if (nchar(midf1)>80) midf1 <- prt.div2(midf1,75)
            cat(paste0("E(",Tn,") = ", pint, ceT1, "}d",var,
                "\n", midf1,
                "\n     = ", round(int12, dig)), "-", ndsp(int11),
                "=", round(ET, dig), "\n")
        } else {
            cat(paste0("E(",Tn,") = ", pint, ceT1, "}d",var),
                "=", round(ET, dig), "\n")
        }
    }

    if (Do.tr && prt == "var") {
        ceT2 <- paste0("(", Tr, ")\U00B2 * ", cfx)
        ceT2f <- paste0("(", Tr, ")^2 * ", cfx)
        fint2 <- Int0.str(ceT2f, var)
        if (!Builtin && !is.na(fint2)) {
            int21 <- Eval.str(fint2, lb, var)
            int22 <- Eval.str(fint2, ub, var)
            midf2 <- paste0("     = ", fint2, bound)
            if (nchar(midf2)>80) midf2 <- prt.div2(midf2,75)
            cat(paste0("E(",Tn,"\U00B2) = ", pint, ceT2,
                "}d",var,"\n", midf2, "\n     = ", round(int22, dig)),
                "-", ndsp(int21), "=", round(ET2, dig), "\n")
        } else {
            cat(paste0("E(",Tn,"\U00B2) = ", pint, ceT2,
                "}d",var), "=", round(ET2, dig), "\n")
        }
        cat(paste0("Var(",Tn,") = ", round(ET2,dig), " - ", 
            round(abs(ET),dig),"\U00B2 = ", round(VT,dig)), "\n")
    }

  # Transformed PDF and the limit
    if (Do.tr) {
        Tr.out <- Tr.pdf(cfx, lb, ub, Vn, var, Tr, Tvar)
        Tfn <- Tr.out[[1]]
        Tlim <- Tr.out[[2]]
    }

  # Plot the PDF F(x) --- [Added, 2023.05.18] ------
    if (is.numeric(ws)) {
        xlb <- ifelse(lb > -Inf, lb, Ex-4*Dx)
        xub <- ifelse(ub < Inf, ub, Ex+4*Dx)
        xlim <- c(min(xlb)-(max(xub)-min(xlb))/10,
                   max(xub)+(max(xub)-min(xlb))/10)
        low <- xlim[1]
        up <- xlim[2]
      # Set the range of X
        xrange <- seq(low, up, length=100)
        PDF <- ifelse(xrange > lb & xrange < ub, fx(xrange), 0)

      # Set arguments
        mt <- paste("PDF of", Vn)
        lwd <- 2
        col <- "red"
        xlab <- var
        ylab <- paste0("f(",var,")")
        pos <- 3
        col.text <- col.seg <- col.arr <- "blue"
        lty.seg <- 2
        cex.leg <- ifelse(Do.tr, 0.8, 1)
        pos.leg <- "topright"
        bg.leg <- "white"

        dots <- list(...)

        if (length(dots) > 0) {
            pars <- names(dots)
            if ("main" %in% pars) mt <- dots$main
            if ("lwd" %in% pars) lwd <- dots$lwd
            if ("col" %in% pars) col <- dots$col
            if ("xlab" %in% pars) xlab <- dots$xlab
            if ("ylab" %in% pars) ylab <- dots$ylab
            if ("cex" %in% pars) cex <- dots$cex
            if ("pos" %in% pars) pos <- dots$pos
            if ("col.text" %in% pars) col.text <- dots$col.text
            if ("col.seg" %in% pars) col.seg <- dots$col.seg
            if ("col.arr" %in% pars) col.arr <- dots$col.arr
            if ("lty" %in% pars) lty.seg <- dots$lty
            if ("cex.leg" %in% pars) cex.leg <- dots$cex.leg
            if ("pos.leg" %in% pars) pos.leg <- dots$pos.leg
            if ("bg" %in% pars) bg.leg <- dots$bg
        }

      # Plot
        win.graph(ws[1], ws[2])
        if (Do.tr) par(mfrow=c(1,2))
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))

        y2 <- max(PDF)
        plot(xrange, PDF, type="l", main=mt, lwd=lwd, col=col,
             ylim=c(-y2/15,y2), xlab=xlab, ylab=ylab)
        grid(col=3)
        abline(h=0, lty=3, col=grey(0.5))

      # Display the expected value
        ym <- 0.5*y2
        segments(Ex, 0, Ex, ym, lty=lty.seg, col=col.seg)
        text(Ex, ym, paste0("E(", Vn, ")"), pos=pos, col=col.text)

      # Display the standard deviation
        x1 <- Ex - Dx
        x2 <- Ex + Dx
        arrows(Ex, ym, x2, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)
        arrows(Ex, ym, x1, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)

      # Display legend
        legend(pos.leg, cex=cex.leg, c(paste0("E(", Vn, ") = ",round(Ex,dig)),
            paste0("D(", Vn, ") = ", round(Dx,dig))), bg=bg.leg)
    }

  # Plot the transformed PDF --- [Added, 2023.05.19] ------
    if (is.numeric(ws) && Do.tr) {
        xlb <- ifelse(Tlim[1] > -Inf, Tlim[1], ET-4*DT)
        xub <- ifelse(Tlim[2] < Inf, Tlim[2], ET+4*DT)
        low <- min(xlb)-(max(xub)-min(xlb))/10
        upp <- max(xub)+(max(xub)-min(xlb))/10

      # Set the range of X
        xrange <- seq(low, upp, length=100)
        PDF2 <- Tfn(xrange)

      # Set arguments
        mt2 <- paste("PDF of", Tn, "=", toupper(Tr))

      # Plot
        y2 <- max(PDF2)
        tn <- tolower(Tn)
        yl2 <- paste0("f(",tn,")")
        plot(xrange, PDF2, type="l", main=mt2, lwd=lwd, col=col,
             ylim=c(-y2/15,y2), xlab=tn, ylab=yl2)
        grid(col=3)
        abline(h=0, lty=3, col=grey(0.5))

      # Display the expected value
        ym <- 0.5*y2
        segments(ET, 0, ET, ym, lty=2, col="blue")
        text(ET, ym, paste0("E(", Tn, ")"), pos=pos, col=col.text)

      # Display the standard deviation
        x1 <- ET - DT
        x2 <- ET + DT
        arrows(ET, ym, x2, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)
        arrows(ET, ym, x1, ym, length=0.1, angle=90, lty=lty.seg, col=col.arr)

      # Display legend
        legend(pos.leg, cex=cex.leg, c(paste0("E(", Tn, ") = ",round(ET,dig)),
            paste0("D(", Tn, ") = ", round(DT,dig))), bg=bg.leg)
    }
  # Return the results
    if (Do.tr) {
        out <- list(ET=ET, VT=VT, EX=Ex, VX=Vx)
    } else {
        out <- list(EX=Ex, VX=Vx)
    }
    invisible(out)
}

# [5-3] Joint Probabilities and Expected Values of Two Discrete Random Variables
#' @title Expected Values of Two Discrete Random Variables
#' @description Joint Probabilities and Expected Values of Two Discrete Random Variables
#' @param X Joint frequency (probability) table or vector.
#' @param Y The second random variable (required if X is a vector).
#' @param FUN Joint frequency (probability) function (if necessary).
#' @param Tr Transformation formula in string.
#' @param prt Print option: one of c("exp", "var", "cov", "cor"), Default: "exp".
#' @param dig Number of decimal places, Default: 4.
#' @param detail Logical: print the joint & marginal PDF? Default: FALSE.
#' 
#' @return list(EX=E(X), EY=E(Y), VX=V(X), VY=V(Y), EXY=E(XY), VXY=Cov(X,Y), CXY=Corr(X,Y))
#' @examples
#'
#' # [1] Input the elements of X and Y, respectively
#' S <- rolldie2(4)
#' X <- apply(S, 1, max)
#' Y <- apply(S, 1, min)
#' disc.jexp(X, Y, prt="cov")
#' disc.jexp(X, Y, prt="cor", detail=TRUE)
#'
#' S = rolldie2(3)
#' X = apply(S, 1, median)
#' Y = apply(S, 1, \(x) max(x)-min(x))
#' disc.jexp(X, Y, prt="cor")
#' 
#' # [2] Functional input
#' fxy = function(x, y) x+y
#' x=0:4; y=0:3
#' disc.jexp(y, x, fxy, prt="cor")
#' 
#' # [3] Tabular input
#' tab = with(mtcars, table(cyl, gear))
#' disc.jexp(tab, prt="cor")
#' 
#' @rdname disc.jexp
#' @export

disc.jexp <- function(X, Y, FUN, Tr, prt="exp", dig=4, detail=FALSE) {
  # Check input
  # [1] Functional input
    if (!missing(FUN)) {
        xn <- tolower(deparse(substitute(X)))
        yn <- tolower(deparse(substitute(Y)))
        Xn <- toupper(xn)
        Yn <- toupper(yn)
        tab <- outer(X, Y, FUN)
        tabXY <- as.table(tab)
        dimnames(tabXY) <- list(X, Y)
        names(attr(tabXY, "dimnames")) <- c(Xn, Yn)

  # [2] Raw input
    } else if (is.vector(X)) {
        xn <- tolower(deparse(substitute(X)))
        yn <- tolower(deparse(substitute(Y)))
        Xn <- toupper(xn)
        Yn <- toupper(yn)
        tabXY <- table(X, Y)

  # [3] Tabular input
    } else if (is.matrix(X)) {
        tabXY <- X
        xn <- tolower(names(attr(tabXY, "dimnames"))[1])
        yn <- tolower(names(attr(tabXY, "dimnames"))[2])
        Xn <- toupper(xn)
        Yn <- toupper(yn)
    } else stop("Input (X,Y,FUN) or (X,Y) or (X=table).")

    N <- sum(tabXY)

  # Marginal frequency distribution of X and Y
    mtabXY <- addmargins(tabXY)
  # Joint and marginal probability of X and Y
    ptabXY <- tabXY/N
    mptabXY <- addmargins(ptabXY)

  # Expected Values of X and Y
    Xv <- as.numeric(rownames(tabXY))
    Yv <- as.numeric(colnames(tabXY))
    nv <- dim(mtabXY)
    Xf <- as.vector(mtabXY[, nv[2]])[-nv[1]]
    Yf <- as.vector(mtabXY[nv[1], ])[-nv[2]]
    Sx <- (Xv %*% Xf)[1,1]
    Ex <- Sx/N
    Sy <- (Yv %*% Yf)[1,1]
    Ey <- Sy/N
 
  # Variances of X and Y
    Sx2 <- (Xv^2 %*% Xf)[1,1]
    Ex2 <- Sx2/N
    Vx <- Ex2 - Ex^2
    Dx <- sqrt(Vx)
    Sy2 <- (Yv^2 %*% Yf)[1,1]
    Ey2 <- Sy2/N
    Vy <- Ey2 - Ey^2
    Dy <- sqrt(Vy)

  # Covariance of X and Y
    XY <- Xv %o% Yv
    Sxy <- (as.vector(XY) %*% as.vector(tabXY))[1,1]
    Exy <- Sxy/N
    Vxy <- Exy - Ex*Ey
    Cxy <- Vxy/(Dx*Dy)

  # Print the joint probabilities
    if (detail) {
        if (N>1.0001) {
            cat(paste0("Joint & Marginal Frequency Distribution f(x,y)=n/", 
		N), "\n")
            print(mtabXY)
        } else {
            cat(paste0("Joint & Marginal Probability Distribution f(x,y)"), 
		"\n")
            print(ptabXY)
        }
    }

  # Print the expected values
    if (prt %in% c("exp", "var", "cov", "cor")) {
        cx1 <- paste0("(", paste(Xv, collapse=","),")")
        cx2 <- paste0("(", paste(Xv^2, collapse=","),")")
        cfr <- paste0("(", paste(Xf, collapse=","),")")
        out1 <- paste0("E(",Xn, ") = ", cx1)
        out2 <- paste0("%*%",cfr,"/",N," = ",Sx,"/",N," = ",round(Ex, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }

        cy1 <- paste0("(", paste(Yv, collapse=","),")")
        cy2 <- paste0("(", paste(Yv^2, collapse=","),")")
        cfr <- paste0("(", paste(Yf, collapse=","),")")
        out1 <- paste0("E(",Yn, ") = ", cy1)
        out2 <- paste0("%*%",cfr,"/",N," = ",Sy,"/",N," = ",round(Ey, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }

        ##cat(paste0("E(",Xn,") = ", Sx, "/", N, " = ", round(Ex, dig)), "\n")
        ##cat(paste0("E(",Yn,") = ", Sy, "/", N, " = ", round(Ey, dig)), "\n")
        ##cat(paste0("E(",Xn,Yn,") = ", Sxy, "/", N, " = ", round(Exy, dig)), "\n")
    }
    if (prt %in% c("var", "cor")) {
        out1 <- paste0("E(",Xn, "\U00B2) = ", cx2)
        out2 <- paste0("%*%",cfr,"/",N," = ",Sx2,"/",N," = ",round(Ex2, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
        out1 <- paste0("E(",Yn, "\U00B2) = ", cy2)
        out2 <- paste0("%*%",cfr,"/",N," = ",Sy2,"/",N," = ",round(Ey2, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
        cat(paste0("Var(",Xn,") = ", Sx2, "/", N, " - ", round(abs(Ex),dig), 
            "\U00B2 = ", round(Vx,dig)), "\n")
        cat(paste0("Var(",Yn,") = ", Sy2, "/", N, " - ", round(abs(Ey),dig), 
            "\U00B2 = ", round(Vy,dig)), "\n")
    }
    if (prt %in% c("cov", "cor")) {
        cxy <- paste0("(", paste(as.vector(XY), collapse=","),")")
        cfr <- paste0("(", paste(as.vector(tabXY), collapse=","),")")
        out1 <- paste0("E(",Xn,Yn, ") = ", cxy)
        out2 <- paste0("%*%",cfr,"/",N," = ",Sxy,"/",N," = ",round(Exy, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
        cat(paste0("Cov(",Xn,",",Yn,") = ", round(Exy,dig), " - ", round(Ex,dig), 
	    " \U00D7 ", round(Ey,dig), " = ", round(Vxy,dig)), "\n")
    }
    if (prt=="cor") {
        cat(paste0("Corr(",Xn,",",Yn,") = ", round(Vxy,dig), " / \U221A(", 
	    round(Vx,dig), "\U00D7", round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
    }

  # Transformation ----------------------------
    Do.tr <- FALSE
    if (!missing(Tr)) {
        if (!is.character(Tr)) stop("Input Tr as string.")
        Do.tr <- TRUE
        Tn <- deparse(substitute(Tr))
        if (grepl("\"", Tn)) Tn <- gsub("\"", "", Tn)

        if (grepl(xn, Tr) || grepl(yn, Tr)) {
            FunTr <- tolower(Tr)
            Use.low <- TRUE
            if (grepl(xn, Tr)) Tn <- gsub(xn, Xn, Tn)
            if (grepl(yn, Tr)) Tn <- gsub(yn, Yn, Tn)
        } else {
            FunTr <- Tr
            Use.low <- FALSE
        }       
    }
  # E(Transformation) ------
    ETr <- VTr <- NA
    if (Do.tr) {
        XL <- length(Xv)
        YL <- length(Yv)
        if (Use.low) {
            Fun2 <- gsub(xn, "Xi", FunTr)
            Fun2 <- gsub(yn, "Yj", Fun2)
        } else {
            Fun2 <- gsub(Xn, "Xi", FunTr)
            Fun2 <- gsub(Yn, "Yj", Fun2)
        }
        Trv <- NULL
        for (Xi in 1:XL) {
            for (Yj in 1:YL) {
                Trv <- c(Trv, eval(parse(text=Fun2)))
            }
        }
        STr <- sum(Trv*tabXY) 
        ETr <- STr / N

        S2Tr <- sum(Trv^2*tabXY) 
        E2Tr <- S2Tr / N
        VTr <- E2Tr - ETr^2
    }

    if (Do.tr && prt %in% c("exp", "var")) {
        cTr <- paste0("(", paste(round(Trv,2), collapse=","),")")
        cfr <- paste0("(", paste(as.vector(tabXY), collapse=","),")")
        out1 <- paste0("E(",Tn, ") = ", cTr)
        out2 <- paste0("%*%",cfr,"/",N," = ", round(STr, dig),
                     "/",N," = ",round(ETr, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
    }
    if (Do.tr && prt == "var") {
        c2Tr <- paste0("(", paste(round(Trv^2,2), collapse=","),")")

        oTn <- ifelse(nchar(Tn)>2, paste0("{", Tn, "}"), Tn)
        out1 <- paste0("E(",oTn, "\U00B2) = ", c2Tr)
        out2 <- paste0("%*%",cfr,"/",N," = ", round(S2Tr, dig),
                     "/",N," = ",round(E2Tr, dig))
        out <- paste0(out1, out2)
        if (nchar(out) > 80) {
            prt.div(out1)
            prt.div(paste0("    ", out2))
        } else {
            cat(out, "\n")
        }
        cat(paste0("Var(",Tn,") = ", S2Tr, "/", N, " - ", round(abs(ETr),dig), 
            "\U00B2 = ", round(VTr,dig)), "\n")
    }

  # Return the results
    ## out <- list(Ex, Ey, Vx, Vy, Exy, Vxy, Cxy, ETr, VTr)
    ## names(out) <- c(paste0("E",Xn), paste0("D",Xn), paste0("E",Yn), paste0("D",Yn),
    ##     paste0("E",Xn,Yn), paste0("V",Xn,Yn), paste0("C",Xn,Yn), "ETr", "VTr")
    if (Do.tr) {
        out <- list(ET=ETr, VT=VTr, 
                 EX=Ex, EY=Ey, VX=Vx, VY=Vy, EXY=Exy, VXY=Vxy, CXY=Cxy)
    } else {
        out <- list(EX=Ex, EY=Ey, VX=Vx, VY=Vy, EXY=Exy, VXY=Vxy, CXY=Cxy)
    }
    invisible(out)
}

clean.fn <- function(cfun) {
    cfun <- gsub(" ", "", cfun)
    if (grepl("[<>]", cfun)) {
        if (grepl("^ifelse", cfun)) {
            dd <- strsplit(cfun,",")[[1]]
            mm <- which.max(c(0, nchar(dd[2:length(dd)])))
            cfun <- dd[mm]
        } else if (grepl("^if", cfun)) {
            stop("Use ifelse... instead of if... else...")
        } else {
            dd <- strsplit(cfun,"(\\&|\\*)")[[1]]
            rr <- grep("[<>]", dd)
            cfun <- paste0(dd[-rr], collapse="*")
        }
    }
    cfun
}

# [5-4] Joint pdf and Expected Values of Two Continuous Random Variables

#' @title Expected Values of Two Continuous Random Variables
#' @description Joint pdf and Expected Values of Two Continuous Random Variables
#' @param fun Continuous joint PDF as string "f(x,y)".
#' @param lb Lower bound (numeric) of X and Y.
#' @param ub Upper bound (numeric) of X and Y.
#' @param y1 Lower limit (function or strings) of Y.
#' @param y2 Upper limit (function or strings) of Y.
#' @param Tr Transformation formula in string.
#' @param prt Print option: one of c("exp", "var", "cov", "cor"), Default: "exp".
#' @param dig Number of decimal places, Default: 4.
#' @param frac Logical: convert the result to fraction? Default: FALSE.
#'
#' @return list(EX=E(X), EY=E(Y), VX=V(X), VY=V(Y), EXY=E(X,Y), VXY=Cov(X,Y), CXY=Corr(X,Y))
#' @examples
#' # [Ex1] f(x,y) = 0.5*(x+3*y), (0<x<1, 0<y<1)
#' cont.jexp("(x+3*y)", 0, 1, prt="cor")
#'
#' # [Ex2] f(x,y) = 2*(x+y), (0<x<y<1)
#' cont.jexp("(x+y)", 0, 1, y1="x", prt="cor")
#'
#' # [Ex3] fxy = 2*exp(-(x+y)), (0<x<y<Inf)
#' cont.jexp("exp(-(x+y))", 0, Inf, y1="x", prt="cor")
#'
#' # [Ex4] fxy = 24*x*y, (0<x<1, 0<y<1, x+y<1)
#' cont.jexp("x*y", 0, 1, y2="1-x", prt="cor")
#'
#' @rdname cont.jexp
#' @export
# [Fortify Print 2023.5.22] ------[dint2.mc] based------------
cont.jexp <- function(fun, lb, ub, y1, y2, Tr, prt="exp", dig=4, frac=FALSE) {

  # Check Input
    if (missing(fun)) stop("Input fun as a string format or a function...")
    if (missing(lb) || missing(ub)) stop("Input lb, ub, ...")
    if (!is.numeric(lb) || !is.numeric(ub)) stop("Input lb, ub as numerics...")
    if (length(lb)==1) lb <- rep(lb, 2)
    if (length(ub)==1) ub <- rep(ub, 2)

  # Check transformation
    Do.tr <- FALSE
    if (!missing(Tr)) {
        if (!is.character(Tr)) stop("Input Tr as string.")
        Do.tr <- TRUE
        Tn <- toupper(deparse(substitute(Tr)))
        if (grepl("\"", Tn)) Tn <- gsub("\"", "", Tn)
    }

  # Functional Input
    if (is.function(fun)) {
        cfxy <- deparse(body(fun))
      # Cleaning
        cfxy <- clean.fn(cfxy)
    } else if (is.character(fun)) {
        cfxy <- fun
    } else {
        stop("Input fun as a string format or a function...")
    }

  # Check the main function ##+
    out <- core.str(cfxy)
    Builtin <- as.logical(out[length(out)])

  # Check the joint PDF
    if (missing(y1)) {
        if (missing(y2)) {
            prob <- dint2.mc(cfxy, lb[1], ub[1])
        } else {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y2=y2)
        }
    } else {
        if (missing(y2)) {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y1=y1)
        } else {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y1, y2)
        }
    }
    if (frac) prob <- MASS::fractions(prob)
    err <- 1e-7
    if (abs(prob-1) > err) {
        coef <- MASS::fractions(1/prob)
        cfxy <- paste0(coef, "*(", cfxy, ")")
        pcfxy <- remove.par(cfxy)
        cat(paste("Coef =", coef, " =>  f(x,y) =", pcfxy), "\n")
    }

  # Create the joint PDF and moment functions
    fxy <- str2fn(cfxy, "x+y")
    cex1 <- paste0("x*(", cfxy, ")")
    cex2 <- paste0("x^2*(", cfxy, ")")
    cey1 <- paste0("y*(", cfxy, ")")
    cey2 <- paste0("y^2*(", cfxy, ")")
    cexy <- paste0("x*y*(", cfxy, ")")

  # Mean and variance of random variable X and Y
    if (missing(y1)) y1 <- lb[2]
    if (missing(y2)) y2 <- ub[2]
    Ex1 <- dint2.mc(cex1, lb[1], ub[1], y1, y2)
    Ex2 <- dint2.mc(cex2, lb[1], ub[1], y1, y2)
    Ey1 <- dint2.mc(cey1, lb[1], ub[1], y1, y2)
    Ey2 <- dint2.mc(cey2, lb[1], ub[1], y1, y2)
    Exy <- dint2.mc(cexy, lb[1], ub[1], y1, y2)

  # Variance, Covariance, Correlation of X and Y
    Vx <- Ex2 - Ex1^2
    Vy <- Ey2 - Ey1^2
    Dx <- sqrt(Vx)
    Dy <- sqrt(Vy)
    Vxy <- Exy - Ex1*Ey1
    Cxy <- Vxy/sqrt(Vx*Vy)

  # Display output
    pint1 <- paste0("\U222B_[", lb[1], ":", ub[1], "]") # outer int
    pint2 <- paste0("\U222B_[", y1, ":", y2, "] {") # inner int
    pint <- paste0(pint1, pint2)
    ndsp <- function(val) ifelse(val>0, round(val, dig), 
              paste0("(", round(val, dig), ")"))
    bound2 <- paste0(":[<", ub[1], ">-<", lb[1], ">]") # outer bound
    bound1 <- paste0(":[<", y2, ">-<", y1, ">]") # inner bound

    if (prt %in% c("exp", "var", "cov", "cor")) {
      # E(X) and E(Y)
        fix11 <- Int0.str(cex1, "y")
        fiy11 <- Int0.str(cey1, "y")
        if (all(c(!Builtin, !is.na(fix11), !is.na(fiy11), prt=="exp", !Do.tr))) {
            lix11 <- gsub("y", y1, fix11)
            uix11 <- gsub("y", y2, fix11)
            liy11 <- gsub("y", y1, fiy11)
            uiy11 <- gsub("y", y2, fiy11)

            vix11 <- Sim.str(paste0(uix11, "-(", lix11, ")"))
            viy11 <- Sim.str(paste0(uiy11, "-(", liy11, ")"))
            fix12 <- Int0.str(vix11, "x")
            fiy12 <- Int0.str(viy11, "x")

            lix12 <- round(Eval.str(fix12, lb[1], "x"), dig)
            uix12 <- round(Eval.str(fix12, ub[1], "x"), dig)
            liy12 <- round(Eval.str(fiy12, lb[1], "x"), dig)
            uiy12 <- round(Eval.str(fiy12, ub[1], "x"), dig)

            mix11 <- paste0("     = ", pint1, "{", fix11, bound1, "}dx")
            miy11 <- paste0("     = ", pint1, "{", fiy11, bound1, "}dx")
            if (nchar(mix11)>80) mix11 <- prt.div2(mix11,75)
            if (nchar(miy11)>80) miy11 <- prt.div2(miy11,75)

            dix11 <- paste0("     = ", pint1, "{", vix11, "}dx")
            diy11 <- paste0("     = ", pint1, "{", viy11, "}dx")
            if (nchar(mix11)>80) dix11 <- prt.div2(dix11,75)
            if (nchar(miy11)>80) diy11 <- prt.div2(diy11,75)

            pcex1 <- remove.par(cex1)
            pcey1 <- remove.par(cey1)
            cat(paste0("E(X) = ", pint, pcex1, "}dy dx",
                "\n", mix11, "\n", dix11, 
                "\n     = ", fix12, bound2,
                "\n     = ", uix12, "-", ndsp(lix12)), 
                "=", round(Ex1, dig), "\n")
            cat(paste0("E(Y) = ", pint, pcey1, "}dy dx",
                "\n", miy11, "\n", diy11, 
                "\n     = ", fiy12, bound2,
                "\n     = ", uiy12, "-", ndsp(liy12)), 
                "=", round(Ey1, dig), "\n")
        } else {
            cat(paste0("E(X) = ", pint, pcex1, "}dy dx"),
                "=", round(Ex1, dig), "\n")
            cat(paste0("E(Y) = ", pint, pcey1, "}dy dx"),
                "=", round(Ey1, dig), "\n")
        }
    }

    if (prt %in% c("var", "cor")) {
        fix21 <- Int0.str(cex2, "y")
        fiy21 <- Int0.str(cey2, "y")
        if (all(c(!Builtin, !is.na(fix21), !is.na(fiy21), prt=="var", !Do.tr))) {
            lix21 <- gsub("y", y1, fix21)
            uix21 <- gsub("y", y2, fix21)
            liy21 <- gsub("y", y1, fiy21)
            uiy21 <- gsub("y", y2, fiy21)

            vix21 <- Sim.str(paste0(uix21, "-(", lix21, ")"))
            viy21 <- Sim.str(paste0(uiy21, "-(", liy21, ")"))
            fix22 <- Int0.str(vix21, "x")
            fiy22 <- Int0.str(viy21, "x")

            lix22 <- round(Eval.str(fix22, lb[1], "x"), dig)
            uix22 <- round(Eval.str(fix22, ub[1], "x"), dig)
            liy22 <- round(Eval.str(fiy22, lb[1], "x"), dig)
            uiy22 <- round(Eval.str(fiy22, ub[1], "x"), dig)

            mix21 <- paste0("      = ", pint1, "{", fix21, bound1, "}dx")
            miy21 <- paste0("      = ", pint1, "{", fiy21, bound1, "}dx")
            if (nchar(mix21)>80) mix21 <- prt.div2(mix21,75)
            if (nchar(miy21)>80) miy21 <- prt.div2(miy21,75)

            dix21 <- paste0("     = ", pint1, "{", vix21, "}dx")
            diy21 <- paste0("     = ", pint1, "{", viy21, "}dx")
            if (nchar(mix21)>80) dix21 <- prt.div2(dix21,75)
            if (nchar(miy21)>80) diy21 <- prt.div2(diy21,75)

            pcex2 <- remove.par(cex2)
            pcey2 <- remove.par(cey2)
            cat(paste0("E(X\U00B2) = ", pint, pcex2, "}dy dx",
                "\n", mix21, "\n", dix21, 
                "\n      = ", fix22, bound2,
                "\n      = ", uix22, "-", ndsp(lix22)), 
                "=", round(Ex2, dig), "\n")
            cat(paste0("E(Y\U00B2) = ", pint, pcey2, "}dy dx",
                "\n", miy21, "\n", diy21, 
                "\n      = ", fiy22, bound2,
                "\n      = ", uiy22, "-", ndsp(liy22)), 
                "=", round(Ey2, dig), "\n")
        } else {
            cat(paste0("E(X\U00B2) = ", pint, pcex2, "}dy dx"),
                "=", round(Ex2, dig), "\n")
            cat(paste0("E(Y\U00B2) = ", pint, pcey2, "}dy dx"),
                "=", round(Ey2, dig), "\n")
        }
        cat(paste0("Var(X) = ", round(Ex2,dig), " - ", round(abs(Ex1),dig), 
            "\U00B2 = ", round(Vx,dig)), "\n")
        cat(paste0("Var(Y) = ", round(Ey2,dig), " - ", round(abs(Ey1),dig), 
            "\U00B2 = ", round(Vy,dig)), "\n")
    }
    if (prt %in% c("cov", "cor")) {
        fixy1 <- Int0.str(cexy, "y")
        pcexy <- remove.par(cexy)

        if (all(c(!Builtin, !is.na(fixy1), prt=="cov", !Do.tr))) {
            lixy1 <- gsub("y", y1, fixy1)
            uixy1 <- gsub("y", y2, fixy1)

            vixy1 <- Sim.str(paste0(uixy1, "-(", lixy1, ")"))
            fixy2 <- Int0.str(vixy1, "x")

            lixy2 <- round(Eval.str(fixy2, lb[1], "x"), dig)
            uixy2 <- round(Eval.str(fixy2, ub[1], "x"), dig)

            mixy1 <- paste0("      = ", pint1, "{", fixy1, bound1, "}dx")
            if (nchar(mixy1)>80) mixy1 <- prt.div2(mixy1,75)

            dixy1 <- paste0("      = ", pint1, "{", vixy1, "}dx")
            if (nchar(mixy1)>80) dixy1 <- prt.div2(dixy1,75)

            cat(paste0("E(XY) = ", pint, pcexy, "}dy dx",
                "\n", mixy1, "\n", dixy1,
                "\n      = ", fixy2, bound2,
                "\n      = ", uixy2, "-", ndsp(lixy2)), 
                "=", round(Exy, dig), "\n")
        } else {
            cat(paste0("E(XY) = ", pint, pcexy, "}dy dx"),
                "=", round(Exy, dig), "\n")
        }
        cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - (", round(Ex1,dig), 
            ") \U00D7 (", round(Ey1,dig), ") = ", round(Vxy,dig)), "\n")
    }
    if (prt=="cor") {
        cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig),
            " \U00D7 ", round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
    }

  # Transformation -----------------------
    ET <- NA
    if (Do.tr) { 
        deco <- Decomp.str(Tr)
        if (length(deco)==2 || deco[1]=="*") {
            ceT <- paste0(Tr, "*(", cfxy, ")")
        } else {
            ceT <- paste0("(", Tr ,")*(", cfxy, ")")
        }
        ET <- dint2.mc(ceT, lb[1], ub[1], y1, y2)

        ceT2 <- paste0("(", Tr ,")^2*(", cfxy, ")")
        ET2 <- dint2.mc(ceT2, lb[1], ub[1], y1, y2)
        VT <- ET2 - ET^2
    }

    if (Do.tr && prt %in% c("exp", "var")) {
        fiT1 <- Int0.str(ceT, "y")
        pceT <- remove.par(ceT)

        if (!Builtin && !is.na(fiT1)) {
            liT1 <- ifelse(y1==-Inf, 0, gsub("y", y1, fiT1))
            uiT1 <- ifelse(y2== Inf, 0, gsub("y", y2, fiT1))

            viT1 <- Sim.str(paste0(uiT1, "-(", liT1, ")"))
            fiT2 <- Int0.str(viT1, "x")

            liT2 <- round(Eval.str(fiT2, lb[1], "x"), dig)
            uiT2 <- round(Eval.str(fiT2, ub[1], "x"), dig)

            miT1 <- paste0("     = ", pint1, "{", fiT1, bound1, "}dx")
            if (nchar(miT1)>80) miT1 <- prt.div2(miT1,75)

            diT1 <- paste0("     = ", pint1, "{", viT1, "}dx")
            if (nchar(miT1)>80) diT1 <- prt.div2(diT1,75)

            wiT1 <- paste0("     = ", fiT2, bound2)
            if (nchar(wiT1)>80) wiT1 <- prt.div2(wiT1,75)

            cat(paste0("E(", Tn, ") = ", pint, pceT, "}dy dx",
                "\n", miT1, "\n", diT1, "\n", wiT1, 
                "\n     = ", uiT2, "-", ndsp(liT2)), 
                "=", round(ET, dig), "\n")
        } else {
            cat(paste0("E(", Tn, ") = ", pint, pceT, "}dy dx"),
                "=", round(ET, dig), "\n")
        }
    }
    if (Do.tr && prt == "var") {
        giT1 <- Int0.str(ceT2, "y")
        pceT2 <- remove.par(ceT2)

        if (!Builtin && !is.na(giT1)) {
            liT1 <- ifelse(y1==-Inf, 0, gsub("y", y1, giT1))
            uiT1 <- ifelse(y2== Inf, 0, gsub("y", y2, giT1))

            viT1 <- Sim.str(paste0(uiT1, "-(", liT1, ")"))
            giT2 <- Int0.str(viT1, "x")

            liT2 <- round(Eval.str(giT2, lb[1], "x"), dig)
            uiT2 <- round(Eval.str(giT2, ub[1], "x"), dig)

            miT1 <- paste0("     = ", pint1, "{", giT1, bound1, "}dx")
            if (nchar(miT1)>80) miT1 <- prt.div2(miT1,75)

            diT1 <- paste0("     = ", pint1, "{", viT1, "}dx")
            if (nchar(miT1)>80) diT1 <- prt.div2(diT1,75)

            wiT1 <- paste0("     = ", giT2, bound2)
            if (nchar(wiT1)>80) wiT1 <- prt.div2(wiT1,75)

            oTn <- ifelse(nchar(Tn)>2, paste0("{",Tn,"}"), Tn)
            cat(paste0("E(", oTn, "\U00B2) = ", pint, pceT2, "}dy dx",
                "\n", miT1, "\n", diT1, "\n", wiT1, 
                "\n     = ", uiT2, "-", ndsp(liT2)), 
                "=", round(ET2, dig), "\n")
        } else {
            cat(paste0("E(", oTn, "\U00B2) = ", pint, pceT2, "}dy dx"),
                "=", round(ET2, dig), "\n")
        }
        cat(paste0("Var(",Tn,") = ", round(ET2,dig), " - ", round(abs(ET),dig), 
            "\U00B2 = ", round(VT,dig)), "\n")
    }
  # Return the results
    if (Do.tr) {
        out <- list(ET=ET, VT=VT,
              EX=Ex1, EY=Ey1, VX=Vx, VY=Vy, EXY=Exy, VXY=Vxy, CXY=Cxy)
    } else {
        out <- list(EX=Ex1, EY=Ey1, VX=Vx, VY=Vy, EXY=Exy, VXY=Vxy, CXY=Cxy)
    }
    invisible(out)
}

# [Modified] ------[dint2.mc] based------------
cont.jexp3 <- function(fun, lb, ub, y1, y2, Tr, prt="exp", dig=4, frac=FALSE) {

  # Check Input
    if (missing(fun)) stop("Input fun as a string format or a function...")
    if (missing(lb) || missing(ub)) stop("Input lb, ub, ...")
    if (!is.numeric(lb) || !is.numeric(ub)) stop("Input lb, ub as numerics...")
    if (length(lb)==1) lb <- rep(lb, 2)
    if (length(ub)==1) ub <- rep(ub, 2)

  # Check transformation
    Do.tr <- FALSE
    if (!missing(Tr)) {
        Do.tr <- TRUE
        if (!is.character(Tr)) stop("Input Tr as string.")
    }

  # Functional Input
    if (is.function(fun)) {
        cfxy <- deparse(body(fun))
      # Cleaning
        cfxy <- clean.fn(cfxy)
    } else if (is.character(fun)) {
        cfxy <- fun
    } else {
        stop("Input fun as a string format or a function...")
    }

  # Check the joint PDF
    if (missing(y1)) {
        if (missing(y2)) {
            prob <- dint2.mc(cfxy, lb[1], ub[1])
        } else {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y2=y2)
        }
    } else {
        if (missing(y2)) {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y1=y1)
        } else {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y1, y2)
        }
    }
    if (frac) prob <- MASS::fractions(prob)
    err <- 1e-7
    if (abs(prob-1) > err) {
        coef <- MASS::fractions(1/prob)
        cfxy <- paste0(coef, "*(", cfxy, ")")
        cat(paste("Coef =", coef, " =>  f(x,y) =", cfxy), "\n")
    }

  # Create the joint PDF and moment functions
    fxy <- str2fn(cfxy, "x+y")
    cex1 <- paste0("x*(", cfxy, ")")
    cex2 <- paste0("x^2*(", cfxy, ")")
    cey1 <- paste0("y*(", cfxy, ")")
    cey2 <- paste0("y^2*(", cfxy, ")")
    cexy <- paste0("x*y*(", cfxy, ")")

  # Mean and variance of random variable X and Y
    if (missing(y1)) y1 <- lb[2]
    if (missing(y2)) y2 <- ub[2]
    Ex1 <- dint2.mc(cex1, lb[1], ub[1], y1, y2)
    Ex2 <- dint2.mc(cex2, lb[1], ub[1], y1, y2)
    Ey1 <- dint2.mc(cey1, lb[1], ub[1], y1, y2)
    Ey2 <- dint2.mc(cey2, lb[1], ub[1], y1, y2)
    Exy <- dint2.mc(cexy, lb[1], ub[1], y1, y2)

  # Variance, Covariance, Correlation of X and Y
    Vx <- Ex2 - Ex1^2
    Vy <- Ey2 - Ey1^2
    Dx <- sqrt(Vx)
    Dy <- sqrt(Vy)
    Vxy <- Exy - Ex1*Ey1
    Cxy <- Vxy/sqrt(Vx*Vy)

  # Transformation
    ETr <- NA
    if (Do.tr) {
        deco <- Decomp.str(Tr)
        if (length(deco)==2 || deco[1]=="*") {
            ceT <- paste0(Tr, "*(", cfxy, ")")
        } else {
            ceT <- paste0("(", Tr ,")*(", cfxy, ")")
        }
        ETr <- dint2.mc(ceT, lb[1], ub[1], y1, y2)
    }

  # Display output
    if (prt %in% c("exp", "var", "cov", "cor")) {
        pint1 <- paste0("\U222B_[", lb[1], ":", ub[1], "]")
        pint2 <- paste0("\U222B_[", y1, ":", y2, "] {")
        pint <- paste0(pint1, pint2)
        ndsp <- function(val) ifelse(val>0, round(val, dig), 
                paste0("(", round(val, dig), ")"))

        cat(paste0("E(X) = ", pint, cex1, "}dy dx"),
                "=", round(Ex1, dig), "\n")
        cat(paste0("E(Y) = ", pint, cey1, "}dy dx"),
                "=", round(Ey1, dig), "\n")
    }
    if (prt %in% c("var", "cor")) {
        cat(paste0("E(X\U00B2) = ", pint, cex2, "}dy dx"),
                "=", round(Ex2, dig), "\n")
        cat(paste0("E(Y\U00B2) = ", pint, cey2, "}dy dx"),
                "=", round(Ey2, dig), "\n")
        cat(paste0("Var(X) = ", round(Ex2,dig), " - ", round(abs(Ex1),dig), 
            "\U00B2 = ", round(Vx,dig)), "\n")
        cat(paste0("Var(Y) = ", round(Ey2,dig), " - ", round(abs(Ey1),dig), 
            "\U00B2 = ", round(Vy,dig)), "\n")
    }
    if (prt %in% c("cov", "cor")) {
        cat(paste0("E(XY) = ", pint, cexy, "}dy dx"),
                "=", round(Exy, dig), "\n")
        cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - (", round(Ex1,dig), 
            ") \U00D7 (", round(Ey1,dig), ") = ", round(Vxy,dig)), "\n")
    }
    if (grepl("cor", prt)) {
        cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig),
            " \U00D7 ", round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
    }

  # Transformation
    if (Do.tr) {
        cat(paste0("E(", Tr, ") = ", pint, ceT, "}dy dx"),
                "=", round(ETr, dig), "\n")
    }

    # Return the results
    out = list(Ex=Ex1, Dx=Dx, Ey=Ey1, Dy=Dy, Exy=Exy, Vxy=Vxy, Cxy=Cxy, ETr=ETr)
    invisible(out)
}

# Symbolic Method ----------------------------
cont.jexp2 <- function(fun, lb, ub, y1, y2, Tr, prt="exp", dig=4, frac=FALSE) {

  # Check Input
    if (missing(fun)) stop("Input fun as a string format or a function...")
    if (missing(lb) || missing(ub)) stop("Input lb, ub, ...")
    if (!is.numeric(lb) || !is.numeric(ub)) stop("Input lb, ub as numerics...")
    if (length(lb)==1) lb <- rep(lb, 2)
    if (length(ub)==1) ub <- rep(ub, 2)

  # Functional Input
    if (is.function(fun)) {
        cfxy <- deparse(body(fun))
      # Cleaning
        cfxy <- clean.fn(cfxy)
    } else if (is.character(fun)) {
        cfxy <- fun
    } else {
        stop("Input fun as a string format or a function...")
    }

  # Check the joint PDF
    if (missing(y1)) {
        if (missing(y2)) {
            prob <- dint2.mc(cfxy, lb[1], ub[1])
        } else {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y2=y2)
        }
    } else {
        if (missing(y2)) {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y1=y1)
        } else {
            prob <- dint2.mc(cfxy, lb[1], ub[1], y1, y2)
        }
    }
    if (frac) prob <- MASS::fractions(prob)
    err <- 1e-7
    if (abs(prob-1) > err) {
        coef <- MASS::fractions(1/prob)
        cfxy <- paste0(coef, "*(", cfxy, ")")
        cat(paste("Coef =", coef, " =>  f(x,y) =", cfxy), "\n")
    }

  # Obtain moments
    if (missing(y1)) {
        if (missing(y2)) {
            mom <- mom2.mc(cfxy, lb, ub)
        } else {
            mom <- mom2.mc(cfxy, lb, ub, y2=y2)
        }
    } else {
        if (missing(y2)) {
            mom <- mom2.mc(cfxy, lb, ub, y1=y1)
        } else {
            mom <- mom2.mc(cfxy, lb, ub, y1=y1, y2=y2)
        }
    }

    Ex1 <- mom["Ex1"]
    Ex2 <- mom["Ex2"]
    Ey1 <- mom["Ey1"]
    Ey2 <- mom["Ey2"]
    Exy <- mom["Exy"]

  # Variance, Covariance, Correlation of X and Y
    Vx <- mom["Vx"]
    Vy <- mom["Vy"]
    Dx <- sqrt(Vx)
    Dy <- sqrt(Vy)
    Vxy <- Exy - Ex1*Ey1
    Cxy <- Vxy/sqrt(Vx*Vy)

  # Display output
    if (prt %in% c("exp", "var", "cov", "cor")) {
      # Create the joint PDF and moment functions
        cex1 <- paste0("x*(", cfxy, ")")
        cex2 <- paste0("x^2*(", cfxy, ")")
        cey1 <- paste0("y*(", cfxy, ")")
        cey2 <- paste0("y^2*(", cfxy, ")")
        cexy <- paste0("x*y*(", cfxy, ")")

        if (missing(y1)) y1 <- lb[2]
        if (missing(y2)) y2 <- ub[2]
        pint1 <- paste0("\U222B_[", lb[1], ":", ub[1], "]")
        pint2 <- paste0("\U222B_[", y1, ":", y2, "] {")
        pint <- paste0(pint1, pint2)

        cat(paste0("E(X) = ", pint, cex1, "}dy dx"),
                "=", round(Ex1, dig), "\n")
        cat(paste0("E(Y) = ", pint, cey1, "}dy dx"),
                "=", round(Ey1, dig), "\n")
    }
    if (prt %in% c("var", "cor")) {
        cat(paste0("E(X\U00B2) = ", pint, cex2, "}dy dx"),
                "=", round(Ex2, dig), "\n")
        cat(paste0("E(Y\U00B2) = ", pint, cey2, "}dy dx"),
                "=", round(Ey2, dig), "\n")
        cat(paste0("Var(X) = ", round(Ex2,dig), " - ", round(abs(Ex1),dig), 
            "\U00B2 = ", round(Vx,dig)), "\n")
        cat(paste0("Var(Y) = ", round(Ey2,dig), " - ", round(abs(Ey1),dig), 
            "\U00B2 = ", round(Vy,dig)), "\n")
    }
    if (prt %in% c("cov", "cor")) {
        cat(paste0("E(XY) = ", pint, cexy, "}dy dx"),
                "=", round(Exy, dig), "\n")
        cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - (", round(Ex1,dig), 
            ") \U00D7 (", round(Ey1,dig), ") = ", round(Vxy,dig)), "\n")
    }
    if (grepl("cor", prt)) {
        cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig),
            " \U00D7 ", round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
    }
    # Return the results
    out = list(Ex=Ex1, Dx=Dx, Ey=Ey1, Dy=Dy, Exy=Exy, Vxy=Vxy, Cxy=Cxy)
    invisible(out)
}

# [Replaced: require Big number] -------------------------------
cont.jexp1 <- function(fun, x1, x2, y1, y2, prt="exp", dig=4, frac=FALSE, Big=1000) {

  # Check Input
    if (missing(fun)) stop("Input fun as a string format or a function...")
    if (missing(x1) || missing(x2)) stop("Input x1, x2, ...")
    if (!is.numeric(x1) || !is.numeric(x2)) stop("Input x1, x2 as numerics...")
    if (missing(y1)) y1 <- x1
    if (missing(y2)) y2 <- x2

  # Functional Input
    if (is.function(fun)) {
        cfxy <- deparse(body(fun))
      # Cleaning
        cfxy <- clean.fn(cfxy)
    } else if (is.character(fun)) {
        cfxy <- fun
    } else {
        stop("Input fun as a string format or a function...")
    }

  # Check the joint PDF
    Use.str <- ifelse(is.character(y1)||is.character(y2), TRUE, FALSE)
    if (Use.str) {
      # Big number trick
        xb1 <- max(x1, -Big)
        xb2 <- min(x2, Big)
        yb1 <- ifelse (is.numeric(y1), max(y1, -Big), y1)
        yb2 <- ifelse (is.numeric(y2), min(y2, Big), y2)
        prob <- dint.mc(cfxy, xb1, xb2, yb1, yb2)
    } else {
        prob <- dint.cub(cfxy, x1, x2, y1, y2)
    }

    if (frac) prob <- MASS::fractions(prob)
    err <- 1e-7
    if (abs(prob-1) > err) {
        coef <- MASS::fractions(1/prob)
        cfxy <- paste0(coef, "*(", cfxy, ")")
        cat(paste("Coef =", coef, " =>  f(x,y) =", cfxy), "\n")
    }

  # Create the joint PDF and moment functions
    fxy <- str2fn(cfxy, "x+y")
    cex1 <- paste0("x*(", cfxy, ")")
    cex2 <- paste0("x^2*(", cfxy, ")")
    cey1 <- paste0("y*(", cfxy, ")")
    cey2 <- paste0("y^2*(", cfxy, ")")
    cexy <- paste0("x*y*(", cfxy, ")")

    ##fex1 <- str2fn(cex1, "x+y")
    ##fex2 <- str2fn(cex2, "x+y")
    ##fey1 <- str2fn(cey1, "x+y")
    ##fey2 <- str2fn(cey2, "x+y")
    ##fexy <- str2fn(cexy, "x+y")

  # Mean and variance of random variable X
    if (Use.str) {
        Ex1 <- dint.mc(cex1, xb1, xb2, yb1, yb2)
        Ex2 <- dint.mc(cex2, xb1, xb2, yb1, yb2)
        Ey1 <- dint.mc(cey1, xb1, xb2, yb1, yb2)
        Ey2 <- dint.mc(cey2, xb1, xb2, yb1, yb2)
        Exy <- dint.mc(cexy, xb1, xb2, yb1, yb2)
    } else {
        Ex1 <- dint.cub(cex1, x1, x2, y1, y2)
        Ex2 <- dint.cub(cex2, x1, x2, y1, y2)
        Ey1 <- dint.cub(cey1, x1, x2, y1, y2)
        Ey2 <- dint.cub(cey2, x1, x2, y1, y2)
        Exy <- dint.cub(cexy, x1, x2, y1, y2)
    }

  # Variance, Covariance, Correlation of X and Y
    Vx <- Ex2 - Ex1^2
    Vy <- Ey2 - Ey1^2
    Dx <- sqrt(Vx)
    Dy <- sqrt(Vy)
    Vxy <- Exy - Ex1*Ey1
    Cxy <- Vxy/sqrt(Vx*Vy)

  # Display output
    if (prt %in% c("exp", "var", "cov", "cor")) {
        pint1 <- paste0("\U222B_[", x1, ":", x2, "]")
        pint2 <- paste0("\U222B_[", y1, ":", y2, "] {")
        pint <- paste0(pint1, pint2)
        ndsp <- function(val) ifelse(val>0, round(val, dig), 
                paste0("(", round(val, dig), ")"))

        cat(paste0("E(X) = ", pint, cex1, "}dy dx"),
                "=", round(Ex1, dig), "\n")
        cat(paste0("E(Y) = ", pint, cey1, "}dy dx"),
                "=", round(Ey1, dig), "\n")
    }
    if (prt %in% c("var", "cor")) {
        cat(paste0("E(X\U00B2) = ", pint, cex2, "}dy dx"),
                "=", round(Ex2, dig), "\n")
        cat(paste0("E(Y\U00B2) = ", pint, cey2, "}dy dx"),
                "=", round(Ey2, dig), "\n")
        cat(paste0("Var(X) = ", round(Ex2,dig), " - ", round(abs(Ex1),dig), 
            "\U00B2 = ", round(Vx,dig)), "\n")
        cat(paste0("Var(Y) = ", round(Ey2,dig), " - ", round(abs(Ey1),dig), 
            "\U00B2 = ", round(Vy,dig)), "\n")
    }
    if (prt %in% c("cov", "cor")) {
        cat(paste0("E(XY) = ", pint, cexy, "}dy dx"),
                "=", round(Exy, dig), "\n")
        cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - (", round(Ex1,dig), 
            ") \U00D7 (", round(Ey1,dig), ") = ", round(Vxy,dig)), "\n")
    }
    if (grepl("cor", prt)) {
        cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig),
            " \U00D7 ", round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
    }
    # Return the results
    out = list(Ex=Ex1, Dx=Dx, Ey=Ey1, Dy=Dy, Exy=Exy, Vxy=Vxy, Cxy=Cxy)
    invisible(out)
}

# [Added] Panel functions for mult.corr() -----------------------
panel.corr <- function(x, y, dig=4) {
    ## usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    cv <- cov(x, y)
    tr <- format(c(r, 0.123456789), digits = dig)[1]
    ftr <- format(c(abs(r), 0.123456789), digits = dig)[1]
    sw <- 0.4/strwidth(ftr)
    scol <- 1
    if (r > 0) scol <- "red"
    if (r < 0) scol <- "blue"
    txt1 <- paste("Cov:", format(cv, digits = dig, nsmall = dig))
    txt2 <- paste("Corr:", tr)
    text(0.5, 0.6, txt2, cex=sw, col=scol)
    text(0.5, 0.3, txt1, cex=sw*0.85, col=1)
}

panel.bar <- function(x){
    usr <- par("usr"); on.exit(par(usr=usr))
    par(usr = c(usr[1], usr[2], 0, 1.5) )

    xf <- table(x)
    xf <- xf/max(xf)
    ## xp <- xf / sum(xf)
    ## bp <- barplot(xf, plot = FALSE)
    bp <- sort(unique(x))
    wt <- (bp[2]-bp[1])/2*0.2
    segments(bp, 0, bp, xf, lwd=3, col = "red") 
}

panel.lm <- function(x, y){
    tab <- table(x, y)
    avg <- max(mean(tab), 1)
    ss <- NULL
    for (k in 1:length(x)) ss <- 
        c(ss, tab[as.character(x[k]), as.character(y[k])])
    points(x, y, pch = 19, cex = sqrt(ss/avg), col = "blue")
    abline(lm(y ~ x), col="red")
}

#Panel of histograms
panel.hist <- function(x, ...){
    usr <- par("usr"); on.exit(par(usr=usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    len <- length(breaks)
    y <- h$counts/max(h$counts)
    ##rect(breaks[-len], 0, breaks[-1], y, col = "red")
    mid <- h$mid
    rect(mid-0.05, 0, mid+0.05, y, col = "red")
}

# [5-5] Correlation Coefficients and Scatter Plots
#' @title Correlation Coefficients and Scatter Plots
#' @description Correlation Coefficients and Scatter Plots of Random variables
#' @param data Input data frame (or list or matrix) of variables.
#' @param item Selected names of the variables.
#' @param prt Logical: print the result? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param detail Logical: print frequency tables? Default: FALSE.
#' @param ws Graphic window size, Default: "n".
#' @param ... Othe graphic parameters.
#' @return Corr(X,Y) table.
#' @examples
#' # Rolling three dice: (Sum, Range, Max, Min)
#' S <- rolldie2(3)
#' X = data.frame(Sum=apply(S, 1, sum), 
#'                Range=apply(S, 1, \(x) max(x)-min(x)),
#'                Max=apply(S, 1, max), 
#'                Min=apply(S, 1, min))
#' mult.corr(X, ws=c(7,6))
#' 
#' # "mtcars" data set: (mpg, disp, hp, drat, wt, qsec)
#' select = c("mpg", "disp", "hp", "drat", "wt", "qsec")
#' mult.corr(mtcars, select, ws=c(7,6))
#' @rdname mult.corr
#' @export
mult.corr <- function(data, item, prt=TRUE, dig=4, detail=FALSE, ws="n", ...) {
  # Check input
    if (missing(data)) stop("Input data as a data frame, list, or matrix.")
    if (!is.data.frame(data)) {
        if (is.list(data)) {
            data <- as.data.frame(data)

        } else if (is.matrix(data)) {
            data <- as.data.frame(data)
            colnames(data) <- item
        } else {
            stop("Input data as a data frame, list, or matrix.")
        }
    }

    if (missing(item)) {
        item <- colnames(data)
    } else {
        X <- data[item]
    }

  # Number of random variables
    nv <- length(X)

  # List of frequency distributions
    Xf <- list()
    for (k in 1:nv) Xf[[k]] <- table(X[[k]])

  # Print the frequency distribution
    if (detail) {
        for (k in 1:nv) {
            cat(paste0(item[k], "(X", k, ") frequency distribution"))
            print(Xf[[k]])
        }
    }

  # List of the values of random variable values
    Xv <- list()
    for (k in 1:nv) Xv[[k]] <- as.numeric(names(Xf[[k]]))

  # Probability distributions
    Xp <- list()
    N <- length(X[[1]])
    for (k in 1:nv) Xp[[k]] <- Xf[[k]] / N

  # Calculate expected values
    SX <- EX <- rep(NA, nv)
    for (k in 1:nv) {
        SX[k] <- (Xv[[k]] %*% Xf[[k]])[1,1]
        EX[k] <- SX[k]/N
    }

  # Calculate variances
    SX2 <- EX2 <- VX <- DX <- rep(NA, nv)
    for (k in 1:nv) {
        SX2[k] <- (Xv[[k]]^2 %*% Xf[[k]])[1,1]
        EX2[k] <- SX2[k]/N
        VX[k] <- EX2[k] - EX[k]^2
        DX[k] <- sqrt(VX[k])
    }

  # Calculate covariance & correlation coefficient
    SXY <- EXY <- VXY <- CXY <- matrix(NA, nv, nv)
    colnames(VXY) <- colnames(CXY) <- paste0("X",1:nv)
    rownames(VXY) <- rownames(CXY) <- paste0("X",1:nv)
    for (k in 1:nv) for (m in 1:nv) {
        XYf <- table(X[[k]], X[[m]])
        SXY[k,m] <- (as.vector(Xv[[k]] %o% Xv[[m]]) %*% as.vector(XYf))[1,1]
        XYp <- XYf / N
        EXY[k,m] <- SXY[k,m] / N
        VXY[k,m] <- EXY[k,m] - EX[k]*EX[m]
        CXY[k,m] <- VXY[k,m] / (VX[k] * VX[m])^0.5
    }
    Cxy <- cor(X)

  # Display output
    if (prt) {
        cat("Correlation Coefficients & E(X) and V(X) -----------\n")
        df1 <- as.data.frame(round(Cxy, dig))
        bar <- rep("|", nv)
        df1 <- cbind(df1, bar, round(EX, dig), round(VX, dig))
        ##tab1 <- cbind(CXY, EX, VX)
        colnames(df1)[nv+1:3] <- c("|", "E(X)", "V(X)")
        print(df1)
    }
    # Display plots -----------------------------------------
    if (is.numeric(ws)) {
      # Open graphic window
        win.graph(ws[1], ws[2])
        pm <- par()$mar
        mrat <- ws[2]/ws[1]
        if (ws[1] > ws[2]) par(mar=pm*c(mrat,1,mrat,1))
        par(mfrow = c(nv, nv))

      # Display Scatterplot matrix and Correlations
        pairs(X, lower.panel = panel.lm, upper.panel = panel.corr, 
             diag.panel = panel.bar, labels = names(X), 
             main="Scatterplot matrix and Correlations")
    }
    invisible(Cxy)
}

# [Added] [2023.05.15] --[mom2.mc]---[dint2.mc]--------------------
mom2.mc <- function(fun, lb, ub, y1, y2, debug0=FALSE, dig=4) {
  # require(mosaicCalc)
  # Check Input
    if (missing(fun)) stop("Input the PDF as a string format...")
    if (!is.character(fun)) stop("Input the PDF as a string format...")
    if (any(c(missing(lb),missing(ub)))) 
        stop("Input lb, ub...")
    if (any(c(!is.numeric(lb),!is.numeric(ub)))) 
        stop("Input lb, ub as numerics...")

    if (length(lb)==1) lb <- rep(lb,2)
    if (length(ub)==1) ub <- rep(ub,2)

  # Joint PDF: f(xx, yy) => E(XY)
    ccxy <- paste0("x*y*(", fun, ")")
    if (debug0) cat("ccxy =", ccxy, "\n")

    if (missing(y1)) {
        if (missing(y2)) {
            fn <- cont.jpdf(fun, lb, ub, prt=FALSE)
            Exy <- dint2.mc(ccxy, lb[1], ub[1])
        } else {
            fn <- cont.jpdf(fun, lb, ub, y2=y2, prt=FALSE)
            Exy <- dint2.mc(ccxy, lb[1], ub[1], y2=y2)
        }
    } else {
        if (missing(y2)) {
            fn <- cont.jpdf(fun, lb, ub, y1=y1, prt=FALSE)
            Exy <- dint2.mc(ccxy, lb[1], ub[1], y1=y1)
        } else {
            fn <- cont.jpdf(fun, lb, ub, y1=y1, y2=y2, prt=FALSE)
            Exy <- dint2.mc(ccxy, lb[1], ub[1], y1=y1, y2=y2)
        }
    }
    if (debug0) cat("Exy =", Exy, "\n")

  # Marginal PDF: f(xx) and f(yy)
    bfx <- body(fn$fx)
    dfx <- gsub(" ", "", as.character(bfx))
    if (debug0) cat("dfx =", dfx, "\n")
    if (dfx[1]=="ifelse") {
        cfx <- dfx[3]
    } else {
        cfx <- gsub(" ", "", deparse(bfx))
    }
    if (debug0) cat("cfx =", cfx, "\n")

    bfy <- body(fn$fy)
    dfy <- gsub(" ", "", as.character(bfy))
    if (debug0) cat("dfy =", dfy, "\n")
    if (dfy[1]=="ifelse") {
        cfy <- dfy[3]
    } else {
        cfy <- gsub(" ", "", deparse(bfy))
    }
    if (debug0) cat("cfy =", cfy, "\n")

    if (!missing(y1)) y1 <- gsub("x", "xx", y1)
    if (!missing(y2)) y2 <- gsub("x", "xx", y2)

  # E(X), E(X^2), V(X)
    cfx1 <- paste0("xx*(", cfx, ")")
    intx1 <- mosaicCalc::antiD(as.formula(paste(cfx1, "~", "xx")))
    xint1 <- gsub(" ", "", deparse(body(intx1)))
    xint1 <- gsub("C", 0, xint1)
    if (debug0) cat("xint1 =", xint1, "\n")
    xint11 <- gsub("xx", lb[1], xint1)
    xint12 <- gsub("xx", ub[1], xint1)
    if (debug0) cat("xint11 =", xint11, "\n")
    if (debug0) cat("xint12 =", xint12, "\n")
    Ex11 <- eval(parse(text=xint11))
    Ex11 <- ifelse(is.na(Ex11), 0, Ex11)
    Ex12 <- eval(parse(text=xint12))
    Ex12 <- ifelse(is.na(Ex12), 0, Ex12)
    Ex1 <- Ex12 - Ex11

    cfx2 <- paste0("xx^2*(", cfx, ")")
    intx2 <- mosaicCalc::antiD(as.formula(paste(cfx2, "~", "xx")))
    xint2 <- gsub(" ", "", deparse(body(intx2)))
    xint2 <- gsub("C", 0, xint2)
    if (debug0) cat("xint2 =", xint2, "\n")
    xint21 <- gsub("xx", lb[1], xint2)
    xint22 <- gsub("xx", ub[1], xint2)
    if (debug0) cat("xint21 =", xint21, "\n")
    if (debug0) cat("xint22 =", xint22, "\n")
    Ex21 <- eval(parse(text=xint21))
    Ex21 <- ifelse(is.na(Ex21), 0, Ex21)
    Ex22 <- eval(parse(text=xint22))
    Ex22 <- ifelse(is.na(Ex22), 0, Ex22)
    Ex2 <- Ex22 - Ex21
    Vx <- Ex2 - Ex1^2

  # E(Y), E(Y^2), V(Y)
    cfy1 <- paste0("yy*(", cfy, ")")
    inty1 <- mosaicCalc::antiD(as.formula(paste(cfy1, "~", "yy")))
    yint1 <- gsub(" ", "", deparse(body(inty1)))
    yint1 <- gsub("C", 0, yint1)
    if (debug0) cat("yint1 =", yint1, "\n")
    yint11 <- gsub("yy", lb[2], yint1)
    yint12 <- gsub("yy", ub[2], yint1)
    if (debug0) cat("yint11 =", yint11, "\n")
    if (debug0) cat("yint12 =", yint12, "\n")
    Ey11 <- eval(parse(text=yint11))
    Ey11 <- ifelse(is.na(Ey11), 0, Ey11)
    Ey12 <- eval(parse(text=yint12))
    Ey12 <- ifelse(is.na(Ey12), 0, Ey12)
    Ey1 <- Ey12 - Ey11

    cfy2 <- paste0("yy^2*(", cfy, ")")
    inty2 <- mosaicCalc::antiD(as.formula(paste(cfy2, "~", "yy")))
    yint2 <- gsub(" ", "", deparse(body(inty2)))
    yint2 <- gsub("C", 0, yint2)
    if (debug0) cat("yint2 =", yint2, "\n")
    yint21 <- gsub("yy", lb[2], yint2)
    yint22 <- gsub("yy", ub[2], yint2)
    if (debug0) cat("yint21 =", yint21, "\n")
    if (debug0) cat("yint22 =", yint22, "\n")
    Ey21 <- eval(parse(text=yint21))
    Ey21 <- ifelse(is.na(Ey21), 0, Ey21)
    Ey22 <- eval(parse(text=yint22))
    Ey22 <- ifelse(is.na(Ey22), 0, Ey22)
    Ey2 <- Ey22 - Ey21
    Vy <- Ey2 - Ey1^2

    out <- c(Ex1, Ex2, Vx, Ey1, Ey2, Vy, Exy)
    names(out) <- c("Ex1", "Ex2", "Vx", "Ey1", "Ey2", "Vy", "Exy")
  # Return output
    return(out)
}

dint2.mc <- function(fun, x1, x2, y1, y2, debug0=FALSE, dig=4) {
  # require(mosaicCalc)
  # Check Input
    if (missing(fun)) stop("Input the PDF as a string format...")
    if (!is.character(fun)) stop("Input the PDF as a string format...")
    if (any(c(missing(x1),missing(x2)))) 
        stop("Input x1, x2 ...")
    if (any(c(!is.numeric(x1),!is.numeric(x2)))) 
        stop("Input x1, x2 as numerics...")

    if (!missing(y1)) {
        if (is.character(y1)) {
            y1 <- gsub("x", "X", y1)
            if (nchar(y1)>2) y1 <- paste0("(", y1, ")")
        }
    } else {
        y1 <- x1
    }
    if (!missing(y2)) {
        if (is.character(y2)) {
            y2 <- gsub("x", "X", y2)
            if (nchar(y2)>2) y2 <- paste0("(", y2, ")")
        }
    } else {
        y2 <- x2
    }

    cfun <- gsub("x", "X", fun)
    cfun <- gsub("eXp", "exp", cfun)
    cfun <- gsub("y", "Y", cfun)
    if (debug0) cat("cfun =", cfun, "\n")

  # Integ(fun(X,Y)) w.r.t. Y => Fn(X)
  # antiD Error in exp() type => use Iex1.str() [2023.05.16]
    if (grepl("Y\\^2.*\\*exp\\(", cfun)) {
        fun0 <- sub("Y\\^2\\*", "", cfun)
        fun0 <- tolower(fun0)
        cint1 <- Iex1.str(fun0, "y")[2]
        cint1 <- gsub("y", "Y", cint1)
        cint1 <- gsub("x", "X", cint1)
        cint1 <- gsub("eXp", "exp", cint1)
    } else {
        int1 <- mosaicCalc::antiD(as.formula(paste(cfun, "~", "Y")))
        cint1 <- deparse(body(int1))
    }

    if (length(cint1) > 1) cint1 <- paste(cint1, collapse="")
    cint1 <- gsub("C", 0, cint1)
    cint1 <- gsub(" ", "", cint1)
    if (debug0) cat("cint1 =", cint1, "\n")
    cint11 <- gsub("Y", y1, cint1)
    cint12 <- gsub("Y", y2, cint1)
    if (debug0) cat("cint11 =", cint11, "\n")
    if (debug0) cat("cint12 =", cint12, "\n")
  # Handle Na, NaN
    cint11 <- gsub("exp\\(-\\(?X?.{1,3}Inf\\)?\\)", 0, cint11)
    cint11 <- gsub("Inf(\\^\\d)?\\*0", 0, cint11)
    cint11 <- gsub("X(\\^\\d)?\\*0", 0, cint11)
    cint11 <- gsub(" ", "", cint11)
    if (debug0) cat("[R1] cint11 =", cint11, "\n")
    cint11 <- Ryacas::yac(paste0("Simplify(", cint11, ")"))
    if (debug0) cat("[R2] cint11 =", cint11, "\n")
    cint12 <- gsub("exp\\(-\\(?X?.{1,3}Inf\\)?\\)", 0, cint12)
    if (debug0) cat("[R1] cint12 =", cint12, "\n")
    cint12 <- gsub("Inf(\\^\\d)?\\*0", 0, cint12)
    if (debug0) cat("[R2] cint12 =", cint12, "\n")
    cint12 <- gsub("X(\\^\\d)?\\*0", 0, cint12)
    cint12 <- gsub(" ", "", cint12)
    if (debug0) cat("[R3] cint12 =", cint12, "\n")
    cint12 <- Ryacas::yac(paste0("Simplify(", cint12, ")"))
    if (debug0) cat("[R4] cint12 =", cint12, "\n")

  # Integ(FX(X)) w.r.t. X
    if (cint11=="0") {
        o211 <- o212 <- 0
    } else {
        int21 <- mosaicCalc::antiD(as.formula(paste(cint11, "~", "X")))
        cint21 <- deparse(body(int21))
        cint21 <- gsub("C", 0, cint21)
        cint21 <- gsub(" ", "", cint21)
        cint211 <- gsub("X", x1, cint21)
        o211 <- eval(parse(text=cint211))
        cint212 <- gsub("X", x2, cint21)
        o212 <- eval(parse(text=cint212))
        if (debug0) cat("cint21 =", cint21, "\n")
    }

    if (cint12=="0") {
        o221 <- o222 <- 0
    } else {
        int22 <- mosaicCalc::antiD(as.formula(paste(cint12, "~", "X")))
        cint22 <- deparse(body(int22))
        cint22 <- gsub("C", 0, cint22)
        cint22 <- gsub(" ", "", cint22)
        cint221 <- gsub("X", x1, cint22)
        o221 <- eval(parse(text=cint221))
        cint222 <- gsub("X", x2, cint22)
        o222 <- eval(parse(text=cint222))
        if (debug0) cat("cint22 =", cint22, "\n")
    }

    ##if (debug0) print(c(o211, o212, o221, o222))
    o211 <- ifelse(is.na(o211), 0, o211)
    o212 <- ifelse(is.na(o212), 0, o212)
    o221 <- ifelse(is.na(o221), 0, o221)
    o222 <- ifelse(is.na(o222), 0, o222)

    if (debug0) {
        res <- c(o222, o221, o212, o211)
        names(res) <- c("222", "221", "212", "211")
        print(res)
    }

    out <- (o222-o221)-(o212-o211)
  # Return output
    return(out)
}
# [OLD] --------------------------------------------------------------
# [5-1]
disc.exp0 <- function(x, xf, prt="exp", dig=4, ws="n", ...) {
    # Check input
    if (missing(x)) stop("Input x as a table or a vector.")
    if (is.table(x)) {
        xv <- as.numeric(names(x))
        xf <- as.numeric(x)
    } else {
        xv <- x
        if (missing(xf)) stop("Input xf: the frequency or probability of x.")
    }
    # Set the name of random variable
    Xn <- toupper(deparse(substitute(x)))

    # Calculate the expected value
    N <- sum(xf)
    sm <- sum(xv*xf)
    ex <- sm/N
    rxv <- round(xv, dig)
    rxf <- round(xf, dig)
    rsm <- round(sm, dig)
    rex <- round(ex, dig)
    rN <- round(N, dig)
    xp <- xf/N
    rxp <- round(xp, dig)

    # Calculate variance & standard deviation
    xq <- xv^2
    sq <- sum(xq*xf)
    ex2 <- sq/N
    vx <- ex2 - ex^2
    dx <- sqrt(vx)
    rxq <- round(xq, dig)
    rsq <- round(sq, dig)
    rex2 <- round(ex2, dig)
    rvx <- round(vx, dig)
    rdx <- round(dx, dig)

    mult1 <- paste(paste(paste0(ifelse(rxv>=0,"+",""), rxv), 
	    rxf, sep="\U00D7"), collapse="")
    mult2 <- paste(paste(rxq, rxf, sep="\U00D7"), collapse="+")

    if (prt %in% c("exp", "var")) {
        if (N > 1.00001) {
            out <- paste0("E(",Xn, ") = (", mult1, ")/", rN, " = ", rsm, 
		"/", rN, " = ", rex)
        } else {
            out <- paste0("E(",Xn, ") = (", mult1, ") = ", rex)
        }

        if (nchar(out) > 68) {
            div <- (nchar(out)-8) %/% 60 + 1
            if (ex >= 0) {
                split <- stringr::str_split_1(out, "\\+")
                sign <- "+"
            } else {
                split <- stringr::str_split_1(out, "-")
                sign <- "-"
            }
            len <- cumsum(nchar(split))
            cut <- rep(1, div)
            for (k in 1:div) cut[k] <- max(which(len <= 8+60*k))

            cat(paste(split[1:cut[1]], collapse=sign), "\n")
            for (k in 2:div) if (cut[k]>cut[k-1]) cat(paste0("        +", 
                paste(split[(cut[k-1]+1):cut[k]], collapse=sign)), "\n")
        } else {
            cat(out, "\n")
        }
    }

    if (prt == "var") {
        if (N > 1.00001) {
            out <- paste0("E(",Xn, "\U00B2) = (", mult2, ")/", rN, " = ", 
		rsq, "/", rN, " = ", rex2)
        } else {
            out <- paste0("E(",Xn, "\U00B2) = (", mult2, ") = ", rex2)
        }

        if (nchar(out) > 68) {
            div <- (nchar(out)-8) %/% 60 + 1
            if (ex >= 0) {
                split <- stringr::str_split_1(out, "\\+")
                sign <- "+"
            } else {
                split <- stringr::str_split_1(out, "-")
                sign <- "-"
            }
            len <- cumsum(nchar(split))
            cut <- rep(1, div)
            for (k in 1:div) cut[k] <- max(which(len <= 8+60*k))

            cat(paste(split[1:cut[1]], collapse=sign), "\n")
            for (k in 2:div) if (cut[k]>cut[k-1]) cat(paste0("        +", 
                paste(split[(cut[k-1]+1):cut[k]], collapse=sign)), "\n")
        } else {
            cat(out, "\n")
        }

        cat(paste0("V(",Xn,") = ", rex2, " - ", rex, "\U00B2 = ", rvx), "\n")
        cat(paste0("D(",Xn,") = \U221A(", rvx, ") = ", rdx), "\n")
    }

    # Plot PDF f(x)
    if (is.numeric(ws)) {
        x1 <- min(xv)
        x2 <- max(xv)
        xr <- x2 - x1
        mt <- paste("Probability Distribution of", Xn)

        arg1 <- list(type="h", main=mt, pch=19, lwd=5, ylim=c(0, max(xp)*1.1),
                   xlim=c(x1-0.1*xr, x2+0.1*xr), col="red", xlab="x", ylab="f(x)")
        dots <- list(...)
        dd1 <- c("type", "main", "pch", "lwd", "ylim", "xlim", "col", "xlab", "ylab")

        cex <- 0.8
        pos <- 3
        col.text <- col.seg <- col.arr <- "blue"
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
        xylist <- list(x=xv, y=xp)
        do.call(plot, c(xylist, arg1, dots))

        ## plot(xv, xp, type="h", main=mt, pch=19, lwd=5, ylim=c(0, max(xp)*1.1),
        ##     xlim=c(x1-0.1*xr, x2+0.1*xr), col="red", xlab="x", ylab="f(x)")
        grid(col="green")

        # Display probability
        text(xv, xp, round(xp, dig), pos=pos, col=col.text, cex=cex)

        # Display the expected value
        ym <- 0.5*max(xp)
        segments(ex, 0, ex, ym, lty=lty.seg, col=col.seg)
        text(ex, ym, paste0("E(", Xn, ")=",round(ex, dig)), pos=pos, col=col.text)

        # Display the standard deviation
        x1 <- ex - dx
        x2 <- ex + dx
        arrows(ex, ym, x2, ym, length=0.1, lty=2, col=col.arr)
        arrows(ex, ym, x1, ym, length=0.1, lty=2, col=col.arr)

        # Display legend
        legend(pos.leg, c(paste0("E(", Xn, ") = ",round(ex,dig)),
            paste0("D(", Xn, ") = ", round(dx,dig))), bg="white")
    }
    invisible(list(Ex=ex, Dx=dx, Vx=vx))
}

# [5-3] Expected Values of a Function of Continuous Random Variable

#' @title Expected Values of a Function of Continuous Random Variable
#' @description Calculate E(g(X)) of a Continuous Random Variable X
#' @param gx Function of X for which the expected value will be obtained.
#' @param fx Continuous PDF of X.
#' @param lo Lower limit of X, Default: -Inf.
#' @param up Upper limit of X, Default: Inf.
#' @param dig Number of decimal places, Default: 4.
#' @param prt Print option: one of c("exp", "var"), Default: "exp".
#' @param err Error limit for integration, Default: 1E-06.
#' @return list(Ex=E(X), Vx=Var(X), Dx=D(X))
#' @examples
#' fx <- function(x) exp(-2*x)*(x>0)
#' y <- function(x) 3*x^2 - 3
#' cont.fnexp(y, fx, 0, Inf, prt="var")
#'
#' @rdname cont.fnexp
#' @export

cont.fnexp <- function(gx, fx, lo=-Inf, up=Inf, dig=4, prt="exp", err=1E-6) {
    # Check the pdf
    Yn <- toupper(deparse(substitute(gx)))
    Yf <- deparse(gx)[2]
    coef <- 1
    int <- "\U222B {"
    prob <- integrate(fx, lo, up)[[1]]

    if (abs(prob-1) > err) {
        coef <- 1/prob
        cat("Coefficient", coef, "must be multiplied.\n")
        int <- paste (round(coef, dig), "\U00D7", int)
    }

    # Mean and variance of random variable Y=g(X)
    ex1 <- function(x) gx(x)*fx(x)
    ex2 <- function(x) gx(x)^2*fx(x)
    Ex <- integrate(ex1, lo, up)[[1]]*coef
    Ex2 <- integrate(ex2, lo, up)[[1]]*coef
    Vx <- Ex2 - Ex^2
    Dx <- sqrt(Vx)

    # Display output
    if (prt %in% c("exp", "var")) {
        cat(paste0("E(",Yn,") = ", int, "(",Yf,") * ", deparse(fx)[2], 
            "} = ", round(Ex, dig)), "\n")
    }
    if (prt == "var") {
        cat(paste0("E(",Yn,"\U00B2) = ", int, "(",Yf,")\U00B2 * ", 
            deparse(fx)[2], "} = ", round(Ex2, dig)), "\n")
        cat(paste0("Var(",Yn,") = ", round(Ex2,dig), " - ", 
		round(abs(Ex),dig), "\U00B2 = ", round(Vx,dig)), "\n")
    }
    # Return the results
    out = list(Ex=Ex, Dx=Dx, Vx=Vx)
    invisible(out)
}

# [5-4] Joint pdf and Expected Values of Two Continuous Random Variables
# Define the expected value P(x1<X<x2, y1<Y<y2) (double integration)
Exp.dbl <- function(FUN, x1, x2, y1, y2) {
  if (is.function(y1) || is.function(y2)) {
    # Use dbl.prob() of CH4 ---------
    out <- dbl.prob(FUN, x1, x2, y1, y2)@Q
  } else {
    out <- integrate(function(y) {
        sapply(y, function(y) {
            integrate(function(x) {
                sapply(x, function(x) FUN(x, y)) }, x1, x2)$value
        })
      }, y1, y2) 
  }
  return(out)
}

#' @title Expected Values of Two Continuous Random Variables
#' @description Joint pdf and Expected Values of Two Continuous Random Variables
#' @param FUN Continuous joint PDF.
#' @param lo1 Lower limit of X, Default: -Inf.
#' @param up1 Upper limit of X, Default: Inf.
#' @param lo2 Lower limit of Y, Default: -Inf.
#' @param up2 Upper limit of Y, Default: Inf.
#' @param dig Number of decimal places, Default: 4.
#' @param prt Print option: one of c("", "exp", "cov", "cor"), Default: ''.
#' @return list(Ex=E(X), Dx=D(X), Ey=E(Y), Dy=D(Y), Vxy=Cov(X,Y), Cxy=Corr(X,Y))
#' @examples
#' pdf <- function(x, y) 0.5*(x+3*y)*(x>0 & x<1)*(y>0 & y<1)
#' cont.jexp(pdf, prt="cor")
#'
#' fxy <- function(x,y) 24*x*y*(x>0)*(y>0)*(x+y<1)
#' cont.jexp(fxy,0,1,0,1, prt="cor")
#' @rdname cont.jexp0
#' @export

cont.jexp0 <- function(FUN, lo1=-Inf, up1=Inf, lo2=-Inf, up2=Inf, dig=4, prt="exp") {
    # Mean and variance of random variable X
    ex1 <- function(x, y) x*FUN(x, y)
    ex2 <- function(x, y) x^2*FUN(x, y)
    Ex <- Exp.dbl(ex1, lo1, up1, lo2, up2)[[1]]
    Ex2 <- Exp.dbl(ex2, lo1, up1, lo2, up2)[[1]]
    Vx <- Ex2 - Ex^2
    Dx <- sqrt(Vx)

    # Mean and variance of random variable X
    ey1 <- function(x, y) y*FUN(x, y)
    ey2 <- function(x, y) y^2*FUN(x, y)
    Ey <- Exp.dbl(ey1, lo1, up1, lo2, up2)[[1]]
    Ey2 <- Exp.dbl(ey2, lo1, up1, lo2, up2)[[1]]
    Vy <- Ey2 - Ey^2
    Dy <- sqrt(Vy)

    # Covariance and correlation of random variable X and Y
    exy <- function(x, y) x*y*FUN(x, y)
    Exy <- Exp.dbl(exy, lo1, up1, lo2, up2)[[1]]
    Vxy <- Exy - Ex*Ey
    Cxy <- Vxy/sqrt(Vx*Vy)

    # Added (2020.05.19)
    if (prt == c("prob")) {
        prb <- Exp.dbl(FUN, lo1, up1, lo2, up2)[[1]]
        cat(paste0("Prob = ", round(prb, dig)), "\n")
    }
    if (prt == c("coef")) {
        prb <- Exp.dbl(FUN, lo1, up1, lo2, up2)[[1]]
        coef <- 1/prb
        cat(paste0("Coeff = ", round(coef, dig)), "\n")
    }

    # Display output
    if (prt %in% c("exp", "cov", "cor")) {
        cat(paste0("E(X) = ", round(Ex, dig)), "\n")
        cat(paste0("E(Y) = ", round(Ey, dig)), "\n")
        cat(paste0("E(XY) = ", round(Exy, dig)), "\n")
    }
    if (prt %in% c("cov", "cor")) {
        cat(paste0("Var(X) = ", round(Ex2,dig), " - ", round(abs(Ex),dig), 
            "\U00B2 = ", round(Vx,dig)), "\n")
        cat(paste0("Var(Y) = ", round(Ey2,dig), " - ", round(abs(Ey),dig), 
            "\U00B2 = ", round(Vy,dig)), "\n")
        cat(paste0("Cov(X,Y) = ", round(Exy,dig), " - ", round(Ex,dig), 
            " \U00D7 ", round(Ey,dig), " = ", round(Vxy,dig)), "\n")
    }
    if (prt=="cor") {
        cat(paste0("Corr(X,Y) = ", round(Vxy,dig), " / \U221A(", round(Vx,dig),
            "\U00D7", round(Vy,dig), ") = ", round(Cxy,dig)), "\n")
    }
    # Return the results
    out = list(Ex=Ex, Dx=Dx, Ey=Ey, Dy=Dy, Exy=Exy, Vxy=Vxy, Cxy=Cxy)
    invisible(out)
}

# [5-6] Correlation Coefficients and Scatter Plots of Discrete Random variables
#' @title Correlation Coefficients and Scatter Plots
#' @description Correlation Coefficients and Scatter Plots of Discrete Random variables
#' @param X Vector: sample space of X.
#' @param Mt Plot title.
#' @param item Names of random variables.
#' @param dig Number of effective digits, Default: 5.
#' @param prt Logical: print the result? Default: TRUE.
#' @param pprt Logical: print frequency tables? Default: FALSE.
#' @param plot Logical: Plot the PDF and scatter plots? Default: FALSE.
#' @return None.
#' @examples
#' S <- prob::rolldie(4)
#' item <- c("Mean", "Median", "R-mean", "Range")
#' X <- list()
#' X[[1]] <- apply(S, 1, mean)
#' X[[2]] <- apply(S, 1, median)
#' X[[3]] <- apply(S, 1, function(x) (max(x)+min(x))/2)
#' X[[4]] <- apply(S, 1, function(x) max(x)-min(x))
#' Mt <- paste("PDF of", item, "in 4 Dice")
#' corr.plot0(X, Mt, item, pprt=T, plot=T)
#' @rdname corr.plot
#' @export
corr.plot <- function(X, Mt, item, dig=4, prt=TRUE, pprt=FALSE, plot=FALSE) {
    # Number of random variables
    nv <- length(X)
    # List of frequency distributions
    Xf <- list()
    for (k in 1:nv) Xf[[k]] <- table(X[[k]])

    # Print the frequency distribution
    if (pprt) {
        for (k in 1:nv) {
            cat(paste0(item[k], "(X", k, ") frequency distribution"))
            print(Xf[[k]])
        }
    }

    # List of random variable values
    Xv <- list()
    for (k in 1:nv) Xv[[k]] <- as.numeric(names(Xf[[k]]))
    # Probability distributions
    Xp <- list()
    N <- length(X[[1]])
    for (k in 1:nv) Xp[[k]] <- Xf[[k]] / N

    # Calculate expected values
    SX <- EX <- rep(NA, nv)
    for (k in 1:nv) {
        SX[k] <- (Xv[[k]] %*% Xf[[k]])[1,1]
        EX[k] <- SX[k]/N
    }
    # Calculate variances
    SX2 <- EX2 <- VX <- DX <- rep(NA, nv)
    for (k in 1:nv) {
        SX2[k] <- (Xv[[k]]^2 %*% Xf[[k]])[1,1]
        EX2[k] <- SX2[k]/N
        VX[k] <- EX2[k] - EX[k]^2
        DX[k] <- sqrt(VX[k])
    }
    # Calculate covariance & correlation coefficient
    SXY <- EXY <- VXY <- CXY <- matrix(NA, nv, nv)
    colnames(VXY) <- colnames(CXY) <- paste0("X",1:nv)
    rownames(VXY) <- rownames(CXY) <- paste0("X",1:nv)
    for (k in 1:nv) for (m in 1:nv) {
        XYf <- table(X[[k]], X[[m]])
        SXY[k,m] <- (as.vector(Xv[[k]] %o% Xv[[m]]) %*% as.vector(XYf))[1,1]
        XYp <- XYf / N
        EXY[k,m] <- SXY[k,m] / N
        VXY[k,m] <- EXY[k,m] -EX[k]*EX[m]
        CXY[k,m] <- VXY[k,m] / (VX[k] * VX[m])^0.5
    }

    # Display output
    if (prt) {
        cat("Expected Values and Variances ------------------------\n")
        for (k in 1:nv) cat(paste0("E(X",k,") ="), 
	    format(EX[k], digits=(dig+1)), "\t")
        cat("\n")
        for (k in 1:nv) cat(paste0("Var(X",k,") ="), 
	    format(VX[k], digits=(dig+1)), "\t")
        cat("\nVariance-Covariance Matrix ------------------------\n")
        print(VXY)
        cat("Correlation Coefficient Matrix ------------------------\n")
        print(CXY)
    }
    # Display plots -----------------------------------------
    if (plot) {
        if (missing(Mt)) Mt = paste(item, "probability distribution")
        # Display bar charts
        nc <- ifelse(nv<=5, 2, 3)
        nr <- ceiling(nv/nc)
        h <- ifelse(nr>2, 9, 6)
        w <- ifelse(nc>2, 9, 7)
        win.graph(w, h)
        par(mfrow=c(nr, nc))
        for (k in 1:nv) plot(Xp[[k]], type="h", col="red", main=Mt[k], 
		ylab="f(x)", xlab="", lwd=3)

        # Scatter plots
        St <- matrix("character", nv, nv)
        for (k in 1:nv) for (m in 1:nv) {
		St[k, m] <- paste(item[m], ":", item[k])
	}
        np <- nv*(nv-1)/2
        nc <- ifelse(np<=5, 2, 3)
        nr <- ceiling(np/nc)
        h <- ifelse(nr>2, 9, 6)
        w <- ifelse(nc>2, 9, 7)
        win.graph(w, h)
        par(mfrow=c(nr, nc))
        for (k in 1:(nv-1)) for (m in (k+1):nv) {
            plot(X[[m]], X[[k]], pch=19, col="blue", main=St[k, m],
                xlab=item[m], ylab=item[k])
            abline(lm(X[[k]]~X[[m]]), col="red")
        }
    }
}