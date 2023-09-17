# [Ch-13 Functions] ----------------------------------------------------------------------------------
# [Ch-13 Function Manual] -----------------------------------------

#' @title Manual for Ch13. Functions
#' @description Ch13. Analysis of Variance
#' @param fn Function number, Default: 0
#' @return None.
#'
#' @examples
#' ch13.man()
#' ch13.man(1)
#' @rdname ch13.man
#' @export
ch13.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] anova1.fit      One-Way ANOVA: Model Fitting and F-Test\n")
        cat("[2] anova1.conf     One-Way ANOVA: CI for the Level Means\n")
        cat("[3] anova1.diff     One-Way ANOVA: CI for the Difference in Means\n")
        cat("[4] anova2.fit      Two-Way ANOVA: Model Fitting and F-Test\n")
        cat("[5] anova2.conf     Two-Way ANOVA: CI for the Level Means\n")
        cat("[6] anova2.diff     Two-Way ANOVA: CI for the Difference in Means\n")
    }
    if (1 %in% fn) {
        cat("[1] One-Way ANOVA: Model Fitting and F-Test\n")
        cat("anova1.fit(f, y, data, alp=0.05, dig=4, ws=c(7,4), dp=1:2, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("f\t Formula (with data), or Factor (without data).\n")
        cat("y\t Response variable (without data).\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("[Optional Input]--------------------------\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size for the box plot, Default: c(7,4).\n")
        cat("dp\t Selection numbers of diagnostic plots, Default: 1:2.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] One-Way ANOVA: CI for the Level Means\n")
        cat("anova1.conf(f, y, data, alp=0.05, dig=4, ws=c(7,4), main)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("f\t Object (aov), Formula (with data), or Factor (without data).\n")
        cat("y\t Response variable (without data).\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("[Optional Input]--------------------------\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size for the CI plot, Default: c(7,4).\n")
        cat("main\t Title of the plot.\n")
    }
    if (3 %in% fn) {
        cat("[3] One-Way ANOVA: CI for the Difference in Means\n")
        cat("anova1.diff(f, y, data, alp=0.05, ci=\"pair\", dig=4, ws=c(7,4), main)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("f\t Object (aov), Formula (with data), or Factor (without data).\n")
        cat("y\t Response variable (without data).\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("[Optional Input]--------------------------\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("ci\t Method of CI, one of (\"pair\", \"hsd\", \"both\"), Default: \"pair\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size for the CI plot, Default: c(7,4).\n")
        cat("main\t Title of the plot.\n")
    }
    if (4 %in% fn) {
        cat("[4] Two-Way ANOVA: Model Fitting and F-Test\n")
        cat("anova2.fit(form, data, alp=0.05, dig=4, ws=c(7,4), dp=1:2, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("form\t Formula of the two-way ANOVA.\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size for the box plot, Default: c(7,4).\n")
        cat("dp\t Selection numbers of diagnostic plots, Default: 1:2.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (5 %in% fn) {
        cat("[5] Two-Way ANOVA: CI for the Level Means\n")
        cat("anova2.conf(f, data, alp=0.05, maxim=TRUE, dig=4, ws=c(7,4), main)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("f\t Object (aov), Formula (with data), or Object (anova2.fit).\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("maxim\t Logical: maximize the response? Default: TRUE.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size for the box plot, Default: c(7,4).\n")
        cat("main\t Title of the plot.\n")
    }
    if (6 %in% fn) {
        cat("[6] Two-Way ANOVA: CI for the Difference in Means\n")
        cat("anova2.diff(f, data, alp=0.05, ci=\"pair\", sel=\"h4\", dig=4, ws=c(7,4), main)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("f\t Object (aov), Formula (with data), or Object (anova2.fit).\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("ci\t Method of CI, one of (\"pair\", \"hsd\", \"both\"), Default: \"pair\".\n")
        cat("sel\t Selected combination levels, Default: \"h4\" (high four).\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size for the box plot, Default: c(7,4).\n")
        cat("main\t Title of the plot.\n")
    }
}

# [13-1] One-Way ANOVA: Model Fitting and F-Test
#' @title One-Way ANOVA: Model Fitting and F-Test
#' @description One-way analysis of variance to produce an ANOVA table.
#' @param f Formula (with data), or Factor (without data).
#' @param y Response variable (without data).
#' @param data Data frame applied to the formula.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size for the box plot, Default: c(7,4).
#' @param dp Selection numbers of diagnostic plots, Default: 1:2.
#' @param ... Other graphic parameters.
#' @return Data and summary statistics.
#'
#' @examples
#' Yield <- c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
#' Temp <- c(rep(150,5), rep(200,6), rep(250,5), rep(300,4))
#' anova1.fit(f=Temp, y=Yield)
#' anova1.fit(Yield ~ Temp)
#' # 'mtcars' dataset
#' anova1.fit(mpg ~ cyl, data=mtcars)
#' @rdname anova1.fit
#' @export

anova1.fit <- function(f, y, data, alp=0.05, dig=4, ws=c(7,4), dp=1:2, ...) {
  # Check input
    if (missing(f)) stop("Formula of factor (independent variable) is required.")
    if (is.language(f)) {
        form <- f
        vars <- all.vars(form)
        yl <- vars[1]
        xl <- vars[2]
        if (missing(data)) {
            y <- eval(parse(text=yl))
            f <- eval(parse(text=xl))
        } else {
            if (!is.data.frame(data)) data <- as.data.frame(data)
            y <- data[[yl]]
            f <- data[[xl]]
        }
    } else if (missing(y)) {
        stop("A vector of dependent variable is required.")
    } else {
        yl <- deparse(substitute(y))
        xl <- deparse(substitute(f))
    }

  # Number of factor levels & data
    m1 <- length(unique(f))
    nn <- length(y)
  # Transform factor levels into factors by as.factor( ) function
    af <- as.factor(f)

  # Calculate the mean of response variable for each factor level
    ym <- tapply(y, af, mean)
    ysm <- tapply(y, af, sum)
    tab <- rbind(ym, ysm)
    rownames(tab) <- c("Mean", "Sum")
    print(round(tab, dig))

  # Calculate the sum of squares
    rr <- tapply(y, af, length)
    CT <- sum(y)^2/nn
    SST <- sum(y^2) - CT
    SSA <- sum(ysm^2 / rr) - CT
    SSE <- SST - SSA

  # Calculation of the sum of squares
    cat("[1] Sum of Squares ___________________________________________________\n")
    cat(paste0("CT = ", round(sum(y),dig), "\U00B2", "/ ", nn, " = ", round(CT,dig)), "\n")
    cat(paste0("SST = ", round(sum(y^2),dig), " - CT = ", round(SST,dig)), "\n")
    psa <- paste0(round(ysm,dig), "\U00B2","/", rr)
    cat(paste0("SSA = (", paste0(psa, collapse=" + "), ") - CT = ", round(SSA,dig)), "\n")
    cat(paste0("SSE = ", round(SST,dig), " - ", round(SSA,dig), " = ", round(SSE,dig)), "\n")

  # Analysis of variance
    an1 <- aov(y ~ af)
    ans1 <- summary(an1)[[1]]
    SSt <- sum(ans1$Sum)
    cr.v <- c(qf(1-alp, ans1$Df[1], ans1$Df[2]), NA)
    antab <- matrix(NA, 3, 6)
    colnames(antab) <- c("Sum Sq.", "df", "Mean Sq.", "F0", "C-value", "P-value")
    rownames(antab) <- c(xl, "Error", "Total")
    antab[1:2,] <- cbind(ans1$Sum, ans1$Df, ans1$Mean, ans1$F, cr.v, ans1$Pr)
    antab[3,1] <- SSt
    antab[3,2] <- nn-1
    # dum <- round(antab, dig)
    temp <- rep(0.123456789, 6)
    temp[2] <- 1
    dum <- as.data.frame(rbind(antab, temp))
    dum <- format(dum, FALSE, dig)
    dum <- dum[-4, ]
    # dum[is.na(dum)] <- ""
    for (k in 3:6) dum[, k] <- sub("NA", "  ", dum[, k])

    cat("[2] ANOVA Table ______________________________________________________\n")
    # print(as.data.frame(dum))
    print(dum)

  # Prior investigation of data
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))

      # Set graphic parameters
        mt <- paste0("Box Plot of ", yl, " for each level of ", xl)
        fill <- "yellow"
        xlab <- paste("Level of", xl)
        ylab <- yl
        pch <- 19
        pch2 <- 17
        col <- "red"
        cex <- 1.2
        lty <- 2
        line.col <- "blue"

        dots <- list(...)
        if (length(dots) > 0) {
            pars <- names(dots)
            if ("main" %in% pars) mt <- dots$main
            if ("fill" %in% pars) fill <- dots$fill
            if ("xlab" %in% pars) xlab <- dots$xlab
            if ("ylab" %in% pars) ylab <- dots$ylab
            if ("pch" %in% pars) pch <- dots$pch
            if ("pch2" %in% pars) pch2 <- dots$pch2
            if ("col" %in% pars) col <- dots$col
            if ("cex" %in% pars) cex <- dots$cex
            if ("lty" %in% pars) lty <- dots$lty
            if ("line.col" %in% pars) line.col <- dots$line.col
        }

        boxplot(y ~ af, col=fill, main=mt, xlab=xlab, ylab=ylab)
        points(af, y, pch=pch, col=col, cex=cex)
        lines(1:m1, ym, type="b", lty=lty, pch=pch2, col=line.col, cex=cex)
    }

    # Diagnostic plot
    if (is.numeric(dp)) {
        nr <- switch(length(dp), 1, 1, 2, 2)
        nc <- switch(length(dp), 1, 2, 2, 2)
        win.graph(4*nc, 4*nr)
        par(mar=c(4.5, 4, 3, 2))
        par(mfrow=c(nr, nc))
        plot(an1, which=dp)
    }
    # [Correction]
    invisible(list(data=data.frame(y=y, af=af), mean=ym, tab=antab,
                  an1=an1, mse=ans1$Mean[2]))
}

# [13-2] One-Way ANOVA: CI for the Level Means
#' @title One-Way ANOVA: CI for the Level Means
#' @description Estimating the confidence intervals of the level means in one-way ANOVA.
#' @param f Object (aov), Formula (with data), or Factor (without data).
#' @param y Response variable (without data).
#' @param data Data frame applied to the formula.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size for the box plot, Default: c(7,4).
#' @param main Title of the plot.
#' @return Table of confidence intervals.
#'
#' @examples
#' Yield <- c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
#' Temp <- c(rep(150,5), rep(200,6), rep(250,5), rep(300,4))
#' # [1] Direct input
#' anova1.conf(f=Temp, y=Yield)
#' # [2] Formula input
#' anova1.conf(Yield ~ Temp)
#' # [3] anova1.fit object input
#' aa <- anova1.fit(f=Temp, y=Yield, ws="n", dp="n")
#' anova1.conf(aa)
#' # [4] aov (lm) object input
#' Temp <- as.factor(Temp)
#' dd <- aov(Yield ~ Temp)
#' anova1.conf(dd)
#' @rdname anova1.conf
#' @export

anova1.conf <- function(f, y, data, alp=0.05, dig=4, ws=c(7,4), main) {
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

    # [Correction]
    if (is.language(f)) {
        form <- f
        vars <- all.vars(form)
        yl <- vars[1]
        xl <- vars[2]
        if (missing(data)) {
            y <- eval(parse(text=yl))
            af <- as.factor(eval(parse(text=xl)))
        } else {
            if (!is.data.frame(data)) data <- as.data.frame(data)
            y <- data[[yl]]
            af <- as.factor(data[[xl]])
        }
        an1 <- aov(y ~ af)
    } else if (is.list(f)) {
        if ("lm" %in% class(f)) {
            an1 <- f
        } else {
            an1 <- f$an1
        }
        xl <- attr(attr(an1$terms,"dataClasses"),"names")[2]
        yl <- attr(attr(an1$terms,"dataClasses"),"names")[1]
        af <- an1$model[[2]]
        y <- an1$model[[1]]
    } else {
      # Check input
        if (missing(y)) stop("A vector of dependent variable is required.")
        if (missing(f)) stop("A vector of factor (independent variable) is required.")
        yl <- deparse(substitute(y))
        xl <- deparse(substitute(f))

        af <- as.factor(f)
      # Analysis of variance
        an1 <- aov(y ~ af)
    }
  # Mean response
    ym <- tapply(y, af, mean)
  # Mean square error
    mse <- summary(an1)[[1]]$Mean[2]

  # Number of factor levels & data
    m1 <- length(levels(af))
    nn <- length(y)

  # Confidence interval of the response mean for each factor level
    ni <- tapply(y, af, length)
    se <- sqrt(mse/ni)
    tval <- qt(1-alp/2, nn-m1)
    dtv <- round(tval, dig)
    tol <- tval*se

  # Confidence limits
    lcl <- ym-tol
    ucl <- ym+tol
    cat("MSE =", round(mse, dig), " ", paste0("t_(", 1-alp/2, ";", nn-m1, 
        ") = ", dtv), "  ")
    cat(paste0("[", (1-alp)*100, "%"), "CI] ",
                "________________________\n")
    CItab <- rbind(ym, tol, lcl, ucl)
    flev <- unique(af)
    myci0 <- paste0(flev,": [",round(ym,dig)," \U00B1 ",dtv, "\U221A",
        "(", round(mse,dig), "/", ni, ")]")
    myci <- paste0("[",round(ym,dig)," \U00B1 ", myf1(tol),"] = [",
                   myf1(lcl),", ", myf1(ucl),"]")
    for (k in 1:m1) cat(myci0[k], "\U2252", myci[k], "\n")

  # Confidence interval plot
    if (is.numeric(ws)) {
        ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "CI\n")
        ##width <- 7+min(8, max(0, m1-6)*0.5)
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))

        fnum <- as.numeric(af)
        lev <- 1:m1
        x1 <- 0.5
        x2 <- m1+0.5
        ymin <- min(y, lcl)
        ymax <- max(y, ucl)
        y1 <- ymin - 0.1*(ymax-ymin)
        y2 <- ymax + 0.1*(ymax-ymin)

      # Remove xlim and ylim !!
        if (missing(main)) mt <- 
            paste0((1-alp)*100, "% CI for the Mean of ", yl, " w.r.t. ", xl)
        plot(fnum, y, pch=19, col="green", cex=1.2, xaxt ="n",
            main=mt, ylab=yl, xlab=xl, xlim=c(x1, x2), ylim=c(y1, y2))
        grid(col="green")
        axis(1, at=1:m1, labels=levels(af))

      # Lines connecting the means, the upper and lower limits
        lines(lev, ym, type="b", lty=2, pch=17, col="red", cex=1.2)
        lines(lev, lcl, type="b", lty=4, pch=18, col="blue", cex=1.2)
        lines(lev, ucl, type="b", lty=4, pch=18, col="blue", cex=1.2)

      # Confidence intervals
        arrows(lev, lcl, lev, ucl, lwd=2, length=0.1, code=3, angle=90)

      # Values of the means, the upper and lower limits
        text(lev, ym, labels=round(ym, 3), cex=0.9, col="red", pos=4)
        text(lev, lcl, labels=round(lcl, 3), cex=0.9, col="blue", pos=4)
        text(lev, ucl, labels=round(ucl, 3), cex=0.9, col="blue", pos=4)
    }
    invisible(CItab)
}

# [13-3] One-Way ANOVA: CI for the Difference in Means
#' @title One-Way ANOVA: CI for the Difference in Means
#' @description Estimating the confidence intervals for difference between means in one-way ANOVA.
#' @param f Object (aov), Formula (with data), or Factor (without data).
#' @param y Response variable (without data).
#' @param data Data frame applied to the formula.
#' @param alp Level of significance, Default: 0.05.
#' @param ci Method of CI, one of ('pair', 'hsd', 'both'), Default: 'pair'.
#' @param dig Number of digits below the decimal point, Default: 4.
#' @param ws Graphic window size for the box plot, Default: c(7,4).
#' @param main Title of the plot.
#' @return None.
#'
#' @examples
#' Yield <- c(79,83,88,78,75, 81,89,91,84,86,82, 86,91,93,90,89, 76,81,82,79)
#' Temp <- c(rep(150,5), rep(200,6), rep(250,5), rep(300,4))
#' # [1] Direct input
#' anova1.diff(f=Temp, y=Yield)
#' # [2] Formula input
#' anova1.diff(Yield ~ Temp)
#' # [3] anova1.fit object input
#' aa <- anova1.fit(f=Temp, y=Yield, ws="n", dp="n")
#' anova1.diff(aa)
#' # [4] aov (lm) object input
#' Temp <- as.factor(Temp)
#' dd <- aov(Yield ~ Temp)
#' anova1.diff(dd)
#' anova1.diff(dd, ci="hsd")
#' anova1.diff(dd, ci="both")
#' @rdname anova1.diff
#' @export

anova1.diff <- function(f, y, data, alp=0.05, ci="pair", dig=4, ws=c(7,4), main) {
  # Check Input
    if (is.language(f)) {
        form <- f
        vars <- all.vars(form)
        yl <- vars[1]
        xl <- vars[2]
        if (missing(data)) {
            y <- eval(parse(text=yl))
            af <- as.factor(eval(parse(text=xl)))
        } else {
            if (!is.data.frame(data)) data <- as.data.frame(data)
            y <- data[[yl]]
            af <- as.factor(data[[xl]])
        }
        an1 <- aov(y ~ af)
    } else if (is.list(f)) {
        if ("lm" %in% class(f)) {
            an1 <- f
        } else {
            an1 <- f$an1
        }
        xl <- attr(attr(an1$terms,"dataClasses"),"names")[2]
        yl <- attr(attr(an1$terms,"dataClasses"),"names")[1]
        af <- an1$model[[2]]
        y <- an1$model[[1]]
    } else {
      # Check input
        if (missing(y)) stop("A vector of dependent variable is required.")
        if (missing(f)) stop("A vector of factor (independent variable) is required.")
        yl <- deparse(substitute(y))
        xl <- deparse(substitute(f))

        af <- as.factor(f)
      # Analysis of variance
        an1 <- aov(y ~ af)
    }
  # Mean response
    ym <- tapply(y, af, mean)
  # Mean square error
    mse <- summary(an1)[[1]]$Mean[2]

  # Number of factor levels & data
    m1 <- length(levels(af))
    nn <- length(y)

  # Confidence interval of the response mean for each factor level
    ni <- tapply(y, af, length)

  # Mean difference vector
    ym2 <- ym[1]-ym[-1]
    for (k in 2:(m1-1)) ym2 <- c(ym2, ym[k]-ym[-(1:k)])
    # [corr]
    yml2 <- paste0(round(ym[1],dig), "-", round(ym[-1],dig))
    for (k in 2:(m1-1)) yml2 <- c(yml2, paste0(round(ym[k],dig), "-", round(ym[-(1:k)],dig)))

    ylv <- paste0("A", 1:m1)
    nylv <- paste0(ylv[1], "-", ylv[-1])
    for (k in 2:(m1-1)) nylv <- c(nylv, paste0(ylv[k], "-", ylv[-(1:k)]))
    names(ym2) <- nylv
    # [corr]
    mul <- paste0("\U03BC", 1:m1)
    mulv <- paste0(mul[1], "-", mul[-1])
    for (k in 2:(m1-1)) mulv <- c(mulv, paste0(mul[k], "-", mul[-(1:k)]))

  # Tolerance of the CIs
    env <- sqrt(1/ni[1]+1/ni[-1])
    for (k in 2:(m1-1)) env <- c(env, sqrt(1/ni[k]+1/ni[-(1:k)]))
    # [corr]
    envl <- paste0("(1/", ni[1], "+1/", ni[-1], ")")
    for (k in 2:(m1-1)) envl <- c(envl, paste0("(1/", ni[k], "+1/", ni[-(1:k)], ")"))

    # [corr]
    fym2 <- format(round(ym2, dig), FALSE, dig)

    if (ci %in% c("pair", "both")) {
        tval <- qt(1-alp/2, nn-m1)
        tol2 <- tval*sqrt(mse)*env
        dtv <- round(tval, dig)
        names(tol2) <- names(ym2)

      # Confidence limits for mean differences
        lcl2 <- ym2-tol2
        ucl2 <- ym2+tol2
      # [corr]
        ftol2 <- format(round(tol2, dig), nsmall=dig)
        flcl2 <- format(round(lcl2, dig), nsmall=dig)
        fucl2 <- format(round(ucl2, dig), nsmall=dig)

        cat("MSE =", round(mse, dig), paste0(" t_(", 1-alp/2, ";", nn-m1, ") = ", dtv), " ")

        cat(paste0("[", (1-alp)*100, "%"), "CI] __________________________\n")
        # [corr] print(round(rbind(ym2, tol2, lcl2, ucl2), dig))
        flev <- unique(af)
        myci0 <- paste0(mulv, ": [(", yml2, ")\U00B1t*\U221A", "(MSE", envl, "]")
        myci <- paste0("[", fym2,"\U00B1", ftol2,"]=[", flcl2,", ", fucl2,"]")
        m2 <- m1*(m1-1)/2
        for (k in 1:m2) cat(myci0[k], "\U2252", myci[k], "\n")
    }

    # Tukey's HSD ---------------------------
    if (ci %in% c("hsd", "both")) {
        hsd <- TukeyHSD(an1)
        lwr <- -hsd[[1]][, 3]
        upr <- -hsd[[1]][, 2]
        htol <- ym2 - lwr
        flwr <- format(round(lwr, dig), nsmall=dig)
        fupr <- format(round(upr, dig), nsmall=dig)
        fhtol <- format(round(htol, dig), nsmall=dig)

        qv <- qtukey(1-alp, m1, nn-m1)
        q2 <- qv/sqrt(2)
        fqv <- format(round(qv, dig), nsmall=dig)
        fq2 <- format(round(q2, dig), nsmall=dig)
        if (ci == "hsd") cat("MSE =", round(mse, dig), " ")
        cat(paste0("q_(", 1-alp, ";", m1, ",", nn-m1, ")/\U221A", "2 \U2252 ", 
                fqv, "/\U221A", "2 \U2252", fq2), " ")

        cat(paste0("[", (1-alp)*100, "%"), "Tukey's HSD] _______________\n")
        # [corr]
        flev <- unique(af)
        hci0 <- paste0(mulv, ": [(", yml2, ")\U00B1q*\U221A", "(MSE", envl, "]")
        hci <- paste0("[", fym2,"\U00B1", fhtol,"]=[", flwr,", ", fupr,"]")
        m2 <- m1*(m1-1)/2
        for (k in 1:m2) cat(hci0[k], "\U2252", hci[k], "\n")
    }
    n2 <- length(ym2)

  # Set confidence intervals and HSD plots
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        dcol <- rep("green2", n2)
        dcol2 <- rep("steelblue", n2)

        if (ci == "pair") {
            ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "CI\n")
            y1 <- min(lcl2) - 0.1*(max(ucl2)-min(lcl2))
            y2 <- max(ucl2) + 0.1*(max(ucl2)-min(lcl2))
            if (missing(main)) main <- 
                         paste0((1-alp)*100, "% CI for the Mean Differences")
            dcol[lcl2>0 | ucl2<0] <- "orange"
            lop <- lcl2
            upp <- ucl2
        } else if (ci == "hsd") {
            ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "HSD\n")
            y1 <- min(lwr) - 0.1*(max(upr)-min(lwr))
            y2 <- max(upr) + 0.1*(max(upr)-min(lwr))
            if (missing(main)) main <- paste0((1-alp)*100, "% HSD for the Means")
            dcol[lwr>0 | upr<0] <- "orange"
            lop <- lwr
            upp <- upr
        } else if (ci == "both") {
            ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "CI and HSD\n")
            y1 <- min(lwr) - 0.1*(max(upr)-min(lwr))
            y2 <- max(upr) + 0.1*(max(upr)-min(lwr))
            if (missing(main)) main <- paste0((1-alp)*100, "% paired CI and HSD")
            dcol[lcl2>0 | ucl2<0] <- "orange"
            dcol2[lwr>0 | upr<0] <- "magenta"
            lop <- lcl2
            upp <- ucl2
        }

      # Draw confidence interval and HSD plots
        ##width <- 7+min(8, max(0, m1-4))
        ##win.graph(width, 5)
        plot(ym2, type="n", xlim=c(0.5, n2+0.5), ylim=c(y1, y2), main=main,
            ylab=paste("Mean Differences of", yl), 
            xlab=paste("Level Combinations of", xl), xaxt="n")
        grid(col="green")
      # Horizontal line of zero difference
        abline(h=0, lty=3, col=grey(0.2))

      # Confidence intervals
        arrows(1:n2, lop, 1:n2, upp, lwd=2, length=0.1, code=3, angle=90, col=dcol)
      # Lines connecting the mean differences, the upper and lower limits
        lines(1:n2, lop, type="b", pch=18, lty=2, col="blue")
        lines(1:n2, upp, type="b", pch=18, lty=2, col="blue")

      # Values of the mean differences, the upper and lower limits
        text(1:n2, ym2, labels=round(ym2, 2), cex=0.9, col="red", pos=4)
        text(1:n2, lop, labels=round(lop, 2), cex=0.9, col=1, pos=4)
        text(1:n2, upp, labels=round(upp, 2), cex=0.9, col=1, pos=4)
        lines(1:n2, ym2, type="b", pch=17, lty=2, col="red", cex=1.2)
        # mtext(names(ym2), side=1, at=1:n2, col=1, line=0.5)
        axis(1, at=1:n2, labels=names(ym2), las=ifelse(m1>5,2,1), cex.axis=ifelse(m1>4,0.8,1))

        if (ci == "both") {
            at2 = 1:n2-0.1
            arrows(at2, lwr, at2, upr, lwd=2, length=0.1, code=3, angle=90, col=dcol2)

          # Values of the mean differences, the upper and lower limits
            text(at2, lwr, labels=round(lwr, 2), cex=0.9, col="blue", pos=1)
            text(at2, upr, labels=round(upr, 2), cex=0.9, col="blue", pos=3)
        }
    } # End of plot
}

# [13-4] Two-Way ANOVA: Model Fitting and F-Test
#' @title Two-Way ANOVA: Model Fitting and F-Test
#' @description Two-way analysis of variance to produce an ANOVA table.
#' @param form Formula of the two-way ANOVA.
#' @param data Data frame applied to the formula.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size for the plots, Default: c(7,4).
#' @param dp Selection numbers of diagnostic plots, Default: 1:2.
#' @param ... Other graphic parameters.
#' @return List of fitted results.
#'
#' @examples
#' Yield <- c(76,79,81,79,83,85, 79,81,84,86,89,88, 87,91,91,94,88,86, 79,82,85,84,77,76)
#' f1 <- c(rep(100,6), rep(150,6), rep(200,6), rep(250,6))
#' f2 <- rep(c(1,1,2,2,3,3), 4)
#' Temp <- as.factor(f1)
#' Press <- as.factor(paste0(f2, "P"))
#' anova2.fit(Yield ~ Temp * Press)
#'
#' # Without replication
#' Yield1 <- c(77.5,80.0,89.0,80.5, 80.0,85.0,92.5,84.5, 84.0,88.5,87.0,76.5)
#' Temp1 <- c(100,150,200,250, 100,150,200,250, 100,150,200,250)
#' Press1 <- c(1,1,1,1, 2,2,2,2 ,3,3,3,3)
#' anova2.fit(Yield1 ~ Temp1 + Press1)
#' @rdname anova2.fit
#' @export
anova2.fit <- function(form, data, alp=0.05, dig=4, ws=c(7,4), dp=1:2, ...) 
{
  # Check input
    if (missing(form)) stop("Input formula.")
    if (!is.language(form)) stop("Formula for the ANOVA is required.")
  # Check Interaction
    nfac <- dim(attr(terms(form), "factors"))[2]
    if (nfac==3) inter <- TRUE else inter <- FALSE

  # Assign variables
    vars <- all.vars(form)
    yl <- vars[1]
    xl1 <- vars[2]
    xl2 <- vars[3]

    if (missing(data)) {
        y <- eval(parse(text=yl))
        f1 <- eval(parse(text=xl1))
        f2 <- eval(parse(text=xl2))
    } else {
        if (!is.data.frame(data)) data <- as.data.frame(data)
        y <- data[[yl]]
        f1 <- data[[xl1]]
        f2 <- data[[xl2]]
    }

  # Number of factor levels & data
    nn <- length(y)
    m1 <- length(unique(f1))
    m2 <- length(unique(f2))
    na <- nn/m1
    nb <- nn/m2
  # Replication
    nr <- nn/(m1*m2)

    interact <- inter
    if (nr == 1) interact <- FALSE

  # Transform factor levels into factors by as.factor( ) function
    if (is.factor(f1)) {
        af1 <- f1
    } else {
        af1 <- as.factor(f1)
    }
    if (is.factor(f2)) {
        af2 <- f2
    } else {
        af2 <- as.factor(f2)
    }

  # Calculate the mean of response variable for each combination of factor levels
    ym1 <- tapply(y, af1, mean)
    ym2 <- tapply(y, af2, mean)
    ym <- tapply(y, list(af2, af1), mean)

    if (nr>1) {
        ##cat("[0] Summary Statistics _______________________________________________\n")
        ymtab <- addmargins(ym, FUN=mean, quiet=TRUE)
        print(round(ymtab, dig))
    }

  # Calculate the sum of squares
    ysm1 <- tapply(y, af1, sum)
    ysm2 <- tapply(y, af2, sum)
    ysm <- tapply(y, list(af2, af1), sum)
    CT <- sum(y)^2/nn
    SST <- sum(y^2) - CT
    SSA <- sum(ysm1^2)/na - CT
    SSB <- sum(ysm2^2)/nb - CT
    SSAB <- sum(ysm^2)/nr - CT

  # Detailed output of the sum of squares
    cat("[1] Sum of Squares ___________________________________________________\n")
    cat(paste0("CT = ", round(sum(y),dig), "\U00B2", "/ ", nn, " = ", 
                round(CT,dig)), " \U21D2 ")
    cat(paste0("SST = ", round(sum(y^2),dig), " - CT = ", round(SST,dig)), "\n")
    psa <- paste0(round(ysm1,dig), "\U00B2")
    psb <- paste0(round(ysm2,dig), "\U00B2")
    cat(paste0("SSA = (", paste(psa, collapse="+"), ")/", na, 
                       " - CT = ", round(SSA,dig)), "\n")
    cat(paste0("SSB = (", paste(psb, collapse="+"), ")/", nb, 
                       " - CT = ", round(SSB,dig)), "\n")

    if (interact) {
        SSE <- SST - SSAB
        SSAxB <- SSAB - SSA - SSB
        # psab <- paste0(round(ysm,dig), "\U00B2")
        # cat(paste0("SSAB = (", paste(psab, collapse="+"), ")/", nr, " - ",
        #     round(CT,dig), " = ", round(SSAB,dig)), "\n")
        cat(paste0("SSAB = ", round(sum(ysm^2),dig),  "/", nr, 
                   " - CT = ", round(SSAB,dig)), " \U21D2 ")
        cat(paste0("SSAxB = ", round(SSAB,dig), " - ", round(SSA,dig), " - ", 
                   round(SSB,dig), " = ", round(SSAxB,dig)), "\n")
        cat(paste0("SSE = SST - SSAB = ", round(SST,dig), " - ", round(SSAB,dig), 
                   " = ", round(SSE,dig)), "\n")
    } else {
        SSE <- SST - SSA - SSB
        cat(paste0("SSE = SST - SSA - SSB = ", round(SST,dig), " - ", round(SSA,dig), 
                   " - ", round(SSB,dig), " = ", round(SSE,dig)), "\n")
    }

  # Analysis of variance
    if (interact) {
        an2 <- aov(y ~ af1 * af2)
        nrtab <- 5
        nc <- 3
        rname <- c(xl1, xl2, paste(xl1,xl2,sep="*"), "Error", "Total")
    } else {
        an2 <- aov(y ~ af1 + af2)
        nrtab <- 4
        nc <- 2
        rname <- c(xl1, xl2, "Error", "Total")
    }
    ans2 <- summary(an2)[[1]]

    SSt <- sum(ans2$Sum)
    antab2 <- matrix(NA, nrtab, 6)
    cr.v <- c(qf(1-alp, ans2$Df[1:nc], ans2$Df[nc+1]), NA)
    colnames(antab2) <- c("Sum of Sq.", "df", "Mean Sq.", "F0", "C-value", "P-value")
    rownames(antab2) <- rname
    antab2[1:(nrtab-1),] <- cbind(ans2$Sum, ans2$Df, ans2$Mean, ans2$F, cr.v, ans2$Pr)
    antab2[nrtab,1] <- SSt
    antab2[nrtab,2] <- nn-1

    # dum <- round(antab2, dig)
    dum <- as.data.frame(antab2)
    dum <- format(dum, FALSE, dig)
    # dum[is.na(dum)] <- ""
    for (k in 3:6) dum[, k] <- sub("NA", "  ", dum[, k])

    cat("[2] ANOVA Table ______________________________________________________\n")
    # print(as.data.frame(dum))
    print(dum)

  # Prior investigation of data
    if (is.numeric(ws) && inter) {
      # Colors
        if (m2<6) {
            dcol <- c("black", "red", "blue", "green4", "purple")
        } else {
            dcol <- rainbow(m2)
        }
        ## cat("[P1] Interaction Plot\n")
      # Set graphic parameters
        mt <- paste("Interaction Plot of", yl, "w.r.t.", xl1, "&", xl2)
        xlab <- paste("Level of", xl1)
        ylab <- paste0(yl, " mean")
        col <- dcol[1:m2]

        dots <- list(...)
        if (length(dots) > 0) {
            pars <- names(dots)
            if ("main" %in% pars) mt <- dots$main
            if ("xlab" %in% pars) xlab <- dots$xlab
            if ("ylab" %in% pars) ylab <- dots$ylab
            if ("col" %in% pars) col <- dots$col
        }

        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        interaction.plot(af1, af2, y, type="b", col=col, lwd=2, 
                         leg.bg="white", leg.bty="o",
                   main=mt, xlab=xlab, ylab=ylab, trace.label = xl2)
        grid(col="green")
    }

  # Diagnostic plot
    if (is.numeric(ws) && is.numeric(dp)) {
        nr <- switch(length(dp), 1, 1, 2, 2)
        nc <- switch(length(dp), 1, 2, 2, 2)
        win.graph(4*nc, 4*nr)
        par(mar=c(4.5, 4, 3, 2))
        par(mfrow=c(nr, nc))
        plot(an2, which=dp)
    }
  # Output
    dout <- data.frame(y, af1, af2)
    colnames(dout) <- c(yl, xl1, xl2)
    invisible(list(data=dout, ym1=ym1, ym2=ym2, ym=ym, 
                   tab=antab2, an2=an2, mse=ans2$Mean[4]))
}

# [13-5] Two-Way ANOVA: CI for the Level Means
#' @title Two-Way ANOVA: CI for the Level Means
#' @description Estimating the confidence intervals for the level means in two-way ANOVA.
#' @param f Object (aov), Formula (with data), or Object (anova2.fit).
#' @param data Data frame applied to the formula.
#' @param alp Level of significance, Default: 0.05.
#' @param maxim Logical: maximize the response? Default: TRUE.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size of the CI plot, Default: c(7,4).
#' @param main Title of the plot.
#' @return CI table.
#'
#' @examples
#' Yield <- c(76,79,81,79,83,85, 79,81,84,86,89,88, 87,91,91,94,88,86, 79,82,85,84,77,76)
#' f1 <- c(rep(100,6), rep(150,6), rep(200,6), rep(250,6))
#' f2 <- rep(c(1,1,2,2,3,3), 4)
#' Temp <- as.factor(f1)
#' Press <- as.factor(paste0(f2, "Psig"))
#' anova2.conf(Yield ~ Temp * Press)
#'
#' bb <- anova2.fit(Yield ~ Temp * Press)
#' anova2.conf(bb)
#'
#' ee <- aov(Yield ~ Temp * Press)
#' anova2.conf(ee)
#'
#' # Without replication
#' Yield1 <- c(77.5,80.0,89.0,80.5, 80.0,85.0,92.5,84.5, 84.0,88.5,87.0,76.5)
#' Temp1 <- c(100,150,200,250, 100,150,200,250, 100,150,200,250)
#' Press1 <- c(1,1,1,1, 2,2,2,2 ,3,3,3,3)
#' anova2.conf(Yield1 ~ Temp1 * Press1)
#'
#' cc <- anova2.fit(Yield1 ~ Temp1 * Press1)
#' anova2.conf(cc)
#'
#' Temp.f <- as.factor(Temp1)
#' Press.f <- as.factor(Press1)
#' ff <- aov(Yield1 ~ Temp.f + Press.f)
#' anova2.conf(ff)
#' @rdname anova2.conf
#' @export

anova2.conf <- function(f, data, alp=0.05, maxim=TRUE, dig=4, ws=c(7,4), main) 
{
  # Check input
    if (missing(f)) stop("Input formula or aov object.")
  # aov(lm) object ------------
    if ("lm" %in% class(f)) {
        vnames <- names(f$model)
        yl <- vnames[1]
        xl1 <- vnames[2]
        xl2 <- vnames[3]
        an2 <- f
        y <- f$model[[1]]
        af1 <- f$model[[2]]
        af2 <- f$model[[3]]
      # Check Interaction
        nfac <- length(attr(f$terms, "term.labels"))
        if (nfac==3) inter <- TRUE else inter <- FALSE

  # anova2.fit object ------------
    } else if (is.list(f)) {
        an2 <- f$an2
        y <- f$data[[1]]
        af1 <- f$data[[2]]
        af2 <- f$data[[3]]
        vnames <- colnames(f$data)
        yl <- vnames[1]
        xl1 <- vnames[2]
        xl2 <- vnames[3]
      # Check Interaction
        nfac <- length(attr(an2$terms, "term.labels"))
        if (nfac==3) inter <- TRUE else inter <- FALSE

    } else if (is.language(f)) {
        vars <- all.vars(f)
        yl <- vars[1]
        xl1 <- vars[2]
        xl2 <- vars[3]
      # Check Interaction
        nfac <- dim(attr(terms(f), "factors"))[2]
        if (nfac==3) inter <- TRUE else inter <- FALSE

        if (missing(data)) {
            y <- eval(parse(text=yl))
            f1 <- eval(parse(text=xl1))
            f2 <- eval(parse(text=xl2))
        } else {
            if (!is.data.frame(data)) data <- as.data.frame(data)
            y <- data[[yl]]
            f1 <- data[[xl1]]
            f2 <- data[[xl2]]
        }
      # Transform factor levels into factors by as.factor( ) function
        if (is.factor(f1)) {
            af1 <- f1
        } else {
            af1 <- as.factor(f1)
        }
        if (is.factor(f2)) {
            af2 <- f2
        } else {
            af2 <- as.factor(f2)
        }
      # Analysis of variance
        if (inter) {
            an2 <- aov(y ~ af1*af2)
        } else {
            an2 <- aov(y ~ af1+af2) 
        }
    }

  # Calculate the mean of response variable
    ym1 <- tapply(y, af1, mean)
    ym2 <- tapply(y, af2, mean)
    ym <- tapply(y, list(af2, af1), mean)

  # Number of factor levels & data
    nn <- length(y)
    m1 <- length(unique(af1))
    m2 <- length(unique(af2))
    na <- nn/m1
    nb <- nn/m2
  # Replication => override interaction
    nr <- nn/(m1*m2)
    if (nr == 1) inter <- FALSE

  # Confidence interval of the response mean for each combination of factor levels
  # Mean square error and the tolerance of confidence intervals
    if (inter) {
        ye <- ym
        mse <- summary(an2)[[1]]$Mean[4]
        se <- sqrt(mse/nr)
        dfe <- m1*m2*(nr-1)
        fse <- paste0("\U221A(", round(mse,dig),"/",nr,")")
    } else {    
        ye <- outer(ym2, ym1, "+") - mean(y)
        mse <- summary(an2)[[1]]$Mean[3]
        se <- sqrt(mse*(1/m1+1/m2-1/(m1*m2)))
        dfe <- (m1-1)*(m2-1)
        fse <- paste0("\U221A(", round(mse,dig),
            "\U00D7(1/",m1,"+1/",m2,"-1/",m1*m2,"))")
    }

    if (maxim) {
        yopt <- max(ye)
    } else {
        yopt <- min(ye)
    }
    tval <- qt(1-alp/2, dfe)
    tol <- tval*se
    idopt <- as.vector(which(ye==yopt, arr.ind=T))

  # Confidence limits
    lcl <- ye - tol
    ucl <- ye + tol
  # Confidence interval for the best combination of factor levels
    lmax <- yopt-tol
    umax <- yopt+tol
    cat("MSE =", round(mse, dig), "\tTolerance =", round(tval, dig), "\U00D7",
        fse, " = ", round(tol, dig), "\n")

    yed <- format(ye, FALSE, dig)
    told <- round(tol, dig)
    # dum <- matrix(paste0("[", round(lcl, dig), ", ", round(ucl, dig), "]"), m2, m1)
    dum <- matrix(paste0("[", yed, " \U00B1 ", told, "]"), m2, m1)
    dimnames(dum) <- dimnames(ym)
    ##cat(paste0("[1] ", 100*(1-alp), "%"), "CI for All Means", 
    ##           "_____________________________________________\n")
    ##print(as.data.frame(dum))
    print(noquote(dum))

    ##cat(paste0("[2] ", 100*(1-alp), "%"), "CI for the Best Mean",
    ##           " _________________________________________\n")
    cat(paste0("[Best] A", idopt[2], "B", idopt[1],
        ": [", round(yopt, dig), " \U00B1 ", round(tol, dig), "] = [",
        round(lmax, dig),", ",round(umax, dig),"]\n"))

  # Confidence interval plot
    if (is.numeric(ws)) {
        ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "CI\n")
      # Colors
        if (m2<6) {
            dcol <- c("black", "red", "blue", "green4", "purple")
        } else {
            dcol <- rainbow(m2)
        }
        # lev = sort(unique(af1))
        # x1 = lev[1]-(lev[2]-lev[1])/2
        # x2 = lev[m1]+(lev[m1]-lev[m1-1])/2
        lev <- 1:m1
        x1 <- 0.5
        x2 <- m1+0.5
        ymin <- min(y, lcl)
        ymax <- max(y, ucl)
        y1 <- ymin - (ymax-ymin)*0.1
        y2 <- ymax + (ymax-ymin)*0.1

        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        mt <- paste0((1-alp)*100, "% CI for the Mean of ", yl, " w.r.t. ", xl1, " & ", xl2)

        plot(unique(af1), rep(-10000, m1), type="n",
             main=mt, ylab=yl, xlab=xl1, xlim=c(x1, x2), ylim=c(y1, y2))
        grid(col="green")
      # Lines connecting the means, the upper and lower limits
        del <- (lev[m1]-lev[1])*0.01
        dv <- del*(-(m2-1)/2):((m2-1)/2)
        for (k in 1:m2) lines(lev+dv[k], ye[k, ], type="b", lty=2, 
                              pch=17, col=dcol[k], cex=1.2)
      # Confidence intervals
        for (k in 1:m2) arrows(lev+dv[k], lcl[k,], lev+dv[k], ucl[k,], 
                               col=dcol[k], lwd=2, length=0.05,
            code=3, angle=90)
      # Display legend
        legend("topright", as.character(unique(af2)), lwd=2, 
               text.col=dcol[1:m2], col=dcol[1:m2])
    }
    invisible(rbind(ye, tol, lcl, ucl))
}

# [13-6] Two-Way ANOVA: CI for the Difference in Means
#' @title Two-Way ANOVA: CI for the Difference in Means
#' @description Estimating the confidence intervals for the difference in means in two-way ANOVA.
#' @param f Object (aov), Formula (with data), or Object (anova2.fit).
#' @param data Data frame applied to the formula.
#' @param alp Level of significance, Default: 0.05.
#' @param ci Method of CI, one of (pair, hsd, both), Default: 'pair'.
#' @param sel Selected combination levels, Default: 'h4' (high four).
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size of the CI plot, Default: c(7,4).
#' @param main Title of the plot.
#' @return CI table.
#'
#' @examples
#' Yield <- c(76,79,81,79,83,85, 79,81,84,86,89,88, 87,91,91,94,88,86, 79,82,85,84,77,76)
#' f1 <- c(rep(100,6), rep(150,6), rep(200,6), rep(250,6))
#' f2 <- rep(c(1,1,2,2,3,3), 4)
#' Temp <- as.factor(f1)
#' Press <- as.factor(paste0(f2, "Psig"))
#' anova2.diff(Yield ~ Temp * Press)
#'
#' bb <- anova2.fit(Yield ~ Temp * Press)
#' anova2.diff(bb)
#'
#' ee <- aov(Yield ~ Temp * Press)
#' anova2.diff(ee)
#'
#' # Without replication
#' Yield1 <- c(77.5,80.0,89.0,80.5, 80.0,85.0,92.5,84.5, 84.0,88.5,87.0,76.5)
#' Temp1 <- c(100,150,200,250, 100,150,200,250, 100,150,200,250)
#' Press1 <- c(1,1,1,1, 2,2,2,2 ,3,3,3,3)
#' anova2.diff(Yield1 ~ Temp1 * Press1)
#'
#' cc <- anova2.fit(Yield1 ~ Temp1 * Press1)
#' anova2.diff(cc)
#'
#' Temp.f <- as.factor(Temp1)
#' Press.f <- as.factor(Press1)
#' ff <- aov(Yield1 ~ Temp.f + Press.f)
#' anova2.diff(ff)
#' @rdname anova2.conf
#' @export

anova2.diff <- function(f, data, alp=0.05, ci="pair", sel="h4", dig=4, ws=c(7,4), main)
{
  # Check input
    if (missing(f)) stop("Input formula or aov object.")
  # aov(lm) object ------------
    if ("lm" %in% class(f)) {
        vnames <- names(f$model)
        yl <- vnames[1]
        xl1 <- vnames[2]
        xl2 <- vnames[3]
        an2 <- f
        y <- f$model[[1]]
        af1 <- f$model[[2]]
        af2 <- f$model[[3]]
      # Check Interaction
        nfac <- length(attr(f$terms, "term.labels"))
        if (nfac==3) inter <- TRUE else inter <- FALSE

  # anova2.fit object ------------
    } else if (is.list(f)) {
        an2 <- f$an2
        y <- f$data[[1]]
        af1 <- f$data[[2]]
        af2 <- f$data[[3]]
        vnames <- colnames(f$data)
        yl <- vnames[1]
        xl1 <- vnames[2]
        xl2 <- vnames[3]
      # Check Interaction
        nfac <- length(attr(an2$terms, "term.labels"))
        if (nfac==3) inter <- TRUE else inter <- FALSE

    } else if (is.language(f)) {
        vars <- all.vars(f)
        yl <- vars[1]
        xl1 <- vars[2]
        xl2 <- vars[3]
      # Check Interaction
        nfac <- dim(attr(terms(f), "factors"))[2]
        if (nfac==3) inter <- TRUE else inter <- FALSE

        if (missing(data)) {
            y <- eval(parse(text=yl))
            f1 <- eval(parse(text=xl1))
            f2 <- eval(parse(text=xl2))
        } else {
            if (!is.data.frame(data)) data <- as.data.frame(data)
            y <- data[[yl]]
            f1 <- data[[xl1]]
            f2 <- data[[xl2]]
        }
      # Transform factor levels into factors by as.factor( ) function
        if (is.factor(f1)) {
            af1 <- f1
        } else {
            af1 <- as.factor(f1)
        }
        if (is.factor(f2)) {
            af2 <- f2
        } else {
            af2 <- as.factor(f2)
        }
      # Analysis of variance
        if (inter) {
            an2 <- aov(y ~ af1*af2)
        } else {
            an2 <- aov(y ~ af1+af2) 
        }
    }

  # Calculate the mean of response variable
    ym1 <- tapply(y, af1, mean)
    ym2 <- tapply(y, af2, mean)
    ym <- tapply(y, list(af2, af1), mean)

  # Number of factor levels & data
    nn <- length(y)
    m1 <- length(levels(af1))
    m2 <- length(levels(af2))
    nr <- nn/(m1*m2)
  # Replication => override interaction
    nr <- nn/(m1*m2)
    if (nr == 1) inter <- FALSE

  # MSE and the tolerance of CIs
    if (inter) {
        ye <- ym
        mse <- summary(an2)[[1]]$Mean[4]
        se <- sqrt(mse/nr)
        dfe <- m1*m2*(nr-1)
    } else {
        ye <- outer(ym2, ym1, "+") - mean(y)
        mse <- summary(an2)[[1]]$Mean[3]
        se <- sqrt(mse*(1/m1+1/m2-1/(m1*m2)))
        dfe <- (m1-1)*(m2-1)
    }
    ye <- as.vector(ym) ## [new]
    names(ye) <- paste0(rep(1:m1, each=m2), rep(1:m2, m1)) ## [new]

  # Check if maximization
    maxim <- tolower(substr(sel, 1, 1))
    if (maxim=="h") {
        yes <- rev(sort(ye))
    } else {
        yes <- sort(ye)
    }
  # Number of selected levels [new]
    if (nchar(sel)==1) {
        nb <- m1*m2
    } else {
        nb <- as.numeric(substr(sel, 2, 2))
    }

  # Mean differences ------------------------------------
  # Select best nb combinations of factor levels
    mm1 <- mm2 <- rep(NA, nb)
    for (k in 1:nb) mm1[k] <- ceiling(which(ye==yes[k])/m2)
    for (k in 1:nb) mm2[k] <- which(ye==yes[k]) %% m2
    mm2[mm2==0] <- m2

    yeb <- yes[1:nb]
    ## ye2 <- yeb[1]-yeb[-1]
    ## for (k in 2:(m1-1)) ye2 <- c(ye2, yeb[k]-yeb[-(1:k)])

    ye2 <- NULL ## [new]
    for (i in 1:(nb-1)) for (j in (i+1):nb) ye2 <- c(ye2, yeb[i]-yeb[j]) ## [new]

    # [corr]
    ## yel2 <- paste0(round(yeb[1],dig), "-", round(yeb[-1],dig))
    ## for (k in 2:(m1-1)) yel2 <- c(yel2, paste0(round(yeb[k],dig), "-", 
    ##                              round(yeb[-(1:k)],dig)))

    ye2 <- yel2 <- NULL ## [new]
    ylv <- names(yeb) ## [new]
    for (i in 1:(nb-1)) for (j in (i+1):nb) {
        ye2 <- c(ye2, yeb[i]-yeb[j]) ## [new]
        yel2 <- c(yel2, paste0(ylv[i], "-", ylv[j]))
    }
    names(ye2) <- yel2

    ##ylv <- paste0(mm1, mm2)
    ##nylv <- paste0(ylv[1], "-", ylv[-1])
    ##for (k in 2:(m1-1)) nylv <- c(nylv, paste0(ylv[k], "-", ylv[-(1:k)]))
    ## names(ye2) <- nylv


  # Tolerance of the confidence intervals
    if (inter) {
        env <- sqrt(2/nr)
        fse <- paste0("\U221A(2\U00D7", round(mse,dig),"/",nr,")")
    } else {
        env <- sqrt(2*(1/m1+1/m2-1/(m1*m2)))
        fse <- paste0("\U221A(2\U00D7", round(mse,dig),
            "\U00D7(1/",m1,"+1/",m2,"-1/",m1*m2,"))")
    }
    tval <- qt(1-alp/2, dfe)
    tol2 <- tval*sqrt(mse)*env

  # HSD -------------------------------
    qv <- qtukey(1-alp, m1*m2, dfe)
    q2 <- qv/sqrt(2)
    htol <- q2*sqrt(mse)*env
    # Confidence limits
    lcl2 <- ye2-tol2
    ucl2 <- ye2+tol2
    # HSD
    lwr <- ye2-htol
    upr <- ye2+htol

  # CI --------------------------------
    if (ci == "pair") {
        cat("MSE =", round(mse, dig), "\tTolerance =", round(tval, dig), "\U00D7",
            fse, "=", round(tol2, dig), "\n")
        cat(paste0("[1] Paired Differences (Best ", nb,")"), 
                      "________________________________________\n")
        diffCI <- rbind(ye2, lcl2, ucl2)
        dum <- format(round(diffCI, dig), nsmall=dig)
        ## dum <- rbind(yel2, dum)
        ## rownames(dum) <- c("Means", "Diff", "LCL", "UCL")
        rownames(dum) <- c("Diff", "LCL", "UCL") ## [New]
        print(as.data.frame(dum))
    } else if (ci == "hsd") {
        cat("MSE =", round(mse, dig), "\tTol-HSD =", paste0(round(qv, dig), 
            "/\U221A", "2 \U00D7"), fse, " = ", round(htol, dig), "\n")
        cat(paste0("[1] Tukey's HSD (Best ", nb,")"), 
                   "_______________________________________________\n")
        diffCI <- rbind(ye2, lwr, upr)
        dum <- format(round(diffCI, dig), nsmall=dig)
        ## dum <- rbind(yel2, dum)
        ## rownames(dum) <- c("Means", "Diff", "LWR", "UPR")
        rownames(dum) <- c("Diff", "LWR", "UPR") ## [New]
        print(as.data.frame(dum))
    } else if (ci == "both") {
        cat("MSE =", round(mse, dig), "\tTolerance =", round(tval, dig), "\U00D7",
            fse, " = ", round(tol2, dig), "\n")
        cat("MSE =", round(mse, dig), "\tTol-HSD =", paste0(round(qv, dig), 
            "/\U221A", "2 \U00D7"), fse, " = ", round(htol, dig), "\n")
        cat(paste0("[1] Paired Diff. and HSD (Best ", nb,")"), 
                   "______________________________________\n")
        diffCI <- rbind(ye2, lcl2, ucl2, lwr, upr)
        dum <- format(round(diffCI, dig), nsmall=dig)
        ## dum <- rbind(yel2, dum)
        ## rownames(dum) <- c("Level", "Diff", "LCL", "UCL", "LWR", "UPR")
        rownames(dum) <- c("Diff", "LCL", "UCL", "LWR", "UPR") ## [New]
        print(as.data.frame(dum))
    }

    n2 <- length(ye2)
  # Set confidence intervals and HSD plots
    if (is.numeric(ws)) {
        dcol <- rep("green2", n2)
        dcol2 <- rep("steelblue", n2)
        if (ci == "pair") {
            ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "CI\n")
            y1 <- min(lcl2) - 0.1*(max(ucl2)-min(lcl2))
            y2 <- max(ucl2) + 0.1*(max(ucl2)-min(lcl2))
            if (missing(main)) {
                mt <- paste0((1-alp)*100, "% CI for the Mean Differences (Best ", nb, ")")
            } else {
                mt <- main
            }
            dcol[lcl2>0 | ucl2<0] <- "orange"
            lop <- lcl2
            upp <- ucl2
        } else if (ci == "hsd") {
            ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "HSD\n")
            y1 <- min(lwr) - 0.1*(max(upr)-min(lwr))
            y2 <- max(upr) + 0.1*(max(upr)-min(lwr))           
            if (missing(main)) {
                mt <- paste0((1-alp)*100, "% HSD for the Means (Best ", nb, ")")
            } else {
                mt <- main
            }
            dcol[lwr>0 | upr<0] <- "orange"
            lop <- lwr
            upp <- upr
        } else if (ci == "both") {
            ##cat(paste0("[P1] Plot of ", (1-alp)*100, "%"), "CI and HSD\n")
            y1 <- min(lwr) - 0.1*(max(upr)-min(lwr))
            y2 <- max(upr) + 0.1*(max(upr)-min(lwr))
            if (missing(main)) {
                mt <- paste0((1-alp)*100, "% paired CI and HSD (Best ", nb, ")")
            } else {
                mt <- main
            }
            dcol[lcl2>0 | ucl2<0] <- "orange"
            dcol2[lwr>0 | upr<0] <- "magenta"
            lop <- lcl2
            upp <- ucl2
        }

      # Draw confidence interval and HSD plots
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        plot(ye2, type="n", xlim=c(0.5, n2+0.5), ylim=c(y1, y2),
            main=mt, ylab=paste("Mean Differences of", yl),
            xlab=paste("Combinations of", xl1, "&", xl2), xaxt="n")
        grid(col="green")

        # Horizontal line of zero difference
        abline(h=0, lty=3, col=grey(0.2))

        # Confidence intervals
        arrows(1:n2, lop, 1:n2, upp, lwd=2, length=0.1, code=3, angle=90, col=dcol)

        # Lines connecting the mean differences, the upper and lower limits
        lines(1:n2, lop, type="b", pch=18, lty=2, col="blue")
        lines(1:n2, upp, type="b", pch=18, lty=2, col="blue")

        # Values of the mean differences, the upper and lower limits
        text(1:n2, ye2, labels=round(ye2, 2), cex=0.9, col="red", pos=4)
        text(1:n2, lop, labels=round(lop, 2), cex=0.9, col=1, pos=4)
        text(1:n2, upp, labels=round(upp, 2), cex=0.9, col=1, pos=4)
        lines(1:n2, ye2, type="b", pch=17, lty=2, col="red", cex=1.2)
        mtext(names(ye2), side=1, at=1:n2, col=1, line=0.5)

        if (ci == "both") {
            at2 <- 1:n2-0.1
            arrows(at2, lwr, at2, upr, lwd=2, length=0.1, code=3, angle=90, col=dcol2)

            # Values of the mean differences, the upper and lower limits
            text(at2, lwr, labels=round(lwr, 2), cex=0.9, col="blue", pos=1)
            text(at2, upr, labels=round(upr, 2), cex=0.9, col="blue", pos=3)
        }
    } # End of plot
    invisible(diffCI)
}

