# [Ch-14 Functions] ----------------------------------------------------------------------------------
# [Ch-14 Function Manual] -----------------------------------------

#' @title Manual for Ch14. Functions
#' @description Ch14. Correlation and Regression Analysis
#' @param fn Function number, Default: 0
#' @return None.
#'
#' @examples
#' ch14.man()
#' ch14.man(1)
#' ch14.man(1:3)
#' ch14.man(c(2,4,6))
#' @rdname ch14.man
#' @export
ch14.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] sreg.pre\t    Simple Regression: Correlation Analysis\n")
        cat("[2] sreg.lse\t    Simple Regression: Least Square Estimation\n")
        cat("[3] sreg.pred\t    Simple Regression: Prediction and Confidence Intervals\n")
        cat("[4] mreg.lse\t    Multiple Regression: Least Square Estimation\n")
        cat("[5] mreg.pred\t    Multiple Regression: Prediction and Confidence Intervals\n")
        cat("[6] mreg.diff\t    Multiple Regression: Test on Two Models\n")
    }
    if (1 %in% fn) {
        cat("[1] Simple Regression: Correlation Analysis\n")
        cat("sreg.pre(x, y, data, r0, alt=\"two\", alp=0.05, dig=4, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Independent (explanatory) variable (or formula).\n")
        cat("y\t Dependent (response) variable.\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to x and y (or formula).\n")
        cat("r0\t Correlation coefficient value under H0 (for test).\n")
        cat("alt\t Type of alternative, one of (\"two\", \"gr\", \"le\").\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Simple Regression: Least Square Estimation\n")
        cat("sreg.lse(x, y, data, type=\"lse\", alp=0.05, dig=4, dp, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Independent (explanatory) variable (or formula).\n")
        cat("y\t Dependent (response) variable.\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to x and y (or formula).\n")
        cat("type\t One of (\"lse\", \"test\", \"aov\", \"all\").\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("dp\t Selection number of diagnostic plots.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Simple Regression: Prediction and Confidence Intervals\n")
        cat("sreg.pred(x, y, data, x0, alp=0.05, dig=4, ws=\"n\", ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Independent variable (or formula or lm() object).\n")
        cat("y\t Dependent variable (optional).\n")
        cat("x0\t Conditional value of the independent variable.\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to x and y (or formula).\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] Multiple Regression: Least Square Estimation\n")
        cat("mreg.lse(form, data, type=\"lse\", alp=0.05, dig=4, ws=\"n\")\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("form\t Formula for the regression model (eg: y ~ x1 + x2).\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("type\t One of (\"lse\", \"test\", \"aov\", \"all\").\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
    }
    if (5 %in% fn) {
        cat("[5] Multiple Regression: Prediction and Confidence Intervals\n")
        cat("mreg.pred(form, data, newd, alp=0.05, dig=4, ws=\"n\", pvx=1, ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("form\t Formula for the regression model (or lm() object).\n")
        cat("newd\t A new data frame of independent variables.\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: \"n\".\n")
        cat("pvx\t Order of selected independent variable, Default: 1.\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (6 %in% fn) {
        cat("[6] Multiple Regression: Test on Two Models\n")
        cat("mreg.diff(form1, form2, data, alp=0.05, dig=4, dp=c(1:3,5))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("form1\t Formula for a regression model (or lm() object).\n")
        cat("form2\t Formula for another regression model (or lm() object).\n")
        cat("[Optional Input]--------------------------\n")
        cat("data\t Data frame applied to the formula.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("dp\t Selection number of diagnostic plots, Default: c(1:3,5).\n")
    }
}

# [14-1] Simple Regression: Correlation Analysis
#' @title Preliminary Analysis for a Simple Linear Regression
#' @description Preliminary Analysis via a Scatter Plot for a Simple Linear Regression.
#' @param x Independent (explanatory) variable (or formula).
#' @param y Dependent (response) variable.
#' @param data Data frame applied to x and y (or formula).
#' @param r0 Correlation coefficient value under H0 (for test).
#' @param alt Type of alternative, one of ("two", "gr", "le"), Default: 'two'.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return List of statistics.
#'
#' @examples
#' # First two columns of "iris" data set
#' sreg.pre(iris[[1]], iris[[2]])
#' sreg.pre(1, 2, iris)
#' # Test for correlation
#' sreg.pre(1, 2, iris, r0=0, ws=c(7,4))
#' # The 3rd and 4th columns of "iris" data set
#' sreg.pre(iris[[3]], iris[[4]], ws=c(7,4))
#' sreg.pre(3, 4, iris, ws=c(7,4))
#'
#' x <- c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y <- c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' sreg.pre(x, y)
#' # Test for correlation
#' sreg.pre(x, y, r0=0.9, ws=c(7,4))
#' sreg.pre(y ~ x, r0=0.9, ws=c(7,4))
#' # Data transformation
#' (y2 <- y*x/1000)
#' sreg.pre(x, y2)
#' # Test for correlation coefficient
#' sreg.pre(x, y2, r0=0.6, alt="gr", ws=c(7,4))
#' @rdname sreg.pre
#' @export

# sreg.pre <- function(x, y, test=FALSE, r0=0, alt="two", alp=0.05, 
#                 plot=FALSE, res=FALSE, xl, yl, mt, dig=4) {
sreg.pre <- function(x, y, data, r0, alt="two", alp=0.05, dig=4, ws="n", ...)
{
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Check input
    if (missing(x)) stop("Input independent variable or formula...")

  # Classify Cases
    if (!missing(data)) is.dat <- TRUE else is.dat <- FALSE
    if (!missing(y)) is.y <- TRUE else is.y <- FALSE

  # Case: with data
    if (is.dat) {
        names <- names(data)
        if (is.y) {
            xl <- deparse(substitute(x))
            yl <- deparse(substitute(y))
            if (yl %in% names) {
                y <- data[[yl]]
            } else {
                y <- data[[y]]
            }
            if (xl %in% names) {
                x <- data[[xl]]
            } else {
                x <- data[[x]]
            }
        } else {
            if (is.language(x)) {
                vars <- all.vars(x)
                yl <- vars[1]
                xl <- vars[2]
                y <- data[[yl]]
                x <- data[[xl]]
            } else {
                stop("Input dependent variable...")
            }
        }
  # Case: without data
    } else {
        if (is.y) {
            xl <- deparse(substitute(x))
            yl <- deparse(substitute(y))
        } else {
            if (is.language(x)) {
                vars <- all.vars(x)
                yl <- vars[1]
                xl <- vars[2]
                y <- eval(parse(text=yl), envir = parent.frame(2))
                x <- eval(parse(text=xl), envir = parent.frame(2))
            } else {
                stop("Input dependent variable...")
            }
        }
    }

  # Estimate correlation coefficients
    nn <- length(y)
    Sxx <- sum(x^2)-sum(x)^2/nn
    Syy <- sum(y^2)-sum(y)^2/nn
    Sxy <- sum(x*y)-sum(x)*sum(y)/nn
    rxy <- Sxy/sqrt(Sxx*Syy)
    bstat <- c(Sxy/(nn-1), Sxx/(nn-1), Syy/(nn-1), rxy)
    names(bstat) <- c("Cov", "Vx", "Vy", "Corr")

    if (missing(r0)) {
        cat("[Calculating Sample Correlation Coefficient] __________\n")
        cat(paste0("Sxx = ", myf0(sum(x^2)), " - (", myf0(sum(x)), 
            ")\U00B2", "/ ", nn, " = ", myf0(Sxx)), "\n")
        cat(paste0("Syy = ", myf0(sum(y^2)), " - (", myf0(sum(y)), 
            ")\U00B2", "/ ", nn,    " = ", myf0(Syy)), "\n")
        cat(paste0("Sxy = ", myf0(sum(x*y)), " - (", myf0(sum(x)), 
            "\U00D7", myf0(sum(y)), ") / ", nn, " = ", myf0(Sxy)), "\n")
        cat(paste0("Cor(xy) = ", myf0(Sxy), " / \U221A(", myf0(Sxx),
            "\U00D7", myf0(Syy), ") = ", myf1(rxy)), "\n")

    # Correlation Test ------------------------
    } else {
        nalt <- alt.num(alt)
      # Correlation test by cor.test( ) function
        ctalt <- switch(nalt, "less", "greater", "two.sided")
        ct <- cor.test(x, y, alternative =ctalt, conf.level = 1-alp)
        df <- nn-2
        T0 <- rxy*sqrt(df/(1-rxy^2))

        cat("[Test for Correlation Coefficient,", paste0("H0: rho=", myf0(r0),"]"),
            "__________\n")
        cat("Sample Corr. Coef. =", myf1(ct$est), " ")
        if (r0==0) {
            cv0 <- switch(nalt, qt(alp, df), qt(1-alp, df), qt(1-alp/2, df))
            cat("Critical value =", myf1(cv0), "\n")
            cat(paste0("T-stat = ", myf1(rxy), " \U00D7 \U221A(", df,
                "/(1-", myf1(rxy), "\U00B2)) = ", myf1(ct$stat), "\n"))
 
            pv0 <- switch(nalt, pt(T0, df), 1 - pt(T0, df), 2*pt(-abs(T0), df))
            if (pv0 > alp) sign2 <- ">" else sign2 <- "<"

            if (nalt==3) {
                cat(paste0("p-value = P(|T", df, "| > ", myf1(abs(T0)), 
                    ") = ", format(pv0, F, dig)), sign2, alp, "\n")
            } else if (nalt==1) {
                cat(paste0("p-value = P(T", df, " < ", myf1(T0), 
                    ") = ", format(pv0, F, dig)), sign2, alp, "\n")
            } else if (nalt==2) {
                cat(paste0("p-value = P(T", df, " > ", myf1(T0), 
                    ") = ", format(pv0, F, dig)), sign2, alp, "\n")
            }
            otest <- c(r0, rxy, T0, pv0)
            names(otest) <- c("rho0", "r_xy", "T_0", "p-val")
        } else {
            cv0 <- switch(nalt, qnorm(alp), qnorm(1-alp), qnorm(1-alp/2))
            cat("Critical value =", myf1(cv0), "\n")

            Z0 <- sqrt(nn-3)*0.5*(log((1+rxy)/(1-rxy))-log((1+r0)/(1-r0)))
            cat(paste0("Z-stat = \U221A", nn-3, "\U00D7", "0.5", "\U00D7[log(",
                myf1(1+rxy), "/", myf1(1-rxy),
                ")-log(", 1+r0, "/", 1-r0, ")] = ", myf1(Z0), "\n"))

            pv0 <- switch(nalt, pnorm(Z0), 1-pnorm(Z0), 2*pnorm(-abs(Z0)))
            if (pv0 > alp) sign2 <- ">" else sign2 <- "<"

            if (nalt==3) {
                pv0 <- 2*pnorm(-abs(Z0))
                cat(paste0("p-value = P(|Z| > ", myf1(abs(Z0)), ") = ",
                    format(pv0, F, dig)), sign2, alp, "\n")
            } else if (nalt==1) {
                pv0 <- pnorm(Z0)
                cat(paste0("p-value = P(Z < ", myf1(Z0), ") = ",
                    format(pv0, F, dig)), sign2, alp, "\n")
            } else if (nalt==2) {
                pv0 <- 1 - pnorm(Z0)
                cat(paste0("p-value = P(Z > ", myf1(Z0), ") = ",
                    format(pv0, F, dig)), sign2, alp, "\n")
            }
            otest <- c(r0, rxy, Z0, pv0)
            names(otest) <- c("rho0", "r_xy", "Z_0", "p-val")
        } # End of r0

        # Confidence intervals for correlation coefficients
        ##cat("[2] Confidence Interval _____________\n")
        cat(paste0(100*(1-alp), "% Confidence Interval = [",
            myf1(ct$conf[1]), ", ", myf1(ct$conf[2]), "]\n"))
        otest <- c(otest, ct$conf)
        names(otest)[5:6] <- c("lcl", "ucl")
    } # End of test

    if (is.numeric(ws)) {
      # Set labels & simple regression
        mt <- paste0("Scatter Plot of ", xl, " vs. ", yl)
        mt2 <- paste0("Simple Linear Regression of ", yl, " w.r.t. ", xl)
        resid <- FALSE
        xrng <- c(min(x), max(x))
        lm1 <- lm(y~x)
        y1 <- floor(min(y, lm1$fit))
        y2 <- ceiling(max(y, lm1$fit))
        yrng <- c(y1, y2)
        pch <- 19
        cex <- 1
        col <- "red"
        lty <- 2
        lwd <- 1
      # Set the list of arguments
        dots <- list(...)
        if (length(dots) > 0) {
            pars <- names(dots)
            if ("resid" %in% pars) resid <- dots$resid
            if (resid==TRUE) mt <- mt2
            if ("main" %in% pars) mt <- dots$main
            if ("ylab" %in% pars) yl <- dots$ylab
            if ("xlab" %in% pars) xl <- dots$xlab
            if ("xlim" %in% pars) xrng <- dots$xlim
            if ("ylim" %in% pars) yrng <- dots$ylim
            if ("pch" %in% pars) pch <- dots$pch
            if ("cex" %in% pars) cex <- dots$cex
            if ("col" %in% pars) col <- dots$col
            if ("lty" %in% pars) lty <- dots$lty
            if ("lwd" %in% pars) lwd <- dots$lwd
        }
      # Scatter plot for prior investigation
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        plot(x, y, pch=pch, cex=cex, main=mt, xlab=xl, ylab=yl, ylim=yrng, xlim=xrng)
        grid(col="green")
        abline(lm1, lty=lty, lwd=lwd, col=col)

        if (resid==TRUE) {
          # Plot regression equation
            pos <- ifelse(lm1$coef[[2]]>0, "bottomright", "topright")
            sign <-  ifelse(lm1$coef[[2]]>0, "+", "")
            legend(pos, c("Regression Equation", paste0("Y = ", 
                  round(lm1$coef[[1]],dig), sign, round(lm1$coef[[2]],dig), "X")), 
                  text.col=c("black","blue"), bg="white")
          # Plot residuals
            segments(x, y, x, lm1$fit, lty=2, col="blue")
            text(x, (y+lm1$fit)/2, labels="e", col="blue", pos=4)
        }
    } # End of plot

    if (!missing(r0)) {
        if (is.numeric(ws)) {
            invisible(list(scc=bstat, test=otest, coef=lm1$coef))
        } else {
            invisible(list(scc=bstat, test=otest))
        }
    } else {
        if (is.numeric(ws)) {
            invisible(list(scc=bstat, coef=lm1$coef))
        } else {
            invisible(list(scc=bstat))
        }
    }
}

# [14-2] Simple Regression: Least Square Estimation
#' @title Simple Regression: Least Square Estimation
#' @description Calculate the least square estimates, test on the significance, or test on the coefficients for a simple regression.
#' @param x Independent (explanatory) variable (or formula).
#' @param y Dependent (response) variable.
#' @param data Data frame applied to x and y (or formula).
#' @param type One of ("lse", "test", "aov", "all").
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param dp Selection number of diagnostic plots.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return Least square estimates, ANOVA table, or test statistics
#'
#' @examples
#' sreg.lse(Petal.Length ~ Sepal.Length, data=iris)
#' sreg.lse(1, 3, data=iris)
#' sreg.lse(1, 3, data=iris, type="aov")
#' sreg.lse(1, 3, data=iris, type="test")
#' sreg.lse(1, 3, data=iris, type=c("lse","aov"), dp=c(1:3,5))
#'
#' x <- c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y <- c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 <- y*x/1000)
#' sreg.lse(x, y2)
#' sreg.lse(y2 ~ x)
#' sreg.lse(y2 ~ x, type="aov")
#' sreg.lse(y2 ~ x, type="test")
#' sreg.lse(y2 ~ x, type="all", dp=c(1:3,5))
#' @rdname sreg.lse
#' @export

sreg.lse <- function(x, y, data, type="lse", alp=0.05, dig=4, dp, ws="n", ...) 
{
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Check input
    if (missing(x)) stop("Input independent variable or formula or lm() object...")

  # Classify Cases
    if (!missing(data)) is.dat <- TRUE else is.dat <- FALSE
    if (!missing(y)) is.y <- TRUE else is.y <- FALSE

  # Case: with data
    if (is.dat) {
        names <- names(data)
        if (is.y) {
            xl <- deparse(substitute(x))
            yl <- deparse(substitute(y))
            if (yl %in% names) {
                y <- data[[yl]]
            } else {
                y <- data[[y]]
            }
            if (xl %in% names) {
                x <- data[[xl]]
            } else {
                x <- data[[x]]
            }
        } else {
            if (is.language(x)) {
                vars <- all.vars(x)
                yl <- vars[1]
                xl <- vars[2]
                y <- data[[yl]]
                x <- data[[xl]]
            } else {
                stop("Input dependent variable...")
            }
        }
  # Case: without data
    } else {
        if (is.y) {
            xl <- deparse(substitute(x))
            yl <- deparse(substitute(y))
        } else {
            if (is.language(x)) {
                vars <- all.vars(x)
                yl <- vars[1]
                xl <- vars[2]
                y <- eval(parse(text=yl), envir = parent.frame(2))
                x <- eval(parse(text=xl), envir = parent.frame(2))
            } else {
                stop("Input dependent variable...")
            }
        }
    }

  # -----------------------------------------------------------------  
    nn <- length(x)
    lm1 <- lm(y ~ x)
    an1 <- anova(lm1)

  # Sum of squares
    Sxx <- sum(x^2)-sum(x)^2/nn
    Syy <- sum(y^2)-sum(y)^2/nn
    Sxy <- sum(x*y)-sum(x)*sum(y)/nn

  # LSE ---------------------
    b1 <- Sxy/Sxx
    xb <- mean(x)
    yb <- mean(y)
    b0 <- yb - b1*xb
    sign <-  ifelse(b1>0, "+", "")
    olse <- c(b0, b1)
    names(olse) <- c("Intercept", "Slope")

    kk <- 0
  # Estimate simple regression coefficients
    if (("lse" %in% type) || ("all" %in% type)) {
        kk <- kk + 1
        ##cat(paste0("[", kk, "]"), "LSE Calculation _________________________________\n")
        cat(paste0("Sxx = ", myf0(sum(x^2)), " - (", myf0(sum(x)),
              ")\U00B2", "/ ", nn, " = ", myf0(Sxx)), "\n")
        cat(paste0("Sxy = ", myf0(sum(x*y)), " - (", myf0(sum(x)),
              "\U00D7", myf0(sum(y)), ") / ", nn, " = ", myf0(Sxy)), "\n")
        cat(paste0("[LSE] b1 = ", myf0(Sxy), " / ", myf0(Sxx), " = ",
              format(b1,F,dig)), "\n")
        cat(paste0("[LSE] b0 = ", myf0(yb), " - ", myf0(b1),
              "\U00D7", myf0(xb), " = ", myf0(b0)), "\n")
        cat(paste0("[Regression Equation] y = ", myf0(b0), " ", sign, " ",
              format(abs(b1),F,dig), " x"), "\n")
    }

  # ANOVA ---------------------------
    SST <- Syy
    SSR <- Sxy^2 / Sxx
    SSE <- SST-SSR
    Rsq <- SSR/SST
    MSE <- SSE/(nn-2)

    cr.v <- c(qf(1-alp, an1$Df[1], an1$Df[2]), NA)
    antab <- matrix(NA, 3, 6)
    colnames(antab) <- c("Sum Sq.", "df", "Mean Sq.", "F0", "C-value", "P-value")
    rownames(antab) <- c("Regress", "Residual", "Total")
    antab[1:2,] <- cbind(an1$Sum, an1$Df, an1$Mean, an1$F, cr.v, an1$Pr)
    antab[3,1:2] <- c(sum(an1$Sum), sum(an1$Df))

  # ANOVA table by calculating sum of squares
    if (("aov" %in% type) || ("all" %in% type)) {
        cat("[Calculating Sum of Squares] _________________\n")
        cat(paste0("SST = ", myf0(sum(y^2)), " - (", myf0(sum(y)), ")\U00B2",
            "/ ", nn,    " = ", myf1(SST)), "\n")
        cat(paste0("SSR = (", myf0(Sxy), ")\U00B2", "/ ", myf0(Sxx),
            " = ", myf1(SSR)), "\n")
        cat(paste0("SSE = ", myf1(SST), " - ", myf1(SSR),
            " = ", myf1(SSE)), "\n")

      # p-value
        pv1 <- an1$Pr[1]
        if (pv1<0.001) {
            star <- "***"
        } else if (pv1<0.01) {
            star <- "**"
        } else if (pv1<0.05) {
            star <- "*"
        } else {
            star <- ""
        }

        cat("[ANOVA Table] ________________________________\n")
        dum <- as.data.frame(antab)
        ## dum <- format(dum, F, dig)
        dum[[2]] <- myf0(dum[[2]])
        for (k in c(1,3:5)) dum[[k]] <- myf1(dum[[k]])
        dum[[6]] <- format(dum[[6]], F, dig)
        for (k in 3:6) dum[, k] <- sub("NA", "  ", dum[, k])
        print(dum)

        ##cat("-------------------------------------------------\n")
        cat(paste0("R-square = ", myf1(SSR), " / ", myf1(SST),
            " = ", myf1(Rsq)), "\n")
    } # End of aov

  # Test for the Regression Coefficient
    se1 <- sqrt(MSE/Sxx)
    se0 <- sqrt(MSE*(1/nn+xb^2/Sxx))

  # Confidence intervals & significance tests
    tval <- qt(1-alp/2, nn-2)
    conf <- 100*(1-alp)
    tol1 <- tval*se1
    tol0 <- tval*se0
    tstat1 <- b1/se1
    tstat0 <- b0/se0
    pv1 <- 2*pt(-abs(tstat1), nn-2)
    pv0 <- 2*pt(-abs(tstat0), nn-2)
  # The test statistics
    out <- rbind(c(b1, tol1, tstat1, pv1), c(b0, tol0, tstat0, pv0))
    colnames(out) <- c("LSE", "Tol", "Stat", "p-val")

    if (("test" %in% type) || ("all" %in% type)) {
        astat1 <- format(round(abs(tstat1), dig), nsmall=dig)
        astat0 <- format(round(abs(tstat0), dig), nsmall=dig)

        lcl1 <- format(round(b1-tol1, dig), nsmall=dig)
        ucl1 <- format(round(b1+tol1, dig), nsmall=dig)
        lcl0 <- format(round(b0-tol0, dig), nsmall=dig)
        ucl0 <- format(round(b0+tol0, dig), nsmall=dig)

        cat("[Tests for Regression Coefficient] __________\n")
        cat(paste0("T1 = ", myf1(b1), "/", myf1(se1), " = ", myf1(tstat1), 
            "\t P-val = P(|T_", nn-2, "| > ", astat1, ") = ", format(pv1,F,dig)), "\n")
        cat(paste0("T0 = ", myf1(b0), "/", myf1(se0), " = ", myf1(tstat0),
            "\t P-val = P(|T_", nn-2, "| > ", astat0, ") = ", format(pv0,F,dig)), "\n")

        ##cat("Confidence Intervals _______________________\n")
        cat(paste0(conf, "%CI(b1): [", myf1(b1), "\U00B1", myf1(tval), 
            "\U00D7", myf1(se1), "]=[", myf1(b1), "\U00B1", myf1(tol1), 
            "]=[", lcl1, ", ", ucl1, "]\n"))
        cat(paste0(conf, "%CI(b0): [", myf1(b0), "\U00B1", myf1(tval), 
            "\U00D7", myf1(se0), "]=[", myf1(b0), "\U00B1", myf1(tol0), 
            "]=[", lcl0, ", ", ucl0, "]\n"))
    } # End of test

  # Draw Diagnosis Plots of a Simple Regression Model
    if (!missing(dp) && is.numeric(dp)) {
        nw <- length(dp)
      # Diagnosis of the regression model
        ##cat("[P] Diagnosis Plots __________________________________________________\n")
        # [Correction]
        nn <- switch(nw, c(4,4,1,1), c(6,3,1,2), c(9,3,1,3), c(7,6,2,2), 
                         c(9,6,2,3), c(9,6,2,3))
        win.graph(nn[1], nn[2])
        par(mfrow=c(nn[3], nn[4]))
        plot(lm1, which=dp)
    }
    rout <- list(lse=olse, aov=antab, test=out)
    invisible(rout)
}

# [14-3] Simple Regression: Prediction and Confidence Intervals
#' @title Prediction and Confidence Interval
#' @description Prediction and Confidence Interval in a Simple Regression.
#' @param x Independent variable (or formula or lm() object).
#' @param y Dependent (response) variable.
#' @param data Data frame applied to x and y (or formula).
#' @param x0 Conditional value of the independent variable.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @param ... Other graphic parameters.
#' @return CI and PI.
#'
#' @examples
#' sreg.pred(iris[[3]], iris[[4]], x0=2)
#' sreg.pred(iris[[3]], iris[[4]], x0=2, ws=c(7,5))
#'
#' x <- c(1095, 1110, 1086, 1074, 1098, 1105, 1163, 1124, 1088, 1064)
#' y <- c(49, 52, 48, 49, 50, 51, 50, 51, 49, 48)
#' (y2 <- y*x/1000)
#' sreg.pred(x, y2, x0=1200, ws=c(7,5))
#'
#' @rdname sreg.pred
#' @export

sreg.pred <- function(x, y, data, x0, alp=0.05, dig=4, ws="n", ...) 
{
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Check input
    if (missing(x)) stop("Input independent variable or formula...")

  # Classify Cases
    if (!missing(data)) is.dat <- TRUE else is.dat <- FALSE
    if (!missing(y)) is.y <- TRUE else is.y <- FALSE

  # Case: with data
    if (is.dat) {
        names <- names(data)
        if (is.y) {
            xl <- deparse(substitute(x))
            yl <- deparse(substitute(y))
            if (yl %in% names) {
                y <- data[[yl]]
            } else {
                y <- data[[y]]
            }
            if (xl %in% names) {
                x <- data[[xl]]
            } else {
                x <- data[[x]]
            }
            lm1 <- lm(y ~ x)
        } else {
            if (is.language(x)) {
                vars <- all.vars(x)
                yl <- vars[1]
                xl <- vars[2]
                y <- data[[yl]]
                x <- data[[xl]]
                lm1 <- lm(y ~ x)
            } else {
                stop("Input dependent variable...")
            }
        }
  # Case: without data
    } else {
        if (is.y) {
            xl <- deparse(substitute(x))
            yl <- deparse(substitute(y))
            lm1 <- lm(y ~ x)
        } else {
            if (is.language(x)) {
                vars <- all.vars(x)
                yl <- vars[1]
                xl <- vars[2]
                y <- eval(parse(text=yl), envir = parent.frame(2))
                x <- eval(parse(text=xl), envir = parent.frame(2))
                lm1 <- lm(y ~ x)
            } else if ("lm" %in% class(x)) {
                lm1 <- x
                y <- lm1$model[[1]]
                x <- lm1$model[[2]]
                yl <- names(lm1$model)[1]
                xl <- names(lm1$model)[2]
            } else {
                stop("Input dependent variable...")
            }
        }
    }

  # Common Procedure
    if (missing(x0)) x0 <- max(x)

    nn <- length(x)
    xb <- mean(x)
    yb <- mean(y)
    Sxx <- sum(x^2)-sum(x)^2/nn
    b0 <- lm1$coef[[1]]
    b1 <- lm1$coef[[2]]
    MSE <- anova(lm1)$Mean[2]
    tval <- qt(1-alp/2, nn-2)
    conf <- 100*(1-alp)

  # Confidence intervals and prediction intervals at x0
    Ex0 <- b0+b1*x0
    vcx0 <- MSE*(1/nn+(x0-xb)^2/Sxx)
    vpx0 <- MSE*(1+1/nn+(x0-xb)^2/Sxx)
    cse <- sqrt(vcx0)
    pse <- sqrt(vpx0)
    ctol <- tval*cse
    ptol <- tval*pse

    if (is.numeric(ws)) plot <- TRUE else plot <- FALSE
    if (!plot) {
        cat("[1] Confidence interval for E[Y|x0] _______________________________\n")
        cat(paste0(conf, "% CI: [", round(Ex0, dig), "\U00B1",
            round(tval, dig), "\U00D7", round(cse, dig),
            "]=[", round(Ex0, dig), "\U00B1", round(ctol, dig), "]=[",
            round(Ex0-ctol,dig), ", ", round(Ex0+ctol,dig), "]\n"))
        cat("[2] Prediction interval for Y|x0 __________________________________\n")
        cat(paste0(conf, "% PI: [", round(Ex0, dig), "\U00B1",
            round(tval, dig), "\U00D7", round(pse, dig),
            "]=[", round(Ex0, dig), "\U00B1", round(ptol, dig), "]=[",
            round(Ex0-ptol,dig), ", ", round(Ex0+ptol,dig), "]\n"))
        otab = rbind(c(x0, Ex0, cse, ctol, Ex0-ctol, Ex0+ctol),
             c(x0, Ex0, pse, ptol, Ex0-ptol, Ex0+ptol))
        colnames(otab) <- c("x0", "Ex0", "s.e.", "Tol", "Low", "Upp")
        rownames(otab) <- c("CI", "PI")
    }

  # Plot confidence bands and prediction bands
    if (plot) {
        ##cat("[P] Plot of Confidence Bands and Prediction Bands\n")
      # Set titles, axes, and graphic parameters
        mt <- paste("Confidence and Prediction Bands of", yl, "given", xl)
        x1 <- min(x0, x)
        x2 <- max(x0, x)
        xrng <- c(x1-0.1*(x2-x1), x2+0.1*(x2-x1))
        by <- (x2-x1)/50
        pch <- 19
        cex <- 1
        lty <- 1
        lwd <- 1
        col <- "blue"
      # Set the list of arguments
        dots <- list(...)
        if (length(dots) > 0) {
            pars <- names(dots)
            if ("main" %in% pars) mt <- dots$main
            if ("xlim" %in% pars) xrng <- dots$xlim
            if ("col" %in% pars) col <- dots$col
            if ("pch" %in% pars) pch <- dots$pch
            if ("cex" %in% pars) cex <- dots$cex
            if ("lwd" %in% pars) lwd <- dots$lwd
            if ("lty" %in% pars) lty <- dots$lty
            if ("ylab" %in% pars) yl <- dots$ylab
            if ("xlab" %in% pars) xl <- dots$xlab
        }

      # Set data frame
        nd <- data.frame(x=seq(xrng[1], xrng[2], by=by))
      # Confidence intervals and prediction intervals
        conf2 <- predict(lm1, interval="confidence", newdata=nd)
        pred2 <- predict(lm1, interval="prediction", newdata=nd)
        y1 <- min(pred2[, "lwr"])
        y2 <- max(pred2[, "upr"])
        ymin <- y1 - (y2-y1)*0.1
        ymax <- y2 + (y2-y1)*0.1
        yrng <- c(ymin, ymax)

     # Plot confidence bands and prediction bands
        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))
        plot(x, y, pch=pch, cex=cex, main=mt, 
             xlab=xl, ylab=yl, ylim=yrng, xlim=xrng)

      # Regression line, confidence band, and prediction band
        abline(lm1, lty=lty, lwd=lwd, col=col)
        abline(v=xb, lty=2, col="green")
        text(xb, ymin, labels=expression(bar(x)), pos=4)
        matlines(nd$x, conf2[,c("lwr","upr")], col="red", type="p", pch="+")
        matlines(nd$x, pred2[,c("lwr","upr")], col="blue", type="p", pch=1)
        abline(v=x0, lty=2, col="orange")
        text(x0, ymin, labels=expression(x[0]), cex=0.9, col="green4", pos=4)
        text(x0, Ex0+c(-ptol, 0, ptol), labels=round(Ex0+c(-ptol, 0, ptol), dig),
            cex=0.8, col="green4", pos=c(1,1,3))
    }

    # Return the results
    if (plot) {
        invisible(list(CI=conf2, PI=pred2))
    } else {
        invisible(otab)
    }
}

# Multiple Regression Analysis --------------------------------------------
# Scatter plot matrix with correlation coefficients
panel.cor = function(x, y, alp=0.05, digits=4, prefix="") {
    ## usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    cxy <- cor(x, y)
    if (cxy<0) prefix <- "-"
    r <- abs(cxy)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    ## if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
    cex.cor <- 1.0/sqrt(strwidth(txt))
    txt <- paste0(prefix, txt)
    cc2 <- cex.cor*0.8
    text(0.5, 0.6, format(cxy, F, digits, nsmall=digits), 
        col=ifelse(cxy>=0, "red", "blue"), cex = cex.cor)
    pv <- cor.test(x, y, conf.level =1-alp)$p.val
    text(0.5, 0.3, paste0("P-v=", format(pv, F, digits)), cex = 1.5, col=1)
}

# [14-4] Multiple Regression: Least Square Estimation
#' @title Calculate Least Square Estimates: Multiple Regression
#' @description Calculate Least Square Estimates for a Multiple Regression.
#' @param form Formula of the regression model (ex: y ~ x1 + x2)
#' @param data Data frame applied to the formula.
#' @param type Type of analysis, one of ("lse", "test", "aov", "all").
#' @param test Logical value for testing on the coefficients, Default: FALSE.
#' @param alp Level of significance, Default: 0.05.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: "n".
#' @return Matrices for calculating the LSE.
#'
#' @examples
#' # Data set 'mtcars'
#' form <- mpg ~ hp + drat + wt
#' mreg.lse(form, data=mtcars)
#' mreg.lse(form, data=mtcars, type="aov")
#' mreg.lse(form, data=mtcars, type="test")
#' # Example 14.9
#' data(exa14_9)
#' form2 <- Score ~ Study + Reading
#' mreg.lse(form2, data=exa14_9)
#' mreg.lse(form2, data=exa14_9, type="aov")
#' mreg.lse(form2, data=exa14_9, type="test")
#' @rdname mreg.lse
#' @export

mreg.lse <- function(form, data, type="lse", alp=0.05, dig=4, ws="n") {
  # Check input
    if (missing(form)) stop("A formula for the regression model is required.")
    if (missing(data)) {
        dum <- model.frame(form)
    } else {
        dum <- model.frame(form, data=data)
    }
  # Set inputs
    kk <- length(dum)
    ke <- kk-1
    y <- dum[1]
    xd <- dum[2:kk]
    xl <- names(xd)
    yl <- names(y)
    y <- dum[[1]]
    nn <- length(y)

  # Check Model
    dd2 <- attr(terms(form), "factors")[-1, ]
    kk2 <- ncol(dd2)
  # Extended Case
    if (kk2 > ke) {
        idx <- list()
        for (k in kk:kk2) idx[[k]] <- as.numeric(which(dd2[,k]==1))
        xd2 <- cbind(xd, matrix(1, nn, kk2-ke))
        for (k in kk:kk2) for (j in 1:length(idx[[k]])) {
            xd2[,k] <- xd2[,k]*xd[, idx[[k]][j]]
        }
        names(xd2) <- colnames(dd2)
        xd <- xd2
        xl <- names(xd)
        ke <- length(xd)
        kk <- ke+1
    }
  # Regression and ANOVA
    if (missing(data)) {
        lm1 <- lm(form)
    } else {
        lm1 <- lm(form, data=data)
    }
    an1 <- anova(lm1)

  # Prior investigation of data -----------------------------------------
    # LSE of multiple regression coefficients
    X <- as.matrix(xd)
    const <- rep(1, nn)
    X <- cbind(const, X)
    XX <- t(X) %*% X
    Xy <- t(X) %*% y
    colnames(Xy) <- yl
    XXI <- solve(XX)
    bh <- XXI %*% Xy

    fbh <- format(c(bh,0.123456789),F,dig)
    fbh <- fbh[-length(fbh)]

    if (("lse" %in% type) || ("all" %in% type)) {
        ##cat("[LSE Calculation] __________________________________________________\n")
        cat("[X'X matrix] _______________________\n")
        print(XX)
        cat("[LSE]   inv(X'X).(X'y) = b __________________________________\n")
        fXXI <- format(XXI,F,dig)
        fXy <- format(Xy,F,dig)

        cat("b0:  ", fXXI[1, ], "  ", fXy[1], "  ", fbh[1], "\n")
        plab <- slab <- blab <- clab <- rep("", ke)
        for (k in 1:ke) clab[k] <- paste0(as.numeric(which(dd2[,k]==1)), collapse="")
        for (k in 1:ke) blab[k] <- paste0("b", clab[k])
        for (k in 1:ke) slab[k] <- paste0(rep(" ", 3-nchar(clab[k])), collapse="")
        for (k in 1:ke) plab[k] <- paste0(blab[k], ":", slab[k])
        for (k in 1:ke) cat(plab[k], fXXI[k+1, ], "  ", fXy[k+1], "  ", fbh[k+1], "\n")
    }
    if (is.numeric(ws)) {
        ##cat("[P] Scatter Plot Matrix\n")
        win.graph(ws[1], ws[2])
        xyd <- as.data.frame(cbind(xd, y))
        names(xyd) <- c(xl, yl)
        pairs(xyd, lower.panel=panel.cor,
            upper.panel=function(x, y){
                points(x, y)
                abline(lm(y~x), col='red') })
    }
    if ("coef" %in% type) {
        cat("[LSE of Regression Coefficients] ______________________\n")
        names(fbh) <- colnames(X)
        print(as.data.frame(t(fbh)))
    } # End of pre

  # Test for the Significance of a Multiple Regression Model
    if (("aov" %in% type) || ("all" %in% type)) {
        # Analysis of variance
        SST <- sum(an1$Sum)
        SSR <- sum(an1$Sum[1:ke])
        SSE <- SST-SSR
        dfe <- nn-kk
        MSR <- SSR/ke
        MSE <- SSE/dfe
        F0 <- MSR/MSE
        pv0 <- 1-pf(F0, ke, dfe)
        crv0 <- qf(1-alp, ke, dfe)
        crv1 <- rep(qf(1-alp, 1, dfe), ke)
        Rsq <- SSR/SST
        aRsq <- 1-SSE/SST*(nn-1)/(nn-kk)
        tval <- qt(1-alp/2, dfe)
        bh <- lm1$coef

      # ANOVA table by calculating sum of squares
        cat("[Calculating Sum of Squares] ___________________________\n")
        cat(paste0("SST = ", round(sum(y^2), dig), " - (", round(sum(y), dig), ")\U00B2",
            "/ ", nn,    " = ", round(SST, dig)), "\n")
        cat(paste0("SSR = (", paste(round(bh, dig), collapse=" "), ").(",
            paste(round(Xy, dig), collapse=" "), ") = ", round(SSR, dig)), "\n")
        cat(paste0("SSE = ", round(SST, dig), " - ", round(SSR, dig), " = ",
            round(SSE, dig)), "\n")

        cat("[ANOVA Table] __________________________________________\n")
        antab <- cbind(an1$Sum[1:ke], an1$Df[1:ke], an1$Mean[1:ke], 
                       an1$F[1:ke], crv1, an1$Pr[1:ke])
        antab <- rbind(antab, c(SSR, ke, MSR, F0, crv0, pv0))
        antab <- rbind(antab, c(an1$Sum[kk], an1$Df[kk], an1$Mean[kk], NA, NA, NA))
        antab <- rbind(antab, c(sum(an1$Sum), sum(an1$Df), NA, NA, NA, NA))
        colnames(antab) <- c("Sum Sq.", "df", "Mean Sq.", "F0", "C-value", "P-value")
        rownames(antab) <- c(xl, "Regression", "Residual", "Total")

        dum <- as.data.frame(antab)
        dum <- format(dum, F, dig, nsmall=dig)
        dum[[2]] <- antab[,2]
        for (k in 3:6) dum[, k] <- sub("NA", "  ", dum[, k])
        print(dum)
        ##cat("--------------------------------------------------------\n")

        cat(paste0("R-square = ", round(SSR, dig), " / ", round(SST, dig), " = ",
            round(Rsq, dig)), "\n")
        cat(paste0("Adj R-sq = 1 - (", round(SSE, dig), "/", nn-kk, ") / (",
            round(SST, dig), "/", nn-1, ") = ", round(aRsq, dig)), "\n")
    } # End of aov

    # Test for the Significance of Multiple Regression Coefficients
    if (("test" %in% type) || ("all" %in% type)) {
      # LSE of multiple regression coefficients
        bh <- lm1$coef
        MSE <- an1$Mean[kk]
        dfe <- an1$Df[kk]
        tval <- qt(1-alp/2, dfe)
      # Confidence intervals & significance tests for regression coefficients
        dXXI <- diag(XXI)
        se <- sqrt(MSE * dXXI)
        tstat <- bh/se
        tol <- tval*se
        lcl <- bh - tol
        ucl <- bh + tol
        pv <- 2*pt(-abs(tstat), dfe)

      # Print output
        cat(paste0("[", 100*(1-alp), "% CI & Tests for Regression Coefficients]"), 
                   "_________________\n")
        citab <- cbind(bh, se, tol, lcl, ucl, tstat, pv)
        colnames(citab) <- c("Estimate", "Std Err", "Tolerance", "LCL", 
                             "UCL", "T-stat", "P-value")
        dum <- format(as.data.frame(rbind(citab,rep(0.123456789,7))), F, dig)
        dum <- dum[-nrow(dum), ]
        print(dum)
    } # End of test

  # Return the results
    if ("all" %in% type) {
        rout <- list(lse=bh, aov=antab, test=citab)
    } else {
        rout <- list(lse=bh)
        if ("aov" %in% type) rout$aov <- antab
        if ("test" %in% type) rout$test <- citab
    }
    invisible(rout)
}

# [14-6] Multiple Regression: Prediction and Confidence Intervals
#' @title Prediction and Confidence Interval: Multiple Regression
#' @description Prediction and Confidence Interval in a Multiple Regression Model.
#' @param form Formula of regression model (ex: y ~ x1 + x2)
#' @param newd A new data frame of independent variables.
#' @param plot Logical value for plotting CI and PI, Default: FALSE.
#' @param pvx Order of selected independent variables, Default: 1.
#' @param xrng Plot range of the independent variable.
#' @param nx Number of plotting points, Default: 50.
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return CI and PI
#'
#' @examples
#' # Data set 'mtcars'
#' form <- mpg ~ hp + drat + wt
#' newd <- data.frame(hp=300, drat=4, wt=4)
#' mreg.pred(form, data=mtcars, newd)
#' mreg.pred(form, data=mtcars, ws=c(7,6), pvx=1, xlim=c(30,400))
#' mreg.pred(form, data=mtcars, ws=c(7,6), pvx=2, xlim=c(2,6))
#' mreg.pred(form, data=mtcars, ws=c(7,6), pvx=3, xlim=c(1,6))
#' # Example 14.9
#' data(exa14_9)
#' form2 <- Score ~ Study + Reading
#' nd <- data.frame(Study=5, Reading=5)
#' mreg.pred(form2, data=exa14_9, newd=nd)
#' mreg.pred(form2, data=exa14_9, ws=c(7,6), xrng=c(0,15), pvx=1)
#' mreg.pred(form2, data=exa14_9, ws=c(7,6), xrng=c(0,15), pvx=2)
#'
#' @rdname mreg.pred
#' @export

##mreg.pred <- function(form, newd, plot=FALSE, pvx=1, xrng, nx=50, alp=0.05, dig=4) {
mreg.pred <- function(form, data, newd, alp=0.05, dig=4, ws="n", pvx=1, ...) 
{
    myf0 <- function(v) round(v, dig)
    myf1 <- function(v) format(round(v, dig), nsmall=dig)

  # Check input
    if (missing(form)) stop("A formula for the regression model is required.")
    if (missing(newd)&&!is.numeric(ws)) stop("New data for the prediction is required.")
    if (is.language(form)) {
        if (missing(data)) {
            lm1 <- lm(form)
        } else {
            lm1 <- lm(form, data=data)
        }
    } else if ("lm" %in% class(form)) {
        lm1 <- form
    }

  # Common Procedure
    dum <- model.frame(lm1)

  # Set inputs
    kk <- length(dum)
    ke <- kk-1
    y <- dum[1]
    xd <- dum[2:kk]
    xl <- names(xd)
    yl <- names(y)
    y <- dum[[1]]
    nn <- length(y)
    conf <- 100*(1-alp)

  # Confidence intervals and prediction intervals at x=newd
    if (!is.numeric(ws)) {
        dm <- attr(terms(lm1),"factors")[-1, ]
        rr <- nrow(newd)
        coef <- as.numeric(lm1$coef)
        cc <- length(coef)
        mse <- anova(lm1)$Mean[cc]
        cov <- summary(lm1)$cov
        dfe <- lm1$df
        tv <- qt(1-alp/2, dfe)
        otab1 <- otab2 <- NULL

      # Calculation
        for (jj in 1:rr) {
            xv0 <- as.numeric(newd[jj, ])
            xv1 <- as.numeric(apply(dm, 2, function(dc) prod(xv0[dc==1])))
            x0 <- c(1, xv1)
            yh <- (coef %*% x0)[1,1]
            dd <- (x0 %*% cov %*% x0)[1,1]
            se <- sqrt(mse*dd)
            tol <- tv*se
            lcl <- yh - tol
            ucl <- yh + tol
            se2 <- sqrt(mse*(1+dd))
            tol2 <- tv*se2
            lcl2 <- yh - tol2
            ucl2 <- yh + tol2
            otab1 <- rbind(otab1, c(xv0, yh, se, tol, lcl, ucl))
            otab2 <- rbind(otab2, c(xv0, yh, se2, tol2, lcl2, ucl2))
        }

        colnames(otab1) <- c(colnames(newd), "E(y|x0)", "se(CI)", "tol(CI)", "LCL", "UCL")
        colnames(otab2) <- c(colnames(newd), "(Y|x0)", "se(PI)", "tol(PI)", "LPL", "UPL")
        rownames(otab1) <- rownames(otab2) <- paste0("Case ", 1:rr, ":")
        cat("[Confidence & Prediction Intervals at x0] ___________________\n")
        cat("\tMSE =", myf1(mse), paste0("\tt_(", 1-alp/2, ",", dfe, 
            ") ="), myf1(tv),"\n")

        conf0 <- predict(lm1, newd, interval="confidence")
        pred0 <- predict(lm1, newd, interval="prediction")
        cat("[a]", paste0(conf, "% Confidence Intervals ________________________________\n"))
        # print(myf1(conf0))
        print(myf0(otab1))
        cat("[b]", paste0(conf, "% Prediction Intervals ________________________________\n"))
        # print(myf1(pred0))
        print(myf0(otab2))

  # Plot confidence bands and prediction bands
    } else {
        ##cat("[P] Plot Confidence & Prediction Bands w.r.t.", xl[pvx], "\n")
        xb <- as.numeric(apply(xd, 2, mean))
      # Set labels & plot range
        xmin <- min(xd[[pvx]])
        xmax <- max(xd[[pvx]])
        xspan <- xmax-xmin
        xrng <- c(xmin, xmax)+xspan*c(-0.1, 0.1)
        mt <- paste("Confidence and Prediction Bands of ", yl, "given", xl[pvx])
        pch <- 19
        pch1 <- "+"
        pch2 <- 1
        cex <- 1.2
        col <- c("purple", "red", "blue")
        text.col <- "red"
        text.cex <- 0.8
        lty <- 2
        lwd <- 1
        nx <- 100

      # Set the list of arguments
        dots <- list(...)
        if (length(dots) > 0) {
            pars <- names(dots)
            if ("main" %in% pars) mt <- dots$main
            if ("ylab" %in% pars) yl <- dots$ylab
            if ("xlab" %in% pars) xl <- dots$xlab
            if ("xlim" %in% pars) xrng <- dots$xlim
            if ("pch" %in% pars) pch <- dots$pch
            if ("pch1" %in% pars) pch1 <- dots$pch1
            if ("pch2" %in% pars) pch2 <- dots$pch2
            if ("cex" %in% pars) cex <- dots$cex
            if ("col" %in% pars) {
                if (length(dots$col)==3) {
                    col <- dots$col
                 } else col[1] <- dots$col[1]
            }
            if ("text.col" %in% pars) text.col <- dots$text.col
            if ("text.cex" %in% pars) text.cex <- dots$text.cex
            if ("lty" %in% pars) lty <- dots$lty
            if ("lwd" %in% pars) lwd <- dots$lwd
            if ("nx" %in% pars) nx <- dots$nx
        }
      # Adjust the range
        xrng2 <- range(xrng)+(max(xrng)-min(xrng))*c(-0.02, 0.02)
        avx <- setdiff(1:ke, pvx)
        ndat <- as.data.frame(matrix(NA, nx, ke))
        ndat[[pvx]] <- seq(xrng[1], xrng[2], length=nx)
        for (k in avx) ndat[[k]] <- rep(xb[k], nx)
        names(ndat) <- xl

      # Confidence intervals and prediction intervals
        conf1 <- predict(lm1, interval="confidence", newdata=ndat)
        pred1 <- predict(lm1, interval="prediction", newdata=ndat)

      # Plot confidence band and prediction band
        y1 <- min(pred1[, "lwr"])
        y2 <- max(pred1[, "upr"])
        ymin <- y1 - (y2-y1)*0.1
        ymax <- y2 + (y2-y1)*0.1

        win.graph(ws[1], ws[2])
        par(mar=c(4.5, 4, 3, 2))

        plot(xd[[pvx]], y, pch=pch, cex=cex, main=mt,
             xlab=xl[pvx], ylab=yl, xlim=xrng2, ylim=c(ymin, ymax))
        lines(ndat[[pvx]], conf1[,1], lty=lty, lwd=lwd, col=col[1])
        abline(v=c(xrng[1], xb[pvx], xrng[2]), lty=lty, col="green")
        text(xb[pvx], ymin, labels=expression(bar(x)), pos=4)
        text(xrng,  ymin, round(xrng, dig), cex=text.cex)
        matlines(ndat[[pvx]], conf1[,c("lwr","upr")], type="b", col=col[2], pch=pch1)
        matlines(ndat[[pvx]], pred1[,c("lwr","upr")], type="b", col=col[3], pch=pch2)
        text(xrng[1], conf1[1,], labels=format(conf1[1,], digit=4), 
               cex=text.cex, col=text.col, pos=c(1,1,3))
        text(xrng[2], conf1[nx,], labels=format(conf1[nx,], digit=4), 
               cex=text.cex, col=text.col, pos=c(1,1,3))
    }
    # Return the results
    if (is.numeric(ws)) {
        invisible(list(CI=conf1, PI=pred1))
    } else {
        invisible(list(CI=conf0, PI=pred0))
    }
}

# [14-7] Multiple Regression: Test on Two Models
#' @title Compare Two Multiple Regression Models
#' @description Compare Two Multiple Regression Models.
#' @param form1 Formula of a regression model (ex: y ~ x1 + x2)
#' @param form2 Formula for another regression model (eg: y ~ x1 * x2)
#' @param plot Logical value for drawing diagnosis plots, Default: TRUE.
#' @param wh Index vector for the diagnosis plots, Default: c(1:3,5).
#' @param alp Level of significance, Default: 0.05
#' @param dig Number of digits below the decimal point, Default: 4
#' @return ANOVA table.
#'
#' @examples
#' # Data set 'mtcars'
#' form.1 <- mpg ~ hp + drat + wt
#' form.2 <- mpg ~ hp * drat * wt
#' mreg.diff(form.1, form.2, data=mtcars)
#' # Example 14.9
#' data(exa14_9)
#' form2.1 <- Score ~ Study + Reading
#' form2.2 <- Score ~ Study * Reading
#' mreg.diff(form2.1, form2.2, data=exa14_9)
#' @rdname mreg.diff
#' @export

mreg.diff <- function(form1, form2, data, alp=0.05, dig=4, dp=c(1:3,5)) {
  # Check input
    if (missing(form1)) stop("A formula for the regression model 1 is required.")
    if (missing(form2)) stop("A formula for the regression model 2 is required.")
    if (is.language(form1)) {
        if (missing(data)) {
            lm1 <- lm(form1)
        } else {
            lm1 <- lm(form1, data=data)
        }
    } else if ("lm" %in% class(form1)) {
        lm1 <- form1
        form1 <- as.character(lm1$call)[2]
    }
    if (is.language(form2)) {
        if (missing(data)) {
            lm2 <- lm(form2)
        } else {
            lm2 <- lm(form2, data=data)
        }
    } else if ("lm" %in% class(form2)) {
        lm2 <- form2
        form2 <- as.character(lm2$call)[2]
    }

  # Compare and diagnose regression models
    cat("[Model 1]", deparse(form1), "\n")
    cat("[Model 2]", deparse(form2), "\n")
    cat("[ANOVA for Comparing Models] ___________________________\n")
    an1 <- anova(lm1)
    an2 <- anova(lm2)
    sse1 <- an1$Sum[length(an1$Sum)]
    sse2 <- an2$Sum[length(an2$Sum)]
    dfe1 <- an1$Df[length(an1$Df)]
    dfe2 <- an2$Df[length(an2$Df)]

    dsse <- abs(sse1 - sse2)
    ddfe <- abs(dfe1 - dfe2)

    if (dfe1 < dfe2) {
        morder <- c("[Model 2]", "[Model 1]")
        row1 <- c(sse2, dfe2, rep(NA, 4))
        row2 <- c(sse1, dfe1, sse1/dfe1, rep(NA, 3))
    } else {
        morder <- c("[Model 1]", "[Model 2]")
        row1 <- c(sse1, dfe1, rep(NA, 4))
        row2 <- c(sse2, dfe2, sse2/dfe2, rep(NA, 3))
    }
    dmse <- dsse/ddfe
    dF0 <- dmse/row2[3]
    cv <- qf(1-alp, ddfe, row2[2])
    pv <- 1-pf(dF0, ddfe, row2[2])
    row3 <- c(dsse, ddfe, dmse, dF0, cv, pv)

    antab <- rbind(row1, row2, row3)
    colnames(antab) <- c("SS_Res", "df", "MS_Res", "F0", "C-value", "P-value")
    rownames(antab) <- c(morder, "Difference")

    dum <- as.data.frame(antab)
    dum[, c(1,3:6)] <- format(round(dum[, c(1,3:6)], dig), nsmall=dig)
    for (k in 3:6) dum[, k] <- sub("NA", "  ", dum[, k])
    print(dum)
    ##cat("----------------------------------------------------------------------\n")
    if (is.numeric(dp)) {
        ##cat("[P] Diagnosis Plots\n")
        ndp <- length(dp)
        win.graph(2.5*ndp, 5)
        par(mfcol=c(2,ndp))
        for (kk in 1:ndp) {
            plot(lm1, which = dp[kk])
            title(main="Model 1", col.main="blue")
            ##if (ndp > 1) plot(lm1, which = dp[2:ndp])
            plot(lm2, which = dp[kk])
            title(main="Model 2", col.main="red")
            ##if (ndp > 1) plot(lm2, which = dp[2:ndp])
        }
    }
    invisible(antab)
}

