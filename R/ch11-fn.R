# [Ch-11 Functions] ----------------------------------------------------------------------------------
# [Ch-11 Function Manual] -----------------------------------------

#' @title Manual for Ch11. Functions
#' @description Ch11. Inference on Two Populations
#' @param fn Function number (0~8), Default: 0
#' @return None.
#'
#' @examples
#' ch11.man()
#' ch11.man(1)
#' @rdname ch11.man
#' @export
ch11.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("[1] mean2.test\tInference on the Difference of Two Means\n")
        cat("[2] prop2.test\tApproximate Inference on the Difference of Two Proportions\n")
        cat("[3] var2.test\tInference on the Ratio of Two Variances\n")
        cat("[4] civar2.sim\tSimulate the CI for the Ratio of Two Variances\n")
    }
    if (1 %in% fn) {
        cat("[1] Inference on the Difference of Two Means\n")
        cat("mean2.test(x, y, alt, alp=0.05, sig, n, d0=0, var, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t First sample data or sample mean.\n")
        cat("y\t Second sample data or sample mean.\n")
        cat("[Optional Input]--------------------------\n")
        cat("alt\t Type of the alternative hypothesis, Default: \"two\")\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("sig\t Standard deviations of two groups (without sample data).\n")
        cat("n\t Sample size of two groups (without sample data).\n")
        cat("d0\t Difference of two population means under H0, Default: 0.\n")
        cat("var\t Status of variance, one of (Default: \"eq\", \"uneq\", \"known\")\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (2 %in% fn) {
        cat("[2] Approximate Inference on the Difference of Two Proportions\n")
        cat("prop2.test(x, y, n1, n2, alt=\"two\", alp=0.05, d0=0, dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t Number of successes in the first sample data.\n")
        cat("y\t Number of successes in the second sample data.\n")
        cat("n1\t First sample size.\n")
        cat("n2\t Second sample size.\n")
        cat("[Optional Input]--------------------------\n")
        cat("alt\t Type of the alternative hypothesis, Default: \"two\".\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("d0\t Difference of two proportions under H0, Default: 0.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (3 %in% fn) {
        cat("[3] Inference on the Ratio of Two Variances\n")
        cat("var2.test(x, y, n1, n2, alp=0.05, alt=\"two\", dig=4, ws=c(7,4), ...)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("x\t First sample data or sample variance.\n")
        cat("y\t Second sample data or sample variance.\n")
        cat("n1\t First sample size (without sample data).\n")
        cat("n2\t Second sample size (without sample data).\n")
        cat("[Optional Input]--------------------------\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("alt\t Type of the alternative hypothesis, Default: \"two\".\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
        cat("...\t Other graphic parameters.\n")
    }
    if (4 %in% fn) {
        cat("[4] Simulate the CI for the Ratio of Two Variances\n")
        cat("civar2.sim(var1, var2, n1, n2, alp=0.05, N=100, seed=9857, dig=4, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("n1\t Sample size of the first population.\n")
        cat("n2\t Sample size of the second population.\n")
        cat("[Optional Input]--------------------------\n")
        cat("var1\t Variance of the first population, Default: 1.\n")
        cat("var2\t Variance of the second population, Default: 1.\n")
        cat("alp\t Level of significance, Default: 0.05.\n")
        cat("N\t Number of iterations, Default: 100.\n")
        cat("seed\t Seed value for generating random numbers, Default: 9857.\n")
        cat("dig\t Number of decimal places, Default: 4.\n")
        cat("ws\t Graphic window size, Default: c(7,4).\n")
    }
}

# Find the relevant alternative (1="less", 2="greater", 3="two sided")
alt.num <- function(alt) {
    nalt <- grep(paste0("^", alt), c("less", "greater", "two.sided"))
    if (length(nalt)>=2) nalt <- max(nalt)
    if (length(nalt)==0) nalt <- grep(paste0("^", alt), c("lower", "upper", "two-sided"))
    invisible(nalt)
}

# [11-1] Inference on the Difference of Two Means
#' @title Inference on the Difference of Two Population Means
#' @description Hypothesis Test and CI for the Difference of Two Population Means
#' @param x First sample data or sample mean.
#' @param y Second sample data or sample mean.
#' @param alt Type of the alternative hypothesis, Default: 'two'.
#' @param alp Level of significance, Default: 0.05.
#' @param sig Standard deviations of two groups (without sample data).
#' @param n Sample size of two groups (without sample data).
#' @param d0 Difference of two population means under H0, Default: 0.
#' @param var Status of variance (one of "eq", "uneq", "known"), Default: 'eq'.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Summary statistics.
#'
#' @examples
#' mean2.test(x=198.5, y=201.3, sig=5, n=c(25,34), var="known")
#' mean2.test(x=198.5, y=201.3, sig=5, n=30, var="known")
#' mean2.test(x=198.5, y=201.3, sig=c(4.8,5.1), n=c(25,34), var="eq")
#' mean2.test(x=198.5, y=201.3, sig=c(2.8,5.5), n=c(25,34), var="uneq")
#'
#' set.seed(3691)
#' x <- rnorm(20, 199, 2)
#' y <- rnorm(25, 200, 4)
#' mean2.test(x, y, var="eq")
#' mean2.test(x, y, var="uneq")
#' @rdname mean2.test
#' @export
mean2.test <- function(x, y, alt="two", alp=0.05, sig, n, d0=0, var="eq", dig=4, ws=c(7,4), ...)
{
  # Check Input
    if (missing(x)) stop("Input the first sample data or sample mean.")
    if (missing(y)) stop("Input the second sample data or sample mean.")

    myf0 <- function(x) round(x, dig)
    myf1 <- function(x) format(round(x, dig), nsmall= dig)

  # Case of sample data input
    if (length(x) >1) {
        n1 <- length(x)
        n2 <- length(y)
        s1 <- sd(x)
        s2 <- sd(y)
        xb1 <- mean(x)
        xb2 <- mean(y)
        if (!grepl(var, "paired")) {
          cat(paste0("Sx\U00B2=(", myf0(sum(x^2)), "-", myf0(sum(x)), "\U00B2/",
             n1, ")/", n1-1, "=", myf0(var(x)), "; Sy\U00B2=(", myf0(sum(y^2)),
             "-", myf0(sum(y)), "\U00B2/", n2, ")/", n2-1, "=", myf0(var(y))), "\n")
        }
    } else {
        xb1 <- x
        xb2 <- y
        if (length(n)==1) n <- rep(n, 2)
        n1 <- n[1]
        n2 <- n[2]
        if (length(sig)==1) sig <- rep(sig, 2)
        s1 <- sig[1]
        s2 <- sig[2]
    }

    xstat <- c(n1, xb1, s1)
    ystat <- c(n2, xb2, s2)

  # Common ------------
    xd <- xb1 - xb2
    tag10 <- paste0("(", myf0(xb1), "-", myf0(xb2), ")")
    tag10a <- paste0("|", myf0(xb1), "-", myf0(xb2), "|")
    if (missing(d0)) {
        tag1 <- tag10
        tag1a <- tag10a
    } else {
        tag1 <- paste0("(", myf0(xb1), "-", myf0(xb2), "-", d0, ")")
        tag1a <- paste0("|", myf0(xb1), "-", myf0(xb2), "-", d0, "|")
    }
    tag2 <- paste0("\U221A(", myf0(s1^2), "/", n1, 
                       "+", myf0(s2^2), "/", n2, ")")

    nalt <- alt.num(alt)

  # [1] Case of known variance -----------------
    if (grepl(var, "known")) {
        cv1 <- qnorm(alp)
        cv2 <- qnorm(1-alp)
        cv3 <- qnorm(1-alp/2)
        cv <- switch(nalt, cv1, cv2, cv3)
        se <- sqrt(s1^2/n1+s2^2/n2)
        tag0 <- "Z0="
        tag0a <- "|Z0|="

        stat <- (xd - d0)/se
        st2 <- abs(stat)
        st1 <- -st2
        pv1 <- pnorm(stat)
        pv2 <- 1 - pnorm(stat)
        pv3 <- 2*pnorm(st1)
        ptag1 <- paste0("Pv=P(Z<", myf1(stat), ")=")
        ptag2 <- paste0("Pv=P(Z>", myf1(stat), ")=")
        ptag3 <- paste0("Pv=2\U00D7P(Z>|", myf1(stat), "|)=")
        cat(paste0("Se=", tag2, "=", myf1(se), "; Cv=", myf1(cv)), "\n")

  # [2~3] Case of unknown variance -----------------
    } else {
        tag0 <- "T0="
        tag0a <- "|T0|="

      # [2] Case of unknown, but equal variances
        if (grepl(var, "equal")) {
            df <- n1+n2-2
            cv1 <- qt(alp, df)
            cv2 <- qt(1-alp, df)
            cv3 <- qt(1-alp/2, df)
            cv <- switch(nalt, cv1, cv2, cv3)

            sp2 <- ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
            sp <- sqrt(sp2)
            se <- sqrt(sp2*(1/n1+1/n2))
            tag2 <- paste0("\U221A(", myf0(sp2), "(1/", n1, 
                       "+1/", n2, "))")
            cat(paste0("Sp\U00B2=", myf0(sp2), " df=", df, 
                    "; Se=", tag2, "=", myf1(se), "; Cv=", myf1(cv)), "\n")
      # [3] Case of unknown and unequal variances
        } else if (grepl(var, "unequal")) {
            df <- (s1^2/n1+s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
            cv1 <- qt(alp, df)
            cv2 <- qt(1-alp, df)
            cv3 <- qt(1-alp/2, df)
            cv <- switch(nalt, cv1, cv2, cv3)

            se <- sqrt(s1^2/n1 + s2^2/n2)
            cat(paste0("nu*=", myf1(df), "; Se=", tag2, "=", myf1(se),
                       "; Cv=", myf1(cv)), "\n")
      # [4] Case of paired samples
        } else if (grepl(var, "paired")) {
            if(n1==1 || n1 != n2) stop("Input paired samples.")
            dx <- x - y
            df <- n1-1
            cv1 <- qt(alp, df)
            cv2 <- qt(1-alp, df)
            cv3 <- qt(1-alp/2, df)
            cv <- switch(nalt, cv1, cv2, cv3)
            dvar <- var(dx)
            se <- sqrt(dvar/n1)
            tag2 <- paste0("\U221A(", myf0(dvar), "/", n1, ")")
            cat(paste0("df=", df, "; Se=", tag2, "=", myf1(se),
                       "; Cv=", myf1(cv)), "\n")
        }

        stat <- (xd - d0)/se
        st2 <- abs(stat)
        st1 <- -st2
        pv1 <- pt(stat, df)
        pv2 <- 1 - pt(stat, df)
        pv3 <- 2*pt(st1, df)
        ptag1 <- paste0("Pv=P(T<", myf1(stat), ")=")
        ptag2 <- paste0("Pv=P(T>", myf1(stat), ")=")
        ptag3 <- paste0("Pv=2\U00D7P(T>|", myf1(stat), "|)=")
    }

  # Alternatives --------------
    if (nalt==2) {
      # CI -----------
        err <- cv2*se
        lcl <- xd - err
        ucl <- Inf
        cat(paste0("[", tag10, "-", myf1(cv2),
            "\U00D7", myf1(se), ", Inf]=[", myf0(xd), "-", myf1(err), 
            ", Inf]=[", myf1(lcl), ", Inf]"), "\n")
      # Test -----------
        sign <- ifelse(stat > cv2, ">", "<")
        cat(paste0(tag0, tag1, "/", myf1(se), "=", 
                myf1(stat), sign, myf1(cv2), "; "))
        cat(paste0(ptag2, myf1(pv2)), "\n")
        pv <- pv2
        cv <- cv2
    } else if (nalt==1) {
      # CI -----------
        err <- cv2*se
        lcl <- -Inf
        ucl <- xd + err
        cat(paste0("[-Inf, ", tag10, "+", myf1(cv2),
            "\U00D7", myf1(se), "]=[-Inf, ", myf0(xd), "+", myf1(err), 
            "]=[-Inf, ", myf1(ucl), "]"), "\n")
      # Test -----------
        sign <- ifelse(stat > cv1, ">", "<")
        cat(paste0(tag0, tag1, "/", myf1(se), "=", 
            myf1(stat), sign, myf1(cv1), "; "))
        cat(paste0(ptag1, myf1(pv1)), "\n")
        pv <- pv1
        cv <- cv1
    } else if (nalt==3) {
      # CI -----------
        err <- cv3*se
        lcl <- xd - err
        ucl <- xd + err
        cat(paste0("[", tag10, "\U00B1", myf1(cv3),
            "\U00D7", myf1(se), "]=[", myf0(xd), "\U00B1", myf1(err), 
            "]=[", myf1(lcl), ", ", myf1(ucl),"]"), "\n")
      # Test -----------
        sign <- ifelse(st2 > cv3, ">", "<")
        cat(paste0(tag0a, tag1a, "/", myf1(se), "=", 
            myf1(st2), sign, myf1(cv3), "; "))
        cat(paste0(ptag3, myf1(pv3)), "\n")
        pv <- pv3
        cv <- cv3
    }

  # Plot the PDF
    if (is.numeric(ws)) {
        win.graph(ws[1], ws[2])
        par(mar=c(4,4,3,2))

      # Legend
        H1 <- switch(nalt, paste0("m1-m2<", d0), paste0("m1-m2>", d0),
                    paste0("m1-m2!=", d0))
        names <- c("Diff ", "Serr", "Stat", "Pval")
        sstat <- c(xd, se, stat, pv)
        leg <- c(paste("H1 :", H1), paste(names, myf1(sstat), sep=" = "))

      # Plot
        if (grepl(var, "known")) df <- Inf
        unitest.plot(stat, pv, cv, nalt, dig, leg, df, ...)
    }

  # Prepare output
    if (grepl(var, "known")) {
        out <- list(stat=stat, se=se, pval=pv, CI=c(lcl, ucl))
    } else {
        out <- list(stat=stat, se=se, pval=pv, df=df, CI=c(lcl, ucl))
    }
    invisible(out)
}

# [11-2] Approximate Inference on the Difference of Two Proportions
#' @title Approximate Test for the Difference of Two Proportions (Large Sample)
#' @description Approximate Test for the Difference of Two Population Proportions (Large Sample).
#' @param x Number of successes in the first sample data.
#' @param y Number of successes in the second sample data.
#' @param n1 First sample size.
#' @param n2 Second sample size.
#' @param alt Type of the alternative hypothesis, Default: "two".
#' @param alp Level of significance, Default: 0.05.
#' @param d0 Difference of two proportions under H0, Default: 0.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return Summary statistics.
#'
#' @examples
#' prop2.test(x=12, y=10, n1=150, n2=250)
#' prop2.test(x=12, y=10, n1=150, n2=250, alt="gr")
#' @rdname prop2.test
#' @export
prop2.test <- function(x, y, n1, n2, alt="two", alp=0.05, d0=0, dig=4, ws=c(7,4), ...) {
  # Check Input
    if (missing(x)) stop("Input the first sample data or sample mean.")
    if (missing(y)) stop("Input the second sample data or sample mean.")

    myf0 <- function(x) round(x, dig)
    myf1 <- function(x) format(round(x, dig), nsmall= dig)
    nalt <- alt.num(alt)

  # Statistic for CI
    p1 <- x/n1
    p2 <- y/n2
    pd <- p1-p2
    pse <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
    cv1 <- qnorm(1-alp)
    cv2 <- qnorm(1-alp/2)
    err1 <- cv1*pse
    err2 <- cv2*pse

  # Print the CI ------------
    nalt <- alt.num(alt)
    head <- paste0("(", myf0(p1), "-", myf0(p2), ")")
    fse <- paste0("\U221A(", myf0(p1), "\U00D7", myf0(1-p1), "/", n1,
        " + ", myf0(p2), "\U00D7", myf0(1-p2), "/", n2, ")")

    if (nalt==2) {
        lcl <- pd - err1
        cat(paste0("[", head," - ", myf1(cv1), "\U00D7", fse, 
            ", Inf]\n = [", myf0(pd), " - ", myf1(cv1), "\U00D7", myf1(pse),
            ", Inf] = [", myf0(pd), " - ", myf1(err1), 
            ", Inf] = [", myf1(lcl), ", Inf]"), "\n")
    } else if (nalt==1) {
        ucl <- pd + err1
        cat(paste0("[-Inf, ", head," + ", myf1(cv1), "\U00D7", fse, 
            "]\n = [-Inf, ", myf0(pd), " + ", myf1(cv1), "\U00D7", myf1(pse),
            "] = [-Inf, ", myf0(pd), " + ", myf1(err1), 
            "] = [-Inf, ", myf1(ucl), "]"), "\n")
    } else if (nalt==3) {
        lcl <- pd - err2
        ucl <- pd + err2
        cat(paste0("[", head, " \U00B1 ", myf1(cv2), " \U00D7 ", fse, 
           "]\n = [", myf0(pd), " \U00B1 ", myf1(cv2), "\U00D7", myf1(pse),
           "] = [", myf0(pd), " \U00B1 ", myf1(err2), 
           "] = [", myf1(lcl), ", ", myf1(ucl),"]"), "\n")
    }

  # Testing Hypothesis
    ph <- (x+y)/(n1+n2)
    se <- sqrt(ph*(1-ph)*(1/n1 + 1/n2))
    z0 <- pd/se

    cv <- switch(nalt, -cv1, cv1, cv2)
    sign <- switch(nalt, ifelse(z0 > -cv1, " > ", " < "),
                     ifelse(z0 < cv1, " < ", " > "),
                     ifelse(abs(z0) < cv2, " < ", " > "))

    # cat(paste0("p1=", myf0(p1), " p2=", myf0(p2), " ph=", myf0(ph), "; "))
    cat(paste0("ph=", myf0(ph), " =>  Se=\U221A(", myf0(ph), "\U00D7", 
        myf0(1-ph), "\U00D7(1/", n1, "+1/", n2, "))=", myf0(se)), "\n")
    if (nalt == 3) {
        cat(paste0("|Z0| = |", myf0(p1), "-", myf0(p2), "|/",
            myf0(se), " = ", myf1(abs(z0)), sign, myf1(cv), "; "))
    } else {
        cat(paste0("Z0 = (", myf0(p1), "-", myf0(p2), ")/",
            myf0(se), " = ", myf1(z0), sign, myf1(cv), "; "))
    }

  # Print the p-value
    if (nalt==2) {
        pv <- pnorm(z0, lower.tail=FALSE)
        cat("P-v = P(Z>Z0) =", myf1(pv), "\n")
    } else if (nalt==1) {
        pv <- pnorm(z0)
        cat("P-v = P(Z<Z0) =", myf1(pv), "\n")
    } else if (nalt==3) {
        pv <- 2*pnorm(-abs(z0))
        cat("P-v = 2P(Z>|Z0|) =", myf1(pv), "\n")
    }

  # Plot the PDF
    if (is.numeric(ws)) {
      # Legend
        H1 <- switch(nalt, paste0("p1-p2<", d0), paste0("p1-p2>", d0),
                    paste0("p1-p2!=", d0))
        names <- c("Diff ", "Serr", "Stat", "Pval")
        sstat <- c(pd, se, z0, pv)
        leg <- c(paste("H1:", H1), paste(names, myf1(sstat), sep=" = "))

        win.graph(ws[1], ws[2])
        par(mar=c(4.5,4,3,2))
        #normtest.plot(z0, prng, alt, mt, prt=FALSE)
        unitest.plot(z0, pv, cv, nalt, dig, leg, df=Inf, ...)
    }
    names(sstat) <- names
    invisible(sstat)
}

# [11-A1] Plot the PDF of the F-test Statistic
#' @title Plot the PDF of the F-test Statistic
#' @description Plot the PDF of the F-test Statistic
#' @param fstat F-test statistic for the ratio of two population variances
#' @param deg Vector of degree of freedoms
#' @param alt Type of the alternative hypothesis, Default: 'two'
#' @param pmax Coverage probability of x-axis, Default: 0.995
#' @param prt Logical value for printing the p-value, Default: FALSE
#' @return None.
#'
#' @examples
#' set.seed(1234)
#' x <- rnorm(20, 10, 2.4); y <- rnorm(25, 12, 1.8)
#' (vo <- var.test(x, y))
#' win.graph(7, 5)
#' ftest.plot(stat=vo$stat, deg=as.vector(vo$para), alt="two", pmax=0.999)
#' @rdname ftest.plot
#' @export
ftest.plot <- function(F0, df, nalt, dig=4, leg, ...) {
    frr <- function(v) format(round(v, dig), nsmall=dig)
    pdf <- function(x) df(x, df[1], df[2])

  # Plot the PDF
    prng <- c(0, qf(0.999, df[1], df[2]))

  # Set titles ans axes
    mt <- paste0("Distribution of F0 under H0 ~ F(", df[1],", ", df[2], ")")
    xl <- "F-statistic (F0)"
    yl <- "f(x)"
    col <- text.col <- "blue"
    lwd <- 2
    fill <- "lightcyan"
    pv.col <- "red"
    cex <- 1
    pos <- "topright"
    fill <- "lightcyan"

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("main" %in% pars) mt <- dots$main
        if ("col" %in% pars) col <- dots$col
            if ("pos" %in% pars) pos <- dots$pos
            if ("text.col" %in% pars) text.col <- dots$text.col
            if ("pv.col" %in% pars) pv.col <- dots$pv.col
            if ("ylab" %in% pars) yl <- dots$ylab
            if ("xlab" %in% pars) xl <- dots$xlab
            if ("xlim" %in% pars) prng <- dots$xlim
            if ("lwd" %in% pars) lwd <- dots$lwd
            if ("cex" %in% pars) cex <- dots$cex
            if ("fill" %in% pars) fill <- dots$fill
        }
  # Plot
    xa <- seq(prng[1], prng[2], length.out=101)

    plot(xa, pdf(xa), type="n", xlab=xl, ylab=yl,
         ylim=c(-0.1, 1)*max(pdf(xa)), main=mt)
  # P-value and the critical region
    plow <- pf(F0, df[1], df[2])
    base <- max(pdf(xa))/20
    hh <- 0.9*pdf(F0)
    if (hh < base) hh <- base

    if (nalt==2) {
        pv <- 1-plow
        cord.x <- c(F0, seq(F0, prng[2], length.out=20), prng[2])
        cord.y <- c(0, pdf(seq(F0, prng[2], length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(F0, 0, F0, pdf(F0), lwd=lwd, col=col)
        text(F0, hh, frr(pv), pos=4, col=pv.col)
        text(F0, 0, frr(F0), pos=1, col=text.col)
    } else if (nalt==1) {
        pv <- plow
        cord.x <- c(prng[1], seq(prng[1], F0, length.out=20), F0)
        cord.y <- c(0, pdf(seq(prng[1], F0, length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(F0, 0, F0, pdf(F0), lwd=lwd, col=col)
        text(F0, hh, frr(pv), pos=2, col=pv.col)
        text(F0, 0, frr(F0), pos=1, col=text.col)
    } else {
        pv <- 2*min(plow, 1-plow)
        mlow <- qf(pv/2, df[1], df[2])
        mup <- qf(1-pv/2, df[1], df[2])
        hh <- 0.9*pdf(mlow)
        if (hh < base) hh <- base
        cord.x <- c(mup, seq(mup, prng[2], length.out=20), prng[2])
        cord.y <- c(0, pdf(seq(mup, prng[2], length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        cord.x <- c(prng[1], seq(prng[1], mlow, length.out=20), mlow)
        cord.y <- c(0, pdf(seq(prng[1], mlow, length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(c(mlow, mup), 0, c(mlow, mup), pdf(c(mlow)), lwd=lwd, col=col)
        text(c(mlow, mup), hh, frr(pv/2), pos=c(2,4), col=pv.col)
        text(c(mlow, mup), 0, frr(c(mlow, mup)), pos=1, col=text.col)
    }
    abline(h=0, lty=3, col=grey(0.5))
    med <- qf(0.5, df[1], df[2])
    segments(med, 0, med, max(pdf(xa))*1.2, lty=3, col=grey(0.5))
    text(med, 0, "median", pos=1, col=text.col)
    lines(xa, pdf(xa), type="l", lwd=lwd, col=col)

  # Legend
    if (!missing(leg)) {
        nb <- length(leg) - 3
        legend(pos, leg, cex=cex*0.9, xjust=0.5, x.intersp=0,
           text.col=c("black", rep("blue", nb), "red", "red"), bg="white")
    }
}


# [11-3] Inference on the Ratio of Two Variances
#' @title Inference on the Ratio of Two Population Variances
#' @description Inference on the Ratio of Two Independent Normal Population Variances.
#' @param x First sample data or sample variance.
#' @param y Second sample data or sample variance.
#' @param n1 First sample size (without sample data).
#' @param n2 Second sample size (without sample data).
#' @param alp Level of significance, Default: 0.05
#' @param alt Type of the alternative hypothesis, Default: 'two'
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @param ... Other graphic parameters.
#' @return list(stat=F0, df=c(df1,df2), cval=cv, pval=pv)
#'
#' @examples
#' var2.test(x=3.5^2, y=5.8^2, n1=25, n2=34)
#'
#' set.seed(1234)
#' x <- rnorm(20, 199, 4)
#' y <- rnorm(25, 200, 3)
#' var2.test(x, y)
#' var2.test(x, y, alt="gr")
#' var2.test(var(x), var(y), length(x), length(y), alt="gr")
#' @rdname var2.test
#' @export
var2.test <- function(x, y, n1, n2, alp=0.05, alt="two", dig=4, ws=c(7,4), ...) {
  # Check Input
    if (missing(x)) stop("Input the first sample data or sample variance.")
    if (missing(y)) stop("Input the second sample data or sample variance.")

    myf0 <- function(x) round(x, dig)
    myf1 <- function(x) format(round(x, dig), nsmall= dig)

    if (missing(x) | missing(y)) stop("Input two standard deviations or data vectors...")
    if (length(x)==1 & missing(n1)) stop("Input n1, the first sample size...")
    if (length(y)==1 & missing(n2)) stop("Input n2, the second sample size...")
    if (length(x)>1) {
        n1 <- length(x)
        s1 <- sd(x)
    } else {
        s1 <- sqrt(x)
    }
    if (length(y)>1) {
        n2 <- length(y)
        s2 <- sd(y)
    } else {
        s2 <- sqrt(y)
    }

  # Test stat & critical values
    F0 <- s1^2/s2^2
    df1 <- n1-1
    df2 <- n2-1
    
    cat("F0 = s1\U00B2/s2\U00B2 =", myf0(s1^2), "/", myf0(s2^2), "=", myf1(F0), "\n")
    nalt <- alt.num(alt)

  # Greater
    if (nalt==2) {
        cv <- qf(1-alp, df1, df2)
      # CI -----------
        lcl <- F0/cv
        ucl <- Inf
        cat("[LCL, UCL] =", paste0("[", myf1(F0), "/", myf1(cv), ", Inf]"), 
            "=", paste0("[", myf1(lcl), ", Inf]"), "\n")

        pv <- 1- pf(F0, df1, df2)
        simb <- ifelse(F0 < cv, " < ", " > ")
        res <- ifelse(F0 >= cv, "Reject H0", "Accept H0")
        cat(paste0("F0 = ", myf1(F0), simb, myf1(cv), " \U21D2 ", res,
            ";  P-v = ", myf1(pv), "\n"))
    } else if (nalt==1) {
        cv <- qf(alp, df1, df2)
      # CI -----------
        lcl <- 0
        ucl <- F0/cv
        cat("[LCL, UCL] =", paste0("[0, ", myf1(F0), "/", myf1(cv), "]"), 
            "=", paste0("[0, ", myf1(ucl), "]"), "\n")

        pv <- pf(F0, df1, df2)
        simb <- ifelse(F0 < cv, " < ", " > ")
        res <- ifelse(F0 < cv, "Reject H0", "Accept H0")
        cat(paste0("F0 = ", myf1(F0), simb, myf1(cv), " \U21D2 ", res,
            ";  P-v = ", myf1(pv), "\n"))
    } else {
        cv <- qf(c(alp/2, 1-alp/2), df1, df2)
      # CI -----------
        lcl <- F0/cv[2]
        ucl <- F0/cv[1]
        cat("[LCL, UCL] =", paste0("[", myf1(F0), "/", myf1(cv[2]), 
            ", ", myf1(F0), "/", myf1(cv[1]), "]"), "=", 
            paste0("[", myf1(lcl), ", ", myf1(ucl), "]"), "\n")

        pv <- 2*min(c(pf(F0, df1, df2), 1-pf(F0, df1, df2)))
        simb <- ifelse(F0<cv[1] | F0>cv[2], " !\U2208 ", " \U2208 ")
        res <- ifelse(F0<cv[1] | F0>cv[2], "Reject H0", "Accept H0")
        cat(paste0("F0 = ", myf1(F0), simb,
            "[", myf1(cv[1]), ", ", myf1(cv[2]), "] \U21D2 ", res,
            ";  P-v = ", myf1(pv), "\n"))
    }

    if (is.numeric(ws)) {
        prng <- 0.995
        win.graph(ws[1], ws[2])
        par(mar=c(4.5,4,3,2))

      # Legend
        H1 <- switch(nalt, expression(H[1]~":"~~ sigma[1]^2 < sigma[2]^2), 
                           expression(H[1]~":"~~ sigma[1]^2 > sigma[2]^2),
                           expression(H[1]~":"~~ sigma[1]^2 != sigma[2]^2))
        names <- c("S1^2", "S2^2", "Stat", "Pval")
        fst <- myf1(c(s1^2, s2^2, F0, pv))
        bqs <- c(bquote(S[1]^2 == .(fst[1])), bquote(S[2]^2 == .(fst[2])),
               bquote(Stat == .(fst[3])), bquote(Pval == .(fst[4])) )
        #leg <- c(H1, paste(names, myf1(sstat), sep=" = "))
        leg <- c(H1, bqs)

      # Plot
        ftest.plot(F0, df=c(df1,df2), nalt, dig, leg, ...)
    }
    invisible(list(stat=F0, df=c(df1,df2), conf=c(lcl, ucl), cval=cv, pval=pv))
}
# [11-4] Simulate the Confidence Interval for a the Ratio of Population Variances
#' @title Simulate the Confidence Interval for a the Ratio of Variances
#' @description Simulate the Confidence Interval for a the Ratio of Population Variances
#' @param var1 Variance of the first population.
#' @param var2 Variance of the second population.
#' @param n1 Sample size of the first population.
#' @param n2 Sample size of the second population.
#' @param alp Level of significance, Default: 0.05.
#' @param N Number of iterations, Default: 100.
#' @param seed Seed value for generating random numbers, Default: 9857.
#' @param dig Number of decimal places, Default: 4.
#' @param ws Graphic window size, Default: c(7,4).
#' @return None.
#'
#' @examples
#' civar2.sim(n1=25, n2=16)
#' civar2.sim(var1=8, var2=2, n1=25, n2=16)
#' civar2.sim(var1=8, var2=2, n1=25, n2=16, N=10000, ws="n")
#' @rdname civar2.sim
#' @export
civar2.sim <- function(var1, var2, n1, n2, alp=0.05, N=100, seed=9857, dig=4, ws=c(7,4)) {
  # Ratio of population variances
  # [Correction]
    if (missing(var1)) {
        var1 <- 1
        cat("Since var1 is missing, it is automatically set to 1.\n")
    }
    if (missing(var2)) {
        var2 <- 1
        cat("Since var2 is missing, it is automatically set to 1.\n")
    }
    vr0 <- var1/var2
    sig1 <- sqrt(var1)
    sig2 <- sqrt(var2)
  # Create the matrix of confidence intervals
    ci <- matrix(0, nrow=N, ncol=3)
    ir <- 1:N
  # Tail values of the F-distribution
    fv1 <- qf(alp/2, n1-1, n2-1)
    fv2 <- qf(1-alp/2, n1-1, n2-1)
  # Set seed for reproducibility
    set.seed(seed)
    # Generate random numbers and calculate the confidence intervals
    for (i in ir) {
        x <- rnorm(n1, 0, sig1)
        y <- rnorm(n2, 0, sig2)
        xv <- var(x)
        yv <- var(y)
        xm <- xv/yv
        lcl <- xm/fv2
        ucl <- xm/fv1
        ci[i, ] <- c(lcl, xm, ucl)
    }
    if (is.numeric(ws)) {
        # Plot the PDF
        win.graph(ws[1], ws[2])
        par(mar=c(4.5,4,3,2))
        plot(ir, ci[ ,2], type="p", pch=19, cex=0.6, col=1, ylim=c(min(ci), max(ci)),
            main="Confidence Intervals for the Ratio of Two Variances",
            ylab="Confidence Interval", xlab="Iteration")
        abline(h=vr0, col="red")
        arrows(ir, ci[ ,1], ir, ci[ ,3], length=0.03, code=3, angle=90, 
            lwd=ifelse((ci[ , 1]>vr0 | ci[ , 3]<vr0), 2, 1),
            col=ifelse((ci[ , 1]>vr0 | ci[ , 3]<vr0), "red", "blue") )
    }
    # Number of confidence intervals without the ratio of population variances
    nup <- sum(ci[ , 1]>vr0)
    nlow <- sum(ci[ ,3]<vr0)
    cat(paste0("P(LCL > ", vr0, ") = ", nup, "/", N, " = ", nup/N,
        "\t P(UCL < ", vr0, ") = ", nlow, "/", N, " = ", nlow/N), "\n")
}

# Test Plot ----------------------------------------------
unitest.plot <- function(stat, pv, cv, nalt, dig=4, leg, df, ...)
# zrng=c(-4,4), main, xlab, ylab, lwd=2, col="blue", text.col="blue", 
# pv.col="red", fill="lightcyan", pos="topright", cex=cex, df) 
{
    rr <- function(v) format(round(v, dig), nsmall=dig)

    if (df==Inf) {
        pdf <- function(x) dnorm(x)
    } else {
        pdf <- function(x) dt(x, df)
    }

  # Set the list of arguments
    if (df==Inf) {
        mt <- "Distribution of Z0 under H0 ~ N(0,1)"
        xl <- "Test Statistic (Z0)"
        yl <- "f(z)"
    } else {
        mt <- paste0("Distribution of T0 under H0 ~ t(", round(df, 3),")")
        xl <- "Test Statistic (T0)"
        yl <- "f(t)"
    }

    zrng <- c(-4, 4)
    col <- text.col <- "blue"
    lwd <- 2
    fill <- "lightcyan"
    pv.col <- "red"
    cex <- 1
    pos <- switch(nalt, "topright", "topleft", "topright")

  # Set the list of arguments
    dots <- list(...)
    if (length(dots) > 0) {
        pars <- names(dots)
        if ("main" %in% pars) mt <- dots$main
        if ("col" %in% pars) col <- dots$col
            if ("pos" %in% pars) pos <- dots$pos
            if ("text.col" %in% pars) text.col <- dots$text.col
            if ("pv.col" %in% pars) pv.col <- dots$pv.col
            if ("ylab" %in% pars) yl <- dots$ylab
            if ("xlab" %in% pars) xl <- dots$xlab
            if ("xlim" %in% pars) zrng <- dots$xlim
            if ("lwd" %in% pars) lwd <- dots$lwd
            if ("cex" %in% pars) cex <- dots$cex
            if ("fill" %in% pars) fill <- dots$fill
        }

    xa <- seq(zrng[1], zrng[2], length.out=101)

    plot(xa, pdf(xa), type="n", xlab=xl, ylab=yl,
            ylim=c(-0.1, 1)*max(pdf(xa)), main=mt)

  # P-value and the critical region
    hh <- pdf(stat)*0.9
    base <- max(pdf(xa))/20
    if (hh < base) hh <- base

    if (nalt==2) {
        cord.x <- c(stat, seq(stat, zrng[2], length.out=20), zrng[2])
        cord.y <- c(0, pdf(seq(stat, zrng[2], length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(stat, 0, stat, pdf(stat), lwd=lwd, col=col)
        text(stat, hh, rr(pv), pos=4, col=pv.col, cex=cex)
        text(stat, 0, rr(stat), pos=1, col=text.col, cex=cex)
        segments(cv, 0, cv, 0.35, col="red", lty=3)
        mtext(rr(cv), side=3, line=-1.2, at=cv, col=1, cex=cex*0.9)
    } else if (nalt==1) {
        cord.x <- c(zrng[1], seq(zrng[1], stat, length.out=20), stat)
        cord.y <- c(0, pdf(seq(zrng[1], stat, length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(stat, 0, stat, pdf(stat), lwd=lwd, col=col)
        text(stat, hh, rr(pv), pos=2, col=pv.col, cex=cex)
        text(stat, 0, rr(stat), pos=1, col=text.col, cex=cex)
        segments(cv, 0, cv, 0.35, col="red", lty=3)
        mtext(rr(cv), side=3, line=-1.2, at=cv, col=1, cex=cex*0.9)
    } else {
        mup <- abs(stat)
        mlow <- -mup
        cord.x <- c(mup, seq(mup, zrng[2], length.out=20), zrng[2])
        cord.y <- c(0, pdf(seq(mup, zrng[2], length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        cord.x <- c(zrng[1], seq(zrng[1], mlow, length.out=20), mlow)
        cord.y <- c(0, pdf(seq(zrng[1], mlow, length.out=20)), 0)
        polygon(cord.x, cord.y, col=fill)
        segments(c(mlow, mup), 0, c(mlow, mup), pdf(stat), lwd=lwd, col=col)
        text(c(mlow, mup), hh, rr(pv/2), pos=c(2,4), col=pv.col, cex=cex)
        text(c(mlow, mup), 0, rr(c(mlow, mup)), pos=1, col=text.col, cex=cex)
        segments(c(-cv,cv), 0, c(-cv,cv), 0.35, col="red", lty=3)
        mtext(c(paste0("-",rr(cv)), rr(cv)), side=3, line=-1.2, at=c(-cv,cv), 
             col=1, cex=cex*0.9)
    }
    abline(h=0, col=grey(0.5))
    abline(v=0, lty=3, col=grey(0.5))
    lines(xa, pdf(xa), type="l", lwd=lwd, col=col)

  # Legend
    if (!missing(leg)) {
        nb <- length(leg) - 3
        legend(pos, leg, cex=cex*0.9, xjust=0.5, x.intersp=0,
           text.col=c("black", rep("blue", nb), "red", "red"), bg="white")
    }
}

