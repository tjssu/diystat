# [Ch-3 Functions] ------------------------------------------------
# [Functions adapted from the "prob" package] ---------------------
# 
# [3-P1] Create Sample Space of Rolling Dice
#' @export
rolldie2 <- function(nroll, nface = 6, order=TRUE)
{
  # The outcomes of a single die
  die <- 1:nface
  # Generate all possible combinations of the n dice rolls
  if (order) {
    if (nroll < 10) {
      exp_grid <- function(...) expand.grid(..., KEEP.OUT.ATTRS = FALSE)
      SS <- do.call(exp_grid, rep(list(die), nroll))
    } else {
      stop("The number of rolls should be < 10 for ordered sample...")
    }
  } else {
    require(RcppAlgos)
    require(multicool)
    mult_dbl <- function(x) multicool::multinom(x, useDouble=TRUE)

    SS <- do.call(RcppAlgos::comboGrid, rep(list(die), nroll))
    # Convert the SS matrix to a data frame
    SS <- data.frame(SS)
    # Add the frequency of each combination
    SS$Freq <- apply(SS, 1, mult_dbl)
  }
  # Rename the columns
  colnames(SS)[1:nroll] <- paste0("X", 1:nroll)
  # Return the sample space
  return(SS)
}

# 
# [3-P2] Create Sample Space of Tossing Coins
#' @export
tosscoin2 <- function(toss)
{
    temp <- list()
    for (i in 1:toss) {
        temp[[i]] <- c("H", "T")
    }
    SS <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
    names(SS) <- paste0("X", 1:toss)
    return(SS)
}

# Create a Card Deck.
# [3-P8] Create a Card Deck
#' @export
cards2 <- function(simple=FALSE, jokers = FALSE)
{
    x <- c(2:10, "J", "Q", "K", "A")
    y <- c("Club", "Diamond", "Heart", "Spade")
    SS <- expand.grid(rank = x, suit = y)
    if (jokers) {
        levels(SS$rank) <- c(levels(SS$rank), "Joker")
        SS <- rbind(SS, data.frame(rank = c("Joker", "Joker"), 
            suit <- c(NA, NA)))
    }
    if (simple) {
        SS$suit <- substr(SS$suit, 1, 1)
        SS <- apply(SS, 1, function(x) paste(x, collapse="-"))
    }
    return(SS)
}

# Sampling with replacement, no order
combn.rep <- function(n, k) combn(n+k-1, k) - seq(from=0, len=k)

# Permutation (without replacement, ordered)
permn <- function(x, n) {
    if (n<1) return(vector(class(x)))
    do.call(rbind, lapply(1:length(x), function(i) {
         cbind(x[i], permn(x[-i], n-1)) } ) )
}

# Create Sample Space of Urn Sampling.
# [3-P3] Create Sample Space of Urn Sampling
#' @export
urnsample2 <- function(x, size, replace = FALSE, ordered = FALSE)
{
    # x should be a vector of length n
    n <- length(x)
    if (is.numeric(x)) {
        xf <- x
    } else {
        xf <- as.factor(x)
    }

    if (replace) {
        if (ordered) {
            temp <- list()
            for (i in 1:size) {
                temp[[i]] <- x
            }
            SS <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
        } else {
            dum <- combn.rep(n, size)
            SS <- as.data.frame(t(matrix(xf[dum], size, ncol(dum))))
        }
    } else {
        if (ordered) {
            dum <- permn(x, size)
            if (is.numeric(x)) {
                df <- dum
            } else {
                df <- as.factor(dum)
            }
            SS <- as.data.frame(matrix(df, nrow(dum), size))
        } else {
            dum <- combn(x, size)
            SS <- as.data.frame(t(dum))
        }
    }
    names(SS) <- paste0("X", 1:ncol(SS))
    return(SS)
}

# Union of Events in Data frame Format.
# [3-P4] Union of Two Events
#' @export
union2 <- function(x, y) {
    if (is.vector(x)) out <- union(x, y)
    if (is.data.frame(x)) {
        nc <- ncol(x)
        x2 <- apply(x, 1, paste, collapse=":")
        y2 <- apply(y, 1, paste, collapse=":")
        dum <- union(x2, y2)
        dd <- strsplit(dum, split=":")
        nr <- length(dd)
        out <- as.data.frame(matrix(unlist(dd), nr, nc, byrow=T))
        names(out) <- paste0("X", 1:nc)
    }
    return(out)
}

# Intersection of Events in Data frame Format.
# [3-P5] Intersection of Two Events
#' @export
intersect2 <- function(x, y) {
    if (is.vector(x)) out <- intersect(x, y)
    if (is.data.frame(x)) {
        nc <- ncol(x)
        x2 <- apply(x, 1, paste, collapse=":")
        y2 <- apply(y, 1, paste, collapse=":")
        dum <- intersect(x2, y2)
        if (length(dum)>0) {
            dd <- strsplit(dum, split=":")
            nr <- length(dd)
            out <- as.data.frame(matrix(unlist(dd), nr, nc, byrow=T))
            names(out) <- paste0("X", 1:nc)
        } else {
            out <- NULL
        }
    }
    return(out)
}

# Set Difference of Events in Data frame Format.
# [3-P6] Set Difference of Two Events
#' @export
setdiff2 <- function(x, y) {
    if (is.vector(x)) out <- setdiff(x, y)
    if (is.data.frame(x)) {
        nc <- ncol(x)
        x2 <- apply(x, 1, paste, collapse=":")
        y2 <- apply(y, 1, paste, collapse=":")
        dum <- setdiff(x2, y2)
        if (length(dum)>0) {
            dd <- strsplit(dum, split=":")
            nr <- length(dd)
            out <- as.data.frame(matrix(unlist(dd), nr, nc, byrow=T))
            names(out) <- paste0("X", 1:nc)
        } else {
            out <- NULL
        }
    }
    return(out)
}

# The greatest common divisor -----------------
gcd <- function(x, y) {
  while(y) {
    temp <- y
    y <- x %% y
    x <- temp
  }
  return(x)
}

# Reduce a fraction -----------------
redfrac <- function(x, y) {
  if (y == 0) stop("The denominator must not equal to zero!")
  if (x == 0) {
    x1 <- x
    y1 <- y
  } else {
    cd <- gcd(x, y)
    x1 <- round(x/cd)
    y1 <- round(y/cd)
  }
  return(c(x1, y1))
}

# [Ch-3 Function Manual] -----------------------------------------

#' @title Ch3. Probability
#' @description List of Ch3. Functions.
#' @param fn Function number to be refered, Default: 0.
#' @return None.
#' @examples 
#' ch3.man()
#' @rdname ch3.man
#' @export
ch3.man <- function(fn=0) {
    if (0 %in% fn) {
        cat("Function for Chapter 3. Probability -----------------\n")
        cat("[1] element \t\tDisplay Elements of an Event\n")
        cat("[2] venn.count\t\tCreate the Venn Diagram of Three Events with Counts\n")
        cat("[3] lln.coin \t\tThe Law of Large Numbers: Tossing a Coin\n")
        cat("[4] pprt    \t\tCalculate Probability of an Event\n")
        cat("[5] cprt    \t\tCalculate the Conditional Probability\n")
        cat("[6] indep.event\t\tDetermine Independence of Two Discrete Random Variables\n")
        cat("[7] bayes.plot\t\tDisplay the Prior and the Posterior Probabilities\n\n")
        
        cat("Functions adapted from the \"prob\" package -----------------\n")
        cat("[P1] rolldie2\t\tCreate Sample Space of Rolling Dice\n")
        cat("[P2] tosscoin2\t\tCreate Sample Space of Tossing Coins\n")
        cat("[P3] urnsample2\t\tCreate Sample Space of Urn Sampling\n")
        cat("[P4] union2 \t\tUnion of Two Events\n")
        cat("[P5] intersect2\t\tIntersection of Two Events\n")
        cat("[P6] setdiff2 \t\tSet Difference of Two Events\n")
    }
    if (1 %in% fn) {
        cat("[1] Display Elements of an Event\n")
        cat("element(A, ncol=10, prt=TRUE)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("A  \t Event A (data frame: each row = element).\n")
        cat("[Optional Input]--------------------------\n")
        cat("ncol  \t Number of columns for printing elements, Default: 10.\n")
        cat("prt   \t Logical: print the elements? Default: TRUE.\n")
    }
    if (2 %in% fn) {
        cat("[2] Create the Venn Diagram of Three Events with Counts\n")
        cat("venn.count(A, B, C, N, col=2:4, alp=0.2, ws=c(7,4))\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("A  \t Event A (data frame or vector).\n")
        cat("B  \t Event B (data frame or vector).\n")
        cat("C  \t Event C (data frame or vector).\n")
        cat("N  \t Total number of elements.\n")
        cat("[Optional Input]--------------------------\n")
        cat("col  \t Event colors, Default: 2:4.\n")
        cat("alp  \t Transparency of colors, Default: 0.2.\n")
        cat("ws   \t Graphic window size, Default: c(7,4).\n")
    }
    if (11 %in% fn) {
        cat("[A1] Count the Elements of Intersections\n")
        cat("inter.count(S, EV, subid)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("S  \t Sample space (data frame or vector).\n")
        cat("EV  \t List of events (data frame or vector).\n")
        cat("[Optional Input]--------------------------\n")
        cat("subid \t Binanry code for a specific event.\n")
        cat("name \t Logical: use the event names? Default: FALSE.\n")
    }
    if (3 %in% fn) {
        cat("[3] The Law of Large Numbers: Tossing a Coin\n")
        cat("lln.coin(pr=0.5, np=30, nmax=100, int=0.01, ws=c(7,4), col=\"blue\")\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt \t The probability of the head, Default: 0.5.\n")
        cat("np \t Times for sampling in each run, Default: 30.\n")
        cat("nmax \t Maximum number of runs, Default: 100.\n")
        cat("int \t Time interval of runs per second, Default: 0.01.\n")
        cat("ws  \t Graphic window size, Default: c(7,4).\n")
        cat("col \t Color of the polygons of estimates, Default: \"blue\".\n")
    }
    if (4 %in% fn) {
        cat("[4] Calculate the Probability of an Event\n")
        cat("pprt(A, N, prt=TRUE, dig=0)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("A  \t Event A (data frame: each row = element).\n")
        cat("N  \t Number of elements in the sample space \n")
        cat("[Optional Input]--------------------------\n")
        cat("prt \t Logical: print detailed output? Default: TRUE)\n")
        cat("dig \t Number of decimal places, Default: 0.\n")
    }
    if (5 %in% fn) {
        cat("[5] Calculate the Conditional Probability\n")
        cat("cprt(A, B, prt=TRUE, dig=0)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("A  \t Data frame of conditioned event.\n")
        cat("B  \t Data frame of conditioning event.\n")
        cat("prt \t Logical: print detailed output? Default: TRUE.\n")
        cat("dig \t Number of decimal places, Default: 0.\n")
    }
    if (6 %in% fn) {
        cat("[6] Determine Independence of Two Discrete Random Variables\n")
        cat("indep.event(X, Y, N, prt=TRUE, dig=0, err=1E-6)\n")
        cat("[Mandatory Input]--------------------------\n")
        cat("X  \t First random variable.\n")
        cat("Y  \t Second random variable.\n")
        cat("N  \t Number of elements in the sample space.\n")
        cat("[Optional Input]--------------------------\n")
        cat("prt \t Logical: print detailed output? Default: TRUE.\n")
        cat("dig \t Number of decimal places, Default: 0.\n")
        cat("err \t Error bound for checking independence, Default: 1E-6.\n")
    }
    if (7 %in% fn) {
        cat("[7] Display the Prior and the Posterior Probabilities\n")
        cat("bayes.plot(prior, cond, post, gname, cname=\"X\", dig=4, ws=c(7,4), ...)\n")       
        cat("[Mandatory Input]--(either cond or post)-------\n")
        cat("prior\t Prior probability vector.\n")
        cat("cond\t Conditional probability vector.\n")
        cat("post\t Posterior probability vector (optional).\n")
        cat("[Optional Input]--------------------------\n")
        cat("gname\t Group name, Default: A, B, C, ...\n")
        cat("cname\t Conditional event name, Default: \"X\".\n")
        cat("dcol\t Bar chart colors, Default: rainbow (transparent).\n")
        cat("cex \t Text size of the probability, Default: 1.\n")
        cat("dig \t Number of decimal places, Default: 4.\n")
        cat("ws  \t Graphic window size, Default: c(7,4).\n")
        cat("... \t Other graphic parameters (col, cex, main).\n")
    }
}

# [3-1] Display Elements of an Event

#' @title Elements of an Event
#' @description Displaying Elements of an Event
#' @param A Event in data frame.
#' @param ncol Maximum number of elements in each line, Default: 10.
#' @param prt Logical value for printing elements, Default: TRUE.
#' @return Vector of elements of the event.
#' @examples
#' S <- rolldie2(2)
#' B <- subset(S, (X1+X2) >=8)
#' element(B)
#' 
#' S <- tosscoin2(4)
#' S = S[order(S[[1]], S[[2]], S[[3]], S[[4]]), ]
#' A <- subset(S, apply(S, 1, function(x) sum(x=="H") >=2))
#' element(A, ncol=6)
#' @rdname element
#' @export
element <- function(A, ncol=10, prt=TRUE) {
    # A must be a data frame
  if (is.null(A)) {
    elem <- NULL
    if (prt) cat("NULL\n")
  } else {
    # Number of elements in A 
    n <- nrow(A)
    # Dimension of each element
    d <- ncol(A)
    # Number of lines for print (ncol elements per line)
    m <- ceiling(n/ncol)

    # Create elements
    elem <- "("
    if (d >= 2) for (k in 1:(d-1)) elem <- paste0(elem, A[[k]], ",")
    elem <- paste0(elem, A[[d]], ")")
    # Print elements
    if (prt) {for (k in 1:m) cat(elem[(ncol*(k-1)+1):min(ncol*k,n)], "\n")}
  }
    invisible(elem)
} 

# [3-2] Create the Venn Diagram of Three Events with Counts
#' @title Venn Diagram of Three Events
#' @description Venn Diagram of Three Events with the Number of Elements
#' @param A Event A (data frame or vector).
#' @param B Event B (data frame or vector).
#' @param C Event C (data frame or vector).
#' @param N Total number of elements.
#' @param col Event colors, Default: c(2,3,4).
#' @param alp Transparency of colors, Default: 0.2.
#' @param ws The size of the graphic window, Default: c(7,4).
#' @return The number of elements for c(A, B, C, AB, AC, BC, ABC, S, S/ABC).
#' @examples 
#' S <- rolldie2(2); N <- nrow(S)
#' A <- subset(S, ((X1+X2) %% 2)==0); element(A)
#' B <- subset(S, (X1+X2) >= 8); element(B)
#' C <- subset(S, abs(X1-X2) <= 1); element(C)
#' venn.count(A, B, C, N)
#' @rdname venn.count
#' @export 
venn.count <- function(A, B, C, N, col=2:4, alp=0.2, ws=c(7,4)) {
    require(gplots)
    require(car)

    En <- vector()
    En[1] <- An <- deparse(substitute(A))
    En[2] <- Bn <- deparse(substitute(B))
    En[3] <- Cn <- deparse(substitute(C))
    En[4] <- ABn <- paste0(An, "\U2229", Bn)
    En[5] <- paste0(An, "\U2229", Cn)
    En[6] <- paste0(Bn, "\U2229", Cn)
    En[7] <- paste0(ABn, "\U2229", Cn)
    En[8] <- "S"
    En[9] <- "S/ABC"
    En2 <- paste0("|", En, "|")

    if (is.vector(A)) {
        Ae <- A
        Be <- B
        Ce <- C
    } else if (is.data.frame(A)) {
        Ae <- apply(A, 1, paste0, collapse="")
        Be <- apply(B, 1, paste0, collapse="")
        Ce <- apply(C, 1, paste0, collapse="")
    } else {
        stop("Events must be vectors or data frames.")
    }
    AB <- intersect(Ae, Be)
    AC <- intersect(Ae, Ce)
    BC <- intersect(Be, Ce)
    ABC <- intersect(AB, Ce)
    Uni <- union(Ae, Be)
    Uni <- union(Uni, Ce)
    
    num <- vector()
    num[1] <- length(Ae)
    num[2] <- length(Be)
    num[3] <- length(Ce)
    num[4] <- ifelse (is.null(AB), 0, length(AB))
    num[5] <- ifelse (is.null(AC), 0, length(AC))
    num[6] <- ifelse (is.null(BC), 0, length(BC))
    num[7] <- ifelse (is.null(ABC), 0, length(ABC))
    num[8] <- N
    num[9] <- N - length(Uni)

    # Venn diagram
    win.graph(ws[1], ws[2])
    par(mar=c(0,2,0,2))
    evlist <- list(Ae, Be, Ce)
    names(evlist) <- c(An, Bn, Cn)
    gplots::venn(evlist, showSet=TRUE)
    M <- matrix(c(1, 0, 0, 1), nrow=2)
    car::ellipse(c(200, 250), shape=M, radius=100, center.pch=NULL, 
	fill=T, col=col[1], lwd=2, segments=101, fill.alpha=alp)
    car::ellipse(c(157, 175), shape=M, radius=100, center.pch=NULL, 
	fill=T, col=col[2], lwd=2, segments=101, fill.alpha=alp)
    car::ellipse(c(243, 175), shape=M, radius=100, center.pch=NULL, 
	fill=T, col=col[3], lwd=2, segments=101, fill.alpha=alp)

    # Legend
    legend("topleft", paste0(En2[c(1:3,8)], " = ", num[c(1:3,8)]), 
	cex=0.9, bg="lightyellow")
    legend("topright", paste0(En2[4:7], " = ", num[4:7]), cex=0.9, bg="lightyellow")
    text(200, 50, "000")
    text(200, 30, num[9])
    names(num) <- c("1**","*1*","**1","11*","1*1","*11","111","S-S","000")
    return(num)
}

# [3-3] The Law of Large Numbers: Tossing a Coin
#' @title The Law of Large Numbers: Tossing a Coin
#' @description Animate the Law of Large Numbers for Tossing a Coin.
#' @param pr The probability of the head, Default: 0.5.
#' @param np Times for sampling in each run, Default: 30.
#' @param nmax Maximum number of runs.
#' @param int Time interval of runs per second, Default: 0.01.
#' @param ws Size of graphic window, Default: c(7,4).
#' @param col Color of the polygons of estimates, Default: "blue".
#' @return The number of elements for all partitions in a data frame.
#' @examples 
#' lln.coin(nmax=500)
#' lln.coin(np=5, nmax=500)
#' lln.coin(pr=0.8, col="magenta")
#' lln.coin(pr=0.8, nmax=1000)
#' @rdname lln.coin
#' @export

lln.coin <- function(pr=0.5, np=30, nmax=100, int=0.01, ws=c(7,4), col="blue") {
  require(animation)
  animation::ani.options(nmax = nmax, interval = int)
  win.graph(ws[1], ws[2])
  animation::lln.ani(FUN = function(n, mu) rbinom(n, size=1, prob = mu), 
                     n=np, mu = pr, type="n", col.poly=col)
  title(main=paste("Law of Large Numbers for Tossing Coins: p =", pr))
  grid()
}

# [3-4] Calculate the Probability of an Event by Counting the Elements

#' @title Probability of an Event
#' @description Calculating the Probability of an Event
#' @param A Event in data frame.
#' @param N Size of the sample space.
#' @param prt Print output? Default: TRUE.
#' @param An The name of the event (pprt2).
#' @param prt Logical value for printing detailed output, Default: TRUE.
#' @param dig The number of decimal places, Default: 0 (fraction).
#' @return The probability of an event.
#' @examples
#' S <- rolldie2(2)
#' B <- subset(S, (X1+X2) >=8)
#' pprt(B, nrow(S))
#' pprt2(B, An="Sum >= 8", nrow(S))
#' 
#' S <- tosscoin2(4)
#' A <- subset(S, apply(S, 1, function(x) sum(x=="H") >=2))
#' pprt(A, nrow(S))
#' pprt2(A, "Heads >= 2", nrow(S))
#' @rdname pprt
#' @export 
pprt <- function(A, N, prt=TRUE, dig=0) { 
    An <- deparse(substitute(A))
    # [corr] [corr2 (2021.1.8)]
    numer <- ifelse (is.null(A), 0, nrow(A))
    pr <- numer/N
    org <- paste0(numer, "/", N)
    ## red <- attr(MASS::fractions(pr, 100, 1E10), "fracs")
    rrf <- redfrac(numer, N)
    red <- paste0(rrf[1], "/", rrf[2])

    # Print the probability
    if (prt==TRUE) {
      if (dig == 0) {
        if (numer==0 | org==red) {
            cat(paste0("P(", An, ") = ", org), "\n")
        } else {
            cat(paste0("P(", An, ") = ", org, " = ", red), "\n")
        }
      } else {
        rpr <- round(pr, dig)
        if (numer==0 | org==red) {
            cat(paste0("P(", An, ") = ", org, " ="), rpr, "\n")
        } else {
            cat(paste0("P(", An, ") = ", org, " = ", red, " ="), rpr, "\n")
        }
      }
    }
    invisible(pr)
}

#' @export 
pprt2 <- function(A, An, N, prt=TRUE, dig=0) {
    # [corr] Add event name  [corr2 (2021.1.8)]
    if (missing(An)) An <- deparse(substitute(A))
    numer <- ifelse(is.null(A), 0, nrow(A))
    pr <- numer/N
    org <- paste0(numer, "/", N)
    ## red <- attr(MASS::fractions(pr, 100, 1E10), "fracs")
    rrf <- redfrac(numer, N)
    red <- paste0(rrf[1], "/", rrf[2])

    # Print the probability
    if (prt==TRUE) {
      if (dig == 0) {
        if (numer==0 | org==red) {
            cat(paste0("P(", An, ") = ", org), "\n")
        } else {
            cat(paste0("P(", An, ") = ", org, " = ", red), "\n")
        }
      } else {
        rpr <- round(pr, dig)
        if (numer==0 | org==red) {
            cat(paste0("P(", An, ") = ", org, " ="), rpr, "\n")
        } else {
            cat(paste0("P(", An, ") = ", org, " = ", red, " ="), rpr, "\n")
        }
      }
    }
    invisible(pr)
}

# [3-5] Calculate the Conditional Probability

#' @title Conditional Probability
#' @description Calculating the Conditional Probability
#' @param A Event to be conditioned.
#' @param B Event to be conditioning.
#' @param An The name of the event to be conditioned (cprt2).
#' @param Bn The name of the event to be conditioning (cprt2).
#' @param prt Logical value for printing detailed output, Default: TRUE.
#' @param dig The number of decimal places, Default: 0 (fraction).
#' @return The conditional probability.
#' @examples 
#' S <- rolldie2(2)
#' A <- subset(S, (X1+X2) >=8)
#' B <- subset(S, (X1==6 | X2==6))
#' cprt(A, B)
#' cprt2(A, An="Sum >=8", B, Bn="At least one 6")
#' @rdname cprt
#' @export 
cprt <- function(A, B, prt=TRUE, dig=0) {
    # [corr]
    if (is.null(B)) stop("The conditioning event must not be empty!")
    An <- deparse(substitute(A))
    Bn <- deparse(substitute(B))
    AB <- intersect2(A, B)
    # [corr] [corr2 (2021.1.8)]
    numer <- ifelse (is.null(AB), 0, nrow(AB))
    pr <- numer/nrow(B)
    org <- paste0(numer, "/", nrow(B))
    ## red <- attr(MASS::fractions(pr, 100, 1E10), "fracs")
    rrf <- redfrac(numer, nrow(B))
    red <- paste0(rrf[1], "/", rrf[2])
    # Print the probability
    if (prt) {
      if (dig == 0) {
        if (numer==0 | org==red) {
          cat(paste0("P(", An, "|", Bn, ") = ", org), "\n")
        } else {
          cat(paste0("P(", An, "|", Bn, ") = ", org, " = ", red), "\n")
        }
      } else {
        rpr <- round(pr, dig)
        if (numer==0 | org==red) {
          cat(paste0("P(", An, "|", Bn, ") = ", org, " ="), rpr, "\n")
        } else {
          cat(paste0("P(", An, "|", Bn, ") = ", org, " = ", red, " ="), rpr, "\n")
        }
      }
    }
    invisible(pr)
}

# [3-5A] Calculate the Conditional Probability
#' @export 
cprt2 <- function(A, An, B, Bn, prt=TRUE, dig=0) {
    # [corr]
    if (is.null(B)) stop("The conditioning event must not be empty!")
    AB <- intersect2(A, B)
    # [corr] [corr2 (2021.1.8)]
    numer <- ifelse (is.null(AB), 0, nrow(AB))
    pr <- numer/nrow(B)
    org <- paste0(numer, "/", nrow(B))
    ## red <- attr(MASS::fractions(pr, 100, 1E10), "fracs")
    rrf <- redfrac(numer, nrow(B))
    red <- paste0(rrf[1], "/", rrf[2])

    # Print the probability
    if (prt) {
      if (dig == 0) {
        if (numer==0 | org==red) {
          cat(paste0("P(", An, "|", Bn, ") = ", org), "\n")
        } else {
          cat(paste0("P(", An, "|", Bn, ") = ", org, " = ", red), "\n")
        }
      } else {
        rpr <- round(pr, dig)
        if (numer==0 | org==red) {
          cat(paste0("P(", An, "|", Bn, ") = ", org, " ="), rpr, "\n")
        } else {
          cat(paste0("P(", An, "|", Bn, ") = ", org, " = ", red, " ="), rpr, "\n")
        }
      }
    }
    invisible(pr)
}

# [3-5B] Calculate the Conditional Probability
# title Conditional Probability for the Events from Sequential Experments
# description Calculating the Conditional Probability
# param a Event in the entire experiment.
# param N1 The size of the sample space of the entire experiment.
# param b Event in the initial experiment.
# param N2 The size of the sample space of the initial experiment.
# param prt Logical value for printing detailed output, Default: TRUE.
# param dig The number of decimal places, Default: 0 (fraction).
# return The conditional probability.
# examples 
# S <- rolldie2(2)
# A <- subset(S, (X1==6 | X2==6))
# B <- subset(S, (X1+X2) >=8)
# cprt(B, A)
# rdname cprt3
# export 
cprt3 <- function(A, N1, B, N2, prt=TRUE, dig=0) {
    if (missing(N1)) stop("Input the size of the entire sample sapce!")
    if (missing(N2)) stop("Input the size of the initial sample sapce!")
    # [corr]
    if (is.null(B)) stop("The conditioning event must not be empty!")
    An <- deparse(substitute(A))
    Bn <- deparse(substitute(B))
    n1 <- nrow(A)
    n2 <- nrow(B)
    # [corr] [corr2 (2021.1.8)]
    pr <- n1/N1/(n2/N2)
    org <- paste0(n1*N2, "/", n2*N1)
    ## red <- attr(MASS::fractions(pr, 100, 1E10), "fracs")
    rrf <- redfrac(n1*N1, n2*N1)
    red <- paste0(rrf[1], "/", rrf[2])
    # Print the probability
    if (prt) {
      if (n1*N2==0 | org==red) {
        cat(paste0("P(", An, "|", Bn, ") = (", 
	n1,"/",N1, ") / (",n2,"/",N2,") =", org, " ="), pr, "\n")
      } else {
        cat(paste0("P(", An, "|", Bn, ") = (", 
	n1,"/",N1, ") / (",n2,"/",N2,") =", org, " = ", red, " ="), pr, "\n")
      }
    } else print(pr)

    if (prt) {
      if (dig == 0) {
        if (n1*N2==0 | org==red) {
          cat(paste0("P(", An, "|", Bn, ") = (", 
	    n1,"/",N1, ") / (",n2,"/",N2,") =", org), "\n")
        } else {
          cat(paste0("P(", An, "|", Bn, ") = (", 
	    n1,"/",N1, ") / (",n2,"/",N2,") =", org, " = ", red), "\n")
        }
      } else {
        rpr <- round(pr, dig)
        if (n1*N2==0 | org==red) {
          cat(paste0("P(", An, "|", Bn, ") = (", 
	    n1,"/",N1, ") / (",n2,"/",N2,") =", org, " ="), rpr, "\n")
        } else {
          cat(paste0("P(", An, "|", Bn, ") = (", 
	    n1,"/",N1, ") / (",n2,"/",N2,") =", org, " = ", red, " ="), rpr, "\n")
        }
      }
    }

    invisible(pr)
}

# [3-6] Determine the Independence of Two Discrete Random Variables

#' @title Independence of Random Variables
#' @description Determining Independence of Two Discrete Random Variables
#' @param X First random variable.
#' @param Y Second random variable.
#' @param N Size of the sample space.
#' @param prt Logical value for printing detailed output, Default: TRUE.
#' @param dig The number of decimal places, Default: 0 (fraction).
#' @param err Error limit for probability calculation, Default: 1e-06.
#' @return Probablities to be compared.
#' @examples 
#' # Three dice: Independence of A (even sum) and B (range of 5)
#' S <- rolldie2(3); (N <- nrow(S))
#' even <- function(x) (sum(x)%%2)==0
#' span5 <- function(x) (max(x)-min(x))==5
#' A <- subset(S, apply(S, 1, even)); nrow(A)
#' B <- subset(S, apply(S, 1, span5)); nrow(B)
#' indep.event(A, B, nrow(S))
#' @rdname indep.event
#' @export
indep.event <- function(X, Y, N, prt=TRUE, dig=0, err=1E-6) {
    Xn <- deparse(substitute(X))
    Yn <- deparse(substitute(Y))
    XYn <- paste0(Xn, Yn)

    # [corr] [corr2 (2021.1.8)] [2023.2.25]
    num1 <- ifelse (is.null(X), 0, nrow(X))
    num2 <- ifelse (is.null(Y), 0, nrow(Y))
    p1 <- num1/N
    or1 <- paste0(num1, "/", N)
    rr1 <- redfrac(num1, N)
    re1 <- paste0(rr1[1], "/", rr1[2])
    p2 <- num2/N
    or2 <- paste0(num2, "/", N)
    rr2 <- redfrac(num2, N)
    re2 <- paste0(rr2[1], "/", rr2[2])

    # P(x) * P(y)
    num12 <- num1*num2
    p12 <- num12/N^2
    or12 <- paste0(num12, "/", N^2)
    rr12 <- c(rr1[1]*rr2[1], rr1[2]*rr2[2])
    re12 <- paste0(rr12[1], "/", rr12[2])

    # Probability of the intersection of two events
    XY <- intersect2(X, Y)
    numXY <- ifelse (is.null(XY), 0, nrow(XY))
    pXY <- numXY/N
    orXY <- paste0(numXY, "/", N)
    rrXY <- redfrac(numXY, N)
    reXY <- paste0(rrXY[1], "/", rrXY[2])

    # Print the probability --------------------------
    if (prt) {
      # Fraction -------------------------
      if (dig==0) {
        # Print P(x) and P(y) ------------
        if (num1==0 | rr1[2]==N) {
          cat(paste0("P(", Xn, ") = ", or1), "\n")
        } else {
          cat(paste0("P(", Xn, ") = ", or1, " = ", re1), "\n")
        }
        if (num2==0 | rr2[2]==N) {
          cat(paste0("P(", Yn, ") = ", or2), "\n")
        } else {
          cat(paste0("P(", Yn, ") = ", or2, " = ", re2), "\n")
        }
        # Print P(x) * P(y) ---------------
        if (num12==0 | rr12[2]==N^2) {
          cat(paste0("P(",Xn,")\U00D7","P(",Yn,") = ",or12),"\n")
        } else {
          cat(paste0("P(",Xn,")\U00D7","P(",Yn,") = ",or12," = ",re12),"\n")
        }
        # Print P(x,y) ---------------
        if (numXY==0 | rrXY[2]==N) {
          cat(paste0("P(", XYn, ") = ", orXY), "\n")
        } else {
          cat(paste0("P(", XYn, ") = ", orXY, " = ", reXY), "\n")
        }
      # Decimal (dig > 0) ------------------
      } else {
        rp1 = round(p1, dig)
        rp2 = round(p2, dig)
        rp12 = round(p12, dig)
        rpXY = round(pXY, dig)
        # Print P(x) and P(y) ------------
        if (num1==0 | rr1[2]==N) {
          cat(paste0("P(", Xn, ") = ", or1), "=", rp1, "\n")
        } else {
          cat(paste0("P(", Xn, ") = ", or1, " = ", re1), "=", rp1, "\n")
        }
        if (num2==0 | rr2[2]==N) {
          cat(paste0("P(", Yn, ") = ", or2), "=", rp2, "\n")
        } else {
          cat(paste0("P(", Yn, ") = ", or2, " = ", re2), "=", rp2, "\n")
        }
        # Print P(x) * P(y) ---------------
        if (num12==0 | rr12[2]==N^2) {
          cat(paste0("P(",Xn,")\U00D7","P(",Yn,") = ",or12),"\n")
        } else {
          cat(paste0("P(",Xn,")\U00D7","P(",Yn,") = ",or12," = ",re12),
              "=", rp12, "\n")
        }
        # Print P(x,y) ---------------
        if (numXY==0 | rrXY[2]==N) {
          cat(paste0("P(", XYn, ") = ", orXY), "=", rpXY, "\n")
        } else {
          cat(paste0("P(", XYn, ") = ", orXY, " = ", reXY), "=", rpXY, "\n")
        }
      }
    } # End of prt ------

    # Display the result of determining independence 
    del <- abs(pXY - p12)

    # Display the marginal and conditional probabilities
    if (del < err) {
      indep <- TRUE
      if (prt) {
        cat(paste0("P(", Xn ,")\U00D7P(", Yn, ") = P(", XYn, ")"), 
               "\U21D2", Xn, "and", Yn, "are independent.\n")
        cprt2(X, Xn, Y, Yn, dig=dig); cat("and "); pprt2(X, Xn, N, dig=dig)
        cprt2(Y, Yn, X, Xn, dig=dig); cat("and "); pprt2(Y, Yn, N, dig=dig)
      }
    } else {
      indep <- FALSE
      if (prt) {
        cat("\U21D2", Xn, "and", Yn, "are not independent.\n")
        cprt2(X, Xn, Y, Yn, dig=dig); cat("  but"); pprt2(X, Xn, N, dig=dig)
        cprt2(Y, Yn, X, Xn, dig=dig); cat("  but"); pprt2(Y, Yn, N, dig=dig)
      }
    }

    # Return the probabilities
    invisible(list(Pmult=p12, Pjoin=pXY, Indep=indep))
}

# [3-7] Display the Prior and the Posterior Probabilities

#' @title Prior and Posterior Probabilities
#' @description Displaying the Prior and the Posterior Probabilities
#' @param prior Prior probability distribution vector.
#' @param cond Conditional distribution vector.
#' @param post Posterior probability distribution vector.
#' @param gname Group names, Default: A, B, C, ...
#' @param cname Conditional event name, Default: X.
#' @param dig Number of decimal places, Default: 4.
#' @param ws The size of the graphic window, Default: c(7,4).
#' @param ... Other graphic parameters (col, cex, main).
#' @return None.
#' @examples 
#' prior <- c(0.2, 0.4, 0.3, 0.1)
#' cond <- c(0.04, 0.02, 0.01, 0.05)
#' # Direct way
#' bayes.plot(prior, cond)
#' tot <- prior*cond
#' # Using posterior probabilities
#' post <- tot / sum(tot)
#' bayes.plot(prior, post=post)
#' @rdname bayes.plot
#' @export 
bayes.plot <- function(prior, cond, post, gname, cname="X", dig=4, ws=c(7,4), ...) {
  # Set graphic elements
    n <- length(prior)
    if (missing(gname)) gname <- LETTERS[1:n]

  # Obtain the posterior distribution
    out.post <- FALSE
    if (missing(post)) {
      out.post <- TRUE
      if (missing(cond)) {
	stop("Input either cond or post...")
      } else {
	total <- prior*cond
	post <- total/sum(total)
      }
    }

  # Display graph
    if (is.numeric(ws)) {
        mt <- "Prior Probability vs. Posterior Probability"
        col <- rainbow(n, alpha=0.3)
        cex <- 1

        dots <- list(...)
        pars <- names(dots)
        if ("main" %in% pars) mt <- dots$main
        if ("col" %in% pars) col <- dots$col
        if ("cex" %in% pars) cex <- dots$cex

        win.graph(ws[1], ws[2])
        dum <- barplot(cbind(prior, post), col=col, main=mt, horiz=T)
      # Calculate central location 
        centprior <- cumsum(prior)-prior/2
        centpost <- cumsum(post)-post/2
      # Display the probabilities
        text(centprior, dum[1], labels=paste0("P(", gname, ")\n", prior), cex=cex)
        text(centpost, dum[2], labels=paste0("P(", gname, "|", cname, ")\n", 
            format(post, digits=dig)), cex=cex)
    }
    if (out.post) return(post)
} 

# [3-A1] Count the Elements of Intersections
#' @title Count the Elements of Intersections
#' @description Make a Data frame of Counting the Elements of Intersections.
#' @param S Sample Space (data frame or vector).
#' @param EV List of events (data frame or vector).
#' @param subid Code for specific events to apply.
#' @param name Logical value for using the event names, Default:FALSE.
#' @return The number of elements for all partitions in a data frame.
#' @examples 
#' S <- rolldie2(4); N <- nrow(S)
#' A <- subset(S, ((X1+X2)%%2) == 0)
#' B <- subset(S, (X1+X2) >= 8)
#' C <- subset(S, abs(X1-X2) <= 1)
#' D <- subset(S, apply(S, 1, max) == 6)
#' E <- subset(S, apply(S, 1, min) == 1)
#' inter.count(S, list(A,B,C,D,E))
#' # Count ABD^E
#' inter.count(S, EV=list(A,B,D,E), subid=c(1,1,0,1))
#' @rdname inter.count
#' @export 
inter.count <- function(S, EV, subid) {
    if (!(is.list(EV))) stop("Events must be in a list.")
    M <- length(EV)
    if (is.null(names(EV))) {
      name <- FALSE 
    } else name <- TRUE

    # Convert data frames to vectors
    if (all(sapply(EV, is.data.frame))) {
        S <- apply(S, 1, paste0, collapse="")
        for (k in 1:M) EV[[k]] <- apply(EV[[k]], 1, paste0, collapse="")
    }

    N <- length(S)
    nn <- 2^M

    # Create a grid for partition
    base <- 0:1
    garg <- list()
    for (k in 1:M) garg[[k]] <- base
    edf <- expand.grid(garg)
    # rownames(edf)=apply(edf, 1, paste0, collapse="")

    # Logical partition (1, 0) of S w.r.t. EV (N x M)
    LP <- list()
    for (k in 1:M) LP[[k]] <- as.numeric(S %in% EV[[k]])

    # Count the number of elements
    counts <- function(x) {
        temp <- rep(1, N)
        for (k in 1:M) {
            if (x[k]==0) {
                temp <- temp*(1-LP[[k]])
            } else if (x[k]==1) {
                temp <- temp*LP[[k]]
            }
        }
        sum(temp)
    }
            
    freq <- apply(edf, 1, counts)
    edf <- cbind(edf, freq)
    if (name) {
        name.E <- names(EV)
    } else {
        name.E <- LETTERS[1:M]
    }
    name.Ec <- paste0(name.E, "^")
    colnames(edf) <- c(name.E, "Freq")

    if (!missing(subid)) {
        if (length(subid) != M) stop("The subid length must be the number of events.")
        # Create the event name
        evname <- function(x) {
            if (sum(x==1)+sum(x==0)==0) {
                temp <- "S"
            } else {
                temp <- ""
                for (k in 1:M) {
                    if (x[k]==0) {
                        #if (temp=="") {
                            temp <- paste0(temp, name.Ec[k])
                        #} else {
                        #    temp <- paste0(temp, "\U2229", name.Ec[k])
                        #}
                    } else if (x[k]==1) {
                        #if (temp=="") {
                            temp <- paste0(temp, name.E[k])
                        #} else {
                        #    temp <- paste0(temp, "\U2229", name.E[k])
                        #}
                    }
                }
                return(temp)
            }
        }

        cat(paste0("|", evname(subid), "|"), "=", counts(subid), "\n")
        invisible(edf)
    } else {
        return(edf)
    }
}

##-----------------------------------------------------
# [Incomplete] Up to 4 events, EV = (named) list of events
# [3-A2] Count the intersections of up to four events
#' @title Count the intersections of up to four events
#' @description Draw a Venn diagram and count the intersections of events.
#' @param S Data frame of a sample space.
#' @param EV List of events to draw and count.
#' @param ws Size of graphic window, Default: c(7,4).
#' @return The vector of counts for the intersections.
#' @examples 
#' S = rolldie2(2)
#' S = S[order(S[[1]], S[[2]]), ]
#' A = subset(S, ((X1+X2) %% 2)==0); element(A)
#' B = subset(S, (X1+X2)>=8); element(B)
#' C = subset(S, abs(X1-X2)<=1); element(C)
#' D = subset(S, (X1*X2)<12); element(D)
#' venn.count2(S, EV=list(A,B,C,D))
#' @rdname venn.count2
#' @export 

venn.count2 <- function(S, EV, ws=c(7,4)) {
  require(gplots)
  require(car)
  # (2 <= mm <= 4) ----------------------
  mm <- length(EV)
  En <- vector()
  if (is.null(names(EV))) {
    evns <- names(EV) <- LETTERS[1:mm]
    for (j in 1:mm) En[j] <- evns[j]
  } else {
    En[1:mm] <- names(EV)
  }

  # Convert data frames to vectors
  S <- apply(S, 1, paste0, collapse="")
  for (k in 1:mm) {
    if (is.data.frame(EV[[k]])) EV[[k]] <- apply(EV[[k]], 1, paste0, collapse="")
  }

  # Set the names of the intersections
  ii <- mm
  for (i in 1:(mm-1)) {
    for (j in (i+1):mm) {
      ii <- ii+1
      En[ii] <- paste(En[i], En[j], sep="")
    }
  }
  # 3 or 4 events
  if (mm ==3) {ii <- ii+1; En[ii] <- paste(En[mm+1], En[mm], sep="")}
  if (mm == 4) {
    for (i in 1:(mm-2)) {
      for (j in (i+1):(mm-1)) {
        for (k in (j+1):mm) {
          ii <- ii+1
          En[ii] <- paste0(En[i], En[j], En[k])
        }
      }
    }
    ii <- ii+1
    En[ii] <- paste(En[mm+mm*(mm-1)/2+1], En[mm], sep="")
  }
  # Sample space
    En[ii+1] <- "S"
    En[ii+2] <- paste0("S/(", paste(En[1:mm],collapse="\U222A"),")")
    En2 <- paste0("|", En, "|")
  # Intersections
    IT <- EV
    IT[[mm+1]] <- intersect(EV[[1]], EV[[2]])
    if (mm == 3) {
      IT[[mm+2]] <- intersect(EV[[1]], EV[[3]])
      IT[[mm+3]] <- intersect(EV[[2]], EV[[3]])
      IT[[mm+4]] <- intersect(IT[[mm+1]], EV[[3]])
    }
    if (mm == 4) {
      IT[[mm+2]] <- intersect(EV[[1]], EV[[3]])
      IT[[mm+3]] <- intersect(EV[[1]], EV[[4]])
      IT[[mm+4]] <- intersect(EV[[2]], EV[[3]])
      IT[[mm+5]] <- intersect(EV[[2]], EV[[4]])
      IT[[mm+6]] <- intersect(EV[[3]], EV[[4]])
      IT[[mm+7]] <- intersect(IT[[mm+1]], EV[[3]])
      IT[[mm+8]] <- intersect(IT[[mm+1]], EV[[4]])
      IT[[mm+9]] <- intersect(IT[[mm+2]], EV[[4]])
      IT[[mm+10]] <- intersect(IT[[mm+3]], EV[[4]])
      IT[[mm+11]] <- intersect(IT[[mm+4]], EV[[4]])
    }
  # Number of elements of intersections
    nn <- length(IT)
    num <- sapply(IT,  function (x) ifelse (is.null(x), 0, length(x)))
    num[nn+1] <- N
    Uni <- EV[[1]]
    for (jj in 1:mm) Uni <- union(Uni, EV[[jj]])
    num[nn+2] <- N - length(Uni)
  # Venn diagram
    win.graph(ws[1], ws[2])
    par(mar=c(0,2,0,2))
    gplots::venn(EV, showSet=TRUE)
  # Legend
    if (mm < 4) lpos="topleft" else lpos="bottomleft"
    if (mm < 4) rpos="topright" else rpos="bottomright"
    legend(lpos, paste0(En2[c(1:mm,nn+1)], " = ", num[c(1:mm,nn+1)]), 
	cex=0.9, box.col="red", text.col="blue")
    legend(rpos, paste0(En2[(mm+1):nn], " = ", num[(mm+1):nn]), 
    	cex=0.9, box.col="red", text.col="blue")
    text(200, 25, paste(rep("0",mm), collapse=""))
    text(200, 5, num[nn+2])
    num2 <- c("1*","*1","11","S-S","00")
    num3 <- c("1**","*1*","**1","11*","1*1","*11","111","S-S","000")
    num4 <- c("1***","*1**","**1*","***1", "11**","1*1*","1**1","*11*","*1*1","**11", 
	"111*","11*1","1*11","*111", "1111", "S-S","0000")
    names(num) <- switch(mm-1, num2, num3, num4)
    return(num)
}

# [3-A3] Conditional probabilities from a Two-way table
#' @title Conditional probabilities from a Two-way table
#' @description Obtain conditional probabilities from a Two-way table.
#' @param tab Two-way frequency table with unique col-names and raw-names.
#' @param evt Vector of event names.
#' @param cev Vector of conditional event names.
#' @param prt Logical value for detailed output, Default: TRUE.
#' @examples 
#' # Create a two-way frequency table
#' set.seed(123)
#' tab = as.table(matrix(sample(10:30, 12), ncol=4))
#' colnames(tab) <- c("V", "W", "X", "Y")
#' rownames(tab) <- c("A", "B", "C")
#' addmargins(tab)
#' # P(A|X), P(X|A)
#' condprob(tab, "A", "X")
#' condprob(tab, "X", "A")
#' # P(A or B|X or Y)
#' condprob(tab, c("A","B"), c("X","Y"))
#' @rdname condprob
#' @export 

# Obtain conditional probabilities from a Two-way table
condprob <- function(tab, evt, cev, prt=TRUE, debug=FALSE) {

  # Variable names (uniquely defined)-----
  colv <- colnames(tab)
  rowv <- rownames(tab)
  # Marginal counts -----
  N <- sum(tab)
  rsum <- rowSums(tab)
  csum <- colSums(tab)
  # Check input
  if (missing(evt)) stop("Input event names...")
  if (missing(cev)) stop("Input conditioning event names...")
  # Number of events -----
  nevt <- length(evt)
  ncev <- length(cev)
  nev1 <- sum(rowv %in% evt)
  nev2 <- sum(colv %in% evt)
  ncv1 <- sum(rowv %in% cev)
  ncv2 <- sum(colv %in% cev)

  if (nev1==0) ev1 <- NULL else ev1 <- rowv[rowv %in% evt]
  if (nev2==0) ev2 <- NULL else ev2 <- colv[colv %in% evt]
  if (ncv1==0) cv1 <- NULL else cv1 <- rowv[rowv %in% cev]
  if (ncv2==0) cv2 <- NULL else cv2 <- colv[colv %in% cev]
  if (debug) cat("nev1=",nev1,"nev2=",nev2,"ncv1=",ncv1,"ncv2=",ncv2,"\n")
  if (debug) cat("ev1=",ev1,"ev2=",ev2,"cv1=",cv1,"cv2=",cv2,"\n")

  # Counting frequency -----
  if (nev1>0 && ncv1>0) com1 <- ev1[ev1 %in% cv1] else com1 <- NULL
  if (nev2>0 && ncv2>0) com2 <- ev2[ev2 %in% cv2] else com2 <- NULL
  if (debug) cat("com1=",com1,"com2=",com2,"\n")

  denf <- sum(rsum[cv1], csum[cv2]) - sum(tab[cv1, cv2])
  if (debug) cat("denf =", denf,"\n")

  g11 <- setdiff(ev1,com1)
  g12 <- setdiff(cv2,com2)
  g21 <- setdiff(cv1,com1)
  g22 <- setdiff(ev2,com2)
  if (debug) cat("g11=",g11,"g12=",g12,"g21=",g21,"g22=",g22,"\n")

  freq0 <- function(x,y) {
    if (is.null(x)) {
      if (is.null(y)) {
        fr <- 0
      } else {
        fr <- sum(csum[y])
      }
    } else {
      if (is.null(y)) {
        fr <- sum(rsum[x])
      } else {
        fr <- sum(tab[x,y])
      }
    }
    return(fr)
  }

  freq <- function(x,y) sum(tab[x,y])

  numf <- rep(0, 5)
  numf[1] <- freq(g11, g12)
  numf[2] <- freq(g21, g22)
  numf[3] <- sum(rsum[com1])
  numf[4] <- sum(csum[com2])
  numf[5] <- -sum(tab[com1,com2])

  if (debug) cat("numf=",numf,"\n")
  tnumf <- sum(numf)

  # Conditional probability
  cp <- tnumf/denf
  if (prt) {
    pev <- paste(evt, collapse="\U222A")
    pcv <- paste(cev, collapse="\U222A")
    cat(paste0("P(",pev,"|",pcv,") = ",tnumf,"/",denf),"=", cp,"\n")
  }
  res <- c(tnumf, denf, cp)
  names(res) <- c("numf", "denf", "condp")
  invisible(res)
}




