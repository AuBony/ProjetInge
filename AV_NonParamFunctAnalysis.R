####
# Non Parametric Functional Data Analysis
# https://www.math.univ-toulouse.fr/~ferraty/SOFTWARES/NPFDA/
####

library(FactoMineR)


######################################
##### Data importation
######################################

#### Spectrometric Data

spec <- read.table("~/2020-2021/PROJET-INGE/npfda-spectrometric.txt")
View(spec)

plot(x = seq(850,1050,length.out=100), y = spec[1,c(1:100)], 
     ylim = c(2,5.5), type = 'l',
     ylab = 'Absorbance', xlab = 'wavelengths', main = 'Spectrometric curves')
for (i in 2:215){
  lines(x = seq(850,1050,length.out=100), y = spec[i,c(1:100)])
}

#### Phoneme Data

pho <- read.table("~/2020-2021/PROJET-INGE/npfda-phoneme.txt")
View(pho)
pho[,151] <- as.factor(pho[,151])

col <- c('blue','red','green','orange','pink')
plot(x = seq(1,150,length.out=150), y = pho[1,c(1:150)], 
     type = 'l', col = col[as.integer(pho[1,151])],
     ylim = c(0,25), ylab = 'magnitude',
     xlab = 'frequencies', main = 'Phonemes')
for (i in 2:2000){
  lines(x = seq(1,150,length.out=150), y = pho[i,c(1:150)],
        col = col[as.integer(pho[i,151])])
}

#### Cat data

load("~/2020-2021/PROJET-INGE/Mars.RData")

test <- son2[,seq(1, 1102500, by = 100)]


######################################
##### PCA
######################################

#### Spectrometric Data

res.PCA1 <- PCA(spec[,c(1:100)], graph = FALSE)
plot.PCA(res.PCA1, choix = "var", label = "none")
plot.PCA(res.PCA1, choix = "ind", label = "none")

#### Phoneme Data

res.PCA2 <- PCA(pho, quali.sup = 151, graph = FALSE)
plot.PCA(res.PCA2, choix = "var", label = "none")
plot.PCA(res.PCA2, choix = "ind", label = "none", habillage = 151)

#### Cat data

res.PCA3 <- PCA(test, graph = FALSE)
plot.PCA(res.PCA3, choix = "var", label = "none")
plot.PCA(res.PCA3, choix = "ind")

#### Clean cat data

res.PCA4 <- PCA(dta, quanti.sup = c(8821,8822), graph = FALSE)
plot.PCA(res.PCA4, choix = "var", label = "none", axes = c(3,4))
plot.PCA(res.PCA4, choix = "ind", habillage='nb_bk', label = "none", axes = c(3,4))

library(Factoshiny)
Factoshiny(dta)

######################################
##### Regression
######################################


### sur donnees mars

load("~/2020-2021/PROJET-INGE/Mars.RData")

dta <- son2[,seq(1, 1102500, by = 200)]

n <- ncol(dta) + 1

dta[,n] <- chat2[,5]
colnames(dta)[n] <- 'nb_bk'
View(dta[1:10,(n-5):n])
dta$nb_bk <- as.integer(dta$nb_bk)
summary(dta[n])

sel <- sample(1:nrow(dta), as.integer(nrow(dta)/3), replace = FALSE)
train <- dta[-sel,]
test <- dta[sel,]

Response <- train$nb_bk
CURVES <- as.matrix(train[,-n])
PRED <- as.matrix(test[,-n])

res <- funopare.knn.lcv(Response, CURVES, PRED, q = 5, 
                        kind.of.kernel = "quadratic", semimetric = "pca")

plot(test[,n], res$Predicted.values, 
     xlim = c(0,5), ylim = c(0,5),
     xlab = 'variable reponse', ylab = 'prediction',
     main = 'Prediction fonctionnelle en fonction \ndes valeurs observees 
     de nombre de break')
lines(seq(0,5,by = 1), seq(0,5,by = 1))


### sur donnees propres

load("~/2020-2021/PROJET-INGE/complete_clean_data.RData")

ordta <- dta
dta <- ordta[,c(seq(1, 882000, by = 100),882001,882002)]

sel <- sample(1:nrow(dta), as.integer(nrow(dta)/3), replace = FALSE)
train <- dta[-sel,]
test <- dta[sel,]

Response <- train$nb_bk
CURVES <- as.matrix(train[,1:(ncol(dta)-2)])
PRED <- as.matrix(test[,1:(ncol(dta)-2)])

res <- funopare.knn.lcv(Response, CURVES, PRED, q = 150, 
                        kind.of.kernel = "quadratic", semimetric = "mplsr")

plot(test$nb_bk, res$Predicted.values, 
     xlim = c(0,5), ylim = c(0,5),
     xlab = 'variable reponse', ylab = 'prediction',
     main = 'Prediction fonctionnelle en fonction \ndes valeurs observees de nombre de break')
lines(seq(0,5,by = 1), seq(0,5,by = 1))

#####################################################################
#### KERNEL FUNCTIONS
#####################################################################

quadratic <- function(u)
{
  #  quadratic kernel
  1 - (u)^2
}

triangle <- function(u)
{
  #  triangle kernel
  1 - u
}

indicator <- function(u)
{
  Logic0 <- u<0
  Logic1 <- u>1
  Logic01 <- as.logical((1-Logic0) * (1-Logic1))
  u[Logic0] <- 0
  u[Logic1] <- 0
  u[Logic01] <- 1
  return(u)
}

#####################################################################
#####################################################################

symsolve <- function(Asym, Bmat)
{
  #  Performed by J.O. Ramsay and available on its 
  #  website http://ego.psych.mcgill.ca/misc/fda which contains a lot 
  #  of functions for analyzing functional data in a different way:
  #   Solves the system ASYM X = BMAT for X where ASYM is symmetric
  #   Returns X   
  n <- ncol(Asym)
  if(max(abs(Asym - t(Asym)))/max(abs(Asym)) > 1e-10)
    stop("Argument not symmetric.")
  Lmat <- chol(Asym, T)
  if(attr(Lmat, "rank") < n)
    stop("Argument singular.")
  Lmatinv <- solve(Lmat[, order(attr(Lmat, "pivot"))])
  Xmat <- Lmatinv %*% t(Lmatinv) %*% Bmat
  return(Xmat)
}

#####################################################################
#####################################################################

mplsr <- function(X, Y, K = 5)
{
  # Copyright (c) October 1993, Mike Denham.
  # Comments and Complaints to: snsdenhm@reading.ac.uk
  #
  # Orthogonal Scores Algorithm for PLS (Martens and Naes, pp. 121--123)
  #
  # X: predictors (matrix) 
  #
  # Y: multivariate response (matrix)
  #
  # K: The number of PLS factors in the model which must be less than or
  #    equal to the  rank of X.
  #
  # Returned Value is the vector of PLS regression coefficients
  #
  tol <- 1e-10
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  dx <- dim(X)
  nbclass <- ncol(Y)
  xbar <- apply(X, 2, sum)/dx[1]
  ybar <- apply(Y, 2, sum)/dx[1]
  X0 <- X - outer(rep(1, dx[1]), xbar)
  Y0 <- Y - outer(rep(1, dx[1]), ybar)
  W <- matrix(0, dx[2], K)
  P <- matrix(0, dx[2], K)
  Q <- matrix(0, nbclass, K)
  sumofsquaresY <- apply(Y0^2, 2, sum)
  u <- Y0[, order(sumofsquaresY)[nbclass]]
  tee <- 0
  for(i in 1:K) {
    test <- 1 + tol
    while(test > tol) {
      w <- crossprod(X0, u)
      w <- w/sqrt(crossprod(w)[1])
      W[, i] <- w
      teenew <- X0 %*% w
      test <- sum((tee - teenew)^2)
      tee <- teenew
      cee <- crossprod(tee)[1]
      p <- crossprod(X0, (tee/cee))
      P[, i] <- p
      q <- crossprod(Y0, tee)[, 1]/cee
      u <- Y0 %*% q
      u <- u/crossprod(q)[1]
    }
    Q[, i] <- q
    X0 <- X0 - tee %*% t(p)
    Y0 <- Y0 - tee %*% t(q)
  }
  COEF <- W %*% solve(crossprod(P, W)) %*% t(Q)
  b0 <- ybar - t(COEF) %*% xbar
  list(b0 = b0, COEF = COEF)
}

#####################################################################
#####################################################################

semimetric.mplsr <- function(Classes1, DATA1, DATA2, q)
{
  ###############################################################
  # Computes between curves a semimetric based on the partial least 
  # squares method.
  #    "Classe1" vector containing a categorical response which 
  #              corresponds to class number for units stored in DATA1
  #    "DATA1" matrix containing a first set of curves stored row by row
  #    "DATA2" matrix containing a second set of curves stored row by row
  #    "q" the retained number of factors
  # Returns a "semimetric" matrix containing the semimetric computed 
  # between the curves lying to the first sample and the curves lying  
  # to the second one.
  ###############################################################
  if(is.vector(DATA1)) DATA1 <- as.matrix(t(DATA1))
  if(is.vector(DATA2)) DATA2 <- as.matrix(t(DATA2))
  testfordim <- sum(dim(DATA1)==dim(DATA2))==2
  twodatasets <- T
  if(testfordim) twodatasets <- sum(DATA1==DATA2)!=prod(dim(DATA1))
  qmax <- ncol(DATA1)
  if(q > qmax) stop(paste("give a integer q smaller than ", qmax))
  n1 <- nrow(DATA1)
  nbclass <- max(Classes1)
  BINARY1 <- matrix(0, nrow = n1, ncol = nbclass)
  for(g in 1:nbclass) {
    BINARY1[, g] <- as.numeric(Classes1 == g)
  }
  mplsr.res <- mplsr(DATA1, BINARY1, q)
  COMPONENT1 <- DATA1 %*% mplsr.res$COEF
  COMPONENT1 <- outer(rep(1, n1), as.vector(mplsr.res$b0)) + COMPONENT1
  if(twodatasets) {
    n2 <- nrow(DATA2)
    COMPONENT2 <- DATA2 %*% mplsr.res$COEF
    COMPONENT2 <- outer(rep(1, n2), as.vector(mplsr.res$b0)) + 
      COMPONENT2
  }
  else {
    COMPONENT2 <- COMPONENT1
  }
  SEMIMETRIC <- 0
  for(g in 1:nbclass)
    SEMIMETRIC <- SEMIMETRIC + outer(COMPONENT1[, g], COMPONENT2[, 
                                                                 g], "-")^2
  return(sqrt(SEMIMETRIC))
}

#####################################################################
#####################################################################

funopare.knn.lcv <- function(Response, CURVES, PRED, ..., kind.of.kernel = "quadratic", semimetric = "pca")
{
  ################################################################
  # Performs functional prediction (regression) of a scalar response 
  # from a sample of curves via the functional kernel estimator. 
  # A local bandwidth (i.e. local number of neighbours) is selected 
  # by a cross-validation procedure.
  #    "Response" vector containing the observations of the scalar 
  #               response
  #    "CURVES" matrix containing the curves dataset (row by row) 
  #             used for the estimating stage
  #    "PRED" matrix containing new curves stored row by row
  #           used for computing predictions
  #    "..." arguments needed for the call of the function computing 
  #          the semi-metric between curves
  #    "kind.of.kernel" the kernel function used for computing of 
  #                     the kernel estimator; you can choose 
  #                     "indicator", "triangle" or "quadratic (default)
  #    "semimetric" character string allowing to choose the function 
  #                 computing the semimetric;  you can select 
  #                 "deriv" (default), "fourier", "hshift", "mplsr", 
  #                 and "pca"
  # Returns a list containing:
  #    "Estimated.values" vector containing estimated reponses for 
  #                        each curve of "CURVES"
  #    "Predicted.values" if PRED different from CURVES, this vector 
  #                       contains predicted responses for each 
  #                       curve of PRED
  #    "Bandwidths" vector containing the local data-driven bandwidths
  #                 for each curve in the matrix "CURVES"
  #    "Mse" mean squared error between estimated values and 
  #          observed values
  ################################################################
  Response <- as.vector(Response)
  if(is.vector(PRED)) PRED <- as.matrix(t(PRED))
  testfordim <- sum(dim(CURVES)==dim(PRED))==2
  twodatasets <- T
  if(testfordim) twodatasets <- sum(CURVES==PRED)!=prod(dim(CURVES))
  sm <- get(paste("semimetric.", semimetric, sep = ""))
  if(semimetric == "mplsr")
    SEMIMETRIC1 <- sm(Response, CURVES, CURVES, ...)
  else SEMIMETRIC1 <- sm(CURVES, CURVES, ...)
  kernel <- get(kind.of.kernel)
  n1 <- ncol(SEMIMETRIC1)
  step <- ceiling(n1/100)
  if(step == 0)
    step <- 1
  Knearest <- seq(from = 10, to = n1 %/% 2, by = step)
  kmax <- max(Knearest)
  # the vector Knearest contains the sequence of the 
  # k-nearest neighbours used for computing the optimal bandwidth
  Response.estimated <- 0
  Bandwidth.opt <- 0
  Knn1 <- 0
  for(i in 1:n1) {
    Norm.diff <- SEMIMETRIC1[, i]
    # "norm.order" gives the sequence k_1, k_2,... such that
    # dq(X_{k_1},X_i) < dq(X_{k_2},X_i) < ...
    Norm.order <- order(Norm.diff)
    # "zz" contains dq(X_{k_2},X_i), dq(X_{k_3},X_i),..., 
    # dq(X_{j_{kamx+2}},X_i)
    zz <- sort(Norm.diff)[2:(kmax + 2)]
    # Bandwidth[l-1] contains (dq(X_{j_l},X_i) + 
    # dq(X_{j_l},X_i))/2 for l=2,...,kmax+2
    Bandwidth <- 0.5 * (zz[-1] + zz[ - (kmax + 1)])
    z <- zz[ - (kmax + 1)]
    ZMAT <- matrix(rep(z, kmax), nrow = kmax, byrow = T)
    UMAT <- ZMAT/Bandwidth
    KMAT <- kernel(UMAT)
    KMAT[col(KMAT) > row(KMAT)] <- 0
    Ind.curves <- Norm.order[2:(kmax + 1)]
    Ind.resp <- Response[Ind.curves]
    YMAT <- matrix(rep(Ind.resp, kmax), nrow = kmax, byrow = T)
    Hat.resp <- apply(YMAT[Knearest,  ] * KMAT[Knearest,  ], 1, sum
    )/apply(KMAT[Knearest,  ], 1, sum)
    Criterium <- abs(Hat.resp - Response[i])
    index <- order(Criterium)[1]
    Knn1[i] <- Knearest[index]
    Response.estimated[i] <- Hat.resp[index]
    Bandwidth.opt[i] <- Bandwidth[index]
  }
  Mse.estimated <- sum((Response.estimated - Response)^2)/n1
  if(twodatasets) {
    if(semimetric == "mplsr")
      SEMIMETRIC2 <- sm(Response, CURVES, PRED, ...)
    else SEMIMETRIC2 <- sm(CURVES, PRED, ...)
    Bandwidth2 <- 0
    n2 <- ncol(SEMIMETRIC2)
    for(k in 1:n2) {
      Sm2k <- SEMIMETRIC2[, k]
      Sm2k.ord <- order(SEMIMETRIC2[, k])
      knn <- Knn1[Sm2k.ord[1]]
      Bandwidth2[k] <- sum(sort(Sm2k)[knn:(knn+1)])*0.5
    }
    KERNEL <- kernel(t(t(SEMIMETRIC2)/Bandwidth2))
    KERNEL[KERNEL < 0] <- 0
    KERNEL[KERNEL > 1] <- 0
    Denom <- apply(as.matrix(KERNEL), 2, sum)
    RESPKERNEL <- KERNEL * Response
    Response.predicted <- apply(as.matrix(RESPKERNEL), 2, sum)/
      Denom
    return(list(Estimated.values = Response.estimated, 
                Predicted.values = Response.predicted, Bandwidths = 
                  Bandwidth.opt, Mse = Mse.estimated))
  }
  else {
    return(list(Estimated.values = Response.estimated, Bandwidths
                = Bandwidth.opt, Mse = Mse.estimated))
  }
}

#####################################################################
#####################################################################

funopare.quantile.lcv <- function(Response, CURVES, PRED, ..., alpha = c(0.05, 0.5, 0.95), Knearest = NULL, kind.of.kernel = "quadratic", semimetric = "deriv")
{
  ################################################################
  # Performs functional prediction of a scalar response from a 
  # sample of curves by computing the functional conditional mode. 
  # A local bandwidth (i.e. local number of neighbours) is selected 
  # by a ``trivial'' cross-validation procedure.
  #    "Response" vector containing the observations of the scalar 
  #               response
  #    "CURVES" matrix containing the curves dataset (row by row) 
  #             used for the estimating stage
  #    "PRED" matrix containing new curves stored row by row
  #           used for computing predictions
  #    "..." arguments needed for the call of the function computing 
  #          the semi-metric between curves
  #    "alpha"  vector giving the quantiles to be computed. By default, 
  #             the 5-percentile, median and 95-percentile are computed.
  #    "Knearest"  vector giving the the sequence of successive authorized 
  #                integers for the smoothing parameters. By default 
  #                (i.e. Knearest=NULL), the vector Knearest contains a 
  #                sequence of 10 integers taking into account card(I1).
  #    "kind.of.kernel" the kernel function used for computing of 
  #                     the kernel estimator; you can choose 
  #                     "indicator", "triangle" or "quadratic (default)
  #    "semimetric" character string allowing to choose the function 
  #                 computing the semimetric;  you can select 
  #                 "deriv" (default), "fourier", "hshift", "mplsr", 
  #                 and "pca"
  # Returns a list containing:
  #    "Estimated.values" a  card(I2)-by-length(alpha) matrix whose 
  #                       columns gives the corresponding estimated 
  #                       conditional quantiles, for all curves in the 
  #                       second learning subsample I2, 
  #    "Predicted.values" if PRED different from CURVES, this matrix 
  #                       contains predicted conditional quantiles 
  #                       for each curve of PRED
  #    "Response.values"  vector of length the size of the second 
  #                       learning subsample giving the corresponding  
  #                       observed responses.
  #    "Mse" mean squared error between estimated values and 
  #          observed values
  ################################################################
  Response <- as.vector(Response)
  Logicalpha <- alpha==0.5
  nomedian <- !as.logical(sum(Logicalpha))
  if(nomedian) stop("Please, add in the argument alpha the median (i.e 0.5)")
  lalpha <- length(alpha)
  testalpha <- lalpha > 1
  if(testalpha) posmedian <- (1:lalpha)[Logicalpha] 
  if(is.vector(PRED)) PRED <- as.matrix(t(PRED))
  onerow <- nrow(PRED) == 1
  testfordim <- sum(dim(CURVES)==dim(PRED))==2
  twodatasets <- T
  if(testfordim) twodatasets <- sum(CURVES==PRED)!=prod(dim(CURVES))
  sm <- get(paste("semimetric.", semimetric, sep = ""))
  kernel <- get(kind.of.kernel)
  int.kernel <- get(paste("integrated.", kind.of.kernel, sep = ""))
  llearn <- nrow(CURVES)
  Learn1 <- seq(2, llearn, by=2)
  llearn1 <- length(Learn1)
  LEARN1 <- CURVES[Learn1,  ]
  LEARN2 <- CURVES[ - Learn1,  ]
  if(semimetric == "mplsr"){
    SMLEARN1 <- sm(Response, LEARN1, LEARN1, ...)
    SMLEARN12 <- sm(Response, LEARN1, LEARN2, ...)
  } else {
    SMLEARN1 <- sm(LEARN1, LEARN1, ...)
    SMLEARN12 <- sm(LEARN1, LEARN2, ...)
  }
  SML12.SOR <- apply(SMLEARN12, 2, sort)
  Resp1 <- Response[Learn1]
  Resp2 <- Response[ - Learn1]
  Resp.range <- range(Response)
  Response.grid <- seq(from = Resp.range[1] * 0.9, to = Resp.range[2] * 
                         1.1, length = 100)	
  # RESPMETRIC[i,j]=yi-yj with i in Response.grid and j in LEARN1 
  RESPMETRIC <- outer(Response.grid, Resp1, "-")
  RESPMET.SOR <- t(apply(abs(RESPMETRIC), 1, sort))
  llearn2 <- nrow(LEARN2)
  lgrid <- length(Response.grid)
  if(is.null(Knearest)) {
    Knearest.min <- max(ceiling(llearn1 * 0.05), 10)
    Knearest.max <- ceiling(llearn1 * 0.25)
    if(Knearest.max <= Knearest.min){
      Knearest.min <- ceiling(llearn1 * 0.05)
    }
    step <- ceiling((Knearest.max - Knearest.min)/10)
    Knearest <- seq(Knearest.min, Knearest.max, by = step)
  }
  lknearest <- length(Knearest)
  BANDL12.CUR <- 0.5 * (SML12.SOR[Knearest,  ] + SML12.SOR[Knearest + 1, ])
  BAND.RESP <- 0.5 * (RESPMET.SOR[, Knearest] + RESPMET.SOR[, Knearest + 
                                                              1])
  CV <- matrix(0, nrow = lknearest^2, ncol = llearn2)
  if(testalpha){
    QUANT <- array(0, dim = c(lknearest^2, llearn2, lalpha))
    
  } else {
    QUANT <- matrix(0, nrow = lknearest^2, ncol = llearn2)
  }
  count <- 0
  for(kk in 1:lknearest) {
    ARG <- t(t(SMLEARN12)/BANDL12.CUR[kk,])
    KERNEL.CURVES <- kernel(ARG)
    KERNEL.CURVES[KERNEL.CURVES < 0] <- 0
    KERNEL.CURVES[KERNEL.CURVES > 1] <- 0
    Denom <- apply(KERNEL.CURVES, 2, sum)
    for(hh in 1:lknearest) {
      count <- count + 1
      IKERNEL.RESP <- apply(RESPMETRIC/BAND.RESP[, hh], 1,
                            int.kernel)
      CDF.EST <- (t(KERNEL.CURVES)/Denom) %*% IKERNEL.RESP
      if(testalpha){
        for(ii in 1:lalpha){
          Ind.quant <- apply(CDF.EST < alpha[ii], 1, sum)
          Ind.quant[Ind.quant==0] <- 1
          QUANT[count,,ii] <- Response.grid[Ind.quant]
        }
        CV[count,] <- (Resp2 - QUANT[count,,posmedian])^2
      } else {
        Ind.quant <- apply(CDF.EST < alpha, 1, sum)
        Ind.quant[Ind.quant==0] <- 1
        QUANT[count, ] <- Response.grid[Ind.quant]
        CV[count, ] <- (Resp2 - QUANT[count,  ])^2
      }
    }
  }
  Ind.knearest.opt <- apply(CV, 2, order)[1,  ]
  IND.OPT <- cbind(Ind.knearest.opt, 1:llearn2)
  if(testalpha){
    Indkopt <- rep(Ind.knearest.opt, lalpha)
    Units <- rep(1:llearn2, lalpha) 
    Typeofest <- sort(rep(1:lalpha,llearn2))
    RESPONSE.ESTIMATED <- matrix(QUANT[cbind(Indkopt,Units,Typeofest)], nrow=llearn2, byrow=F)
    dimnames(RESPONSE.ESTIMATED) <- list(NULL, as.character(alpha))
  } else {
    RESPONSE.ESTIMATED <- QUANT[IND.OPT]
  }
  Mse.estimated <- sum(CV[IND.OPT])/llearn2
  if(twodatasets) {
    if(semimetric == "mplsr")
      SMLEARN2NEW <- sm(Response, LEARN2, PRED, ...)
    else SMLEARN2NEW <- sm(LEARN2, PRED, ...)
    Order.new <- apply(SMLEARN2NEW, 2, order)[1,  ]
    if(testalpha){
      if(onerow){
        RESPONSE.PREDICTED <- as.matrix(t(RESPONSE.ESTIMATED[Order.new,]))
      } else {
        RESPONSE.PREDICTED <- as.matrix(RESPONSE.ESTIMATED[Order.new,])
        dimnames(RESPONSE.PREDICTED) <- list(NULL, as.character(alpha))}
    } else {
      RESPONSE.PREDICTED <- RESPONSE.ESTIMATED[Order.new]
    }
    return(list(Estimated.values = RESPONSE.ESTIMATED, 
                Predicted.values = RESPONSE.PREDICTED, Response.values
                = Resp2, Mse = Mse.estimated))
  }
  else {
    return(list(Estimated.values = RESPONSE.ESTIMATED, 
                Response.values = Resp2, Mse = Mse.estimated))
  }
}

#####################################################################
#####################################################################