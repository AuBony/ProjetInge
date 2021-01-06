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


######################################
##### Regression
######################################


### sur donnees mars

dta <- son2[,seq(1, 1102500, by = 200)]
dta[,5514] <- chat2[,5]
colnames(dta)[5514] <- 'nb_bk'
View(dta[1:10,5510:5514])
dta$nb_bk <- as.integer(dta$nb_bk)
summary(dta[5514])

sel <- sample(1:194, 64, replace = FALSE)
train <- dta[-sel,]
test <- dta[sel,]

Response <- train$nb_bk
CURVES <- as.matrix(train[,-5514])
PRED <- as.matrix(test[,-5514])

res <- funopare.knn.lcv(Response, CURVES, PRED, q = 2, nknot = 5, range.grid = c(194,1000), kind.of.kernel = "quadratic", semimetric = "deriv")

plot(test[,5514], res$Predicted.values, 
     xlim = c(0,5), ylim = c(0,5),
     xlab = 'variable reponse', ylab = 'prediction',
     main = 'Prediction fonctionnelle en fonction \ndes valeurs observees de nombre de break')
lines(seq(0,5,by = 1), seq(0,5,by = 1))


### sur donnees propres

load("~/2020-2021/PROJET-INGE/complete_clean_data.RData")

sel <- sample(1:48, 18, replace = FALSE)
train <- dta[-sel,]
test <- dta[sel,]

Response <- train$nb_bk
CURVES <- as.matrix(train[,1:882000])
PRED <- as.matrix(test[,1:882000])

res <- funopare.knn.lcv(Response, CURVES, PRED, q = 2, nknot = 5, range.grid = c(1,882000), kind.of.kernel = "quadratic", semimetric = "deriv")

plot(test$nb_bk, res$Predicted.values, 
     xlim = c(0,5), ylim = c(0,5),
     xlab = 'variable reponse', ylab = 'prediction',
     main = 'Prediction fonctionnelle en fonction \ndes valeurs observees de nombre de break')
lines(seq(0,5,by = 1), seq(0,5,by = 1))

#####################################################################

quadratic <- function(u)
{
  #  quadratic kernel
  1 - (u)^2
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

semimetric.deriv <- function(DATA1, DATA2, q, nknot, range.grid)
{
  ###############################################################
  # Computes a semimetric between curves based on their derivatives.
  #    "DATA1" matrix containing a first set of curves stored row by row
  #    "DATA2" matrix containing a second set of curves stored row by row
  #    "q" order of derivation
  #    "nknot" number of interior knots (needed for defining the B-spline basis)
  #    "range.grid" vector of length 2 containing the range of the grid at 
  #                 which the curve are evaluated (i.e. range of the 
  #                 discretization)
  # Returns a "semimetric" matrix containing the semimetric computed 
  # between the curves lying to the first sample and the curves lying  
  # to the second one.
  ###############################################################
  library(splines)
  if(is.vector(DATA1)) DATA1 <- as.matrix(t(DATA1))
  if(is.vector(DATA2)) DATA2 <- as.matrix(t(DATA2))
  testfordim <- sum(dim(DATA1)==dim(DATA2))==2
  twodatasets <- T
  if(testfordim) twodatasets <- sum(DATA1==DATA2)!=prod(dim(DATA1))
  #####################################################################
  # B-spline approximation of the curves containing in DATASET :
  # -----------------------------------------------------------
  # "knot" and "x" allow to define the B-spline basis
  # "coef.mat1[, i]" corresponds to the B-spline expansion
  # of the discretized curve contained in DATASET[i, ]. 
  # The B-spline approximation of the curve contained in "DATA1[i, ]" 
  # is given by "Bspline %*% coef.mat1[, i]"
  #####################################################################
  p <- ncol(DATA1)
  a <- range.grid[1]
  b <- range.grid[2]
  x <- seq(a, b, length = p)
  order.Bspline <- q + 3
  nknotmax <- (p - order.Bspline - 1)%/%2
  if(nknot > nknotmax){
    stop(paste("give a number nknot smaller than ",nknotmax, " for avoiding ill-conditioned matrix"))
  }
  Knot <- seq(a, b, length = nknot + 2)[ - c(1, nknot + 2)]
  delta <- sort(c(rep(c(a, b), order.Bspline), Knot))
  Bspline <- splineDesign(delta, x, order.Bspline)
  Cmat <- crossprod(Bspline)
  Dmat1 <- crossprod(Bspline, t(DATA1))
  coef.mat1 <- symsolve(Cmat, Dmat1)
  #######################################################################
  # Numerical integration by the Gauss method :
  # -------------------------------------------
  # The objects ending by "gauss" allow us to compute numerically  
  # integrals by means the "Gauss method" (lx.gauss=6 ==> the computation 
  # of the integral is exact for polynom of degree less or equal to 11).
  #######################################################################
  point.gauss <- c(-0.9324695142, -0.6612093865, -0.2386191861, 
                   0.2386191861, 0.6612093865, 0.9324695142)
  weight.gauss <- c(0.1713244924, 0.360761573, 0.4679139346, 0.4679139346,0.360761573, 0.1713244924)
  x.gauss <- 0.5 * ((b + a) + (b - a) * point.gauss)
  lx.gauss <- length(x.gauss)
  Bspline.deriv <- splineDesign(delta, x.gauss, order.Bspline, rep(q, lx.gauss))
  H <- t(Bspline.deriv) %*% (Bspline.deriv * (weight.gauss * 0.5 * (b - a)))
  eigH <- eigen(H, sym = T)
  eigH$values[eigH$values < 0] <- 0
  Hhalf <- t(eigH$vectors %*% (t(eigH$vectors) * sqrt(eigH$values)))
  COEF1 <- t(Hhalf %*% coef.mat1)
  if(twodatasets){
    Dmat2 <- crossprod(Bspline, t(DATA2))
    coef.mat2 <- symsolve(Cmat, Dmat2)
    COEF2 <- t(Hhalf %*% coef.mat2)
  } else {
    COEF2 <- COEF1
  }
  SEMIMETRIC <- 0
  nbasis <- nrow(H)
  for(f in 1:nbasis)
    SEMIMETRIC <- SEMIMETRIC + outer(COEF1[, f], COEF2[, f], "-")^2
  return(sqrt(SEMIMETRIC))
}

#####################################################################
#####################################################################

funopare.knn.lcv <- function(Response, CURVES, PRED, ..., kind.of.kernel = "quadratic", semimetric = "deriv")
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