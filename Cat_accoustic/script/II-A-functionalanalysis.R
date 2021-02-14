###########################################################################
## PROJECT : CAT ACCOUSTIC
## TITLE : II-A-functionalanalysis
## AUTHOR : BONY & de CROUTTE (AGROCAMPUS OUEST)
## DATE : DECEMBER 2020 TO FEBRUARY 2021
###########################################################################

# PACKAGES ----

library(plotly)
library(ggplot2)
library(tuneR)
library(seewave)

# FUNCTIONS ----

imp_left <- function(filename, time){
  # INPUT
  # filename : character, name of the file we want to import
  # OUTPUT
  # vec : vector, left channel of the wav file + 0 in the end to make a 25 seconds
  # (1102500 points) sample
  m <- time*44100
  vec <- rep(0, m)
  file <- normalize(readWave(filename), center = TRUE)@left
  vec[1:min(length(file), m)] <- file[1:min(length(file), m)]
  return(vec)
}

# the functions come from the paper 
# https://www.math.univ-toulouse.fr/~ferraty/SOFTWARES/NPFDA/

### Kernel functions

quadratic <- function(u){
  #  quadratic kernel
  1 - (u)^2
}

triangle <- function(u){
  #  triangle kernel
  1 - u
}

indicator <- function(u){
  Logic0 <- u<0
  Logic1 <- u>1
  Logic01 <- as.logical((1-Logic0) * (1-Logic1))
  u[Logic0] <- 0
  u[Logic1] <- 0
  u[Logic01] <- 1
  return(u)
}

integrated.quadratic <- function(u){
  #  integrated quadratic kernel
  result <- u
  Logic0 <- (u <= -1)
  Logic1 <- (u >= 1)
  Logic2 <- (u > -1 & u < 1)
  result[Logic0] <- 0
  result[Logic1] <- 1
  Uval <- result[Logic2]
  result[Logic2] <- 0.75 * Uval * (1 - (Uval^2)/3) + 0.5
  return(result)
}


### Other needed function

mplsr <- function(X, Y, K = 5){
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

hshift <- function(x,y, grid){
  ####################################################################
  # Returns the "horizontal shifted proximity" between two discretized 
  # curves "x" and "y" (vectors of same length). 
  # The user has to choose a "grid".
  #####################################################################
  lgrid <- length(grid)
  a <- grid[1]
  b <- grid[lgrid]
  rang <- b - a
  lagmax <- floor(0.2 * rang)
  integrand <- (x-y)^2
  Dist1 <- sum(integrand[-1] + integrand[-lgrid])/(2 * rang)
  Dist2 <- Dist1
  for(i in 1:lagmax){
    xlag <- x[-(1:i)]
    xforward <- x[-((lgrid-i+1):lgrid)]
    ylag <- y[-(1:i)]
    yforward <- y[-((lgrid-i+1):lgrid)]
    integrand1 <- (xlag-yforward)^2
    integrand2 <- (xforward-ylag)^2
    lintegrand <- length(integrand1)
    rescaled.range <- 2 * (rang - 2 * i)
    Dist1[i+1] <- sum(integrand1[-1] + integrand1[-lintegrand])/rescaled.range
    Dist2[i+1] <- sum(integrand2[-1] + integrand2[-lintegrand])/rescaled.range
  }
  lag1 <- (0:lagmax)[order(Dist1)[1]]
  lag2 <- (0:lagmax)[order(Dist2)[1]]
  distmin1 <- min(Dist1)
  distmin2 <- min(Dist2)
  if(distmin1 < distmin2){
    distmin <- distmin1
    lagopt <- lag1
  }else{
    distmin <- distmin2
    lagopt <- lag2		
  }
  return(list(dist=sqrt(distmin),lag=lagopt))
}


### Semi-metrics functions

semimetric.pca <- function(DATA1, DATA2, q){
  ###############################################################
  # Computes between curves a pca-type semimetric based on the
  # functional principal components analysis method.
  #    "DATA1" matrix containing a first set of curves stored row by row
  #    "DATA2" matrix containing a second set of curves stored row by row
  #    "q" the retained number of principal components
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
  n <- nrow(DATA1)
  COVARIANCE <- t(DATA1) %*% DATA1/n
  EIGENVECTORS <- eigen(COVARIANCE, sym = T)$vectors[, 1:q]
  COMPONENT1 <- DATA1 %*% EIGENVECTORS
  if(twodatasets) {
    COMPONENT2 <- DATA2 %*% EIGENVECTORS
  }
  else {
    COMPONENT2 <- COMPONENT1
  }
  SEMIMETRIC <- 0
  for(qq in 1:q)
    SEMIMETRIC <- SEMIMETRIC + outer(COMPONENT1[, qq], COMPONENT2[, 
                                                                  qq], "-")^2
  return(sqrt(SEMIMETRIC))
}

semimetric.mplsr <- function(Classes1, DATA1, DATA2, q){
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

semimetric.hshift <- function(DATA1, DATA2, grid){
  ###############################################################
  # Computes between curves a semimetric taking into account an 
  # horizontal shift effect.   
  #    "DATA1" matrix containing a first set of curves stored row by row
  #    "DATA2" matrix containing a second set of curves stored row by row
  #    "grid" vector which defines the grid (one can choose 1,2,...,nbgrid
  #           where nbgrid is the number of points of the discretization) 
  # Returns a "semimetric" matrix containing the semimetric computed 
  # between the curves lying to the first sample and the curves lying  
  # to the second one.
  ###############################################################
  if(is.vector(DATA1)) DATA1 <- as.matrix(t(DATA1))
  if(is.vector(DATA2)) DATA2 <- as.matrix(t(DATA2))
  testfordim <- sum(dim(DATA1)==dim(DATA2))==2
  twodatasets <- T
  if(testfordim) twodatasets <- sum(DATA1==DATA2)!=prod(dim(DATA1))
  n1 <- nrow(DATA1)
  if(twodatasets) n2 <- nrow(DATA2) else n2 <- n1
  SEMIMETRIC <- matrix(0, nrow=n1, ncol=n2)
  if(!twodatasets){
    for(i in 1:(n1-1)){
      for(j in (i+1):n2){
        SEMIMETRIC[i,j] <- hshift(DATA1[i,], DATA2[j,], grid)$dist
      }  
    }
    SEMIMETRIC <- SEMIMETRIC + t(SEMIMETRIC)
  }else{
    for(i in 1:n1){
      for(j in 1:n2){
        SEMIMETRIC[i,j] <- hshift(DATA1[i,], DATA2[j,], grid)$dist
      }  
    }
  }
  return(SEMIMETRIC)
}


### Regression function

funopare.knn.lcv <- function(Response, CURVES, PRED, ..., kind.of.kernel = "quadratic", semimetric = "pca"){
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



# II-A-1) NON PARAMETRIC FUNCTIONAL DATA ANALYSIS ON MARS DATA ----

# import the data
load("data/Mars.RData")

# cut the data because otherwise it is too heavy
dta <- son2[,seq(1, 1102500, by = 200)]

# index of the column which will be the response variable (nb_bk)
n <- ncol(dta) + 1

# add this response variable
dta[,n] <- chat2[,5]
colnames(dta)[n] <- 'nb_bk'
dta$nb_bk <- as.integer(dta$nb_bk)
summary(dta[n])

# separate the dataset in train and test (randomly select the lines)
sel <- sample(1:nrow(dta), as.integer(nrow(dta)/3), replace = FALSE)
train <- dta[-sel,]
test <- dta[sel,]

# prepare the inputs of the function
Response <- train$nb_bk
CURVES <- as.matrix(train[,-n])
PRED <- as.matrix(test[,-n])

# use the prediction function for different parameters to find a minimal MSE
# here example with semimetric pca and q is the number of dimensions kept for the PCA
kernel <- 'quadratic'
semimet <- 'pca'

mse <- rep(0, 65)
for (i in 1:65){
  print(i)
  res <- funopare.knn.lcv(Response, CURVES, PRED, q = i, 
                          kind.of.kernel = kernel, semimetric = semimet)
  pred <- res$Predicted.values
  mse[i] <- sum((pred - test$nb_bk)^2) / nrow(test)
}

# plot MSE depending on q (number of dimensions kept )
df <- data.frame('q' = seq(1, 65, by = 1), 'mse' = mse)
p <- ggplot(df, aes(x = q, y = mse)) +
  geom_line() +
  theme_light() +
  xlab('number of latent variables (q)') +
  ylab('MSE of the prediction') +
  scale_x_continuous(breaks = seq(0, 66, by = 5)) +
  theme(plot.title = element_text()) +
  labs(title = paste0('For method ', semimet, ', kernel ', kernel))
ggplotly(p)


# use the prediction function with the chosen kernel function, semimetric, and
# parameters (example here : q = number of dimensions kept for the PCA)
# see poossible kernel and semimetrics in the description of the functions above
q_opt <- which.min(mse)
res <- funopare.knn.lcv(Response, CURVES, PRED, q = q_opt, 
                        kind.of.kernel = "quadratic", semimetric = "pca")

# plot the predicted values vs actual values
df <- data.frame(response = test$nb_bk, prediction = res$Predicted.values)
title <- paste0('Prediction vs actual for semimetric ', semimet,
                 ', kernel ', kernel, ' and q = ', q_opt)
ggplot(df, aes(x = response, y = prediction)) +
  geom_segment(aes(x = -1, y = -1, xend = 7, yend = 7),
               alpha = 0.5) +
  geom_point(size = 1) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  theme_light() +
  theme(plot.title = element_text(),
        panel.grid.minor = element_blank()) +
  labs(title = title)

cat('MSE = ', sum((res$Predicted.values - test$nb_bk)^2)/length(test$nb_bk))


# II-A-2) NON PARAMETRIC FUNCTIONAL DATA ANALYSIS ON NEW DATA ----

# importation of the data
res <- read.table('data/nb_bk_79samples.csv', sep = ';', header = TRUE)
fich <- list.files('data/wav')
fichmat <-  matrix(fich, nrow = length(fich))

# import the left channel of the data, choosing the time limit we want for the 
# recordings in seconds
time <- 25 
leftlist <- lapply(paste0('data/wav/',fichmat), imp_left, time = time)

# transform the matrix in a dataframe, and row names are file names
df <- data.frame(t(data.frame(leftlist)))
dim(df) # 79 1102500

# add nb_bk column (response variable) (knowing both df and res have the files in 
# alphabetic order)
df$nb_bk <- res$nb_bk
row.names(df) <- fich
View(df[1:5,1:5])

# cut the data because otherwise it is too heavy
dta <- df[,c(seq(1, time*44100, by = 100), time*44100+1)]

# separate the dataset in train and test (randomly select the lines)
sel <- sample(1:nrow(dta), as.integer(nrow(dta)/3), replace = FALSE)
train <- dta[-sel,]
test <- dta[sel,]

# preparer the inputs of the function
Response <- train$nb_bk
CURVES <- as.matrix(train[,1:(ncol(dta)-2)])
PRED <- as.matrix(test[,1:(ncol(dta)-2)])

# technique we try
semimet <- "mplsr"
kernel <- 'quadratic'

# use the prediction function for different parameters to find a minimal MSE
# here example with semimetric mplsr and q is the number of latent variables
mse <- rep(0, 190)
for (i in 1:190){
  print(i)
  res <- funopare.knn.lcv(Response, CURVES, PRED, q = i, 
                          kind.of.kernel = kernel, semimetric = semimet)
  pred <- res$Predicted.values
  mse[i] <- sum((pred - test$nb_bk)^2) / nrow(test)
}

# plot MSE depending on q (number of latent variables)
df1 <- data.frame('q' = seq(1, 190, by = 1), 'mse' = mse)
p <- ggplot(df1, aes(x = q, y = mse)) +
  geom_line() +
  theme_light() +
  xlab('number of latent variables (q)') +
  ylab('MSE of the prediction') +
  theme(plot.title = element_text()) +
  labs(title = paste0('For method ', semimet, ', kernel ', kernel))
ggplotly(p)

# prediction with minimal MSE
q_opt <- which.min(mse)
res <- funopare.knn.lcv(Response, CURVES, PRED, q = q_opt, 
                        kind.of.kernel = kernel, semimetric = semimet)
df2 <- data.frame(response = test$nb_bk, prediction = res$Predicted.values)
title2 <- paste0('Prediction vs actual for semimetric ', semimet,
                 ' and kernel ', kernel, ' and q =', q_opt)
ggplot(df2, aes(x = response, y = prediction)) +
  stat_density2d(geom ='tile', aes(fill=..density..), contour = FALSE) +
  geom_segment(aes(x = -1, y = -1, xend = 4, yend = 4),
               colour = 'lightgrey', alpha = 0.5) +
  geom_point(colour='lightgrey', size = 1) +
  theme(plot.title = element_text()) +
  labs(title = title2) +
  theme_light()
cat('MSE = ', sum((res$Predicted.values - test$nb_bk)^2)/length(test$nb_bk))
