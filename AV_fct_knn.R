load("~/2020-2021/PROJET-INGE/Mars.RData")
library(ggplot2)
library(plotly)
library(dplyr)

dta <- son2[,seq(1, 1102500, by = 600)]

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

# on trace la courbe du mse en fonction du nombre de dimensions gardees pour l'ACP
# (q), on teste pour q entre 1 et 20

mse <- rep(0, 20)
for (i in 1:20){
  res <- funopare.knn.lcv(Response, CURVES, PRED, q = i, 
                          kind.of.kernel = "quadratic", semimetric = "pca")
  pred <- res$Predicted.values
  mse[i] <- sum((pred - test$nb_bk)^2) / nrow(test)
  print(i)
}
df <- data.frame('q' = seq(1, 20, by = 1), 'mse' = mse)
p <- ggplot(df, aes(x = q, y = mse)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text()) +
  labs(title = 'Pour methode pca, kernel quadratic')
ggplotly(p)

# on choisi le q optimal, cad celui ou le mse est minimal

q_opt <- which.min(mse)

# on calcule la prediction pour cette valeur de q

res <- funopare.knn.lcv(Response, CURVES, PRED, q = q_opt,
                        kind.of.kernel = "quadratic", semimetric = "pca")

df <- data.frame(response = test$nb_bk, prediction = res$Predicted.values)
df <- df %>% add_count(prediction)

ggplot(df, aes(x = response, y = prediction, z = n)) +
  geom_segment(aes(x = 0, y = 0, xend = 6, yend = 6)) +
  geom_point() +
  geom_contour() +
  ylim(0,6) +
  xlim(0,6) +
  theme(plot.title = element_text()) +
  labs(title = 'Pour methode pca, kernel quadratic, q_opt = 8') +
  theme_light()


###############################################################################
################################## FONCTIONS ##################################
###############################################################################

# Fonctions kernel

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

# Fonctions semimetrics

semimetric.pca <- function(DATA1, DATA2, q)
{
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

semimetric.hshift <- function(DATA1, DATA2, grid)
{
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

# Fonction knn

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