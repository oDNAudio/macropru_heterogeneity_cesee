# Functions for estimating a panel vector autoregression with pooling prior on 
# autoregressive coefficients and variances as described in Vashold (EM, 2025)
PVAR <- function(Yraw, Wraw = NULL, p, q, nsave = 5000, nburn = 5000, 
                 cons = FALSE, store.draws = FALSE, store.post=TRUE,
                 crit_eig = 1.00, Multiplier = 5, thin = 3) {
  args <- c(as.list(environment()))
  
  #------------------------------checks----------------------------------------------#
  cN        <- names(Yraw)
  N         <- length(cN)
  varnames  <- colnames(Yraw[[1]])
  Traw      <- unlist(lapply(Yraw,nrow))
  timeraw   <- lapply(Yraw, rownames)
  if(!is.null(Wraw)){
    Wraw.l <- list()
    for(cc in 1:N){
      idx <- which(rownames(Wraw)%in%timeraw[[cc]])
      Wraw.l[[cN[cc]]] <- Wraw[idx,,drop=FALSE]
      if(q > 0) {
        temp <- mlag(Wraw.l[[cN[cc]]],q)
        Wraw.l[[cN[cc]]] <- cbind(Wraw.l[[cN[cc]]], temp)
      }
    }
    varnamesw <- colnames(Wraw.l[[cN[cc]]]);w<-ncol(Wraw.l[[cN[cc]]])
  }else{varnamesw<-NULL;w<-0}
  #------------------------------Setup----------------------------------------------#
  ntot <- nburn+nsave
  Xraw <- list()
  for(cc in 1:N){
    temp <- mlag(Yraw[[cc]],p)
    if(!is.null(Wraw)){temp<-cbind(temp,Wraw.l[[cc]])}
    if(cons){temp <- cbind(temp,1);c<-1;colnames(temp)[ncol(temp)]<-"cons"}else{c<-0}
    Xraw[[cN[cc]]] <- temp
  }
  Y <- lapply(Yraw,function(l)l[(p+1):nrow(l),,drop=FALSE])
  X <- lapply(Xraw,function(l)l[(p+1):nrow(l),,drop=FALSE])
  bigT <- unlist(lapply(Y,nrow))
  M    <- length(varnames)
  K    <- M*p
  k    <- M*p+c+w
  q    <- K*M
  h    <- q*N
  MN   <- M*N
  v    <- (M*(M-1))/2
  #--------------------------OLS estimates------------------------------------------#
  A_OLS <- array(NA, c(k, M, N))
  E_OLS <- list()
  S_OLS <- array(NA, c(M, M, N))
  S_inv <- array(NA, c(M, M, N))
  for(cc in 1:N){
    Y.c <- as.matrix(Y[[cc]])
    X.c <- as.matrix(X[[cc]])
    temp        <- try(solve(crossprod(X.c))%*%t(X.c)%*%Y.c, silent=TRUE)
    if(is(temp,"try-error")) temp <- ginv(crossprod(X.c))%*%t(X.c)%*%Y.c
    A_OLS[,,cc] <- temp
    E_OLS[[cc]] <- Y.c - X.c%*%A_OLS[,,cc]
    S_OLS[,,cc] <- crossprod(E_OLS[[cc]])/(bigT[cc]-k)
    temp <- try(solve(S_OLS[,,cc]), silent=TRUE)
    if(is(temp,"try-error")) temp <- ginv(S_OLS[,,cc])
    S_inv[,,cc] <- temp
  }
  #--------------------------Initialize Gibbs sampler--------------------------------#
  Y.big <- do.call("rbind",Y)
  X.big <- do.call("rbind",X)
  arvar <- arloop(Y.big,X.big,p)
  A_draw <- A_OLS
  dimnames(A_draw)[[1]]<-colnames(X.big)
  dimnames(A_draw)[[2]]<-colnames(Y.big)
  dimnames(A_draw)[[3]]<-cN
  S_draw <- S_OLS
  Em <- Em.str <- lapply(bigT,function(l)matrix(NA,l,M))
  #----------------------------PRIORS-----------------------------------------------#
  lambda1_draw <- .1
  lambda2 <- .5
  lambda3 <- 1
  lambda4 <- 10^2
  Omegab_prior <- array(0, c(k,k,M))
  for(mm in 1:M) {
    diags <- seq(1,K+w)
    ondiag <- seq(mm,M*p,by=M)
    for(pp in 1:p){
      Omegab_prior[ondiag[pp],ondiag[pp],mm] <- (1/(pp^lambda3))^2
      for(mmm in 1:M) {
        z <- (pp-1)*M+mmm
        if(z != ondiag[pp]) Omegab_prior[diags[z],diags[z],mm] <-
            (arvar[mm,1]/arvar[mmm,1])*(lambda2/(pp^lambda3))^2
      }
    }
    if(!is.null(Wraw)){
      for(ww in 1:w) {
        z <- diags[K+ww]
        Omegab_prior[z,z,mm] <- arvar[mm,1]*(lambda4)^2
      }
    }
    if(cons) Omegab_prior[K+w+c,K+w+c,mm] <- arvar[mm,1]*(lambda4)^2
  }
  # make list
  Omegab_prior <- lapply(seq(dim(Omegab_prior)[3]), function(x) Omegab_prior[ , , x])
  Omegab_prior <- Reduce(adiag,Omegab_prior)
  Omegab_priorinv <- diag(1/diag(Omegab_prior))
  V_prior <- lambda1_draw * Omegab_prior
  Vinv_prior <- diag(1/diag(V_prior))
  
  # prior mean for autoregressive parameters
  alpha_draw <- apply(A_OLS, c(1,2), mean)
  
  # prior for lambda 1, the heterogeneity coefficient
  s0 <- 0.01 
  v0 <- 0.01 

  # common mean prior
  cm_prior <-  matrix(10^2, k, M)
  
  # variances
  sigma2.scale <- array(0,c(M,1,N))
  for(cc in 1:N) {
    for (mm in 1:M){
      temp0 <- lm(Y[[cc]][-1,mm]~Y[[cc]][-nrow(Y[[cc]]),mm])  
      sigma2.scale[mm,,cc] <- summary(temp0)$sigma
    }
  }
  m0 <- 2.5 + (M - 1) / 2
  n0 <- 0.5 + (M - 1) / 2
  Q0 <- 100 * n0 / m0 * diag(as.numeric(arvar))
  
  #---------------------------------Create storage matrices--------------------------#
  A_store              <- array(NA, c(nsave, k, M, N))
  alpha_store          <- array(NA, c(nsave, k, M))
  S_store              <- array(NA, c(nsave, M, M, N))
  C0_store             <- array(NA, c(nsave, M, M))
  lambda1_store        <- array(NA, c(nsave, 1))
  dimnames(A_store)[[2]]<-dimnames(alpha_store)[[2]]<-colnames(X.big)
  dimnames(A_store)[[3]]<-dimnames(alpha_store)[[3]]<-varnames
  dimnames(A_store)[[4]]<-cN
  
  #---------------------------Gibbs loop---------------------------------------#
  ntot <- nburn + nsave * thin
  counter<-0
  irep<-1
  n_thin <- 1
  while(irep < (ntot+1)) {
    
    #----------------------------------------------------------------------------------------
    # Step I: Sample autoregressive parameters per country
    for(cc in 1:N) {
      Y.c       <- Y[[cc]]
      X.c       <- X[[cc]]
      S_inv.c <- S_inv[,,cc]
      
      coefs <- drawVARcoef(Y=Y.c, X=X.c, aprior=alpha_draw,
                           Vprior=V_prior, Sigma_inv = S_inv.c)
      A_draw[,,cc]    <- coefs$A
      Em[[cc]]        <- coefs$Em
    }
    
    #----------------------------------------------------------------------------------------
    #Step II: Pooling prior
    alpha_draw  <- drawPOOLcoef(mean=A_draw, var=V_prior, priorvar=cm_prior)
    # Step IIb: update heterogeneity coefficient with Gamma prior -> GIG posterior
    dev <- apply(A_draw, 3, 
                 function(a) t(c(a)-c(alpha_draw))%*%Omegab_priorinv%*%(c(a)-c(alpha_draw)))
    lambda1_draw <- rgig(1, -h/2 + v0, sum(dev), 2 * s0)
    V_prior <- lambda1_draw * Omegab_prior
    Vinv_prior <- diag(1/diag(V_prior))


    #----------------------------------------------------------------------------------------
    # Step III: Sample Sigma from hierarchical Wishart setup
    C0_j <- bayesm::rwishart(N * (n0 + m0 * N),
                             1/N * chol2inv(chol(Q0 + apply(S_inv, c(1,2), sum))))$W 
    for(cc in 1:N) {
      scale0 <- crossprod(Em[[cc]])/2 + C0_j
      v_post <- bigT[[cc]] / 2 + m0
      S_draw[,,cc] <- bayesm::rwishart(N * v_post, 1/N * chol2inv(chol(scale0)))$IW 

      S_inv[,,cc] <- solve(S_draw[,,cc])
    }
    
    #----------------------------------------------------------------------------------------
    # Step IV: Check Stationarity
    Cm <- gen_compMat(alpha_draw,M,p)$Cm
    if(max(abs(Re(eigen(Cm)$values)))>crit_eig && 
       irep > nburn && counter < (nburn+Multiplier*nsave)){
      irep    <- irep
      counter <- counter+1
      next
    }
    #----------------------------------------------------------------------------------------
    # Step V: Store draws after burn-in/ Compute forecasts/ Impulse responses etc.
    if(irep > nburn & (irep %% thin == 0)) {
      A_store[n_thin,,,]                          <- A_draw
      alpha_store[n_thin,,]                       <- alpha_draw
      lambda1_store[n_thin,]                      <- lambda1_draw
      S_store[n_thin,,,]                          <- S_draw
      C0_store[n_thin,,]                          <- C0_j

      n_thin <- n_thin + 1
    }
    
    irep    <- irep+1
    counter <- counter+1
    if(irep%%100==0) print(paste0("Round: ",irep))
  }
  print(paste0("Needed rounds: ", counter, " for ", irep, " total rounds."))
  # end of MCMC loop
  
  
  #------------------------EX POST STUFF-------------------------------------#
  store <- post <- NULL
  #---------------------store draws-------------------------------------------#
  if(store.draws) {
    namespace    <- ls()
    namestore    <- namespace[grepl("_store",namespace)]
    store        <- lapply(namestore, get, envir=sys.frame(sys.parent(0)))
    names(store) <- gsub("_store","",namestore)
  }
  #---------------------compute posteriors-------------------------------------#
  if(store.post) {
    quantile_set <- c(0.05,0.10,0.16,0.5,0.84,0.9,0.95)
    quantile_nam <- c("q05","q10","q16","q50","q84","q90","q95")
    
    for(nn in 1:length(namestore)){
      temp        <- get(namestore[nn], envir=sys.frame(sys.parent(0)))
      if(is.list(temp)){
        post.list <- list()
        for(cc in 1:N){
          dims <- 2:length(dim(temp[[cc]]))
          post        <- lapply(quantile_set, function(q) apply(temp[[cc]], dims, quantile, q,na.rm=TRUE))
          names(post) <- quantile_nam
          post.list[[cN[cc]]] <- post
        }
        assign(gsub("store","post",namestore[nn]),post.list)
      }else{
        dims        <- 2:length(dim(temp))
        post        <- lapply(quantile_set, function(q) apply(temp, dims, quantile, q,na.rm=TRUE))
        names(post) <- quantile_nam
        assign(gsub("store","post",namestore[nn]),post)
      }
    }
    
    namespace    <- ls()
    namepost     <- namespace[grepl("_post",namespace)]
    post         <- lapply(namepost,get, envir=sys.frame(sys.parent(0)))
    names(post)  <- gsub("_post","",namepost)
    post[["counter"]] <- counter
  }
  return(list(post=post,
              store=store,
              args=args))
}

# Auxiliary functions -----------------------------------------------------

# draw country-specific VAR coefficients
drawVARcoef <- function(Y,X,aprior,Vprior,Sigma_inv) {
  M <- ncol(Y)
  T <- nrow(Y)
  K <- round(ncol(X))
  if(all(dim(Vprior)==c(K,M))){
    Vinvprior <- matrix(0, K*M, K*M)
    for(mm in 1:M) {
      Vinvprior[((mm-1)*K+1):(mm*K),((mm-1)*K+1):(mm*K)] <- diag(1/Vprior[,mm])
    }
  } else {
    Vinvprior <- diag(1/diag(Vprior))
  }
  
  # container
  A   <- matrix(NA, K, M)
  Em  <- matrix(NA, T, M)
  rownames(Em) <- rownames(Y)
  colnames(Em) <- colnames(Y)
  
  psi_xx  <-  kronecker(Sigma_inv,crossprod(X))
  
  V_post  <-  try(solve(psi_xx + Vinvprior),silent=TRUE)
  if (is(V_post,"try-error")) V_post <- MASS::ginv(psi_xx + Vinvprior)
  
  IXY  <-   kronecker(diag(M),crossprod(X,Y))
  visig <- as.vector(Sigma_inv)
  a_post  <-  V_post%*%(IXY%*%visig + Vinvprior%*%as.vector(aprior))
  
  alpha  <-  try(a_post + t(chol(V_post))%*%rnorm(M*ncol(X),0,1),silent=TRUE) # Draw alpha
  if (is(alpha,"try-error")) alpha <- t(mvtnorm::rmvnorm(1, a_post, V_post))
  
  A <- matrix(alpha, K, M)
  
  Em <- Y - X %*% A
  
  return(list(A=A,
              Em=Em))
}

# draw common mean parameters
drawPOOLcoef <- function(mean, var, priormean = NULL, priorvar = NULL){
  K <- dim(mean)[[1]]
  M <- dim(mean)[[2]]
  N <- dim(mean)[[3]]
  if(!is.null(priorvar)){
    if(all(dim(priorvar)==c(K,M))){
      priorvarinv <- matrix(0, K*M, K*M)
      for(mm in 1:M) {
        priorvarinv[((mm-1)*K+1):(mm*K),((mm-1)*K+1):(mm*K)] <- diag(1/priorvar[,mm])
      }
    }else{
      priorvarinv <- diag(1/diag(priorvar))
    }
  }else{
    priorvarinv <- diag(K*M)/10
  }
  if(is.null(priormean)) priormean <- matrix(0, K, M)
  
  coef <- matrix(NA, K, M)
  for(mm in 1:M){
    mean.i        <- mean[,mm,]
    varinv.i      <- diag(1/diag(var[((mm-1)*K+1):(mm*K),((mm-1)*K+1):(mm*K)]))
    priormean.i   <- priormean[,mm]
    priorvarinv.i <- priorvarinv[((mm-1)*K+1):(mm*K),((mm-1)*K+1):(mm*K)]
    
    # posterior para
    S_post <- try(chol2inv(chol(N * varinv.i + priorvarinv.i)), silent=TRUE)
    if(is(S_post,"try-error")) S_post <- solve(N * varinv.i + priorvarinv.i)
    mu_post <- S_post %*% (varinv.i %*% apply(mean.i, 1, sum) + priorvarinv.i %*% priormean.i)
    
    # posterior draw
    temp <- try(mu_post + t(chol(S_post))%*%rnorm(K), silent=TRUE)
    if(is(temp,"try_error")) temp <- rmvnorm(1, mu_post, S_post)
    coef[,mm] <- temp
  }
  
  return(coef)
}

# lag matrices
mlag <- function(X,lag)
{
  p <- lag
  X <- as.matrix(X)
  Traw <- nrow(X)
  N <- ncol(X)
  Xlag <- matrix(0,Traw,p*N)
  for (ii in 1:p){
    Xlag[(p+1):Traw,(N*(ii-1)+1):(N*ii)] <- X[(p+1-ii):(Traw-ii),(1:N)]
  }
  colnames(Xlag) <- paste0(colnames(X),".lag",rep(seq(p),each=N))
  return(Xlag)
}

# calculate AR coefficients of individual time series and countries
arloop <- function(Y,X,p,W=NULL) {
  M     <- ncol(Y)
  bigT  <- nrow(Y)
  arvar <- matrix(0, M, 1)
  k     <- ncol(X)
  
  for(mm in 1:M) {
    Y.i <- Y[,mm]
    X.i <- X
    if(!is.null(W)) X.i <- cbind(X.i,W)
    
    B <- try(solve(crossprod(X.i))%*%t(X.i)%*%Y.i,silent=TRUE)
    if(is(B,"try-error")) B <- ginv(crossprod(X.i))%*%t(X.i)%*%Y.i
    eps <- Y.i - X.i%*%B
    
    arvar[mm,] <- crossprod(eps)/(bigT-(k))
  }
  return(arvar)
}

# function to create companion matrix
gen_compMat <- function(A, M, p){
  Jm          <- matrix(0, M*p, M)
  Jm[1:M,1:M] <- diag(M)
  
  Cm  <- matrix(0, M*p, M*p)
  if(p==1) Cm <- t(A[1:(M*p),]) else {
    for(j in 1:(p-1)){
      Cm[(j*M+1):(M*(j+1)),(M*(j-1)+1):(j*M)] <- diag(M)
    }
  }
  bbtemp <- A[1:(M*p),]
  splace <- 0
  for(ii in 1:p){
    for(iii in 1:M) {
      Cm[iii,((ii-1)*M+1):(ii*M)] <- t(bbtemp[(splace+1):(splace+M),iii])
    }
    splace <- splace+M
  }
  return(list(Cm=Cm,
              Jm=Jm))
}

# function to compute IRF identified recursively
compute_irf <- function(rf.coeff, nhor){
  
  p <- rf.coeff$p
  M <- dim(rf.coeff$A)[3]
  
  # group-wide parameters
  A          <-rf.coeff$alpha[,1:(M*p),]
  C0         <-rf.coeff$C0
  # country-specific parameters
  A.c        <-rf.coeff$A[,1:(M*p),,]
  SIGMA      <-rf.coeff$SIGMA

  nsave      <- dim(A)[1]
  K          <- dim(A)[2]
  p          <- round(K/M)
  varnames   <- dimnames(A.c)[[3]]
  cN         <- dimnames(A.c)[[4]]
  N          <- length(cN)
  
  IRF_store       <- array(NA, c(nsave, M, M, nhor)) # mcmc draws, response, shock, nhor
  IRF_ind         <- rep(NA, nsave)
  IRFc_store      <- array(NA, c(nsave, M, M, nhor, N)) # mcmc draws, response, shock, nhor, countries
  dimnames(IRFc_store)[[2]] <- dimnames(IRFc_store)[[3]] <- 
    dimnames(IRF_store)[[2]] <- dimnames(IRF_store)[[3]] <- varnames
  dimnames(IRFc_store)[[5]] <- cN
  
  for(irep in 1:nsave){
    ### regional (mean) responses
    temp <- comp_irf(A=A[irep,,],
                     SIGMA=C0[irep,,], nhor = nhor)
    IRF_store[irep,,,]  <- temp$impresp
    
    ### CC-Responses
    for(cc in 1:N){
      IRFc_store[irep,,,,cc] <- comp_irf(A=A.c[irep,,,cc],
                                         SIGMA=SIGMA[irep,,,cc], nhor = nhor)$impresp
    }
    IRF_ind[irep] <- 1
  }
  ret <- list(IRF_ind = IRF_ind,
              IRF_store  = IRF_store,
              IRFc_store  = IRFc_store)
  
  return(ret)
}

comp_irf <- function(A, SIGMA, nhor){
  M <- ncol(A)
  p <- nrow(A)/M
  
  # companion matrix
  temp    <- gen_compMat(A=A, M=M, p=p)
  compMat <- temp$Cm
  J       <- temp$Jm
  
  # Cholesky
  shock <- t(chol(SIGMA))
  
  # normalizing to unit shock
  diagonal <- diag(diag(shock))
  shock <- solve(diagonal)%*%shock
  
  impresp <- array(0, c(M, M, nhor))
  impresp[,,1] <- shock
  compMati <- compMat
  for(j in 2:nhor) {
    impresp[,,j] <- t(J) %*% compMati %*% J %*% shock
    compMati <- compMati %*% compMat
  }
  
  return(list(impresp=impresp))
}


# FEVD function
compute_fevd <- function(irf_obj) {
  
  nsave <- dim(irf_obj$IRFc_store)[1]
  M <- dim(irf_obj$IRFc_store)[2]
  horz <- dim(irf_obj$IRFc_store)[4]
  N <- dim(irf_obj$IRFc_store)[5]
  var_names <- dimnames(irf_obj$IRFc_store)[2]
  cN <- dimnames(irf_obj$IRFc_store)[5]
  
  IRF_common <- irf_obj$IRF_store
  IRF_c <- irf_obj$IRFc_store
  
  FEVD_common <- array(0, c(nsave, horz, M, M))
  dimnames(FEVD_common)[3] <- dimnames(FEVD_common)[4] <- var_names 
  FEVD_c <- array(0, c(nsave, horz, M, M, N))
  dimnames(FEVD_c)[3] <- dimnames(FEVD_c)[4] <- var_names 
  dimnames(FEVD_c)[5] <- cN
  
  for(i in seq_len(nsave)){
    
    irf_comp <- IRF_common[i,,,]
    fevd_comp <- apply(irf_comp * irf_comp, c(1, 2), cumsum)
    tmp <- matrix(0, M, M)
    for(j in 1:horz) {
      tmp <- tmp + tcrossprod(irf_comp[, , j])
      fevd_comp[j, , ] <- fevd_comp[j, , ] * (1 / diag(tmp))
    }
    
    FEVD_common[i,,,] <- fevd_comp
    
    for(cc in seq_len(N)) {
      
      irf_comp <- IRF_c[i,,,,cc]
      
      fevd_comp <- apply(irf_comp * irf_comp, c(1, 2), cumsum)
      tmp <- matrix(0, M, M)
      for(j in 1:horz) {
        tmp <- tmp + tcrossprod(irf_comp[, , j])
        fevd_comp[j, , ] <- fevd_comp[j, , ] * (1 / diag(tmp))
      }
      
      FEVD_c[i,,,,cc] <- fevd_comp
    }
    
  }
  
  return(list(FEVD_common = FEVD_common,
              FEVD_c = FEVD_c))
}