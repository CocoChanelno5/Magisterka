pCADFtest <- function(Y, X=NULL, covariates=NULL, crosscorr=0.10, 
                      type="trend", data=list(), max.lag.y=1, 
                      min.lag.X=0, max.lag.X=0, dname=NULL, 
                      criterion=c("none", "BIC", "AIC", "HQC", "MAIC"))
{
  T <- dim(Y)[1]
  N <- dim(Y)[2]
  if (is.ts(Y)==FALSE) Y <- ts(Y)
  if (is.null(X)==FALSE) {if (is.ts(X)==FALSE) X <- ts(X)}
  fake.Y <- ts(Y, start=1, frequency=1)
  
  if (is.null(covariates)) method <- "Panel-ADF test" else method <- "Panel-CADF test"
  
  # the covariate is the first difference of the first principal component of Y
  # the same covariate is used for all individual tests
  if (is.null(covariates)==FALSE)
  {
    if (covariates[1]=="PC")
    {
      YY <- na.trim(Y)
      X <- ts(princomp(YY, cor=TRUE)$scores[,1], start=start(YY), frequency=frequency(YY))
      XX <- X
      for (i in 2:N) XX <- cbind(XX, X)
      X <- diff(XX)
      X <- window(X, start=start(Y), end=end(Y), frequency=frequency(Y), extend=TRUE)
      covariates <- 1:N
    }
    else
      # for the i-th test, the covariate is the difference of the average of the other Y's
      if (covariates[1]=="DY")
      {
        X <- matrix(NA, T, N)
        for (i in 1:N)
          X[,i] <- apply(Y[,-i], 1, "mean")
        X <- ts(X, start=start(Y), frequency=frequency(Y))
        X <- diff(X)
        X <- window(X, start=start(Y), end=end(Y), frequency=frequency(Y), extend=TRUE)
        covariates <- 1:N
      }
  }
  
  if (length(type)==1)
    type <- rep(type, N)
  
  if (length(max.lag.y)==1)
    max.lag.y <- rep(max.lag.y, N)
  
  if (length(min.lag.X)==1)
    min.lag.X <- rep(min.lag.X, N)
  
  if (length(max.lag.X)==1)
    max.lag.X <- rep(max.lag.X, N)
  
  criterion <- match.arg(criterion)
  
  # individual.results: Nx5 matrix to store 
  # (1) individual p-values
  # (2) individual rho2
  # (3-5) p, q1, q2: orders of the model CADF(p,q1,q2)
  individual.results <- matrix(NA, N, 5)
  colnames(individual.results) <- c("p.value", "rho2", "p", "q1", "q2")
  rownames(individual.results) <- colnames(Y)
  
  # resids: TxN matrix of residuals from individual tests
  resids <- matrix(NA, T, N)
  resids <- ts(resids, start=start(Y), end=end(Y), frequency=frequency(Y))
  
  # Perform individual tests
  for (i in 1:N)
  {
    # dep: dependent variable
    depv <- Y[,i]
    if (is.null(covariates))
    {
      test <- CADFtest(depv, max.lag.y=max.lag.y[i], 
                       min.lag.X=min.lag.X[i], max.lag.X=max.lag.X[i], 
                       type=type[i], criterion=criterion)
      r <- residuals(test)
      r <- window(r, start=start(Y), end=end(Y), frequency=frequency(Y), extend=TRUE)
      test$parameter <- NA
    }
    else
    {
      # covariates
      covs <- X[,which(covariates==i)]
      covs <- window(covs, start=start(depv), end=end(depv), frequency=frequency(depv), extend=TRUE)
      test <- CADFtest(depv~covs, max.lag.y=max.lag.y[i], 
                       min.lag.X=min.lag.X[i], max.lag.X=max.lag.X[i], 
                       type=type[i], criterion=criterion, ...)
      r <- residuals(test)
      r <- window(r, start=start(fake.Y), end=end(fake.Y), frequency=frequency(fake.Y), extend=TRUE)
      r <- ts(r, start=start(Y), end=end(Y), frequency=frequency(Y))
    }
    resids[,i] <- r
    tpv <- test$p.value
    tpv <- max(tpv, 1e-16); tpv <- min(tpv, 1 - 1e-16)
    individual.results[i,1] <- tpv              # p-value 
    individual.results[i,2] <- test$parameter   # estimated rho^2 (for CADF only)
    individual.results[i,3] <- test$max.lag.y   # selected max lag of the dependent 
    individual.results[i,4] <- test$max.lag.X   # selected max lag  of the covariate (for CADF only)
    individual.results[i,5] <- test$min.lag.X   # selected max lead of the covariate (for CADF only)
  }
  # Combination of individual tests
  ptest <- Pesaran(resids)
  if (ptest$p.value >= crosscorr)
  {
    # Perform Choi's test (inverse normal combination)
    Z    <- sum(qnorm(individual.results[,1]))/sqrt(N)   # simple combination statistic
    pval <- pnorm(Z)                                     # pvalue of Choi's test
  }
  else
  {
    # Perform Hartung's adjustment
    hart <- Hartung(individual.results[,1])              # Hartung's correction
    Z    <- hart$statistic
    pval <- hart$p.value
  }
  
  panel.results <- list(statistic=c("test statistic"=Z),
                        parameter=c("mean.rho2" = mean(individual.results[,2])),
                        method=method,
                        p.value=as.vector(pval),
                        corr=(ptest$p.value < crosscorr),
                        individual.tests=individual.results,
                        Pesaran=ptest)
  class(panel.results) <- c("pCADFtest", "htest")
  return(panel.results)
}

Hartung <- function(p, lambda=rep(1,length(p)), kappa=0.2, alpha=0.10)
  #
  # This function applies the modified inverse normal method for the combination of dependent p-values.
  #
  # Arguments:
  # p:         vector of p-values.
  # lambda:    vector of weights. It must be of the same length of p.
  # kappa:     adjustment parameter. It can be either a positive value (0.2 is the default value) or "formula". If 
  #            k == "formula", then it is computed as in Hartung, p. 853.
  # alpha:     level for the 1-alpha confidence interval for rho (0.10 is the default).
  #
  # Value: 
# 	     a list of class ``htest'' containing
#	     statistic: the Ht test statistic
#	     parameter: the number of combined tests (p-values)
#	     p.value: the combined test p-value
#	     conf.int: the confidence interval for the estimated correlation
#	     estimate: the estimated correlation
#	     null.value: the specified hypothesized value under the null
#	     alternative: a character string describing the alternative hypothesis
#	     method: a character srting indicating the type of combination test (only Z-test is implemented)
#	     data.name: a character string giving the name of the data set
#
# Reference:
# Hartung, J. (1999): "A note on combining dependent tests of significance",
#                     Biometrical Journal, 41(7), 849--855.
#
# Author:        Claudio Lupi
# This version:  August 22, 2010.
#
{
  t       <- qnorm(p)
  n       <- length(p)
  avt     <- sum(t)/n
  q       <- sum((t - avt)^2)/(n-1)                          # Hartung, eqn. (2.2)
  rhohat  <- 1 - q
  rhostar <- max(-1/(n-1), rhohat)                           # Hartung, p. 851
  if (kappa=="formula") kappa <- (1 + 1/(n-1) - rhostar)/10  # Hartung, p. 853
  if (kappa=="formula2") kappa <- (1 + 1/(n-1) - rhostar)/5  # Hartung, p. 853
  
  # Hartung inverse normal corrected. See eqn. (2.4)
  Ht <- sum(lambda*t)/sqrt(sum(lambda^2)+((sum(lambda))^2-sum(lambda^2))*(rhostar+kappa*sqrt(2/(n-1))*(1-rhostar)))
  lower <- 1 - (n-1)/qchisq(alpha/2, (n-1)) * q
  upper <- 1 - (n-1)/qchisq((1-alpha/2), (n-1)) * q          # Hartung, eqn. (2.3)
  
  output <- list(statistic=c("Ht"=Ht),
                 parameter=c("N"=n),
                 p.value=pnorm(Ht),
                 conf.int=c(lower, upper),
                 estimate=c("average estimated correlation"=as.vector(rhohat)),
                 null.value=("Ht"=0),
                 alternative="less",
                 method="modified inverse normal combination",
                 data.name=deparse(substitute(p)))
  
  class(output) <- "htest"
  
  return(output)
}

Pesaran <- function(resids)
  #
  # This function computes Pesaran's (2004) test for cross-dependence in panels.
  # Arguments: 
  #		resids: TxN matrix of residuals from N single-equation models over N time series of length T
  #
  # Value:
  #		a list of class ``htest'' containing
  #		statistic: the test statistic
  #		parameter: the length of time series (T) and the number of models (N)
  #		p.value: the test p-value
  #		conf.int: not used
#		estimate: not used
#		null.value: the specified hypothesized value under the null
#		alternative: a character string describing the alternative hypothesis
#		method: a character srting indicating the type of test
#		data.name: a character string giving the name of the data set
#
# Reference:
# Pesaran, M.H. (2004): "General Diagnostic Tests for Cross Section Dependence in Panels"
#			University of Cambridge, mimeo
#
{ 
  T <- dim(resids)[1]
  N <- dim(resids)[2]
  residCorr <- cor(resids, use="pairwise.complete.obs")
  # test statistic
  CD <- sqrt(2*T/N/(N-1)) * sum(residCorr - diag(N))/2
  # p.value
  p.value <- 2*(1-pnorm(abs(CD)))
  
  output <- list(statistic=c("CD"=CD),
                 parameter=c("N"=N, "T"=T),
                 p.value=p.value,
                 conf.int=NULL,
                 estimate=NULL,
                 null.value=c("CD"=0),
                 alternative="two.sided",
                 method="Pesaran's test for residuals cross-correlation",
                 data.name=deparse(substitute(resids)))
  
  class(output) <- "htest"
  
  return(output)
}
Simes <- function(pCADFtest.results, alpha=0.05)
{
  # This function computes the panel unit root Simes test.
  # Arguments:
  # pCADFtest.results: an object of class pCADFtest
  # alpha            : the level of the test (can be a vector)
  # Values:
  # outcome          : logical. TRUE = "Don't reject", FALSE = "Reject" the null
  #		       it is a vector if alpha is a vector. 
  sort.pval <- sort(pCADFtest.results$individual.tests[,1])
  N         <- length(sort.pval)
  critval   <- matrix(NA, N, length(alpha))
  for (i in 1:length(alpha))
  {
    critval[,i] <- (1:N)/N * alpha[i]
  }
  outcome   <- (apply((sort.pval <= critval), 2, "sum")==0)
  return(outcome)
}

## Fisher-type tests (combining p-values)

pfisher <- function(object, 
                    method = c("invchisq", "invnorm", "invlogit"), 
                    N = NULL, exo = c("intercept", "none", "trend"), 
                    lags = NULL) {
  
  #require("fUnitRoots")
  
  data.name <- paste(deparse(substitute(object)))
  
  if(mode(lags) != "numeric") stop("'lags' must be a number")
  if(round(lags) != lags) stop("'lags' must be an integer")
  
  exo <- match.arg(exo)
  method <- match.arg(method)
  
  ## quick fix
  if(exo == "intercept") adfarg <- "c"
  if(exo == "none")      adfarg <- "nc"
  if(exo == "trend")     adfarg <- "ct"
  
  
  ## ADFs and their p-values
  tstats <- apply(object, 2, function(x) unitrootTest(x, lags = lags, type = adfarg)@test$statistic)
  
  pvals <- sapply(tstats, function(x) punitroot(x, N = N, trend = adfarg, statistic = "t"))
  n <- length(pvals)
  
  ## inverse chi-squared aka Fisher aka 'madwu' 
  if(method == "invchisq"){
    stat <-  -2*sum(log(pvals))
    names(stat) <- "P"
    pvalue <- pchisq(stat, df = 2*n, lower.tail = FALSE)
    parameter <- c(df = 2 * n)
    method <-  "Inverse chi-square test (Maddala and Wu)"
  }
  
  ## inverse normal from Choi 2001 
  if(method == "invnorm"){
    stat <-  sum(qnorm(pvals))/sqrt(n)
    names(stat) <- "Norm"
    pvalue <- pnorm(stat, lower.tail = TRUE)
    parameter <- NULL
    method <-  "Inverse normal test (Choi)"
  }
  
  ## inverse logit from Choi 2001 
  if(method == "invlogit"){
    stat <-  sum(log(pvals / (1 - pvals)))
    k <- 3 * (5*n + 4) / pi^2 / n / (5*n + 2) 
    stat <- sqrt(k) * stat
    names(stat) <- "L*"
    pvalue <- pt(stat, df = 5*n+4, lower.tail = TRUE)
    parameter <- c(df = 5*n+4)
    method <-  "Inverse logit test (Choi)"
  }
  
  result <- structure(list(statistic = stat,
                           parameter = parameter,
                           alternative = "at least one series is stationary",
                           data.name = data.name,
                           method = method,
                           p.value = pvalue),
                      class = "htest")
  
  result
  
}
## Hadri test (combining KPSS tests)

phadri <- function(object, exo = c("intercept", "trend"), 
                   kernel = c("Bartlett", "Quadratic Spectral", 
                              "Truncated", "Parzen", "Tukey-Hanning"), 
                   bw = NULL, het = TRUE, ...) {
  
  #require("sandwich")
  
  data.name <- paste(deparse(substitute(object)))
  
  #if(mode(lags) != "numeric") stop("'lags' must be a number")
  #if(round(lags) != lags) stop("'lags' must be an integer")
  
  kernel <- match.arg(kernel)
  exo <- match.arg(exo)
  
  nobs <- nrow(object)
  nseries <- ncol(object)
  
  trend <- 1:nobs
  
  ## internal function for kpss test
  kpss <- function(x, exo = exo, bw = bw, ...){
    if(exo == "intercept") lmobj <- lm(x ~ 1)
    if(exo == "trend")     lmobj <- lm(x ~ trend)
    #  lmobj <- ifelse(exo == "intercept", lm(x ~ 1), lm(x ~ trend)) 
    u <- resid(lmobj)
    uu <- mean(cumsum(u)^2) / nobs
    ## warning: note def. of kernHAC!
    lrv <- kernHAC(lm(u ~ 1), prewhite = FALSE, bw = bw, 
                   kernel = kernel, ...) * nobs             
    uu <- uu / lrv
    list(kpss = uu, lrv = lrv)
  }
  
  if(exo == "intercept") adj <- c(1/6, 1/45)
  if(exo == "trend")     adj <- c(1/15, 11/6300)
  
  ## individual KPSS statistics and long-run variances
  stati <- apply(object, 2, function(x) kpss(x, exo = exo, bw = bw, ...)$kpss)
  lrvi <- apply(object, 2, function(x) kpss(x, exo = exo, bw = bw, ...)$lrv)
  mlrv <- mean(lrvi)
  
  ## cross-sectional heteroskedasticity?
  if(het) {
    stat <- mean(stati)
  } else {
    stat <- mean(stati * lrvi) / mean(lrvi)
  }
  
  ## Hadri statistic 
  stat <- sqrt(nseries) * (stat - adj[1]) / sqrt(adj[2])
  names(stat) <- "H"
  pvalue <- pnorm(stat, lower.tail = FALSE)
  parameter <- NULL
  method <-  "Hadri panel stationarity test"
  
  result <- structure(list(statistic = stat,
                           parameter = parameter,
                           alternative = "at least one series has a unit root",
                           data.name = data.name,
                           method = method,
                           istat = stati,
                           ilrv = lrvi,
                           mlrv = mlrv,
                           p.value = pvalue),
                      class = "htest")
  
  ##result <- list(statistic = htest,
  #               call = cl,
  #               args = args,
  #               idres = idres,
  #               adjval = adjval)
  #class(result) <- "htest"
  result
  
}

print.pCADFtestsummary <- function(x, ...)
{
  # x is an object of class `pCADFtestsummary'
  ttype <- "Panel Covariate Augmented DF test"
  if (nrow(x$test.summary)==2) ttype <- "Panel Augmented DF test"
  cat(ttype, "\n")
  cat("Correction for cross-correlation:", x$corr, "\n")
  cat("\n")
  print(x$individual.tests, ...)
  print(x$test.summary, ...)
}

spsm <- function(pCADFtest.results, alpha = 0.05)
{
  ## Sequential Panel Selection Method
  ##
  ## pCADFtest.results:	pCADFtest object from pCADFtest()
  ## alpha:      	significance level
  
  ## Initialize some values
  N <- nrow(pCADFtest.results$individual.tests) # N: number of time series
  K.rej <- NULL                          # K.rej: variables identified as I(0)
  
  ## Define the full set of time series
  S <- 1:N
  
  ## continue only if the panel test rejects
  if(pCADFtest.results$p.value < alpha)
  {
    ## identify the minimum p-value of univariate tests (u.rej)
    pvalues <- pCADFtest.results$individual.tests[,1]
    r <- which.min(pvalues)
    K.rej <- r
    pvalues[r] <- 9999
    ## S1: all the series except the one under test
    S1 <- S[-r]
    rej <- TRUE
    while((length(S1) > 0)&(rej == TRUE))
    {
      sub.p.values <- pvalues[S1]
      if (pCADFtest.results$corr == FALSE)
      {
        # Perform Choi's test (inverse normal combination)
        Z    <- sum(qnorm(sub.p.values))/sqrt(length(sub.p.values))   # simple combination statistic
        pval <- pnorm(Z)                      # pvalue of Choi's test
      }
      else
      {
        if (length(S1) > 1)
        {
          # Perform Hartung's adjustment
          hart <- Hartung(sub.p.values)       # Hartung's correction
          Z    <- hart$statistic
          pval <- hart$p.value
        }
        else
        {
          pval <- sub.p.values
        }
      }
      rej <- (pval < alpha)
      if (rej == TRUE)
      {
        r <- c(r, which.min(pvalues))
        K.rej <- r
        pvalues[r] <- 9999
        S1 <- S[-r]
      }
    }
  }
  return(K.rej)
}

summary.pCADFtest <- function(object, ...)
{
  # object is an object of class pCADFtest
  ind.tests <- object$individual.tests
  rnames <- 
    c("test statistic:          ",
      "average estimated rho^2: ",
      "p-value:                 ")
  
  cnames <- "Panel-CADF test"
  
  if (is.na(sum(ind.tests[,2])))
  {
    rnames <- rnames[c(1,3)]
    cnames <- "Panel-ADF test"
    ind.tests <- ind.tests[,c(1,3)]
  }
  
  test.summary <- matrix(NA,length(rnames), 1, dimnames=list(rnames,cnames))
  
  test.summary[1] <- object$statistic
  test.summary[2] <- object$parameter
  test.summary[3-(3-length(rnames))] <- object$p.value
  
  pCADFtestsummary <- list(individual.tests=ind.tests,
                           test.summary=test.summary,
                           corr=object$corr)
  
  class(pCADFtestsummary) <- c("pCADFtestsummary")  
  return(pCADFtestsummary)
}

