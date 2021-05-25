lastY<-Y[nrow(Y),]

p00<-theta_posterior_means$p_00
p11<-theta_posterior_means$p_11

p0<-(1-p11)/(2-p11-p00)
p1<-(1-p00)/(2-p11-p00)

N <- ncol(Y)
RNG_for_S <- matrix(runif(N), nrow = 1, ncol = N)
S_rr <- matrix(NA, nrow = 1, ncol = N)
S_rr <-ifelse(RNG_for_S < p0, 0, 1)
#epsilon <- rnorm(,0,1)
epsilon<-matrix(0, nrow = 1, ncol = N)

''' notatki
predY<-theta_posterior_means$rho * W %*% predY 
      + S_rr*theta_posterior_means$mu_1 
      + (1-S_rr)*theta_posterior_means$mu_0
      + epsilon
      + theta_posterior_means$phi * lastY

'''
#predY - theta_posterior_means$rho * W %*% predY <-
T<-10
temp <-matrix(NA, nrow = T, ncol = N)
predY <- matrix(NA, nrow = T, ncol = N)
pY<-lastY
#theta_posterior_means$phi*lastY
for (i in 1:T){
  temp[i,] <- S_rr*theta_posterior_means$mu_1 +
    (1-S_rr)*theta_posterior_means$mu_0 +
    epsilon + theta_posterior_means$phi * pY
  
  #predY %*% (diag(N) - theta_posterior_means$rho * W) = temp  
  predY[i,] <- solve(diag(N) - theta_posterior_means$rho * W) %*% temp[i,]
  pY <- predY[i,]
}
  

  
  