N <- n_regions
initial <- list(rho = 0.5,
               phi = 0.5,
               mu_1 = rep(9.5, N),
               mu_0 = rep(3.5, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 100,
                 m_prior = matrix(c(3.5,9.5), nrow = 2),
                 M_prior = diag(2))

S = 5000
S0 = 1000
S_rho = 100
S0_rho = 10
S_phi = 100
S0_phi = 10
Y = Y
Yl = Y_lag
W = W

sample_posterior <- function(initial, hyperpar, S, S0, S_rho, S0_rho, S_phi, S0_phi, Y, Yl, W) {
  
  #Step 0: imply from arguments and initialize result matrix
  N <- ncol(Y)
  TT <- nrow(Y)
  simulation <- data.frame(matrix(nrow = S, ncol = length(unlist(initial))))
  colnames(simulation) <- names(unlist(initial))
  rownames(simulation) <- 1:S
  simulation[1,] <- unlist(initial)
  if(!require("truncnorm")) {install.packages("truncnorm"); library(truncnorm)}
  
  lowerbound_rho <- 1/min(eigen(W)$values)
  
  #Step 1: hyperparameters for priors
  attach(hyperpar, warn.conflicts = FALSE)
  
  for (ss in 2:S) {
    ss<-2
    #Step 2: draw S from MM-Gibbs
    theta_prev <- relist(as.numeric(simulation[ss-1,]), N)
    p_Hamilton <- Hamilton_filter(Y, Yl, theta_prev, W)
    S_simul <- multiMoveGibbs(theta_prev, p_Hamilton, 1)[[1]]
    
    #Steps 3-4: sample p_00 and p_11 
    transitions <- ifelse(S_simul[2:TT,] == 0 & S_simul[1:(TT-1),] == 0, 0,
                          ifelse(S_simul[2:TT,] == 0 & S_simul[1:(TT-1),] == 1, 10,
                                 ifelse(S_simul[2:TT,] == 1 & S_simul[1:(TT-1),] == 0, 1,
                                        ifelse(S_simul[2:TT,] == 1 & S_simul[1:(TT-1),] == 1, 11, NA))))
    if(!require("plyr")) {install.packages("plyr"); library(plyr)}
    trans_sum <- apply(data.frame(transitions), 2, FUN = count, vars = colnames(transitions))
    rm(transitions)
    n <- as.data.frame(c(0,1,10,11))
    colnames(n) <- c("x")
    for (ii in 1:length(trans_sum)) {
      colnames(trans_sum[[ii]]) <- c("x", paste0("freq",ii))
      n <- merge(x = n, y = trans_sum[[ii]], by.x = "x", by.y = "x", all = TRUE)
    }
    n[is.na(n)] <- 0
    rm(trans_sum)
    rownames(n) <- paste0("n", c(0,1,10,11))
    n <- n[,-1]
    alpha_posterior <- n + kronecker(matrix(1, nrow = 1, ncol = N), rbind(as.matrix(alpha_prior[1,]), as.matrix(alpha_prior[2,])))
    for (nn in 1:N) {
      p_11_nn <- rbeta(1, alpha_posterior[4,nn], alpha_posterior[3,nn])
      p_00_nn <- rbeta(1, alpha_posterior[1,nn], alpha_posterior[2,nn])
      simulation[ss, 3*N+2+nn] <- p_00_nn
      simulation[ss, 4*N+2+nn] <- p_11_nn
    }
    
    #Step 5: sample sigma^2
    v_posterior <- v_prior + TT
    epsilon <- make_residuals(Y, Yl, S_simul, theta_prev, W)
    delta_posterior <- delta_prior + diag(var(epsilon)) * (TT-1)
    if(!require("nimble")) {install.packages("nimble"); library(nimble)}
    for (nn in 1:N) {
      sigma2_nn <- rinvgamma(n=1, shape = v_posterior/2, scale = delta_posterior[nn]/2)
      simulation[ss, 2*N+2+nn] <- sigma2_nn
    }
    
    #Step 6: sample mu
    X_n <- list()
    m_posterior_n <- list()
    M_posterior_n <- list()
    for (nn in 1:N) {
      X_n[[nn]] <- cbind(matrix(1,nrow = TT) - S_simul[,nn], S_simul[,nn])
      sigma2_nn <- simulation[ss, 2*N+2+nn]
      rho <- simulation[ss-1, 1]
      phi <- simulation[ss-1, 2]
      M_posterior_n[[nn]] <- solve(solve(M_prior) + (sigma2_nn^(-1))*t(X_n[[nn]]) %*% X_n[[nn]])
      m_posterior_n[[nn]] <- M_posterior_n[[nn]] %*% (solve(M_prior) %*% m_prior 
                                                      + (sigma2_nn^(-1)) * t(X_n[[nn]]) 
                                                      %*% (as.matrix(Y[,nn] - rho * Y %*% as.matrix(W[nn,]) - phi * as.matrix(Yl[,nn]))))
      mu_nn <- rmvnorm(n=1, mean=m_posterior_n[[nn]], sigma = M_posterior_n[[nn]])
      simulation[ss, 2+nn] <- mu_nn[2]
      simulation[ss, N+2+nn] <- mu_nn[1]
    }
    
    #Step 7: rho (M-H)
    simul_rho <- rep(NA, S_rho)
    simul_phi <- rep(NA, S_rho)
    acceptance_p <- rep(NA, S_rho-1)
    
    theta_now <- relist(as.numeric(simulation[ss,]), N)
    theta_now[[1]] <- as.numeric(simulation[ss-1,1])
    theta_now[[2]] <- as.numeric(simulation[ss-1,2])
    theta_cand <- theta_now
    simul_rho[1] <- as.numeric(theta_prev[1])
    simul_phi[1] <- as.numeric(theta_prev[2])
    MH_accepted <- FALSE
    
    while(MH_accepted == FALSE) {
      
      if(is.na(acceptance_p[1])) {
        c <- 0.02
        print(paste0("Initiating c to ", c))
      } else {
        if(mean(acceptance_p) < 0.2) {
          c <- c * 0.66
          print(paste0("Downscaling c to ", c))
        } 
        if(mean(acceptance_p) > 0.4) {
          c <- c * 1.5
          print(paste0("Upscaling c to ", c))
        }
        if(mean(acceptance_p) >= 0.2 && mean(acceptance_p) <= 0.4) {
          MH_accepted <- TRUE
          print(paste0("Terminating procedure for rho with c=", c, " and avg accept p = ", mean(acceptance_p)))
        }
      }
      
      if(MH_accepted == FALSE) {
        for (sr in 2:S_rho) {
          #sr<-2
          rho_candidate <- rtruncnorm(1, a=lowerbound_rho, b=1, mean = simul_rho[sr-1], sd = c)
          phi_candidate <- rtruncnorm(1, a=-1, b=1, mean = simul_phi[sr-1], sd = c*2)
          theta_cand[[1]] <- rho_candidate
          theta_cand[[2]] <- phi_candidate
          log_post_now <- MH_posterior_cond(Y, Yl, S_simul, theta_now, W)
          log_post_cand <- MH_posterior_cond(Y, Yl, S_simul, theta_cand, W)
          acceptance_p[sr-1] <- min(exp(log_post_cand - log_post_now), 1)
          if (runif(1) <= acceptance_p[sr-1]) {
            simul_rho[sr] <- rho_candidate
            simul_phi[sr] <- phi_candidate
            theta_now <- theta_cand
          } else {
            simul_rho[sr] <- simul_rho[sr-1]
            simul_phi[sr] <- simul_phi[sr-1]
          }
        }
        print(paste0("Avg acceptance prob for rho and phi: ", mean(acceptance_p)))
      }
    }
    
    simul_rho <- simul_rho[(S0_rho+1):length(simul_rho)]
    draw_rho <- simul_rho[ceiling(runif(1, min = 0, max = length(simul_rho)))]
    
    simul_phi <- simul_phi[(S0_rho+1):length(simul_phi)]
    draw_phi <- simul_phi[ceiling(runif(1, min = 0, max = length(simul_phi)))]
    
    simulation[ss, 1] <- draw_rho
    simulation[ss, 2] <- draw_phi
    
    print(paste0("### Iteration ", ss))
    
  }
  
  return(simulation[(S0+1):nrow(simulation),])
}

MH_posterior_cond <- function(Y, Yl, S, theta, W) {
  #theta dwuelementowa
  #theta<-theta_now
  #S<-S_simul
  N <- length(theta$omega_d)
  TT <- nrow(Y)
  det_M_inv <- det(diag(N) - theta$rho * W)
  epsilon <- make_residuals(Y, Yl, S, theta, W)
  p <- TT * log(det_M_inv)
  inv_Omega <- solve(diag(theta$omega_d))
  for (tt in 1:TT) {
    #tt<-1
    p <- p - 0.5 * as.numeric(t(as.matrix(epsilon[tt, ])) %*% inv_Omega %*% as.matrix(epsilon[tt, ]))
    print(p)#post_phi <- post_rho - 0.5 * as.numeric(t(as.matrix(epsilon[tt, ])) %*% inv_Omega %*% as.matrix(epsilon[tt, ]))
  }
  return(p)
}
