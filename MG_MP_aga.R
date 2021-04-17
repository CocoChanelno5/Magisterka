############## COMMENTS ####################

### REGIMES
# 1 - EXPANSION
# 0 - RECESSION
set.seed(42)

setwd("~/Desktop/Magisterka/Master_git")
source("MC_MG_HF_functions.R")

# order of regions
order_idusa<-colnames(W_USA)
order_idpl<-colnames(W_PL)

W_list <- mat2listw(W, style="W")
rm(gamma)

# the loop through all files makes the posterior estimation
posterior_a <- list()
# preparing matryx Y for GDP in Poland
dftemp<-PL_GDP_ch
Y<-dftemp%>% select(c(ID,Period, Value)) %>% pivot_wider(names_from = ID,values_from = Value)
Y <- as.matrix(Y[1:18,-1])
W<-W_PL
table(is.na(Y))
## preparing matryx Y for unemployment rate in Poland
dftemp<-PL_UE_ch
Y<-dftemp%>% select(c(Name,Period, Value)) %>% pivot_wider(names_from = Name,values_from = Value)
Y <- as.matrix(Y[,-1])
W<-W_PL
table(is.na(Y))

# preparing matryx Y for GDP in USA
dftemp<-USA_GDP_ch
Y<-dftemp%>% select(c(ID,Period, Value)) %>% pivot_wider(names_from = ID,values_from = Value)
Y <- as.matrix(Y[,-1])
W<-W_USA
table(is.na(Y))

## preparing matryx Y for unemployment rate in USA
dftemp<-USA_UE_ch
Y<-dftemp%>% select(c(Name,Period, Value)) %>% pivot_wider(names_from = Name,values_from = Value)
Y <- as.matrix(Y[,-1])
W<-W_USA
table(is.na(Y))

############## INITIATE PARAMETERS AND SET HYPERPARAMETERS ##############
N <- n_regions  # n_states

theta0 <- list(rho = 0.7,
               mu_1 = rep(3, N),
               mu_0 = rep(-3, N),
               omega_d = rep(5, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.4,
                 m_prior = matrix(c(-3, 3), nrow = 2),
                 M_prior = diag(2))
##KONDO 
N <- n_regions  # PL
# N <- n_states  # USA

theta0 <- list(rho = 0.5,
               mu_1 = rep(6, N),
               mu_0 = rep(-6, N),
               omega_d = rep(5, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(6, 4, 3, 7), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.8,
                 m_prior = matrix(c(-6, 6), nrow = 2),
                 M_prior = diag(2))
######################### PARAMETRY DLA PL GDP  ##########
N <- n_regions
theta0 <- list(rho = 0.5,
               mu_1 = rep(9.5, N),
               mu_0 = rep(3.5, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.4,
                 m_prior = matrix(c(3.5,9.5), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 1000, S0_rho = 100, Y = Y, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_PL_GDP_", format(Sys.time(), "%b%d"), ".RData"))
######################### PARAMETRY DLA PL STOPA BEZROBOCIA  ##########
N <- n_regions
theta0 <- list(rho = 0.5,
               mu_1 = rep(-1.3, N),
               mu_0 = rep(-0.2, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 10,
                 m_prior = matrix(c(0.2,-1.3), nrow = 2),
                 M_prior = diag(2))

start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 1000, S0_rho = 100, Y = Y, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_PL_UE_", format(Sys.time(), "%b%d"), ".RData"))



######################### PARAMETRY DLA USA GDP  ##########
N <- n_states
theta0 <- list(rho = 0.5,
               mu_1 = rep(4, N),
               mu_0 = rep(2.3, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 2,  ## changed from 0.4
                 m_prior = matrix(c(2.3,4), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 1000, S0_rho = 100, Y = Y, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_USA_GDP_", format(Sys.time(), "%b%d"), ".RData"))

######################### PARAMETRY DLA USA STOPA BEZROBOCIA  ##########
N <- n_states
theta0 <- list(rho = 0.5,
               mu_1 = rep(-0.2, N),
               mu_0 = rep(0.3, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.4,
                 m_prior = matrix(c(0.3,-0.2), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 1000, S0_rho = 100, Y = Y, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_USA_UE_", format(Sys.time(), "%b%d"), ".RData"))

############ POSTERIOR SIMULATION #################################
posterior_a <- list()
start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 10000, S0_rho = 2000, Y = Y, W = W)
end <- Sys.time()
print(end - start)

save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_USA_GDP", format(Sys.time(), "%b%d"), ".RData"))
#save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_PL_GDP", format(Sys.time(), "%b%d"), ".RData"))
#save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_USA_UE", format(Sys.time(), "%b%d"), ".RData"))
#save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_PL_UE", format(Sys.time(), "%b%d"), ".RData"))

########### IF SIMULATION RUN BEFORE, START HERE ###################
setwd("~/post_simul/")
posterior <- posterior_a
load(paste0("posterior", format(Sys.time(), "%a %b %d %X %Y"), ".RData"))
n<-n_regions
#n<-n_states
#rm(posterior, Y, cc, end, start, y_names_estim, yy, yyy)

########### PRIORS for illustration
setwd("~/Desktop/Magisterka/Master_git/output")
attach(hyperpar0)
sigma_domain <- seq(from = 0, to = max(posterior[,(2*n+2):(3*n+1)]), by = 0.01)
sigma_prior <- dinvgamma(sigma_domain, shape = v_prior/2, scale = delta_prior/2)
m1_domain <- seq(from = min(posterior[,2:(n+1)]), to = max(posterior[,2:(n+1)]), by = 0.01)
m0_domain <- seq(from = min(posterior[,(n+2):(2*n+1)]), to = max(posterior[,(n+2):(2*n+1)]), by = 0.01)
m_domain <- seq(from = min(c(m0_domain, m1_domain)), to = max(c(m1_domain, m0_domain)), by = 0.01)
m1_prior <- dnorm(m_domain, mean = m_prior[2], sd = M_prior[2,2]^0.5)
m0_prior <- dnorm(m_domain, mean = m_prior[1], sd = M_prior[1,1]^0.5)
p_domain <- seq(from = 0, to = 1, by = 0.01)
p11_prior <- dbeta(p_domain, alpha_prior[2,2], alpha_prior[2,1])
p00_prior <- dbeta(p_domain, alpha_prior[1,1], alpha_prior[1,2])

lowerbound_rho <- 1/min(eigen(W)$values)
lowerbound_rho2 <- -0.5
rho_domain <- seq(from = lowerbound_rho2, to = 1, by = 0.01)
rho_prior <- rep(1/(1-lowerbound_rho2), length(rho_domain))

v_m1<-posterior[,2:(n+1)]
v_m0<-posterior[,(n+2):(2*n+1)]
v_omega<-posterior[,(2*n+2):(3*n+1)]
v_p0<-posterior[,(3*n+2):(4*n+1)]
v_p1<-posterior[,(4*n+2):(5*n+1)]
v_rho<-posterior[,1]

########### ILLUSTRATE POSTERIORS (TOTAL) ##############
  
  variable<-'UE'
  country<-'PL'
  cex<-1
  n_col<-4
  n_row<-7
  m<- n_col*n_row
  names<-colnames(Y)
  N<-length(colnames(Y))
  pages<-ceiling(N/m)
  
### ILLUSTRATE M
for (i in 1:pages){
  page<-i
#m1+m0
  png(file = paste0("m1m0_",country,variable,"_",page,".png"), width = 8.27, height = 11.69, units ="in",res=300)
  par(mfrow = c(n_row, n_col),family="mono",mar=c(3, 1, 2, 1)+ 0.1)
  for (pp in 1:m) {
    pp<-pp+(page-1)*m
    if (pp<=N){    
      hist(v_m1[,pp], freq = FALSE, main = colnames(Y)[pp], 
          xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
          xlim = c(min(m_domain), max(m_domain)), #ylim = c(0,1.5), 
          cex.main = cex, cex.axis = cex/1.5)
      hist(v_m0[,pp], freq = FALSE, main = colnames(Y)[pp], 
           xlab = NULL, ylab = NULL, nclass = 20, col = rgb(1, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(m_domain), max(m_domain)), #ylim = c(0,1.5), 
           cex.main = cex, cex.axis = cex/1.5, add = TRUE)
      lines(x=m_domain, y=m1_prior, lwd = 2, col = "grey")
      lines(x=m_domain, y=m0_prior, lwd = 2, col = main_colour2)
      legend(x="topleft", legend = c("m1 a priori", "m1 a posteriori", "m0 a priori", "m0 a posteriori"), 
             fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1), main_colour2, rgb(1, 0, 0, 0.5, maxColorValue = 1)), 
             bty = "n", cex = cex/1.5)}}
  dev.off()
}
page<-0
for (i in seq(1,N,m)){
  page<-page+1
  i=1
  df<-cbind(Name=names[i:(i+m-1)],t(v_p1[,i:(i+m-1)]))
  draw<-ggplot(df,aes(group=Name)) +
    geom_histogram(color="black", fill="white")+
    geom_line(color=main_colour) +
    theme_ipsum() +
    ggtitle(paste0("Poziom zmiennej ",variable," w "))+
    facet_wrap(~Name, ncol=n_col, nrow=n_row,scales = "free_y")+
    theme(strip.text = element_text(family = f),plot.title=element_text(hjust=1, vjust=0.5, face='bold',size = 15,family = f),
          axis.text = element_text(size = 0.05, angle=50,family = f))+
    scale_y_continuous(name="Wartość opisywanej zmiennej")
  plot(draw)
  ggsave(paste0("p1p0_",country,variable,"_",page,".png"), draw, width = 8.27, height = 11.69, units = "in")
}


  ### ILLUSTRATE P
for (i in 1:pages){
  page<-i
  #p11+p00
  png(file = paste0("p1p0_",country,variable,"_",page,".png"), width = 8.27, 
        height = 11.69, units ="in", res=300)
  par(mfrow = c(n_row, n_col),family="mono",mar=c(3, 1, 2, 1)+ 0.1)
  for (pp in 1:m) {
    pp<-pp+(page-1)*m
    if (pp<=N){
    hist(v_p1[,pp], freq = FALSE, main = names[pp], 
           xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(p_domain), max(p_domain)), #ylim = c(0, 8), 
           cex.main = cex, cex.axis = cex/1.5)
    hist(v_p0[,pp], freq = FALSE, main = names[pp], 
           xlab = NULL, ylab = NULL, nclass = 20, col = rgb(1, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(p_domain), max(p_domain)), #ylim = c(0, 8), 
           add = TRUE, cex.main = cex, cex.axis = cex/1.5)
    lines(x=p_domain, y=p11_prior, lwd = 2, col = "grey")
    lines(x=p_domain, y=p00_prior, lwd = 2, col = main_colour2)
    legend(x="topleft", legend = c("p11 a priori", "p11 a posteriori", "p00 a priori", "p00 a posteriori"), 
             fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1), main_colour2, rgb(1, 0, 0, 0.5, maxColorValue = 1)), 
             bty = "n", cex = cex/1.5)
      }}
  dev.off()
}

### ILLUSTRATE RHO
title="Stopa bezrobocia w Polsce"
#rho
png(file = paste0("rho_",country,"_",variable,".png"), width = 400, height = 400)
hist(v_rho, freq = FALSE, main = title, 
     xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
     #xlim = c(lowerbound_rho2, 1), #ylim = c(0, 15), 
     cex.main = cex, cex.axis = cex/2)
lines(x = rho_domain, y = rho_prior, lwd = 2, col = "grey")
legend(x = "topleft", legend = c("rho a priori", "rho a posteriori"), 
       fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1)), 
       bty = "n", cex = cex/2)
dev.off()

############# TABLES #######################
rho.sum <- matrix(NA, nrow = 4, ncol = 4)
colnames(rho.sum) <- c("post mean", "post SD", "HPDI 95 L", "HPDI 95 U")
zestaw <- c("Zestaw I - USA GDP", "Zestaw II - USA UE", "Zestaw III - PL GDP", "Zestaw IV - PL UE")
rownames(rho.sum) <- zestaw 

post <- posterior
post.sum <- matrix(NA, nrow = 4, ncol = ncol(posterior))
  
v_m1<-posterior[,2:(n+1)]
v_m0<-posterior[,(n+2):(2*n+1)]
v_omega<-posterior[,(2*n+2):(3*n+1)]
v_p0<-posterior[,(3*n+2):(4*n+1)]
v_p1<-posterior[,(4*n+2):(5*n+1)]
post.sum[1,] <- colMeans(post)
post.sum[2,] <- vapply(post, 2, FUN = sd)
post.sum[3,] <- vapply(post, 2, FUN = quantile, probs = c(0.025))
post.sum[4,] <- vapply(post, 2, FUN = quantile, probs = c(0.975))
post2 <- cbind(t(post.sum[,(n+2):(2*n+1)]), 
              t(post.sum[, 2:(n+1)]),
              t(post.sum[,(3*n+2):(4*n+1)]),
              t(post.sum[,(4*n+2):(5*n+1)]),
              t(post.sum[,(2*n+2):(3*n+1)]))
rownames(post2) <- names
colnames(post2) <- paste0(rep(c("m0", "m1", "p00", "p01", "sigma"), each = 4), " ", rep(c("post mean", "post SD", "HPDI 95 L", "HPDI 95 U"), 5))
post2 <- round(post2,2)
write.table(post2, file = paste0(country,variable, "_results.csv"), sep = ";", dec = ",")
rho.sum <- post.sum[,1]

theta_posterior_means <- list(rho = post.sum[1,1],
                                mu_1 = as.vector(post2[,5]),
                                mu_0 = as.vector(post2[,1]),
                                omega_d = as.vector(post2[,17]),
                                p_00 = as.vector(post2[,9]),
                                p_11 = as.vector(post2[,13]))
p_Hamilton <- Hamilton_filter(Y, theta_posterior_means, W)
p_Hamilton <- p_Hamilton$p_1
  
png(file = paste0("Hamilton_", country,variable, ".png"), width = 1800, height = 1000)
par(mfrow = c(4,4))
dates<-unique(PL_UE_ch$Period)
for (pp in 1:N) {
  plot(x=1:110, y=p_Hamilton[,pp],type="l",lwd=2,xlab="",
        ylab="p-stwo ekspansji",main=names[pp],
        cex.axis=cex,cex.main=cex,cex.lab=cex)
}
dev.off()
  
impulse <- theta_posterior_means$mu_1 - theta_posterior_means$mu_0
pal <- colorRampPalette(c("white", "black"), bias = 1)
map<-PL_map
for (pp in 1:N) {
  impulse2 <- as.matrix(rep(0,N))
  impulse2[pp] <- impulse[pp]
  effect <- solve(diag(N) - theta_posterior_means$rho * W) %*% impulse2
  map@data$response <- as.vector(effect)
  sp<-spplot(map, zcol = "response", colorkey = TRUE, col.regions = pal(100), cuts = 99,
           par.settings = list(axis.line = list(col =  'transparent')),
           main = names[pp])
}
write.table(rho.sum, file = "rho_results.csv", sep = ";", dec = ",")
