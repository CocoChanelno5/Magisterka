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
                 m_prior = matrix(c(3.5 ; 9.5), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 1000, S0 = 100, S_rho = 10000, S0_rho = 2000, Y = Y, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_PL_GDP_", format(Sys.time(), "%b%d"), ".RData"))
######################### PARAMETRY DLA PL STOPA BEZROBOCIA  ##########
N <- n_regions
theta0 <- list(rho = 0.5,
               mu_1 = rep(-0.2, N),
               mu_0 = rep(-1.3, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.4,
                 m_prior = matrix(c(-1.3 ; -0.2), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 1000, S0 = 100, S_rho = 10000, S0_rho = 2000, Y = Y, W = W)
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
                 delta_prior = 0.4,
                 m_prior = matrix(c(2.3,4), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 1000, S0 = 100, S_rho = 10000, S0_rho = 2000, Y = Y, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_USA_GDP_", format(Sys.time(), "%b%d"), ".RData"))

######################### PARAMETRY DLA USA STOPA BEZROBOCIA  ##########
N <- n_states
theta0 <- list(rho = 0.5,
               mu_1 = rep(0.3, N),
               mu_0 = rep(-0.2, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.4,
                 m_prior = matrix(c(-0.2 ; 0.3), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 1000, S0 = 100, S_rho = 10000, S0_rho = 2000, Y = Y, W = W)
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
posterior.all <- posterior_a
load(paste0("posterior", format(Sys.time(), "%a %b %d %X %Y"), ".RData"))
PL_regions
USA_states
#rm(posterior, Y, cc, end, start, y_names_estim, yy, yyy)

########### PRIORS for illustration
attach(hyperpar0)
sigma_domain <- seq(from = 0, to = max(posterior.all[[1]][,34:49]), by = 0.01)
sigma_prior <- dinvgamma(sigma_domain, shape = v_prior/2, scale = delta_prior/2)
m1_domain <- seq(from = min(posterior.all[[1]][,2:17]), to = max(posterior.all[[1]][,2:17]), by = 0.01)
m0_domain <- seq(from = min(posterior.all[[1]][,18:33]), to = max(posterior.all[[1]][,18:33]), by = 0.01)
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

id_pl<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
                    sheet="PL_regiony")
id_usal<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
                  sheet="USA_reg")
country<-id_pl
regions_ordered <- data.frame(code = colnames(Y), v.order = 1:73, 
                                  names = country$Name,
                                  old_code = country$ID,
                                  stringsAsFactors = FALSE)

library(rgdal)
map <- merge(x = PL_map, y = regions_ordered, by.x = "ID", by.y = "old_code")
map <- map[order(map$v.order), ]

map <- merge(x = USA_map, y = regions_ordered, by.x = "ID", by.y = "old_code")
map <- map[order(map$v.order), ]

########### ILLUSTRATE POSTERIORS (TOTAL) ##############
N <- ncol(Y)
setwd("~/Desktop/Magisterka/Master_git/output")

#m1+m0
png(file = "m1m0.png", width = 1700, height = 1200)
par(mfrow = c(6, 3))
starting.point <- 1
starting.point.2 <- N
for (pp in 1:N) {
  hist(posterior.all[[1]][,starting.point + pp], freq = FALSE, main = map$names[pp], 
       xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
       xlim = c(min(m_domain), max(m_domain)), ylim = c(0,1.5), cex.main = 3, cex.axis = 3)
  hist(posterior.all[[1]][,starting.point.2 + pp], freq = FALSE, main = map$names[pp], 
       xlab = NULL, ylab = NULL, nclass = 20, col = rgb(1, 0, 0, 0.5, maxColorValue = 1),
       xlim = c(min(m_domain), max(m_domain)), ylim = c(0,1.5), cex.main = 3, cex.axis = 3, add = TRUE)
  lines(x=m_domain, y=m1_prior, lwd = 2, col = "grey")
  lines(x=m_domain, y=m0_prior, lwd = 2, col = "pink")
  legend(x="topleft", legend = c("m1 a priori", "m1 a posteriori", "m0 a priori", "m0 a posteriori"), 
         fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1), "pink", rgb(1, 0, 0, 0.5, maxColorValue = 1)), 
         bty = "n", cex = 2)
}
dev.off()

#p11+p00
png(file = "p11p00.png", width = 1700, height = 1200)
par(mfrow = c(4, 4))
starting.point <- 65
starting.point.2 <- 49
for (pp in 1:N) {
  hist(posterior.all[[1]][,starting.point + pp], freq = FALSE, main = map$names[pp], 
       xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
       xlim = c(min(p_domain), max(p_domain)), ylim = c(0, 8), cex.main = 3, cex.axis = 3)
  hist(posterior.all[[1]][,starting.point.2 + pp], freq = FALSE, main = map$names[pp], 
       xlab = NULL, ylab = NULL, nclass = 20, col = rgb(1, 0, 0, 0.5, maxColorValue = 1),
       xlim = c(min(p_domain), max(p_domain)), ylim = c(0, 8), add = TRUE, cex.main = 3, cex.axis = 3)
  lines(x=p_domain, y=p11_prior, lwd = 2, col = "grey")
  lines(x=p_domain, y=p00_prior, lwd = 2, col = "pink")
  legend(x="topleft", legend = c("p11 a priori", "p11 a posteriori", "p00 a priori", "p00 a posteriori"), 
         fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1), "pink", rgb(1, 0, 0, 0.5, maxColorValue = 1)), 
         bty = "n", cex = 2)
}
dev.off()

#rho
png(file = "rho.png", width = 400, height = 400)
hist(posterior.all[[1]][,1], freq = FALSE, main = "Polska", 
     xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
     xlim = c(lowerbound_rho2, 1), ylim = c(0, 15), cex.main = 2, cex.axis = 2)
lines(x = rho_domain, y = rho_prior, lwd = 2, col = "grey")
legend(x = "topleft", legend = c("rho a priori", "rho a posteriori"), 
       fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1)), 
       bty = "n", cex = 1.5)
dev.off()

############# TABLES #######################
rho.sum <- matrix(NA, nrow = 7, ncol = 4)
colnames(rho.sum) <- c("post mean", "post SD", "HPDI 95 L", "HPDI 95 U")
y_names_PL <- c("OGӣEM", "przetw?rstwo przemys?owe (C)", "budownictwo (F)", "gospodarka wodna (E)", "handel (G)", "transport (H)", "gospodarka nieruchomo?ciami (L)")
rownames(rho.sum) <- y_names_PL
for (yyy in 1:1) {      #tu zmieni?!!!! jak si? b??d wyja?ni
  post <- posterior.all[[yyy]]
  post.sum <- matrix(NA, nrow = 4, ncol = ncol(post))
  post.sum[1,] <- colMeans(post)
  post.sum[2,] <- vapply(post, 2, FUN = sd)
  post.sum[3,] <- vapply(post, 2, FUN = quantile, probs = c(0.025))
  post.sum[4,] <- vapply(post, 2, FUN = quantile, probs = c(0.975))
  post2 <- cbind(t(post.sum[,18:33]), 
                 t(post.sum[, 2:17]),
                 t(post.sum[,50:65]),
                 t(post.sum[,66:81]),
                 t(post.sum[,34:49]))
  rownames(post2) <- map@data$names
  colnames(post2) <- paste0(rep(c("m0", "m1", "p00", "p01", "sigma"), each = 4), " ", rep(c("post mean", "post SD", "HPDI 95 L", "HPDI 95 U"), 5))
  post2 <- round(post2,2)
  write.table(post2, file = paste0(y_names[yyy], "_results.csv"), sep = ";", dec = ",")
  rho.sum[yyy,] <- post.sum[,1]

  theta_posterior_means <- list(rho = post.sum[1,1],
                                mu_1 = as.vector(post2[,5]),
                                mu_0 = as.vector(post2[,1]),
                                omega_d = as.vector(post2[,17]),
                                p_00 = as.vector(post2[,9]),
                                p_11 = as.vector(post2[,13]))
  p_Hamilton <- Hamilton_filter(Y[[yyy]], theta_posterior_means, W)
  p_Hamilton <- p_Hamilton$p_1
  
  png(file = paste0("Hamilton_", yyy, ".png"), width = 1800, height = 1000)
  par(mfrow = c(4, 4))
  starting.point <- 1
  starting.point.2 <- 17
  for (pp in 1:N) {
    plot(x=c(2007:2018), y=p_Hamilton[,pp],type="l",lwd=2,xlab="",
         ylab="p-stwo ekspansji",main=map$names[pp],
         cex.axis=2,cex.main=3,cex.lab=2)
  }
  dev.off()
  
  impulse <- theta_posterior_means$mu_1 - theta_posterior_means$mu_0
  pal <- colorRampPalette(c("white", "black"), bias = 1)
  for (pp in 1:N) {
    impulse2 <- as.matrix(rep(0,16))
    impulse2[pp] <- impulse[pp]
    effect <- solve(diag(N) - theta_posterior_means$rho * W) %*% impulse2
    map@data$response <- as.vector(effect)
    spplot(map, zcol = "response", colorkey = TRUE, col.regions = pal(100), cuts = 99,
           par.settings = list(axis.line = list(col =  'transparent')),
           main = map$names[pp])
  }
  
}
write.table(rho.sum, file = "rho_results.csv", sep = ";", dec = ",")





