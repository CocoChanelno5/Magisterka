############## COMMENTS ####################

### REGIMES
# 1 - EXPANSION
# 0 - RECESSION
set.seed(42)

setwd("~/Desktop/Magisterka/Master_git/Master_git_2")
source("MC_MG_HF_functions.R")

# order of regions
order_idusa<-colnames(W_USA)
order_idpl<-colnames(W_PL)

# the loop through all files makes the posterior estimation
posterior_a <- list()
load("~/Desktop/Magisterka/Master_git/Master_git_2/dane/danedane.RData")

# preparing matryx Y for GDP in Poland
dftemp<-PL_GDP_ch
Y<-dftemp%>% select(c(ID,Period, Value)) %>% pivot_wider(names_from = ID,values_from = Value)
Y_lag <- as.matrix(Y[1:17,-1])
Y <- as.matrix(Y[2:18,-1])
W<-W_PL
table(is.na(Y))
## preparing matryx Y for unemployment rate in Poland
dftemp<-PL_UE_ch
Y<-dftemp%>% select(c(Name, Period, Value)) %>% pivot_wider(names_from = Name, values_from = Value)
Y_lag <- as.matrix(Y[1:109,-1])
Y <- as.matrix(Y[-1,-1])
W<-W_PL
table(is.na(Y))

# preparing matryx Y for GDP in USA
dftemp<-USA_GDP_ch
Y<-dftemp%>% select(c(ID,Period, Value)) %>% pivot_wider(names_from = ID,values_from = Value)
Y_lag <- as.matrix(Y[1:,-1])
Y <- as.matrix(Y[2:,-1])
W<-W_USA
table(is.na(Y))

## preparing matryx Y for unemployment rate in USA
dftemp<-USA_UE_ch
Y<-dftemp%>% select(c(Name, Date, Value)) %>% pivot_wider(names_from = Name,values_from = Value)
Y_lag <- as.matrix(Y[1:265,-1])
Y <- as.matrix(Y[2:266,-1])
W<-W_USA
table(is.na(Y))

######################### PARAMETRY DLA PL GDP  ##########
N <- n_regions
theta0 <- list(rho = 0.5,
               phi = 0.5,
               mu_1 = rep(12, N),
               mu_0 = rep(-1, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(7, 3, 2, 8), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 0.4,
                 m_prior = matrix(c(3.5,15), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 100, S0_rho = 10, S_phi = 100, S0_phi = 10, Y = Y, Yl = Y_lag, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/Master_git_2/post_simul/posterior2_PL_GDP_", format(Sys.time(), "%b%d"), ".RData"))
######################### PARAMETRY DLA PL STOPA BEZROBOCIA  ##########
N <- n_regions
theta0 <- list(rho = 0.5,
               phi = 0.5,
               mu_1 = rep(-1.3, N),
               mu_0 = rep(0.2, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 100,
                 m_prior = matrix(c(0.2,-1.3), nrow = 2),
                 M_prior = diag(2))

start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, 
                                S_rho = 1000, S0_rho = 10, S_phi = 100, S0_phi = 10, Y = Y, Yl = Y_lag, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/Master_git_2/post_simul/posterior_PL_UE_", format(Sys.time(), "%b%d"), ".RData"))



######################### PARAMETRY DLA USA GDP  ##########
N <- n_states
theta0 <- list(rho = 0.5,
               phi = 0.5,
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
save.image(paste0("~/Desktop/Magisterka/Master_git/Master_git_2/post_simul/posterior_USA_GDP_", format(Sys.time(), "%b%d"), ".RData"))

######################### PARAMETRY DLA USA STOPA BEZROBOCIA  ##########
N <- n_states
theta0 <- list(rho = 0.5,
               phi = 0.5,
               mu_1 = rep(-0.2, N),
               mu_0 = rep(0.3, N),
               omega_d = rep(1, N), #VARIANCES (already squared)
               p_00 = rep(0.8, N),
               p_11 = rep(0.8, N))

hyperpar0 = list(alpha_prior = matrix(c(8, 2, 1, 9), nrow = 2, byrow = TRUE),
                 v_prior = 6,
                 delta_prior = 8,
                 m_prior = matrix(c(0.5,-1), nrow = 2),
                 M_prior = diag(2))


start <- Sys.time()
posterior_a <- sample_posterior(initial = theta0, hyperpar = hyperpar0, S = 5000, S0 = 1000, S_rho = 1000, S0_rho = 100, Y = Y, Yl = Y_lag, W = W)
end <- Sys.time()
print(end - start)
save.image(paste0("~/Desktop/Magisterka/Master_git/Master_git_2/post_simul/posterior_USA_UE_", format(Sys.time(), "%b%d"), ".RData"))

############ POSTERIOR SIMULATION #################################
setwd("~/post_simul/")

posterior <- posterior_a
load(paste0("posterior", format(Sys.time(), "%a %b %d %X %Y"), ".RData"))
n<-n_regions
n<-n_states

########### PRIORS for illustration
attach(hyperpar0)
sigma_domain <- seq(from = 0, to = max(posterior[,(2*n+3):(3*n+2)]), by = 0.01)
sigma_prior <- dinvgamma(sigma_domain, shape = v_prior/2, scale = delta_prior/2)
m1_domain <- seq(from = min(posterior[,3:(n+2)]), to = max(posterior[,3:(n+2)]), by = 0.01)
m0_domain <- seq(from = min(posterior[,(n+3):(2*n+2)]), to = max(posterior[,(n+3):(2*n+2)]), by = 0.01)
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

lowerbound_phi <- -1
lowerbound_phi2 <- -0.5
phi_domain <- seq(from = lowerbound_phi, to = 1, by = 0.01)
phi_prior <- rep(1/(1-lowerbound_phi2), length(phi_domain)) #??????

'''id_pl<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
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
'''
########### ILLUSTRATE POSTERIORS (TOTAL) ##############
N <- ncol(Y)
map<-PL_GDP
map@data$names<-colnames(Y)
setwd("~/Desktop/Magisterka/Master_git/Master_git_2/output")
n_col<-4
n_row<-7
m<- n_col*n_row
variable<-'UE'
country<-'PL'
cex<-1
names<-colnames(Y)
N<-length(colnames(Y))
pages<-ceiling(N/m)

### ILLUSTRATE M
for (i in 1:pages){
  page<-i
  #m1+m0
  png(file = paste0("m1m0_",country,variable,"_",page,"_t.png"), width = 8.27, height = 11.69, units ="in",res=300)
  par(mfrow = c(n_row, n_col), family="serif",mar=c(3, 2, 2, 0)+ 0.1,mgp=c(1.5,0.2,0))
  for (pp in 1:m) {
    pp<-pp+(page-1)*m
    if (pp<=N){    
      hist(v_m1[,pp], freq = FALSE, main = colnames(Y)[pp], border=rgb(1, 1, 1, 0, maxColorValue = 1),
           xlab = NULL, ylab = NULL, nclass = 20, col="skyblue4",#col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(m_domain), max(m_domain)), #ylim = c(0,1.5), 
           cex.main = cex, cex.axis = cex/1.2,tck=-0.02)
      hist(v_m0[,pp], freq = FALSE, main = colnames(Y)[pp], border=rgb(1, 1, 1, 0, maxColorValue = 1),
           xlab = NULL, ylab = NULL, nclass = 20, col = rgb(1, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(m_domain), max(m_domain)), #ylim = c(0,1.5), 
           cex.main = cex, cex.axis = cex/1.2, add = TRUE)
      lines(x=m_domain, y=m1_prior, lwd = 2, col = "steelblue4")
      lines(x=m_domain, y=m0_prior, lwd = 2, col = main_colour2)
      legend(x="topleft", legend = c("m1 a priori", "m1 a posteriori", "m0 a priori", "m0 a posteriori"), 
             fill = c("steelblue4", "skyblue4", main_colour2, rgb(1, 0, 0, 0.5, maxColorValue = 1)), 
             bty = "n", cex = cex/1.4)}}
  dev.off()
}

### ILLUSTRATE P
for (i in 1:pages){
  page<-i
  #p11+p00
  png(file = paste0("p1p0_",country,variable,"_",page,"_t.png"), width = 8.27, 
      height = 11.69, units ="in", res=300)
  par(mfrow = c(n_row, n_col),family="serif",mar=c(3, 1, 2, 1)+ 0.1,mgp=c(1.5,0.2,0))
  for (pp in 1:m) {
    pp<-pp+(page-1)*m
    if (pp<=N){
      hist(v_p1[,pp], freq = FALSE, main = names[pp], col="skyblue4",
           xlab = NULL, ylab = NULL, nclass = 10, #col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(p_domain), max(p_domain)), #ylim = c(0, 8), 
           cex.main = cex, cex.axis = cex/1.2,tck=-0.02)
      hist(v_p0[,pp], freq = FALSE, main = names[pp], border=main_colour2,
           xlab = NULL, ylab = NULL, nclass = 10, col = rgb(1, 0, 0, 0.5, maxColorValue = 1),
           xlim = c(min(p_domain), max(p_domain)), #ylim = c(0, 8), 
           add = TRUE, cex.main = cex, cex.axis = cex/1.2)
      lines(x=p_domain, y=p11_prior, lwd = 2, col = "steelblue4")
      lines(x=p_domain, y=p00_prior, lwd = 2, col = main_colour2)
      legend(x="topleft", legend = c("p11 a priori", "p11 a posteriori", "p00 a priori", "p00 a posteriori"), 
             fill = c("steelblue4", "skyblue4", main_colour2, rgb(1, 0, 0, 0.5, maxColorValue = 1)), 
             bty = "n", cex = cex/1.4)
    }}
  dev.off()
}


### ILLUSTRATE RHO
title="Stopa bezrobocia w Polsce"
main_colour <- "navy"
main_colour2<- "deeppink3"
main_colour_phi<-
main_colour_rho<-
# pink2, ppink3, violetred3, navy, blue3
pal <- colorRampPalette(c(main_colour2, main_colour), bias = 1)
library(RColorBrewer)
# display.brewer.all()
# PuBu, Blues,RdPu, PuBuGn
cuts <- 9
my.palette <- brewer.pal(n = cuts, name = "OrRd")
#rho
png(file = paste0("rho&phi_",country,"_",variable,"_t.png"), width = 400, height = 400)

hist(v_rho, freq = FALSE, #main = title, 
     xlab = NULL, ylab = NULL, nclass = 20, col = main_colour_rho,
     #xlim = c(lowerbound_rho2, 1), #ylim = c(0, 15), 
     cex.main = cex, cex.axis = cex/1.7)
hist(v_phi, freq = FALSE, #main = title, 
     xlab = NULL, ylab = NULL, nclass = 20, col = main_colour_phi,
     #xlim = c(lowerbound_rho2, 1), #ylim = c(0, 15), 
     cex.main = cex, cex.axis = cex/1.7)
lines(x = rho_domain, y = rho_prior, lwd = 2, col = main_colour_rho)
lines(x = phi_domain, y = phi_prior, lwd = 2, col = main_colour_phi)
legend(x = "topleft", legend = c("rho a priori", "rho a posteriori","phi a priori", "phi a posteriori"), 
       fill = c(main_colour_rho, rgb(0, 0, 0, 0.5, maxColorValue = 1),main_colour_phi,rgb(0, 0, 1, 0.5, maxColorValue = 1)), 
       bty = "n", cex = cex/2)
dev.off()

############# TABLES #######################
rho.sum <- matrix(NA, nrow = 7, ncol = 4)
colnames(rho.sum) <- c("post mean", "post SD", "HPDI 95 L", "HPDI 95 U")

for (yyy in 1:1) {      #tu zmieni?!!!! jak si? b??d wyja?ni
  post <- posterior[[yyy]]
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
  write.table(post2, file = paste0(y_names[yyy], "_results_t.csv"), sep = ";", dec = ",")
  rho.sum[yyy,] <- post.sum[,1]

  theta_posterior_means <- list(rho = post.sum[1,1],
                                mu_1 = as.vector(post2[,5]),
                                mu_0 = as.vector(post2[,1]),
                                omega_d = as.vector(post2[,17]),
                                p_00 = as.vector(post2[,9]),
                                p_11 = as.vector(post2[,13]))
  p_Hamilton <- Hamilton_filter(Y[[yyy]], theta_posterior_means, W)
  p_Hamilton <- p_Hamilton$p_1
  
  png(file = paste0("Hamilton_", yyy, "_t.png"), width = 1800, height = 1000)
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
write.table(rho.sum, file = "rho_results_t.csv", sep = ";", dec = ",")





