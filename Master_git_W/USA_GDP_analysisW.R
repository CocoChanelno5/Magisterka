############## COMMENTS ####################
### REGIMES
# 1 - EXPANSION
# 0 - RECESSION
set.seed(42)

# preparing matryx Y for GDP in USA
dftemp<-USA_GDP_ch
Y<-dftemp%>% select(c(ID,Period, Value)) %>% pivot_wider(names_from = ID,values_from = Value)
Y <- as.matrix(Y[,-1])
W<-W_USA
table(is.na(Y))

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
save.image(paste0("~/Desktop/Magisterka/Master_git/post_simul/posterior_USA_GDP_W_", format(Sys.time(), "%b%d"), ".RData"))

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


########### PRIORS for illustration

load("/Users/agnieszka/Desktop/Magisterka/Master_git/Master_git_W/post_simul/posterior_USA_GDP_W_cze04.rdata")
library(RColorBrewer)
library(classInt)
path3<-"~/Desktop/Magisterka/Master_git/Master_git_W/output/"
posterior <- posterior_a$simul
n<-n_states
setwd("~/Desktop/Magisterka/Master_git/Master_git_W/output")
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
main_colour <- "navy"
main_colour2<- "deeppink3"
  variable<-'GDP_W'
  country<-'USA'
  cex<-1
  n_col<-4
  n_row<-7
  m<- n_col*n_row
  names<-USA_states
  colnames(Y)<-names
  rownames(Y)<-names
  N<-length(colnames(Y))
  pages<-ceiling(N/m)
  
### ILLUSTRATE M
for (i in 1:pages){
  page<-i
#m1+m0
  png(file = paste0("m1m0_",country,variable,"_",page,".png"), width = 8.27, height = 11.69, units ="in",res=300)
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
  png(file = paste0("p1p0_",country,variable,"_",page,".png"), width = 8.27, 
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
#rho
png(file = paste0("rho_",country,"_",variable,".png"), width = 400, height = 400)

hist(v_rho, freq = FALSE, main = t"", 
     xlab = NULL, ylab = NULL, nclass = 20, col = rgb(0, 0, 0, 0.5, maxColorValue = 1),
     #xlim = c(lowerbound_rho2, 1), #ylim = c(0, 15), 
     cex.main = cex, cex.axis = cex/1.7)
lines(x = rho_domain, y = rho_prior, lwd = 2, col = "grey")
legend(x = "topleft", legend = c("rho a priori", "rho a posteriori"), 
       fill = c("grey", rgb(0, 0, 0, 0.5, maxColorValue = 1)), 
       bty = "n", cex = cex/2)
dev.off()

############# TABLES #######################
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
colnames(post2) <- paste0(rep(c("m0", "m1", "p00", "p11", "sigma"), each = 4), " ", rep(c("post mean", "post SD", "HPDI 95 L", "HPDI 95 U"), 5))
post2 <- round(post2,3)
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


for (i in 1:pages){
  page<-i
  png(file = paste0("Hamilton_", country,variable,"_",page,".png"), width = 8.27, height = 11.69, units ="in",res=300)
  par(mfrow = c(n_row, n_col), family="serif",mar=c(2, 2, 2, 0.5)+ 0.1,mgp=c(1,0,0))
  dates<-unique(USA_GDP_ch$Period)
  for (pp in 1:m) {
    pp<-pp+(page-1)*m
    if (pp<=N){ 
        plot(x=1:length(dates), y=p_Hamilton[,pp],type="l",lwd=2,xlab="",
        ylab="p-stwo ekspansji",main=names[pp],col="navy",
        cex.axis=cex/2,cex.main=cex,cex.lab=cex/2,xaxt="none",yaxt="none")
        axis(2, cex.axis=cex/1.5, tck=-0.015)
        axis(1, seq(1,60,4), cex.axis=cex/1.5, srt = 45,tck=-0.015, #col.axis="red",
             labels=c("2006","2007","2008","2009","2010","2011","2012",
                      "2013","2014","2015","2016","2017","2018","2019","2020"))
      }}
  dev.off()
}


############### TWORZENEI RYSUNKÓW IMPULSU - indywidualnie dla każdego zestawu
nclr<-5
e<-c()
impulse <- theta_posterior_means$mu_1 - theta_posterior_means$mu_0
for (pp in 1:N) {
  impulse2 <- as.matrix(rep(0,N))
  impulse2[pp] <- impulse[pp]
  effect <- solve(diag(N) - theta_posterior_means$rho * W) %*% impulse2
  e<-cbind(e,effect)}
effect_mean<-mean(e)
vec_e<-c(e)
breaks_qt <- classIntervals(vec_e,2, n = nclr, style = "equal")
r <- breaks_qt$brks 
# choice of folder to keep maps
n_col<-4
n_row<-5
m<- n_col*n_row
pages<-ceiling(N/m)
pal<-c()
draw_impulse2<-function(map,N,n,theta,W,ef,r,legend,i){
  #pp<-1
  #theta<-theta_posterior_means
  nclr<-5
  #pal <- colorRampPalette(c("white", "black"), bias = 1)
  impulse <- theta$mu_1 - theta$mu_0
  pal <- brewer.pal(5, "Oranges")
  #pal<-rev(pal)
  #pal[9:10] <- brewer.pal(3, "Oranges")[2:3]
  impulse2 <- as.matrix(rep(0,N))
  impulse2[i] <- impulse[i]
  map@data$response <- as.vector(ef[,i])
  map@data$bracket <- cut(map@data$response, r)
  spplot(map, "bracket", lwd=0.1, col.regions=pal,colorkey=legend,
         par.settings = list(axis.line = list(col =  'transparent')),
         main = list(label=n[i],cex=0.8,fontfamily="serif"))
}

draw_impulse_empty<-function(map,N,n,theta,W,ef,r,i){
  #pp<-1
  #theta<-theta_posterior_means
  nclr<-5
  #pal <- colorRampPalette(c("white", "black"), bias = 1)
  impulse <- theta$mu_1 - theta$mu_0
  pal <- brewer.pal(5, "Oranges")
  impulse2 <- as.matrix(rep(0,N))
  impulse2[i] <- impulse[i]
  map@data$response <- as.vector(ef[,i])
  map@data$bracket <- cut(map@data$response, r)
  s<-spplot(map, "bracket", lwd=0, col.regions=pal,
         colorkey=list(space='left',height = 2,width =4),
         par.settings = list(axis.line = list(col =  'transparent')),
         main = list(label='',cex=0.8,fontfamily="serif"))
  library(grid)
  library(lattice)
  args <- s$legend$left$args$key
  ## Prepare list of arguments needed by `legend=` argument (as described in ?xyplot)
  legendArgs <- list(fun = draw.colorkey,
                     args = list(key = args),
                     corner = c(0.5,.5),
                     fontfamily="serif")
  ## Call spplot() again, this time passing in to legend the arguments
  ## needed to print a color key
  spplot(USA_map, "ID", colorkey =FALSE,
         panel=function(x, y, ...){
           panel.rect(xleft=180000, ybottom=330000,
                      xright=181000, ytop=330500, alpha=1)},lwd=0, par.settings = list(axis.line = list(col =  'transparent')),
         legend = list(inside = legendArgs))
}

png(file = paste0("legend_effect_",country,"_",variable,".png"), width = 3, height = 4, units ="in",res=300)
draw_impulse_empty(USA_map,48,names,theta_posterior_means,W,e,r,1)
dev.off()

setwd(path3)
## UNEMPLOYMENT RATE IN POLAND
for (page in 1:pages){
  if (m+(page-1)*(m-1)>N){
    dif<- m - (N-(m+(page-1)*(m-1))+1)
    temp<-seq(1+(page-1)*(m-1),N)
    png(file = paste0("effect_",country,variable,"_",page,".png"), width = 8.27, height = 11.69, units ="in",res=300)
    plots = lapply(temp, function(.x) draw_impulse2(USA_map,48,names,theta_posterior_means, W, e, r, FALSE, .x))
    p<-marrangeGrob(plots, nrow=n_row, ncol=n_col)
    print(p)
    dev.off()
  }else{
    temp<-seq(1+(page-1)*(m-1), m+(page-1)*(m-1)-1)
    png(file = paste0("effect_",country,variable,"_",page,".png"), width = 8.27, height = 11.69, units ="in",res=300)
    plots = lapply(temp, function(.x) draw_impulse2(USA_map,48,names,theta_posterior_means,W,e,r,FALSE,.x))
    do.call(grid.arrange,plots)
    dev.off()
  }
}

write.table(rho.sum, file = paste0("rho_results_",country,"_",variable,".csv"), sep = ";", dec = ",")
