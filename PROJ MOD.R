


###############################################
y.t = DATA[,6:12] ; y.t.all = y.t
N.t = ncol(y.t) ##number of months
n = nrow(y.t) ##number of observation per months



##add some missing observations to illistrate prediction
miss = 1
#holdout.station.id <- seq(1,100,7)
holdout.station.id <- seq(1,100,14)
y.t.holdout <- y.t[holdout.station.id, miss]
y.t[holdout.station.id, miss] <- NA
###############################################

coords = as.matrix(DATA[,c("X", "Y")])
max.d = max(iDist(coords))

##set starting and priors
p <- 3 #number of regression parameters in each month
starting <- list("beta"=rep(0,N.t*p), "phi"=rep(3/(0.5*max.d), N.t),
                 "sigma.sq"=rep(2,N.t), "tau.sq"=rep(1, N.t),
                 "sigma.eta"=diag(rep(0.01, p)))

starting <- list("beta"=rep(c(.5,0,0),N.t), "phi"=rep(3/(0.5*max.d), N.t),
                 "sigma.sq"=rep(.25,N.t), "tau.sq"=rep(.01, N.t),
                 "sigma.eta"=diag(rep(0.01, p)))



tuning <- list("phi"=rep(5, N.t))

priors <- list("beta.0.Norm"=list(rep(0,p), diag(1000,p)),
               "phi.Unif"=list(rep(3/(0.9*max.d), N.t), rep(3/(0.05*max.d), N.t)),
               "sigma.sq.IG"=list(rep(2,N.t), rep(10,N.t)),
               "tau.sq.IG"=list(rep(2,N.t), rep(5,N.t)),
               "sigma.eta.IW"=list(2, diag(0.001,p)))

priors <- list("beta.0.Norm"=list(c(.5,0,0), diag(c(.1,1,1),p)),
               "phi.Unif"=list(rep(3/(0.9*max.d), N.t), rep(3/(0.05*max.d), N.t)),
               "sigma.sq.IG"=list(rep(5,N.t), rep(1,N.t)),
               "tau.sq.IG"=list(rep(10,N.t), rep(1,N.t)),
               "sigma.eta.IW"=list(2, diag(0.001,p)))


##make symbolic model formula statement for each month
mods <- lapply(paste(colnames(y.t),'elev',sep='~'), as.formula)
mods = 

lapply(
c(
"Y_3 ~ DATA$Y_2 + DATA$Y_1",
"Y_4 ~ DATA$Y_3 + DATA$Y_2",
"Y_5 ~ DATA$Y_4 + DATA$Y_3",
"Y_6 ~ DATA$Y_5 + DATA$Y_4",
"Y_7 ~ DATA$Y_6 + DATA$Y_5",
"Y_8 ~ DATA$Y_7 + DATA$Y_6",
"Y_9 ~ DATA$Y_8 + DATA$Y_7"),as.formula)


n.samples <- 5000
m.1 <- spDynLM(mods, data=y.t, coords=coords,
               starting=starting, tuning=tuning, priors=priors, get.fitted =TRUE,
               cov.model="exponential", n.samples=n.samples, n.report=25)



burn.in <- floor(0.75*n.samples)
quant <- function(x){quantile(x, prob=c(0.5, 0.025, 0.975))}

beta <- apply(m.1$p.beta.samples[burn.in:n.samples,], 2, quant)
theta <- apply(m.1$p.theta.samples[burn.in:n.samples,], 2, quant)
sigma.sq <- theta[,grep("sigma.sq", colnames(theta))]
tau.sq <- theta[,grep("tau.sq", colnames(theta))]
phi <- theta[,grep("phi", colnames(theta))]



y.hat <- apply(m.1$p.y.samples[,burn.in:n.samples], 1, quant)
y.hat.med <- matrix(y.hat[1,], ncol=N.t)
y.hat.up <- matrix(y.hat[3,], ncol=N.t)
y.hat.low <- matrix(y.hat[2,], ncol=N.t)

y.obs <- as.vector(as.matrix(y.t[-holdout.station.id, -miss]))
y.obs.hat.med <- as.vector(y.hat.med[-holdout.station.id, -miss])
y.obs.hat.up <- as.vector(y.hat.up[-holdout.station.id, -miss])
y.obs.hat.low <- as.vector(y.hat.low[-holdout.station.id, -miss])

y.ho <- as.matrix(y.t.holdout)
y.ho.hat.med <- as.vector(y.hat.med[holdout.station.id, miss])
y.ho.hat.up <- as.vector(y.hat.up[holdout.station.id, miss])
y.ho.hat.low <- as.vector(y.hat.low[holdout.station.id, miss])

###########################################################################
###########################################################################





###########################################################################
###########################################################################










