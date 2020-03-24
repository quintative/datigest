library(data.table)
library(TDA) # Topological Data Analysis
library(rgl) # 3d plotting
library(ggplot2)
library(dplyr)

rm(list = ls())

##############################################################################
# Noisy 2D circles
##############################################################################

par.1.dens <- 1000 # Points per circle
par.1.r2   <- 1.3  # Radius of 2nd circle
par.1.dd2  <- 3    # Euclidian distance of centers
par.1.nn   <- 100  # No. of noise
par.1.nc   <- c(-2, par.1.dd2 / sqrt(2) + par.1.r2 + 1)

smp.1.2d <- data.table(circleUnif(n = par.1.dens, r = 1), clust = "A")
smp.1.2d <- rbind(smp.1.2d, data.table(circleUnif(n = par.1.dens, r = par.1.r2) + par.1.dd2 / sqrt(2), clust = "B"))
smp.1.2d <- rbind(smp.1.2d, data.table(x1 = runif(par.1.nn, par.1.nc[1], par.1.nc[2]), 
                                       x2 = runif(par.1.nn, par.1.nc[1], par.1.nc[2]), 
                                       clust = "noise"))

colnames(smp.1.2d) <- c("x", "y", "clust")
rm(par.1.dens, par.1.r2, par.1.dd2, par.1.nn, par.1.nc)
#-----------------------------------------------------------------------------

smp.1.2d %>% ggplot(aes(x = x, y = y, color = clust)) + geom_point()

##############################################################################
# Noisy 3D spheres
##############################################################################

par.2.dens <- 10000 # Points per sphere
par.2.r2   <- 1.3   # Radius of second sphere
par.2.dd2  <- 1.8     # Euclidian distance of centers
par.2.nn   <- 1000  # No. of noise
par.2.nc   <- c(-2, par.2.dd2 / sqrt(3) + par.2.r2 + 1)

smp.2.3d <- data.table(sphereUnif(n = par.2.dens, d = 2, r = 1), clust = "1")
smp.2.3d <- rbind(smp.2.3d, data.table(sphereUnif(n = par.2.dens, d = 2, r = par.2.r2) + par.2.dd2 / sqrt(3), clust = "2"))
smp.2.3d <- rbind(smp.2.3d, data.table(V1 = runif(par.2.nn, par.2.nc[1], par.2.nc[2]), 
                                       V2 = runif(par.2.nn, par.2.nc[1], par.2.nc[2]), 
                                       V3 = runif(par.2.nn, par.2.nc[1], par.2.nc[2]), 
                                       clust = "3"))

colnames(smp.2.3d) <- c("x", "y", "z", "clust")
rm(par.2.dens, par.2.r2, par.2.dd2, par.2.nn, par.2.nc)
#-----------------------------------------------------------------------------

plot3d(smp.2.3d, col = smp.2.3d$clust)

##############################################################################
# Noisy 3D Toruses
##############################################################################

par.3.dens <- 10000 # Points per torus
par.3.r2   <- 1.3   # Radius of second torus (center to center of tube)
par.3.dd2  <- 2.5   # Euclidian distance of centers
par.3.nn   <- 1000  # No. of noise
par.3.nc   <- c(-2, par.3.dd2 / sqrt(3) + par.3.r2 + 1)

smp.3.3d <- data.table(torusUnif(n = par.3.dens, a = 1, c = 2), clust = "1")
smp.3.3d <- rbind(smp.3.3d, data.table(torusUnif(n = par.3.dens, a = 1, c = par.3.r2) + par.3.dd2 / sqrt(3), clust = "2"))
smp.3.3d <- rbind(smp.3.3d, data.table(x = runif(par.3.nn, par.3.nc[1], par.3.nc[2]), 
                                       y = runif(par.3.nn, par.3.nc[1], par.3.nc[2]), 
                                       z = runif(par.3.nn, par.3.nc[1], par.3.nc[2]), 
                                       clust = "3"))

colnames(smp.3.3d) <- c("x", "y", "z", "clust")
rm(par.3.dens, par.3.r2, par.3.dd2, par.3.nn, par.3.nc)
#-----------------------------------------------------------------------------

plot3d(smp.3.3d, col = smp.3.3d$clust)


##############################################################################
# Creating a 2-d grid for estimating the distances and other matrices
##############################################################################

# Defining sample matrix
# m.smp <- as.matrix(circleUnif(400, 1))
m.smp <- as.matrix(smp.1.2d[, .(x, y)])

# Defining grid
v.seq.x <- seq(-2, 5, 0.025)
v.seq.y <- seq(-2, 5, 0.025)
d.grd   <- as.data.table(expand.grid(v.seq.x, v.seq.y))


##############################################################################
# Distance matrix
##############################################################################

v.d <- distFct(X = m.smp, Grid = d.grd)

par(mfrow = c(1, 2))
plot(m.smp)
persp(x = v.seq.x, y = v.seq.y, 
      z = matrix(v.d, nrow = length(v.seq.x), ncol = length(v.seq.y)), 
      main = "Distance function",
      xlab = "", ylab = "", zlab = "", expand = 3, col = "red", border = NA, scale = F,
      theta = -20, phi = 35, ltheta = 50, shade = 0.5)
par(mfrow = c(1, 1))

##############################################################################
# Distance to measure
##############################################################################

v.dtm <- dtm(X = m.smp, Grid = d.grd, m0 = 0.1)

par(mfrow = c(1, 2))
plot(m.smp)
persp(x = v.seq.x, y = v.seq.y, 
      z = matrix(v.dtm, nrow = length(v.seq.x), ncol = length(v.seq.y)), 
      main = "Distance to measure",
      xlab = "", ylab = "", zlab = "", expand = 3, col = "red", border = NA, scale = F,
      theta = -20, phi = 35, ltheta = 50, shade = 0.5)
par(mfrow = c(1, 1))

##############################################################################
# k nearest neighbor density estimator
##############################################################################

v.knn <- knnDE(X = m.smp, Grid = d.grd, k = 60)

par(mfrow = c(1, 2))
plot(m.smp)
persp(x = v.seq.x, y = v.seq.y, 
      z = matrix(v.knn, nrow = length(v.seq.x), ncol = length(v.seq.y)), 
      main = "k nearest neighbor density",
      xlab = "", ylab = "", zlab = "", expand = 3, col = "red", border = NA, scale = F,
      theta = -20, phi = 35, ltheta = 50, shade = 0.5)
par(mfrow = c(1, 1))


##############################################################################
# Kernel density estimator
##############################################################################

v.kde <- kde(X = m.smp, Grid = d.grd, h = 0.3)

par(mfrow = c(1, 2))
plot(m.smp)
persp(x = v.seq.x, y = v.seq.y, 
      z = matrix(v.kde, nrow = length(v.seq.x), ncol = length(v.seq.y)), 
      main = "Kernel density estimator",
      xlab = "", ylab = "", zlab = "", expand = 3, col = "red", border = NA, scale = F,
      theta = -20, phi = 35, ltheta = 50, shade = 0.5)
par(mfrow = c(1, 1))

##############################################################################
# 
##############################################################################





pers.diag <- gridDiag(X = m.smp, FUN = kde, lim = cbind(c(min(smp.2.3d$x), max(smp.2.3d$x)),
                                                        c(min(smp.2.3d$y), max(smp.2.3d$y)),
                                                        c(min(smp.2.3d$z), max(smp.2.3d$z))), 
                      by = 0.1, sublevel = F, library = "Dionysus", printProgress = T, h = 0.3)


plot(x = pers.diag[["diagram"]])


