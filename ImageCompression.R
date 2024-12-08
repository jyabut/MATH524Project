library(imager)
setwd('C:/Users/hello/LinAlg')
getwd()

##### importing data (photos) #####
lbry <- load.image('library.jpg')
gate <- load.image('gate.jpg')
wthr <- load.image('weather.jpg')
yarn <- load.image('yarn.jpg') 

##### Figure 1: creating a chart of all the photos to put in paper #####
layout(matrix(c(1,2,3,4), nrow = 1),widths = c(33,29,22,16))
par(mar=c(1,0.5,8,0.5))

plot(wthr, axes=FALSE)
title(main="Weather\n240 x 165 pixels",cex.main=2)

plot(gate, axes=FALSE)
title(main="Gate\n640 x 480 pixels",cex.main=2)

plot(lbry, axes=FALSE)
title(main="Library\n980 x 979 pixels",cex.main=2)

plot(yarn, axes=FALSE)
title(main="Yarn\n1512 x 2016 pixels",cex.main=2)

dev.off()

##### get dimensions to calculate modes for percentages #####
# the matrices' sizes depend on the smaller dimension
dims <- dim(yarn)
row = dims[1]
col = dims[2]

if (row <= col) {
  modes = row
}else {
  modes = col
}

##### Make the photo data black-and-white #####
graydat = grayscale(yarn)

##### Figure 2: Showing Colored vs Grayscale #####

par(mfrow=c(1,2), mar=c(1,1,4,1))

plot(yarn, xlim = c(0, row), ylim = c(col,0), axes = FALSE,
     main = 'Yarn Photo in Color\n(3 channels)')

plot(graydat, xlim = c(0, row), ylim = c(col,0), axes = FALSE,
     main = 'Yarn Photo in Grayscale\n(1 channel)')

dev.off()

##### SVD analysis of the grayscale data #####
svdDat = svd(graydat)
SVDd = svdDat$d

# calculating the variance of the data
# for the first 20 modes (benchmark)
# which modes contribute the most to the photo?
K = 20
lam = (svdDat$d)^2
lamK=lam[1:K]
lamK
variancePercent = 100*lamK/sum(lam)
round(variancePercent,2)

# Accessing SVD matrices #
U = svdDat$u
D = diag(svdDat$d)
V = svdDat$v

dim(U)
dim(D)
dim(V)

##### Figure 3: plotting scree plot with variance and cumulative variance #####
# allows to put the scree plot and photo next to each other
layout(matrix(c(1,2), nrow = 1),widths = c(3,1))

par(mar=c(4,5,4,5), mgp=c(2.2,0.7,0))

# creating the scree plot
plot(1:K, 100*lamK/sum(lam), ylim=c(0,100), type="o", 
     ylab="", xlab="EOF Mode Number", 
     cex.lab=1.2, cex.axis = 1.2, lwd=2,
     main="Scree Plot of\nthe First 20 Eigenvalues")

# adding axis label and legend for % of variance
mtext("Percentage of Variance [%]",col="black", 
      cex=1.1,side=2,line=3)
legend(3,30, col=c("black"),lty=1, lwd=2.0,
       legend=c("Percentange Variance"),bty="n",
       text.font=1,cex=1.2, text.col="black")

# adding cumulative % variance to the plot
par(new=TRUE)
plot(1:K,cumsum(100*lamK/sum(lam)),
     ylim = c(90,100), type="o",
     col="blue",lwd=2, axes=FALSE,
     xlab="",ylab="")

# add axis, lable, and legend
axis(4, col="blue", col.axis="blue", mgp=c(3,0.7,0), cex.axis=1.2)
mtext("Cumulative Variance [%]",col="blue", 
      cex=1.1,side=4,line=3)
legend(3,94.5, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative Percentage Variance"),bty="n",
       text.font=1,cex=1.2, text.col="blue")

# plotting the photo reconstructed with some of data
# from scree plot calculations
par(mar=c(3,1,4,1))

# using only first 6 modes
k6 = 6
R6 = as.cimg(U[,1:k6]%*%D[1:k6, 1:k6]%*%t(V[,1:k6]))
plot(R6, xlim = c(0, row), ylim = c(col,0), axes=FALSE,
     main = "Photo Reconstructed\nwith 6 Modes")

dev.off()

##### Figure 4: plotting all 4 reconstructions #####
par(mfrow = c(1,4), mar=c(0.5,1,4,1))

### PROCESS OF CODE BELOW ###
# grabbing the modes
# reconstruct image from the modes
# plot reconstructed image
# add title to the plot

k100 = modes # 100% recon
R100 = as.cimg(U[,1:k100]%*%D[1:k100, 1:k100]%*%t(V[, 1:k100]))
plot(R100, xlim = c(0, row), ylim = c(col,0), axes=FALSE)
title(main = "100% - 1512 modes",cex.main = 3)

k50 = round(modes/2) # grabbing 50% of modes
R50 = as.cimg(U[,1:k50]%*%D[1:k50, 1:k50]%*%t(V[, 1:k50]))
plot(R50, xlim = c(0, row), ylim = c(col,0), axes=FALSE)
title(main = "50% - 756 modes", cex.main = 3)

k10 = round(modes/10) # grabbing 10% of modes
R10 = as.cimg(U[,1:k10]%*%D[1:k10, 1:k10]%*%t(V[, 1:k10]))
plot(R10, xlim = c(0, row), ylim = c(col,0), axes=FALSE)
title(main = "10% - 151 modes", cex.main = 3)

k5 = round(modes/20) # grabbing 5% of modes
R5 = as.cimg(U[,1:k5]%*%D[1:k5, 1:k5]%*%t(V[, 1:k5]))
plot(R5, xlim = c(0, row), ylim = c(col,0), axes=FALSE)
title(main = "5% - 76 modes", cex.main = 3)

dev.off()
