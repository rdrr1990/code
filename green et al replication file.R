### Replication code for "Testing the Accuracy of Regression Discontinuity Analysis 
### Using Experimental Benchmarks" by Donald P. Green, Terence Y. Leong, Holger L. Kern,
### Alan S. Gerber, and Christopher W. Larimer

# using R 2.9.0

setwd("Dropbox/krls/gelman/")
rm(list = ls(all = TRUE))


require(sandwich)

source("Green_et_al_polanalysis_2009_optimal_bw.R")
source("Green_et_al_polanalysis_2009_edge.R")
source("Green_et_al_polanalysis_2009_CV_bw.R")
source("Green_et_al_polanalysis_2009_CV_kern 2.R")
source("Green_et_al_polanalysis_2009_poly_global.R")

# c <- 55 # age cutoff; < 55 = treated, >= 55 = control 

# data1<-read.csv(file="Green_et_al_polanalysis_2009.csv",head=TRUE,sep=",");
# data1$id<-as.character(data1$id);

load("Green_et_al_polanalysis_2009_BW5.RData")

# rescale age at election
# data1$ageatelection <- data1$ageatelection - c
c <- 0


data1$ageatelection2 <- data1$ageatelection^2
data1$ageatelection3 <- data1$ageatelection^3
data1$ageatelection4 <- data1$ageatelection^4

data1$ageatelectionN <- data1$ageatelection * (as.numeric(data1$treatmen) -1)
data1$ageatelection2N <- data1$ageatelection^2 * (as.numeric(data1$treatmen) -1)
data1$ageatelection3N <- data1$ageatelection^3 * (as.numeric(data1$treatmen) -1)
data1$ageatelection4N <- data1$ageatelection^4 * (as.numeric(data1$treatmen) -1)

# data1$voted <- as.numeric(data1$voted) - 1
table(data1$voted)  # post-treatment turnout



###################
# col 1 -- baseline
###################

out1a <- lm(voted ~ treatmen, data = data1)
summary(out1a)
c(summary(out1a)$coeff[2,1], sqrt(diag(vcovHC(out1a, type = "HC3")))[2])


out1b <- lm(voted ~ treatmen + ageatelection + ageatelection2 + 
              ageatelectionN + ageatelection2N, data = data1)
summary(out1b)
c(summary(out1b)$coeff[2,1], sqrt(diag(vcovHC(out1b, type = "HC3")))[2])


out1c <- lm(voted ~ treatmen + ageatelection + ageatelection2 + ageatelection3 +
              ageatelectionN + ageatelection2N + ageatelection3N,
            data = data1)
summary(out1c)
c(summary(out1c)$coeff[2,1], sqrt(diag(vcovHC(out1c, type = "HC3")))[2])


out1d <- lm(voted ~ treatmen + ageatelection + ageatelection2 + ageatelection3 +
              ageatelection4 + ageatelectionN + ageatelection2N +
              ageatelection3N + ageatelection4N,
            data = data1)
summary(out1d)
c(summary(out1d)$coeff[2,1], sqrt(diag(vcovHC(out1d, type = "HC3")))[2])



##################################
# col 2 -- RDD (restricted sample)
##################################
# restrict sample to observed units in observational study
sel <-  data1$ageatelection >= 0 & data1$treatmen == "control" | 
  data1$ageatelection < 0 & data1$treatmen == "neighbors"
table(sel)  # number of cases dropped in faux RDD analysis

data2 <- data1[sel,]
dim(data2)


# some housekeeping...
data2$below <- data2$ageatelection < 0

data2$ageatelectionN <- data2$ageatelection * (as.numeric(data2$below))
data2$ageatelection2N <- data2$ageatelection^2 * (as.numeric(data2$treatmen))
data2$ageatelection3N <- data2$ageatelection^3 * (as.numeric(data2$treatmen))
data2$ageatelection4N <- data2$ageatelection^4 * (as.numeric(data2$treatmen))



out2a <- lm(voted ~ below + ageatelection + ageatelectionN,
            data = data2)
summary(out2a)
c(summary(out2a)$coeff[2,1], sqrt(diag(vcovHC(out2a, type = "HC3")))[2])


out2b <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelectionN +
              ageatelection2N,
            data = data2)
summary(out2b)
c(summary(out2b)$coeff[2,1], sqrt(diag(vcovHC(out2b, type = "HC3")))[2])


out2c <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelectionN + ageatelection2N + ageatelection3N,
            data = data2)
summary(out2c)
c(summary(out2c)$coeff[2,1], sqrt(diag(vcovHC(out2c, type = "HC3")))[2])


out2d <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelection4 + ageatelectionN + ageatelection2N +
              ageatelection3N + ageatelection4N,
            data = data2)
summary(out2d)
c(summary(out2d)$coeff[2,1], sqrt(diag(vcovHC(out2d, type = "HC3")))[2])



#####################################################################
# col 3 -- RDD (restricted sample) [age within 20 years of threshold]
#####################################################################
sel <-  abs(data2$ageatelection) < 20
table(sel)  # number of cases dropped outside age window

data3 <- data2[sel,]
dim(data3)



out3a <- lm(voted ~ below + ageatelection + ageatelectionN,
            data = data3)
summary(out3a)
c(summary(out3a)$coeff[2,1], sqrt(diag(vcovHC(out3a, type = "HC3")))[2])


out3b <- lm(voted ~ below + ageatelection + ageatelection2 +
              ageatelectionN + ageatelection2N,
            data = data3)
summary(out3b)
c(summary(out3b)$coeff[2,1], sqrt(diag(vcovHC(out3b, type = "HC3")))[2])


out3c <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelectionN + ageatelection2N + ageatelection3N,
            data = data3)
summary(out3c)
c(summary(out3c)$coeff[2,1], sqrt(diag(vcovHC(out3c, type = "HC3")))[2])


out3d <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelection4 + ageatelectionN + ageatelection2N +
              ageatelection3N + ageatelection4N,
            data = data3)
summary(out3d)
c(summary(out3d)$coeff[2,1], sqrt(diag(vcovHC(out3d, type = "HC3")))[2])



#####################################################################
# col 4 -- RDD (restricted sample) [age within 10 years of threshold]
#####################################################################
sel <-  abs(data3$ageatelection) < 10
table(sel)  # number of cases dropped outside age window

data4 <- data3[sel,]
dim(data4)



out4a <- lm(voted ~ below + ageatelection + ageatelectionN,
            data = data4)
summary(out4a)
c(summary(out4a)$coeff[2,1], sqrt(diag(vcovHC(out4a, type = "HC3")))[2])


out4b <- lm(voted ~ below + ageatelection + ageatelection2 +
              ageatelectionN + ageatelection2N,
            data = data4)
summary(out4b)
c(summary(out4b)$coeff[2,1], sqrt(diag(vcovHC(out4b, type = "HC3")))[2])


out4c <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelectionN + ageatelection2N + ageatelection3N,
            data = data4)
summary(out4c)
c(summary(out4c)$coeff[2,1], sqrt(diag(vcovHC(out4c, type = "HC3")))[2])


out4d <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelection4 + ageatelectionN + ageatelection2N +
              ageatelection3N + ageatelection4N,
            data = data4)
summary(out4d)
c(summary(out4d)$coeff[2,1], sqrt(diag(vcovHC(out4d, type = "HC3")))[2])



#####################################################################
# col 5 -- RDD (restricted sample) [age within 5 years of threshold]
#####################################################################
sel <-  abs(data4$ageatelection) < 5
table(sel)  # number of cases dropped outside age window

data5 <- data4[sel,]
dim(data5)



out5a <- lm(voted ~ below + ageatelection + ageatelectionN,
            data = data5)
summary(out5a)
c(summary(out5a)$coeff[2,1], sqrt(diag(vcovHC(out5a, type = "HC3")))[2])


out5b <- lm(voted ~ below + ageatelection + ageatelection2 +
              ageatelectionN + ageatelection2N,
            data = data5)
summary(out5b)
c(summary(out5b)$coeff[2,1], sqrt(diag(vcovHC(out5b, type = "HC3")))[2])


out5c <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelectionN + ageatelection2N + ageatelection3N,
            data = data5)
summary(out5c)
c(summary(out5c)$coeff[2,1], sqrt(diag(vcovHC(out5c, type = "HC3")))[2])


out5d <- lm(voted ~ below + ageatelection + ageatelection2 + ageatelection3 +
              ageatelection4 + ageatelectionN + ageatelection2N +
              ageatelection3N + ageatelection4N,
            data = data5)
summary(out5d)
c(summary(out5d)$coeff[2,1], sqrt(diag(vcovHC(out5d, type = "HC3")))[2])



######################
# IK OPTIMAL BANDWIDTH
######################
# multiply estimate with (-1) because the treatment happens on the left side of the cutoff


# rescale the data to have range 2
data9 <- data2

rescale.factor <-
  ((range(data9$ageatelection)[2] - range(data9$ageatelection)[1]) * .5)

data9$ageatelection <- data9$ageatelection / rescale.factor
stopifnot(range(data9$ageatelection)[2] - range(data9$ageatelection)[1] == 2)


hhat.opt <- optimal.bw(Y = data9$voted, X = data9$ageatelection, c = 0, reg = T)
c(hhat.opt, hhat.opt * rescale.factor) # h on new scale and original scale (years)

out.opt <- edge(Y = data9$voted, X = data9$ageatelection, c = 0, h = hhat.opt) 
out.opt



# bootstrapped SEs
bs <- 10000
boot.coeff <- matrix(NA,bs,2)

for(i in 1:bs)  {
  
  sel <- sample(1:nrow(data9), replace = T)
  boot.data <- data9[sel,]
  
  boot.coeff[i,1] <- optimal.bw(  Y = boot.data$voted,
                                  X = boot.data$ageatelection,
                                  c = 0, reg = T)
  temp <- edge(   Y = boot.data$voted, X = boot.data$ageatelection,
                  c = 0, h = boot.coeff[i,1])
  boot.coeff[i,2] <- temp$theta
  
  
  if(i/100 == round(i/100))   { cat(i,"\n") }
  
}

temp <- boot.coeff[,1] * rescale.factor
mean(temp)
sd(temp)

mean(boot.coeff[,2])
sd(boot.coeff[,2])



#############################
# LM CROSSVALIDATED BANDWIDTH
#############################
# multiply estimate with (-1) because the treatment happens on the left side of the cutoff
h <- seq(from = .01, to = .5, by = .01)

hhat.CV <- CV.bw(   Y = data9$voted, X = data9$ageatelection, c = 0, delta = .1,
                    h = h, kty = "edge")

save.image(file = "Green_et_al_polanalysis_2009_BW5.RData")


postscript( file = "Green_et_al_polanalysis_2009_BW5.eps", height = 4, family = "sans", pointsize = 2,
            horizontal = FALSE)
par(mfrow = c(1,1), cex = 1.2, cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2, cex.sub = 1.2)

plot(h*rescale.factor, hhat.CV, type = "l", xlab = "Bandwidth (years)",
     ylab = "Cross-validation function", col.axis = "grey40", col.main = "grey40",
     col.lab = "grey40", col.sub = "grey40", fg = "grey40")
abline(v = rescale.factor*h[which(hhat.CV == min(hhat.CV))][1],
       col = "green2", lwd = 1.3)

dev.off()


out.CV <- edge(Y = data9$voted, X = data9$ageatelection, c = 0,
               h = h[which(hhat.CV == min(hhat.CV))[1]])
out.CV

h[which(hhat.CV == min(hhat.CV))[1]] * rescale.factor   # bandwidth



##################################
# RESULTS FOR DIFFERENT BANDWIDTHS
##################################
# multiply estimate with (-1) because the treatment happens on the left side of the cutoff

h <- seq(from = .01, to = .5, by = .001)
out.bw <- matrix(NA,length(h),2)

for(i in 1:length(h))   {
  
  temp <- edge(Y = data9$voted, X = data9$ageatelection, c = 0, h = h[i]) 
  
  out.bw[i,1] <- temp$theta*-1
  out.bw[i,2] <- temp$se
  
}


postscript( file = "Green_et_al_polanalysis_2009_figureBW.eps", height = 4, family = "sans", pointsize = 2,
            horizontal = FALSE)

par(mfrow = c(1,1), cex = 1.2, cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2, cex.sub = 1.2)

plot(h[4:477]*rescale.factor, out.bw[4:477,1], type = "l",
     xlab = "Bandwidth (years)", ylab = "Treatment effect", ylim = c(-.05,.30),
     lwd = 2, col.axis = "grey40", col.main = "grey40", col.lab = "grey40",
     col.sub = "grey40", fg = "grey40")

lines(h[1:477]*rescale.factor, out.bw[1:477,1] + (out.bw[1:477,2] * qnorm(.975)),
      lty = 3, lwd = 1.5)
lines(h[1:477]*rescale.factor, out.bw[1:477,1] - (out.bw[1:477,2] * qnorm(.975)),
      lty = 3, lwd = 1.5)
abline(h = summary(out1d)$coeff[2,1], lwd = 1, lty = 2)
abline(v = hhat.opt * rescale.factor, col = "red", lwd = 1.3)

dev.off()



##########
# FIGURE 1
##########
data10 <- data1
data10$age <- round(data10$ageatelection)


sel <-  data10$age >= 0 & data10$treatmen == "control"
dataObsRight <- data10[sel,]

sel <-  data10$age <  0 & data10$treatmen == "control"
dataCounterLeft <- data10[sel,]

sel <-  data10$age >= 0 & data10$treatmen == "neighbors"
dataCounterRight <- data10[sel,]

sel <-  data10$age <  0 & data10$treatmen == "neighbors"
dataObsLeft <- data10[sel,]

# consistency check
stopifnot(nrow(dataObsRight) + nrow(dataCounterLeft) + nrow(dataCounterRight) +
            nrow(dataObsLeft) == nrow(data10))


agerange <- -33:49
figdata <- matrix(NA,length(agerange),8)

for(i in 1:length(agerange))    {
  
  figdata[i,1] <- sum(dataCounterLeft$age == agerange[i])
  figdata[i,2] <- sum(dataCounterRight$age == agerange[i])
  figdata[i,3] <- sum(dataObsLeft$age == agerange[i])
  figdata[i,4] <- sum(dataObsRight$age == agerange[i])
  
  figdata[i,5] <- mean(dataCounterLeft$voted[dataCounterLeft$age == agerange[i]])
  figdata[i,6] <- mean(dataCounterRight$voted[dataCounterRight$age == agerange[i]])
  figdata[i,7] <- mean(dataObsLeft$voted[dataObsLeft$age == agerange[i]])
  figdata[i,8] <- mean(dataObsRight$voted[dataObsRight$age == agerange[i]])
  
}


figdataLeft <- as.data.frame(cbind(figdata[1:33,7], -33:-1, (-33:-1)^2, (-33:-1)^3, (-33:-1)^4))
out.lmLeft <- lm(V1 ~ V2 + V3 + V4 + V5, data = figdataLeft)

figdataRight <- as.data.frame(cbind(figdata[34:83,8], 0:49, (0:49)^2, (0:49)^3, (0:49)^4))
out.lmRight <- lm(V1 ~ V2 + V3 + V4 + V5, data = figdataRight)

fitdataLeft <- predict.lm(out.lmLeft)
fitdataRight <- predict.lm(out.lmRight)


circleareaCounterLeft <- figdata[1:33,1]
circleareaCounterRight <- figdata[34:83,2]
circleareaObsLeft <- figdata[1:33,3]
circleareaObsRight <- figdata[34:83,4]

radiiCounterLeft <- sqrt(circleareaCounterLeft / pi)
radiiCounterRight <- sqrt(circleareaCounterRight / pi)
radiiObsLeft <- sqrt(circleareaObsLeft / pi)
radiiObsRight <- sqrt(circleareaObsRight / pi)

radiiCounterRight[radiiCounterRight == 0] <- 1  # 4 ages without observations


plotsymbolx <- c(-33:49, -33:49)
plotsymboly <- c(   figdata[1:33,7], figdata[34:83,8],
                    figdata[1:33,5], figdata[34:83,6])  # first plot observed then counterfactual
plotsymboly[is.na(plotsymboly)] <- -2   # 4 ages without observations plotted outside the clipping region


color <- c(rep("red",83), rep("blue",83))
circles <- c(radiiObsLeft,radiiObsRight,radiiCounterLeft,radiiCounterRight)

# consistency check
stopifnot(max(circles)^2 / min(circles)^2 == 
            max(c(circleareaCounterLeft, circleareaCounterRight, circleareaObsLeft, circleareaObsRight)))



postscript( file = "Green_et_al_polanalysis_2009_figure1.eps", height = 4, family = "sans", pointsize = 2,
            horizontal = FALSE)

par(mfrow = c(1,1), cex = 1.2, cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2, cex.sub = 1.2)

plot(plotsymbolx, plotsymboly, ylim = c(0,.7), xlim = range(agerange),
     col.axis = "grey40", col.main = "grey40", col.lab = "grey40", col.sub = "grey40",
     xlab = "Age (centered at 55 years)", ylab = "Turnout", fg = "grey40", type = "n")

axis(side = 1, at = c(-30,-20,-10,0,10,20,30,40,50), labels = T, col.axis = "grey40",
     col.tick = "grey40", col = "grey40")

symbols(plotsymbolx, plotsymboly, circles = circles, inches = .04, fg = color, bg = color,
        add = T)

lines(-33:-1, fitdataLeft, lwd = 1.5)
lines(0:40, fitdataRight[1:41], lwd = 1.5)

dev.off()