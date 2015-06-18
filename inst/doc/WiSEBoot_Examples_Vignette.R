## ----sel0, eval=TRUE, echo=FALSE, message=FALSE--------------------------
library(WiSEBoot)

## ----sel1, eval=TRUE, echo=TRUE------------------------------------------
data(SimulatedSmoothSeries)
dim(SimulatedSmoothSeries)
SimulatedSmoothSeries[1:3, ]

## ----sel2, eval=TRUE, echo=FALSE, out.width='12cm', out.height='8cm'-----
plot(seq(1, 2^10), SimulatedSmoothSeries[,4], ty="l", 
    main=bquote(paste("Smooth Series with Threshold ", J[0],"=3")), 
    xlab="Time", ylab="Observation")

## ----sel3, eval=TRUE, echo=FALSE-----------------------------------------
data(SimulatedSNR0.5Series); data(SimulatedSNR1.0Series); data(SimulatedSNR1.5Series); data(SimulatedSNR2.0Series)
par(mfrow=c(2,2))
plot(seq(1, 2^10), SimulatedSNR0.5Series[,4], ty="l", 
    main=bquote(paste("SNR=0.5 Data, Threshold ", J[0],"=3")), 
    xlab="Time", ylab="Observation", col="gray")
lines(seq(1, 2^10), SimulatedSmoothSeries[,4], col="red", lwd=2)
plot(seq(1, 2^10), SimulatedSNR1.0Series[,4], ty="l", 
    main=bquote(paste("SNR=1.0 Data, Threshold ", J[0],"=3")), 
    xlab="Time", ylab="Observation", col="gray")
lines(seq(1, 2^10), SimulatedSmoothSeries[,4], col="red", lwd=2)
plot(seq(1, 2^10), SimulatedSNR1.5Series[,4], ty="l", 
    main=bquote(paste("SNR=1.5 Data, Threshold ", J[0],"=3")), 
    xlab="Time", ylab="Observation", col="gray")
lines(seq(1, 2^10), SimulatedSmoothSeries[,4], col="red", lwd=2)
plot(seq(1, 2^10), SimulatedSNR2.0Series[,4], ty="l", 
    main=bquote(paste("SNR=2.0 Data, Threshold ", J[0],"=3")), 
    xlab="Time", ylab="Observation", col="gray")
lines(seq(1, 2^10), SimulatedSmoothSeries[,4], col="red", lwd=2)

## ----sel4, echo=TRUE, eval=TRUE, out.width='1.1\\textwidth', out.height='17cm'----
smoothPlot <- smoothTimeSeries(SimulatedSNR1.0Series[ ,4], plot="all")

## ----sel5, echo=TRUE, eval=TRUE------------------------------------------
set.seed(1414)
SNR10Boot <- WiSEBoot(SimulatedSNR1.0Series[ ,4], R=10)
SNR10Boot$MSECriteria

## ----sel6, echo=TRUE, eval=TRUE, out.width='15cm', out.height='7cm', fig.width='15cm', fig.height='7cm'----
par(mfrow=c(1,2))
boxplot(SNR10Boot$BootIntercept, 
        main=expression(paste("R=10 Bootstrap Estimates of ", gamma[0])),
        ylab=expression(hat(gamma)[0][b]))
boxplot(SNR10Boot$BootSlope, 
        main=expression(paste("R=10 Bootstrap Estimates of ", gamma[1])),
        ylab=expression(hat(gamma)[1][b]))

## ----sel7, echo=TRUE, eval=TRUE, out.width='15cm', out.height='7cm'------
par(mfrow=c(1,2))
boxplot(SNR10Boot$BootWavelet[,3], 
        main=expression(paste("R=10 Boot. Est. of level=1, coef. 1, ", gamma)),
        ylab=expression(hat(gamma)[b]))
boxplot(SNR10Boot$BootWavelet[,4], 
        main=expression(paste("R=10 Boot. Est. of level=1, coef. 2, ", gamma)),
        ylab=expression(hat(gamma)[b]))

## ----hyp1, echo=TRUE, eval=TRUE------------------------------------------
data(CM20N20S60E)
CM20N20S60E[1:3,]

## ----hyp2, echo=TRUE, eval=TRUE, out.width='15cm', out.height='15cm'-----
par(mfrow=c(2,1))
plot.ts(CM20N20S60E[,1], main="AIRS", ylab="Obs. Climate")
plot.ts(CM20N20S60E[,10], main="MIROC5, run 5", ylab="Model Climate")

## ----hyp3, echo=TRUE, eval=TRUE------------------------------------------
pad60E <- padMatrix(CM20N20S60E)
dim(pad60E$xPad)

## ----hyp4, ehco=TRUE, eval=TRUE------------------------------------------
hypObj <- WiSEHypothesisTest(pad60E$xPad[,1], pad60E$xPad[,10], R=10, J0=5, 
                             XParam=pad60E$linearParam[,1], 
                             YParam=pad60E$linearParam[,10])

