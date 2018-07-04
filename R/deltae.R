


## Adaptation of Bob Rudis's `swatch:::deltaE2000` Function.
##
## Only substantive change is to remove the colorspace dependency, directly
## accepting Lab matrices instead of the S4 LAB objects from colorspace.
## CIEDE2000
##
## Gaurav Sharma & Maynard P Baalthazar
##
## @param Labstd numeric[, 1:3] with L, a, b coordinates
## @param Labsample numeric[, 1:3] with L, a, b coordinates

deltaE2000_1 <- function(Labstd, Labsample, kl=1, kc=1, kh=1) {
  lstd <- Labstd[, 1]
  astd <- Labstd[, 2]
  bstd <- Labstd[, 3] 
  Cabstd <- sqrt(astd^2+bstd^2)
  lsample <- Labsample[,1]
  asample <- Labsample[,2]
  bsample <- Labsample[,3]
  Cabsample <- sqrt(asample^2 + bsample^2)
  Cabarithmean <- (Cabstd + Cabsample)/2
  G <- 0.5* ( 1 - sqrt( (Cabarithmean^7)/(Cabarithmean^7 + 25^7)))
  apstd <- (1+G)*astd
  apsample <- (1+G)*asample
  apstd <- (1+G)*astd
  apsample <- (1+G)*asample
  Cpsample <- sqrt(apsample^2+bsample^2)
  Cpstd <- sqrt(apstd^2+bstd^2)
  Cpprod <- (Cpsample*Cpstd)
  zcidx <- which(Cpprod == 0)
  hpstd <- atan2(bstd,apstd)
  hpstd <- hpstd+2*pi*(hpstd < 0)
  hpstd[which( (abs(apstd)+abs(bstd))== 0) ] <- 0
  hpsample <-atan2(bsample,apsample)
  hpsample <- hpsample+2*pi*(hpsample < 0)
  hpsample[which( (abs(apsample)+abs(bsample))==0) ] <- 0
  dL <- lsample-lstd
  dC <- Cpsample-Cpstd
  dhp <- hpsample-hpstd
  dhp <-dhp - 2*pi* (dhp > pi )
  dhp <- dhp + 2*pi* (dhp < (-pi) )
  dhp[zcidx ] <- 0
  dH <- 2*sqrt(Cpprod)*sin(dhp/2)
  Lp <- (lsample+lstd)/2
  Cp <- (Cpstd+Cpsample)/2
  hp <- (hpstd+hpsample)/2
  hp <- hp - ( abs(hpstd-hpsample) > pi ) *pi
  hp <- hp + (hp < 0) *2*pi
  hp[zcidx] <- hpsample[zcidx]+hpstd[zcidx]
  Lpm502 <- (Lp-50)^2
  Sl <-1 + 0.015*Lpm502/sqrt(20+Lpm502)
  Sc <- 1+0.045*Cp
  T <- 1-0.17*cos(hp-pi/6 ) + 0.24*cos(2*hp) + 0.32*cos(3*hp+pi/30) -0.20*cos(4*hp-63*pi/180)
  Sh <- 1 + 0.015*Cp*T
  delthetarad <- (30*pi/180)*exp(- ( (180/pi*hp-275)/25)^2)
  Rc <- 2*sqrt((Cp^7)/(Cp^7 + 25^7))
  RT <- -sin(2*delthetarad)*Rc
  klSl <- kl*Sl
  kcSc <- kc*Sc
  khSh <- kh*Sh
  de00 <- sqrt( (dL/klSl)^2 + (dC/kcSc)^2 + (dH/khSh)^2 + RT*(dC/kcSc)*(dH/khSh) )

  return(as.numeric(de00))
}


