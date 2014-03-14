### R code from vignette source 'NclasstestVignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Multiclasstesting
###################################################
options(keep.source = TRUE, width = 60)
Multiclasstesting <- packageDescription("Multiclasstesting")


###################################################
### code chunk number 2: NclasstestVignette.Rnw:103-107
###################################################
library(Multiclasstesting)
GS <- cbind(c(0,1),c(0,0),c(1,1))
T <-    cbind(c(1,1),c(1,0),c(1,1))
Nclasstest(T,GS)


###################################################
### code chunk number 3: NclasstestVignette.Rnw:113-117
###################################################
library( Multiclasstesting)
GS <- cbind(c(0,-1,1),c(0,1,0),c(1,0,1))
T <-    cbind(c(1,-1,1),c(0,1,-1),c(0,1,1))
Nclasstest(T,GS)


