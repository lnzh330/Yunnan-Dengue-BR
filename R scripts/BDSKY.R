# Extract results from BDSKY analysis 
# Install the package by uncommenting the next line and running it
#
# devtools::install_github("laduplessis/bdskytools")
#
# If you cannot install the package, source the R-files in the folder and make sure the 
# packages boa and RColorBrewer are installed

# Load the package (if successfully installed)
library(bdskytools)
library(ggplot2)
library(openxlsx)
library(dplyr)

# Extract the data and HPDs from the logfile
############################################

# Set the working directory to the directory where your log files are stored 
# (on RStudio navigate to Session > Set Working Directory > Choose Directory)
# or change "BDSKY.log" to the path of the BDSKY logfile on your computer
fname <- "BDSKY.log"
lf    <- readLogfile(fname, burnin=0.1)

Re_sky    <- getSkylineSubset(lf, "reproductiveNumber")
Re_hpd    <- getMatrixHPD(Re_sky)
delta_hpd <- getHPD(lf$becomeUninfectiousRate)

timegrid       <- seq(0,7,length.out=40)
Re_gridded     <- gridSkyline(Re_sky,    lf$origin, timegrid)
Re_gridded_hpd <- getMatrixHPD(Re_gridded)

#95% CI

Q2.5<-Re_gridded_hpd[1,]
Q97.5<-Re_gridded_hpd[3,]
ReMean<-Re_gridded_hpd[2,]

Remeanname<-names(ReMean)
Remeanname<-as.numeric(Remeanname)
for (i in 1:40) {Remeanname[i]<-2019.83-Remeanname[i]


}
Rmean<-c()
for (i in 1:40) {Rmean[i]<-ReMean[[i]]


}


q2.5hpd<-c()
for (i in 1:40) {q2.5hpd[i]<-Q2.5[[i]]


}

q97.5hpd<-c()
for (i in 1:40) {q97.5hpd[i]<-Q97.5[[i]]

}

Rdata<-data.frame(Remeanname,Rmean,q2.5hpd,q97.5hpd)
write.xlsx(Rdata,'BDSKYD.xlsx')
