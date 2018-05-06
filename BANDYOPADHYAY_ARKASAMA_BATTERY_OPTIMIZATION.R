#This code optimizes the operation of a 25 MW (50 MWh) energy storage system/battery by buying energy when ERCOT market prices 
#are low and selling when market prices are high
#Set the working directory
dir = getwd()
setwd(dir)

#defining functions used later in the code
norm_vec <- function(x){return(sqrt(sum(x^2)))} 
whichmedian <- function(x) which.min(abs(x - median(x)))


#Importing real time prices from 2014-2016
RTM_Price_raw <- read.csv('LZ_AEN_RTM.csv', header = TRUE, stringsAsFactors = FALSE)
RTM_Price_raw_2016 <- read.csv('LZ_AEN_2016.csv', header=FALSE, stringsAsFactors = FALSE)

#Dividing the prices into different years
year_price <- RTM_Price_raw [,8]
price <- RTM_Price_raw[,2]
RTM_2014_raw <- price [which(year_price == 2014)]
RTM_2015_raw <- price [which(year_price == 2015)]
RTM_2016_raw <- as.numeric(as.character(RTM_Price_raw_2016[,1]))
RTM_2016_raw <- rev(RTM_2016_raw)

#combining with the first price point of the next year: this is necessary for interpolation
RTM_2014 <- c(RTM_2014_raw, RTM_2015_raw[1])
RTM_2015 <- c(RTM_2015_raw, 16.12)
RTM_2016 <- c(RTM_2016_raw, 21.83 )

#Interpolating to get from 15 min to 5 min data

#2014
x <- c(1: length(RTM_2014))
xout <- seq(from = 1, to = length(RTM_2014), length.out=105121)
interp_2014 <- approx(x, RTM_2014, xout, method = "linear")
RTM_2014_5 <-as.data.frame(interp_2014[2])
RTM_2014_5 <-RTM_2014_5[-nrow(RTM_2014_5),]

#2015
x <- c(1: length(RTM_2015))
xout <- seq(from = 1, to = length(RTM_2015), length.out=105121)
interp_2015 <- approx(x, RTM_2015, xout, method = "linear")
RTM_2015_5 <-as.data.frame(interp_2015[2])
RTM_2015_5 <-RTM_2015_5[-nrow(RTM_2015_5),]

#2016
x <- c(1: length(RTM_2016))
xout <- seq(from = 1, to = length(RTM_2016), length.out=105409)
interp_2016 <- approx(x, RTM_2016, xout, method = "linear")
RTM_2016_5 <-as.data.frame(interp_2016[2])
RTM_2016_5 <-RTM_2016_5[-nrow(RTM_2016_5),]

#Insert year of analysis here
year<- 2019

#generating random numbers to increase prices by
if (year==2018){
  incr<- runif(1, min=1, max=3)
}else if  (year==2019){
  incr<- runif(1, min=1.1, max=3.1)
}else if  (year==2020){
  incr<- runif(1, min=1.2, max=3.2)
}else if  (year==2021){
  incr<- runif(1, min=1.3, max=3.3)
}else if  (year==2022){
  incr<- runif(1, min=1.4, max=3.4)
}else if  (year==2023){
  incr<- runif(1, min=1.5, max=3.5)
}else if  (year==2024){
  incr<- runif(1, min=1.6, max=3.6)
}else if  (year==2025){
  incr<- runif(1, min=1.7, max=3.7)
}else if  (year==2026){
  incr<- runif(1, min=1.8, max=3.8)
}else if  (year==2027){
  incr<- runif(1, min=1.9, max=3.9)
}
#incr=2.64 - this is the value used for analysis in the report

#for leap years use RTM from 2016, for non-leap years, use RTM from 2015
if (year==2020 || year==2024 || year==2028 ){
RTM <- (1+incr/100)* RTM_2016_5
} else {
  RTM <-(1+incr/100)* RTM_2015_5 
}

#Known Values
E_max <- 50 #maximum battery energy
E_in <- E_max/2  #initial battery energy at the beginning of the analysis period
E_min <-5 #minimum battery energy
storage_base <- 25 #storage capacity is 25 MW (50 MWh)
eff <- sqrt(0.85) #roundtrip charge and discharge efficiency

#initialize different variables 
revenue_for_jan <- 0
rev_1 <- 0
rev_2 <- 0
rev_2 <- 0

#Jan 2019 analysis
no_of_days <- 30

#Arrange prices into groups for each day
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))

RTM_Daily_list <- chunk2(RTM[1:(288*no_of_days)], no_of_days)
#RTM_Daily_list <- chunk2(RTM[((288*90)+1):(288*120)], no_of_days)

RTM_Daily <- matrix(0, nrow=no_of_days, ncol=288)
for (i in 1:no_of_days){
RTM_Daily[i, ] <-as.numeric(as.character(unlist(RTM_Daily_list[i])))
}

# do the clustering analysis on the data itself
RTM_clust <- kmeans(x = RTM_Daily, centers = 3)


RTM_Daily_c1 = RTM_Daily[RTM_clust$cluster==1,]   # data in cluster 1
RTM_Daily_c2 = RTM_Daily[RTM_clust$cluster==2,]   # data in cluster 2
RTM_Daily_c3 = RTM_Daily[RTM_clust$cluster==3,]   # data in cluster 3

reprdat_min = matrix(0,nrow=3,ncol=288)        # will store the data values closest to clus cent
reprdat_median = matrix(0,nrow=3,ncol=288)

clustcent = as.matrix(RTM_clust$centers)   # cluster centers
numclust1 = sum(RTM_clust$cluster==1)      # number of days in cluster 1
numclust2 = sum(RTM_clust$cluster==2)      # number of days in cluster 2
numclust3 = sum(RTM_clust$cluster==3)      # number of days in cluster 3

#find the representative data set from cluster 1 - this should be at median distance to the cluster center
distfrmcent1 = rep(0,numclust1)  # distances of points from cluster center in clus1
if (numclust1==1){
  RTM_1 <- RTM_Daily_c1
} else {
for (i in 1:numclust1){
  distfrmcent1[i] = norm_vec(RTM_Daily_c1[i,] - clustcent[1,])
}
reprdat_min[1,] = RTM_Daily_c1[which.min(distfrmcent1),] 
reprdat_median[1,] = RTM_Daily_c1[whichmedian(distfrmcent1),] 
RTM_1=reprdat_median[1,]
}

#find the representative data set from cluster 2 - this should be at median distance to the cluster center
distfrmcent2 = rep(0,numclust2) # distances of points from cluster center in clus2
if (numclust2==1){
  RTM_2 <- RTM_Daily_c2
} else {
for (i in 1:numclust2){
  distfrmcent2[i] = norm_vec(RTM_Daily_c2[i,] - clustcent[2,])
}
  reprdat_min[2,] = RTM_Daily_c2[which.min(distfrmcent2),] 
  reprdat_median[2,] = RTM_Daily_c2[whichmedian(distfrmcent2),] 
  RTM_2=reprdat_median[2,]
}

#find the representative data set from cluster 3 - this should be at median distance to the cluster center
distfrmcent3 = rep(0,numclust3)            # distances of points from cluster center in clus3
if (numclust3==1){
  RTM_3 <- RTM_Daily_c3
} else {
for (i in 1:numclust3){
  distfrmcent3[i] = norm_vec(RTM_Daily_c3[i,] - clustcent[3,])
}
reprdat_min[3,] = RTM_Daily_c3[which.min(distfrmcent3),] 
reprdat_median[3,] = RTM_Daily_c3[whichmedian(distfrmcent3),] 
RTM_3=reprdat_median[3,]
}


#-------------------------------------------------------------------------------------------------
#Optimization for day from cluster 1
#-------------------------------------------------------------------------------------------------
library(Rglpk)
no_of_var <-(288) #no of 5 min prices for each day
#objective function
obj <- c(RTM_1[1:no_of_var]/(12),RTM_1[1:no_of_var]/(-12)) 
#LHS of constraint matrix
mat_c <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_c[i, 1:i] <- eff
}
mat_d <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_d[i, 1:i] <- -1/eff
}
mat_CP <- matrix(0, nrow=12, ncol=2*no_of_var)
#-------------------
mat_x <- cbind(mat_c, mat_d)
mat <- rbind(mat_x, mat_x)
#RHS of constraint matrix
rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var)))
dir <- c(rep('>=', no_of_var), rep('<=', no_of_var)) 
#mat <- rbind(mat_x, mat_x, mat_CP)
#rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var), rep(-25, 12)))
#dir <- c(rep('>=', no_of_var), rep('<=', no_of_var), rep('==', 12))
#bounds
bounds <- list(lower = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(0, 2*no_of_var))),
               upper = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(25, 2*no_of_var))))
#minimize the objective function
max <- FALSE
#solve
sol<-Rglpk_solve_LP(obj, mat, dir, rhs, bounds,types = NULL, max)
x_values = as.data.frame(seq(from = 1, to = 2* no_of_var))
x<- as.data.frame(sol[2])
charge_1 <- c(x$solution) #power
rev_1 <- round(sol$optimum,2) #revenue generated 
y_1 <-matrix(0, nrow=no_of_var, ncol=1) #defining size of charge matrix
y_1[1] <- E_in
for (i in 1:(no_of_var-1)){
  y_1[i+1] <- y_1[i] + charge_1[i]*eff/12 - charge_1[i+no_of_var]/(eff*12)
}
#plot(y_1, type='l')   
#plot(charge_1[1:no_of_var], type ='l')
#lines(charge_1[(no_of_var+1): (2*no_of_var)], col="red")
#dev.off()
print(rev_1)
#-------------------------------------------------------------------------------------------------
#Optimization for day from cluster 2
#-------------------------------------------------------------------------------------------------
library(Rglpk)
no_of_var <-(288) #no of 5 min prices for each day
#objective function
obj <- c(RTM_2[1:no_of_var]/(12),RTM_2[1:no_of_var]/(-12))
#LHS of the constraint matrix
mat_c <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_c[i, 1:i] <- eff
}
mat_d <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_d[i, 1:i] <- -1/eff
}
mat_CP <- matrix(0, nrow=12, ncol=2*no_of_var)
#-------------------
mat_x <- cbind(mat_c, mat_d)
mat <- rbind(mat_x, mat_x)
#RHS of the constraint matrix
rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var)))
dir <- c(rep('>=', no_of_var), rep('<=', no_of_var))
#mat <- rbind(mat_x, mat_x, mat_CP)
#rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var), rep(-25, 12)))
#dir <- c(rep('>=', no_of_var), rep('<=', no_of_var), rep('==', 12))
bounds <- list(lower = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(0, 2*no_of_var))),
               upper = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(25, 2*no_of_var))))
#minimize the objective function
max <- FALSE
#solve
sol<-Rglpk_solve_LP(obj, mat, dir, rhs, bounds,types = NULL, max)
x_values = as.data.frame(seq(from = 1, to = 2* no_of_var))
x<- as.data.frame(sol[2])
charge_2 <- c(x$solution) #power
rev_2 <- round(sol$optimum,2) #revenue generated 
y_2 <-matrix(0, nrow=no_of_var, ncol=1) #defining size of charge matrix
y_2[1] <- E_in
for (i in 1:(no_of_var-1)){
  y_2[i+1] <- y_2[i] + charge_2[i]*eff/12 - charge_2[i+no_of_var]/(eff*12)
}
#plot(y_2, type='l')   
#plot(charge_2[1:no_of_var], type ='l')
#lines(charge_2[(no_of_var+1): (2*no_of_var)], col="red")
#dev.off()

#-------------------------------------------------------------------------------------------------
#Optimization for day from cluster 3
#-------------------------------------------------------------------------------------------------
library(Rglpk)
no_of_var <-(288)#no of 5 min prices for each day
obj <- c(RTM_3[1:no_of_var]/(12),RTM_3[1:no_of_var]/(-12))
mat_c <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_c[i, 1:i] <- eff
}
mat_d <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_d[i, 1:i] <- -1/eff
}
mat_CP <- matrix(0, nrow=12, ncol=2*no_of_var)
#-------------------
mat_x <- cbind(mat_c, mat_d)
mat <- rbind(mat_x, mat_x)
rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var)))
dir <- c(rep('>=', no_of_var), rep('<=', no_of_var))
#mat <- rbind(mat_x, mat_x, mat_CP)
#rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var), rep(-25, 12)))
#dir <- c(rep('>=', no_of_var), rep('<=', no_of_var), rep('==', 12))
bounds <- list(lower = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(0, 2*no_of_var))),
               upper = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(25, 2*no_of_var))))
max <- FALSE
sol<-Rglpk_solve_LP(obj, mat, dir, rhs, bounds,types = NULL, max)
x_values = as.data.frame(seq(from = 1, to = 2* no_of_var))
x<- as.data.frame(sol[2])
charge_3 <- c(x$solution)
rev_3 <- round(sol$optimum,2)
y_3 <-matrix(0, nrow=no_of_var, ncol=1) #defining size of matrix
y_3[1] <- E_in
for (i in 1:(no_of_var-1)){
  y_3[i+1] <- y_3[i] + charge_3[i]*eff/12 - charge_3[i+no_of_var]/(eff*12)
}
#plot(y_3, type='l')   
#plot(charge_3[1:no_of_var], type ='l')
#lines(charge_3[(no_of_var+1): (2*no_of_var)], col="red")
#dev.off()

#-------------------------------------------------------------------------------------------------
#calculate total revenue for the month of January
#-------------------------------------------------------------------------------------------------
revenue_for_jan= rev_1*RTM_clust$size[1] + rev_2*RTM_clust$size[2] +rev_3*RTM_clust$size[3]
rev=c(rev_1, rev_2, rev_3)
print(revenue_for_jan)

#-------------------------------------------------------------------------------------------------
#Optimization for entire month of Jan 2019
#-------------------------------------------------------------------------------------------------
revenue_for_jan <- 0
rev_1 <- 0
rev_2 <- 0
rev_2 <- 0
library(Rglpk)
no_of_var <-(288*31) #no of 5 min prices in each day
#objective function
obj <- c(RTM[1:no_of_var]/(12),RTM[1:no_of_var]/(-12))
#obj <- c(RTM[((288*90)+1):(288*120)]/(12),RTM[((288*90)+1):(288*120)]/(-12))
#LHS of the constraint matrix
mat_c <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_c[i, 1:i] <- eff
}
mat_d <- matrix(0, nrow=no_of_var, ncol=no_of_var)
for (i in 1:no_of_var){
  mat_d[i, 1:i] <- -1/eff
}
mat_CP <- matrix(0, nrow=12, ncol=2*no_of_var)
#-------------------
mat_x <- cbind(mat_c, mat_d)
mat <- rbind(mat_x, mat_x)
#RHS of the constraint matrix
rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var)))
dir <- c(rep('>=', no_of_var), rep('<=', no_of_var))
#mat <- rbind(mat_x, mat_x, mat_CP)
#rhs <- (c(rep((E_min-E_in)*12, no_of_var), rep((E_max-E_in)*12, no_of_var), rep(-25, 12)))
#dir <- c(rep('>=', no_of_var), rep('<=', no_of_var), rep('==', 12))
#Bounds
bounds <- list(lower = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(0, 2*no_of_var))),
               upper = list(ind = seq(from = 1, to =2*no_of_var), val = c(rep(25, 2*no_of_var))))
#minimize the objective function
max <- FALSE
#solve
sol<-Rglpk_solve_LP(obj, mat, dir, rhs, bounds,types = NULL, max)
x_values = as.data.frame(seq(from = 1, to = 2* no_of_var))
x<- as.data.frame(sol[2])
charge_2 <- c(x$solution) #power
rev <- round(sol$optimum,2)#revenue generated
y <-matrix(0, nrow=no_of_var, ncol=1) #defining size of charge matrix
y[1] <- E_in
for (i in 1:(no_of_var-1)){
  y[i+1] <- y[i] + charge_2[i]*eff/12 - charge_2[i+no_of_var]/(eff*12)
}
#plot(y, type='l')   
#plot(charge_2[1:no_of_var], type ='l')
#lines(charge_2[(no_of_var+1): (2*no_of_var)], col="red")
#print(rev)


#-------------------------------------------------------------------------------------------------
#Graphs
#-------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------
#Jan 14, 2019 RTM Prices
#-------------------------------------------------------------------------------------------------
library(ggplot2)
library(scales)
date_vector <- seq(ISOdatetime(2019, 01, 14, 0, 0, 0), ISOdatetime(2019, 01, 14, 23, 55, 0), by="5 min", tz="CST")
#df <- data.frame(interval_plot=date_vector, RTM_plot=RTM[3745: 4608])
df <- data.frame(interval_plot=date_vector, RTM_plot=RTM[3745: (3745+287)])
my_plot <- ggplot(df, aes(x=interval_plot, y=RTM_plot)) + geom_line(aes(color='red'), size=2, show.legend = FALSE)+ labs(title="Projected Real Time Market Prices", x="Hour", y = "Price ($)")
my_plot
my_plot + theme_bw() +theme(axis.text=element_text(size=22), axis.title=element_text(size=30))+ theme(plot.title = element_text(size=36, face="bold"))+  coord_cartesian( ylim=c(0, 100))+ theme(legend.position="bottom") +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )

#-------------------------------------------------------------------------------------------------
#Jan 14, 2019 charging and discharging schedules
#-------------------------------------------------------------------------------------------------
date_vector <- seq(ISOdatetime(2019, 01, 14, 0, 0, 0), ISOdatetime(2019, 01, 14, 23, 55, 0), by="5 min")
df <- data.frame(interval_plot=date_vector, charge_plot=charge_2[3745: (3745+287)], discharge_plot = -charge_2[(3745+288*31):(3745+287+288*31)])
my_plot <- ggplot(df, aes(x=interval_plot)) + geom_line(aes(y=charge_plot),color='red', size=2)+ geom_line(aes(y=discharge_plot),color='red')+labs(title="Charging and Discharging Schedule \n of the ESSs", x="Hour", y = "Rate (MW)")

my_plot + theme_bw() +theme(axis.text=element_text(size=22), axis.title=element_text(size=30))+ theme(plot.title = element_text(size=36, face="bold"))+  coord_cartesian( ylim=c(-30, 30))+ theme(legend.position="bottom") +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime( breaks=date_breaks("4 hour"),labels = date_format("%H:%M", tz="America/Chicago"))

