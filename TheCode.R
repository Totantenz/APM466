BondPrices <- read.csv("/Users/markchen/Desktop/apm466/data.csv")
DataforDirty <- read.csv("/Users/markchen/Desktop/apm466/dirtyPrice.csv") #read the calculated  dirty prices of bonds 
DirtyPrices <- DataforDirty[,2:11]
library(BB)
options(warn = -1)

coupon_rate <- c(0.0075, 0.0075, 0.005, 0.0275, 0.0175, 0.015, 0.0225, 0.015, 0.0125,0.005)
coupon <- c(0.75, 0.75, 0.5, 2.75, 1.75, 1.5, 2.25, 1.5, 1.25,0.5)
par <- 100


Maturity_Date <- c('2021-03-01', '2021-09-01', '2022-03-01', '2022-06-01', 
                   '2023-03-01', '2023-06-01', '2024-03-01', '2024-09-01', 
                   '2025-03-01', '2025-09-01')



###



Dates = c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
          "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")
####YTM


#### Use dirty price to calculate ytm
####
####
#################
# I calculate the ytm of each bond with different maturity date.
# The first bond is the one that matures in March 2021.
bond1yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- 100.375/(1+x[1])^(1/12)-DirtyPrices[1,i]
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata1 <- (result$par)
  thata1
  bond1yield[i] <- thata1
}
# The second bond is the one that matures in Sep 2021.
bond2yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- 0.375/(1+x[1])^(1/12)+ 100.375/(1+x[1])^(7/12)-DirtyPrices[2,i]
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata2 <- (result$par)
  thata2
  bond2yield[i] <- thata2
}
bond2yield


# The third bond is the one that matures in March 2022.
bond3yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[3]/(1+x[1])^(1/12)
            +0.5*coupon[3]/(1+x[1])^(7/12)
            +100.25/(1+x[1])^(13/12)-DirtyPrices[3,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata3 <- (result$par)
  thata3
  bond3yield[i] <- thata3
}
bond3yield

# The fourth bond is the one that matures in June 2022.
bond4yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[4]/(1+x[1])^(4/12)
             +0.5*coupon[4]/(1+x[1])^(10/12)
             +(100+0.5*coupon[4])/(1+x[1])^(16/12)-DirtyPrices[4,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata4 <- (result$par)
  thata4
  bond4yield[i] <- thata4
}
bond4yield


# # The fifth bond is the one that matures in March 2023.
bond5yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[5]/(1+x[1])^(1/12)
             +0.5*coupon[5]/(1+x[1])^(7/12)
             +0.5*coupon[5]/(1+x[1])^(13/12)
             +0.5*coupon[5]/(1+x[1])^(19/12)
             +(100+0.5*coupon[5])/(1+x[1])^(25/12)-DirtyPrices[5,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata5 <- (result$par)
  thata5
  bond5yield[i] <- thata5
}
bond5yield


# The sixth bond is the one that matures in June 2023.
bond6yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[6]/(1+x[1])^(4/12)
             +0.5*coupon[6]/(1+x[1])^(10/12)
             +0.5*coupon[6]/(1+x[1])^(16/12)
             +0.5*coupon[6]/(1+x[1])^(22/12)
             +(100+0.5*coupon[6])/(1+x[1])^(28/12)-DirtyPrices[6,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata6 <- (result$par)
  thata6
  bond6yield[i] <- thata6
}
bond6yield

# The seventh bond is the one that matures in March 2024.
bond7yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[7]/(1+x[1])^(1/12) #21.3
             +0.5*coupon[7]/(1+x[1])^(7/12) #21.9
             +0.5*coupon[7]/(1+x[1])^(13/12) # 22.3
             +0.5*coupon[7]/(1+x[1])^(19/12) # 22.9
             +0.5*coupon[7]/(1+x[1])^(25/12)  #23.3
             +0.5*coupon[7]/(1+x[1])^(31/12) # 23.9
             +(100+0.5*coupon[7])/(1+x[1])^(37/12)-DirtyPrices[7,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata7 <- (result$par)
  thata7
  bond7yield[i] <- thata7
}
bond7yield


# The eighth bond is the one that matures in Sep 2024.
bond8yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[8]/(1+x[1])^(1/12) #21.3
             +0.5*coupon[8]/(1+x[1])^(7/12) #21.9
             +0.5*coupon[8]/(1+x[1])^(13/12) # 22.3
             +0.5*coupon[8]/(1+x[1])^(19/12) # 22.9
             +0.5*coupon[8]/(1+x[1])^(25/12)  #23.3
             +0.5*coupon[8]/(1+x[1])^(31/12) # 23.9
             +0.5*coupon[8]/(1+x[1])^(37/12) # 24.3
             +(100+0.5*coupon[8])/(1+x[1])^(43/12)-DirtyPrices[8,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata8 <- (result$par)
  thata8
  bond8yield[i] <- thata8
}
bond8yield


# The nineth bond is the one that matures in March 2025.
bond9yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[9]/(1+x[1])^(1/12) #21.3
             +0.5*coupon[9]/(1+x[1])^(7/12) #21.9
             +0.5*coupon[9]/(1+x[1])^(13/12) # 22.3
             +0.5*coupon[9]/(1+x[1])^(19/12) # 22.9
             +0.5*coupon[9]/(1+x[1])^(25/12)  #23.3
             +0.5*coupon[9]/(1+x[1])^(31/12) # 23.9
             +0.5*coupon[9]/(1+x[1])^(37/12) # 24.3
             +0.5*coupon[9]/(1+x[1])^(43/12) # 24.9
             +(100+0.5*coupon[9])/(1+x[1])^(49/12)-DirtyPrices[9,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata9 <- (result$par)
  thata9
  bond9yield[i] <- thata9
}
bond9yield




# The tenth bond is the one that matures in Sep 2025.
bond10yield <- vector("numeric",10)
for (i in 1:10){
  fun<- function(x) {
    f<- numeric(length(x))
    f[1] <- (0.5*coupon[10]/(1+x[1])^(21/12) 
             +0.5*coupon[10]/(1+x[1])^(27/12)
             +0.5*coupon[10]/(1+x[1])^(13/12)
             +0.5*coupon[10]/(1+x[1])^(19/12)
             +0.5*coupon[10]/(1+x[1])^(25/12)
             +0.5*coupon[10]/(1+x[1])^(31/12)
             +0.5*coupon[10]/(1+x[1])^(37/12) 
             +0.5*coupon[10]/(1+x[1])^(43/12)+
               0.5*coupon[10]/(1+x[1])^(49/12)+
               (100+0.5*coupon[10])/(1+x[1])^(55/12)-DirtyPrices[10,i])
    f
  }
  startx <- c(0.03)
  result = dfsane(startx,fun,control = (trace=FALSE))
  thata10 <- (result$par)
  thata10
  bond10yield[i] <- thata10
}
bond10yield

ytm_matrix <- data.frame(bond1yield,bond2yield,bond3yield,bond4yield,bond5yield,bond6yield,bond7yield
                         ,bond8yield,bond9yield,bond10yield)
rownames(ytm_matrix) <- c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
                          "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")
ytm_matrix

#### plot the yield curve
set.seed(10)
Interval = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
Dates = c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
          "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")
colours = sample(colors(), length(Dates))
for (i in 1:length(Dates)){
  if (i == 1){
    y1 <- as.vector(ytm_matrix[i, 1:10])
    plot(Interval, y1, xlim = c(0, 5.5), ylim = c(0.0005,0.005), 
         type = 'l', xlab = 'Time to Maturity', ylab = 'Yield', col = colours[i])
  }
  else{
    par(new = TRUE)
    y1 <- as.vector(ytm_matrix[i, 1:10])
    plot(Interval, y1, axes = FALSE, xlim = c(0, 5.5), ylim = c(0.0005,0.005),
         type = 'l', xlab = 'Time to Maturity', ylab = 'Yield', col = colours[i])
  }
}
legend("topleft", legend = Dates, col = colours, pch = 12, cex = 0.55)
title('Yield Curve')
########


###### Calculate Spot rate

TtM <- c(1/12, 7/12, 13/12, 4/3, 25/12, 28/12, 37/12, 43/12, 49/12, 55/12) # time to maturity with units in years
# Create a matrix
SpotMatrix <- rep(0,10*10)
dim(SpotMatrix) <- c(10,10)
colnames(SpotMatrix) <- c("bond1","bond2","bond3","bond4","bond5","bond6","bond7","bond8","bond9","bond10")
rownames(SpotMatrix) <- c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
                 "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")
SpotMatrix <- as.data.frame(SpotMatrix)

## Firstly, we need to calculate the first bond maturing within 6 months using the discrete compounding formula.
coupon[1]

bond1_Spot <- c()
for (i in c(1:10)) {
  ratev <- 2*(((0.5*coupon[1]+par)/DirtyPrices[1,i])^(1/(2*TtM[1]))-1)
  bond1_Spot[i] <- ratev
} 
SpotMatrix[1] <- bond1_Spot


## Next, we need to calculate the spot rates for bond2-bond10, which matures longer than 6 months. 
## The bootstrapping strategies are used in order to calculate the spot rates using discrete compounding models.



Last_payed_month <- c(5, 5, 5, 2, 5, 2, 5, 5, 5, 5)
Maturing_in_month <- c(1, 7, 13, 16, 25, 28, 37, 43, 49, 55)

for (l in c(2:10)){ # starting from bond2 
  
  for (j in c(1:10)){ # for each day among ten days
    pre_valuesum <- 0
    TtM <- c(1/12, 7/12, 13/12, 4/3, 25/12, 28/12, 37/12, 43/12, 49/12, 55/12)
    # Aiming for future coupon payment time period except for the final one
    FCT <- seq((6-Last_payed_month[l]),(Maturing_in_month[l])-1, by=6)/12 
    for (i in c(1:length(FCT))){
      pre_valuesum <- pre_valuesum + 0.5*coupon[l]/(1+SpotMatrix[l]/2)^(2*FCT[i])
      
    }
   Price_after_subtracted <- DirtyPrices[l,j] - pre_valuesum
   SpotMatrix[j,l]<- 2*(((0.5*coupon[l]+par)/Price_after_subtracted)^(1/(2*TtM[l]))-1)
   pre_valuesum <- 0
  }
} 
SpotMatrix


###### plot the spot curve
set.seed(11)
Interval = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
Dates = c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
          "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")
colours = sample(colors(), length(Dates))
for (i in 1:length(Dates)){
  if (i == 1){
    y2 <- as.vector(SpotMatrix[i, 1:10])
    plot(Interval, y2, xlim = c(0, 5.5), ylim = c(0.0005,0.005),
         type = 'l', xlab = 'Time to Maturity', ylab = 'Spot rate', col = colours[i])
  }
  else{
    par(new = TRUE)
    y2 <- as.vector(SpotMatrix[i, 1:10])
    plot(Interval, y2, axes = FALSE, xlim = c(0, 5.5), ylim = c(0.0005,0.005),
         type = 'l', xlab = 'Time to Maturity', ylab = 'Spot rate', col = colours[i])
  }
}
legend("topleft", legend = Dates, col = colours, pch = 12, cex = 0.55)
title('Spot Curve')


###### calculate forward rate

# create a matrix for forward rate
ForwardMatrix <- rep(0,10*4)
dim(ForwardMatrix) <- c(10,4)
colnames(ForwardMatrix) <- c("1yr1yr","1yr2yr","1yr3yr","1yr4yr")
rownames(ForwardMatrix) <- c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
                          "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")

ForwardMatrix <- as.data.frame(ForwardMatrix)


###  calculating forward rate
for (i in c(2,4,6,8)){
  for (j in c(1:10)){
    nth_yr <- (1+SpotMatrix[j,i]/2)^(2*i/2)
    one_year_forward <- (1+SpotMatrix[j,i+2]/2)^(i+2)
    ForwardMatrix[j,i/2] <- ((one_year_forward/nth_yr)^(1/2)-1)*2
  }
}



#### plot the forward curve
set.seed(13)
Dates = c("2021-1-18","2021-1-19","2021-1-20","2021-1-21","2021-1-22",
          "2021-1-25","2021-1-26","2021-1-27","2021-1-28","2021-1-29")
Interval = c(1,2,3,4)
colours = sample(colors(), length(Dates))
for (i in 1:length(Dates)){
  if (i == 1){
    y3<- as.vector(ForwardMatrix[i,1:4])
    plot(Interval, y3, xlim = c(1,4), ylim =c(0,0.012),  
         type = 'l', xlab = 'Years', ylab = 'Forward rate', col = colours[i])
  }
  else{
    par(new = TRUE)
    y3<- as.vector(ForwardMatrix[i,1:4])
    plot(Interval, y3, axes = FALSE, xlim = c(1,4),ylim =c(0,0.012),  
         type = 'l', xlab = 'Years', ylab = 'Forward rate', col = colours[i])
  }
}
legend("topleft", legend = Dates, col = colours, pch = 12, cex = 0.55)
title('Forward Curve')



#### Covariance matrices for yield rate
YTM_covmatrix <- matrix(nrow = 9, ncol = 5)
rownames(YTM_covmatrix) <-  c(1:9)
colnames(YTM_covmatrix) <- c("logYield1","logYield2","logYield3","logYield4","logYield5")
# pick the columns of bonds that will mature in March every year.
Pick<- as.vector(seq(1,9,by=2))

for (i in c(1:5)){
  for (j in c(1:9)){
    YTM_covmatrix[j,i]<- log(ytm_matrix[j+1,Pick[i]]/ytm_matrix[j,Pick[i]])
  
  }
}
YTM_covmatrix <- cov(YTM_covmatrix)
YTM_covmatrix

#### Covariance matrices for forward rate

Forward_covmatrix <- matrix(nrow = 9,ncol = 4)
rownames(Forward_covmatrix) <-  c(1:9)
colnames(Forward_covmatrix) <- c("log_1yr1yr","log_1yr2yr","log_1yr3yr","log_1yr4yr")

for (i in c(1:4)){
  for (j in c(1:9)){
    value <- log(ForwardMatrix[j+1, i]/ForwardMatrix[j, i])
    Forward_covmatrix[j, i] <- value
  }
  
}
Forward_covmatrix <- cov(Forward_covmatrix)
Forward_covmatrix



#### Eigenvalues and Eignenvectors
eigenve_ytm <- eigen(YTM_covmatrix)
eigenve_ytm

eigenve_forward <- eigen(Forward_covmatrix)
eigenve_forward
