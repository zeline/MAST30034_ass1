
N = 240
V = 441
x1 = 21
x2 = 21
p = 0.5 #between 0 and 1
nsrcs = 6
#Q1.1
#X = standardized generated dataset
AV = c(0, 20, 0, 0, 0, 0) #onset arrival vector
IV = c(30, 45, 60, 40, 40, 40) #increment vector, the period
DUR_ONES = c(15, 20, 25, 15, 20, 25) #duration of ones
DUR_NEG = IV - DUR_ONES #the other section what makes up the period (IV)

#Q1.1
TC <- matrix(data = NA, nrow = 240, ncol = 6)

  for(i in 1:6){
   
    start = c(rep(NA, AV[i]))
    period = c(rep(1, DUR_ONES[i]), rep(-1, DUR_NEG[i]))
    n = length(c(start, period))
    branch = rep_len(period, N-n)
    vec = c(start, period, branch)
    TC[, i] <- vec

  }
TC[,1]
matplot(TC[,1], type = 'l', xlab = 'Time', main = 'Time Course 1')

#now we need to standardise each column
scaled_TC <- matrix(data = NA, nrow = 240, ncol = 6)
for(i in 1:6){
  scaled_col = (TC[,i] - mean(TC[,i], na.rm = TRUE)) / sd(TC[,i], na.rm = TRUE)
  scaled_TC[,i] = scaled_col
}
apply(scaled_TC, 2, mean, na.rm=TRUE)
apply(scaled_TC, 2, sd, na.rm=TRUE)
#exporting the matrix
#write.table(scaled_TC, file = "~/scaled_TC.csv") #NOT WORKING???

#now we make the subplots 

par(mfrow=c(2,3))
matplot(scaled_TC[,1], type = 'l', xlab = 'Time', ylab = 'Source Value', 
        main = 'Time Course 1')
matplot(scaled_TC[,2], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 2')
matplot(scaled_TC[,3], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 3')
matplot(scaled_TC[,4], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 4')
matplot(scaled_TC[,5], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 5')
matplot(scaled_TC[,6], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 6')

" this totally ruined the proportions, but looks better as pdf"
par(mfrow=c(3,2))
matplot(scaled_TC[,1], type = 'l', xlab = 'Time', ylab = 'Source Value', 
        main = 'Time Course 1')
matplot(scaled_TC[,2], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 2')
matplot(scaled_TC[,3], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 3')
matplot(scaled_TC[,4], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 4')
matplot(scaled_TC[,5], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 5')
matplot(scaled_TC[,6], type = 'l', xlab = 'Time', ylab = 'Source Value',
        main = 'Time Course 6')

'standardise instead of normalise as normalising means every value
takes a value between [0,1] * a scalar. it is minmax scaling.
standardising is making the data fit a standard normal distribution
(assumes it is already following a normal distribution)
'
#1.2
corr_matrix = rcorr(scaled_TC, type= 'spearman')$r
#using spearman as the values are not normally distributed
min(corr_matrix)
max(corr_matrix)
#since the heatmap will be from [-0.07, 1], there isnt any strong negative
#correlation so I will focus on only displaying the strong positive correlations
colour = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(10)
heatmap = heatmap(x = corr_matrix, col = colour, symm = TRUE, 
  main = 'Correlation Between TC Vectors', Colv = NA, Rowv = NA)
legend(title = 'Scale',x = "right", 
       legend = c("0.0", "0.6", "1.0"),
       fill = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(3),
       xpd=TRUE)


#1.3
sm1 = matrix(0, 21, 21)
sm1[2:6, 2:6] <- 1

sm2 = matrix(0, 21, 21)
sm2[15:19, 2:6] <- 1

sm3 = matrix(0, 21, 21)
sm3[2:6, 8:13] <- 1

sm4 = matrix(0, 21, 21)
sm4[15:19, 8:13] <- 1

sm5 = matrix(0, 21, 21)
sm5[2:6, 15:19] <- 1

sm6 = matrix(0, 21, 21)
sm6[15:19, 15:19] <- 1


tmpSM <- array(c(sm1, sm2, sm3, sm4, sm5, sm6), dim = c(21, 21, 6))

tmpSM[,,1]
typeof(tmpSM)
sm1
#plotting
" if (!requireNamespace('BiocManager', quietly = TRUE))
  install.packages('BiocManager')

BiocManager::install('ComplexHeatmap')
install.packages('ComplexHeatmap')

legend(title = 'Scale',x = 'right', 
       legend = c('0.0', '1.0'),
       fill = colorRampPalette(c('lightblue', 'red'))(2) )
install.packages('nat') "

par(mfrow=c(3,2))
new_col = colorRampPalette(c("lightblue", 'yellow'))(2)
h1 = heatmap(x = tmpSM[,,1], col = colour, symm = TRUE, 
        main = 'Heatmap SM 1', Colv = NA, Rowv = NA)
h2 = heatmap(x = tmpSM[,,2], col = colour, symm = TRUE, 
        main = 'Heatmap SM 2', Colv = NA, Rowv = NA)
h3 = heatmap(x = tmpSM[,,3], col = colour, symm = TRUE, 
        main = 'Heatmap SM 3', Colv = NA, Rowv = NA)
h4 = heatmap(x = tmpSM[,,4], col = colour, symm = TRUE, 
        main = 'Heatmap SM 4', Colv = NA, Rowv = NA)
h5 = heatmap(x = tmpSM[,,5], col = colour, symm = TRUE, 
        main = 'Heatmap SM 5', Colv = NA, Rowv = NA)
h6 = heatmap(x = tmpSM[,,6], col = colour, symm = TRUE, 
        main = 'Heatmap SM 6', Colv = NA, Rowv = NA)

h3_ = heatmap(x = tmpSM[,,3], col = colour, symm = TRUE, 
             main = 'Heatmap SM 3', Colv = NA, Rowv = NA)
legend(x = "bottomright", 
       legend = c("0.0", "1.0"),
       fill = new_col, xpd=TRUE, cex = 1.5)

h6_ = heatmap(x = tmpSM[,,6], col = colour, symm = TRUE, 
              main = 'Heatmap SM 6', Colv = NA, Rowv = NA)
legend(x = "bottomright", 
       legend = c("0.0", "1.0"),
       fill = new_col, xpd=TRUE, cex = 1.5)
#make a matrix SM
SM <- matrix(data = NA, nrow = 441, ncol = 6)
for(i in 1:6){
  
  col <- as.vector(tmpSM[,, i])
  SM[, i] <- col
}

#now check if these are independent
SM
corr_matrix2 = rcorr(SM, type= 'spearman')$r
corr_matrix2

colour = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(20)
heatmap = heatmap(x = corr_matrix2, col = colour, symm = TRUE, 
                  main = 'Correlation Between SM Vectors', Colv = NA, Rowv = NA)
legend(title = 'Scale',x = "right", 
       legend = c("0.0", "0.6", "1.0"),
       fill = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(3),
       xpd=TRUE, cex = 1.5)

#1.4 - generate white noise
#scaled_TC
tnoise = rnorm(1440, mean = 0, sd = 0.25)
TC_noise <- matrix(data = tnoise, nrow = 240, ncol = 6)
corr_matrix_tn = rcorr(TC_noise, type= 'spearman')$r

colour = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(20)
heatmap = heatmap(x = corr_matrix_tn, col = colour, symm = TRUE, 
                  main = 'Correlation Between TC Noise', Colv = NA, Rowv = NA)
legend(title = 'Scale',x = "right", 
       legend = c("0.0", "0.6", "1.0"),
       fill = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(3),
       xpd=TRUE, cex = 1.5)

#SM
snoise = rnorm(2646, mean = 0, sd = 0.015)
SM_noise <- matrix(data = snoise, nrow = 441, ncol = 6)
corr_matrix_sn = rcorr(SM_noise, type= 'spearman')$r

colour = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(20)
heatmap = heatmap(x = corr_matrix_sn, col = colour, symm = TRUE, 
                  main = 'Correlation Between SM Noise', Colv = NA, Rowv = NA)
legend(title = 'Scale',x = "right", 
       legend = c("0.0", "0.6", "1.0"),
       fill = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(3),
       xpd=TRUE, cex = 1.5)

#histograms of noise dist
#sm noise
par(mfrow=c(1,1))
hist(snoise)
hist(tnoise)
#noise product
trans_SMN = t(SM_noise)
productM = TC_noise %*% trans_SMN
corr_matrix_Xn = rcorr(productM, type= 'pearson')$r
min(corr_matrix_Xn)

colour = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(20)
heatmap = heatmap(x = corr_matrix_Xn, col = colour, symm = TRUE, 
                  main = 'Correlation Between Noise Product Variables', 
                  Colv = NA, Rowv = NA)
library(Matrix)

#1.5
A = (TC + TC_noise)
B = (SM + SM_noise)
BT = t(B)
dim(A)

colSums(is.na(AA))
AA[,2]
#we need to get rid of these null values now, so we will mean impute
#we set the mean as 0
A <- replace(A, is.na(A), 0)
colSums(is.na(A))
#now we can do the cross product
X = A %*% BT

matplot(X[,1], type = 'l', xlab = 'Time', main = 'X1')

#gotta use gg plot to plot 100 lines
#https://stackoverflow.com/questions/27826666/plotting-1000-lines-with-ggplot2
#ggplot(data = X, aes(x=x, y=val)) + geom_line(aes(colour=variable))
Xdf = as.data.frame(X)
length(Xdf)
Xdf[1]
#random selection of 100 columns
numbers = sample(1:411, 100, replace=F)
numbers = sort(numbers)
df2 = Xdf[c(numbers)]
df2[1]
dfex <- data.frame(time = 1:240, df2)
dfex <- melt(dfex ,  id.vars = 'time', variable.name = 'series')

dfex

ggplot(dfex, aes(time, value, group = series)) + theme_bw() + theme(panel.grid=element_blank()) + geom_line(size=0.2, alpha=0.1)
X
variances = (colVars(X))
variances
plot(variances, type = "p")
dim(X)

#X_scaled <- matrix(data = NA, nrow = 240, ncol = 441)
#for(i in 1:6){
#  scaled_col = (X[,i] - mean(X[,i], na.rm = TRUE)) / sd(X[,i], na.rm = TRUE)
#  X_scaled[,i] = scaled_col
#} this resulted in many nulls due to the tiny numbers, will use a package instead
X_scaled = scale(X, center = TRUE, scale = TRUE)
X_new = matrix(X_scaled, nrow = 240, ncol = 441)

#2.1
TC #either replace nan with 0 or mean. mean = -0.09 so for simplicity i will
#do mean replacement as they are close in value
TC[,2][is.na(TC[,2])] <- mean(TC[,2], na.rm = TRUE)
solve(t(TC)%*%TC)

A_lsr = solve(t(TC)%*%TC) %*% t(TC) %*% X_new #(estimate)

D_lsr = X_new %*% t(A_lsr)

c(dim(A_lsr), dim(D_lsr)) #A_lsr is spatial and D is temporal

A1 = matrix(A_lsr[1,], nrow = 21, ncol = 21)
A1

colour = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(20)

par(mfrow=c(1,1))
heatmap_a1 = heatmap(x = t(matrix(A_lsr[6,], nrow = 21, ncol = 21)),
                     col = colour, symm = TRUE, main = 'Heatmap of A6', 
                  Colv = NA, Rowv = NA)
legend(title = 'Scale',x = "left", 
       legend = c("0.0", "0.6", "1.0"),
       fill = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(3),
       xpd=TRUE, cex = .75)

plot(D_lsr[,6], type = 'l', main = 'D6 Course')


#30th column of standardised X
X_scaled[,30]
D_lsr[,3]
plot(x = D_lsr[,3], y = X_scaled[,30])
plot(x = X_scaled[,30], y = D_lsr[,3])

plot(X_scaled[,30])
plot(D_lsr[,3])

plot(X[,30])
plot(x = X_scaled[,30], y = D_lsr[,4])
plot(x =  D_lsr[,4], y = X_scaled[,30])

#2.2 Ridge Regression
lambda = 0.5*V

A_rr = solve(t(TC)%*%TC + lambda *diag(6)) %*% t(TC) %*% X_new
D_rr = X_new %*% t(A_rr)
  #now compare rr to lsr
dim(D_rr)
dim(D_lsr)
dim(TC)

c_tlsr = cor(D_lsr, TC)
c_trr = cor(D_rr, TC)
c(sum(c_trr), sum(c_tlsr))
#12.36234 > 10.62412

#now let lambda = 1000
lambda2 = 1000
A_rr2 = solve(t(TC)%*%TC + lambda2 *diag(6)) %*% t(TC) %*% X_scaled
plot(A_rr2[1,])
plot(A_lsr[1,])

#2.3 LR
rho = seq(0, 1, by = 0.05)

#R code for LASSO Regresison
step = 1/(norm(TC %*% t(TC)) * 1.1) 
thr = rho*N*step
Ao = matrix(0, nsrcs ,1) 
A_ = matrix (0 , nsrcs ,1)
A_lr = matrix(0, nsrcs, x1*x2)
MSEs = matrix(0, 21, 1)

for(p in 1:21) {
  for (k in 1:(x1*x2)) {
    A_ <- Ao+step*(t(TC) %*% (X_new[,k]-(TC%*%Ao)))
    A_ <- (1/(1+thr[p])) * (sign(A_)*pmax(replicate(nsrcs, 0), abs(A_)-thr[p]))
    for ( i in 1:10)
    {
      snoise = rnorm(2646, mean = 0, sd = i*0.015)
      SM_noise <- matrix(data = snoise, nrow = 441, ncol = 6)
      tnoise = rnorm(1440, mean = 0, sd = i*0.25)
      TC_noise <- matrix(data = tnoise, nrow = 240, ncol = 6)
      
      A_loop = (TC + TC_noise)
      B_loop = (SM + SM_noise)
      BT_loop = t(B_loop)
      #get rid of nulls
      A_loop <- replace(A_loop, is.na(A_loop), 0)
      BT_loop <- replace(BT_loop, is.na(BT_loop), 0)
      
      X2 = A_loop %*% BT_loop
      X_scaled2 = scale(X2, center = TRUE, scale = TRUE)
      X_new2 = matrix(X_scaled2, nrow = 240, ncol = 441)
      
      Ao <- A_
      A_ <- Ao+step*(t(TC)%*%(X_new2[,k]-(TC%*%Ao)))
      A_ <- (1/(1+thr[p]))*(sign(A_)*pmax(replicate(nsrcs ,0), abs(A_)-thr[p]))
    }
    A_lr[ ,k] <- A_
    D_lr = X_new2 %*% t(A_lr)
  }
  mse = sum(sum((X_scaled2 - D_lr %*% A_lr)^2))/(N*V)
  MSEs[p,] <- mse
}
rho
MSEs
plot(y= MSEs, x= rho, main = "Average MSE Per Rho Value", xlab = "Rho Value")
#minimum is at index 5
#converges at index 9
sum(sum((X_scaled2 - D_lr %*% A_lr)^2))/(N*V)

#increases at value = 1
#minimum value at index 14
rho_val
#2.4
Ao = matrix(0, nsrcs ,1) 
A_ = matrix (0 , nsrcs ,1)
A_lr = matrix(0, nsrcs, x1*x2)
rho_val = rho[5]
thr = rho_val*N*step
rho_val
for (k in 1:(x1*x2)) {
  A_ <- Ao+step*(t(TC) %*% (X_new[,k]-(TC%*%Ao)))
  A_ <- (1/(1+thr)) * (sign(A_)*pmax(replicate(nsrcs, 0), abs(A_)-thr))
  for ( i in 1:10)
  {
    snoise = rnorm(2646, mean = 0, sd = 0.015)
    SM_noise <- matrix(data = snoise, nrow = 441, ncol = 6)
    tnoise = rnorm(1440, mean = 0, sd = 0.25)
    TC_noise <- matrix(data = tnoise, nrow = 240, ncol = 6)
    
    A_loop = (TC + TC_noise)
    B_loop = (SM + SM_noise)
    BT_loop = t(B_loop)
    #get rid of nulls
    A_loop <- replace(A_loop, is.na(A_loop), 0)
    BT_loop <- replace(BT_loop, is.na(BT_loop), 0)
    
    X2 = A_loop %*% BT_loop
    X_scaled2 = scale(X2, center = TRUE, scale = TRUE)
    X_new2 = matrix(X_scaled2, nrow = 240, ncol = 441)
    
    Ao <- A_
    A_ <- Ao+step*(t(TC)%*%(X_new2[,k]-(TC%*%Ao)))
    A_ <- (1/(1+thr))*(sign(A_)*pmax(replicate(nsrcs ,0), abs(A_)-thr))
  }
  A_lr[ ,k] <- A_
  D_lr = X_new2 %*% t(A_lr)
}

#i TC and DRR
C_trr = cor(TC, D_rr)
#i SM and Arr
C_srr = cor(SM, t(A_rr))
#i TC Dlr
C_tlr = cor(TC, D_lr)
#i TC and DRR
C_slr = cor(SM, t(A_lr))

c(sum(C_tlr), sum(C_trr)) 
c(sum(C_slr), sum(C_srr)) 

#C_tlr > C_trr but it isnt

#d estimates
D_lr
t(A_rr)

#d's should look like this
matplot(D_rr[,1], type = 'l', xlab = 'Time', main = 'Drr 1')
matplot(D_rr[,2], type = 'l', xlab = 'Time', main = 'Drr 2')
matplot(D_rr[,3], type = 'l', xlab = 'Time', main = 'Drr 3')
matplot(D_rr[,4], type = 'l', xlab = 'Time', main = 'Drr 4')
matplot(D_rr[,5], type = 'l', xlab = 'Time', main = 'Drr 5')
matplot(D_rr[,6], type = 'l', xlab = 'Time', main = 'Drr 6')

par(mfrow=c(3,2))

matplot(D_lr[,1], type = 'l', xlab = 'Time', main = 'Dlr 1')
matplot(D_lr[,2], type = 'l', xlab = 'Time', main = 'Dlr 2')
matplot(D_lr[,3], type = 'l', xlab = 'Time', main = 'Dlr 3')
matplot(D_lr[,4], type = 'l', xlab = 'Time', main = 'Dlr 4')
matplot(D_lr[,5], type = 'l', xlab = 'Time', main = 'Dlr 5')
matplot(D_lr[,6], type = 'l', xlab = 'Time', main = 'Dlr 6')
#A's should look like this
new_col = colorRampPalette(c("lightblue", 'yellow'))(2)

arr1 = heatmap(x = matrix(data = A_rr[1,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
             main = 'Heatmap A_rr 1', Colv = NA, Rowv = NA)
arr2 = heatmap(x = matrix(data = A_rr[2,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
        main = 'Heatmap A_rr 2', Colv = NA, Rowv = NA)
arr3 = heatmap(x = matrix(data = A_rr[3,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
        main = 'Heatmap A_rr 3', Colv = NA, Rowv = NA)
arr4 = heatmap(x = matrix(data = A_rr[4,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
        main = 'Heatmap A_rr 4', Colv = NA, Rowv = NA)
arr5 = heatmap(x = matrix(data = A_rr[5,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
        main = 'Heatmap A_rr 5', Colv = NA, Rowv = NA)
arr6 = heatmap(x = matrix(data = A_rr[6,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
        main = 'Heatmap A_rr 6', Colv = NA, Rowv = NA)

alr1 = heatmap(x = matrix(data = A_lr[1,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_lr 1', Colv = NA, Rowv = NA)
alr2 = heatmap(x = matrix(data = A_lr[2,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_lr 2', Colv = NA, Rowv = NA)
alr3 = heatmap(x = matrix(data = A_lr[3,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_lr 3', Colv = NA, Rowv = NA)
alr4 = heatmap(x = matrix(data = A_lr[4,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_lr 4', Colv = NA, Rowv = NA)
alr5 = heatmap(x = matrix(data = A_lr[5,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_lr 5', Colv = NA, Rowv = NA)
alr6 = heatmap(x = matrix(data = A_lr[6,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_lr 6', Colv = NA, Rowv = NA)

#2.5
dim(TC)
svd(TC)
# [u, v, w] = svds(TC)
# U is the eigen vector of XX’ and it is where you obtain ”Z” for your case
# V are the eigen values
# W is the eigen vector of X’X, 
par(mfrow=c(1,1))
eigen_vals = svd(TC)$u
#columns of u correspond to the principle components

matplot(eigen_vals, main = 'All Eigen Values Separated by TC', ylab = "Eigen values")

plot(eigen_vals[,1], main = "TC1 Eigen Values", pch = 18)
plot(eigen_vals[,2], main = "TC2 Eigen Values", pch = 18)
plot(eigen_vals[,3], main = "TC3 Eigen Values", pch = 18)
plot(eigen_vals[,4], main = "TC4 Eigen Values", pch = 18)
plot(eigen_vals[,5], main = "TC5 Eigen Values", pch = 18)
plot(eigen_vals[,6], main = "TC6 Eigen Values", pch = 18)

Z = eigen_vals
dim(TC)
#now applying lasso to X_new, and using Z instead of TC

Ao = matrix(0, nsrcs ,1) 
A_ = matrix (0 , nsrcs ,1)
A_pcr = matrix(0, nsrcs, x1*x2)
rho_val = 0.001
thr = rho_val*N*step

for (k in 1:(x1*x2)) {
  A_ <- Ao+step*(t(Z) %*% (X_new[,k]-(Z%*%Ao)))
  A_ <- (1/(1+thr)) * (sign(A_)*pmax(replicate(nsrcs, 0), abs(A_)-thr))
  for ( i in 1:10)
  {
    Ao <- A_
    A_ <- Ao+step*(t(Z)%*%(X_new[,k]-(Z%*%Ao)))
    A_ <- (1/(1+thr))*(sign(A_)*pmax(replicate(nsrcs ,0), abs(A_)-thr))
  }
  A_pcr[ ,k] <- A_
  D_pcr = X_new %*% t(A_pcr)
}

#plots of A_pcr
heatmap(x = matrix(data = A_pcr[1,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_pcr 1', Colv = NA, Rowv = NA)
heatmap(x = matrix(data = A_pcr[2,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_pcr 2', Colv = NA, Rowv = NA)
heatmap(x = matrix(data = A_pcr[3,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_pcr 3', Colv = NA, Rowv = NA)
heatmap(x = matrix(data = A_pcr[4,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_pcr 4', Colv = NA, Rowv = NA)
heatmap(x = matrix(data = A_pcr[5,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_pcr 5', Colv = NA, Rowv = NA)
heatmap(x = matrix(data = A_pcr[6,], nrow = 21, ncol = 21), col = colour, symm = TRUE, 
               main = 'Heatmap A_pcr 6', Colv = NA, Rowv = NA)

#plots of D_pcr
par(mfrow=c(3,2))

matplot(D_pcr[,1], type = 'l', xlab = 'Time', main = 'Dpcr 1')
matplot(D_pcr[,2], type = 'l', xlab = 'Time', main = 'Dpcr 2')
matplot(D_pcr[,3], type = 'l', xlab = 'Time', main = 'Dpcr 3')
matplot(D_pcr[,4], type = 'l', xlab = 'Time', main = 'Dpcr 4')
matplot(D_pcr[,5], type = 'l', xlab = 'Time', main = 'Dpcr 5')
matplot(D_pcr[,6], type = 'l', xlab = 'Time', main = 'Dpcr 6')
