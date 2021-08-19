
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

#now we make the subplots 
scaled_TC
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
       fill = colorRampPalette(c("lightblue", "lightgreen", "yellow"))(3) )

#1.3



#R code for LASSO Regresison
  #step <= 1/(norm(TC%*%t(TC))*1.1) 
  #thr <= rho*N*step
  #Ao <= matrix(0,nsrcs ,1) A <= matrix (0 , nsrcs ,1)
  #Alr <= matrix(0,nsrcs ,x1*x2)
  #for (k in 1:(x1*x2)) {
   # A<= Ao+step*(t(TC)%*%(X[,k]=(TC%*%Ao)))
    #A <= (1/(1+thr))*(sign(A)*pmax(replicate(nsrcs ,0), abs(A) =thr))
    #for ( i in 1:10)
    #{
    #}
    #Alr [ , k]<=A } 
