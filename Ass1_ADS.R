
N = 240
V = 441
x1 = 21
x2 = 21
p = 0.5 #between 0 and 1
nsrcs = 6
#Q1.1
#X = standardized generated dataset
AV = c(0, 20, 0, 0, 0, 0) #onset arrival vector
IV = c(30, 45, 60, 40, 40, 40) #increment vector
DUR_ONES = c(15, 20, 25, 15, 20, 25) #duration of ones

TC1 = matrix(c(rep(1, DUR_ONES[1]), AV[1]:IV[1]:N-20), nrow = 240, ncol = 6, byrow = FALSE)
TC2 = matrix(c(rep(1, DUR_ONES[2]),AV[2]:IV[2]:N-20), nrow = 240, ncol = 6, byrow = FALSE)
TC3 = matrix(c(rep(1, DUR_ONES[3]),AV[3]:IV[3]:N-20), nrow = 240, ncol = 6, byrow = FALSE)
TC4 = matrix(c(rep(1, DUR_ONES[4]),AV[4]:IV[4]:N-20), nrow = 240, ncol = 6, byrow = FALSE)
TC5 = matrix(c(rep(1, DUR_ONES[5]),AV[5]:IV[5]:N-20), nrow = 240, ncol = 6, byrow = FALSE)
TC6 = matrix(c(rep(1, DUR_ONES[6]),AV[6]:IV[6]:N-20), nrow = 240, ncol = 6, byrow = FALSE)

'standardise instead of normalise as normalising means every value
takes a value between [0,1] * a scalar. it is minmax scaling.
standardising is making the data fit a standard normal distribution
(assumes it is already following a normal distribution)
'
TC1
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
