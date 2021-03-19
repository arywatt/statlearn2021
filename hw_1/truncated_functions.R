##ex 2.2

##used in order to generate those functions such that gi(x)=x^i-1 (the blue functions)
g_b=function(x,d){
  gx=x^(d-1)
  return(gx)
}

##plotting the power functions together
plot_power = function(d){
  par(mfrow=c(1,1))
  curve(g_b(x,1), main="power functions", from = -2, to=2, ylim=c(-0.5, 10), col="darksalmon")
  for (i in 2:d) {
    curve(g_b(x,i), add=T, col=i)
  }
}



##generates the truncated power functions (brown functions)

trunc=function(x, j, d, q) { #j is the jth knot considered, d is the degree of the polinomial, q is the number of knots
  knots= quantile((0:1), probs = seq(0,1,length.out=q)) 
  #generates q equispaced knots in (0,1)
  knot=knots[j] #considers the jth knot 
  ifelse(x- knot>0, ((x-knot)^d), 0)} 
#if the difference between x and the jth knot is greater than 0, then the function has value (x - jth knot)^d, otherwise it's truncated at zero


##plots the truncated fncs separately, one graph each
plot_trunc_sep=function(q,d){ 
par(mfrow = c(2,q/2))
for (j in 1:q) {
        curve(trunc(x, j, d, q), from = -2, to=2, col="red", main = "truncated power fnc")}}  




#plots the truncated power functions on same page
plot_trunc_tog=function(q,d,from, to){ 
  #par(mfrow = c(1,1))
  curve(trunc(x, 1, d, q), from = from, to=to, col="red", main = "truncated power fnc")
  for (j in 2:q) {
    curve(trunc(x, j, d, q), col=j, main = "truncated power fnc", add=T)
    
  }
}




##1st combination of d=1, q=3
plot_power(1)
plot_trunc_tog(3,1, from = -1, to=2)



curve(g_b(x,1), main="power functions", col="yellow", from = -2, to=2, ylim=c(-0.5, 2))  ##g1 = x^0 constant
curve(g_b(x,2), add=T, col="pink")  ##g2 = x^1 bisector    
curve(trunc(x, 1, 1, 3), col="red", add = T)
curve(trunc(x, 2, 1, 3), from = -2, to=2, col="blue",  add = T)
curve(trunc(x, 3, 1, 3), from = -2, to=2, col="green", add=T)


##2nd combination of d=3, q=5
plot_power(3)
plot_trunc_tog(5,3, -0.5,5 )

curve(g_b(x,1), main="constant fnc", col="yellow", from = -2, to=2, ylim=c(-0.5, 5))  ##g1 = x^0 constant
for (i in 2:4 ) {
  curve(g_b(x,i), add=T)
  }



#3rd combination d=5, q=10
plot_power(d=5)
plot_trunc_tog(10,5)




####ex. 2.3
load("C:/Users/Sony/iCloudDrive/materie esami magistrale/1st year- 2nd semester/statistical learning/hw/hw1/ieri_domani.RData")

d=3
q=3 #5,10
obs=df$x
knots= quantile((0:1), probs = seq(0,1,length.out=q))
n=length(obs)
X=matrix(NA, nrow = nrow(df), ncol = q+d+1)

#the generic entry of the design matrix X in i,j is the value of the jth fnc computed for the ith unit of the observations vector x, X[i, j] = gj(xi)


obs=df$x
knots=function(q){return(quantile(obs,probs = seq(0,1,length.out = q)))}
#This matrix is of order w x (k+d+1).
#We want to study this matrix for 3 different values of w, i.e. (3,5,10) with 
#poly degree d fixed at d=3

design=function(q,d){
  
  values = matrix(NA,nrow= length(obs) , ncol =q+d+1)
  knots=knots(q)
  numcol = q+d+1
  
  for(j in 1:numcol){
    knot=ifelse(j<= d+1, 0, knots[j-d-1])
    values[,j] = (j <=d+1 )* obs^(j-1) + (j >d+1 )*(obs - knot)^3
    values=ifelse(values>0,values,0)
  }
  return(values)
}

X_3_3=cbind(design(3,3), df$y.yesterday)


X_3_3=as.data.frame(X_3_3)
fit_3_3=lm(V8 ~ V1+ V2+ V3 + V4 + V5 + V6 + V7, data=X_3_3) 
fit_3_3$coefficients  ##na coefficients are the 1st one the 5th one and the 7th one
summary(fit_3_3)



X_5_3=cbind(design(5,3), df$y.yesterday)
X_5_3=as.data.frame(X_5_3)
fit_5_3=lm(V10 ~ V1+ V2+ V3 + V4 + V5 + V6 + V7 + V8 + V9, data=X_5_3)
fit_5_3$coefficients  ## na coefficients are the 1st (all ones) one the fifth one and the last one (all zero)
summary(fit_5_3)

head(design(10,3))

X_10_3=cbind(design(10,3), df$y.yesterday)
X_10_3=as.data.frame(X_10_3)
fit_10_3=lm(V15 ~ V1+ V2+ V3 + V4 + V5 + V6 + V7 + V8 + V9+ V10 + V11+ V12+ V13+ V14, data=X_10_3)
fit_10_3$coefficients  ## na coefficients are the 1st (all ones) one the fifth one and the last one (all zero)
summary(fit_10_3)


#### Mallow's CP
##defining the MSE function
MSE_tr=function(residuals){
  mse=1/length(residuals)*(sum((residuals)^2))
  return(mse)
  }

MSE_tr(fit_3_3$residuals)

##Mallows's cp estimation
Cp=function(ps, residuals){ #ps=tot # of parameters
  n=length(residuals)
  MSE.tr=MSE_tr(residuals)
  hatsigma2 <- ( n*MSE.tr ) / (n - ps)
  Cps <- MSE.tr + (2 * hatsigma2 * ps) / n
  return(Cps)
}

##q=3 d=3 model
cp_3_3=Cp(8, fit_3_3$residuals)


##q=5 d=3 model
cp_5_3=Cp(10, fit_5_3$residuals)


##q=10 d=3 model
cp_10_3=Cp(15, fit_10_3$residuals)

which.min(c(cp_3_3, cp_5_3, cp_10_3)) ##the best model according to the evaluation of its mallow's cp is the one with 10 knots

##generalized cross validation  
###wiki-if we fit the model and compute the MSE on the training set, we will get an optimistically biased assessment of how well the model will fit an independent data set. This biased estimate is called the in-sample estimate of the fit, whereas the cross-validation estimate is an out-of-sample estimate. 
MSEs.tr=c(MSE_tr(fit_3_3$residuals), MSE_tr(fit_5_3$residuals), MSE_tr(fit_10_3$residuals))

ps=c(length(fit_3_3$coefficients), length(fit_5_3$coefficients), length(fit_10_3$coefficients))                                      
GCV= MSEs.tr / (1 - (ps)/n )^2

plot(c(3,5,10), GCV, type = "b", xlab = "q")
