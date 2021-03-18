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
plot_trunc_tog(3,1, -1, 2)



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


#genero d+q+1 colonne all'interno delle quali applico la stessa funzione per ogni elemento

c1=matrix(NA, n, 1)
for (i in 1:n) {
  c1[i]=g_b(x[i], d)
}



