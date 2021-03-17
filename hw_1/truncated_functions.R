##used in order to generate those functions such that gi(x)=x^i-1 (the blue functions)
g_b=function(x,d){
  gx=x^(d-1)
  return(gx)
}

##generates the truncated power functions

trunc=function(x, j, d, q) { #j  is the jth knot considered, d is the degree of the polinomial, q is the number of knots
  knots= quantile((0:1), probs = seq(0,1,length.out=q)) 
  #generates q equispaced knots in (0,1)
  knot=knots[j] #considers the jth knot 
  ifelse(x- knot>0, ((x-knot)^d), 0)} 
#if the difference between x and the jth knot is greater than 0, then the function has value (x - jth knot)^d, otherwise it's truncated at zero



par(mfrow(d+q+1,1))
curve(g_b(x,1))  ##g1 = x^0 constant
curve(g_b(x,2))  ##g2 = x^1 bisector

plot_trunc=function(q,d){ 
par(mfrow = c(1,q))
for (j in q:1) {
        curve(trunc(x, j, d, q), from = -2, to=2, col="red", main = "truncated power fnc")
  
}
}  
#plots q truncated power functions 

plot_trunc(3,1)

