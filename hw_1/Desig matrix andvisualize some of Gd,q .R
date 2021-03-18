
#EX 2.3#

#create supporto function that depend on w=number of knots in (0:1)
#we will use it to enpower the next function
supporto=function(w){return(quantile((0:1), probs = seq(0,1,length.out = w)))}
supp=df$x
supporto_x=function(w){return(quantile(supp,probs = seq(0,1,length.out = w)))}
#a(k) function (it depends only on k=#knots) generates the design matrix X that is required in point 2.3.
#This matrix is of order w x (k+d+1).
#We want to study this matrix for 3 different values of w, i.e. (3,5,10) with 
#poly degree d fixed at d=3

a=function(k,d){

values = matrix(NA,nrow= k , ncol = k+ d+1)
supporto=supporto_x(k)
numcol = k+d+1

for(i in 1:numcol){
  
  values[,i] = (i <=d+1 )* supporto^(i-1) + (i >d )*(supporto - supporto[max(1,(i-d-1))])^3
  values=ifelse(values>0,values,0)
}
return(values)
}

manipulate(plot(supporto_x(q),a(q,z)[,k]),q=slider(3,10),k=slider(1,10),z=slider(0,10))

#this function show also some elemenets of Gd,q
#it's enough to choose a specific d and q (in manipulate are rappresented by z and q rispectively),
#AND the specific column k
#Remember that for k<=d+1 manipulates shows the normal power function applied to
#our vector x, BUT
#for k>d+1 the function shows the values obtained with  truncated power applied
#to the k°th node

#
#NOTATION
# z=poly degree
# q=#of knots choosen
# k= k°th column of a(q,z)

#N.B.:(?)
# So if(k<=q) with manipulate we study the columns of Design Matrix obtained 
#with the "normal" power function.
# If(k>knots & k<=q+z+1) then manipulate function plots the trunc function 
#applied to the k°th knot
#If (k>q+z+1) manipulate gives error because it has no sense for the Matrix structure




#required cases: FIX z=d=3 for hypothesy then we are interested in
#1) case number of knots=q=3 and k=number of columns of trunc power=number of knots
#manipulate has sense if k<=q+z+1
#2)case q=5
#3)case q=10

a(3,3)
values_3=values  #Design Matrix X of order 3x7 

a(5,3)
values_5=values #Matrix X of order 5x9

a(10,3)
values_10=values#Matrix X of order 10x14




