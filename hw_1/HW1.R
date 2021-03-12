#HW1#

###2.1###

supporto=NULL

fun1=function(knots,d){
  if(knots>2){
  supporto= quantile((0:1), probs = seq(0,1,length.out=knots))
  values=supporto^d
  return(values)
  }
  
  else if(knots==1 || knots==2){
    supporto=c(0,1)
    values=supporto^d
    return(values)
  }
}

#Caso degenere:
#Non considero caso knots=0 poichè non ha senso

#1 CASO:
#Se knots=1 o knots=2 ,indipendentemente dal grado del polinomio d,
#otteniamo grafico=bisettrice (vedi graficamente). (Situazione analoga
#per d=1)

#2 CASO:
#Per d=1 è indifferente il numero di knots che considero poichè 
#la funzione f(x)=x^d=x^1=x (bisettrice 3 e 4 Q)

#QUINDI PER I CASI SOPRA (1 e 2) graficamente sarà del tipo:
plot(seq(0,1,0.5),seq(0,1,0.5),type="l") 


#3 CASO:
#per d=0 ho f(x)=x^0=1 per ogni x, e quindi grafic:
plot((0:1),(0:1)^0,type="l")


#4 CASO:
#in tutti gli altri situazione grafica data da:
manipulate(plot(quantile((0:1),probs = seq(0,1,length.out=alfa)),fun1(alfa,beta)),alfa=slider(3,10),beta=slider(1,10))



##NO##
###2.3###
train <- data.frame(x = df$x, y = df$y.yesterday)

ftrue=c(0.4342,0.4780,0.5072,0.5258,0.5369,0.5426,0.5447,0.5444,0.5425,0.5397,
               0.5364,0.5329,0.5294,0.5260,0.5229,0.5200,0.5174,0.5151,0.5131,0.5113,
               0.5097,0.5083,0.5071,0.5061,0.5052,0.5044,0.5037,0.5032,0.5027,0.5023)

plot((0:29),ftrue,type="l",col="red")

fun3knots=quantile(ftrue, probs = seq(0,1,length.out=100))
mod1=lm(y ~ poly(x, degree = 3), train)


