
library(ggplot2)
library(scales)
library(gridExtra)


##################
##################

#coefficienti

##################
##################

y=nf.coeff
x=seq(1:201)

line=hue_pal()(201)

base=ggplot() + xlim(0, 201) 

base + geom_segment( aes(x=x, xend=x, yend=0, y=y), color=line,size=0.6)+theme(panel.background = element_rect( fill = "#FAFAFA"))+xlab("")+ylab("Beta[j]")+labs(title= paste(""))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))+theme(axis.title.y = element_text( face="bold"))



#############################################
#############################################

#funzione dopler

############################################
############################################


based =  ggplot() + xlim(0, 1) 

based+geom_function(fun = doppler.fun, n=1001, color="#C5CAE9", size = 1.5)+xlab("")+ylab("m(x)")+theme(panel.background = element_rect( fill = "#FAFAFA"))+labs(title= paste("Doppler function"))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))+theme(axis.title.y = element_text( face="bold"))



#approssimazioni

j.seq=c(5,50,150)

color= c("#00C1AA","#9590FF","#FC717F")



gg_list=list()

gg_list2=list()

for (idx in 1:length(j.seq)) {
  
  gg_list[[idx]]= based+geom_function(fun = doppler.fun, n=1001, color="#E8EAF6", size = 1.2)+xlab("")+ylab("m(x)")+theme(panel.background = element_rect( fill = "#FAFAFA")) + geom_function(fun= function(x) proj.cos(x,f.coeff=f.coeff, j=j.seq[idx]), color=color[idx], size=1.4)+xlab(paste("Error =", round(mse(doppler.fun,proj.cos,j.seq[idx]),4)))+theme(axis.title.x = element_text(color=color[idx], size=14, face="bold"))+theme(axis.title.y = element_text( face="bold"))+labs(title= paste(j.seq[idx],"-term Linear Approximation"))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))
  
  
  
  gg_list2[[idx]]=based+geom_function(fun = doppler.fun, n=1001, color="#E8EAF6", size = 1.2)+xlab("")+ylab("m(x)")+theme(panel.background = element_rect( fill = "#FAFAFA")) + geom_function(fun= function(x) proj.cos.nl(x,f.coeff=f.coeff,j=j.seq[idx]), color=color[idx], size=1.4)+theme(axis.title.y = element_text( face="bold")) +labs(title= paste(j.seq[idx],"-term Non Linear Approximation"))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold")) +xlab(paste("Error =", round(mse(doppler.fun,proj.cos,j.seq[idx]),4)))+theme(axis.title.x = element_text(color=color[idx], size=14, face="bold"))    
  
  
  
  
}


grid.arrange(gg_list[[1]],gg_list2[[1]],gg_list[[2]],gg_list2[[2]],gg_list[[3]],gg_list2[[3]] ,ncol=2)




