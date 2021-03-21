

library(RColorBrewer)

col.tr=brewer.pal(n = 9, name = "Set1")
col.pp=c("#E76BF3","#BF80FF","#7997FF","#00B0FC") 

plot_power_gg= function(d){
  basep =  ggplot() + xlim(-2, 2) +ylim(-5.5,10) +geom_function(fun = function(x) g_b(x,1),size = 1.3,color="#FF6A98") +labs(title= paste("Power Function","d=",d))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))
  
  for(i in 2:(d+1)){
    
    basep= basep + geom_function(fun = function(x,i) g_b(x,i),size = 1.2,color=col.pp[i],args=list(i=i))+theme(panel.background = element_rect( fill = "#FAFAFA"))+ylab("")
  }
  print(basep)
  
}
plot_power_gg(1)





###################################



plot_trunc_gg= function(q,d,from,to){
  basetr= ggplot() + xlim(from, to)+theme(panel.background = element_rect( fill = "#FAFAFA"))+ylab("")+labs(title= paste("Truncated Power Fnc","q=",q))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))
  
  tronc=basetr +geom_function(fun = function(x) trunc(x, 1, d, q),size = 1.3, color="#FF61C9")
  for (j in 2:q) {
    
    tronc= tronc+ geom_function(fun = function(x,j) trunc(x, j, d, q),size = 1.3,color=col.tr[j-1],args=list(j=j))
    
  }
  print(tronc)
  
}

##################


#caso d=1, q=3
plot2=plot_trunc_gg(3,1,-1,2)

plot1=plot_power_gg(1)

grid.arrange(plot1, plot2,ncol=2)








####################################
#caso d=3 q=5



plotd4=plot_power_gg(3)


plotq5=plot_trunc_gg(5,3, -0.5, 5)


grid.arrange(plotd4, plotq5,ncol=2)


####################################

#caso d=5 q=10


plot_power(d=5)


plotd5=plot_power_gg(5)




plot_trunc_tog(10,5,1.5,5)


plotq10=plot_trunc_gg(10,5,1.5,5)

grid.arrange(plotd5, plotq10,ncol=2)

####################################



p <- data.frame(dose=c(3,5,10),
                 len=GCV)

ggplot(data=p, aes(x=dose, y=len, group=1))+theme(panel.background = element_rect( fill = "#FAFAFA")) +geom_line(col="#90CAF9", size=1.2)+geom_point(col="#BF80FF", size=2)+xlab("q")+theme(axis.title.x = element_text( size=14, face="bold",color="#5C6BC0"))+ylab("GCV")+theme(axis.title.y = element_text( face="bold",color="#5C6BC0"))+labs(title= "General CV Score for different models")+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))


p2 <- data.frame(dose=c(3,5,10),
                len=KCV)

ggplot(data=p2, aes(x=dose, y=len, group=1))+theme(panel.background = element_rect( fill = "#FAFAFA")) +geom_line(col="#90CAF9", size=1.2)+
  geom_point(col="#BF80FF", size=2)+xlab("q")+theme(axis.title.x = element_text( size=14, face="bold",color="#5C6BC0"))+ylab("GCV")+theme(axis.title.y = element_text( face="bold",color="#5C6BC0"))+labs(title= "5 Fold CV for different models")+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))
















