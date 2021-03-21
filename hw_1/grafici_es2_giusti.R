

library(RColorBrewer)

col.tr=brewer.pal(n = 9, name = "Set1")
col.pp=c("#E76BF3","#BF80FF","#7997FF","#00B0FC") 

plot_power_gg= function(d){
  basep =  ggplot() + xlim(-2, 2) +ylim(-5.5,10) +geom_function(fun = function(x) g_b(x,1),size = 1.3,color="#FF6A98") +labs(title= paste("Power Function","d=",d))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))
  
  for(i in 2:d){
    
    basep= basep + geom_function(fun = function(x,i) g_b(x,i),size = 1.2,color=col.pp[i],args=list(i=i))+theme(panel.background = element_rect( fill = "#FAFAFA"))+ylab("")
  }
  print(basep)
  
}
plot_power_gg(1)



################################





plot_trunc_gg= function(q,d,from,to){
  basetr= ggplot() + xlim(from, to)+theme(panel.background = element_rect( fill = "#FAFAFA"))+ylab("")+labs(title= paste("Truncated Power Function","d=",d,"q=",q))+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))
  
  tronc=basetr +geom_function(fun = function(x) trunc(x, 1, d, q),size = 1.3, color="#FF61C9")
  for (j in 2:q) {
    
    tronc= tronc+ geom_function(fun = function(x,j) trunc(x, j, d, q),size = 1.3,color=col.tr[j-1],args=list(j=j))
    
  }
  print(tronc)
  
}


plot_trunc_gg(3,1,-1,2)



###################################




basepf =  ggplot() + xlim(-2, 2) +ylim(-0.5,2)+theme(panel.background = element_rect( fill = "#FAFAFA"))+ylab("")

basepf + geom_function(fun = function(x) g_b(x,1),size = 1.3,color="#AC88FF") +geom_function(fun = function(x) g_b(x,2),size = 1.3,color="#00BC59") + geom_function(fun = function(x) trunc(x, 1, 1, 3),size = 1.3,color="#F37B59") +geom_function(fun = function(x) trunc(x, 2, 1, 3),size = 1.3,color="#00C1AA" )+ geom_function(fun = function(x) trunc(x, 3, 1, 3),size = 1.3,color="#00A5FF")+labs(title= "Power Functions")+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))




####################################




plot_power_gg(3)




####################################





plot_trunc_tog(5,3, -0.5,5 )


plot_trunc_gg(5,3, -0.5, 5)



####################################




curve(g_b(x,1), main="constant fnc", col="yellow", from = -2, to=2, ylim=c(-0.5, 5))  ##g1 = x^0 constant
for (i in 2:4 ) {
  curve(g_b(x,i), add=T)
}


colorifnc=c("#CEC1EA","#90CAF9","#EF9A9A")

basepfnc=  ggplot() + xlim(-2, 2) +ylim(-0.5,5)+theme(panel.background = element_rect( fill = "#FAFAFA"))+ylab("")+ geom_function(fun = function(x) g_b(x,1),size = 1.3,color="#E6A0C4")+labs(title= "Constant fnc")+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))

for (j in 2:4){
  basepfnc = basepfnc + geom_function(fun = function(x,j) g_b(x,j),size = 1.3,color=colorifnc[j-1],args=list(j=j))
  
}

print(basepfnc)




#####################################




plot_power(d=5)


plot_power_gg(5)



#####################################



plot_trunc_tog(10,5,1.5,5)


plot_trunc_gg(10,5,1.5,5)



####################################



p <- data.frame(dose=c(3,5,10),
                 len=GCV)

ggplot(data=p, aes(x=dose, y=len, group=1))+theme(panel.background = element_rect( fill = "#FAFAFA")) +geom_line(col="#90CAF9", size=1.2)+geom_point(col="#BF80FF", size=2)+xlab("q")+theme(axis.title.x = element_text( size=14, face="bold",color="#5C6BC0"))+ylab("GCV")+theme(axis.title.y = element_text( face="bold",color="#5C6BC0"))+labs(title= "General CV Score for different models")+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))


p2 <- data.frame(dose=c(3,5,10),
                len=KCV)

ggplot(data=p2, aes(x=dose, y=len, group=1))+theme(panel.background = element_rect( fill = "#FAFAFA")) +geom_line(col="#90CAF9", size=1.2)+
  geom_point(col="#BF80FF", size=2)+xlab("q")+theme(axis.title.x = element_text( size=14, face="bold",color="#5C6BC0"))+ylab("GCV")+theme(axis.title.y = element_text( face="bold",color="#5C6BC0"))+labs(title= "5 Fold CV for different models")+theme(plot.title = element_text(color="#5C6BC0", size=16, face="bold"))




"#E76BF3","#BF80FF"















