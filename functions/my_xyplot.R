my_xyplot<-function(x,y,myxlab,myylab,mycolor){
  p =
  ggplot(data.frame(x =x, y =y), aes(x=x,y=y))+
    
           geom_point(col=mycolor,alpha=0.7)+
           
           ggtitle('',subtitle = paste('r=',round(cor(x,y),3)))+
           
           xlab(myxlab)+ylab(myylab)+ 
           
    #xlim(0,1)+ylim(0,1)+
    theme_classic()
   
return(p)
  
}
