library("tidyverse")
library("ggpubr")
tryptone <- read_table("/home/norbert/Desktop/R/Tryptone.dat.txt")
ggplot(data=tryptone)+ geom_point(mapping = aes(x=Conc,y=Count1,color=factor(Time)),size=4)+facet_wrap(~Temp)
#ggplot(data=tryptone)+ geom_point(mapping = aes(x=Conc,y=Count2,color=Time,size=3))+facet_wrap(~Temp)
#ggplot(data=tryptone)+ geom_bar(mapping = aes(x=Conc,y=Count1,color=Time,size=3))+facet_wrap(~Temp)
library(lazyeval)
ggplot(data=tryptone)+geom_point(aes(Conc,Count1),size=5)
plots <- list()
varnames <- names(tryptone)

for(i in varnames[c(2:6,10)]){
  ggplot(mutate(tryptone,Time=factor(Time)))+ geom_point(mapping = aes_string(x="Conc",y=i,color="Time"),size=4)+facet_wrap(~Temp)
  ggsave(filename=paste("interaction_plot_",i,".png",sep=""))
}

for(i in varnames[c(2:6,10)]){
  plots[[i]] <- ggplot(data=tryptone)+geom_point(aes_string(x='Conc',y=i))
}
ggarrange(plotlist=plots)
ggsave(filename=paste("plot_count_Conc",".png",sep=""))

for(i in varnames[c(2:6,10)]){
  plots[[i]] <- ggplot(data=tryptone)+geom_point(aes_string(x='Temp',y=i))
}
ggarrange(plotlist=plots)
ggsave(filename=paste("plot_count_Temp",".png",sep=""))
aux_list=list()
nested_plotlist = list()
plots = list()
df_list=list()
for(n in 1:3){
  for(i in 1:6){
    if(i==1){
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[i+1]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- group_by_(tryptone,varnames[[n+6]]) %>% summarise_(.dots=aux_list)
    }
    else if(i==6){
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[10]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- merge(chuju,group_by_(tryptone,varnames[[n+6]]) %>% summarise_(.dots=aux_list))
      
    }
    else{
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[i+1]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- merge(chuju,group_by_(tryptone,varnames[[n+6]]) %>% summarise_(.dots=aux_list))
      
    }
    plots[[i]] <- ggplot(data=chuju)+geom_bar(aes_string(x=varnames[[n+6]],y=newvarnames[[i]]),stat="identity")
  }
  nested_plotlist[[n]] <- plots
  df_list[[n]] <- chuju
  
}
for(i in 1:3){
  ggarrange(plotlist=nested_plotlist[[i]])
  ggsave(filename=paste("plot_meancount_",varnames[[i+6]],".png",sep=""))
}


newvarnames = c("meancount1","meancount2","meancount3","meancount4","meancount5","mean_meancount")
mean_count <- group_by(tryptone,Time) %>% summarise(totalcount1=sum(Count1))

ggplot(totalcount1)+geom_bar(aes(Time,totalcount1),stat="identity")
sum(tryptone["Count1"])
sum(totalcount1["totalcount1"])
summarise(tryptone,totalcount1=sum(Count1))
mutate(tryptone,totalcount1=sum(Count1))

mean_count_time <- group_by(tryptone,Time) %>% summarise(meancount1=mean(as.name("Count1")))
#JAPIERDOLE
#list1=list()
#list1[1]="newvarnames[[1]]=~mean(Count1)"
#list1=list(~mean(tryptone[[i]]),~sum(Count1))
#list2 <- setNames(list1,c(newvarnames[[1]],newvarnames[[2]]))
#list2[["meancount2"]]
#KURWAWAAA JEST

ggarrange(plotlist = nested_plotlist[[3]])
aux_list=list()
nested_plotlist = list()
plots = list()
df_list=list()
for(n in 1:3){
  for(i in 1:6){
    if(i==1){
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[i+1]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- group_by_(tryptone,varnames[[n+6]]) %>% summarise_(.dots=aux_list)
    }
    else if(i==6){
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[10]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- merge(chuju,group_by_(tryptone,varnames[[i+6]]) %>% summarise_(.dots=aux_list))
      
    }
    else{
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[i+1]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- merge(chuju,group_by_(tryptone,varnames[[i+6]]) %>% summarise_(.dots=aux_list))
      
    }
    plots[[i]] <- ggplot(data=chuju)+geom_bar(aes_string(x=varnames[[n+6]],y=newvarnames[[i]]),stat="identity")
  }
  nested_plotlist[[n]] <- plots
  df_list[[n]] <- chuju
  
}
for(i in 1:3){
  ggarrange(plotlist=nested_plotlist[[i]])
  ggsave(filename=paste("plot_meancount_",varnames[[i+6]],".png",sep=""))
}


aux_list=list()
nested_plotlist = list()
plots = list()
df_list=list()
for(n in 1:6){
  for(i in 1:3){
    
    if(n==6){
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[10]])))
      aux_list <- setNames(aux_list,newvarnames[[n]])
      chuju <- group_by_(tryptone,varnames[[i+6]]) %>% summarise_(.dots=aux_list)
      
    }
    
    else{
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[n+1]])))
      aux_list <- setNames(aux_list,newvarnames[[n]])
      chuju <- group_by_(tryptone,varnames[[i+6]]) %>% summarise_(.dots=aux_list)
    }
    plots[[i]] <- ggplot(data=chuju)+geom_bar(aes_string(x=varnames[[i+6]],y=newvarnames[[n]]),stat="identity")
  }
  nested_plotlist[[n]] <- plots
  df_list[[n]] <- chuju
  
}
for(i in 1:6){
  ggarrange(plotlist=nested_plotlist[[i]])
  ggsave(filename=paste("plot_meancount_",newvarnames[[i]],".png",sep=""))
}

ggplot(mutate(chuju,Conc=factor(Conc)))+geom_bar(aes(x=Conc,y=mean_meancount),stat="identity")





lmresult1 <- lm(Count1~Time+Temp+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Time+Temp+Conc+Time:Temp,data=tryptone)
summary(lmresult1)

lmresult1 <- lm(Count1~Time,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(formula = Count1 ~ Time + I(Time^3),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(formula = Count1 ~ Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(formula = Count1 ~ Conc + I(Conc^2),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(formula = Count1 ~ Conc + I(Conc^2)+I(Conc^3),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(formula = Count1 ~ Conc + I(Conc^2)+I(Conc^3)+I(Conc^4),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(formula = Count1 ~ Conc + I(Conc^2)+I(Conc^3)+I(Conc^4)+I(Conc^5),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+I(Temp^2),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+I(Temp^2)+I(Temp^3),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+Time+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+I(Temp^2)+Time+Conc,data=tryptone)
summary(lmresult1)

lmresult1 <- lm(Count1~Temp+Time,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+Time+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+Time+I(Time*Temp),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+Conc+I(Temp*Conc),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Time+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Time+Conc+I(Time*Conc),data=tryptone)
summary(lmresult1)

lmresult1 <- lm(Count1~Temp+I(Temp^2)+Time+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+I(Temp^2)+Time+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+I(Temp^2)+Time+Conc+I(Temp*Time)+I(Temp^2*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(Count1~Temp+I(Temp*Time)+Time+Conc,data=tryptone)
summary(lmresult1)


lmresult1 <- lm(Count1~Temp+Time+Conc+I(Temp*Time)+I(Conc*Time)+I(Conc*Temp)+I(Conc*Time*Temp),data=tryptone)
summary(lmresult1)

tryptone$meancount <- (tryptone$Count1+tryptone$Count2+tryptone$Count3+tryptone$Count4+tryptone$Count5)/5

lmresult1 <- lm(meancount~Temp+Time+Conc+I(Temp*Time)+I(Conc*Time)+I(Conc*Temp)+I(Conc*Time*Temp),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+I(Conc^3)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+I(Conc^3)+I(Conc^4)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+I(Conc^3)+I(Conc^4)+I(Conc^5)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)

lmresult1 <- lm(meancount~Temp+Time+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+Time+Conc+I(Conc^2),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+Time+Conc+I(Conc^2)+I(Conc^3),data=tryptone)
summary(lmresult1)

lmresult1 <- lm(meancount~Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Conc+I(Conc^2),data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Conc+I(Conc^2)+I(Conc^3),data=tryptone)
summary(lmresult1)

result2 <- lm(meancount~Temp+I(Temp^2)+Time+Conc+I(Temp*Time),data=tryptone)
summary(result2)
result2 <- lm(meancount~Temp+Time+Conc+I(Temp*Time),data=tryptone)
summary(result2)
lmresult3 <- lm(meancount~Temp+I(Temp^2)+Time+Conc,data=tryptone)
summary(lmresult3)result2 %>% 
  augment() %>% 
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0), col = "red") + 
  xlab("Fitted Hardness_cement") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs fitted Hardness_cement") + 
  theme_bw() + 
  geom_linerange(aes(ymin = -.resid, ymax = .resid))

#CHUJCIWDUPE

plot(tryptone$Temp,tryptone$Conc)

res <- result2$residuals
Y_hat <- result2$fitted.values
result2 %>% 
  augment() %>% 
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0), col = "red") + 
  xlab("Fitted Hardness_cement") + 
  ylab("Residuals") + 
  ggtitle("Residuals vs fitted Hardness_cement") + 
  theme_bw() + 
  geom_linerange(aes(ymin = -.resid, ymax = .resid))

ggplot(tryptone)+geom_point(aes(Conc,meancount))+geom_point(data=filter(tryptone,Row==9|Row==14|Row==29),aes(Conc,meancount,color="red"))
filter(tryptone,Row==9)

ggplot(mutate(tryptone,Time=factor(Time)))+ geom_point(mapping = aes_string(x="Conc",y="meancount",color="Time"),size=4)+geom_point(data=filter(tryptone,Row==9|Row==14|Row==29),aes(Conc,meancount,shape="k"))+facet_wrap(~Temp)


lmresult1 <- lm(meancunt~Temp+I(Temp^2)+Time+Conc,data=tryptone)
summary(lmresult1)
lmresult1 <- lm(meancount~Temp+I(Temp^2)+Time+Conc+I(Temp*Time),data=tryptone)
summary(lmresult1)
lmresult2 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult2)
lmresult3 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+I(Conc^3)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult3)
lmresult4 <- lm(meancount~Temp+I(Temp^2)+Time+I(Conc^2)+I(Conc^3)+I(Conc^4)+Conc+I(Temp*Time),data=tryptone)
summary(lmresult4)

y_vec2 <- -1.305e+03+5.461e+01*35-6.419e-01*(35)^2+1.061e+01*48-2.073e-01*(35*48)+3.460e+02*concvec2-1.265e+02*(concvec2)^2
plot(concvec2,y_vec)
y_vec <- -6.093e+02+5.461e+01*35-6.419e-01*(35)^2+1.061e+01*48-2.073e-01*(35*48)-1.960e+03*concvec2+2.289e+03*(concvec2)^2-8.052e+02*(concvec2)^3
y_vec <- -3.831e+03+5.461e+01*35-6.419e-01*(35)^2+1.061e+01*48-2.073e-01*(35*48)+1.226e+04*concvec-2.042e+04*(concvec)^2+1.480e+04*(concvec)^3-3.900e+03*(concvec)^4
df <- data.frame(concvec,y_vec)
names(df) <- c("x","y")
concvec2 <- seq(0.4,1.8,length=100)
y_vec2 <- -3.831e+03+5.461e+01*35-6.419e-01*(35)^2+1.061e+01*48-2.073e-01*(35*48)+1.226e+04*concvec2-2.042e+04*(concvec2)^2+1.480e+04*(concvec2)^3-3.900e+03*(concvec2)^4
df2 <- data.frame(concvec2,y_vec2)
names(df2) <- c("x","y")
ggplot(filter(tryptone,Temp==35&Time==48))+geom_point(aes(x=Conc,y=meancount))+geom_line(data=df2,aes(x,y))
