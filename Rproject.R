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

for(i in varnames[2:6]){
  ggplot(mutate(tryptone,Time=factor(Time)))+ geom_point(mapping = aes_string(x="Conc",y=i,color="Time"),size=4)+facet_wrap(~Temp)
  ggsave(filename=paste("interaction_plot_",i,".png",sep=""))
}

for(i in varnames[2:6]){
  plots[[i]] <- ggplot(data=tryptone)+geom_point(aes_string(x='Conc',y=i))
}
ggarrange(plotlist=plots)
ggsave(filename=paste("plot_count_Conc",".png",sep=""))

for(i in varnames[2:6]){
  plots[[i]] <- ggplot(data=tryptone)+geom_point(aes_string(x='Temp',y=i))
}
ggarrange(plotlist=plots)
ggsave(filename=paste("plot_count_Temp",".png",sep=""))

for(i in varnames[2:6]){
  plots[[i]] <- ggplot(data=tryptone)+geom_point(aes_string(x='Time',y=i))
}
ggarrange(plotlist=plots)
ggsave(filename=paste("plot_count_Time",".png",sep=""))

newvarnames = c("meancount1","meancount2","meancount3","meancount4","meancount5")
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
  for(i in 1:5){
    if(i==1){
      aux_list <- list(interp(~mean(x),x=as.name(varnames[[i+1]])))
      aux_list <- setNames(aux_list,newvarnames[[i]])
      chuju <- group_by_(tryptone,varnames[[n+6]]) %>% summarise_(.dots=aux_list)
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
