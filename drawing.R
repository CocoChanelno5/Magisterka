draw_timelines_matrix<-function(data,rows, columns,text){
  N<-length(unique(data$Name))
  r<-rows
  c<-columns
  m<-r*c
  count<-0
  name_list<-list()
  for (i in seq(1,N,28)){
    count<-count+1
    if (i+r*c-1>N){
      name_list[[count]]<-unique(data$Name)[i:N]
    }else{
      name_list[[count]]<-unique(data$Name)[i:(i+r*c-1)]
    }
  }
  m<-mean(d$Value)
  dmax<-d[which(d$Value == max(d$Value)),]$Date
  dmin<-d[which(d$Value == min(d$Value)),]$Date
  for (i in 1:length(name_list)){
    regions<-name_list[[i]]
    d<-data[which(data$Name %in% regions),]%>%filter(Value!=0)
    draw<-ggplot(data=d,aes(x=Date,y=Value)) +
                    geom_line( color=main_colour) +
                    geom_point(shape=21, color=main_colour, fill=main_colour, size=1) +
                    theme_ipsum() +
                    theme(plot.title=element_text( hjust=1, vjust=0.5, face='bold'))+
                    #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
                    #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
                    #geom_hline(yintercept=m, color=main_colour2, size=.5)+
                    #geom_vline(xintercept = dmax,color="grey", size=2,alpha = 1/2)+
                    #geom_vline(xintercept = dmin,color="grey", size=2,alpha = 1/2)+
                    #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
                    ggtitle(paste0("Level of GDP in "))+
                    facet_wrap(~Name, ncol=c, nrow=r)
    plot(draw)
    ggsave(paste0("~/Desktop/Magisterka/Master_git/output/level",i,text,".png"), draw)
  }
}
draw_timelines_matrix(PL_ST,4, 4,"GDP_PL")






draw_timeseries<-function(data){

for (i in unique(data$ID)){
  d<-data%>%filter(ID==i) %>%filter(Value!=0)
  t<-d$Name
  m<-mean(d$Value)
  dmax<-d[which(d$Value == max(d$Value)),]$Date
  dmin<-d[which(d$Value == min(d$Value)),]$Date
  
  draw<-ggplot(data=d,aes(x=Date,y=Value)) +
    geom_line( color=main_colour) +
    geom_point(shape=21, color=main_colour, fill=main_colour, size=10) +
    theme_ipsum() +
    theme(plot.title=element_text(size=1, hjust=1, vjust=0.5, face='bold'))+
    #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
    #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
    geom_hline(yintercept=m, color=main_colour2, size=.5)+
    geom_vline(xintercept = dmax,color="grey", size=2,alpha = 1/2)+
    geom_vline(xintercept = dmin,color="grey", size=2,alpha = 1/2)+
    #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
    ggtitle(paste0("Level of GDP in ",t))
  plot(draw)
}
}

draw_timeseries(PL_ST)