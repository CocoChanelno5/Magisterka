path<-"~/Desktop/Magisterka/Master_git/output/level"
path2<-"~/Desktop/Magisterka/Master_git/output/change"
draw_timelines_matrix<-function(data,rows, columns,text, variable, dir){
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
  #data<-PL_GDP
  l_max<-list()
  l_min<-list()
  v_max<-c()
  v_min<-c()
  n_min<-c()
  n_max<-c()
  for (i in unique(data$Name)){
    d<-data%>%filter(Name==i)
    l_max[[i]]<-d[which(d$Value == max(d$Value)),]$Date
    l_min[[i]]<-d[which(d$Value == min(d$Value)),]$Date
    v_max<-append(v_max,l_max[[i]][1])
    v_min<-append(v_min,l_min[[i]][1])
    n_max<-append(n_max,unique(d$Name))
    n_min<-append(n_min,unique(d$Name))
    #n_max<-append(n_max,rep(i,length(l_max[[i]])))
    #n_min<-append(n_min,rep(i,length(l_min[[i]])))
  }
  vline_max<-as.data.frame(cbind(n_max,as.character(v_max)))
  vline_min<-as.data.frame(cbind(n_min,as.character(v_min)))
  names(vline_max)<-c("Name","DATE_MAX")
  vline_max$DATE_MAX<-as.Date(ymd(vline_max$DATE_MAX))
  names(vline_min)<-c("Name","DATE_MIN")
  vline_min$DATE_MIN<-as.Date(ymd(vline_min$DATE_MIN))
  #data<-cbind(data,DATE_MIN=vline_min$DATE_MIN,DATE_MAX=vline_max$DATE_MAX)
  data<-left_join(data, vline_max, by = "Name")
  data<-left_join(data, vline_min, by = "Name")

  #d_vline<-as.data.frame(unique(PL_UE$Name),v_max,v_min)
    #d[which(d$Value == max(d$Value)),]$Date
    #dmin<-d[which(d$Value == min(d$Value)),]$Date
  for (i in 1:length(name_list)){
    regions<-name_list[[i]]
    d<-data[which(data$Name %in% regions),]%>%filter(Value!=0)
    #vline_max<-vline_max[which(vline_max$Name %in% regions),]
    #vline_min<-vline_min[which(vline_min$Name %in% regions),]
    draw<-ggplot(data=d,aes(x=Date,y=Value,group=Name)) +
                    geom_line( color=main_colour) +
                    geom_point(shape=21, color=main_colour, fill=main_colour, size=1) +
                    theme_ipsum() +
                    #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
                    #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
                    #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
                    ggtitle(paste0("Level of ",variable," in "))+
                    facet_wrap(~Name, ncol=c, nrow=r)+
                    geom_hline(aes(yintercept=MEAN, group=Name), color=main_colour2, size=.5)+
                    geom_hline(aes(yintercept=mean(MEAN)), color=main_colour2, size=.5,alpha = 1/2,linetype = "dashed")+
                    geom_vline(aes(xintercept = DATE_MIN,group=Name),color="grey", size=1,alpha = 1/2)+
                    geom_vline(aes(xintercept = DATE_MAX,group=Name),color="grey", size=1,alpha = 1/2)+
                    theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold',size = 15),
                          strip.text.x = element_text(size = 10),strip.text.y = element_text(size = 2),axis.text= element_text(size = 0.8,angle=50),text = element_text(size = 1))
          plot(draw)
          ggsave(paste0(dir,i,text,".png"), draw, width = 8.27, height = 11.69, units = "in")
  }
}
draw_timelines_matrix(PL_UE,7, 4,"BEZR_PL","unemployment rate",path)
draw_timelines_matrix(PL_GDP,7, 4,"GDP_PL","GDP",path)
draw_timelines_matrix(USA_UE,7, 4,"BEZR_USA","unemployment rate",path)
draw_timelines_matrix(USA_GDP,7, 4,"GDP_USA","GDP",path)



draw_timeseries<-function(data,variable){

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
    #geom_vline(xintercept = dmax,color="grey", size=2,alpha = 1/2)+
    #geom_vline(xintercept = dmin,color="grey", size=2,alpha = 1/2)+
    #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
    ggtitle(paste0("Level of ",variable," in ",t))
  plot(draw)
}
}

draw_timeseries(PL_ST)