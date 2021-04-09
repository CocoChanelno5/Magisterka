path<-"~/Desktop/Magisterka/Master_git/output/level"
path2<-"~/Desktop/Magisterka/Master_git/output/change"
draw_timelines_matrix<-function(data,rows, columns,text, variable, dir, point){
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
                    geom_point(shape=21, color=main_colour, fill=main_colour, size=point) +
                    theme_ipsum() +
                    #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
                    #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
                    #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
                    ggtitle(paste0("Poziom zmiennej ",variable," w "))+
                    facet_wrap(~Name, ncol=c, nrow=r,scales = "free_y")+
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
draw_timelines_matrix(PL_UE,7, 4,"BEZR_PL","'stopa bezrobocia'",path,0)
draw_timelines_matrix(PL_GDP,7, 4,"GDP_PL","'PKB'",path,1)
draw_timelines_matrix(USA_UE,7, 4,"BEZR_USA","'stopa bezrobocia'",path,0)
draw_timelines_matrix(USA_GDP,7, 4,"GDP_USA","'PKB'",path,0)

################### DRAWING MAPS WITH DATA #####################
#illustrate variable http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf?utm_source=twitterfeed&utm_medium=twitter
main_colour <- "navy"
main_colour2<- "deeppink3"
# pink2, ppink3, violetred3, navy, blue3
pal <- colorRampPalette(c(main_colour2, main_colour), bias = 1)
library(RColorBrewer)
# display.brewer.all()
# PuBu, Blues,RdPu, PuBuGn
cuts <- 9
my.palette <- brewer.pal(n = cuts, name = "OrRd")
# controling breaks
#library(classInt)
breaks.qt <- classIntervals(palo_alto$PrCpInc, n = 6, style = "quantile", intervalClosure = "right")
spplot(palo_alto, "PrCpInc", col = "transparent", col.regions = my.palette, at = breaks.qt$brks)

##########################
path<-"~/Desktop/Magisterka/Master_git/raw_maps/map"
draw_rawY_maps<-function(data,map,text,var,p,nclr,w,h){
  library(RColorBrewer)
  library(classInt)

  d<-data%>%select(ID, Name, Period, Value)%>%pivot_wider(names_from =Period, values_from = Value)
  sp <- merge(x = map, y = d, by.x = "ID", by.y = "ID")
  #plotclr <- brewer.pal(nclr,"PuOr")
  #class <- classIntervals(data$Value, nclr, style="quantile", dataPrecision=4)
  #colcode <- findColours(class, plotclr)
  #plot(sp,col=colcode)
  #legend(legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), cex=0.8, bty="n")
  pal <- brewer.pal(nclr, "OrRd") # we select 7 colors from the palette
  breaks_qt <- classIntervals(data$Value/1000, n = nclr, style = "quantile")
  br <- breaks_qt$brks 
  # categoreis for choropleth map
  for (i in unique(data$Period)){
    print(i)
    png(file = paste0("map",text,i,".png"), width = w, height = h)
    sp@data$bracket <- cut(sp@data[,i]/1000, breaks_qt$brks)
    # plot
    print(spplot(sp, "bracket", col.regions=pal,colorkey=FALSE, 
                 main = paste0("Wartośći ",var," według regionów w roku ",i)))
    dev.off()
    #ggsave(paste0(p,text,".png"), draw, width = w,height = h, units = "mm")
    #8.27, height = 11.69, units = "in"))
  }}
draw_rawY_maps(PL_UE,PL_map,"BEZR_PL","'stopa bezrobocia'",path,8,400,400)
draw_rawY_maps(PL_GDP,PL_map,"GDP_PL","'PKB'",path,8,400,400)
draw_rawY_maps(USA_UE,7, 4,"BEZR_USA","'stopa bezrobocia'",path,0)
draw_rawY_maps(USA_GDP,7, 4,"GDP_USA","'PKB'",path,0)


# function which draws the map with colours according to Value for one period
draw_map_variable <- function(map, data, cut, variable, year, title,kolorki) {
  
  data<-filter(data,Period==year)
  sp <- merge(x = map, y = data, by.x = "ID", by.y = "ID")
  text<-list("sp.text", coordinates(sp), as.character(sp@data$Name),col="black", cex=0.5,font=2)
  drawing<-spplot(sp, zcol = variable, colorkey = TRUE, col.regions = kolorki#(cut) 
                  ,cuts = cut,sp.layout = list(text),do.log=TRUE,
                  par.settings = list(axis.line = list(col =  'transparent')),
                  main = paste("Wartości",title,"w roku",year))
  return(drawing)
}
draw_map_variable(PL_map, PL_GDP, cuts-1, "Value", 2018, "GDP",my.palette)

draw_map_variable(PL_map, PL_GDP, cuts-1, "Value", 2018, "GDP",my.palette)


data<-PL_GDP
map<-PL_map
drawing<-spplot(sp, zcol = "Value", colorkey = TRUE, col.regions = my.palette#(cut) 
                ,cuts = cuts,sp.layout = list(text),do.log=TRUE,
                par.settings = list(axis.line = list(col =  'transparent')),
                main = paste("Wartości w roku ",2018))


draw_usamap_variable <- function(map, data, cut, variable, year, per, title, kolorki) {
  data$Year<-year(data$Date)
  data$Month<-month(data$Date)
  data<-data%>%filter(Year==year)%>%filter(Month==per)
  sp <- merge(y = data, x = map, by.y = "Name", by.x = "NAME")
  text<-list("sp.text", coordinates(sp), as.character(sp@data$NAME),col="black", cex=0.5,font=2)
  drawing<-spplot(sp, zcol = variable, colorkey = TRUE, col.regions = kolorki#(cut) 
                  ,cuts = cut,sp.layout = list(text),do.log=TRUE,
                  par.settings = list(axis.line = list(col =  'transparent')),
                  main = paste0("Wartości ",title,"w roku ",year," w miesiącu ",per))
  return(drawing)
}

draw_usamap_variable <- function(map, data, cut, variable, per, title, kolorki) {
  data$Year<-year(data$Date)
  data$Month<-month(data$Date)
  drawing<-list()
  for (i in 1:length(USA_years)){
    d<-data%>%filter(Year==(i+1998))%>%filter(Month==per)
    sp <- merge(y = d, x = map, by.y = "Name", by.x = "NAME")
    text<-list("sp.text", coordinates(sp), as.character(sp@data$NAME),col="black", cex=0.5,font=2)
    drawing[[i]]<-spplot(sp, zcol = variable, colorkey = TRUE, col.regions = kolorki#(cut) 
                    ,cuts = cut,sp.layout = list(text),do.log=TRUE,
                    par.settings = list(axis.line = list(col =  'transparent')),
                    main = paste0("Wartości ",title," w roku ",i," w miesiącu ",per))}
  return(drawing)
}
s<-draw_usamap_variable(USA_map, USA_UE, cuts-1, "Value", 4,"GDP", my.palette)

setwd("~/Desktop/Magisterka/Master_git/output")
library(graphics)
png(file = "cos.png", width = 1700, height = 2000, units = "px")
par(mfrow = c(5, 5))
for (i in USA_years){
  print(i)
  i<-as.numeric(i)
  draw_usamap_variable(USA_map, USA_UE, cuts-1, "Value", i, 4,"GDP", my.palette)
}

draw_usamap_variable(USA_map, USA_GDP, cuts-1, "Value", 2018,4,"GDP", my.palette)
draw_usamap_variable(USA_map, USA_UE, cuts-1, "Value", 2018,4,"GDP", my.palette)

op <- par(mfrow=c(3,2)) # funkcja dzielaca obszar roboczy na rzędy i kolumny
lapply(2005:2018,function(i){
  plot(draw_usamap_variable(USA_map, USA_UE, cuts-1, "Value", i, 4,"GDP", my.palette))# funkcja main to tytul wykresu
})
par(op)


par(mfrow = c(3, 2))  # 3 rows and 2 columns
for (i in c("Sturges", "st", "Scott", "sc", "FD", "fr")) {
  hist(cars$speed, breaks = i, main = paste("method is", i, split = ""))
}




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