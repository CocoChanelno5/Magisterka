install.packages("extrafont")
library(extrafont)
font_import(pattern = "lmodern*")
# Run each time
library(extrafont)
loadfonts(device = "win")

theme.novpadding <-
  list(axis.line = 
         list(col =  'transparent'),
       layout.heights =
         list(top.padding = 0.2,
              main.key.padding = 0.2,
              key.axis.padding = 1,
              axis.xlab.padding = 0.5,
              xlab.key.padding = 1,
              key.sub.padding = 1,
              bottom.padding = 1),
       layout.widths =
         list(left.padding = 0.5,
              key.ylab.padding = 2,
              ylab.axis.padding = 0,
              axis.key.padding = 2,
              right.padding =0))
par(mfrow=c(5,2))
par()
f<-"Times"
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
#breaks.qt <- classIntervals(palo_alto$PrCpInc, n = 6, style = "quantile", intervalClosure = "right")

#############################################
file1<-"~/Desktop/Magisterka/Master_git/output/level"
file2<-"~/Desktop/Magisterka/Master_git/output/change"
draw_timelines_matrix<-function(data,rows, columns,text, variable, dir, point, div){
  N<-length(unique(data$Name))
  r<-rows
  c<-columns
  m<-r*c
  count<-0
  name_list<-list()
  for (i in seq(1,N,m)){
    count<-count+1
    if (i+m-1>N){
      name_list[[count]]<-unique(data$Name)[i:N]
    }else{
      name_list[[count]]<-unique(data$Name)[i:(i+m-1)]
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
  a<-mean(data$MEAN/div)
  #d_vline<-as.data.frame(unique(PL_UE$Name),v_max,v_min)
    #d[which(d$Value == max(d$Value)),]$Date
    #dmin<-d[which(d$Value == min(d$Value)),]$Date
  for (i in 1:length(name_list)){
    regions<-name_list[[i]]
    d<-data[which(data$Name %in% regions),]%>%filter(Value!=0)
    #vline_max<-vline_max[which(vline_max$Name %in% regions),]
    #vline_min<-vline_min[which(vline_min$Name %in% regions),]
    draw<-ggplot(data=d,aes(x=Date,y=Value/div,group=Name)) +
                    geom_line( color=main_colour) +
                    geom_point(shape=21, color=main_colour, fill=main_colour, size=point) +
                    theme_ipsum() +
                    #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
                    #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
                    #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
                    #ggtitle(paste0("Poziom zmiennej ",variable," w "))+
                    facet_wrap(~Name, ncol=c, nrow=r,scales = "free_y")+
                    geom_hline(aes(yintercept=MEAN/div, group=Name), color=main_colour2, size=.5)+
                    geom_hline(aes(yintercept=a), color=main_colour2, size=.5,alpha = 1/2,linetype = "dashed")+
                    geom_vline(aes(xintercept = DATE_MIN,group=Name),color="grey", size=1,alpha = 1/2)+
                    geom_vline(aes(xintercept = DATE_MAX,group=Name),color="grey", size=1,alpha = 1/2)+
                    scale_y_continuous(name=" ")+
                    theme(strip.text = element_text(family = f,size = 10),plot.title=element_text(hjust=1, vjust=0.5, face='bold',size = 15,family = f),
                          axis.text = element_text(size = 0.04, angle=50,family = f))
                    
          plot(draw)
          ggsave(paste0(dir,i,text,".png"), draw, width = 8.27, height = 11.69, units = "in")
  }
}
draw_timelines_matrix(PL_UE,7, 4,"BEZR_PL","'stopa bezrobocia'",file1,0,1)
draw_timelines_matrix(PL_GDP,7, 4,"GDP_PL","'PKB'",file1,1,1000)
draw_timelines_matrix(USA_UE,7, 4,"BEZR_USA","'stopa bezrobocia'",file1,0,1)
draw_timelines_matrix(USA_GDP,7, 4,"GDP_USA","'PKB'",file1,0,1000)

############################################# drawing timelines with grouped/selected regions
draw_timelines_selected<-function(data, lista, rows, columns,text, variable, dir, point, div=1, width = 8.27, height = 11.69){
for (i in 1:length(lista)){
  data<-data
  a<-mean(data$MEAN/div)
  d<-data[which(data$Name %in% lista_USA_GDP[[i]]),]%>%filter(Value!=0)
  library("ggsci")
  library("viridis")
  library(wesanderson)
  pal <- wes_palette("Zissou1", 100, type = "continuous")
  draw[[i]]<-ggplot(data=d,aes(x=Date,y=Value/div, group=Name, colour = Name)) +
    geom_line(size=1) +
    theme_ipsum() +
    scale_color_viridis(discrete = TRUE, option = "D") +
    #scale_fill_gradientn(colours = pal) +
    #scale_color_npg() +
    #scale_color_gradientn(colours = rainbow(5)) +
    #scale_fill_material("cyan") +
    xlab('data') +
    ylab(paste("wartość", variable)) +
    #geom_hline(aes(yintercept=a), color=main_colour2, size=.5,alpha = 1/2,linetype = "dashed")+
    scale_y_continuous(name=" ") +
    labs(color = "Podregiony:",face="bold") +
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
          axis.title.x = element_text(size = 5, family = f),
          axis.title.y = element_text(size = 5, family = f),
          #plot.title=element_text(hjust=1, vjust=0.5, face='bold',size = 5,family = f),
          axis.text.x = element_text(size = 5, angle=50,family = f),
          axis.text.y = element_text(size = 5, angle=50,family = f),
          legend.text = element_text(size = 4, family = f),
          legend.title = element_text(size = 6, family = f),
          legend.position = "right",
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(0.5,"cm"))
  plot(draw[[i]])
  
}
png(file = paste0("lev",text,".png"), width = width, height = height, units ="in",res=300)
p<-marrangeGrob(draw, nrow=rows, ncol=columns)
print(p)
dev.off()
}

draw_timelines_selected(PL_UE, lista_PL_UE, 3, 2,"BEZR_PL_sel","'stopa bezrobocia'",path2, 0, div=1, width = 8.27,height = 8.27)
draw_timelines_selected(PL_GDP, lista_PL_GDP, 3, 2,"GDP_PL_sel","'PKB'",path2, 0, div=1000, width = 8.27,height = 8.27)
draw_timelines_selected(USA_UE, lista_USA_UE, 3, 2,"BEZR_USA_sel","'stopa bezrobocia'",path2, 0, div=1, width = 8.27,height = 8.27)
draw_timelines_selected(USA_GDP, lista_USA_GDP, 3, 2,"GDP_USA_sel","'PKB'", path2, 0, div=1000, width = 8.27, height = 8.27)
  
########################## drawing single maps with yearly data
path<-"~/Desktop/Magisterka/Master_git/raw_maps/map"
path2<-"~/Desktop/Magisterka/Master_git/raw_maps/"
path3<-"~/Desktop/Magisterka/Master_git/output/"
draw_rawY_maps<-function(data,map,text,var,p,nclr,w,h){
  library(RColorBrewer)
  library(classInt)
  setwd(p)
  d<-data%>%select(ID, Name, Period, Value)%>%pivot_wider(names_from =Period, values_from = Value)
  sp <- merge(x = map, y = d, by.x = "ID", by.y = "ID")
  pal <- brewer.pal(nclr, "OrRd") # we select 7 colors from the palette
  breaks_qt <- classIntervals(data$Value/1000, n = nclr, style = "quantile")
  br <- breaks_qt$brks 
  for (i in unique(data$Period)){
    print(i)
    png(file = paste0("map",text,i,".png"), width = w, height = h)
    sp@data$bracket <- cut(sp@data[,i]/1000, breaks_qt$brks)
    print(spplot(sp, "bracket", col.regions=pal,colorkey=FALSE, 
                 main = paste0("Wartośći ",var," według regionów w roku ",i)))
    dev.off()
  }}
draw_rawY_maps(PL_UE,PL_map,"BEZR_PL","'stopa bezrobocia'",path2,8,400,400)
draw_rawY_maps(PL_GDP,PL_map,"GDP_PL","'PKB'",path2,8,400,400)
draw_rawY_maps(USA_UE,7, 4,"BEZR_USA","'stopa bezrobocia'",path2,0)
draw_rawY_maps(USA_GDP,7, 4,"GDP_USA","'PKB'",path2,0)

########################## drawing single maps
draw_raw_maps<-function(data,map,text,var,p,nclr,w,h,div,kolorki){
  library(RColorBrewer)
  library(classInt)
  setwd(p)
  #data<-PL_UE
  #map<-PL_map
  #nclr<-6
  pal <- brewer.pal(nclr, kolorki) # we select 7 colors from the palette
  breaks_qt <- classIntervals(data$Value/div, n = nclr, style = "quantile")
  br <- breaks_qt$brks 
  data$Month<-as.character(month(data$Date))
  data$Year<-as.character(year(data$Date))
  for (i in unique(data$Year)){
    d<-data%>%select(ID, Name, Date,Month,Year,Value)%>%filter(Month=="1")%>%filter(Year==i)
    sp <- merge(x = map, y = d, by.x = "ID", by.y = "ID")
    #plotclr <- brewer.pal(nclr,"PuOr")
    #class <- classIntervals(data$Value, nclr, style="quantile", dataPrecision=4)
    #colcode <- findColours(class, plotclr)
    #plot(sp,col=colcode)
    #legend(legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), cex=0.8, bty="n")
    print(i)
    png(file = paste0("map",text,i,".png"), width = w, height = h)
    sp@data$bracket <- cut(sp@data$Value/div, breaks_qt$brks)
    # plot
    print(spplot(sp, "bracket", col.regions=pal,colorkey=TRUE,par.settings = list(axis.line = list(col =  'transparent')),
                 main = paste0("Wartośći ",var," według regionów w roku ",i)))
    dev.off()
    #ggsave(paste0(p,text,".png"), draw, width = w,height = h, units = "mm")
    #8.27, height = 11.69, units = "in"))
  }}
draw_raw_maps(PL_UE,PL_map,"BEZR_PL","'stopa bezrobocia'",path2,8,400,400,1,"PuBuGn")
draw_raw_maps(PL_GDP,PL_map,"GDP_PL","'PKB'",path2,8,400,400,1000,"OrRd")
draw_raw_maps(USA_UE,USA_map,"BEZR_USA","'stopa bezrobocia'",path2,8,400,400,1,"PuBuGn")
draw_raw_maps(USA_GDP,USA_map,"GDP_USA","'PKB'",path2,8,400,400,1,"OrRd")


########################## drawing maps in matrix
draw_raw_matrixMaps<-function(data,map,var,nclr,div,kolorki,i,key,st,years){
  data$Month<-as.character(month(data$Date))
  data$Year<-as.character(year(data$Date))
  ranges<-data[which(data$Year %in% years),]%>%select(ID, Name, Date,Month,Year,Value)%>%filter(Month=="1")
  pal <- brewer.pal(nclr, kolorki) # we select 7 colors from the palette
  breaks_qt <- classIntervals(ranges$Value/div, n = nclr, style = st)
  br <- breaks_qt$brks

  d<-data%>%select(ID, Name, Date,Month,Year,Value)%>%filter(Month=="1")%>%filter(Year==i)
  sp <- merge(x = map, y = d, by.x = "ID", by.y = "ID")
  sp@data$bracket <- cut(sp@data$Value/div, breaks_qt$brks)
  spplot(sp, "bracket", lwd=0.1,col.regions=pal,colorkey=key, main =list(label=i,cex=0.8,fontfamily=f),#paste0("Wartośći ",var," według regionów w roku ",i),
         par.settings = theme.novpadding)
  }

# choice of folder to keep maps
setwd(path3)
## UNEMPLOYMENT RATE IN POLAND
require(gridExtra)
temp<-unique(year(PL_UE$Date))
png(file = paste0("mapM_BEZR_PL.png"), width = 8.27, height = 11.69, units ="in",res=300)
temp<-c(2011, 2013, 2015, 2017, 2019, 2021)
png(file = paste0("mapselM_BEZR_PL.png"), width = 8.27, height = 6.27, units ="in",res=300)
plots = lapply(temp, function(.x) draw_raw_matrixMaps(PL_UE,PL_map,"'stopa bezrobocia'",8,1,"OrRd",.x,FALSE,"equal",temp))
do.call(grid.arrange,c(plots, ncol=3))
dev.off()
png(file = paste0("legmap_BEZR_PL.png"), width = 8, height =6, units ="in",res=300)
draw_raw_matrixMaps(PL_UE,PL_map,"'stopa bezrobocia'",8,1,"OrRd",2018,list(space='left',height = 1,width =4),"equal",temp)
dev.off()

#### GDP IN POLAND
temp<-unique(year(PL_GDP$Date))
png(file = paste0("mapM_GDP_PL.png"), width = 8.27, height = 11.69, units ="in",res=300)
temp<-c(2000, 2004, 2008, 2012, 2016, 2018)
png(file = paste0("mapselM_GDP_PL.png"), width = 8.27, height = 6.27, units ="in",res=300)
plots = lapply(temp, function(.x) draw_raw_matrixMaps(PL_GDP,PL_map,"'PKB'",8,1000,"PuBuGn",.x,FALSE,"equal",temp))
do.call(grid.arrange,c(plots, ncol=3))
dev.off()
# legend
png(file = paste0("legmap_GDP_PL.png"), width = 8, height =6, units ="in",res=300)
draw_raw_matrixMaps(PL_GDP,PL_map,"'PKB'",8,1000,"PuBuGn",2018,list(space='left',height = 1,width =4),"equal",temp)
dev.off()

#### UNEMPLOYMENT RATE IN USA
temp<-unique(year(USA_UE$Date))
png(file = paste0("mapM_BEZR_USA.png"), width = 8.27, height = 11.69, units ="in",res=300)
temp<-c(2002, 2006, 2010, 2014, 2018, 2021)
png(file = paste0("mapselM_BEZR_USA.png"), width = 8.27, height = 5.27, units ="in",res=300)
plots = lapply(temp, function(.x) draw_raw_matrixMaps(USA_UE,USA_map,"'stopa bezrobocia'\n",8,1,"OrRd",.x,FALSE,"equal",temp))
do.call(grid.arrange,c(plots, ncol=3))
dev.off()
# legend
png(file = paste0("legmap_BEZR_USA.png"), width = 8, height =6, units ="in",res=300)
draw_raw_matrixMaps(USA_UE,USA_map,"'stopa bezrobocia'",8,1,"OrRd",2018,list(space='left',height = 1,width =4),"equal",temp)
dev.off()

#### GDP IN USA
temp<-unique(year(USA_GDP$Date))
png(file = paste0("mapM_GDP_USA.png"), width = 8.27, height = 11.69, units ="in",res=300)
temp<-c(2008, 2012, 2016, 2020)
png(file = paste0("mapselM_GDP_USA.png"), width = 8.27, height = 5.27, units ="in",res=300)
plots = lapply(temp, function(.x) draw_raw_matrixMaps(USA_GDP,USA_map,"'PKB'",8,1000,"PuBuGn",.x,FALSE,"equal",temp))
do.call(grid.arrange,c(plots, ncol=2))
dev.off()
# legend
png(file = paste0("legmap_GDP_USA.png"), width = 8, height =6, units ="in",res=300)
draw_raw_matrixMaps(USA_GDP,USA_map,"'PKB'",8,1000,"PuBuGn",2018,list(space='left',height = 1,width =4),"equal",temp)
dev.off()

##################### LISTY 
# USA GDP 2008, 2012, 2016, 2020
lista_USA_GDP <-list()
lista_USA_GDP[[1]]<- c("Vermont", "West Virginia", "Wyoming", "South Dakota", "North Dakota", "Rhode Island", 
"New Hampshire", "Maine", "Montana", "Delaware", "Idaho") # 11
lista_USA_GDP[[2]]<- c("Alabama", "Arkansas",  "Iowa", "Kentucky", "Nebraska", "New Mexico", 
"Kansas", "Mississippi", "Nevada",  "Oklahoma",  "South Carolina", "Utah" , "Connecticut") # 13
lista_USA_GDP[[3]]<- c("Louisiana", "Oregon", "Arizona", "Minnesota", "Maryland", "Indiana", "Missouri", 
"Tennessee", "Wisconsin", "Colorado") # 10
lista_USA_GDP[[4]]<- c("Georgia", "Michigan", "Ohio", "Pennsylvania", "Virginia", "Washington", "Massachusetts", 
"New Jersey", "North Carolina")# 9
lista_USA_GDP[[5]]<- c("Florida","Illinois", "New York", "Texas") # 4
lista_USA_GDP[[6]]<- c("California") # 1

# USA UE 2002, 2006, 2010, 2014, 2018, 2021
lista_USA_UE <-list()
lista_USA_UE[[1]] <- c("Arkansas", "Iowa", "Maryland", "Minnesota", "Virginia", "Maine", "North Dakota", 
"South Dakota", "Utah", "Wyoming", "Nebraska") #11
lista_USA_UE[[2]] <- c("Idaho", "Montana", "Connecticut", "North Carolina", "Oklahoma", "South Carolina", "Colorado",
"Kansas") #3
lista_USA_UE[[3]] <- c("Alabama", "Delaware", "Georgia", "Louisiana", "Missouri", "New Mexico", "Oregon", "Texas")  #16
lista_USA_UE[[4]] <- c("Kentucky", "Ohio", "Wisconsin", "Rhode Island", "Vermont", "Mississippi", "Arizona", "Florida") #8
lista_USA_UE[[5]] <- c("California", "Illinois", "Indiana", "New York", "Pennsylvania", "Tennessee", 
"Washington", "Massachusetts", "New Hampshire", "New Jersey", "West Virginia")  #11
lista_USA_UE[[6]] <- c("Nevada", "Michigan")  #2

# PL UE 2011, 2013, 2015, 2017, 2019, 2021
lista_PL_UE <-list()
lista_PL_UE[[1]] <- c("Miasto Kraków", "Miasto Szczecin", "Miasto Wrocław", "Miasto Warszawa", "Miasto Poznań", "Miasto Łódź") #6
lista_PL_UE[[2]] <- c("Kaliski", "Poznański", "Trójmieski", "Gliwicki", "Katowicki", "Leszczyński", "Lubelski", "Tyski", 
"Bielski", "Rybnicki", "Warszawski Zachodni", #11
"Bydgosko-toruński", "Nowotarski", "Opolski", "Siedlecki", "Skierniewicki", "Wrocławski", 
"Legnicko-głogowski", "Krakowski", "Żyrardowski") #9
lista_PL_UE[[3]] <- c("Łomżyński", "Puławski", "Sandomiersko-jędrzejowski", "Oświęcimski", "Bytomski", "Koniński", "Łódzki", "Piotrkowski", "Rzeszowski", 
"Sieradzki", "Tarnobrzeski","Tarnowski", "Warszawski Wschodni", "Częstochowski") #14
lista_PL_UE[[4]] <- c("Białostocki", "Pilski", "Gdański", "Zielonogórski", "Sosnowiecki", "Gorzowski", "Bialski", "Kielecki", "Ostrołęcki", 
"Nowosądecki", "Suwalski", "Chełmsko-zamojski", "Ciechanowski", "Przemyski", "Krośnieński") #15
lista_PL_UE[[5]] <- c("Nyski", "Olsztyński", "Słupski", "Chojnicki", "Elbląski", "Jeleniogórski", "Koszaliński", "Starogardzki", 
"Szczeciński", "Wałbrzyski", "Grudziącki", "Płocki", "Świecki") #13
lista_PL_UE[[6]] <- c("Ełcki", "Radomski", "Włocławski", "Szczeciniecko-pyrzycki", "Inowrocławski") #5

# PL GDP 2000, 2004, 2008, 2012, 2016, 2018
lista_PL_GDP <-list()
lista_PL_GDP[[1]] <- c("Miasto Kraków", "Miasto Szczecin", "Miasto Wrocław", "Miasto Warszawa", "Miasto Poznań", "Miasto Łódź") #6
lista_PL_GDP[[2]] <- c("Chojnicki", "Suwalski", "Bialski", "Ełcki", "Łomżyński", "Nowotarski", "Nyski", "Ostrołęcki", "Puławski", "Siedlecki", 
"Słupski", "Skierniewicki", "Włocławski", "Ciechanowski", "Grudziądzki", "Inowrocławski", "Przemyski", "Świecki", 
"Krośnieński", "Żyrardowski", "Gorzowski") # 20
lista_PL_GDP[[3]] <- c("Bytomski", "Elbląski", "Jeleniogórski", "Koszaliński", "Łódzki", "Nowosądecki", "Pilski", "Sieradzki", "Starogardzki", 
"Szczeciniecko-pyrzycki", "Szczeciński", "Tarnowski", "Radomski", "Chełmsko-zamojski", "Sandomiersko-jędrzejowski") #15
lista_PL_GDP[[4]] <- c("Gliwicki", "Rybnicki", "Koniński", "Leszczyński", "Piotrkowski", "Rzeszowski", "Tarnobrzeski", "Tyski", "Wałbrzyski", 
"Warszawski Wschodni", "Białostocki", "Gdański", "Olsztyński", "Opolski", "Oświęcimski", "Zielonogórski", 
"Częstochowski", "Krakowski", "Płocki") #19
lista_PL_GDP[[5]] <- c("Legnicko-głogowski", "Lubelski", "Bydgosko-toruński", "Kaliski", "Kielecki", "Poznański", "Wrocławski", "Sosnowiecki", 
"Warszawski Zachodni", "Bielski") #10
lista_PL_GDP[[6]] <- c("Katowicki", "Trójmiejski") # 2



#################### function which draws the map with colours according to Value for one period
draw_map_variable <- function(map, data, cut, variable, year, title,kolorki) {
  
  data<-filter(data,Period==year)
  sp <- merge(x = map, y = data, by.x = "ID", by.y = "ID")
  text<-list("sp.text", coordinates(sp), as.character(sp@data$Name),col="black", cex=0.5,font=1)
  drawing<-spplot(sp, zcol = variable, colorkey = FALSE, col.regions = kolorki#(cut) 
                  ,cuts = cut,sp.layout = list(text),do.log=TRUE,
                  par.settings = list(axis.line = list(col =  'transparent')),
                  main = list(label=paste("Podział",title),cex=0.8,fontfamily=f))
                  #main = paste("Wartości",title,"w roku",year))
  return(drawing)
}

png(file = paste0("map_names_PL.png"), width = 4, height = 4, units ="in",res=300)
draw_map_variable(PL_map, PL_GDP, cuts-1, "Value", 2000, "Polski na 73 regiony","white")
dev.off()

draw_usamap_variable <- function(map, data, cut, variable, year, per, title, kolorki) {
  data$Year<-year(data$Date)
  data$Month<-month(data$Date)
  data<-data%>%filter(Year==year)%>%filter(Month==per)
  sp <- merge(y = data, x = map, by.y = "Name", by.x = "NAME")
  text<-list("sp.text", coordinates(sp), as.character(sp@data$NAME),col="black", cex=0.5,font=1.5)
  drawing<-spplot(sp, zcol = variable, colorkey = FALSE, col.regions = kolorki#(cut) 
                  ,cuts = cut,sp.layout = list(text),do.log=TRUE,
                  par.settings = list(axis.line = list(col =  'transparent')),
                  main = list(label=title,cex=0.8,fontfamily=f))
                  #main = paste0("Wartości ",title,"w roku ",year," w miesiącu ",per))
  return(drawing)
}

png(file = paste0("map_names_USA.png"), width = 4, height = 4, units ="in",res=300)
draw_usamap_variable(USA_map, USA_UE, 1, "Value", 2010,1,"Podział USA na 48 stany", "white")
dev.off()


############ OLD

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