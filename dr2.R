theme.novpadding <-
  list(layout.heights =
         list(top.padding = 0,
              main.key.padding = 0,
              key.axis.padding = 0,
              axis.xlab.padding = 0,
              xlab.key.padding = 0,
              key.sub.padding = 0,
              bottom.padding = 0),
       layout.widths =
         list(left.padding = 0,
              key.ylab.padding = 0,
              ylab.axis.padding = 0,
              axis.key.padding = 0,
              right.padding = 0))
par(mfrow = c(1, 1))
par()

path<-"~/Desktop/Magisterka/Master_git/raw_maps/map"
draw_raw_maps<-function(data,map,text,var,p,nclr,w,h,div,kolorki){
      library(RColorBrewer)
      library(classInt)
      #data<-PL_UE
      #map<-PL_map
      #nclr<-6
      pal <- brewer.pal(nclr, kolorki) # we select 7 colors from the palette
      breaks_qt <- classIntervals(data$Value/1, n = nclr, style = "quantile")
      br <- breaks_qt$brks 
      data$Month<-as.character(month(data$Date))
      data$Year<-as.character(year(data$Date))
      for (i in unique(data$Year)){
        d<-data%>%select(ID, Name, Date,Month,Year,Value)%>%filter(Month=="1")%>%filter(Year=="2018")
        sp <- merge(x = map, y = d, by.x = "ID", by.y = "ID")
      #plotclr <- brewer.pal(nclr,"PuOr")
      #class <- classIntervals(data$Value, nclr, style="quantile", dataPrecision=4)
      #colcode <- findColours(class, plotclr)
      #plot(sp,col=colcode)
      #legend(legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), cex=0.8, bty="n")
        print(i)
        png(file = paste0("map",text,i,".png"), width = w, height = h)
        sp@data$bracket <- cut(sp@data$Value/1, breaks_qt$brks)
        # plot
        print(spplot(sp, "bracket", col.regions=pal,colorkey=FALSE,
                     main = paste0("Wartośći ",var," według regionów w roku ",i)))
        dev.off()
        #ggsave(paste0(p,text,".png"), draw, width = w,height = h, units = "mm")
                 #8.27, height = 11.69, units = "in"))
      }}
draw_raw_maps(PL_UE,PL_map,"BEZR_PL","'stopa bezrobocia'",path,3,400,400,1,"OrRd")
draw_raw_maps(PL_GDP,PL_map,"GDP_PL","'PKB'",path,8,400,400,1000,"OrRd")
draw_raw_maps(USA_UE,USA_map,"BEZR_USA","'stopa bezrobocia'",path,3,400,400,1,"OrRd")
draw_raw_maps(USA_GDP,USA_map,"GDP_USA","'PKB'",path,3,400,400,1,"OrRd")


  setwd("~/Desktop/Magisterka/Master_git/raw_maps")
png(file = paste0("map",title,year,".png", width = 800, height = 1200)
nclr <- 5
plotclr <- brewer.pal(nclr,"PuOr")
class <- classIntervals(PL_GDP$Value, nclr, style="quantile", dataPrecision=4)
colcode <- findColours(class, plotclr)
plot(sp,col=colcode)
#legend(-1,1, legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), cex=0.8, bty="n")
pal <- brewer.pal(nclr, "OrRd") # we select 7 colors from the palette
class(pal)
breaks_qt <- classIntervals(PL_GDP$Value/1000, n = nclr, style = "quantile")
br <- breaks_qt$brks 
# categoreis for choropleth map
sp@data$bracket <- cut(sp@data[,year()]/1000, breaks_qt$brks)
# plot
spplot(sp, "bracket", col.regions=pal, main = "Wartośći GDP według reginów w roku")






require(gridExtra)
plots = lapply(lata, function(.x) spplot(sp,.x))
do.call(grid.arrange,plots)

my.settings <- list(
  strip.background=list(col="lightblue"),
  strip.border=list(col="transparent")
)

spplot(sp, c("2018", "2017", "2016"), do.log = TRUE,
       key.space = "right", as.table = TRUE,
       sp.layout=list(rv, scale, text1, text2, arrow), # note that rv is up front!
       main = "Heavy metals (top soil), ppm", cex = .7, cuts = cuts,
       par.settings=my.settings)


a<-colnames(data)
data<-filter(data,Period==year)

data<-PL_GDP
map<-PL_map
data<-data%>%select(ID, Name, Period, Value)%>%pivot_wider(names_from =Period, values_from = Value)
data<-filter(data,Period==2010)
sp <- merge(x = map, y = data, by.x = "ID", by.y = "ID")
q <- quantile(sp@data[,20], seq(0.1, 0.9, 0.1))
breaks.qt <- classIntervals(colnames(sp@data)[20], n = 6, style = "quantile", intervalClosure = "right")
spplot(sp, sp@data[,20], col = "transparent", col.regions = brewer.pal(9, "YlGnBu"), at = breaks_qt$brks)


text<-list("sp.text", coordinates(sp), as.character(sp@data$Name),col="black", cex=0.5,font=2)
drawing<-spplot(sp, zcol = colnames(sp@data)[20], colorkey = TRUE, col.regions = brewer.pal(9, "YlGnBu")) 
                ,cuts = cuts,sp.layout = list(text),do.log=TRUE,
                par.settings = list(axis.line = list(col =  'transparent')))
main = paste("Wartości",title,"w roku",year))


library(gstat)
library(RColorBrewer)
load(system.file("data", "meuse.rda", package = "sp"))

# Create a SpatialPointsDataFrame Object from the data.frame
meuse.sp <- meuse  #Copy the data.  It's still a data.frame
coordinates(sp) <- ~x + y  # Now it's SpatialPointsDataFrame, with coordinates x and y
# Create a categorical variable and plot it
q <- quantile(meuse$zinc, seq(0.1, 0.9, 0.1))