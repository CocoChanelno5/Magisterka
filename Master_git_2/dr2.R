

for (i in unique(data$Year)){
  d<-data%>%select(ID, Name, Date,Month,Year,Value)%>%filter(Month=="1")%>%filter(Year==i)
  sp <- merge(x = map, y = d, by.x = "ID", by.y = "ID")
  plot(sp,col=colcode)}





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
