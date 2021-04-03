setwd("~/Desktop/Magisterka/Master_git")
source("MC_MG_HF_functions.R")

# file ALL_USA_PL
# sheets:
# PL_GDP_PER_CAP
# PL_GDP_M
# PL_UE
# PL_%BEZR
# USA_UE
# USA_GDP


############## LOAD REAL DATA ####################
PL_map<-draw_map(poland=1)[[1]]  ###draw PL
USA_map<-draw_map(0)[[1]]
n_regions<-draw_map(poland=1)[[2]]  ###draw PL
n_states<-draw_map(0)[[2]]
USA_states<-draw_map(0)[[3]]###draw USA
plot(PL_map@data)
plot(USA_map)
a<-PL_map@data$NUTS_NAME

############## W MATRIX - distance
W_PL<- matrix_W_distance(PL_map)
W_USA<- matrix_W_distance(USA_map)

# setting dataset of unemployment in Poland
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
           sheet="PL_%BEZR")
id_list<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="PL_regiony")
temp<-data.frame(ID=colnames(W_PL))
temp<-merge(x=temp, y=id_list,by.x="ID",by.y="ID")
df<-merge(x=temp, y=df,by.x="NAME",by.y="Name")
df<-df[match(colnames(W_PL), df$ID), ]
y<-as.integer((ncol(df)-3)/12)
months<-rep(seq(1,12,1),y)
years<-rep(seq(2011,2011+y-1,1),each=12)
for (i in 4:ncol(df)){
  colnames(df)[i]<-paste0(years[i-3],"/",months[i-3],"/1")
}

df_change<-select(df,"ID")
for (i in 16:ncol(df)){
  temp<-(df[,i] - df[,i-12])
  print(temp)
  df_change<-cbind(df_change,temp)}

PL_ST <- df %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )
PL_ST$Date<-as.Date(ymd(PL_ST$Period))
tail(PL_ST,12)
mlata<-ncol(df)-3
#PL_ST$Period<-as.numeric(PL_ST$Period)

# setting dataset of unemployment in Poland
#PL_EU <- df%>%filter(Gender == "ogółem")%>%select(ID, Name, Year, Value)

# setting dataset of unemployment in USA
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="USA_UE")
colnames(df)
names(df)<-c("ID","Year","Period", "Label","Value","%Change(M)","Name","seasonal_adj")
df <- df%>%filter(seasonal_adj == 0) #%>%select(Code, Name, Year, Value)
USA_UE<-df
USA_UE$ID<-as.factor(USA_UE$ID)
df<-df[df$Name%in%USA_states,]
df<-df[match(colnames(W_USA), df$ID), ]

# setting dataset of GDP in Poland
library(stringr)
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="PL_GDP_M",na=":")
colnames(df)
names(df)[names(df) == 'LABEL'] <- 'ID'
names(df)[names(df) == 'NAME'] <- 'Name'
df <- filter(df,str_length(df$ID) == 5)    ###filtering by poviats(5 as number of chars in ID)
df<-df[match(colnames(W_PL), df$ID), ]
PL_GDP <- df %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )
PL_GDP$Period<-as.numeric(PL_GDP$Period)
lata<-unique(PL_GDP$Period)
n_regions<-unique(PL_GDP$Name)

df_change<-select(df,c(ID, Name))
for (i in 4:ncol(df)){
    temp<-(df[,i] - df[,i-1])/df[,i-1]*100
    print(temp)
    df_change<-cbind(df_change,temp)}
PL_GDP_ch <- df_change %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )  ###filtering by poviats(5 as number of chars in ID)
PL_GDP_ch$Period<-as.numeric(PL_GDP_ch$Period)

# setting dataset of GDP in USA
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="USA_GDP",na="(D)")
colnames(df)
names(df)[names(df) == 'GeoName'] <- 'Name'
df$Description<-as.factor(df$Description)
df<-df[df$Name%in%USA_states,]
#df$Name<-as.factor(df$Name)
#creating vectors of available industries
sectors_usa<-levels(df$Description)

for (i in 5:67)
  df[,i]<-as.numeric(df[,i])

USA_GDP <- df %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Time",
    values_to = "Value",
  )
names(USA_GDP)<-c("ID","Name","LineCode", "Sect","Time","Value")

USA_GDP$Period<-ifelse(endsWith(USA_GDP$Time,"Q1"),"Q1",
                    ifelse(endsWith(USA_GDP$Time,"Q2"),"Q2",ifelse(endsWith(USA_GDP$Time,"Q3"),"Q3","Q4")))
USA_GDP$Period<-as.factor(USA_GDP$Period)
USA_GDP$Year<-substr(USA_GDP$Time,1,4)

USA_GDP$Year<-as.Date(USA_GDP$Year,format="%Y")
USA_GDP$Year<-year(USA_GDP$Year)

###
df_change<-select(df,c(ID, Name))
for (i in 4:ncol(df)){
  temp<-(df[,i] - df[,i-4])/df[,i-4]*100
  print(temp)
  df_change<-cbind(df_change,temp)}
PL_GDP_ch <- df_change %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )  ###filtering by poviats(5 as number of chars in ID)
PL_GDP_ch$Period<-as.numeric(PL_GDP_ch$Period)

N <- n_regions
setwd("~/Desktop/Magisterka/Master_git/output")

#m1+m0
png(file = "lines.png", width = 1700, height = 1200)
par(mfrow = c(7, 4))
starting.point <- 1
starting.point.2 <- N



data<-PL_ST
p<-0
#draw_timeseries<-function(data){
plot_list<-list()
for (i in unique(data$ID)){
  p<-p+1
  d<-data%>%filter(ID==i) %>%filter(Value!=0)
  t<-d$Name
  m<-mean(d$Value)
  dmax<-d[which(d$Value == max(d$Value)),]$Date
  dmin<-d[which(d$Value == min(d$Value)),]$Date

  plot_list[[p]]<-ggplot(data=d,aes(x=Date,y=Value)) +
                        geom_line( color=main_colour) +
                        geom_point(shape=21, color=main_colour, fill=main_colour, size=1) +
                        theme_ipsum() +
                        theme(plot.title=element_text(size=1, hjust=1, vjust=0.5, face='bold'))+
                        #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
                        #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
                        geom_hline(yintercept=m, color=main_colour2, size=.5)+
                        geom_vline(xintercept = dmax,color="grey", size=2,alpha = 1/2)+
                        geom_vline(xintercept = dmin,color="grey", size=2,alpha = 1/2)+
                        #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
                        ggtitle(paste0("Level of GDP in ",t))
  #plot(plot_list)
}
do.call(grid.arrange,c(plot_list,nrow=r,ncol=c))
res <- marrangeGrob(plot_list, nrow = 4, ncol = 4)

dev.off()
}

draw<-ggplot(data=PL_ST,aes(x=Date,y=Value)) +
  geom_line( color=main_colour) +
  geom_point(shape=21, color=main_colour, fill=main_colour, size=1) +
  theme_ipsum() +
  theme(plot.title=element_text( hjust=1, vjust=0.5, face='bold'))+
  #annotate(geom="text", x=as.Date("2017-01-01"), y=20089, label="Bitcoin price reached 20k $\nat the end of 2017") +
  #annotate(geom="point", x=as.Date("2017-12-17"), y=20089, size=10, shape=21, fill="transparent") +
  geom_hline(yintercept=m, color=main_colour2, size=.5)+
  geom_vline(xintercept = dmax,color="grey", size=2,alpha = 1/2)+
  geom_vline(xintercept = dmin,color="grey", size=2,alpha = 1/2)+
  #scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11")))
  ggtitle(paste0("Level of GDP in "))+
  facet_wrap(~Name,ncol=4, nrow=7)
plot(draw)


draw_gg<-(ggplot(head(draw), aes(x=Period,y=Value,col=factor(Value)))
          +
            geom_point(shape=1,size=1)
          +
            geom_line(linetype=1)
          +
            xlab("Year")
          +
            ylab("GDP value")
          +
            ggtitle(paste0("Level of GDP in ","Poland")) #ggtitle("Quaterly level of GDP in USA")
          +
            theme(axis.text.x = element_text(angle=45)))
draw_gg

draw <- USA_GDP%>%filter(Sect == sectors_usa[2])
draw<-PL_ST
######################### CHARTS 
draw_gg<-(ggplot(draw, aes(x=Period,y=Value, group=factor(Name),
                           col=factor(Name)))
          +
            geom_point(shape=1,size=1)
          +
            geom_line(linetype=2)
          +
            xlab("Year")
          +
            ylab("GDP value")
          +
            ggtitle(paste0("Level of GDP in ","Poland")) #ggtitle("Quaterly level of GDP in USA")
          +
            theme(axis.text.x = element_text(angle=45)))
draw_gg
img_USA_GDP <- recordPlot()  # zmienna z zapisanym obrazem
img_PL_GDP <- recordPlot()
img_USA_UE <- recordPlot()
img_PL_UE <- recordPlot()

# boxplot
draw_gg <- (
  ggplot(draw, aes(x=Name, y=Value))
  +
    geom_boxplot(
      #fill = Value,
      colour = main_colour,
      outlier.colour = main_colour2,
      outlier.shape = 5)
  +
    theme(axis.text.x = element_text(angle=45)))
print(draw_gg)
boxplot_USA_GDP <- recordPlot()  # zmienna z zapisanym obrazem
boxplot_PL_GDP <- recordPlot()

draw <- PL_GDP #%>%filter(Sect == sectors_usa[2])

draw_l <- xyplot(Value~Period,
                 group=Name,
                 data=draw, 
                 pch=16, col="red"
)
print(draw_l)
hist(draw$Value,breaks=50)

draw_l <- bwplot(
  Value~Name,
  data=draw,
  fill="blue",
  horizontal=F)
print(draw_l)

##################################### MATRIX W #########################################
#Way 1: Neighbourhood matrix with row normalisation
library(spdep)
cont1 <- poly2nb(spatial_data_pl, queen = T)
W1_list <- nb2listw(cont1, style = "W")
W1 <- listw2mat(W1_list)
plot.nb(cont1, centroids, col = sgh_green, pch = 16)
W1_list$weights

#Way 2: Neighbourhood matrix with row normalisation - 2-nd order neighbourhood relationship included (neighbour of my neighbour is my neighbour)
cont2 <- nblag(cont1, 2)
cont2acum <- nblag_cumul(cont2)
W2 <- nb2mat(cont2acum)
centroids <- coordinates(spatial_data_pl)
plot.nb(cont2acum, centroids, col = main_colour, pch = 16)
W2_list <- mat2listw(W2, style = "W")

#Way 3: Matrix of inverted distance
library(geosphere)
distance <- distm(coordinates(spatial_data), fun = distCosine) / 1000
rownames(distance) <- spatial_data@data$jpt_kod_je
colnames(distance) <- spatial_data@data$jpt_kod_je
gamma <- 1
W3 <- 1 / (distance ^ gamma)
diag(W3) <- 0
W3 <- W3 / as.matrix(rowSums(W3)) %*% matrix(1, nrow = 1, ncol = N)
W3_list <- mat2listw(W3, style="W")




############# old ###########
# uploading USA data
USA<-read.csv("~/Desktop/Magisterka/Master_git/dane/USA_realGDP_states_Q.csv", header=TRUE, sep=",",skip=4,na.strings="")
USA<-as.data.frame(USA)
head(USA)
table(USA$GeoName)
#changing to factors
USA$GeoName<-as.factor(USA$GeoName)
USA$Description<-as.factor(USA$Description)

#creating vectors of available industries
sectors<-levels(USA$Description)
for (i in 5:67)
  USA[,i]<-as.integer(USA[,i])

T1 <- USA%>%filter(Description == sectors[2])

USA_GDP <- USA %>%
  pivot_longer(
    cols = starts_with("X2"),
    names_to = "Period",
    values_to = "Value",
  )

USA2$QUATER<-ifelse(endsWith(USA2$PERIOD,"Q1"),"Q1",
                    ifelse(endsWith(USA2$PERIOD,"Q2"),"Q2",ifelse(endsWith(USA2$PERIOD,"Q3"),"Q3","Q4")))
USA2$QUATER<-as.factor(USA2$QUATER)
USA2$YEAR<-substr(USA2$PERIOD,2,5)

USA2$YEAR<-as.Date(USA2$YEAR,format="%Y")
USA2$YEAR<-year(USA2$YEAR)

USA2$quarterly<-paste(USA2$YEAR," ",USA2$QUATER)
library(zoo)
USA2$q= as.yearqtr(USA2$quarterly,format="%Yq%q")

########################### PREPARING MAPS ###########################
install.packages("cshapes", dependencies = TRUE)
library(cshapes)

library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
plot_usmap(regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())
library(maps)
usa_map <- map_data("state")
head(usa_map)
ggplot(data = usa_map) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend
usa <- map_data("usa")

### stare liczenie
PL_GDP$Change<-ifelse(PL_GDP$Period ==2000,0,
                      for (i in regiony){
                        for (j in lata[-1]){
                          print(j)
                          (PL_GDP$Change=
                              PL_GDP$Value[which((PL_GDP$Name == i)&(PL_GDP$Period == j))]-
                              PL_GDP$Value[which((PL_GDP$Name == i)&(PL_GDP$Period == j-1))])/
                            PL_GDP$Value[which(PL_GDP$Name == i &PL_GDP$Period == j-1)]    }})
