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
polska<-draw_map(poland=1)
USA<-draw_map(0)
PL_map<-polska[[1]]  ###draw PL
USA_map<-USA[[1]]
n_regions<-polska[[2]]  ###draw PL
n_states<-USA[[2]]
USA_states<-USA[[3]]###draw USA
plot(PL_map@data)
plot(USA_map)
a<-PL_map@data$NUTS_NAME

############## W MATRIX - distance
W_PL<- matrix_W_distance(PL_map)
W_USA<- matrix_W_distance(USA_map)

############## setting dataset of unemployment in Poland
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
df_ch<-df[,1:125]
df<-df[,1:125]
df$MEAN<-rowMeans(df[,4:ncol(df)],na.rm=TRUE)
df$MIN<-apply(df[,4:ncol(df)],1,FUN=min,na.rm=TRUE)
df$MAX<-apply(df[,4:ncol(df)],1,FUN=max,na.rm=TRUE)

'''
v_max<-c()
v_min<-c()
vmax_name<-c()
vmin_name<-c()
for (i in 1:nrow(df)){
  d<-df[i,]
  v_max<-append(v_max,colnames(d)[which(d[,4:125] == d$MAX, arr.ind = FALSE)])
  vmax_name<-append(vmax_name,rep(d$Name,length(colnames(d)[which(d[,4:125] == d$MAX, arr.ind = FALSE)])))
  v_min<-append(v_min,colnames(d)[which(d[,4:125] == d$MIN, arr.ind = FALSE)])
  vmin_name<-append(vmin_name,rep(d$Name,length(colnames(d)[which(d[,4:125] == d$MIN, arr.ind = FALSE)])))
  d$trial<-v_max}
  '''

## level
PL_UE <- df %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )
PL_UE$Date<-as.Date(ymd(PL_UE$Period))
tail(PL_UE,12)
nper<-ncol(df)-3

## change
df_change<-select(df_ch,c("NAME","ID","Name"))
for (i in 16:ncol(df_ch)){
  temp<-(df_ch[,i] - df_ch[,i-12])
  print(temp)
  df_change<-cbind(df_change,temp)}
colnames(df_change)[4:ncol(df_change)]<-colnames(df_ch)[16:ncol(df_ch)]

PL_UE_ch <- df_change %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )
summary(PL_UE_ch$Value)   # -1.3 ; -0.2
var(PL_UE_ch$Value)       # 1.472203
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -4.100  -1.700      -0.800  -0.739  0.300      2.200

#PL_ST$Period<-as.numeric(PL_ST$Period)

############## setting dataset of unemployment in USA
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="USA_UE")
colnames(df)
names(df)<-c("ID","Year","Month", "Label","Value","NetChange_m","%Change_m","Name","seasonal_adj")
df$ID<-substr(df$ID,6,7)
df <- df%>%filter(seasonal_adj == 0) #%>%select(Code, Name, Year, Value)
df<-df[df$Name%in%USA_states,]

USA_years<-unique(df$Year)
USA_UE<-df
USA_UE$Period<-paste0(USA_UE$Year,"/",substr(USA_UE$Month, start = 2, stop = 3),"/01")
USA_UE$Date<-as.Date(ymd(USA_UE$Period))

## change
df_ch<-USA_UE%>%select(ID,Name,Date,"NetChange_m")
colnames(df_ch)[4] <- "Value"
df_ch<-df_ch%>%pivot_wider(names_from = Date, values_from = Value)
df_ch<-df_ch[match(colnames(W_USA), df_ch$ID), ]

USA_UE_ch<-df_ch%>%pivot_longer(
  cols = ends_with("1"),
  names_to = "Date",
  values_to = "Value",
)

summary(USA_UE_ch$Value)   # -0.2 ; 0.3
var(USA_UE_ch$Value)       # 2.42776
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -7.6000 -0.6000 -0.2000  0.1543  0.4000 25.9000

## level
df<-USA_UE%>%select(ID,Name,Date,Value)%>%
  pivot_wider(names_from = Date, values_from = Value)
df$MEAN<-rowMeans(df[,3:ncol(df)],na.rm=TRUE)
df$MIN<-apply(df[,3:ncol(df)],1,FUN=min,na.rm=TRUE)
df$MAX<-apply(df[,3:ncol(df)],1,FUN=max,na.rm=TRUE)
df<-df[match(colnames(W_USA), df$ID), ]

USA_UE <- df %>%
  pivot_longer(
    cols = ends_with("1"),
    names_to = "Date",
    values_to = "Value",
  )

USA_UE$Date<-as.Date(USA_UE$Date)

############## setting dataset of GDP in Poland
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="PL_GDP_M",na=":")
colnames(df)

df <- filter(df,str_length(df$ID) == 5)    ###filtering by poviats(5 as number of chars in ID)
df<-df[match(colnames(W_PL), df$ID), ]
df_ch<-df[,1:21]
df<-df[,1:21]
df$MEAN<-rowMeans(df[,3:ncol(df)],na.rm=TRUE)
df$MIN<-apply(df[,3:ncol(df)],1,FUN=min,na.rm=TRUE)
df$MAX<-apply(df[,3:ncol(df)],1,FUN=max,na.rm=TRUE)

PL_GDP <- df %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )
PL_GDP$Date<-as.Date(PL_GDP$Period,format="%Y")
month(PL_GDP$Date)<-1
day(PL_GDP$Date)<-1
lata<-unique(PL_GDP$Period)

df_change<-select(df_ch,c(ID, Name))
for (i in 4:ncol(df_ch)){
    temp<-(df_ch[,i] - df_ch[,i-1])/df_ch[,i-1]*100
    print(temp)
    df_change<-cbind(df_change,temp)}
PL_GDP_ch <- df_change %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  )  ###filtering by poviats(5 as number of chars in ID)
PL_GDP_ch$Period<-as.Date(as.character(PL_GDP_ch$Period),format="%Y")
summary(PL_GDP_ch$Value)   # 3.5 ; 9.5
var(PL_GDP_ch$Value)       # 81.42033
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -22.2620   0.7782   5.7229   5.8367  12.2333  38.4070

############## setting dataset of GDP in USA
df<-read_excel("~/Desktop/Magisterka/Master_git/dane/ALL_USA_PL.xlsx",
               sheet="USA_GDP",na="(D)")
colnames(df)
df[, c(5:ncol(df))] <- sapply(df[, c(5:ncol(df))], as.numeric)
names(df)[names(df) == 'GeoName'] <- 'Name'
df$Description<-as.factor(df$Description)

df<-df[df$Name%in%USA_states,]
df<-select(df,-c(3))
df$GeoFips<-substr(df$GeoFips,1,2)
colnames(df)[1]<-"ID"
df<-df[match(colnames(W_USA), df$ID), ]
#df$Name<-as.factor(df$Name)
#creating vectors of available industries
sectors_usa<-levels(df$Description)

## change
df<-filter(df,Description=="All industry total")
df_change<-df%>%select(c(ID, Name))
for (i in 8:ncol(df)){
  temp<-(df[,i] - df[,i-4])/df[,i-4]*100
  print(temp)
  df_change<-cbind(df_change,temp)}
USA_GDP_ch <- df_change %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Period",
    values_to = "Value",
  ) 
table(is.na(USA_GDP_ch$Value))
summary(USA_GDP_ch$Value)   # 2.3 ; 4
var(USA_GDP_ch$Value)       # 14.03696
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -21.004   1.668   3.470   3.095   5.019  27.639

df$MEAN<-rowMeans(df[,5:ncol(df)],na.rm=TRUE)
df$MIN<-apply(df[,5:ncol(df)],1,FUN=min,na.rm=TRUE)
df$MAX<-apply(df[,5:ncol(df)],1,FUN=max,na.rm=TRUE)


## level
USA_GDP <- df%>%filter(Description=="All industry total") %>%
  pivot_longer(
    cols = starts_with("2"),
    names_to = "Time",
    values_to = "Value",
  )%>%select(ID,Name,Time,MEAN,MAX,MIN,Value)


USA_GDP$Time<-gsub("_Q1", "-01-01", USA_GDP$Time)
USA_GDP$Time<-gsub("_Q2", "-04-01", USA_GDP$Time)
USA_GDP$Time<-gsub("_Q3", "-07-01", USA_GDP$Time)
USA_GDP$Time<-gsub("_Q4", "-10-01", USA_GDP$Time)
USA_GDP$Date <- as.Date(USA_GDP$Time, format = "%Y-%m-%d")
USA_GDP$Date2 <- as.yearqtr(USA_GDP$Time, format = "%Y-%m-%d")
#names(USA_GDP)<-c("ID","Name","LineCode", "Sect","Time","Value")
'''USA_GDP$Period<-ifelse(endsWith(USA_GDP$Time,"Q1"),"Q1",
                    ifelse(endsWith(USA_GDP$Time,"Q2"),"Q2",ifelse(endsWith(USA_GDP$Time,"Q3"),"Q3","Q4")))
USA_GDP$Period<-as.factor(USA_GDP$Period)
USA_GDP$Year<-substr(USA_GDP$Time,1,4)

USA_GDP$Year<-as.Date(USA_GDP$Year,format="%Y")
USA_GDP$Year<-year(USA_GDP$Year)'''

############################## OTHER
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
