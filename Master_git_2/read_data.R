setwd("~/Desktop/Magisterka/Master_git/Master_git_2")
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
df<-read_excel("~/Desktop/Magisterka/Master_git/Master_git_2/dane/ALL_USA_PL.xlsx",
           sheet="PL_%BEZR")
id_list<-read_excel("~/Desktop/Magisterka/Master_git/Master_git_2/dane/ALL_USA_PL.xlsx",
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
df<-read_excel("~/Desktop/Magisterka/Master_git/Master_git_2/dane/ALL_USA_PL.xlsx",
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
df<-read_excel("~/Desktop/Magisterka/Master_git/Master_git_2/dane/ALL_USA_PL.xlsx",
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
df<-read_excel("~/Desktop/Magisterka/Master_git/Master_git_2/dane/ALL_USA_PL.xlsx",
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

df$MEAN<-rowMeans(df[,4:ncol(df)],na.rm=TRUE)
df$MIN<-apply(df[,4:ncol(df)],1,FUN=min,na.rm=TRUE)
df$MAX<-apply(df[,4:ncol(df)],1,FUN=max,na.rm=TRUE)


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