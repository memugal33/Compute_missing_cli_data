setwd("F:/WORK/ERI prop/puyallup climate")
library(tidyr)
library(erer)
library(base)

### uses lattitudes and longitude to calculate distances.. 
### This code will look at the available stations (29 station in this case) and 
###calculate the distance between stations using their latitude and longitude and picks station within 100 km and 
### uses Inverse distance weighing method to calculate missing precipitation data for a station whenever applicable.
#### counts the number of data replaced with IDW or Zero.

datacli<-read.csv("2908980.csv", header=T)

mydata2<-datacli[c('NAME','LATITUDE','LONGITUDE','DATE','PRCP','TMAX','TMIN')]

unique(mydata2$NAME)
unique(mydata2$LATITUDE)

cliloc<-aggregate(PRCP~NAME+LATITUDE+LONGITUDE, mydata2, mean)[,-4]


##prcp average ###
mydata2_2<-separate(mydata2,"DATE",c("Month", "Day", "Year"), sep = "/")

mydata_seatac<-mydata2_2[mydata2_2$NAME==unique(mydata2_2$NAME)[1],]
agg_sea<-aggregate(PRCP~Year, mydata_seatac, sum)
mean(agg_sea$PRCP)

avg_each_loc<-aggregate(PRCP~NAME+Year+LATITUDE+LONGITUDE, mydata2_2, sum)
agg_annual_vals<-aggregate(PRCP~NAME+LATITUDE+LONGITUDE, avg_each_loc, mean)
agg_annual_vals$PRCP<-round(agg_annual_vals$PRCP,2)

########################### Calculate distance matrix for the staions ##############################

#loc<-read.csv("CLIMATE_STATION_LOATIONS.csv")




library(geosphere)


#distm(cliloc[1, c(3,2)], cliloc[2, c(3,2)], fun = distHaversine )/1000



## calculating distance using distance Haversine function 
### distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine) 
### answers in meter
l1<-list()
l2<-list()
for (i in 1:nrow(cliloc)) {
  
  l1<-NULL
  l1<-list()
  for ( j in 1:nrow(cliloc)) {
    d<- (distm(cliloc[i, c(3,2)], cliloc[j, c(3,2)], fun = distHaversine ))/1000
    
    nm<-as.character(cliloc[j, 1])
    
    l1[[nm]]<-d
    
  }
  
  mat<- matrix(unlist(l1), nrow = 1)
  colnames(mat)<-names(unlist(l1))
  nm2<-as.character(cliloc[i, 1])
  l2[[nm2]]<-mat
  
}

dist_mat<-matrix(unlist(l2), nrow=nrow(cliloc), ncol=nrow(cliloc), byrow = T) ## distance matrix
rownames(dist_mat)<-names(unlist(l1))
colnames(dist_mat)<-names(unlist(l1))

#####################################################################################################
data_puy <- mydata2[mydata2$NAME==unique(mydata2$NAME)[4],]

cliloc

library(sf)
library(ggplot2)

my_cli<-st_as_sf(cliloc, coords = c('LONGITUDE','LATITUDE'))

# my_cli<-st_set_crs(my_cli, crs = 4326)
st_crs(my_cli) = 4326

ggplot(my_cli)+geom_sf(aes(color=NAME), size=6)+
  scale_color_manual(values = c(`PUYALLUP 2 W EXPERIMENTAL STATION, WA US`="#dbd556",
                                `KENT, WA US` ='black',
                                `SEATTLE TACOMA AIRPORT, WA US` = "blue",
                                `TACOMA NUMBER 1, WA US` = "green",
                                `TACOMA NARROWS AIRPORT, WA US`="purple",
                                `WAUNA 3 W, WA US` = "darkblue",
                                `LAKELAND NORTH 0.5 ENE, WA US` = "darkgreen",
                                `MCMILLIN RESERVOIR, WA US` = "red",
                                `STEILACOOM 0.4 NW, WA US` = "darkred"))+
  ggtitle("Station used for IDW")
  #+scale_color_manual()


for (i in (1:9)){
  nm = unique(mydata2$NAME)[i]
  d = mydata2[mydata2$NAME==nm,]
  sd = d[1,4]
  ed = d[nrow(d),4]
  if (i == 1) {
  vec = matrix(c(nm,sd,ed), nrow=1)
  date_mat = vec
  }else{
    vec = matrix (c(nm,sd,ed), nrow=1)
    date_mat = rbind(date_mat,vec)
  }
}
colnames(date_mat)<-c("NAME","DATE_FROM","DATE_TO")

mat_plot<-merge(agg_annual_vals,date_mat, by="NAME")

my_cli<-st_as_sf(mat_plot, coords = c('LONGITUDE','LATITUDE'))

# my_cli<-st_set_crs(my_cli, crs = 4326)
st_crs(my_cli) = 4326

ggplot(my_cli)+geom_sf(aes(color=NAME), size=6)+
  geom_sf_text(aes(label=PRCP), nudge_x = 0.02, nudge_y = -0.02)+
  scale_color_manual(values = c(`PUYALLUP 2 W EXPERIMENTAL STATION, WA US`="#dbd556",
                                `KENT, WA US` ='black',
                                `SEATTLE TACOMA AIRPORT, WA US` = "blue",
                                `TACOMA NUMBER 1, WA US` = "green",
                                `TACOMA NARROWS AIRPORT, WA US`="purple",
                                `WAUNA 3 W, WA US` = "darkblue",
                                `LAKELAND NORTH 0.5 ENE, WA US` = "darkgreen",
                                `MCMILLIN RESERVOIR, WA US` = "red",
                                `STEILACOOM 0.4 NW, WA US` = "darkred"))+
  ggtitle("Stations used for IDW with annual precipitation (in)")
#+scale_color_manual()



#write.csv(date_mat, "Station_dates.csv")







#################################### Correct missing date and missing  temperature  #########################

start_date = "1914/01/01"
end_date = "2021/12/31"

Dates<-as.data.frame(matrix(as.character(seq.Date(as.Date(start_date), as.Date(end_date), 1)), ncol = 1))
colnames(Dates)<-"Date"
Dates<-separate(Dates,"Date",c("Year", "Month", "Day"), sep = "-")
date<-Dates
#date<-read.csv("dates.csv")
nmss<-list.files(pattern = ".csv")


### missing precip data collection 
infs<-NULL
infs<-list()
infs2<-list()
repl_list<-list()

################################# Puyallup data addition #####################

### some amount of missing data needs to be added from different source ###
data_pag<-read.csv("Puyallup_1995_2022.csv", header = T,skip = 1)

data_pag<-data_pag[,c(1,2,4,14)]

data_pag_2<-separate(data_pag, "DATE", c("Year","Month","Day"), sep = "-")
colnames(data_pag_2)<-c("Year","Month","Day","TMIN","TMAX","PRCP")
data_pag_2<-data_pag_2[data_pag_2$Year<2022,]
##############################################################################


for (w in "2908980.csv") { 
  
  # w = "2908980.csv"
data<-read.csv(paste0(w), header = T)
data<-data[c('NAME','LATITUDE','LONGITUDE','DATE','PRCP','TMAX','TMIN')]
nm<-unique(data$NAME)

nm2<-nm[c(1,2,4,7,9)]

data<-data[(data$NAME==nm2[1]|data$NAME==nm2[2]|data$NAME==nm2[3]|data$NAME==nm2[4]|data$NAME==nm2[5]),]
nm<-unique(data$NAME)
#nm<-names(n)
  # 
  # if (w == "2122245.csv") {
  #   data<-separate(data, "DATE", c("Year","Month", "Day"), sep = '-')  ### date separation have to work different in these two cases because they are formatted differently
  # } else {
  #   
  #   data$DATE<-strptime(as.character(data$DATE), "%m/%d/%y")
  #   data$DATE<-format(data$DATE, "%Y/%m/%d")
  #   data<-separate(data, "DATE", c("Year","Month", "Day"), sep = '/') 
  # }

mydata2<- separate(data, "DATE", c("Year","Month", "Day"), sep = '-') 
infs<-NULL
infs<-list()
  
for (q in 1:NROW(nm)) {
    
    #q=3
    data1<-data[data$NAME==nm[q],]
    

    # if (as.numeric(data1[1,3])>1988|as.numeric(data1[nrow(data1),3])<2018) {next}
    
    # |as.numeric(data1[nrow(data1),4])<12&as.numeric(data1[nrow(data1),5])<31)
    # data1<-subset(data1, Year>1987&Year<2019) ### data from 1988
    # data1<-data1[data1$Year>1987&data1$Year<2019,]
    #data1<-separate(data1, "DATE", c("Year","Month", "Day"), sep = '-') 
    data1<-separate(data1, "DATE", c("Month","Day", "Year"), sep = '/') 
    
    ### when it is puyallup the number of data needs to be added in the bottom ##
    if (nm[q]=="PUYALLUP 2 W EXPERIMENTAL STATION, WA US"){
      data_pag_2$LATITUDE = data1$LATITUDE[1]
      data_pag_2$LONGITUDE = data1$LONGITUDE[1]
      data_pag_2$NAME = data1$NAME[1]
      data1 = rbind(data1,data_pag_2[62:nrow(data_pag_2),])
      # data_pag_2 <-data_pag_2[62:nrow(data_pag_2),]#c(9,7,8,2,3,1,6,4,5)
    }
    
    #write.csv(data1, "raw_puyallup_full.csv", row.names = F)
    
    
    
    cdata<-date
    cdata$PRCP<-NA
    cdata$Tmax<-NA
    cdata$Tmin<-NA
    j = 1
    if(q==5){data1 = data1[data1$Year>1913,]}
    #data1<-data_puy
    ### taking care of the missing dates in the originally downloaded data ############### 

    
        
    for (i in 1:nrow(cdata)) {
      #i=1
      if (as.numeric(cdata[i,1])==as.numeric(data1[j,6])&as.numeric(cdata[i,2])==as.numeric(data1[j,4])&as.numeric(cdata[i,3])==as.numeric(data1[j,5])) {
        
        cdata[i,4]=data1$PRCP[j]
        cdata[i,5]=data1$TMAX[j]
        cdata[i,6]=data1$TMIN[j]
        j = j + 1
        if (j>nrow(data1)) {break}
      }
    }
    
    ### creating the matrix that will count the missing data records 
    
    rep_0 <- matrix(, nrow = nrow(cdata), ncol=4)
    colnames(rep_0)<-c("rep_Tmax", "rep_Tmin","rep_0", "rep_IDW")
    rep_0<-cbind(cdata[,c(1,2,3)], rep_0)
    
    ###### taking care of missing precip and temperature in the data #############
  avgmaxtemp = mean(cdata$Tmax,na.rm = T)  
  avgmintemp = mean(cdata$Tmin,na.rm = T)   
    ### missing temperature 
    flg <- 0 
    
    for (k in 1:nrow(cdata)) {
      
      if (cdata[k,1] == cdata[1,1]) {  ### for the first year 
        
        if(cdata[k,5]== "" | is.na(cdata[k,5]) == T | cdata[k,5] == 99999 ) {    ### tmax correction
          
          cdata[k,5]<-avgmaxtemp
          rep_0[k,4]<-1
        }
        if(cdata[k,6]== "" | is.na(cdata[k,6]) == T | cdata[k,6] == 99999 ) {    ### tmin correction
          
          cdata[k,6]<-avgmintemp
          rep_0[k,5]<-1
        }
        # if(cdata[k,4]== "" | is.na(cdata[k,4]) == T | cdata[k,4] == 999.99 | cdata[k,4] == "T" ) {   ### Prcp correction
        #   
        #   cdata[k,4]<-0
        # }
      } else {   ### for years other than Year 1
        
        ### Temperature 
        if(cdata[k,5]== "" || is.na(cdata[k,5]) == T || cdata[k,5] == 99999 ) { ## tmax correction 
          
          cdata[k,5]<-cdata[k-365,5]
          rep_0[k,4]<-1
          
        }
        if(cdata[k,6]== "" || is.na(cdata[k,6]) == T || cdata[k,6] == 99999 ) { #### tmin correction
          
          cdata[k,6]<-cdata[k-365,6]
          rep_0[k,5]<-1
        }   
        # ### prcp correction
        # if(cdata[k,4]== "" || is.na(cdata[k,4]) == T || cdata[k,4] == 999.99 || cdata[k,4] == "T" ) {
        #   
        #   if (flg == 0) { 
        #     mflg <- 0
        #     flg<- 1
        #     while (mflg < 6) {  ### doing this to check whether next 6 consecutive values also have missing data or not
        #       mflg = mflg + 1
        #       if (cdata[k+mflg,4] != "" & is.na(cdata[k+mflg,4])==F & cdata[k+mflg,4] != 999.99 & cdata[k+mflg,4] != "T" ) {
        #         flg = 0
        #         break
        #       }
        #     }
        #   }
        #   
        #   if (flg == 0) {
        #     cdata[k,4]<-0
        #   }
        #   else { 
        #     cdata[k,4]<- cdata[k-365, 4]
        #   }
        #   
        # }
        # 
        # else {
        #   flg = 0
        # }
        # 
        # 
        
      }
      
      
    }
    
    # ag_prcp<-aggregate(PRCP~Year, cdata[cdata$Year!=1988,], sum)
    # mean_prcp<-mean(ag_prcp[,2])
    # info<-list("Year ann. Prcp."= ag_prcp, "Avg. ann. Prcp" = mean_prcp)
    # name=paste0(nm[q])
    # infs[[name]]<-info
    # write.csv(cdata[cdata$Year!=1988,], paste0(nm[q], ".csv"), row.names = F,col.names = T)
    # write.table(cdata[cdata$Year!=1988,], paste0(nm[q], ".txt"), sep="\t", row.names = F,col.names = F)
    # 
    
stn_name = nm[q]
infs[[stn_name]]<-cdata
repl_list[[stn_name]]<-rep_0
  #   
  #   
    }
  
  
  #write.list(infs, "infos5.csv")
  
  # filename<-paste(w)
  # infs2[[filename]]<-infs
  
  
}

#######################################################################################################

################### correcting precipitation using inverse distance method #############################
infsz<-list()


for ( l in 1:length(infs)) {
#for (l in 4){  
  
  # l = 4 #### replace it later
  s_name<-names(infs[l])
  #s_name<-data1$NAME[1]
  s_data<-matrix(unlist(infs[[s_name]]), ncol = 6, byrow = F)
  colnames(s_data)<-c("Year", "Month", "Day", "PRCP", "TMAX", "TMIN")
  dist_mat2<-dist_mat[as.character(s_name),]
  dist_mat3<-dist_mat2[dist_mat2<=100][nm2]
  
  rep_0<-matrix(unlist(repl_list[[s_name]]), ncol = 7, byrow = F)
  colnames(rep_0)<-c("Year", "Month", "Day","rep_Tmax", "rep_Tmin","rep_0", "rep_IDW")
  
  for (z in 1:length(dist_mat3)) {
  scrtch<-infs[[names(dist_mat3[z])]]
  scrtch<-matrix(scrtch[,4], ncol = 1)
  colnames(scrtch)<-names(dist_mat3[z])
  
  s_data<- cbind(s_data, scrtch)
  
  }
 
  
  for ( a in 1: nrow(s_data)) {
  if(s_data[a,4]== " " || is.na(s_data[a,4]) == T || s_data[a,4] == 999.99 || s_data[a,4] == "T" ) {
    # a = 1 ### replace it later
  n_nb<-ncol(s_data)-6 ## number of neigbouring station 
  
  if((length(which(is.na(s_data[a,c(7:ncol(s_data))])))==n_nb)||(length(which((s_data[a,c(7:ncol(s_data))]== 999.99)))==n_nb)||
     (length(which((s_data[a,c(7:ncol(s_data))]== "T")))==n_nb)||(length(which((s_data[a,c(7:ncol(s_data))]== " ")))==n_nb)||(length(which((s_data[a,c(7:ncol(s_data))]== 0)))==n_nb)) {
    
    s_data[a,4] <- 0 
    rep_0[a,6]<- 1### counting the number of data replaced with 0
    }  else {
    

    calc_matrix<-matrix(s_data[a,c(7:ncol(s_data))], ncol = 1)
    Distance<-matrix(, nrow = nrow(calc_matrix), ncol = 1)
    calc_matrix<-cbind(calc_matrix,Distance)
    rownames(calc_matrix)<-names(s_data[a,c(7:ncol(s_data))])
    
    
    numrow<-nrow(calc_matrix)
    for(xx in 1:numrow) {
      
      calc_matrix[xx,2]<-as.numeric(dist_mat3[names(calc_matrix[xx,1])])
      
      
      
      if (calc_matrix[xx,1]=="T"||calc_matrix[xx,1]==" "||calc_matrix[xx,1]==999.99||calc_matrix[xx,1]==99999||is.na(calc_matrix[xx,1]) == T) {
        
        calc_matrix[xx,2]<-0
        
      }
      
    }
    colnames(calc_matrix)<-c("Prcp", "distance")
    calc_matrix<-as.data.frame(calc_matrix)
    
    calc_matrix<-calc_matrix[calc_matrix$distance>0,]
    calc_matrix$Inv.DistanceSq <-1/((as.numeric(calc_matrix[,2]))^2)
    calc_matrix$prduct<-as.numeric(calc_matrix[,1])*as.numeric(calc_matrix[,3])
    
    prcp_val<-sum(calc_matrix$prduct)/sum(calc_matrix$Inv.DistanceSq)
    
    s_data[a,4]<-signif(prcp_val,3)
    
    rep_0[a,7]<- 1 ### counting the number of data replaced with IDW
    
    # length(which(is.na(s_data[,4])))
  }
  
    
  }
    
    
  }
  
  s_data<-as.data.frame(s_data)
  # write.csv(s_data, "precip_filled_temp_filled.csv", row.names = F)
  # rep_0<-as.data.frame(rep_0)
  # rep_0[is.na(rep_0)] = 0
  # s_data$PRCP = as.numeric(s_data$PRCP)
  # ag_prcp<-aggregate(PRCP~Year, s_data, sum)
  # 
  # 
  # ag_rep0<-aggregate(rep_0~Year, rep_0[rep_0$Year!=1988,], sum)
  # ag_rep0<-as.matrix(ag_rep0[,2]) ### doing this so that there are no repetitive colomns in info file
  # ag_repIDW<-aggregate(rep_IDW~Year, rep_0[rep_0$Year!=1988,], sum)
  # ag_repIDW<-as.matrix(ag_repIDW[,2])
  # ag_reptmx<-aggregate(rep_Tmax~Year, rep_0[rep_0$Year!=1988,], sum)
  # ag_reptmx<-as.matrix(ag_reptmx[,2])
  # ag_reptmn<-aggregate(rep_Tmin~Year, rep_0[rep_0$Year!=1988,], sum)
  # ag_reptmn<-as.matrix(ag_reptmn[,2])
  # mean_prcp<-mean(ag_prcp[,2])
  # info_3<-list("Year ann. Prcp."= ag_prcp, "Avg. ann. Prcp" = mean_prcp, "Replaced Prcp with 0"= ag_rep0, "Replaced Prcp with IDW"=ag_repIDW,"Replaced Tmax"= ag_reptmx, "Replaced Tmin"=ag_reptmn)
  # # namez=paste0(nm[q])
  # infsz[[s_name]]<-info_3
  # s_data<-s_data[s_data$Year!=1988,]
  write.csv(s_data[,c(1,2,3,4,5,6)], paste0("newm_final/",s_name,".csv"), row.names = F)
  write.table(s_data[,c(1,2,3,4,5,6)], paste0("newm_final/",s_name,".txt"), row.names = F, col.names = F, sep = "\t")
}
