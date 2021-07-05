
### read Data set ####
# caatinga municipalities shapefile
mun_caat_shp<-readOGR("mun_caat_shp.shp")
crs(mun_caat_shp)<-"+proj=longlat +datum=WGS84 +no_defs"

#csv from lamd tenure raster
tab_CF_caat<-read.table("tab_CF_caat.txt",head=T) 
# private lands
tab_CF_caat<-tab_CF_caat[tab_CF_caat$fate_of_land %in% c("PL","PL1") ,] #& tab_CF_caat$farmsizerange %in% c("S","M","L")  

mun_cod<-mun_caat_shp$CD_MUN
mun_cod<-mun_cod[-c(481,915,1027)] #they are out of extet of MapBiomas


## read table number of pixel per property #####
mun_rds_final<-read.table("Results/mun_caat_CF_19.08.txt",head=T)
# per hectare
mun_rds_final_ha<-cbind(mun_rds_final[c(1:2)],mun_rds_final[c(3:8)]*900/10000)


mun_rds_final_ha$farmsizerange<-NA

mun_rds_final_ha$farmsizerange[match(tab_CF_caat$primkey[tab_CF_caat$farmsizerange %in% c("S") 
     & tab_CF_caat$ibgecode %in% mun_rds_final_ha$mun_cod],mun_rds_final_ha$id_prop)]<-"S"

mun_rds_final_ha$farmsizerange[match(tab_CF_caat$primkey[tab_CF_caat$farmsizerange %in% c("M") 
     & tab_CF_caat$ibgecode %in% mun_rds_final_ha$mun_cod],mun_rds_final_ha$id_prop)]<-"M"

mun_rds_final_ha$farmsizerange[match(tab_CF_caat$primkey[tab_CF_caat$farmsizerange %in% c("L") 
     & tab_CF_caat$ibgecode %in% mun_rds_final_ha$mun_cod],mun_rds_final_ha$id_prop)]<-"L"

# sum props areas from diverse municipalities
mun_rds_final_ha2<-with(mun_rds_final_ha, aggregate(mun_rds_final_ha[,c(3:8)], by = list(id_prop), FUN = sum))#[,2]
colnames(mun_rds_final_ha2)<- c("id_prop",colnames(mun_rds_final_ha2[c(2:7)]))

mun_rds_final_ha<-mun_rds_final_ha[!is.na(mun_rds_final_ha$farmsizerange),]## 4 muns did not have class of farmsizerange in imaflora data (tab_CF_caat)
mun_rds_final_ha2_<-merge(mun_rds_final_ha2,mun_rds_final_ha[,c(1,2,9)],by="id_prop")

mun_rds_final_ha2_<-mun_rds_final_ha2_[,c(8,1,9, 2:7)]



### VSD #### tabel per property and farm size ####
x<-mun_rds_final_ha2_

names(tab_CF_caat)

#x$area_imaflora_ha<- tab_CF_caat$areaproc[match(x$id_prop,tab_CF_caat$primkey[tab_CF_caat$ibgecode %in% x$mun_cod])]
x$farmsizerange <- as.factor(x$farmsizerange)
x[is.na(x)]<-0

## analysis

plot(x$area_imaflora_ha,x$area_prop.MB)

names(x)

x$nome_Mun<-mun_caat_shp@data$NM_MUN[match(x$mun_cod,mun_caat_shp@data$CD_MUN)]
x$UF<-mun_caat_shp@data$NM_UF[match(x$mun_cod,mun_caat_shp@data$CD_MUN)]

x$RL20_ha<-(x$area_prop.MB*0.2)#related to area inserted in the Caatinga boundaries
x$vsd_ha<-x$NV19-(x$RL20_ha) # Vegetation area in 2019 - área related to 20% of total area

x<-x[,c(1,14,15,2:13)]

## vsd total >> accounting art67 (1st) & art67 (2nd)
##### art.67 - prop < 4 MFs (farmsizerange=S) & threshold == NV 08 when 2008 < 20% Area prop
##### art.15 - APP acounting in RL when NV19 - NV08 >=0 (no deforastation)


for(i in 1:length(x$mun_cod)){
  
    x$vsd_total[i]<- x$NV19[i] - x$RL20[i]
  
  if(x$RL_Perc08[i] < 20 & x$farmsizerange[i]=="S") {
    x$vsd_total[i] <- x$NV19[i] - x$NV08[i]
  }
  
  if(x$NV19[i] - x$NV08[i] >= 0 & x$NV_app19[i]) {
    x$vsd_total[i]<- (x$NV19[i] + x$NV_app19[i]) - x$RL20[i]
  }
  
}

write.table(x,"Results/vsd_prop.txt")


### VSD #### tabel per municipalitie and farm size ####
names(x)

any(is.na(x$vsd_total))

vsd_mun<-data.frame(tapply(x$vsd_total,list(x$mun_cod,x$farmsizerange),sum),
                    tapply(x$vsd_ha,list(x$mun_cod,x$farmsizerange),sum))

vsd_mun$UF<-x$UF[match(rownames(vsd_mun),x$mun_cod)]
vsd_mun$nome_Mun<-x$nome_Mun[match(rownames(vsd_mun),x$mun_cod)]
vsd_mun$mun_cod<-x$mun_cod[match(rownames(vsd_mun),x$mun_cod)]
vsd_mun<-vsd_mun[,c(7:9,1:6)]
colnames(vsd_mun)<-c(colnames(vsd_mun[,c(1:3)]),"vsd_art15.67_L","vsd_art15.67_M","vsd_art15.67_S",
                    "vsd_20RL_L","vsd_20RL_M","vsd_20RL_S")


write.table(vsd_mun,"Results/vsd_mun.txt")

### VSD #### tabel per state and farm size (accounting art15.67 and 20RL (no anistia) ####

names(vsd_mun)
vsd_estados<-with(vsd_mun, aggregate(cbind(vsd_mun[,c(4:9)]), by = list(UF), FUN = sum,na.rm = T))

write.table(vsd_estados,"Results/vsd_estados.txt")



### Lucas 
names(x)

area_mun<-data.frame(tapply(x$area_prop.MB,list(x$mun_cod,x$farmsizerange),sum))

area_mun$UF<-x$UF[match(rownames(area_mun),x$mun_cod)]
area_mun$nome_Mun<-x$nome_Mun[match(rownames(area_mun),x$mun_cod)]
area_mun$mun_cod<-x$mun_cod[match(rownames(area_mun),x$mun_cod)]
area_mun<-area_mun[,c(4:6,1:3)]

a <- do.call("rbind",with(x, tapply(farmsizerange, list(mun_cod), table)))
area_mun[,7:9] <- NA
area_mun[match(rownames(a), rownames(area_mun)),7:9] <- a


colnames(area_mun)<-c(colnames(area_mun[c(1:3)]),"areaProp_L","areaProp_M","areaProp_S",
               "NumProp_L","NumProp_M","NumProp_S")
area_mun$areaMunIBGE_ha<-mun_caat_shp@data$AREA_HA[match(area_mun$mun_cod,mun_caat_shp@data$CD_MUN )]


write.table(area_mun,"Results/area_mun.txt")

