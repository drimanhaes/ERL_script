setwd("C:/Users/adria/Google Drive/Malha_nova_CF")

### PACKAGES ####
library(sp)
library(maptools)
library(rgdal)
library(raster)
#library(GISTools)
library(rgeos)
#library(geosphere)
#library(foreign)
#library(data.table)
#library(readr)
library(sf)
#library(shapefiles)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(geobr)
library(ggthemes)
library(cowplot)
library(dplyr)


### INFOs - PER PROPERTY ####
vsd_prop<-read.table("C:/Users/adria/Google Drive/Malha_nova_CF/Results/vsd_prop.txt")
vsd_estados<-read.table("C:/Users/adria/Google Drive/Malha_nova_CF/Results/vsd_estados.txt")

unique(vsd_prop$propSize2)

unique(vsd_prop$propSize2)
vsd_prop$propSize2<-as.character(vsd_prop$propSize2)

## informations #
names(vsd_prop)

nrow(vsd_prop) # numer of properties: 1,666,756  
sum(vsd_prop$area_prop.MB.1)/1000000 #total area: 81,5 Mha
sum(vsd_prop$area_prop.MB.1[vsd_prop$propSize2=="C" | 
                              vsd_prop$propSize2=="L" | vsd_prop$propSize2=="S"  ])/1000000 # areas of registered lands: 37,5 Mha
sum(vsd_prop$area_prop.MB.1[vsd_prop$propSize2=="Sim_S" | 
                              vsd_prop$propSize2=="Sim_L"  ])/1000000 # areas of simulated lands: 44 Mha


# numer of properties
table(vsd_prop$propSize2[vsd_prop$propSize2=="C"])/sum(table(vsd_prop$propSize2))*100+
  table(vsd_prop$propSize2[vsd_prop$propSize2=="L"])/sum(table(vsd_prop$propSize2))*100+
  table(vsd_prop$propSize2[vsd_prop$propSize2=="S"])/sum(table(vsd_prop$propSize2))*100

table(vsd_prop$propSize2[vsd_prop$propSize2=="Sim_S"])/sum(table(vsd_prop$propSize2))*100+
  table(vsd_prop$propSize2[vsd_prop$propSize2=="S"])/sum(table(vsd_prop$propSize2))*100

# total area
a<-data.frame(tapply((vsd_prop$NV19.1/vsd_prop$area_prop.MB.1*100),list(vsd_prop$propSize2),mean))#,
a[c(3,5),]/sum(a$tapply.vsd_prop.area_prop.MB.1..list.vsd_prop.propSize2...sum.)*100
100-(sum(a[c(4,5),]))/sum(a)*100

# vegetation area
a<-data.frame(tapply(vsd_prop$area_prop.MB.1,list(vsd_prop$propSize2),sum))#,
a[c(2,4),]/sum(a$tapply.vsd_prop.area_prop.MB.1..list.vsd_prop.propSize2...sum.)*100
sum(a[c(2,4),])/1000000
(sum(a[c(2,4),]))/sum(a)*100

table(vsd_prop$vsd_total<0) # 143,748 properties with deficit


t<- theme (line = element_blank(),
           rect = element_blank(),
           axis.line = element_line(color = "black"),
           text = element_text(size=16),
           axis.text.y.left =  element_text(size=14),
           axis.text.x.bottom = element_text(size=14))



#remove simulates [!(vsd_prop$propSize2 == "Simul_S" | vsd_prop$propSize2 == "Simul_L" ),]


### FIG.S1 - number and area of properties ####

vsd_prop  %>%
  select(propSize2, area_prop.MB.1) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_area
sum(vsd_prop_summ_area$count[c(1:3)])

vsd_prop  %>%
  select(propSize2, NV19.1) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_veg

vsd_prop_summ_area$veg<-vsd_prop_summ_veg$sum

vsd_prop_summ_area$sum[2]/sum(vsd_prop_summ_area$sum[c(1:3)])*100 ##47.9% S, 40.7% L, 11.4% C
vsd_prop_summ_area$count[3]/sum(vsd_prop_summ_area$count[c(1:3)])*100 #97.7%
vsd_prop_summ_area$veg[1]/sum(vsd_prop_summ_area$veg[c(1:3)])*100 ## 40.6% S, 46.45% L, 12.9% C


p1<-ggplot(vsd_prop , 
           aes(x=propSize2)) +
  geom_bar() +
  labs(x=NULL) +
  scale_y_continuous("Total number of private properties") + 
  t +
  theme(axis.text.x = element_blank())

p2<-ggplot(vsd_prop, aes(x = propSize2,y= area_prop.MB.1/1000000)) 


  geom_col() + 
  labs(x=NULL) +
  scale_y_continuous("Total area of private properties (Mha)") + 
  t+
  theme(axis.text.x = element_blank())

windows()
grid.arrange(p1,p2,ncol=2,nrow=1)

### FIG.2 -  NVC (Mha and %) ####
names(vsd_prop)

vsd_prop  %>%
  filter(area_prop.MB.1>0) %>%
  select(propSize2, NV19.1) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_nvc

sum(vsd_prop_summ_nvc$sum)/1000000
sum(vsd_prop_summ_nvc$sum[c(1:3)])/sum(vsd_prop_summ_nvc$sum[c(1:5)]) *100 ## % registered lands

vsd_prop_summ_nvc$sum[1]/sum(vsd_prop_summ_nvc$sum[c(1:3)])*100

vsd_prop  %>%
  filter(area_prop.MB.1>0) %>%
  select(propSize2, area_prop.MB.1) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_area2

vsd_prop_summ_nvc$sum[3]/vsd_prop_summ_area2$sum[3]*100




vsd_prop  %>%
  filter(area_prop.MB.1>0) %>%
  select(propSize2, RL_Perc19) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_nvc_p


p2a<-ggplot(vsd_prop_summ_nvc,aes(x = propSize2,y= sum/1000000,fill=propSize2))+
  geom_col()+
  labs(x=NULL)+
  scale_y_continuous("Native vegetation cover (Mha)") + 
  theme(legend.position = "none")+
  t

p2b<-ggplot(vsd_prop, aes(x = propSize2,y= RL_Perc19, fill=propSize2)) +
  geom_boxplot()+
  stat_summary(fun.y="mean")+
  theme(legend.position = "none")+
  labs(x=NULL) +
  #stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  scale_y_continuous("Native vegetation cover (%)") + 
  t+
  theme(axis.text.x = element_blank())



windows()
grid.arrange(p2a, p2b, ncol=2,nrow=1) # Fig2

### FIG4.a-b - NVD in LR (Mha e %) #####
vsd_regist<-vsd_prop[vsd_prop$propSize2=="S" | vsd_prop$propSize2=="L" | vsd_prop$propSize2=="C" ,]

vsd_def<-vsd_regist[vsd_regist$vsd_total<0,]
sum(vsd_def$vsd_total)


nrow(vsd_def)/nrow(vsd_regist)*100
nrow(vsd_app)/nrow(vsd_regist)*100

vsd_def$nvd_Perc19<-(vsd_def$vsd_total/vsd_def$area_prop.MB.1)*100

vsd_def  %>%
  #filter(area_prop.MB.1>0) %>%
  #filter(vsd_total<0) %>%
  select(propSize2, vsd_total) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_vsd

sum(vsd_prop_summ_vsd$sum[c(1,3)])/
  sum(vsd_prop_summ_vsd$sum[c(1:3)])*100

vsd_def  %>%
  #filter(area_prop.MB.1>0) %>%
  #filter(vsd_total<0) %>%
  select(propSize2, nvd_Perc19) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_nvd_p

names(vsd_def)

p1<-ggplot(vsd_def, 
           aes(x = propSize2,y= vsd_total/1000000,fill=propSize2)) +
  geom_col() + 
  theme(legend.position = "none")+
  labs(x=NULL) +
  scale_y_continuous("Native vegetation deficit - LR (Mha)") + 
  t+
  theme(axis.text.x = element_blank())

p2<-ggplot(vsd_def, aes(x = propSize2,y= nvd_Perc19, fill=propSize2)) +
  geom_boxplot()+
  stat_summary(fun.y="mean")+
  theme(legend.position = "none")+
  labs(x=NULL) +
  #stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  scale_y_continuous("Native vegetation deficit - LR (%)") + 
  t+
  theme(axis.text.x = element_blank())

windows()
grid.arrange(p1,p2,ncol=2,nrow=1) 

### FIG4.c-d - NVD in APP (Mha e %) ####
vsd_app<-vsd_regist[!vsd_regist$vsd_def_APP ==0,]

nrow(vsd_app)/nrow(vsd_regist)*100 #%NVD APP
sum(vsd_regist$area_app.1[!vsd_regist$area_app.1==0])

names(vsd_app)

vsd_app$nvd_Perc19<-(vsd_app$vsd_def_APP/vsd_app$area_app.1)*100

vsd_app  %>%
  #filter(area_prop.MB.1>0) %>%
  #filter(vsd_total<0) %>%
  select(propSize2, vsd_def_APP) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_nvd_app

sum(vsd_prop_summ_nvd_app$sum[c(1:3)])

vsd_app  %>%
  #filter(area_prop.MB.1>0) %>%
  #filter(vsd_total<0) %>%
  select(propSize2, nvd_Perc19) %>%
  gather(key, value, -propSize2) %>%
  group_by(propSize2) %>%
  summarise(sum=sum(value),
            avg = mean(value),
            stdev = sd(value),
            count = table(propSize2))-> vsd_prop_summ_nvd_app_p

p1<-ggplot(vsd_app, 
           aes(x = propSize2,y= vsd_def_APP/1000000*-1,fill=propSize2)) +
  geom_col() + 
  theme(legend.position = "none")+
  labs(x=NULL) +
  scale_y_continuous("Native vegetation deficit - APP (Mha)") + 
  t+
  theme(axis.text.x = element_blank())

p2<-ggplot(vsd_app, aes(x = propSize2,y= nvd_Perc19*-1, fill=propSize2)) +
  geom_boxplot()+
  stat_summary(fun="mean")+
  theme(legend.position = "none")+
  labs(x=NULL) +
  #stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  scale_y_continuous("Native vegetation deficit - APP (%)") + 
  t+
  theme(axis.text.x = element_blank())


windows()
grid.arrange(p1,p2,ncol=2,nrow=1)

sum(vsd_prop_summ_app$sum[c(1,3)]) 


### Table 2 -SATES AND BIOME ####

vsd_estados
vsd_estados_p



p1<-ggplot(vsd_estados2[-10,], 
           aes(x = area_total,y= def_total,col=UF)) +
  geom_jitter() + 
  
  labs(x=NULL) +
  scale_y_continuous("Total native vegetation deficit (ha)") + 
  t+
  
  theme(axis.text.x = element_blank())













### maps ####
#a<-list_geobr()

UFs<-read_state(code_state = "all", year=2019)
uf_caat<-UFs[UFs$abbrev_state %in% vsd_prop$UF,]
unique(uf_caat$abbrev_state)

muns <- read_municipality(code_muni="all", year=2019)
muns_caat<-muns[muns$code_muni %in% unique(vsd_prop$mun_cod),]

biom<-read_biomes(simplified = T, year=2019)
caat<-biom[biom$name_biome=="Caatinga",]


vsd_prop_map<-vsd_prop
vsd_prop_map$code_muni<-vsd_prop_map$mun_cod
names(vsd_prop)


### Fig3 - %NVC per land tenure ######
windows()
# Large - L
vsd_prop_map %>% 
  filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(RL_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->RL_perc

ggplot()+  geom_sf(data=RL_perc$geom, aes(fill=RL_perc$RL_Perc19), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA)+
          #  breaks=c(seq(0,1,0.5)),labels=c(seq(0,1,0.5)),limits=c(0,1))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(subtitle="Large") +
  theme_map()+
  theme(legend.position = "none",title=element_text(size=35,face="bold"))->map_L

# Community -C
vsd_prop_map %>% 
  filter(propSize2 == "C") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(RL_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->RL_perc

ggplot()+  geom_sf(data=RL_perc$geom, aes(fill=RL_perc$RL_Perc19), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA) +
                    #  breaks=c(seq(0,1,0.5)),labels=c(seq(0,1,0.5)),limits=c(0,1))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(subtitle="Community") +
  theme_map()+
  theme(legend.position = "none",title=element_text(size=35,face="bold"))->map_C

# small -S
vsd_prop_map %>% 
  filter(propSize2 == "S") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(RL_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->RL_perc

ggplot()+  geom_sf(data=RL_perc$geom, aes(fill=RL_perc$RL_Perc19), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA)+
                    #  breaks=c(seq(0,0.8,0.2)),labels=c(seq(0,0.8,0.2)),limits=c(0,0.8))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(title="Small") +
  theme_map()+
  theme(legend.position = "none",title=element_text(size=25,face="bold"))->map_S

# Simulated Large - Sim_L
vsd_prop_map %>% 
  filter(propSize2 == "Sim_L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(RL_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->RL_perc

ggplot()+  geom_sf(data=RL_perc$geom, aes(fill=RL_perc$RL_Perc19), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA)+
                     # breaks=c(seq(0,1,0.5)),labels=c(seq(0,1,0.5)),limits=c(0,1))+
  # geom_sf(data=uf_caat2, fill="transparent")+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(title="Sim_L") +
  theme_map()+
  theme(legend.position = "none",title=element_text(size=25,face="bold"))->map_SimL


# Sim Small - Sim_S
vsd_prop_map %>% 
  filter(propSize2 == "Sim_S") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(RL_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->RL_perc

ggplot()+  
  geom_sf(data=RL_perc$geom, aes(fill=RL_perc$RL_Perc19), lwd = 0)+
    scale_fill_gradient(low = "red", high = "blue", na.value = NA,
   breaks=c(seq(0,100,25)),labels=c(seq(0,100,25)),limits=c(0,100))+
  # geom_sf(data=uf_caat2, fill="transparent")+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(title="Sim_S", fill = "") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_SimS


map_C

### Fig S2 LR and APP  ######
names(vsd_def)
vsd_def$code_muni<-vsd_def$mun_cod

windows()
# Large - L
vsd_def %>% 
  filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(nvd_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->def_L

ggplot()+  geom_sf(data=def_L$geom, aes(fill=def_L$nvd_Perc19), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
  breaks=c(seq(-20,0,5)),labels=c(seq(-20,0,5)),limits=c(-20,0))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(subtitle="Large") +
  theme_map()+
  theme(legend.position = "none",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_L

# Small properties (C + S)
vsd_def %>% 
  filter(propSize2 == "C" | propSize2 == "S") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(nvd_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->def_S
nrow(def_S)

ggplot()+  geom_sf(data=def_S$geom, aes(fill=def_S$nvd_Perc19), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
  breaks=c(seq(-20,0,5)),labels=c(seq(-20,0,5)),limits=c(-20,0))+
  #breaks=c( -1.553220e+02, -5.853600e+01 ,-1.443600e+01 ,0 ),labels=c( -155, -58, -14, 0  ))+
    geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(subtitle="Small",fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_S


windows()
plot_grid(map_L,map_S, nrow=1, ncol=2)


vsd_app$code_muni<-vsd_app$mun_cod
names(vsd_app)

# Large - L
vsd_app %>% 
  filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(nvd_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->def_L

ggplot()+  geom_sf(data=def_L$geom, aes(fill=def_L$nvd_Perc19*-1), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
    breaks=c(seq(-100,0,25)),labels=c(seq(-100,0,25)),limits=c(-100,0))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(subtitle="Large - APP") +
  theme_map()+
  theme(legend.position = "none",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_L

# Small properties (C + S)
vsd_app %>% 
  filter(propSize2 == "C" | propSize2 == "S") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(nvd_Perc19), funs(mean)) %>% 
  inner_join(muns, by = "code_muni")->def_S

ggplot()+  geom_sf(data=def_S$geom, aes(fill=def_S$nvd_Perc19*-1), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
    breaks=c(seq(-100,0,25)),labels=c(seq(-100,0,25)),limits=c(-100,0))+
  #breaks=c( -1.553220e+02, -5.853600e+01 ,-1.443600e+01 ,0 ),labels=c( -155, -58, -14, 0  ))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(subtitle="Small",fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_S


### Fig 5ab LR #####
names(vsd_def)

vsd_def %>% 
  filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(vsd_total), funs(sum)) %>% 
  inner_join(muns, by = "code_muni")->def_L

vsd_def %>% 
  filter(propSize2 == "C" | propSize2 == "S") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(vsd_total), funs(sum)) %>% 
  inner_join(muns, by = "code_muni")->def_S

vsd_def %>% 
 # filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(vsd_total), funs(sum)) %>% 
  inner_join(muns, by = "code_muni")->def_all


def_all$ratio<-NA

def_all$ratio<-def_L$vsd_total[match(def_all$code_muni,def_L$code_muni)]
def_all$ratioL<-def_all$ratio/def_all$vsd_total*100

def_all$ratio<-def_S$vsd_total[match(def_all$code_muni,def_S$code_muni)]
def_all$ratioS<-def_all$ratio/def_all$vsd_total*100


ggplot()+  geom_sf(data=def_all$geom, aes(fill=def_all$ratioL*-1), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
                      breaks=c(seq(-100,0,25)),labels=c(seq(-100,0,25)),limits=c(-100,0))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "none",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->ratio_L

ggplot()+  geom_sf(data=def_all$geom, aes(fill=def_all$ratioS*-1), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
  breaks=c(seq(-100,0,25)),labels=c(seq(-100,0,25)),limits=c(-100,0))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->ratio_S


windows()
grid.arrange(ratio_S)#,ratio_S,ncol=2,nrow=1)

nrow(def_all)/nrow(vsd_mun)*100
length(which(def_all$ratioS > def_all$ratioL))

nrow(def_all[def_all$ratioS > 50,])/nrow(def_all)*100

       
### Fig 5cd APP #####
names(vsd_app)

vsd_app %>% 
  filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(vsd_total), funs(sum)) %>% 
  inner_join(muns, by = "code_muni")->def_L

vsd_app %>% 
  filter(propSize2 == "C" | propSize2 == "S") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(vsd_total), funs(sum)) %>% 
  inner_join(muns, by = "code_muni")->def_S

vsd_app %>% 
  # filter(propSize2 == "L") %>% 
  group_by(code_muni) %>% 
  summarise_at(vars(vsd_total), funs(sum)) %>% 
  inner_join(muns, by = "code_muni")->def_all


def_all$ratio<-NA

def_all$ratio<-def_L$vsd_total[match(def_all$code_muni,def_L$code_muni)]
def_all$ratioL<-def_all$ratio/def_all$vsd_total*100

def_all$ratio<-def_S$vsd_total[match(def_all$code_muni,def_S$code_muni)]
def_all$ratioS<-def_all$ratio/def_all$vsd_total*100


ggplot()+  geom_sf(data=def_all$geom, aes(fill=def_all$ratioL*-1), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
                      breaks=c(seq(-100,0,25)),labels=c(seq(-100,0,25)),limits=c(-100,0))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "none",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->ratio_L

ggplot()+  geom_sf(data=def_all$geom, aes(fill=def_all$ratioS*-1), lwd = 0)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
                      breaks=c(seq(-100,0,25)),labels=c(seq(-100,0,25)),limits=c(-100,0))+
  geom_sf(data=caat, fill="transparent",col="black")+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->ratio_S


windows()
grid.arrange(ratio_L)#,ratio_S,ncol=2,nrow=1)

nrow(def_all)/nrow(vsd_mun)*100
length(which(def_all$ratioS > def_all$ratioL))

nrow(def_all[def_all$ratioS > 50,])/nrow(def_all)*100


### Fig6 - states #####
vsd_estados<-read.table("C:/Users/adria/Google Drive/Malha_nova_CF/Results/vsd_estados.txt")

names(vsd_estados)

vsd_estados$NVC_p<-(vsd_estados$NVC.RL_reg/vsd_estados[,c(1)])*100
vsd_estados$NVD_p<-((rowSums(vsd_estados[,c(9:10)]))/(vsd_estados[,c(1)]))*100
vsd_estados$NVD.APP_p<-((rowSums(vsd_estados[,c(11:12)]))/(vsd_estados[,c(5)]))*100

vsd_estados$abbrev_state<-rownames(vsd_estados)

plot(uf_caat$geom)


vsd_estados[c(1:9),] %>% 
  #group_by(code_muni) %>% 
  #summarise_at(vars(NVD_p), funs(mean)) %>% 
  inner_join(uf_caat, by = "abbrev_state")->UF_p

plot(vsd_estados$NVC_p, vsd_estados$NVD_p,"n")
text(vsd_estados$NVC_p, vsd_estados$NVD_p,vsd_estados$abbrev_state)

windows()

ggplot()+  geom_sf(data=UF_p$geom, aes(fill=UF_p$NVC_p), lwd = 0)+
  geom_sf(data=caat, fill="transparent",col="white")+
  geom_sf_text(data=UF_p$geom,aes(label =UF_p$abbrev_state),size=6,family="sans",fontface = "bold",)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
  breaks=c(seq(0,100,25)),labels=c(seq(0,100,25)),limits=c(0, 100))+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_nvc

ggplot()+  geom_sf(data=UF_p$geom, aes(fill=UF_p$NVD_p), lwd = 0)+
  geom_sf(data=caat, fill="transparent",col="white")+
  geom_sf_text(data=UF_p$geom,aes(label =UF_p$abbrev_state),size=6,family="sans",fontface = "bold",)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
  breaks=c(seq(-4,0,1)),labels=c(seq(-4,0,1)),limits=c(-4,0))+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_nvd

ggplot()+  geom_sf(data=UF_p$geom, aes(fill=UF_p$NVD.APP_p), lwd = 0)+
    geom_sf(data=caat, fill="transparent",col="white")+
  geom_sf_text(data=UF_p$geom,aes(label =UF_p$abbrev_state),size=6,family="sans",fontface = "bold",)+
  scale_fill_gradient(low = "red", high = "blue", na.value = NA,
  breaks=c(seq(-100,0,20)),labels=c(seq(-100,0,20)),limits=c(-100,0))+
  coord_sf(expand = FALSE)+
  labs(fill="") +
  theme_map()+
  theme(legend.position = "right",legend.key.size=unit(12,"mm"),
        legend.text = element_text(size = 20),legend.title = element_text(size = 20),
        title=element_text(size=25,face="bold"))->map_nvd_app

names(vsd_prop)
vsd_prop$NVC_p<-(vsd_prop$NV19.1/vsd_prop$area_prop.MB.1)*100

vsd_mun<-data.frame(tapply(vsd_prop[,c(5:9)],list(vsd_prop$mun_cod,vsd_prop$propSize2),sum))

nrow(vsd_mun[vsd_mun$S<25,])/nrow(!is.na(vsd_mun$S))*100
nrow(vsd_mun[vsd_mun$L<25,])/nrow((vsd_mun))*100

                                  
### Fig7 - vegetation loss #####
names(vsd_def)

vsd_def$desm.area<-(vsd_def$NV19.1-vsd_def$NV08.1)/vsd_def$area_prop.MB.1*100# (%)
vsd_def$desm.tx<-(vsd_def$NV19.1-vsd_def$NV08.1)/11 # (ha/ano)

vsd_estados2<-vsd_estados[-10,]
vsd_estados2[,c(17)]<-data.frame(tapply(vsd_def$desm.area,list(vsd_def$UF),mean))
vsd_estados2[,c(18)]<-data.frame(tapply(vsd_def$desm.tx,list(vsd_def$UF),sum))
colnames(vsd_estados2)<-c(colnames(vsd_estados[1:16]),"desm_area","desm_tx")
names(vsd_estados2)

vsd_estados2$NVD_t<-rowSums(vsd_estados2[,c(9:12)])/vsd_estados2$area_reg*100
caat<-c(mean(vsd_estados2$desm_area),mean(vsd_estados2$NVD_t))

windows()
ggplot(vsd_estados2, aes(x=desm_area*-1,y=NVD_t))+
  #  geom_point(size=2,alpha=0.2) + 
  geom_text(aes(x=desm_area*-1,y=NVD_t, label=as.character(abbrev_state)), vjust = 1,size=5,fontface = "bold")+
  geom_text(aes(x=caat[1]*-1, y=caat[2], label="Caatinga"), vjust = 1,size=5,fontface = "bold",color="red")+
#  geom_smooth(method=lm, formula = y ~ x,se=F,color="grey")+
  scale_x_continuous("Native vegetation loss rate (ha.y-1)") +
  scale_y_continuous("Total native vegetation deficit (%)") + 
  t


