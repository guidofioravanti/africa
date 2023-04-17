rm(list=objects())
library("terra")
library("sf")
library("dplyr")
library("stringr")
library("lubridate")
library("ggplot2")
library("tidyr")
library("scico")
library("ggspatial")
library("ggnewscale")
library("dtwclust")
#library("elevatr")
#elevatr::get_elev_raster(locations = st_geometry(africa_ipcc_ar6_areas),z =6,clip = "bbox")->elev

rast("elev.tif")->elev
rworldmap::countriesCoarse->world
cleangeo::clgeo_Clean(world)->world
st_as_sf(world)->world
world %>%
  filter(SOVEREIGNT %in% c("Somalia","Eritrea","Ethiopia","Kenya","Uganda","Djibouti","Sudan","South Sudan","United Republic of Tanzania","Burundi"))->sf_africa

seq.Date(from=as.Date("1981-01-01"),to=as.Date("2022-12-01"),by="month")->calendar

#IPCC AR6 macro regions
st_read("IPCC-WGI-reference-regions-v4.geojson.json")->ipcc_ar6_areas
ipcc_ar6_areas |>
  filter(grepl("^[NS]EAF$",Acronym))->africa_ipcc_ar6_areas


st_intersection(africa_ipcc_ar6_areas,sf_africa)->africa_ipcc_ar6_areas

rast("africa_remapcon_chirps-v2.0.1981_2022.monthly_p05.nc")->chirps_africa
names(chirps_africa)<-calendar

chirps_africa[[grepl("^19[89].-.+",names(chirps_africa))]]->sub1
chirps_africa[[grepl("^200.-.+",names(chirps_africa))]]->sub2
chirps_africa[[grepl("^2010-.+",names(chirps_africa))]]->sub3
rm(chirps_africa)
rast(list(sub1,sub2,sub3))->sub_chirps_africa

crop(sub_chirps_africa,st_union(st_geometry(africa_ipcc_ar6_areas)))->chirps_ipcc_ar6_areas
rm(list = objects(pattern = "sub[123]"))

st_union(africa_ipcc_ar6_areas)->subArea
vect(subArea)->vsubArea
crop(chirps_ipcc_ar6_areas,subArea)->mygrid
mask(mygrid,vsubArea)->mygrid


purrr::map(1:12,.f=function(.month){
  
  mygrid[[grep(paste0("^[0-9]{4}-",str_pad(.month,pad="0",side="left",width=2),"-01$"),names(mygrid))]]->.x
  mean(.x)
  
})->listOut

rast(listOut)->mybrick
names(mybrick)<-month.abb
rm(listOut)
as.data.frame(mybrick,cell=TRUE,xy=TRUE)->mydf
dist(mydf|> select(-cell,-x,-y),method = "euclidean")->mydistance
hclust(mydistance,method = "ward.D2")->hclust_result
plot(hclust_result)
K<-10
cutree(hclust_result,k=K)->mydf$cluster

mydf %>%
  mutate(cluster=ifelse(cluster==1,NA,cluster),
         cluster=ifelse(cluster==10,NA,cluster),
         cluster=ifelse(cluster==9,NA,cluster)) %>%
  filter(!is.na(cluster))->mydf

dist(mydf |> select(-cell,-x,-y),method = "euclidean")->mydistance
hclust(mydistance,method = "ward.D2")->hclust_result
plot(hclust_result)
K<-5
cutree(hclust_result,k=K)->mydf$cluster

rast(mydf |> select(x,y,cluster))->grid_clusters
crs(grid_clusters)<-"epsg:4326"

classify(grid_clusters,rcl=matrix(data=c(0,1,0,1,2,1,2,3,2,3,4,3,4,5,4,5,6,5,6,7,6,7,8,7,8,9,8,9,10,9)))->grid_clusters


mask(elev,vsubArea)->elev
elev[elev<0]<-NA
ggplot()+
  layer_spatial(data=elev)+
  scico::scale_fill_scico(palette="grayC",na.value="transparent",guide="none")+
  new_scale_fill()+
  ggspatial::layer_spatial(data=grid_clusters,alpha=0.5)+
  scale_fill_discrete(na.value="transparent")+ #,guide="none"
  geom_sf(data=africa_ipcc_ar6_areas,fill="transparent")+
  theme_bw()+
  theme(panel.grid = element_blank())->mappa
print(mappa)
  
pivot_longer(mydf %>% filter(!is.na(cluster)),cols=!c("x","y","cluster","cell"),names_to = "mm") %>%
  mutate(mm=factor(mm,levels = month.abb,ordered=TRUE))->lmydf

ggplot(data=lmydf,aes(x=mm,y=value))+
  geom_line(aes(group=cell,colour=cell))+
  facet_wrap(~cluster)->grafico
print(grafico)


lmydf %>%
  dplyr::select(cluster,mm,value) %>%
  filter(!is.na(cluster)) %>%
  group_by(mm,cluster) %>%
  summarize(sintesi=median(value)) %>%
  ungroup()->lmydf_sintesi_cluster

ggplot(data=lmydf_sintesi_cluster,aes(x=mm,y=sintesi))+
  geom_line(aes(group=cluster,colour=cluster))+
  facet_wrap(~cluster)->grafico
print(grafico)

writeRaster(grid_clusters,"grid_clusters.tif",overwrite=TRUE)