##################################
#7 march 2023
##################################
rm(list=objects())
library("SPEI")
library("terra")
library("tidyverse")
library("sf")
library("imputeTS")

################################################################
# Input file name: if empty "", the program looks for a .nc file with remapcon string not containing "spi"
################################################################
nomeFile<-""

####################
#accumulation period
####################
spi_scale<-3

#############################
#firstYear in the data time series
#############################
firstYear<-1981

############################
#years for SPI output maps
############################
annoI<-2018
annoF<-2022

############################
#SPI baseline
############################
annoICLIM<-1981
annoFCLIM<-2010


IMPUTE<-FALSE #TRUE if we want to imput missing data

if(!nchar(nomeFile)){
  
  print("#######################################################################")
  print("nomeFile is empty, I will look for one file to use for SPI computation!")
  print("#######################################################################\n\n\n")
  
  list.files(pattern=".+remapcon.+\\.nc$")->possibleFiles
  possibleFiles[!grepl("^spi[0-9]{1,2}",possibleFiles)]->resultsFiles
  
  if(length(resultsFiles)!=1){
    stop("zero or too many netCDF files found! I cannot go on, sorry!")
  }
  
  nomeFile<-resultsFiles[1]
  print("#######################################################################")
  print(glue::glue("Found {nomeFile} for SPI computation!"))
  print("#######################################################################\n\n\n")
  
}#su nchar



######################################################################################################
#We have two masks: one used for calculating SPI (larger than the African coastline); one for plotting
######################################################################################################

#load the buffered mask for AFRICA (to speed up SPI calculation)
st_read("../africa_mask_buffered/africa.shp")->africa_buffered
vect(africa_buffered)->vectAfrica_buffered

#non buffered mask for the African continent
st_read("../africa_mask/africa.shp")->africa
st_crs(africa)<-4326
vect(africa)->vectAfrica

rast(nomeFile)->mygrid

#omit small islands in the Ocean
terra::focal(mygrid,fun="modal",na.rm=FALSE,na.policy="omit")->mygridSmoothed #usiamo come maschera per eliminare punti nel mare, isolette
terra::mask(mygrid,mygridSmoothed)->mygrid
terra::mask(mygrid,vectAfrica_buffered)->mygrid
ncell(mygrid)->numeroCelle
as.data.frame(mygrid[[1]],xy=TRUE,cell=TRUE)->dfGrid

calcolaSPI<-function(.grid,.qualeCella,.annoI=annoI,.meseI=1,.annoF=annoF,.meseF=12,.spi_scale=spi_scale){
  
  unlist(terra::extract(.grid,.qualeCella,list=F))->x
  if(all(is.nan(x)|| is.na(x))){return(rep(NA_real_,((.annoF-.annoI+1)*12)))} #no data available
  
  ts(x,start=c(firstYear,1),frequency = 12)->myts
  
  if(any(is.na(x)) && (!IMPUTE)){return(rep(NA_real_,((.annoF-.annoI+1)*12)))} #no data available
  if(any(is.na(x)) && (IMPUTE)){imputeTS::na_locf(myts,na_remaining="mean")->myts} #no data available
  
  spi(myts,scale = .spi_scale,na.rm=FALSE,ref.start = c(annoICLIM,1),ref.end = c(annoFCLIM,12))->dfSPI
  
  nrow(dfSPI$fitted)->numeroRighe #(2022-1981+1)*12
  
  as.vector(window(dfSPI$fitted,start=c(.annoI,.meseI),end=c(.annoF,.meseF)))->ris
  if(any(is.na(ris) || is.nan(ris)) ) {} #{browser()}
  
  ris
  
}#fine function

purrr::walk(c(3,6,9,12),.f=function(spi_scale){

    #calculate SPI
    purrr::map(1:numeroCelle,.f=~(calcolaSPI(.grid=mygrid,.qualeCella=.,.spi_scale = spi_scale))) ->listaOut
    
    purrr::map(1:((annoF-annoI+1)*12),.f=function(.x){
      
      purrr::map_dbl(listaOut,.x)->valoriSPI
      data.frame(valoriSPI,cell=1:numeroCelle)->dfSPI
    
      left_join(dfGrid,dfSPI)[,c("valoriSPI","cell","x","y")]->dfSPI
      names(dfSPI)[1]<-c("z")
      rast(dfSPI[,c("x","y","z")])->mygrid
      
      #mask following the coastline
      mask(mygrid,vectAfrica)
      
    })->listaRastersSPI
    
    tidyr::expand_grid(yy=annoI:annoF,mm=1:12) %>%
      mutate(yymmdd=glue::glue("{yy}-{str_pad(mm,pad='0',side='left',width='2')}-01")) %>%
      pull(yymmdd)->names(listaRastersSPI)
    
    rast(listaRastersSPI)->mybrick
    crs(mybrick)<-"epsg:4326"
    try({writeCDF(mybrick,glue::glue("spi{spi_scale}_climatol_{annoICLIM}_{annoFCLIM}_{nomeFile}"),overwrite=TRUE,prec="float",unit="",zname="time",compression=1,missval=-9999)})

})#fine walk



