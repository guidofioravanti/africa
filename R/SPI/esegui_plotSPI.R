rm(list=objects())


purrr::walk(c(3,6,9,12),.f=function(spi_scale){

  list.files(pattern=paste0("^spi",spi_scale,"_climatol.+nc$"))->ffile
  if(length(ffile)!=1) stop("Zero or too many files!")
  
  rmarkdown::render(input="plotSPI.Rmd",output_file = paste0("plotSPI",spi_scale,".html"),params = list(spi_scale=spi_scale,nomeFile=ffile))
  
})

