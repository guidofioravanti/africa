rm(list=objects())
library("ggplot2")
library("readr")
library("dplyr")

read_delim("sintesi_cluster.csv",delim=";",col_names = TRUE) |>
  mutate(yy=as.Date(yy)) |>
  ggplot()+
  geom_line(aes(x=yy,y=sintesi,colour=modello,group=modello))+
  facet_wrap(~cluster)
