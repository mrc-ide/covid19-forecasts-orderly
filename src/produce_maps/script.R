# Produce maps of deaths by continent

# loading the map data (simple features format)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ens_week <- readRDS("ensemble_weekly_qntls.rds")

# generating the ISO3 code for the model predictions to link with the simple features map data
ens_week$iso_a3 <- countrycode::countrycode(gsub("_"," ",ens_week$country,fixed=T),"country.name","iso3c")
# filling in the missing values
if(any(is.na(ens_week$iso_a3))) warning(print("Country names need matching to ISO 3 codes"))

sis<-unique(ens_week$si)
for(j in 1:length(sis)){
  si<-sis[j]
  world_df<-merge(world,ens_week[ens_week$si==si,])
  ##world_df_pts<-st_point_on_surface(world_df)
  world_df_pts <- sf::st_centroid(world_df)
  coords <- as.data.frame(st_coordinates(world_df_pts))
  coords$label <- paste0(world_df$geounit,"\n",prettyNum(world_df$`50%`,big.mark=","))
  ##coords$label <- paste0(world_df$geounit)
  coords$continent<-world_df$continent

  p <- ggplot() +
      geom_sf(data = world,fill="grey",col="white",size=0.1)+
      geom_sf(data = world_df_pts, aes(size=`50%`),alpha=0.2,col="red") +
      geom_sf(data = world_df_pts, aes(size=`50%`),shape=1,col="red") +
      geom_text_repel(data = coords, aes(x=X,y=Y,label=label)) +
      guides(size=F) + labs(x="",y="")+
      theme(panel.background = element_rect("white"),panel.grid=element_blank()) +
      ##coord_sf(xlim=dim[c(1,3)],ylim=dim[c(2,4)])
      coord_sf()

    ggsave(paste0("map_death_",gsub(" ","_",c,fixed=T),"_",si,".png"), p)
  }
}
