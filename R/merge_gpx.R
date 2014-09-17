#' Agrupar ficheiros GPX num unico shapefile
#' Packages necessarios
kpacks <- c('rgdal', 'XML', 'maptools', 'rgeos')
new.packs <- kpacks[!(kpacks %in% installed.packages()[,"Package"])]
if(length(new.packs)) install.packages(new.packs)
lapply(kpacks, require, character.only=T)
remove(kpacks, new.packs)

#' Construir a lista de bases de dados DB do Birdmonitor ---------------------------
gpxfolder <- '../gpx'

lgpx <- data.frame('filename' = list.files(file.path(gpxfolder), pattern = '.gpx',
                                           recursive = T),
                   stringsAsFactors = F)

#' Listas para agregar os objetos GPX ----------------------------------------------
gpxp <- list()
gpxl <- list()

#' Loop sobre os ficheiros GPX para leitura com readOGR ----------------------------
for(i in 1:nrow(lgpx)){
  gpxpi <- try(readOGR(dsn = file.path(gpxfolder, lgpx$filename[i]),
                       layer = 'waypoints')[, 1:23], silent = T)
  gpxli <- try(readOGR(dsn = file.path(gpxfolder, lgpx$filename[i]),
                       layer = 'tracks')[, 1:14], silent = T)
  if(class(gpxpi) == "try-error"){NULL} else{
    gpxp[[i]] <- gpxpi}
  if(class(gpxli) == "try-error"){NULL} else{
    gpxli <- spChFIDs(gpxli, paste0(i, '_',gpxli@lines[[1]]@ID))
    gpxl[[i]] <- gpxli}
}

#' Combinar os objetos espaciais num unico -----------------------------------------
if(length(gpxp) > 0){
  gpxp <- gpxp[!sapply(gpxp, is.null)]
  gpxall_p <- do.call('rbind', gpxp)
  writeOGR(gpxall_p, gpxfolder, "shape_p", driver="ESRI Shapefile")
}
if(length(gpxl) > 0){
  gpxl <- gpxl[!sapply(gpxl, is.null)]
  gpxall_l <- do.call('rbind', gpxl)
  writeOGR(gpxall_l, gpxfolder, "shape_l", driver="ESRI Shapefile")
}

#' Check for duplicated lines  
duplicated(gpxall_p@data)
