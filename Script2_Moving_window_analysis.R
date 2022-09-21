rm(list=ls(all=TRUE)) 
library("raster")
library("rgdal") 
library("reshape") 

#before processing the analysis, use "Mosaic" ArcGIS function to put together all rasters created in Script1 step as one single/final forest cover image

#open rasters
forest2000 <- raster("ForestCover2000_00N_000E - Copia (2).tif") #forest cover
n.patches <- raster("NumberPatches2000_00N_000E - Copia (2).tif") #number of patches
edge.density <- raster("EdgeDensity2000_00N_000E - Copia (2).tif") #edge density
temperatura_dia <- raster("Temperatura_Media_dia2001_hdf.tif") #daytime LST
temperatura_noite <- raster("Temperatura_Media_noite2001_hdf.tif") #nighttime LST
temp_media <- raster("temp_media.tif") #mean temperature
albedo <- raster("Albedo_Media_GLASS_2001_hdf.tif") #albedo
evapo <- raster("Evapotranspiracao_Media_2001_005.tif") #evapotranspiration
altitude<- raster("alt_005.tif") #altitude

#standardizing geographic coordinate systems
crs(temp_media) <- crs(forest2000)
crs(temperatura_dia) <- crs(forest2000)
crs(temperatura_noite) <- crs(forest2000)
crs(albedo) <- crs(forest2000)
crs(evapo) <- crs(forest2000)
crs(altitude)<- crs(temp_media)

#crop altitude and climatic variables by forest extent
temp_media<- crop(temp_media,extent(forest2000))
temperatura_dia<- crop(temperatura_dia,extent(forest2000))
temperatura_noite<- crop(temperatura_noite,extent(forest2000))
albedo<- crop(albedo,extent(forest2000))
evapo<- crop(evapo,extent(forest2000))
altitude<- crop(altitude,extent(forest2000))

#moving window configuration
a <- extent(forest2000) #total raster extension

total_longitude <- a[2] - a[1]
total_latitude <- a[4] - a[3]

#window dimension: 9 x 5.
lado_long <- 0.45 #1 pixel (=landscape) = 0.05; 9 pixels = 0.45 
lado_lat<- 0.25 #5 pixels = 0.25

n.janelas.longitude <- floor((total_longitude-0.2)/(lado_long-0.2)) #allows 4-pixel horizontal overlap between neighboring windows 
n.janelas.latitude <- floor((total_latitude-0.1)/(lado_lat-0.1)) #allows 2-pixel vertical overlap between neighboring windows 

janelas <- data.frame(janela=c(1:(n.janelas.longitude*n.janelas.latitude)), lat=rep(c(1:n.janelas.latitude), each=n.janelas.longitude), long=rep(c(1:n.janelas.longitude), times=n.janelas.latitude)) #dataframe with every window position - it is going to be used for the loop
#head(janelas)


#dataframe to store loop results
#dataframe with a single line - with NAs; for every valid pair of landscapes in the window, a new line with values will be added to the frame
results <- data.frame(forest_p1=NA, forest_p2=NA, long_p1=NA, lati_p1=NA, long_p2=NA, lati_p2=NA, temp_media_p1=NA, temp_media_p2=NA, n.patches_p1=NA, n.patches_p2=NA, edge.density_p1=NA, edge.density_p2=NA, altitude_p1=NA, altitude_p2=NA, temperatura_dia_p1=NA, temperatura_dia_p2=NA, temperatura_noite_p1=NA, temperatura_noite_p2=NA, albedo_p1=NA, albedo_p2=NA, evapo_p1=NA, evapo_p2=NA) 

tempo1<-Sys.time()

valores <- seq(1000,1341148, 1000)

for (i in 1:(n.janelas.longitude*n.janelas.latitude)){
  #i=1
  
  lat <- janelas[i,2] #window latitudinal position
  long <- janelas[i,3] #window longitudinal position
  
  extensao.janela <- extent(a[1]+((long-1)*(lado_long-0.2)), (a[1]+((long-1)*(lado_long-0.2))+lado_long), a[4]-lado_lat-((lat-1)*(lado_lat-0.1)), (a[4]-lado_lat-((lat-1)*(lado_lat-0.1))+lado_lat))
  
  forest2000_3 <- crop(forest2000, extensao.janela) #clips forest raster to show only window area
  
  #example for testing:
  #forest2000_3 <- crop(forest2000_2, extent(c(-52.45,-52.00, -25.25, -25.00)))
  #plot(forest2000_3)
  #forest2000_3[] <- c(21:65)
  
  if (sum(which(!is.na(forest2000_3[])))>0){ #if at least 1 landscape present some forest cover amount (greater than zero), runs the loop
    diferencas <- melt(as.matrix(dist(forest2000_3[])), varnames = c("p1", "p2")) #calculates the differences in forest cover between each pair of landscapes
    #head(diferencas)
    diferencas <- diferencas[diferencas$p1 > diferencas$p2,] #removes repeated pairs of landscapes 
    colnames(diferencas)[3] <- "forest_dif" #changes column names
    #head(diferencas)
    
    n.patches_3 <- crop(n.patches, extensao.janela)
    #n.patches_3[1] <- 8
    #n.patches_3[2] <- 150
    #n.patches_3[3] <- 130                                                                                             
    diferencas$porc_p1 <- forest2000_3[diferencas$p1]
    diferencas$porc_p2 <- forest2000_3[diferencas$p2]
    diferencas$npatch_p1 <- n.patches_3[diferencas$p1]
    diferencas$npatch_p2 <- n.patches_3[diferencas$p2]
    diferencas$diff_npatches <- abs(diferencas$npatch_p2 - diferencas$npatch_p1)
    
    #which pairs have a forest cover amount difference less than 5%, intermediate coverage between 0% and 100% and one of the two pixels has <=10 patches and one of the two pixels has >10 patches
    condicao1 <- abs(diferencas$forest_dif)<5
    condicao2 <- diferencas$porc_p1 > 0 & diferencas$porc_p1 < 100
    condicao3 <- diferencas$npatch_p1 <= 10 | diferencas$npatch_p2 <= 10
    condicao4 <- diferencas$npatch_p1 > 10 | diferencas$npatch_p2 > 10
    
    pares.validos <- which(condicao1 & condicao2 & condicao3  & condicao4)
    
    
    if(length(pares.validos)>0) {#if at least one of the pairs is valid, runs the loop
      
      temp_media_3 <- crop(temp_media, extensao.janela)
      temperatura_dia_3 <- crop(temperatura_dia, extensao.janela)
      temperatura_noite_3 <- crop(temperatura_noite, extensao.janela)
      albedo_3 <- crop(albedo, extensao.janela)
      evapo_3 <- crop(evapo, extensao.janela)
      edge.density_3 <- crop(edge.density, extensao.janela)
      altitude_3<- crop(altitude, extensao.janela) 
      #plot(temp_media_3)
      
      resultados.pares <- diferencas[pares.validos,] #condensed dataframe, which stores only the valid pairs of landscapes values
      #head(resultados.pares)
      
      #selection of one pair of landscape per window - the one with the smallest difference
      menor.dif <- which.max(resultados.pares$diff_npatches) #which pair has the smallest difference in forest cover between landscapes
      resultados.pares <- resultados.pares[menor.dif,]
      #resultados.pares
      
      resultados.pares$diff_npatches <- NULL
      
      # forest cover
      resultados.pares$forest_p1 <- forest2000_3[resultados.pares$p1] # adds new column in data.frame with landscape forest cover 1
      #head(resultados.pares)
      resultados.pares$forest_p2 <- forest2000_3[resultados.pares$p2]
      
      
      #central coordinates of the landscapes
      resultados.pares$long_p1 <- xyFromCell(forest2000_3, resultados.pares$p1)[,1]
      resultados.pares$lati_p1 <- xyFromCell(forest2000_3, resultados.pares$p1)[,2]
      resultados.pares$long_p2 <- xyFromCell(forest2000_3, resultados.pares$p2)[,1]
      resultados.pares$lati_p2 <- xyFromCell(forest2000_3, resultados.pares$p2)[,2]
      #head(resultados.pares)
      
      #mean LST
      resultados.pares$temp_media_p1 <- temp_media_3[resultados.pares$p1] 
      resultados.pares$temp_media_p2 <- temp_media_3[resultados.pares$p2]
      
      # n. patches
      resultados.pares$n.patches_p1 <- n.patches_3[resultados.pares$p1]   
      resultados.pares$n.patches_p2 <- n.patches_3[resultados.pares$p2]
      
      # edge density
      resultados.pares$edge.density_p1 <- edge.density_3[resultados.pares$p1]   
      resultados.pares$edge.density_p2 <- edge.density_3[resultados.pares$p2]
      #head(resultados.pares)
      
      #altitude
      resultados.pares$altitude_p1 <- altitude_3[resultados.pares$p1] 
      resultados.pares$altitude_p2 <- altitude_3[resultados.pares$p2] 
      
      #daytime_LST
      resultados.pares$temperatura_dia_p1 <- temperatura_dia_3[resultados.pares$p1] 
      resultados.pares$temperatura_dia_p2 <- temperatura_dia_3[resultados.pares$p2] 
      
      #nighttime_LST
      resultados.pares$temperatura_noite_p1 <- temperatura_noite_3[resultados.pares$p1] 
      resultados.pares$temperatura_noite_p2 <- temperatura_noite_3[resultados.pares$p2]
      
      #albedo
      resultados.pares$albedo_p1 <- albedo_3[resultados.pares$p1] 
      resultados.pares$albedo_p2 <- albedo_3[resultados.pares$p2]
      
      #evapotranspiration
      resultados.pares$evapo_p1 <- evapo_3[resultados.pares$p1] 
      resultados.pares$evapo_p2 <- evapo_3[resultados.pares$p2]
      #head(resultados.pares)
      
      resultados.pares$p1 <- NULL
      resultados.pares$p2 <- NULL
      resultados.pares$forest_dif <- NULL
      resultados.pares$porc_p1 <- NULL
      resultados.pares$porc_p2 <- NULL
      resultados.pares$npatch_p1 <- NULL
      resultados.pares$npatch_p2 <- NULL
      #head(resultados.pares)
      #head(results)
      
      results <- rbind(results, resultados.pares) #puts together the window results to the bigger dataframe
      
    }
  }
  
  
  if(i %in% valores){
    write.table(results, paste("ResultadosABRIL_Parciais10manchasUM_PAR_JANELA", i, ".txt", sep = "")) #saving results
  }
  
}

results <- results[-1,]#obs:delete 1st line

write.table(results, "ResultadosABRIL_Analise_Mundial10manchasUM_PAR_JANELA.txt") #saving results

tempo2<-Sys.time()
tempo2-tempo1

