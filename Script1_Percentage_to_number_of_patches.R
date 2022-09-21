removeTmpFiles(1)#

library("raster")
library("reshape")
library("igraph")

pasta1 <- "C:\\Users\\RITA\\Documents\\Mestrado - Uerj\\Projeto\\Script - Análise mundial\\treecover2000" #original location of Hansen forest cover rasters
diretorio1 <- dir(pasta1) #raster's names
lista_granules1 <- as.data.frame(diretorio1) #organize raster's names in a dataframe

#main loop
for (i in 1:dim(lista_granules1)[1]){ 
  #i=1
  name1 <- as.character(lista_granules1[i,1]) #raster name 
     
  #creating raster
  cover2000 <- raster(name1)
  
  #cover2000<-crop(cover2000,extent(-40,-39,-10,-9))#just for testing

  #transforms into forest (1) and non-forest (0) pixels
  cover2000 <- calc(cover2000, fun=function(x){ifelse(x >60, 1,0)}) 
  #the value of 60, in this case, is from the forest cover depicted by Hansen's rasters
  
  #plot(cover2000) #in case you would like to visualize - may take some time to process
  
  #a) calculates forest cover (%) in each 5 x 5 km landscape (0.05 x 0.05 arc seconds) 
  cover2000.upscaled <- aggregate(cover2000, fact=c(200,200),  fun=sum)
  cover2000.upscaled <- (cover2000.upscaled/(200*200))*100 #we multiply by 100 to transform into a percentage, as we can see in the map!
  #plot(cover2000.upscaled)
  
  name4 <- substring(name1,29,nchar(name1)-4) #removes "Hansen_GFC2015_treecover2000_" from every analyzed raster

  writeRaster(cover2000.upscaled, paste("ForestCover2000",name4,".tif", sep=""),overwrite=TRUE) #saves new raster, which presents information of forest cover in each landscape (5 x 5 km)

  #b) calculates the number of patches and edge density in each 5 x 5 km landscape
  n.paisagens <- ncell(cover2000.upscaled)
  xy.centro.paisagens <- xyFromCell(cover2000.upscaled, 1:n.paisagens)
  
  numero.manchas <- rep(0, n.paisagens)
  densidade.borda <- rep(NA, n.paisagens)
  
  for(j in 1:n.paisagens){
    #j=1
    #defining landscape limits
    xy.centro <- xy.centro.paisagens[j,] #landscape central coordinate
    xy.limites <- c(xy.centro[1]-(100*0.00025), xy.centro[1]+(100*0.00025), xy.centro[2]-(100*0.00025), xy.centro[2]+(100*0.00025)) #xmin, xmax, ymin and ymax values
    paisagem <- crop(cover2000, extent(xy.limites)) #clips raster using "xy.limites" extension 
    
    #plot(paisagem)
    
    if(sum(paisagem[])>0){ #analysis is going to take place only if there is any forest percentage
      
    	#calculating number of patches in the landscape
    	patches <- clump(paisagem, gaps=F, directions=8)
    	#plot(patches)
    	n.patches <- max(patches[], na.rm=T)
    	numero.manchas[j] <- n.patches 
    
    
    	#calculating edge density in the landscape
    	paisagem[which(paisagem[]==0)] <- NA #non-forest areas are replaced by NA values
    	bordas <- boundaries(paisagem, type='inner') #all edges become 1
    	#plot(bordas)
    	n.pixels.borda <- length(which(bordas[]==1))
    	n.pixels.floresta <- length(which(!is.na(bordas[])))
    	densidade.borda[j] <- (n.pixels.borda/n.pixels.floresta)*100
    }
  }
  
  #creating 5 x 5 km rasters with number of patches and edge density values
  raster.numero.manchas <- cover2000.upscaled
  raster.numero.manchas[] <- numero.manchas
  #plot(raster.numero.manchas)
  
  raster.densidade.borda <- cover2000.upscaled
  raster.densidade.borda[] <- densidade.borda
  #plot(raster.numero.manchas)
  
  writeRaster(raster.numero.manchas, paste("NumberPatches2000",name4,".tif", sep=""),overwrite=TRUE) #saves new raster, which presents information of the number of patches in each landscape (5 x 5 km)
  
  writeRaster(raster.densidade.borda, paste("EdgeDensity2000",name4,".tif", sep=""),overwrite=TRUE) #saves new raster, which presents information on edge density in each landscape (5 x 5 km)
  
  removeTmpFiles(1) #removes original Hansen raster from temporary archives, releasing computing memory for the next raster processing 
  

}
