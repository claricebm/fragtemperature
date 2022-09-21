#####INCLUIR ALTITUDE

#Este é o script com a correção da linha de comando de como se calcular a temperatura média

rm(list=ls(all=TRUE)) # limpar arquivos anteriores - libera memoria
library("raster")
library("rgdal") # pacote para lidar com shapefiles
library("reshape") # pacote para formatar dataframes

#antes de rodar a análise, fazer o mosaico de cada um dos rasters abaixo no ArcGis
#ou seja, "colar" os distintos rasters da pasta "hansen_rasters_2000" para formar a imagem final desejada, que será "cropada" pela mata atlântica

#abre rasters
forest2000 <- raster("ForestCover2000_00N_000E - Copia (2).tif") #mudar nome para o raster de cobertura florestal correto
n.patches <- raster("NumberPatches2000_00N_000E - Copia (2).tif") #mudar nome
edge.density <- raster("EdgeDensity2000_00N_000E - Copia (2).tif") #mudar nome
temperatura_dia <- raster("Temperatura_Media_dia2001_hdf.tif") #mudar nome
temperatura_noite <- raster("Temperatura_Media_noite2001_hdf.tif")
temp_media <- raster("temp_media.tif")
albedo <- raster("Albedo_Media_GLASS_2001_hdf.tif")
evapo <- raster("Evapotranspiracao_Media_2001_005.tif")
altitude<- raster("alt_005.tif") 

#passos para criar o raster de altitude com 0.05
#altitude<- raster("DEM_world_Res.tif") 
#altitude<-resample(altitude,clima,method="bilinear") 
#writeRaster(altitude, "altitude_resampled.tif")

#passos para calcular a temperatura média
#temp_media <- stack(temperatura_dia,temperatura_noite)#empilha os rasters
#temp_media <- calc(temp_media,fun=mean,na.rm=FALSE)#na.rm remove os NAs na média  
#writeRaster(temp_media,"temp_media.tif") 
#plot(temp_media)
#plot(temperatura_dia)
#plot(temperatura_noite)

#plot(forest2000)
#plot(n.patches)
#plot(edge.density)
#plot(clima)
#plot(altitude)


#mata_atlantica <- readOGR(".", "bioma")
#plot(mata_atlantica)


#padronizar sistema de coordenadas
crs(temp_media) <- crs(forest2000)
crs(temperatura_dia) <- crs(forest2000)
crs(temperatura_noite) <- crs(forest2000)
crs(albedo) <- crs(forest2000)
crs(evapo) <- crs(forest2000)
crs(altitude)<- crs(temp_media)

#crop altitude e clima pela floresta
temp_media<- crop(temp_media,extent(forest2000))
temperatura_dia<- crop(temperatura_dia,extent(forest2000))
temperatura_noite<- crop(temperatura_noite,extent(forest2000))
albedo<- crop(albedo,extent(forest2000))
evapo<- crop(evapo,extent(forest2000))
altitude<- crop(altitude,extent(forest2000))


# crop todos pela mata atlantica
#forest2000_2 <- mask(crop(forest2000, extent(mata_atlantica)), mata_atlantica)
#plot(forest2000_2)
#n.patches_2 <- mask(crop(n.patches, extent(mata_atlantica)), mata_atlantica)
#plot(n.patches_2)
#edge.density_2 <- mask(crop(edge.density, extent(mata_atlantica)), mata_atlantica)
#plot(edge.density_2)
#temp_media_2 <- mask(crop(temp_media, extent(mata_atlantica)), mata_atlantica)
#plot(temp_media_2)
#temperatura_dia_2 <- mask(crop(temperatura_dia, extent(mata_atlantica)), mata_atlantica)
#plot(temperatura_dia_2)
#temperatura_noite_2 <- mask(crop(temperatura_noite, extent(mata_atlantica)), mata_atlantica)
#plot(temperatura_noite_2)
#albedo_2 <- mask(crop(albedo, extent(mata_atlantica)), mata_atlantica)
#plot(albedo_2)
#evapo_2 <- mask(crop(evapo, extent(mata_atlantica)), mata_atlantica)
#plot(evapo_2)
#altitude_2<- mask(crop(altitude, extent(mata_atlantica)), mata_atlantica)
#plot(altitude_2)

#plots exploracao
#plot(forest2000_2[],albedo_2[])

# NAs - remover
#NA_Cells <- which(is.na(forest2000_2[]) |is.na(clima_2[])) # se tiver NA no raster de floresta ou no raster climatico, vira NA nos dois rasters
#forest2000_2[NA_Cells] <- NA
#plot(forest2000_2)
#plot(clima_2)

# configuração das janelas
a <- extent(forest2000) #extensao total do raster

total_longitude <- a[2] - a[1]
total_latitude <- a[4] - a[3]

lado_long <- 0.45 # tamanho da janela: 9x5. 1 pixel (=paisagem) = 0.05; 9 pixels = 0.45
lado_lat<- 0.25 # 5 pixels = 0.25



n.janelas.longitude <- floor((total_longitude-0.2)/(lado_long-0.2)) #permite sobreposicao horizontal de 4 pixels entre janelas vizinhas
n.janelas.latitude <- floor((total_latitude-0.1)/(lado_lat-0.1)) #permite sobreposicao vertical de 2 pixels entre janelas vizinhas

janelas <- data.frame(janela=c(1:(n.janelas.longitude*n.janelas.latitude)), lat=rep(c(1:n.janelas.latitude), each=n.janelas.longitude), long=rep(c(1:n.janelas.longitude), times=n.janelas.latitude)) # tabela com posicao das janelas - será usada dentro do loop
#head(janelas)


# dataframe para guardar resultados do loop
#dataframe com uma única linha, com NAs; para cada par na Mata Atlântica que tenha valores válidos, será inserida uma nova linha (com valores)
results <- data.frame(forest_p1=NA, forest_p2=NA, long_p1=NA, lati_p1=NA, long_p2=NA, lati_p2=NA, temp_media_p1=NA, temp_media_p2=NA, n.patches_p1=NA, n.patches_p2=NA, edge.density_p1=NA, edge.density_p2=NA, altitude_p1=NA, altitude_p2=NA, temperatura_dia_p1=NA, temperatura_dia_p2=NA, temperatura_noite_p1=NA, temperatura_noite_p2=NA, albedo_p1=NA, albedo_p2=NA, evapo_p1=NA, evapo_p2=NA) 

tempo1<-Sys.time()

valores <- seq(1000,1341148, 1000)

for (i in 1:(n.janelas.longitude*n.janelas.latitude)){
  #i=1
  
  lat <- janelas[i,2] # posicao latitudinal da janela
  long <- janelas[i,3] # posicao longitudinal da janela
  
  extensao.janela <- extent(a[1]+((long-1)*(lado_long-0.2)), (a[1]+((long-1)*(lado_long-0.2))+lado_long), a[4]-lado_lat-((lat-1)*(lado_lat-0.1)), (a[4]-lado_lat-((lat-1)*(lado_lat-0.1))+lado_lat))
  
  forest2000_3 <- crop(forest2000, extensao.janela) #corta raster de floresta pra deixar so a area da janela
  #forest2000_3 <- crop(forest2000_2, extent(c(-52.45,-52.00, -25.25, -25.00)))
  #plot(forest2000_3)
  #forest2000_3[] <- c(21:65)
  
  
  if (sum(which(!is.na(forest2000_3[])))>0){ # se tiver pelo menos 1 paisagem com alguma cobertura florestal (maior que zero), roda o loop
    diferencas <- melt(as.matrix(dist(forest2000_3[])), varnames = c("p1", "p2")) # calcula a diferenca de cobertura entre cada par de paisagens
    #head(diferencas)
    diferencas <- diferencas[diferencas$p1 > diferencas$p2,] # tira pares repetidos (já que dif absoluta de paisagem 1 para paisagem 2 é igual a dif de pais 2 para 1)
    colnames(diferencas)[3] <- "forest_dif" # muda nome da coluna
    #head(diferencas) # se quiser visualizar começo da tabela
    
    n.patches_3 <- crop(n.patches, extensao.janela)
    #n.patches_3[1] <- 8
    #n.patches_3[2] <- 150
    #n.patches_3[3] <- 130                                                                                             
    diferencas$porc_p1 <- forest2000_3[diferencas$p1]
    diferencas$porc_p2 <- forest2000_3[diferencas$p2]
    diferencas$npatch_p1 <- n.patches_3[diferencas$p1]
    diferencas$npatch_p2 <- n.patches_3[diferencas$p2]
    diferencas$diff_npatches <- abs(diferencas$npatch_p2 - diferencas$npatch_p1)
    
    # quais pares têm diferença de cobertura menor que 5%, cobertura intermediária entre 0% e 100% e um dos dois pixels têm <=10 manchas e um dos dois pixels têm >10 manchas
    condicao1 <- abs(diferencas$forest_dif)<5
    condicao2 <- diferencas$porc_p1 > 0 & diferencas$porc_p1 < 100
    condicao3 <- diferencas$npatch_p1 <= 10 | diferencas$npatch_p2 <= 10
    condicao4 <- diferencas$npatch_p1 > 10 | diferencas$npatch_p2 > 10
    
    pares.validos <- which(condicao1 & condicao2 & condicao3  & condicao4)
    
    
    if(length(pares.validos)>0) {#se tiver ao menos 1 par valido, roda o loop
      
      temp_media_3 <- crop(temp_media, extensao.janela)
      temperatura_dia_3 <- crop(temperatura_dia, extensao.janela)
      temperatura_noite_3 <- crop(temperatura_noite, extensao.janela)
      albedo_3 <- crop(albedo, extensao.janela)
      evapo_3 <- crop(evapo, extensao.janela)
      edge.density_3 <- crop(edge.density, extensao.janela)
      altitude_3<- crop(altitude, extensao.janela) 
      #plot(temp_media_3)
      
      resultados.pares <- diferencas[pares.validos,] # faz um dataframe menor, apenas com os pares, pra guardar nele os dados seguintes
      #head(resultados.pares)
      
      #seleciona so 1 par por janela - aquele com menor dif
      menor.dif <- which.max(resultados.pares$diff_npatches) # qual o par com menor diferenca de cobertura
      resultados.pares <- resultados.pares[menor.dif,] # deixa so a linha (par) com menor dif
      #resultados.pares
      
      resultados.pares$diff_npatches <- NULL
      
      # forest cover
      resultados.pares$forest_p1 <- forest2000_3[resultados.pares$p1] # adiciona nova coluna no data.frame com cobertura florestal da paisagem 1
      #head(resultados.pares)
      resultados.pares$forest_p2 <- forest2000_3[resultados.pares$p2]
      
      
      # coordenadas centrais das paisagens
      resultados.pares$long_p1 <- xyFromCell(forest2000_3, resultados.pares$p1)[,1]
      resultados.pares$lati_p1 <- xyFromCell(forest2000_3, resultados.pares$p1)[,2]
      resultados.pares$long_p2 <- xyFromCell(forest2000_3, resultados.pares$p2)[,1]
      resultados.pares$lati_p2 <- xyFromCell(forest2000_3, resultados.pares$p2)[,2]
      #head(resultados.pares)
      
      # temp_media
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
      
      #temperatura_dia
      resultados.pares$temperatura_dia_p1 <- temperatura_dia_3[resultados.pares$p1] 
      resultados.pares$temperatura_dia_p2 <- temperatura_dia_3[resultados.pares$p2] 
      
      #temperatura_noite
      resultados.pares$temperatura_noite_p1 <- temperatura_noite_3[resultados.pares$p1] 
      resultados.pares$temperatura_noite_p2 <- temperatura_noite_3[resultados.pares$p2]
      
      #albedo
      resultados.pares$albedo_p1 <- albedo_3[resultados.pares$p1] 
      resultados.pares$albedo_p2 <- albedo_3[resultados.pares$p2]
      
      #evapo
      resultados.pares$evapo_p1 <- evapo_3[resultados.pares$p1] 
      resultados.pares$evapo_p2 <- evapo_3[resultados.pares$p2]
      #head(resultados.pares)
      
      resultados.pares$p1 <- NULL # tira as colunas p1, p2 e forest_diff (nao sao mais necessarias)
      resultados.pares$p2 <- NULL
      resultados.pares$forest_dif <- NULL
      resultados.pares$porc_p1 <- NULL
      resultados.pares$porc_p2 <- NULL
      resultados.pares$npatch_p1 <- NULL
      resultados.pares$npatch_p2 <- NULL
      #head(resultados.pares)
      #head(results)
      
      results <- rbind(results, resultados.pares) #junta resultados da janela na tabelona geral
      
    }
  }
  
  
  if(i %in% valores){
    write.table(results, paste("ResultadosABRIL_Parciais10manchasUM_PAR_JANELA", i, ".txt", sep = "")) #salva resultados com o nome que quiser
  }
  
}

results <- results[-1,]#obs:deletar 1ª linha

write.table(results, "ResultadosABRIL_Analise_Mundial10manchasUM_PAR_JANELA.txt") #salva resultados com o nome que quiser

tempo2<-Sys.time()
tempo2-tempo1

