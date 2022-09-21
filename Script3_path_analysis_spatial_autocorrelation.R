rm(list=ls(all=TRUE))
library(nlme)
library(lme4)
library(MuMIn)
library(ncf)
library(raster)
library(gplots)
library(lavaan)
library(piecewiseSEM)
library(visreg)
library(scales)

#Global analysis
nome<-"ResultadosABRIL_Analise_Mundial10manchasUM_PAR_JANELA.txt"
resul_ana_mundi <- read.table(nome,header=T)

NAs<-which(is.na(resul_ana_mundi$temp_media_p1)|is.na(resul_ana_mundi$temp_media_p2)|is.na(resul_ana_mundi$n.patches_p1) |is.na(resul_ana_mundi$n.patches_p2)|is.na(resul_ana_mundi$edge.density_p1)|is.na(resul_ana_mundi$edge.density_p2)|is.na(resul_ana_mundi$altitude_p1)|is.na(resul_ana_mundi$altitude_p2)|is.na(resul_ana_mundi$temperatura_dia_p1)|is.na(resul_ana_mundi$temperatura_dia_p2)|is.na(resul_ana_mundi$temperatura_noite_p1)|is.na(resul_ana_mundi$temperatura_noite_p2)|is.na(resul_ana_mundi$albedo_p1)|is.na(resul_ana_mundi$albedo_p2)|is.na(resul_ana_mundi$evapo_p1)|is.na(resul_ana_mundi$evapo_p2))

resul_ana_mundi <- resul_ana_mundi[-NAs,]

for(i in 1:dim(resul_ana_mundi)[1]){
  a1 <- which(resul_ana_mundi$long_p1[]==resul_ana_mundi$long_p1[i])
  a1 <- a1[-which(a1==i)]
  a2 <- which(resul_ana_mundi$long_p2[a1]==resul_ana_mundi$long_p2[i])
  a2 <- a1[a2]
  a3 <- which(resul_ana_mundi$lati_p1[a2]==resul_ana_mundi$lati_p1[i])
  a3 <- a2[a3]
  a4 <- which(resul_ana_mundi$lati_p2[a3]==resul_ana_mundi$lati_p2[i])
  a4 <- a3[a4]
  
  if(sum(a4)>0){
    resul_ana_mundi <<- resul_ana_mundi[-a4,]
  }
}
dim(resul_ana_mundi)
write.table(resul_ana_mundi, nome)

#histogram with number of patches
hist(resul_ana_mundi$n.patches_p1)
hist(resul_ana_mundi$n.patches_p2)


#loop to organize data of each pair, always comparing "continue" (landscape less fragmented) vs "fragmented" (fragmented landscape) order
dados_organizados_am_t<- data.frame(npatches_c=rep(NA, nrow(resul_ana_mundi)), npatches_f=NA, forest_c=NA, forest_f=NA, temp_media_c=NA, temp_media_f=NA, altitude_c=NA, altitude_f=NA, latitude_c=NA, latitude_f=NA, temp_dia_c=NA, temp_dia_f=NA, temp_noite_c=NA, temp_noite_f=NA, albedo_c=NA, albedo_f=NA, evapo_c=NA, evapo_f=NA, longitude_c=NA, longitude_f=NA)

for(i in 1:nrow(resul_ana_mundi)){
  #i=1
  #n. patches
  npatches <- c(resul_ana_mundi$n.patches_p1[i], resul_ana_mundi$n.patches_p2[i])
  npatches.c <- which.min(npatches)
  npatches.f <- which.max(npatches)
  dados_organizados_am_t$npatches_c[i] <- npatches[npatches.c]
  dados_organizados_am_t$npatches_f[i] <- npatches[npatches.f]
  
  #forest
  forest <- c(resul_ana_mundi$forest_p1[i], resul_ana_mundi$forest_p2[i])
  dados_organizados_am_t$forest_c[i] <- forest[npatches.c]
  dados_organizados_am_t$forest_f[i] <- forest[npatches.f]
  
  #mean LST
  temp_media <- c(resul_ana_mundi$temp_media_p1[i], resul_ana_mundi$temp_media_p2[i])
  dados_organizados_am_t$temp_media_c[i] <- temp_media[npatches.c]
  dados_organizados_am_t$temp_media_f[i] <- temp_media[npatches.f]
  
  #altitude
  altitude <- c(resul_ana_mundi$altitude_p1[i], resul_ana_mundi$altitude_p2[i])
  dados_organizados_am_t$altitude_c[i] <- altitude[npatches.c]
  dados_organizados_am_t$altitude_f[i] <- altitude[npatches.f]
  
  #latitude - will be used for spatial autocorrelation analysis
  latitude <- c(resul_ana_mundi$lati_p1[i], resul_ana_mundi$lati_p2[i])
  dados_organizados_am_t$latitude_c[i] <- latitude[npatches.c]
  dados_organizados_am_t$latitude_f[i] <- latitude[npatches.f]
  
  #daytime LST
  temp_dia <- c(resul_ana_mundi$temperatura_dia_p1[i], resul_ana_mundi$temperatura_dia_p2[i])
  dados_organizados_am_t$temp_dia_c[i] <- temp_dia[npatches.c]
  dados_organizados_am_t$temp_dia_f[i] <- temp_dia[npatches.f]
  
  #nighttime LST
  temp_noite <- c(resul_ana_mundi$temperatura_noite_p1[i], resul_ana_mundi$temperatura_noite_p2[i])
  dados_organizados_am_t$temp_noite_c[i] <- temp_noite[npatches.c]
  dados_organizados_am_t$temp_noite_f[i] <- temp_noite[npatches.f]
  
  #albedo
  albedo <- c(resul_ana_mundi$albedo_p1[i], resul_ana_mundi$albedo_p2[i])
  dados_organizados_am_t$albedo_c[i] <- albedo[npatches.c]
  dados_organizados_am_t$albedo_f[i] <- albedo[npatches.f]
  
  #evapotranspiration
  evapo <- c(resul_ana_mundi$evapo_p1[i], resul_ana_mundi$evapo_p2[i])
  dados_organizados_am_t$evapo_c[i] <- evapo[npatches.c]
  dados_organizados_am_t$evapo_f[i] <- evapo[npatches.f]
  
  #longitude - also for autocorrelation analysis
  longitude <- c(resul_ana_mundi$long_p1[i], resul_ana_mundi$long_p2[i])
  dados_organizados_am_t$longitude_c[i] <- longitude[npatches.c]
  dados_organizados_am_t$longitude_f[i] <- longitude[npatches.f]
  
}

write.table(dados_organizados_am_t,"Dados_organizados_AM.txt") #write valid data

##SELECTING CLIMATIC REGIONS

dados_organizados_am<-read.table("Dados_organizados_AM.txt",header=T)
nrow(dados_organizados_am)#colNumber(n=18570)

#boreal region
#dados_boreal_1<-dados_organizados_am[dados_organizados_am$latitude_c>=50,]
#dados_boreal_2<-dados_organizados_am[dados_organizados_am$latitude_c<=-50,]
#dados_boreal<-rbind(dados_boreal_1,dados_boreal_2)
#write.table(dados_boreal,"Dados_boreal_AM.txt")

#tropical region
dados_tropical_1<-dados_organizados_am[dados_organizados_am$latitude_c<=20,]
write.table(dados_tropical_1,"Dados_tropical_1_AM.txt")
dados_tropical_1<-read.table("Dados_tropical_1_AM.txt",header=T) 
dados_tropical_2<-dados_tropical_1[dados_tropical_1$latitude_c>=-20,]
write.table(dados_tropical_2,"Dados_tropical_AM.txt")

#temperate region
#dados_temprd_1<-dados_organizados_am[dados_organizados_am$latitude_c>=20,]
#write.table(dados_temprd_1,"Dados_temprd_1_AM.txt")
#dados_temprd_1<-read.table("Dados_temprd_1_AM.txt",header=T)
#dados_temprd_2<-dados_temprd_1[dados_temprd_1$latitude_c<=50,]
#write.table(dados_temprd_2,"Dados_temprd_primeiro_AM.txt")
#dados_temprd_3<-dados_organizados_am[dados_organizados_am$latitude_c<=-20,]
#write.table(dados_temprd_3,"Dados_temprd_3_AM.txt")
#dados_temprd_3<-read.table("Dados_temprd_3_AM.txt",header=T)
#dados_temprd_4<-dados_temprd_3[dados_temprd_3$latitude_c>=-50,]
#write.table(dados_temprd_4,"Dados_temprd_segundo_AM.txt")
#temprd_1<-read.table("Dados_temprd_primeiro_AM.txt",header=T)
#temprd_2<-read.table("Dados_temprd_segundo_AM.txt",header=T)
#dados_temperado<-rbind(temprd_1,temprd_2)
#write.table(dados_temperado,"Dados_temperado_AM.txt")


##ANALYSIS
dados_organizados_am<-read.table("Dados_tropical_AM.txt",header=T)

diff_altitude <- dados_organizados_am$altitude_f-dados_organizados_am$altitude_c
linhas_validas <- which(abs(diff_altitude)<50) #pairs with an altitudinal difference less than 50m
dados_organizados_am<- dados_organizados_am[linhas_validas,]

diff_npatches <- dados_organizados_am$npatches_f-dados_organizados_am$npatches_c
diff_forest <- dados_organizados_am$forest_f-dados_organizados_am$forest_c
mean_forest <- rowMeans(dados_organizados_am[,3:4])
diff_temp_media <- dados_organizados_am$temp_media_f-dados_organizados_am$temp_media_c
diff_altitude <- dados_organizados_am$altitude_f-dados_organizados_am$altitude_c
mean_latitude <- rowMeans(dados_organizados_am[,9:10])
mean_longitude <- rowMeans(dados_organizados_am[,19:20])
diff_temp_dia <- dados_organizados_am$temp_dia_f-dados_organizados_am$temp_dia_c
diff_temp_noite <- dados_organizados_am$temp_noite_f-dados_organizados_am$temp_noite_c
diff_albedo <- dados_organizados_am$albedo_f-dados_organizados_am$albedo_c
diff_evapo <- dados_organizados_am$evapo_f-dados_organizados_am$evapo_c

hist(diff_evapo)
abline(v=0,col="red")

#a) mean LST
dados_path <- data.frame(n.patches=scale(diff_npatches), temp=scale(diff_temp_media), albedo=scale(diff_albedo), evapo=scale(diff_evapo), latitude=mean_latitude, longitude=mean_longitude) #scale is used for standardizing procedures


#samples 800 points, otherwise it is too much time spending
amostra <- sample(1:nrow(dados_path), 800, replace=F)
dados_path <- dados_path[amostra, ]

mycontrol = lmeControl(opt='optim') # to improve model convergence
#mycontrol = lmeControl(opt='optim', optimMethod="SANN")

#temperature sub-model - best autocorrelation structure
temp1 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol)
temp2 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
temp3 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
temp4 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
temp5 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
temp6 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#temp7 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))

model.sel(temp1, temp2, temp3, temp4, temp5, temp6)
#temp5 selected

#albedo sub-model - best autocorrelation structure
alb1 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol)
alb2 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
alb3 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
alb4 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
alb5 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
alb6 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#alb7 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))

model.sel(alb1, alb2, alb3, alb4, alb5, alb6)
#alb5 selected

#evapotranspiration sub-model - best autocorrelation structure
evapo1 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol)
evapo2 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
evapo3 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
evapo4 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
evapo5 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
evapo6 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#evapo7 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))

model.sel(evapo1, evapo2, evapo3, evapo4, evapo5, evapo6)
#evapo2 selected


#PATH MODEL

path_m = list( #selected autocorrelation structures are set for each selected sub-model
  temp <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude)),
  alb <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude)),
  evapo <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
)

sem.coefs(path_m, dados_path)

efeito.total.npatches <- -0.019 -0.092 -0.218 -0.002
 
desvio.npatches <- sd(diff_npatches)
desvio.temp <- sd(diff_temp_media)

efeito.em.celsius <- efeito.total.npatches*desvio.temp  #The standard deviation of the number of patches is 48.4, and the temperature is 0.41 °C. Therefore, an increase of 48.4 patches increases the temp by 0.01681°C (0.041*0.41).
efeito.em.celsius

#b) daytime LST
dados_path <- data.frame(n.patches=scale(diff_npatches), temp=scale(diff_temp_dia), albedo=scale(diff_albedo), evapo=scale(diff_evapo), latitude=mean_latitude, longitude=mean_longitude) 

amostra <- sample(1:nrow(dados_path), 800, replace=F)
dados_path <- dados_path[amostra, ]

mycontrol = lmeControl(opt='optim') # to improve model convergence
#mycontrol = lmeControl(opt='optim', optimMethod="SANN")

#temperature sub-model - best autocorrelation structure
temp1 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol)
temp2 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
temp3 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
temp4 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
temp5 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
temp6 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#temp7 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))

model.sel(temp1, temp2, temp3, temp4, temp5, temp6)
#temp5 selected

#albedo sub-model - best autocorrelation structure
alb1 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol)
alb2 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
alb3 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
alb4 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
alb5 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
alb6 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#alb7 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))

model.sel(alb1, alb2, alb3, alb4, alb5, alb6)
#alb5 selected

#evapotranspiration sub-model - best autocorrelation structure
evapo1 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol)
evapo2 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
evapo3 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
evapo4 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
evapo5 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
evapo6 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#evapo7 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))
#Symm nao roda

model.sel(evapo1, evapo2, evapo3, evapo4, evapo5, evapo6)
#evapo2 selected

path_m = list( 
  temp <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude)),
  alb <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude)),
  evapo <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
)

sem.coefs(path_m, dados_path)

efeito.total.npatches <- -0.028 -0.087 -0.222 -0.002
  
desvio.npatches <- sd(diff_npatches)
desvio.temp <- sd(diff_temp_dia)

efeito.em.celsius <- efeito.total.npatches*desvio.temp  
efeito.em.celsius


#c) nighttime LST
dados_path <- data.frame(n.patches=scale(diff_npatches), temp=scale(diff_temp_noite), albedo=scale(diff_albedo), evapo=scale(diff_evapo), latitude=mean_latitude, longitude=mean_longitude) 


amostra <- sample(1:nrow(dados_path), 800, replace=F)
dados_path <- dados_path[amostra, ]

mycontrol = lmeControl(opt='optim') # to improve model convergence
#mycontrol = lmeControl(opt='optim', optimMethod="SANN")

temp1 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol)
temp2 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
temp3 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
temp4 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
temp5 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
temp6 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#temp7 <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))
#Symm nao roda

model.sel(temp1, temp2, temp3, temp4, temp5, temp6)
#temp5 selected

alb1 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol)
alb2 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
alb3 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
alb4 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
alb5 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
alb6 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#alb7 <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))


model.sel(alb1, alb2, alb3, alb4, alb5, alb6)
#alb5 selected

evapo1 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol)
evapo2 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corRatio(form = ~ longitude+latitude))
evapo3 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corLin(form = ~ longitude+latitude))
evapo4 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corGaus(form = ~ longitude+latitude))
evapo5 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
evapo6 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corSpher(form = ~ longitude+latitude))
#evapo7 <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corSymm(form = ~ longitude+latitude))

model.sel(evapo1, evapo2, evapo3, evapo4, evapo5, evapo6)
#evapo2 selected


path_m = list( 
  temp <- gls(temp ~ albedo + evapo + n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude)),
  alb <- gls(albedo ~ n.patches, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude)),
  evapo <- gls(evapo ~ n.patches + albedo, data=dados_path, control=mycontrol, correlation = corExp(form = ~ longitude+latitude))
)

sem.coefs(path_m, dados_path)

efeito.total.npatches <- 0.010 -0.041 -0.066 -0.001   
  
desvio.npatches <- sd(diff_npatches)
desvio.temp <- sd(diff_temp_noite)

efeito.em.celsius <- efeito.total.npatches*desvio.temp  
efeito.em.celsius