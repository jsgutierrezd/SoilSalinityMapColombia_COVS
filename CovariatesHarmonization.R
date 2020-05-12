setwd("~/SoilSalinityMapColombia_COVS")

library(raster)
library(sp)
library(rgdal)
library(soilassessment)
library(magrittr)

##Country boundary
lim <- readOGR("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\LIMITES\\LIMITE_NAL_CONT_WGS84.shp")

##Covariates >> WorldGrids and DEM derivated maps
cov <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\WORLDGRIDS\\CovWorldGrids.tif")
names(cov) <- readRDS("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\WORLDGRIDS\\NamesCovWorldGrids.rds")
names(cov)


##Function to compute binary variables (dummy)
dummyRaster <- function(rast){
  rast <- as.factor(rast)
  result <- list()
  for(i in 1:length(levels(rast)[[1]][[1]])){
    result[[i]] <- rast == levels(rast)[[1]][[1]][i]
    names(result[[i]]) <- paste0(names(rast),
                                 levels(rast)[[1]][[1]][i])
  }
  return(stack(result))
}

###MATERIAL PARENTAL######
MATPAR <- readOGR(dsn="G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\SHP_IGAC\\MATERIAL_PARENTAL\\Material_Parental.shp")
MATPAR <- spTransform (MATPAR, CRS=projection(cov))
MATPAR$Clas_MP_char <- as.character(MATPAR$Clas_MP)
MATPAR$MP_RAST <- ifelse(MATPAR$Clas_MP_char=="Aeropuerto"|MATPAR$Clas_MP_char=="Arenal"|
                           MATPAR$Clas_MP_char=="Base Militar"|MATPAR$Clas_MP_char=="Basurero"|
                           MATPAR$Clas_MP_char=="CA"|MATPAR$Clas_MP_char=="Cantera"|
                           MATPAR$Clas_MP_char=="Represa"|MATPAR$Clas_MP_char=="EdificaciÃ³n"|
                           MATPAR$Clas_MP_char=="ZU",
                         NA,MATPAR$Clas_MP_char) %>% as.factor()
MATPAR_rast <- rasterize(MATPAR, cov, 'MP_RAST')
MATPAR_rast_res <- resample(MATPAR_rast,cov,method="bilinear")
values(MATPAR_rast_res) <- round(values(MATPAR_rast_res),0)
names(MATPAR_rast_res) <- 'PAR_MAT'
(mp_dummy <- dummyRaster(MATPAR_rast_res))
names(mp_dummy) <- levels(factor(MATPAR$MP_RAST))
cov <- stack(cov,MATPAR_rast_res, mp_dummy)
names(cov)


##Land cover
landcover <- readOGR(dsn="G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\SHP_IGAC\\COBERTURAS_V1.0_2010_2012\\Cobertura_RP.shp")
landcover <- spTransform (landcover, CRS=projection(cov))
landcover$LEYENDA3N_char <- as.character(landcover$LEYENDA3N)
landcover$landcover_RAST <- ifelse(landcover$LEYENDA3N_char=="1.1.1. Tejido urbano continuo"|landcover$LEYENDA3N_char=="1.1.2. Tejido urbano discontinuo"|
                                     landcover$LEYENDA3N_char=="1.2.1. Zonas industriales o comerciales"|landcover$LEYENDA3N_char=="1.2.2. Red vial, ferroviaria y terrenos asociados"|
                                     landcover$LEYENDA3N_char=="1.2.3. Zonas portuarias"|landcover$LEYENDA3N_char=="9.9. Nubes"|
                                     landcover$LEYENDA3N_char=="1.4.2. Instalaciones recreativas"|landcover$LEYENDA3N_char=="1.2.4. Aeropuertos",
                                   NA,landcover$LEYENDA3N_char) %>% as.factor()
landcover_rast <- rasterize(landcover, cov, 'landcover_RAST')
landcover_rast_res <- resample(landcover_rast,cov,method="bilinear")
values(landcover_rast_res) <- round(values(landcover_rast_res),0)
names(landcover_rast_res) <- 'LANDCOVER'
(lc_dummy <- dummyRaster(landcover_rast_res))
names(lc_dummy) <- levels(factor(landcover$landcover_RAST))
cov <- stack(cov,landcover_rast_res, lc_dummy)
names(cov)


##SOIL ORDER
order <- readOGR(dsn="G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\SHP_IGAC\\ORDENES_SUELOS\\Ordenes_Suelos.shp")
order <- spTransform (order, CRS=projection(cov))
order$Significad <- as.character(order$Significad)
data.frame(unique(order$Significad))
order$order_RAST <- ifelse(order$Significad=="Aeropuerto"|order$Significad=="Represa"|
                             order$Significad=="Tierra Relave Carbón"|order$Significad=="Arenal"|
                             order$Significad=="Base Militar"|order$Significad=="Basurero"|
                             order$Significad=="CA"|order$Significad=="Cantera"|
                             order$Significad=="Cuerpos de agua"|order$Significad=="EdificaciÃ³n.2"|
                             order$Significad=="Fosa Mina Carbón"|order$Significad=="Misceláneo Erosionado"|
                             order$Significad=="Misceláneo Rocoso"|order$Significad=="Nieves Perpetuas"|
                             order$Significad=="Saladares"|order$Significad=="Zonas urbanas"|
                             order$Significad=="Tierra Relave CarbÃ³n"|order$Significad=="EdificaciÃ³n"|
                             order$Significad=="Fosa Mina CarbÃ³n"|order$Significad=="MiscelÃ¡neo Erosionado"|
                             order$Significad=="MiscelÃ¡neo Rocoso",
                            NA,order$Significad) %>% as.factor()
order_rast <- rasterize(order, cov, 'order_RAST')
order_rast_res <- resample(order_rast,cov,method="bilinear")
values(order_rast_res) <- round(values(order_rast_res),0)
names(order_rast_res) <- 'soilorder'
(or_dummy <- dummyRaster(order_rast_res))
names(or_dummy) <- levels(factor(order$order_RAST))
cov <- stack(cov,order_rast_res, or_dummy)
names(cov)

####SOIL SALINITY-IDEAM
#tipo de salinidad
ideam <- readOGR(dsn="G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\SHP\\E_DS_Salinizacion_100K_2016_2017.shp")
ideam <- spTransform (ideam, CRS=projection(cov))
data.frame(unique(ideam$TIPO))
ideam$TIPO <- ideam$TIPO %>%  as.factor()
levels(ideam$TIPO) <- c("Anthropogenic","Mixed","Natural","Non_saline")
tipo_rast <- rasterize(ideam, cov, 'TIPO')
tipo_rast_res <- resample(tipo_rast,cov,method="bilinear")
values(tipo_rast_res) <- round(values(tipo_rast_res),0)
names(tipo_rast_res) <- 'sal_type'
(tip_dummy <- dummyRaster(tipo_rast_res))
names(tip_dummy) <- levels(factor(ideam$TIPO))
cov <- stack(cov,tipo_rast_res, tip_dummy)
names(cov)

#grado de salinidad
data.frame(unique(ideam$GRADO))
ideam$GRADO <- ideam$GRADO %>%  as.factor()
levels(ideam$GRADO) <- c("Ligero","Moderado","Muy_ligero","Muy_salino","No_salino","Salino")
grado_rast <- rasterize(ideam, cov, 'GRADO')
grado_rast_res <- resample(grado_rast,cov,method="bilinear")
values(grado_rast_res) <- round(values(grado_rast_res),0)
names(grado_rast_res) <- 'sal_degree'
(gr_dummy <- dummyRaster(grado_rast_res))
names(gr_dummy) <- levels(factor(ideam$GRADO))
#cov <- stack(cov,grado_rast_res, gr_dummy)
#names(cov)

#clase de salinidad
data.frame(unique(ideam$clase))
ideam$CLASE <- ideam$CLASE %>%  as.factor()
ideam$clase <- ifelse(ideam$CLASE=="N/A",NA,ideam$CLASE)%>%as.factor()
levels(ideam$clase) <- c("Al","AlNa","Ca","Mg","MgCa","Na",
                         "NaCa","NaMg","NaMgSA","NaSA","NS","SA","SAMg","Sl","SlCa",
                         "SlNa","SlNaCa","SlNaMg","SlNaMgCa","SlSA")
levels(ideam$clase)
clas_rast <- rasterize(ideam, cov, 'clase')
clas_rast_res <- resample(clas_rast,cov,method="bilinear")
values(clas_rast_res) <- round(values(clas_rast_res),0)
names(clas_rast_res) <- 'sal_class'
(cl_dummy <- dummyRaster(clas_rast_res))
names(cl_dummy) <- levels(factor(ideam$clase))
cov <- stack(cov,clas_rast_res, cl_dummy)
names(cov)

##Mosaics Landsat8 - Sentinel2 - MODISMOD09A1.006
ST2 <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\IMAGENES\\ST2.tif")
LS8 <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\IMAGENES\\LS8_2015-2018.tif")
MODIS <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\IMAGENES\\MOD.tif")

names(ST2) <- c("B1", "BLUE","GREEN","RED","B5","B6",
                "B7","NIR","B9","B10","SWIR1",
                "SWIR2","B13","B14","B15","B16")
names(LS8)<- c("B1", "BLUE","GREEN","RED","NIR","SWIR1",
               "SWIR2","B8","B9","B10","B11","B12")
names(MODIS)<- c("RED", "NIR","BLUE","GREEN","B5","SWIR1",
                 "SWIR2","B8","B9","B10","B11",
                 "B12","B13")


##Mosaic resampling
ST2 <- resample(ST2,cov,method="bilinear")
LS8 <- resample(LS8,cov,method="bilinear")
MODIS <- resample(MODIS,cov,method="bilinear")


##Derive remote sensing indexes
indexes <- c("NSI","SI1","SI2","SI3","SI4","SI5","SI6","SAVI","VSSI","NDSI","SR","BI","NDVI")

#Sentinel2
st2fin <- raster()
for(i in 1:length(indexes)){
  temp <- imageIndices(ST2$BLUE,ST2$GREEN,ST2$RED,ST2$NIR,ST2$SWIR1,ST2$SWIR2,indexes[i])
  st2fin <- stack(st2fin,temp)
}
st2fin
names(st2fin) <- indexes
st2fin$CRSI <- sqrt((ST2$NIR * ST2$RED - ST2$BLUE * ST2$GREEN)/
                      (ST2$NIR * ST2$GREEN + ST2$BLUE * ST2$GREEN))

#Landsat8
ls8fin <- stack()
for(i in 1:length(indexes)){
  temp <- imageIndices(LS8$BLUE,LS8$GREEN,LS8$RED,LS8$NIR,LS8$SWIR1,LS8$SWIR2,indexes[i])
  ls8fin <- stack(ls8fin,temp)
}
ls8fin
names(ls8fin) <- indexes
ls8fin$CRSI <- sqrt((LS8$NIR * LS8$RED - LS8$BLUE * LS8$GREEN)/
                      (LS8$NIR * LS8$GREEN + LS8$BLUE * LS8$GREEN))

class(ls8fin)

#ModisMOD09A1.006
modisfin <- stack()
for(i in 1:length(indexes)){
  temp <- imageIndices(MODIS$BLUE,MODIS$GREEN,MODIS$RED,MODIS$NIR,MODIS$SWIR1,MODIS$SWIR2,indexes[i])
  modisfin <- stack(modisfin,temp)
}
modisfin
names(modisfin) <- indexes
modisfin$CRSI <- sqrt((MODIS$NIR * MODIS$RED - MODIS$BLUE * MODIS$GREEN)/
                        (MODIS$NIR * MODIS$GREEN + MODIS$BLUE * MODIS$GREEN))

###PCA

library(corrplot)
library(factoextra)
ls8fin1 <- as(ls8fin, "SpatialGridDataFrame")
ls8fin1 <- as(ls8fin1, "data.frame")
summary(ls8fin1)
ls8fin1 <- na.omit(ls8fin1)
M <- cor(ls8fin1[,-c(1,15,16)])

#Correlation plot
names(ls8fin1)
x11()
corrplot::corrplot(M, method="number",number.cex = 0.8)
pca<-prcomp(ls8fin1[,-c(1,15,16)], scale=TRUE) 
summary(pca)
(corvar <- pca$rotation %*% diag(pca$sdev))

#Biplot
x11()
plot(-1:1, -1:1, type='n', asp=1, xlab='PC1', ylab='PC2')
abline(h=0, v=0, lty=2, col=8)
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)
arrows(0, 0, corvar[,1], corvar[,2], length=.1)
text(corvar[,1], corvar[,3], colnames(ls8fin1[,-c(1,15,16)]), pos=4, offset=.6, col=2, font=2)

biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

#Scree plot
x11()
fviz_eig(pca)


Pred.pcs<-predict(pca,ls8fin1[])
dim(Pred.pcs)
names(ls8fin1)
ls8fin1$PCA1=Pred.pcs[,1] 
ls8fin1$PCA2=Pred.pcs[,2]
ls8fin1$PCA3=Pred.pcs[,3] 
ls8fin1$PCA4=Pred.pcs[,4] 

gridded(ls8fin1) <- ~s1+s2
class(ls8fin1)
proj4string(ls8fin1) <- CRS("+init=epsg:4326")
ls8fin1 <- stack(ls8fin1)
ls8fin1 <- resample(ls8fin1,cov,method="bilinear")
ls8fin1 <- stack(ls8fin1)
names(ls8fin1)
# x11()
# plot(ls8fin1[[8]]) %>% 

##Covariates WorldGrids - DEM - IGAC + REMOTE SENSING INDEXES
names(cov)
cov <- stack(cov,ls8fin1)
names(cov)

cov <- as(cov, "SpatialGridDataFrame")
cov <- stack(cov)
writeRaster(cov,"COVS_30042020.tif")
cov1 <- data.frame(cov@data, coordinates(cov))
# cov <- as(cov, "data.frame")
class(cov1)
dir()
save(cov1, file="covariateStack.rda")
load("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\covariateStack.rda")
names(cov1)
data.frame(names(cov1))

cov <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\CovSSMAP.tif")
names(cov) <- readRDS("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\NamesCovSSMAP.rds")

tipo <- raster("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\tipo.tif")
tipo_d <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\tipo_dummies.tif")
names(tipo_d) <- c("t_antrop","t_mixto","t_natural","t_nosalino")
saveRDS(names(tipo_d), "G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\namestipo_dummies.rds")

grado <- raster("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\grado.tif")
grado_d <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\grado_dummies.tif")
names(grado_d) <- c("g_ligero","g_mod","g_muylig","g_muysal","g_nosal","g_salino")
saveRDS(names(grado_d), "G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\namesgrado_dummies.rds")

clase <- raster("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\clase.tif")
clase_d <- stack("G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\clase_dummies.tif")
names(clase_d) <- c("c_NS","c_Al","c_Na","c_Ca","c_Mg",
                    "c_NaMg","c_SlNa","c_SlNaMg","c_NaMgSA","c_SA",
                    "c_SAMg","c_Sl","c_SlCa","c_SlNaMgCa","c_MgCa",
                    "c_SlNaCa","c_AlNa","c_SlSA","c_NaSA","c_NaCa")
saveRDS(names(clase_d), "G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\IDEAM\\namesclase_dummies.rds")


cov <- stack(cov,tipo,tipo_d,
             grado,grado_d,
             clase,clase_d)
cov
names(cov)
names(cov[[190]]) <- "tipo_salinidad"
names(cov[[195]]) <- "grado_salinidad"
names(cov[[202]]) <- "clase_salinidad"

writeRaster(cov,"G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\CovSSMAP_12052020.tif")
saveRDS(names(cov),"G:\\My Drive\\IGAC_2020\\SALINIDAD\\INSUMOS\\COVARIABLES\\COV_SSMAP\\NamesCovSSMAP_12052020.rds")
