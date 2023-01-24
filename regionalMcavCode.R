setwd("/Users/student/Documents/GitHub/regionalMcav/")
if (!require("pacman")) install.packages("pacman")
pacman::p_load("adegenet", "dendextend", "gdata", "ggdendro", "hierfstat", "Imap", "kableExtra", 
               "paletteer", "patchwork", "officer", "poppr", "RColorBrewer", "reshape2", "StAMPP", 
               "tidyverse", "vcfR", "vegan", "WGCNA", "ggnewscale", "rnaturalearth", "sf", "ggspatial", 
               "cowplot", "dplyr", "grDevices", "sdmpredictors", "leaflet", "adespatial",
               "codep", "adegraphics", "ape", "car", "psych", "raster", "rgdal", "Hmisc", 
               "pairwiseAdonis", "pcadapt", "ggmap", "TeachingDemos", "LaplacesDemon")

pacman::p_load_gh("eliocamp/ggnewscale", "ropensci/rnaturalearthhires", "moldach/vapoRwave", "coolbutuseless/emphatic")
#Summary Sample Table###########################################
meta=read.csv("regionalInds2PopsNoClones.csv")
sampleTableRegion= meta %>%
  group_by(region, depthZone) %>%
  summarise(n = n(), depthMin= min(depthM), depthMax=max(depthM), meanDepth=mean(depthM))

sampleTableRegion=data.frame(sampleTableRegion)

sampleTableSite= meta %>%
  group_by(region, site, depthZone, lat, lon) %>%
  summarise(n = n()) 

sampleTableSite=data.frame(sampleTableSite)
#################################################################
#Examining clonal groups
bams=read.table("bams")[,1] # list of bam files
goods=c(1:length(bams))
ma = as.matrix(read.table("regionalMcavClones.ibsMat"))
ma=ma[goods,goods]
dimnames(ma)=list(bams[goods],bams[goods])
hc=hclust(as.dist(ma),"ave")
plot(hc,cex=0.4) # with clones
#################################################################
bamsNoClones=read.table("mcavBamsNoClones")[,1] # list of bam files
goodsNoClones=c(1:length(bamsNoClones))
maNoClones = as.matrix(read.table("regionalMcavNoClones.ibsMat"))
maNoClones=maNoClones[goodsNoClones,goodsNoClones]
dimnames(maNoClones)=list(bamsNoClones[goodsNoClones],bamsNoClones[goodsNoClones])
hcNoClones=hclust(as.dist(maNoClones),"ave")
plot(hcNoClones)
#Map################################################################
colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
sites = read.csv("regionalInds2popsNoClones.csv", header = TRUE)
sites$depthZone=as.factor(sites$depthZone)
sites$depthZone = factor(sites$depthZone, levels = levels(sites$depthZone)[c(2,1)])
sites$region = factor(sites$region, levels = levels(sites$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])
reefs = sites %>% group_by(region) %>% dplyr::summarize(lat = mean(lat), lon = mean(lon))
gomCountries = st_as_sf(ne_countries(country = c("United States of America", "Cuba", "Mexico", "The Bahamas", "Belize", "Guatemala", "Cayman Islands"), scale = "Large"))
regionMap = ggplot() +
  geom_sf(data = gomCountries, fill = "white", size = 0.25) +
  #geom_point(data = subset(sites, subset = sites$region %in% c("CUBA", "SEFL")), aes(x = lon, y = lat, shape = depthZone, fill=region), size = 3) +
  #geom_point(data = subset(reefs, subset = reefs$region %in% c("BLZ", "DRT", "FK", "NWGOM", "SGOM", "PRG")), aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
  #geom_point(data = reefs, aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
  geom_point(data = sites, aes(x = lon, y = lat, shape = depthZone, fill=region), size = 3) +
  scale_fill_manual(values = colPal, name = "Site")+
  scale_shape_manual(values = c(24,25), name = "Depth Zone") +
  coord_sf(xlim = c(-99, -75), ylim = c(16, 30)) +
  scale_x_continuous(breaks = c(seq(-99, -75, by = 2))) +
  scale_y_continuous(breaks = c(seq(16, 30, by = 2))) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  guides(fill = guide_legend(override.aes = list(shape = 22, color = NA, size = 4), order = 1), shape = guide_legend(override.aes = list(size = 3), order = 2)) +
  #guides(fill = guide_legend(override.aes = list(shape = 21, size = 4), order = 1))+ #,
  #shape = guide_legend(order=2),
  #color = guide_legend(title=NULL, order = 3))+ #override.aes = list(fill=NA),
  theme_bw() +
  theme(plot.title = element_text(size = 9) ,
        panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(color="black"))

regionMap

ggsave("regionMapSites.png", plot = regionMap, width = 30, height = 20, units = "cm", dpi = 300)
# 
# inset = ggplot() +
#   geom_segment(aes(x = -99, xend = -75, y = 16, yend = 30), color = "gray92", size = .55) +
#   geom_sf(data = gomCountries, fill = "white", size = 0.25) +
#   geom_point(data = sites, aes(x = lon, y = lat, fill = region, shape=depthZone), size = 3) +
#   scale_fill_manual(values = colPal, name = "Region") +
#   scale_shape_manual(values = c(24,25), name = "Depth Zone") +
#   #geom_jitter()+
#   #geom_sf(data = gom5.100Bathy, color = "gray") +
#   #guides(fill = guide_legend(override.aes = list(shape = 21, color = NA, size = 4))) +
#   theme_bw() +
#   theme(legend.title = element_text(size = 9, face = "bold"),
#         axis.title = element_blank(),
#         panel.background = element_rect(fill = "aliceblue"),
#         legend.text = element_text(size = 9),
#         legend.position = "none",
#         legend.direction = "vertical",
#         legend.box = "vertical",
#         panel.grid=element_blank(),
#         rect = element_blank())
# 
# blz = inset+
#   ggtitle("Belize") +
#   annotation_scale(location = "bl") +
#   coord_sf(xlim = c(-88.1, -87.75), ylim = c(16.73, 16.85)) +
#   scale_x_continuous(breaks = c(seq(-88.1, -87.75, by = .01))) +
#   scale_y_continuous(breaks = c(seq(16.73, 16.85, by = .01)))+
#   theme(legend.position = "none",
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.background=element_blank(),
#         panel.border = element_rect(size = 1))
# 
# blz
# 
# drt = inset+
#   ggtitle("Dry Tortugas") +
#   annotation_scale(location = "bl") +
#   coord_sf(xlim = c(-83.15, -83.05), ylim = c(24.47, 24.69)) +
#   scale_x_continuous(breaks = c(seq(-83.15, -83.05, by = .01))) +
#   scale_y_continuous(breaks = c(seq(4.47, 24.69, by = .01)))+
#   theme(legend.position = "none",
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.background=element_blank(),
#         panel.border = element_rect(size = 1))
# 
# drt
# 
# fk = inset+
#   ggtitle("Florida Keys") +
#   annotation_scale(location = "bl") +
#   coord_sf(xlim = c(-83.15, -83.05), ylim = c(24.47, 24.69)) +
#   scale_x_continuous(breaks = c(seq(-83.15, -83.05, by = .01))) +
#   scale_y_continuous(breaks = c(seq(4.47, 24.69, by = .01)))+
#   theme(legend.position = "none",
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.background=element_blank(),
#         panel.border = element_rect(size = 1))
# 
# fk

ggsave("./figures/regionalMap.tiff", plot = regionMap, width = 25, height = 20, units = "cm", dpi = 600)

#regionMap with Bounding Boxes
colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
sites = read.csv("regionalInds2popsNoClones.csv", header = TRUE)
#sites$depthZone=as.factor(sites$depthZone)
#sites$depthZone = factor(sites$depthZone, levels = levels(sites$depthZone)[c(2,1)])
#reefs = sites %>% group_by(region) %>% dplyr::summarize(lat = mean(lat), lon = mean(lon))
gomCountries = st_as_sf(ne_countries(country = c("United States of America", "Cuba", "Mexico", "The Bahamas", "Belize", "Guatemala", "Cayman Islands"), scale = "Large"))

bb= sites %>%
  group_by(region) %>%
  summarise(latMin=min(lat), latMax=max(lat), lonMin=min(lon), lonMax=max(lon))
bb$region = factor(bb$region, levels = levels(bb$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])

regionMapBB = ggplot() +
  geom_sf(data = gomCountries, fill = "white", size = 0.25) +
  #geom_point(data = subset(sites, subset = sites$region %in% c("CUBA", "SEFL")), aes(x = lon, y = lat, shape = depthZone, fill=region), size = 3) +
  #geom_point(data = subset(reefs, subset = reefs$region %in% c("BLZ", "DRT", "FK", "NWGOM", "SGOM", "PRG")), aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
  #geom_point(data = reefs, aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
  #geom_point(data = sites, aes(x = lon, y = lat, shape = depthZone, fill=region), size = 3) +
  geom_rect(data=bb, aes(xmin =  lonMin, xmax = lonMax, ymin = latMin, ymax = latMax, fill=region), alpha=0.9)+
  scale_fill_manual(values = colPal, name = "Site")+
  #scale_shape_manual(values = c(24,25), name = "Depth Zone") +
  coord_sf(xlim = c(-99, -75), ylim = c(16, 30)) +
  scale_x_continuous(breaks = c(seq(-99, -75, by = 2))) +
  scale_y_continuous(breaks = c(seq(16, 30, by = 2))) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
  guides(fill = guide_legend(override.aes = list(shape = 22, color = NA, size = 4), order = 1), shape = guide_legend(override.aes = list(size = 3), order = 2)) +
  #guides(fill = guide_legend(override.aes = list(shape = 21, size = 4), order = 1))+ #,
  #shape = guide_legend(order=2),
  #color = guide_legend(title=NULL, order = 3))+ #override.aes = list(fill=NA),
  theme_bw() +
  theme(plot.title = element_text(size = 9) ,
        panel.background = element_rect(fill = "aliceblue"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        legend.position = "right",
        axis.text = element_text(color="black"))

regionMapBB

ggsave("regionMapBB.png", plot = regionMapBB, width = 30, height = 20, units = "cm", dpi = 300)

#Heterozygosity, Inbreeding, and Relatedness###############################################################################################
meta=read.csv("regionalInds2PopsNoClones.csv")
hetSnps=read.table("hetSnps")
hetSnps=dplyr::select(hetSnps, -V1) 
colnames(hetSnps)[1] <- "hetSnps"
popStats=bind_cols(meta, hetSnps)

mcavBreed0 = read.delim("newres")
mcavBreed2 = mcavBreed0 %>% group_by(a) %>% dplyr::select("inbreed" = Fa)
mcavBreed3 = mcavBreed0 %>% group_by(b) %>% dplyr::select("inbreed" = Fb)
mcavBreed = bind_rows(mcavBreed2, mcavBreed3) %>% group_by(a) %>% dplyr::summarize("inbreed" = mean(inbreed))%>% dplyr::select(-a)
#popStats=popStats%>% left_join(mcavBreed, by = "a")
popStats=bind_cols(popStats, mcavBreed)
popStats$depthZone = factor(popStats$depthZone, levels(popStats$depthZone)[c(2,1)])
popStats$region = factor(popStats$region, levels = levels(popStats$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])

hetSnpAnova <- aov(hetSnps ~ region * depthZone, data = popStats)
summary(hetSnpAnova)
hetTukey=TukeyHSD(hetSnpAnova, which = "region")

inbreedAnova <- aov(inbreed ~ region * depthZone, data = popStats)
summary(inbreedAnova)
inbreedTukey=TukeyHSD(inbreedAnova, which = "region")

meta$a=c(0:751)
mcavRelate = read.delim("newres")
mcavRelate2 = mcavRelate %>% group_by(a, b) %>% dplyr::select("relate" = rab)
mcavRelate2 = mcavRelate2 %>% left_join(meta, by = "a") %>% left_join(meta, by = c("b" = "a"), suffix = c(".a", ".b")) 
#mcavRelate2$popDepth.a = paste(mcavRelate2$Region.a, mcavRelate2$Depth.a, sep = " ")
#mcavRelate2$popDepth.b = paste(mcavRelate2$Region.b, mcavRelate2$Depth.b, sep = " ")
mcavRelate = mcavRelate2 %>% filter(regionDepth.a == regionDepth.b) %>% rename(depthZone = depthZone.a, region = region.a)
mcavRelate$depthZone = factor(mcavRelate$depthZone, levels(mcavRelate$depthZone)[c(2,1)])
mcavRelate$region = factor(mcavRelate$region, levels = levels(mcavRelate$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])
mcavRelate = filter(mcavRelate, regionDepth.a != "CUBAMesophotic")


mcavRelateAnova <- aov(relate ~ region * depthZone, data = mcavRelate)
summary(mcavRelateAnova)
relateTukey=TukeyHSD(mcavRelateAnova, which = "region")


colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
hetTheme = theme(axis.title.x = element_blank(),
                 axis.text.x = element_text(color = "black", size = 12),
                 axis.ticks.x = element_line(color = "black"),
                 axis.title.y = element_text(color = "black", size = 12),
                 axis.text.y = element_text(color = "black", size = 10),
                 axis.ticks.y = element_line(color = "black"),
                 legend.position = "right",
                 legend.key.size = unit(0.3, 'cm'),
                 panel.border = element_rect(color = "black"),
                 panel.background = element_rect(fill = "white"),
                 plot.background = element_rect(fill = "white"),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
dodge <- position_dodge(width = 1)

hetPlotSnps = ggplot(data = popStats, aes(x = depthZone, y = hetSnps, fill = region)) +
  geom_point(aes(color = region),shape = 21, position = position_jitterdodge(seed = 1, dodge.width = 1), size = 0.5, alpha = 0.7) +
  geom_violin(size = 0.25, alpha = 0.7, position = dodge, color = "gray35") +
  xlab("Depth") +
  ylab("Heterozygosity") +
  ggtitle("SNPs") +
  scale_fill_manual(values = colPal,name = "Site") +
  scale_color_manual(values = colPal, name="Site") +
  scale_y_continuous(breaks=seq(0, 1, by = .05)) +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5, color = NA, alpha = NA)))+
  hetTheme

inbreedingPlot = ggplot(data = popStats, aes(x = depthZone, y = inbreed, fill = region)) +
  geom_point(aes(color = region),shape = 21, position = position_jitterdodge(seed = 1, dodge.width = 1), size = 0.5, alpha = 0.7) +
  geom_violin(size = 0.25, alpha = 0.7, position = dodge, color = "gray35") +
  xlab("Depth") +
  ylab("Inbreeding coefficient") +
  ggtitle("Inbreeding") +
  scale_fill_manual(values = colPal, name = "Site") +  
  scale_color_manual(values = colPal, name = "Site") +
  scale_y_continuous(breaks=seq(0, 1, by = .05)) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5, color = NA, alpha = NA)))+
  theme_bw() +
  hetTheme

relatePlot = ggplot(data = mcavRelate, aes(x = depthZone, y = relate, fill = region)) +
  geom_point(aes(color = region),shape = 21, position = position_jitterdodge(seed = 1, dodge.width = 1), size = 0.5, alpha = 0.7) +
  geom_violin(size = 0.25, alpha = 0.7, position = dodge, color = "gray35") +
  xlab("Depth") +
  ylab("Relatedness") +
  ggtitle("Relatedness") +
  scale_fill_manual(values = colPal, name = "Site") +  
  scale_color_manual(values = colPal, name="Site") +
  scale_y_continuous(breaks=seq(0, 1, by = .1)) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5, color = NA, alpha = NA)))+
  theme_bw() +
  hetTheme

hetPlots = ( hetPlotSnps | inbreedingPlot | relatePlot) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect")& 
  theme(plot.tag = element_text(size = 16),
        legend.position = "right")
hetPlots
ggsave("hetPlots.tiff", plot = hetPlots, height = 5, width = 15, units = "in", dpi = 300)
#db-RDA#######################################################################################################

###########Create Moran Eigenvector's Maps (MEMs) to be the spatial variables
meta=read.csv("regionalInds2PopsNoClones.csv")
coordsxy = dplyr::select(meta, lat, lon)
coordsyx = dplyr::select(meta, lon, lat)
plot(coordsyx, asp=1)
distSpatial=gcd.hf(coordsxy)  
dbmem = dbmem(distSpatial, MEM.autocor="positive")  
summary(dbmem)
adegraphics::s.label(coordsyx, nb = attr(dbmem, "listw"))
#ade4::s.value(coordsyx, dbmem[,1])

#Environmental Data
  #--------------#
  #
  # Extract data
  #
  #--------------#
  
  datasets = list_datasets(terrestrial = FALSE, marine = TRUE)
  
  # Extract present-day data sets
  present = list_layers(datasets) %>% dplyr::select(dataset_code, layer_code, name, units, description, contains("cellsize"), version) %>% filter(version == 2.2) %>% filter(layer_code %in% c("BO22_calcite", "BO22_cloudmean", "BO22_damean", "BO22_parmean", "BO22_ph", "BO22_carbonphytomean_ss", "BO22_chlomean_ss", "BO22_curvelmean_ss", "BO22_dissoxmean_ss", "BO22_ironmean_ss", "BO22_nitratemean_ss", "BO22_phosphatemean_ss", "BO22_ppmean_ss", "BO22_salinitymean_ss", "BO22_silicatemean_ss", "BO22_tempmean_ss"))

  options(sdmpredictors_datadir = "./bioOracleData")  
  envVar = load_layers(present$layer_code)

# Import coordinates of sites
meta=read.csv("regionalInds2PopsNoClones.csv")
popData = dplyr::select(meta, sample, region, depthM, depthZone, lat, lon)
envData = data.frame(popData, raster::extract(envVar, popData[,6:5]))
envData=cbind(envData, dbmem) 
envData=envData%>%dplyr::select(-BO22_dissoxmean_ss, -BO22_carbonphytomean_ss, -BO22_nitratemean_ss, -BO22_chlomean_ss, -BO22_ppmean_ss, -BO22_silicatemean_ss, -BO22_ironmean_ss, -BO22_parmean, -BO22_cloudmean, -BO22_tempmean_ss, -BO22_ph)

#removing all variables that had significant colinearity >0.7, or that had vif >10
corData = rcorr(as.matrix(envData[,c(3,7:ncol(envData))]), type = "pearson")
corDataFlat = melt(corData$r, value.name = "r")
pDataFlat = melt(corData$P, value.name = "p")
corDataBind = corDataFlat %>% left_join(pDataFlat, by = c("Var1","Var2"))
 
 # corData = cor(envData[,c(4:ncol(envData))])
 # corMelt = melt(corData)
 dev.off()
 
 ggplot(corDataBind) +
   geom_tile(aes(x = Var1, y = Var2, fill = r)) +
   scale_fill_gradient2(low = "#3B9AB2FF", high = "#F21A00FF") +
   geom_text(data = filter(corDataBind, r >= 0.7, p < 0.05),aes(x = Var1, y = Var2, label = round(r, 2))) +
   geom_text(data = filter(corDataBind, r <= -0.7, p < 0.05),aes(x = Var1, y = Var2, label = round(r, 2))) +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90))

  
  # Export data as a csv file
  #write.csv(df, file="environmental_data.csv", row.names = FALSE)
  
  
  snpMa = as.matrix(read.table("regionalMcavNoClones.ibsMat"))
  Pcoa=pcoa(snpMa)
  Pcoa$values$Cum_corr_eig
  ibs=Pcoa$vectors

  rda=rda(ibs, envData[,c(3, 7:ncol(envData))])  
  vif(rda)
  #Salinity has high vif, remove
  envData=envData%>%dplyr::select(-BO22_salinitymean_ss)
  
  rda=rda(ibs, envData[,c(3, 7:ncol(envData))])
  vif(rda)
  RsquareAdj(rda)
  anova(rda, perm=999)  
  rda0<-rda(ibs ~ 1, envData[,c(3, 7:ncol(envData))])  
  rdaG<- rda(ibs ~ ., envData[,c(3, 7:ncol(envData))])
  Sel <- ordiR2step(rda0, scope = formula(rdaG), direction="both") 
  Sel$anova
  
  #Now build a model with only the selected variables
  
  rdaS<- rda(ibs , envData[,c(3, 7:ncol(envData))])
  RsquareAdj(rdaS)
  anova(rdaS, perm=999)
  smry=summary(rdaS, scaling=1)
  smry$sites
  smry$biplot
  summary(eigenvals(rdaS, model = "constrained"))
    

  envSummary=envData %>% dplyr::select(-sample) %>% group_by(region, depthZone, lat, lon) %>% summarise_all(mean)
  write.csv(envSummary, "envSummary.csv")
  model = envData %>% dplyr::select("depth" = depthM,
                                    "light_attenuation"=BO22_damean,
                                    "phosphate" = BO22_phosphatemean_ss, 
                                    "calcite" = BO22_calcite,
                                    "current_velocity" = BO22_curvelmean_ss,
                                    MEM1, MEM2, MEM3, MEM4, MEM5, MEM6, MEM7, MEM8, MEM9, MEM10)
  
  regionalDbrda = dbrda(snpMa ~ depth + light_attenuation + phosphate + calcite + current_velocity + MEM1+ MEM2+ MEM3+ MEM4+ MEM5+ MEM6+ MEM7+ MEM8+ MEM9+ MEM10, model)
  
  endo.var <- varpart(snpMa, ~ depth, ~ light_attenuation + phosphate + calcite + current_velocity, ~ MEM1+ MEM2+ MEM3+ MEM4+ MEM5+ MEM6+ MEM7+ MEM8+ MEM9+ MEM10, data=model)
  par(mfrow=c(1, 2))
  showvarparts(3)
  plot(endo.var)
  install.packages("ggVennDiagram")
  library(ggVennDiagram)
 
  regionalRdaVarTotal = round(regionalDbrda$CA$eig/sum(regionalDbrda$CA$eig)*100, 1)
  head(regionalRdaVarTotal)
  
  regionalRdaVarFitted = round(regionalDbrda$CCA$eig/sum(regionalDbrda$CCA$eig)*100, 1)
  head(regionalRdaVarFitted)

  regionalRdaPoints = as.data.frame(scores(regionalDbrda)$sites)
  rdaI2P = read.csv("regionalInds2popsNoClones.csv")
  regionalRdaPoints$sample = rdaI2P$sample
  head(regionalRdaPoints)
  
  regionalDbrdaData1 = rdaI2P %>% left_join(regionalRdaPoints) 
  
  #%>% cbind(K = dapc1$grp)
  head(regionalDbrdaData1)
  tail(regionalDbrdaData1)
  
  envLoad = as.data.frame(regionalDbrda$CCA$biplot)
  envLoad$var = c("depth", "light_attenuation", "phosphate", "calcite", "current_velocity", "MEM1", "MEM2", "MEM3", "MEM4", "MEM5", "MEM6", "MEM7", "MEM8", "MEM9", "MEM10")
  
  regionalDbrdaData = merge(regionalDbrdaData1, aggregate(cbind(mean.x = dbRDA1, mean.y = dbRDA2)~regionDepth, regionalDbrdaData1, mean), by="regionDepth") 
  
  #%>% merge(., aggregate(cbind(mean.x.K = dbRDA1, mean.y.K = dbRDA2)~K, regionalDbrdaData1, mean), by="K")
  
  regionalDbrdaData$depthZone = factor(regionalDbrdaData$depthZone)
  regionalDbrdaData$depthZone = factor(regionalDbrdaData$depthZone, levels(regionalDbrdaData$depthZone)[c(2,1)])
  regionalDbrdaData$region = factor(regionalDbrdaData$region)
regionalDbrdaData$region = factor(regionalDbrdaData$region, levels(regionalDbrdaData$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])
  
  head(regionalDbrdaData)
  
  colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
  regionalDbrdaPlotA = ggplot() +
    geom_hline(yintercept = 0, color = "gray90", size = 0.5) +
    geom_vline(xintercept = 0, color = "gray90", size = 0.5) +
    geom_point(data = regionalDbrdaData, aes(x = dbRDA1, y = dbRDA2, fill = region, shape = depthZone), 
               size = 2, stroke = NA, alpha = 0.5, show.legend = FALSE) +
    scale_shape_manual(values = c(24,25), name = "Depth Zone") +
    # geom_segment(data = regionalDbrdaData, aes(x = mean.x, y = mean.y, xend = dbRDA1, yend = dbRDA2, color = pop), size = 0.5, alpha = 0.5) +
    #geom_segment(data = envLoad, aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2), color = "black", arrow = arrow(length = unit(0.25, "cm"), type = "open"), size = 0.65) +
    geom_segment(data = subset(envLoad, subset=envLoad$var %in% c("light_attenuation", "phosphate", "calcite", "current_velocity" )), aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2), color = "black", arrow = arrow(length = unit(0.25, "cm"), type = "open"), size = 0.65) +
    #geom_text(data = envLoad, aes(x = dbRDA1, y = dbRDA2, label = var), color = "black", size = 3) +
    geom_point(data = regionalDbrdaData, aes(x = mean.x, y = mean.y, fill = region, shape = depthZone),
               size = 5, color = "black") + #population centroids indicated by triangles
    scale_fill_manual(values = colPal, name = "Site") +
    scale_color_manual(values = colPal, name = "Site") +
    xlab(paste ("dbRDA 1 (", regionalRdaVar[1],"%)", sep = "")) + #Prints percent variation explained by first axis
    ylab(paste ("dbRDA 2 (", regionalRdaVar[2],"%)", sep = "")) + #Prints percent variation explained by second axis
    guides(shape = guide_legend(override.aes = list(size = 3.5, stroke = 0.5, alpha = 0.7), order = 2), fill = guide_legend(override.aes = list(shape = 22, size = 5, color = NA, alpha = NA), order = 1))+
    theme_bw()
  
  regionalDbrdaPlot = regionalDbrdaPlotA +
    theme(axis.title.x = element_text(color = "black", size = 10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_text(color = "black", size = 10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "right",
          panel.border = element_rect(color = "black", size = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  regionalDbrdaPlot
  
  ggsave("regionalDbrdaEnviro.tiff", plot = regionalDbrdaPlot, height = 5, width = 7, units = "in", dpi = 300)
  ggsave("regionalDbrdaSpatial.tiff", plot = regionalDbrdaPlot, height = 5, width = 7, units = "in", dpi = 300)
  ggsave("regionalDbrdaDepth.tiff", plot = regionalDbrdaPlot, height = 5, width = 7, units = "in", dpi = 300)
  ggsave("regionalDbrdaPlotNoVectors.tiff", plot = regionalDbrdaPlot, height = 5, width = 7, units = "in", dpi = 300)
  ggsave("regionalDbrdaPlot.tiff", plot = regionalDbrdaPlot, height = 5, width = 7, units = "in", dpi = 300)
  ggsave("regionalDbrdaPlotUnlabeled.tiff", plot = regionalDbrdaPlot, height = 5, width = 7, units = "in", dpi = 300)
#AMOVA####################################################
  regionalVcf = read.vcfR("regionalMcavNoClones.bcf")
  regionalGenlightPopDepth = vcfR2genlight(regionalVcf, n.cores = 2) # Converts the vcf file into a file format that poppr uses the "genlight" format
  #locNames(regionalGenlightPopDepth) = paste(regionalVcf@fix[,1],regionalVcf@fix[,2],sep="_")
  popData = read.csv("regionalInds2PopsNoClones.csv") # Reads in population data for each sample
  
  strata(regionalGenlightPopDepth) = data.frame(popData)
  setPop(regionalGenlightPopDepth) = ~regionDepth
  ploidy(regionalGenlightPopDepth) <- 2
  amova <- poppr.amova(regionalGenlightPopDepth, ~regionDepth) #Runs AMOVA
  amova
  
  set.seed(1999)
  #amovasignif <- randtest(amova, nrepet = 99) #Calculates significance levels of the AMOVA with 99 permutations
  #amovasignif
  #plot(amovasignif)
  #Test       Obs    Std.Obs   Alter Pvalue
  # Variations between regionDepth  12.04648  76.358764 greater   0.01
  
  set.seed(694)
  regional.fst <- stamppFst(regionalGenlightPopDepth, nboots = 99, percent = 95, nclusters = 4) #99 permutations
  regional.fst$Fsts
  regional.fst$Pvalues
  
#FST Heat Map####################################################  
  
  pop.order <- c("SEFLShallow", "FKShallow", "FKMesophotic", "DRTShallow", "DRTMesophotic", "PRGMesophotic", "CUBAShallow", "CUBAMesophotic", "NWGOMShallow", "NWGOMMesophotic", "SGOMShallow", "SGOMMesophotic", "BLZShallow", "BLZMesophotic")
  # reads in fst matrix
  snpFstMa <- as.matrix(regional.fst$Fsts)
  upperTriangle(snpFstMa, byrow=TRUE) <- lowerTriangle(snpFstMa)
  snpFstMa <- snpFstMa[,pop.order] %>%
    .[pop.order,]
  snpFstMa[upper.tri(snpFstMa)] <- NA
  snpFstMa <- as.data.frame(snpFstMa)
  
  snpFstMa$Pop = factor(row.names(snpFstMa), levels = unique(pop.order))
  
  snpQMa <- as.matrix(regional.fst$Pvalues)
  upperTriangle(snpQMa, byrow=TRUE) <- lowerTriangle(snpQMa)
  snpQMa <- snpQMa[,pop.order] %>%
    .[pop.order,]
  snpQMa[upper.tri(snpQMa)] <- NA
  snpQMa <- as.data.frame(snpQMa)
  snpQMa$Pop = factor(row.names(snpQMa), levels = unique(pop.order))
  
  snpFstMa$Pop = factor(row.names(snpFstMa), levels = unique(pop.order))
  snpFst = melt(snpFstMa, id.vars = "Pop", value.name = "Fst", variable.name = "Pop2", na.rm = TRUE)
  snpFst = snpFst[snpFst$Pop != snpFst$Pop2,]
  snpFst$Fst = round(snpFst$Fst, 3)
  snpFst = snpFst %>% mutate(Fst = replace(Fst, Fst < 0, 0))
  head(snpFst)
  
  snpQ = melt(snpQMa, id.vars = "Pop", value.name = "Pval", variable.name = "Pop2", na.rm = TRUE)
  snpQ = snpQ[snpQ$Pop != snpQ$Pop2,]
  snpQ$Qval = p.adjust(snpQ$Pval, method = "BH")
  head(snpQ)
  
  snpHeatmapA = ggplot(data = snpFst, aes(Pop, Pop2, fill = Fst))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "white", high = "turquoise", midpoint = 0, limit = c(0, 0.155),
                         space = "Lab", name = expression(paste(italic("F")[ST])))+
    geom_text(data = snpFst, aes(Pop, Pop2, label = Fst), color = ifelse(snpQ$Qval <= 0.05,"black", "darkgrey"), size = ifelse(snpQ$Qval < 0.05, 6, 5), fontface = ifelse(snpQ$Qval <= 0.05, "bold", "plain")) +
    guides(fill=guide_colorbar(barwidth = 1, barheight = 12, title.position = "top", title.hjust = 0.5))+                     
    scale_y_discrete(position = "right", labels=str_wrap(c("SEFL Shallow", "FK Shallow", "FK Mesophotic", "DRT Shallow", "DRT Mesophotic", "PRG Mesophotic", "CUBA Shallow", "CUBA Mesophotic", "NWGOM Shallow", "NWGOM Mesophotic", "SGOM Shallow", "SGOM Mesophotic", "BLZ Shallow")))+
    scale_x_discrete(labels = (c("FK\nShallow", "FK\nMesophotic", "DRT\nShallow", "DRT\nMesophotic", "PRG\nMesophotic", "CUBA\nShallow", "CUBA\nMesophotic", "NWGOM\nShallow", "NWGOM\nMesophotic", "SGOM\nShallow", "SGOM\nMesophotic", "BLZ\nShallow", "BLZ\nMesophotic"))) +
    #ggtitle("   SNP") +
    #geom_rect(xmin=1.5, xmax=2.5, ymin=1.5, ymax=2.5, color="#FFA58BFF", fill=NA, size=2.5)+
    geom_rect(xmin=3.5, xmax=4.5, ymin=3.5, ymax=4.5, color="#FFDE8BFF" , fill=NA, size=2.5)+
    geom_rect(xmin=6.5, xmax=7.5, ymin=6.5, ymax=7.5, color="#94D0FFFF" , fill=NA,size=2.5)+
    #geom_rect(xmin=8.5, xmax=9.5, ymin=8.5, ymax=9.5, color="#C774E8FF" , fill=NA,size=2.5)+
    geom_rect(xmin=10.5, xmax=11.5, ymin=10.5, ymax=11.5, color="#966BFFFF" , fill=NA,size=2.5)+
    geom_rect(xmin=12.5, xmax=13.5, ymin=12.5, ymax=13.5, color="slateblue4" , fill=NA,size=2.5)+
    #geom_rect(xmin=3.5, xmax=4.5, ymin=2.5, ymax=3.5, color="red" , fill=NA, size=2)+
    theme_minimal()
  
  snpHeatmap = snpHeatmapA + theme(
    axis.text.x = element_text(vjust = 1, size = 16, hjust = 0.5, color = "black", angle=90),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 16, color="black"),
    legend.text = element_text(size = 14, color="black"),
    plot.title = element_text(size = 16)
  )
  
  snpHeatmap
  
  ggsave("heatMapHigh.tiff", plot = snpHeatmap, width = 34, height = 15, units = "cm", dpi = 300)
  
#NGSAdmix####################################################
  colPalAdmix = paletteer_c("viridis::viridis", n=4)
  k4=read.csv("k4ngsAdmix.csv")
  meta=read.csv("regionalInds2PopsNoClones.csv")
  k4Admix=bind_cols(meta, k4)
  shapeLabs=(c("Shallow"=expression("\U25B2"), "Mesophotic"=expression("\U25BC")))
 
  blzShallowK4=filter(k4Admix, regionDepth == "BLZShallow") %>% arrange(-cluster3, cluster2)
  sum(blzShallowK4$cluster1)
  sum(blzShallowK4$cluster2)
  sum(blzShallowK4$cluster3)
  sum(blzShallowK4$cluster4)
  blzShallowK4$ord=1:nrow(blzShallowK4)
  
  blzMesoK4=filter(k4Admix, regionDepth == "BLZMesophotic") %>% arrange(-cluster2, cluster3)
  sum(blzMesoK4$cluster1)
  sum(blzMesoK4$cluster2)
  sum(blzMesoK4$cluster3)
  sum(blzMesoK4$cluster4)
  blzMesoK4$ord=88:116
  
  blzAdmix=bind_rows(blzShallowK4, blzMesoK4)
  
  meltBlzK4 = melt(blzAdmix, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  meltBlzK4$depthZone = factor(meltBlzK4$depthZone, levels = levels(meltBlzK4$depthZone)[c(2,1)])
  
  shapeLabs=as_labeller(c(`Shallow`="\u25B2", `Mesophotic`="\u25BC"))
  
  blzK4Plot = ggplot(meltBlzK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster4", "cluster1", "cluster2", "cluster3")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone,labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    scale_shape_manual(values = c(24,25))+
    #labs(x = "Population", y = "Ancestry") +
    #ggtitle("Belize") +
    theme(plot.title = element_blank(),
          #plot.title = element_text(size=22),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_blank(),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "slateblue4", vjust=2),
          #strip.text=element_text(size=20, angle=45),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  blzK4Plot 
 ############### 
  cubaShallowK4=filter(k4Admix, regionDepth == "CUBAShallow") %>% arrange(-cluster3, cluster1)
  sum(cubaShallowK4$cluster1)
  sum(cubaShallowK4$cluster2)
  sum(cubaShallowK4$cluster3)
  sum(cubaShallowK4$cluster4)
  cubaShallowK4$ord=1:nrow(cubaShallowK4)
  
  cubaMesoK4=filter(k4Admix, regionDepth == "CUBAMesophotic") %>% arrange(-cluster2, cluster1)
  sum(cubaMesoK4$cluster1)
  sum(cubaMesoK4$cluster2)
  sum(cubaMesoK4$cluster3)
  sum(cubaMesoK4$cluster4)
  cubaMesoK4$ord=77:78
  
  #cubaAdmix=bind_rows(cubaShallowK4, cubaMesoK4)
  
  meltcubaShallowK4 = melt(cubaShallowK4, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  meltcubaMesoK4 = melt(cubaMesoK4, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  #meltcubaK4$depthZone = factor(meltcubaK4$depthZone, levels = levels(meltcubaK4$depthZone)[c(2,1)])
  
  cubaK4ShallowPlot = ggplot( meltcubaShallowK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster4", "cluster1", "cluster2", "cluster3")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Cuba") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill=NA),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#94D0FFFF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  cubaK4ShallowPlot 
  
  
  cubaK4MesoPlot = ggplot( meltcubaMesoK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster4", "cluster1", "cluster2", "cluster3")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Cuba") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill=NA),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#94D0FFFF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  cubaK4MesoPlot 
  ###############  
  drtShallowK4=filter(k4Admix, regionDepth == "DRTShallow") %>% arrange(-cluster4, cluster1)
  sum(drtShallowK4$cluster1)
  sum(drtShallowK4$cluster2)
  sum(drtShallowK4$cluster3)
  sum(drtShallowK4$cluster4)
  drtShallowK4$ord=1:nrow(drtShallowK4)
  
  drtMesoK4=filter(k4Admix, regionDepth == "DRTMesophotic") %>% arrange(-cluster2, cluster4)
  sum(drtMesoK4$cluster1)
  sum(drtMesoK4$cluster2)
  sum(drtMesoK4$cluster3)
  sum(drtMesoK4$cluster4)
  drtMesoK4$ord=56:115
  
  drtAdmix=bind_rows(drtShallowK4, drtMesoK4)
  
  meltdrtK4 = melt(drtAdmix, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  meltdrtK4$depthZone = factor(meltdrtK4$depthZone, levels = levels(meltdrtK4$depthZone)[c(2,1)])
  
  drtK4Plot = ggplot(meltdrtK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster3","cluster1","cluster2","cluster4")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Dry Tortugas") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill=NA),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#FFDE8BFF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  drtK4Plot 
  
  ############### 
  fkShallowK4=filter(k4Admix, regionDepth == "FKShallow") %>% arrange(-cluster2, cluster4)
  sum(fkShallowK4$cluster1)
  sum(fkShallowK4$cluster2)
  sum(fkShallowK4$cluster3)
  sum(fkShallowK4$cluster4)
  fkShallowK4$ord=1:nrow(fkShallowK4)
  
  fkMesoK4=filter(k4Admix, regionDepth == "FKMesophotic") %>% arrange(-cluster2, cluster4)
  sum(fkMesoK4$cluster1)
  sum(fkMesoK4$cluster2)
  sum(fkMesoK4$cluster3)
  sum(fkMesoK4$cluster4)
  fkMesoK4$ord=63:100
  
  fkAdmix=bind_rows(fkShallowK4, fkMesoK4)
  
  meltfkK4 = melt(fkAdmix, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  meltfkK4$depthZone = factor(meltfkK4$depthZone, levels = levels(meltfkK4$depthZone)[c(2,1)])
  
  fkK4Plot = ggplot(meltfkK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster3","cluster1","cluster2","cluster4")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Florida Keys") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill="black"),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#FFA58BFF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  fkK4Plot 
  ###############
  sgomShallowK4=filter(k4Admix, regionDepth == "SGOMShallow") %>% arrange(-cluster1, cluster4)
  sum(sgomShallowK4$cluster1)
  sum(sgomShallowK4$cluster2)
  sum(sgomShallowK4$cluster3)
  sum(sgomShallowK4$cluster4)
  sgomShallowK4$ord=1:nrow(sgomShallowK4)
  
  sgomMesoK4=filter(k4Admix, regionDepth == "SGOMMesophotic") %>% arrange(-cluster2, cluster4)
  sum(sgomMesoK4$cluster1)
  sum(sgomMesoK4$cluster2)
  sum(sgomMesoK4$cluster3)
  sum(sgomMesoK4$cluster4)
  sgomMesoK4$ord=72:97
  
  sgomAdmix=bind_rows(sgomShallowK4, sgomMesoK4)
  
  meltsgomK4 = melt(sgomAdmix, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  meltsgomK4$depthZone = factor(meltsgomK4$depthZone, levels = levels(meltsgomK4$depthZone)[c(2,1)])
  
  sgomK4Plot = ggplot(meltsgomK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster3", "cluster4","cluster2","cluster1")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Southern Gulf of Mexico") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill="black"),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#966BFFFF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  sgomK4Plot 
  ###############
  ###############
  nwgomShallowK4=filter(k4Admix, regionDepth == "NWGOMShallow") %>% arrange(-cluster1, cluster2)
  sum(nwgomShallowK4$cluster1)
  sum(nwgomShallowK4$cluster2)
  sum(nwgomShallowK4$cluster3)
  sum(nwgomShallowK4$cluster4)
  nwgomShallowK4$ord=1:nrow(nwgomShallowK4)
  
  nwgomMesoK4=filter(k4Admix, regionDepth == "NWGOMMesophotic") %>% arrange(-cluster1, cluster2)
  sum(nwgomMesoK4$cluster1)
  sum(nwgomMesoK4$cluster2)
  sum(nwgomMesoK4$cluster3)
  sum(nwgomMesoK4$cluster4)
  nwgomMesoK4$ord=54:157
  
  nwgomAdmix=bind_rows(nwgomShallowK4, nwgomMesoK4)
  
  meltnwgomK4 = melt(nwgomAdmix, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  meltnwgomK4$depthZone = factor(meltnwgomK4$depthZone, levels = levels(meltnwgomK4$depthZone)[c(2,1)])
  
  nwgomK4Plot = ggplot(meltnwgomK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster3", "cluster4","cluster2","cluster1")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Northwest Gulf of Mexico") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill="black"),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#C774E8FF" , vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  nwgomK4Plot 
  ###############
  ###############
  seflShallowK4=filter(k4Admix, regionDepth == "SEFLShallow") %>% arrange(-cluster3, cluster2)
  sum(seflShallowK4$cluster1)
  sum(seflShallowK4$cluster2)
  sum(seflShallowK4$cluster3)
  sum(seflShallowK4$cluster4)
  seflShallowK4$ord=1:nrow(seflShallowK4)
  
  meltseflK4 = melt(seflShallowK4, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  
  seflK4Plot = ggplot(meltseflK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster4", "cluster1","cluster2","cluster3")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Southeast Florida") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill=NA),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#FF6AD5FF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  seflK4Plot 
  ###############
  ###############
  prgMesoK4=filter(k4Admix, regionDepth == "PRGMesophotic") %>% arrange(-cluster1, cluster3)
  sum(prgMesoK4$cluster1)
  sum(prgMesoK4$cluster2)
  sum(prgMesoK4$cluster3)
  sum(prgMesoK4$cluster4)
  prgMesoK4$ord=1:nrow(prgMesoK4)
  
  meltprgK4 = melt(prgMesoK4, id.vars=c("sample", "site", "pop", "region", "depthM", "depthZone", "dateCollected", "lat", "lon", "regionDepth", "ord"), variable.name="ancestry", value.name="fraction")
  
  prgK4Plot = ggplot(meltprgK4, aes(x=ord, y=fraction, fill=factor(ancestry, levels=c("cluster4", "cluster2","cluster3","cluster1")))) +
    geom_bar(stat="identity", position="fill", width=1) +
    facet_grid(.~depthZone, labeller = shapeLabs, scales = "free", switch = "x", space = "free") +
    #labs(x = "Population", y = "Ancestry") +
    ggtitle("Pulley Ridge") +
    theme(plot.title = element_blank(),
          plot.background=element_blank(),
          panel.grid=element_blank(),
          panel.background=element_rect(fill=NA),
          panel.spacing.x=grid:::unit(0, "lines"),
          panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"),
          #axis.text.x=element_text(size=12, angle=90)
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=15, color = "#20DE8BFF", vjust=2),
          legend.key=element_blank(),
          legend.position = "none",
          legend.title = element_blank()) +
    scale_x_discrete(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    scale_fill_manual(values = c("cluster1"="#440154FF", "cluster2"="#31688EFF", "cluster3"="#35B779FF", "cluster4"="#FDE725FF"), name = "Cluster") +
    #scale_fill_manual(values = colPal3, name = "Cluster") +
    guides(fill=guide_legend(override.aes=list(colour=NULL)))
  prgK4Plot 
  ###############
  colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
  sites = read.csv("regionalInds2popsNoClones.csv", header = TRUE)
  #sites$depthZone=as.factor(sites$depthZone)
  sites$depthZone = factor(sites$depthZone, levels = levels(sites$depthZone)[c(2,1)])
  sites$region = factor(sites$region, levels = levels(sites$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])
  reefs = sites %>% group_by(region) %>% dplyr::summarize(lat = mean(lat), lon = mean(lon))
  gomCountries = st_as_sf(ne_countries(country = c("United States of America", "Cuba", "Mexico", "The Bahamas", "Belize", "Guatemala", "Cayman Islands"), scale = "Large"))
  admixMapA = ggplot() +
    geom_sf(data = gomCountries, fill = "white", size = 0.25) +
    geom_point(data = reefs, aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
    geom_point(data = sites, aes(x = lon, y = lat, fill = region, shape=depthZone), size = 1, fill=NA, alpha=0) +
    scale_fill_manual(values = colPal, name = "Site")+
    scale_shape_manual(values = c(24,25), name = "Depth Zone") +
    coord_sf(xlim = c(-99, -75), ylim = c(16, 30)) +
    scale_x_continuous(breaks = c(seq(-99, -75, by = 2))) +
    scale_y_continuous(breaks = c(seq(16, 30, by = 2))) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
    guides(fill = guide_legend(override.aes = list(shape = 22, color = NA, size = 4), ncol=2, order = 1), shape = guide_legend(override.aes = list(size = 4, alpha=1), order = 2)) +
    #guides(fill = guide_legend(override.aes = list(shape = 21, size = 4), order = 1))+ #,
    #shape = guide_legend(order=2),
    #color = guide_legend(title=NULL, order = 3))+ #override.aes = list(fill=NA),
    theme_bw() +
    theme(plot.title = element_text(size = 9) ,
          panel.background = element_rect(fill = "aliceblue"),
          plot.background = element_rect(fill="white"),
          axis.title = element_blank(),
          legend.position = "bottom")
  
  admixMapA
  ggsave("regionMapNoSites.png", plot = admixMapA, width = 30, height = 20, units = "cm", dpi = 300)
  
  #0.002 times the number of samples for width
  admixMap <-
    ggdraw() +
    draw_plot(admixMapA) +
    draw_plot(blzK4Plot, x = 0.5, y = .18, width = .232, height = .15)+
    draw_plot(cubaK4ShallowPlot, x = 0.60, y = .33, width = .152, height = .15)+
    draw_plot(cubaK4MesoPlot, x = 0.73, y = .33, width = .03, height = .15)+
    draw_plot(drtK4Plot, x = 0.38, y = .59, width = .23, height = .15)+
    draw_plot(fkK4Plot, x = 0.72, y = .56, width=0.2, height=0.15)+
    draw_plot(sgomK4Plot, x = 0.22, y = .46, width=0.194, height=0.15)+
    draw_plot(nwgomK4Plot, x = 0.19, y = .82, width=0.314, height=0.15)+
    draw_plot(prgK4Plot, x = 0.60, y = .71, width=0.044, height=0.15)+
    draw_plot(seflK4Plot, x = 0.74, y = .70, width=0.132, height=0.15)
  
  ggsave("admixMap.png", plot = admixMap, width = 30, height = 20, units = "cm", dpi = 300)
  #BayesAss############################################################################
  ## Genetic connectivity
  
  #Checking deviance among model runs from BayesAss we ran on HPC
  
  fileList = substr(list.files("./ba3TraceFiles/", "BA3trace.*.txt$"),1,20)
  
  bayesian_deviance <- function(trace, burnin = 0, sampling.interval = 0){
    if(burnin == 0) stop('No burnin specified')
    if(sampling.interval == 0) stop('No sampling interval specified')
    range <- (trace$State > burnin & trace$State %% sampling.interval == 0)
    D <- -2*mean(trace$LogProb[range])
    return(D)
  }
  
  # for(i in 1:length(fileList)){
  #   assign(fileList[i], read.delim(paste("./ba3TraceFiles/", fileList[i], ".txt", sep = ""))) %>% dplyr::select(-last_col())
  #   print(paste(fileList[i], bayesian_deviance(get(fileList[i]), burnin = 9000000, sampling.interval = 100)))
  # }
  
  for(i in 1:length(fileList)){
    assign(fileList[i], read.delim(paste("./ba3TraceFiles/", fileList[i], sep = ""))) %>% dplyr::select(-last_col())
    print(paste(fileList[i], bayesian_deviance(get(fileList[i]), burnin = 9000000, sampling.interval = 100)))
  }
  
  
  #All traces have similar deviance (this is good!).
  #Using the trace with the lowest deviance (BA3trace08.txt, in this case)
  
  bayesAss = read.delim("./ba3TraceFiles/BA3trace12.txt") %>% filter(State > 9000000) %>% dplyr::select(-State, -LogProb, -X)
  
  baMean = bayesAss %>% summarise(across(everything(), list(mean))) %>% t() %>% as_tibble() %>% rename(., mean=V1) %>% mutate(pops = colnames(bayesAss))
  
  baSumm = bayesAss %>% summarise(across(everything(), list(median))) %>% t() %>% as.data.frame() %>% rename(., median=V1) %>% mutate(pops = baMean$pops, mean = round(baMean$mean, 3)) %>% relocate(median, .after = mean)
  
  baSumm$median = round(baSumm$median, 3)
  
  #Make sure teachingdemos package is installed
  baHpd =as.data.frame(t(sapply(bayesAss, emp.hpd)))
  colnames(baHpd) = c("hpdLow", "hpdHigh")
  baHpd$pops = rownames(baHpd)
  
  ESS = as.data.frame(sapply(bayesAss, ESS))
  colnames(ESS) = "ESS"
  
  baSumm = baSumm %>% left_join(baHpd)
  baSumm$hpdLow = round(baSumm$hpdLow, 3)
  baSumm$hpdHigh = round(baSumm$hpdHigh, 3)
  baSumm$ESS = ESS$ESS
  
  ### FROM BAYESASS: ###
  #1->BLZMesophotic 0->BLZShallow 2->CUBAMesophotic 
  #3->CUBAShallow 4->DRTMesophotic 5->DRTShallow 
  #7->FKMesophotic 6->FKShallow 10->NWGOMMesophotic 
  #11->NWGOMShallow 12->PRGMesophotic 13->SEFLShallow 
  #9->SGOMMesophotic 8->SGOMShallow
  
  popi = rep(c("BLZ\nShallow", "BLZ\nMesophotic", "CUBA\nMesophotic", "CUBA\nShallow", "DRT\nMesophotic", "DRT\nShallow", "FK\nShallow", "FK\nMesophotic", "SGOM\nShallow", "SGOM\nMesophotic", "NWGOM\nMesophotic", "NWGOM\nShallow", "PRG\nMesophotic", "SEFL\nShallow"), each = 14)
  
  popj = rep(c("BLZ\nShallow", "BLZ\nMesophotic", "CUBA\nMesophotic", "CUBA\nShallow", "DRT\nMesophotic", "DRT\nShallow", "FK\nShallow", "FK\nMesophotic", "SGOM\nShallow", "SGOM\nMesophotic", "NWGOM\nMesophotic", "NWGOM\nShallow", "PRG\nMesophotic", "SEFL\nShallow"), times = 14)
  
  baSumm = baSumm %>% mutate(pop.i = popi, pop.j = popj) %>% relocate(c(pop.i, pop.j), .after = pops) %>% dplyr::select(-pops)
  
  baSumm$pop.i = factor(baSumm$pop.i)
  levels(baSumm$pop.i)
  baSumm$pop.i = factor(baSumm$pop.i, levels = levels(baSumm$pop.i)[c(12, 8, 6, 4, 10, 14, 2, 7, 5, 11, 3, 9, 13, 1)])
  
  baSumm$pop.j = factor(baSumm$pop.j)
  baSumm$pop.j = factor(baSumm$pop.j, levels = levels(baSumm$pop.j)[c(12, 8, 6, 4, 10, 14, 2, 7, 5, 11, 3, 9, 13, 1)])
  
  baSumm$site.i = word(baSumm$pop.i, 1, sep = "\n")
  baSumm$site.i = factor(baSumm$site.i)
  levels(baSumm$site.i)
  baSumm$site.i = factor(baSumm$site.i, levels = levels(baSumm$site.i)[c(7, 4, 3, 6, 2, 5, 8, 1)])
  
  baSumm$site.j = word(baSumm$pop.j, 1, sep = "\n")
  baSumm$site.j = factor(baSumm$site.j)
  baSumm$site.j = factor(baSumm$site.j, levels = levels(baSumm$site.j)[c(7, 4, 3, 6, 2, 5, 8, 1)])
  
  baSumm$depth.i = word(baSumm$pop.i, 2, sep = "\n")
  baSumm$depth.i = factor(baSumm$depth.i)
  baSumm$depth.i = factor(baSumm$depth.i, levels = levels(baSumm$depth.i)[c(2, 1)])
  
  baSumm$depth.j = word(baSumm$pop.j, 2, sep = "\n")
  baSumm$depth.j = factor(baSumm$depth.j)
  baSumm$depth.j = factor(baSumm$depth.j, levels = levels(baSumm$depth.j)[c(2, 1)])
  

  # times100 <- function(x) {
  # round((x*100),2)
  # }
  
  #All sites (excluding self retention)
  baMeans = baSumm %>% filter(pop.i != pop.j) %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.))) %>% mutate(dataset = "Global")
  
  #mesophotic sources
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.j == "Mesophotic") %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.))) %>% mutate(dataset = "Mesophotic Source") %>% bind_rows(baMeans, .)
  
  #shallow sources
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.j == "Shallow") %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.))) %>% mutate(dataset = "Shallow Source") %>% bind_rows(baMeans, .)
  
  #mesophotic sinks
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.i == "Mesophotic") %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.)))  %>% mutate(dataset = "Mesophotic Sink") %>% bind_rows(baMeans, .)
  
  #shallow sinks
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.i == "Shallow") %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.)))  %>% mutate(dataset = "Shallow Sink") %>% bind_rows(baMeans, .)
  
  #mesophotic -> shallow
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.j == "Mesophotic", depth.i == "Shallow") %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.))) %>% mutate(dataset = "Mesophotic -> Shallow") %>% bind_rows(baMeans, .)
  
  #mesophotic -> mesophotic
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.j == "Mesophotic", depth.i == "Mesophotic") %>% summarise(mean = mean(mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.))) %>% mutate(dataset = "Mesophotic -> Mesophotic") %>% bind_rows(baMeans, .)
  
  #shallow -> mesophotic
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.j == "Shallow", depth.i == "Mesophotic") %>% summarise(mean = mean(.$mean), sd = sd(.$mean), se = sd(.$mean)/sqrt(nrow(.))) %>% mutate(dataset = "Shallow -> Mesophotic") %>% bind_rows(baMeans, .)
  
  #shallow -> shallow
  baMeans = baSumm %>% filter(pop.i != pop.j, depth.j == "Shallow", depth.i == "Shallow") %>% summarise(mean = round(mean(.$mean), 3), sd = round(sd(.$mean), 3), se = round(sd(.$mean)/sqrt(nrow(.)), 3)) %>% mutate(dataset = paste("Shallow -> Shallow")) %>% bind_rows(baMeans, .) %>% relocate(dataset, .before = mean) %>% as.data.frame()
  
  baMeans[,c(2:4)] = baMeans[,c(2:4)] %>% round(3)
  
  baMeans
  
  baMeansTabPub = baMeans %>%
    flextable() %>%
    flextable::compose(part = "header", j = "dataset", value = as_paragraph("Dataset")) %>%
    flextable::compose(part = "header", j = "mean", value = as_paragraph(as_i("m"))) %>%
    flextable::compose(part = "header", j = "sd", value = as_paragraph("SD")) %>%
    flextable::compose(part = "header", j = "se", value = as_paragraph("SEM")) %>%
    # flextable::compose(part = "header", j = "Rab", value = as_paragraph(as_i("R"), as_i(as_sub("AB")))) %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::autofit()
  
  table3 = read_docx()
  table3 = body_add_flextable(table3, value = baMeansTabPub)
  print(table3, target = "../tables/table3.docx")
  
  baMeansTabPub

  #MigrationPlot
  baSumm$mean = sprintf('%.3f', baSumm$mean)
  baSumm$mean2 = baSumm$mean
  baSumm$hpdLow = sprintf('%.3f', baSumm$hpdLow)
  baSumm$hpdHigh = sprintf('%.3f', baSumm$hpdHigh)
  
  colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
  migrateA = ggplot(data = baSumm, aes(pop.i, pop.j))+
    geom_tile(data = subset(baSumm, subset = baSumm$mean2>0.67), aes(fill = as.numeric(as.character(mean2))), color = "white") +
    #geom_segment(data = baSumm, aes(x = 0.4755, xend = -0.625, y = pop.j, yend = pop.j, color = pop.j), size = 14) +
    geom_segment(data = baSumm, aes(x = -0.45, xend = 0.45, y = pop.j, yend = pop.j, color = pop.j), size = 15) +
    #geom_segment(data = baSumm, aes(x = pop.i, xend = pop.i, y = 0.45, yend = -0.425, color = pop.i), size = 32.25) +
    geom_segment(data = baSumm, aes(x = pop.i, xend = pop.i, y = 0.45, yend = -0.35, color = pop.i), size = 35) +
    scale_color_manual(values = colPal[c(1:3,5:8, 2:8)], guide = NULL) +
    scale_fill_gradient(low = "grey70", high = "grey5", limit = c(0.67, 0.87), space = "Lab", name = expression(paste(italic("m"[r]))),  guide = "colourbar", breaks = c(0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)) +
    guides(fill = guide_colorbar(ticks.colour = "black")) +
    new_scale("fill") +
    geom_tile(data = subset(baSumm, subset = baSumm$mean<0.67), aes(fill = as.numeric(as.character(mean))), color = "white") +
    scale_fill_gradientn(colours = paletteer_c("viridis::viridis", n = 10)[3:10], limit = c(0,0.24), space = "Lab", name = expression(paste(italic("m"[i]))), na.value = "transparent",  guide = "colourbar", values = c(0, 0.05, 0.1, 0.15, 0.2,0.5,0.75,1)) +
    geom_text(data = baSumm, aes(x = pop.i, y = pop.j, label = paste(mean, "\n", sep = "")), color = ifelse(baSumm$mean > 0.6, "white", "black"), fontface = ifelse(baSumm$hpdLow > 0.001, "bold", "plain"), size = ifelse(baSumm$hpdLow > 0.001, 4, 3)) +
    geom_text(data = baSumm, aes(x = pop.i, y = pop.j, label = paste("\n(",hpdLow,"–",hpdHigh, ")", sep = "")), color = ifelse(baSumm$mean > 0.6, "white", "black"), fontface = ifelse(baSumm$hpdLow > 0.001, "bold", "plain"), size = ifelse(baSumm$hpdLow > 0.001, 4, 3)) +
    labs(x = "Sink", y = "Source") +
    # guides(fill = guide_colorbar(barwidth = 1, barheight = 12, title.position = "top", title.hjust = 0.5, frame.colour = "black", frame.linewidth = 1, ticks.colour = "black")) +
    scale_y_discrete(limits = rev(levels(baSumm$pop.i))[c(1:14)], position = "left") +
    # scale_x_discrete() +
    coord_cartesian(xlim = c(1, 14), ylim = c(1, 14), clip = "off") +
    theme_minimal()
  
  migrate = migrateA + theme(
    axis.text.x = element_text(vjust = 1, size = 10, hjust = 0.5, color = "black", face= "bold"),
    axis.text.y = element_text(size = 10, color = "black",hjust = 0.5, face= "bold"),
    axis.title.x = element_text(size = 14, vjust=0.1),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10)
  )
  
   migrate
  
  ggsave("migrate.png", plot = migrate, width = 45, height = 20, units = "cm", dpi = 300)
  # ggsave("../figures/migrate.svg", plot = migrate, width = 26, height = 11, units = "cm", dpi = 300)
  
####Migration Map
  baSumm$mean = as.numeric(baSumm$mean)
  baSumm$hpdLow = as.numeric(baSumm$hpdLow)
  baSumm$hpdHigh = as.numeric(baSumm$hpdHigh)
  
  baSummSelf = baSumm %>% filter(pop.i == pop.j) %>% mutate(regionDepth = paste(site.i,depth.i, sep="")) %>% mutate(retention = mean) %>% dplyr::select(-mean)
  
  sites = read.csv("regionalInds2popsNoClones.csv", header = TRUE) 
  sites$depthZone=as.factor(sites$depthZone)
  sites$depthZone = factor(sites$depthZone, levels = levels(sites$depthZone)[c(2,1)])
  #sites$region = factor(sites$region, levels = levels(sites$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])
  sitesAdj=sites %>% group_by(region, depthZone, regionDepth) %>% dplyr::summarise(latDD = mean(lat), longDD = mean(lon))%>% as.data.frame()
  #sitesAdj["rowName", "columnName"] <- value
  sitesAdj[2, 5] <- sitesAdj[2 , 5] + 0.5 #Move BLZ Meso over a bit
  sitesAdj[8, 5] <- sitesAdj[8 , 5] - 0.5 #Move FK Meso over a bit
  sitesAdj[5, 5] <- sitesAdj[5 , 5] + 0.5 #Move DRT Meso over a bit
  sitesAdj[14, 5] <- sitesAdj[14 , 5] - 0.5 #Move SGOM Meso over a bit
  #Paste pop depth before
  regionPopsMigrate = sitesAdj %>% left_join(dplyr::select(baSummSelf, regionDepth, retention))
  
  #regionPopsMigrate = fknmsPopsMigrate2[c(1,2,4,3,5:8),]
  
  # fknmsPopsMigrate = fknmsPopsMigrate2[-5,]
  
  migratePal = c("SEFL" = colPal[1], "FK" = colPal[2], "DRT" = colPal[3], "PRG" = colPal[4], "CUBA" = colPal[5], "NWGOM" = colPal[6], "SGOM" = colPal[7],"BLZ" = colPal[8])
  
  lines = c("Shallow" = 5, "Mesophotic" = 1)
  
  baMapData = dplyr::select(baSumm, -mean2) %>% left_join(dplyr::select(regionPopsMigrate,-retention,-regionDepth), by = c("site.i" = "region", "depth.i" = "depthZone")) %>% left_join(dplyr::select(regionPopsMigrate,-retention,-regionDepth), by = c("site.j" = "region", "depth.j" = "depthZone"), suffix = c(".i", ".j")) %>% filter(mean >= 0.05)
  
  for(x in 1:nrow(baMapData)) {
    if (baMapData$pop.i[x] == baMapData$pop.j[x]) {
      baMapData$latDD.i[x] = NA;
      baMapData$latDD.j[x] = NA;
      baMapData$longDD.i[x] = NA;
      baMapData$longDD.j[x] = NA;
      baMapData$mean[x] = NA;
      baMapData$median[x] = NA
    }
  }

  
  gomCountries = st_as_sf(ne_countries(country = c("United States of America", "Cuba", "Mexico", "The Bahamas", "Belize", "Guatemala", "Cayman Islands"), scale = "Large"))
  migrateMap = ggplot() +
    geom_sf(data = gomCountries, fill = "white", size = 0.25) +
    #geom_point(data = subset(sites, subset = sites$region %in% c("CUBA", "SEFL")), aes(x = lon, y = lat, shape = depthZone, fill=region), size = 3) +
    #geom_point(data = subset(reefs, subset = reefs$region %in% c("BLZ", "DRT", "FK", "NWGOM", "SGOM", "PRG")), aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
    #geom_point(data = reefs, aes(x = lon, y = lat, fill = region), size = 3, shape=21) +
    geom_curve(data = baMapData, aes(x = longDD.j, y = latDD.j, xend = longDD.i, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.01, "npc")), alpha = 0.7, na.rm = TRUE) +
    
    # #SHALLOW SOURCES
    # #ggtitle("Shallow sources") +
    # #NWGOM-S->BLZ-S
    # geom_curve(data = baMapData[2,], aes(x = longDD.j, y = latDD.j, xend = longDD.i-0.1, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE) +
    # #SEFL->BLZ-S
    # geom_curve(data = baMapData[3,], aes(x = longDD.j, y = latDD.j-0.2, xend = longDD.i+0.1, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature=-0.5) +
    # #NWGOM-S->DRT-S
    # geom_curve(data = baMapData[12,], aes(x = longDD.j, y = latDD.j+0.2, xend = longDD.i, yend = latDD.i+0.25, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature = -0.4) +
    # #DRT-S->FK-S
    # geom_curve(data = baMapData[14,], aes(x = longDD.j-0.2, y = latDD.j, xend = longDD.i, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), alpha = 0.7, na.rm = TRUE, curvature = 0.5) + #arrow = arrow(type = "open", length = unit(0.02, "npc"))
    # #DRT-S->SGOM-S
    # geom_curve(data = baMapData[18,], aes(x = longDD.j, y = latDD.j, xend = longDD.i, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature=-0.3) +
    # #NWGOM-S->SGOM-S
    # geom_curve(data = baMapData[20,], aes(x = longDD.j, y = latDD.j, xend = longDD.i, yend = latDD.i+0.2, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature=-0.3) +
    # 
    # #geom_curve(data = baMapData[3,], aes(x = longDD.j, y = latDD.j, xend = longDD.i, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE) +
    # # #MESO SOURCES
    # # #ggtitle("Mesophotic sources") +
    # #FK-M->BLZ-M
    # geom_curve(data = baMapData[5,], aes(x = longDD.j, y = latDD.j, xend = longDD.i+0.3, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature = -0.2) +
    # #FK-M->CUBA-M
    # geom_curve(data = baMapData[7,], aes(x = longDD.j, y = latDD.j, xend = longDD.i, yend = latDD.i+0.15, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE) +
    # #FK-M->DRT-M
    # geom_curve(data = baMapData[10,], aes(x = longDD.j, y = latDD.j, xend = longDD.i, yend = latDD.i+0.3, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature = 1) +
    # #DRT-M->FK-S
    # geom_curve(data = baMapData[13,], aes(x = longDD.j, y = latDD.j-0.3, xend = longDD.i, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature=1) +
    # #FK-M->FK-S
    # geom_curve(data = baMapData[16,], aes(x = longDD.j, y = latDD.j, xend = longDD.i+0.1, yend = latDD.i, color = site.j, linetype = depth.j, size = mean), arrow = arrow(type = "open", length = unit(0.02, "npc")), alpha = 0.7, na.rm = TRUE, curvature=-5) +

    scale_fill_manual(values = migratePal, name = "Region") +
    scale_color_manual(values = migratePal, guide = NULL) +
    scale_shape_manual(values = c(24, 25), name = "Depth") +
    #scale_size(range = c(0.5, 2), breaks = c(0.02,0.06,0.1,0.14,0.18,0.22),name = expression(paste("Migration (", italic("m"), ")", sep = "")), guide = guide_legend(ncol = 3, order = 5)) +
    scale_size(range = c(0.5, 2.5), breaks = c(.05, .1, .15, .2, .25),name = expression(paste("Migration (", italic("m"), ")", sep = "")), guide = guide_legend(ncol = 3, order = 5)) +
    new_scale("size") +
    geom_point(data = regionPopsMigrate, aes(x = longDD, y = latDD, fill = region, shape = depthZone, size = retention)) +#, size = 3.5) +
    # scale_size_binned(range = c(0.25, 1.5), breaks = seq(0.02,0.2, by = 0.04), name = expression(paste(italic("m")))) +
    scale_size(range = c(1.5, 6.5), limit = c(0.67, 0.87), name = expression(paste("Self-recruitment (",italic("m"), ")", sep ="")), breaks = c(0.67, 0.75, 0.8, 0.85, 0.87), 
               guide = guide_legend(override.aes = list(shape = 24, fill = "gray80"), ncol = 3, order = 3)) +
    scale_linetype_manual(values = lines, name = "Source depth", guide = guide_legend(order = 4)) +
    scale_shape_manual(values = c(24,25), name = "Depth Zone") +
    coord_sf(xlim = c(-99, -75), ylim = c(16, 30)) +
    scale_x_continuous(breaks = c(seq(-99, -75, by = 2))) +
    scale_y_continuous(breaks = c(seq(16, 30, by = 2))) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal()) +
    guides(fill = guide_legend(override.aes = list(shape = 22, color = NA, size = 4), order = 1), shape = guide_legend(override.aes = list(size = 3), order = 2)) +
    #guides(fill = guide_legend(override.aes = list(shape = 21, size = 4), order = 1))+ #,
    #shape = guide_legend(order=2),
    #color = guide_legend(title=NULL, order = 3))+ #override.aes = list(fill=NA),
    theme_bw() +
    theme(plot.title = element_text(size = 9) ,
          panel.background = element_rect(fill = "aliceblue"),
          plot.background = element_rect(fill = "white"),
          axis.title = element_blank(),
          legend.position = "right",
          axis.text = element_text(color="black"))
  
  #migrateMap
  
  ggsave("migrateMap.png", plot = migrateMap, width = 30, height = 20, units = "cm", dpi = 300)
  
  
  #Putting the plots together into a single figure panel
  ```{r, migrationFigure}
  migrationPlots = (migrate/(migrateMap) + theme(legend.box.margin = margin(-20, 0, 0, 0))) + plot_layout(heights = c(1, 1.1)) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 20))
  
  ggsave("../figures/figure6.png", plot = migrationPlots, width = 27, height = 25, units = "cm", dpi = 300)
  
  ggsave("../figures/figure6.svg", plot = migrationPlots, width = 27, height = 25, units = "cm", dpi = 300)
  
  ```
#Bayescan###############################################################################
  regionalPca=read.pcadapt("/Users/student/Documents/GitHub/regionalMcav/regionalMcavNoClones.bcf", type = "vcf")
  
  #choose a number of K more than any number it would be
  
  scree <- pcadapt(input = regionalPca, K = 20)
  plot(scree, option = "screeplot")
  
  #Look at the scree plot and select the number K to the left of the straight line, in this case K=4
  
  regionalPcadapt <- pcadapt(regionalPca, K = 7, min.maf=0) #The default minMaf is 0.05 but for some reason it still filters out SNPs (even though the dataset should already be filtered for minMaf so I set it to 0 so it wouldn't do that)
  summary(regionalPcadapt)
  head(regionalPcadapt)
  head(regionalPcadapt$pvalues)
  qval = p.adjust(regionalPcadapt$pvalues, method = "BH")
  qPcadapt=as.data.frame(qval)
  qPcadapt$locus = c(1:nrow(qPcadapt))
  outsPcadapt=which(qPcadapt[,"qval"]<0.1)
  
  
  source('plot_R.r')
  
  datReefDepth = read.table("regionalMcav.baye_fst.txt",header=T)
  head(datReefDepth)
  table(datReefDepth[,"qval"]<0.1)
  datReefDepth$locus = c(1:nrow(datReefDepth))
  outsReefDepth=which(datReefDepth[,"qval"]<0.1)
  plot_bayescan("regionalMcav.baye_fst.txt",FDR=0.05,add_text=F,size=0.5,highlight=outsReefDepth)
  outliersReefDepth= datReefDepth %>% filter(qval < 0.1)
  outsQvalReefDepth=dplyr::select(outliersReefDepth, qval, locus)
  
  
  datBayescenv = read.table("regionalBayescenv_fst.txt",header=T)
  head(datBayescenv)
  table(datBayescenv[,"qval_g"]<0.1)
  datBayescenv$locus = c(1:nrow(datBayescenv))
  outsBayescenv=which(datBayescenv[,"qval"]<0.1)
  plot_bayescan("regionalMcav.baye_fst.txt",FDR=0.05,add_text=F,size=0.5,highlight=outsReefDepth)
  outliersBayescenv= datBayescenv %>% filter(qval < 0.1)
  outsQvalBayescenv=dplyr::select(outliersBayescenv, qval, locus)
  
  ## SNPs potentially under selection
  genes = read.table("mcav_gene_regions.tab")
  names(genes) = c("chromo","start","end","gene")
  
  # expand gene regions ± 2000 bp
  genes$start = genes$start -2000
  genes$end = genes$end +2000
  
  gnames = read.table("mcav_cog.txt", sep = "\t")
  names(gnames) = c("gene", "cog", "protein")
  
  genes = full_join(genes, gnames, by = "gene")
  genes$protein=as.character(genes$protein)
  genes$protein[is.na(genes$protein)]="unknown"
  
  #how many annotated genes do we have?
  nrow(genes[genes$protein!="unknown",])
  head(genes)
  
  snpLoci = read.table("regionalMcavNoClones.mafs.gz", header = TRUE)
  snpLoci$locus = c(1:nrow(snpLoci))
  
  #########Annotated outliers by reef:depth
  outsByGenePcAdapt = snpLoci %>% dplyr::select(locus, chromo, position) %>%
    filter(locus %in% outsPcAdapt) %>%
    #filter(locus %in% outsQval) %>%
    full_join(., outsQvalPcAdapt, by = "locus") %>%
    full_join(., genes, by = "chromo") %>%
    filter(position>start, position<end)
  
  outsByGenePcAdapt = outsByGenePcAdapt[c(1:3,5:9,4)]
  
  for(i in 1:nrow(outsByGenePcAdapt)) {
    if (outsByGenePcAdapt$qval[i] < .01) {
      outsByGenePcAdapt$sig[i] = "***"
    } else {
      if (outsByGenePcAdapt$qval[i] < .05) {
        outsByGenePcAdapt$sig[i] = "**"
      } else {
        outsByGenePcAdapt$sig[i] = "*"
      }
    }
  }
  
  outsByGenePcAdapt
  
  #only show annotated genes by reef depth
  outsAnnoPcAdapt = outsByGenePcAdapt %>% filter(protein != "unknown")
  outsAnnoPcAdapt
  
  df_uniqPcAdapt <- unique(outsAnnoPcAdapt$locus)
  length(df_uniqPcAdapt)
  df_uniq1PcAdapt <- unique(outsAnnoPcAdapt$gene)
  length(df_uniq1PcAdapt)
  
  write.csv(x=outsAnnoPcAdapt, file="annotatedOutliersPcAdapt.csv")
 
  #GOMWU 
  mcavGo=read.table("Mcavernosa_gene2go.tab")
  names(mcavGo)=c("gene", "GO")
  
  genes = read.table("mcav_gene_regions.tab")
  names(genes) = c("chromo","start","end","gene")
  
  # expand gene regions ± 2000 bp
  genes$start = genes$start -2000
  genes$end = genes$end +2000
  
  #gnames = read.table("mcav_cog.txt", sep = "\t")
  #names(gnames) = c("gene", "cog", "protein")
  
  genes = full_join(genes, mcavGo, by = "gene")
  genes$GO=as.character(genes$GO)
  genes$protein[is.na(genes$GO)]="unknown"
  
  #how many annotated genes do we have?
  nrow(genes[genes$protein!="unknown",])
  head(genes)
  
  snpLoci = read.table("regionalMcavNoClones.mafs.gz", header = TRUE)
  snpLoci$locus = c(1:nrow(snpLoci))
  
  snpsByGene = snpLoci %>% dplyr::select(locus, chromo, position) %>%
    #filter(locus %in% highOuts) %>%
    full_join(., genes, by = "chromo") %>%
    filter(position>start, position<end, !is.na(GO))
  
  snpsByGene$outlier = 0
  for(i in 1:nrow(snpsByGene)){
    if(snpsByGene$locus[i] %in% outsPcAdapt){
      snpsByGene$outlier[i] = 1
    }else{
      snpsByGene$outlier[i] = 0
    }
  }
  
  snpsByGene$outlier
  
  input=snpsByGene %>% dplyr::select(gene, outlier)
  
  write.csv(input, file="input.csv", row.names = FALSE, quote = FALSE)
  # two columns of comma-separated values: gene id, To perform standard GO enrichment analysis based on Fisher's exact test, use binary measure (0 or 1, i.e., either sgnificant or not).
  
  # First, press command-D on mac or ctrl-shift-H in Rstudio and navigate to the directory containing scripts and input files. Then edit, mark and execute the following bits of code, one after another.
  
  
  # Edit these to match your data file names: 
  input="input.csv"
  #input="heats.csv" # two columns of comma-separated values: gene id, continuous measure of significance. To perform standard GO enrichment analysis based on Fisher's exact test, use binary measure (0 or 1, i.e., either sgnificant or not).
  goAnnotations="Mcavernosa_gene2go.tab"
  #goAnnotations="amil_defog_iso2go.tab" # two-column, tab-delimited, one line per gene, multiple GO terms separated by semicolon. If you have multiple lines per gene, use nrify_GOtable.pl prior to running this script.
  goDatabase="go.obo" # download from http://www.geneontology.org/GO.downloads.ontology.shtml
  goDivision="MF" # either MF, or BP, or CC
  source("gomwu.functions.R")
  
  
  # ------------- Calculating stats
  # It might take a few minutes for MF and BP. Do not rerun it if you just want to replot the data with different cutoffs, go straight to gomwuPlot. If you change any of the numeric values below, delete the files that were generated in previos runs first.
  
  GOterm=gomwuStats(input, goDatabase, goAnnotations, goDivision,
             perlPath="/usr/bin/perl", # replace with full path to perl executable if it is not in your system's PATH already
             largest=0.1,  # a GO category will not be considered if it contains more than this fraction of the total number of genes
             smallest=5,   # a GO category should contain at least this many genes to be considered
             clusterCutHeight=0.25, # threshold for merging similar (gene-sharing) terms. See README for details.
             #	Alternative="g" # by default the MWU test is two-tailed; specify "g" or "l" of you want to test for "greater" or "less" instead. 
             #Module=TRUE,Alternative="g" # un-remark this if you are analyzing a SIGNED WGCNA module (values: 0 for not in module genes, kME for in-module genes). In the call to gomwuPlot below, specify absValue=0.001 (count number of "good genes" that fall into the module)
             Module=TRUE # un-remark this if you are analyzing an UNSIGNED WGCNA module 
  )
  # do not continue if the printout shows that no GO terms pass 10% FDR.
  # ----------- Plotting results
  # 
  quartz()
  results=gomwuPlot(input, goAnnotations, goDivision,
   	absValue=-log(0.05,10),  # genes with the measure value exceeding this will be counted as "good genes". This setting is for signed log-pvalues. Specify absValue=0.001 if you are doing Fisher's exact test for standard GO enrichment or analyzing a WGCNA module (all non-zero genes = "good genes").
   #	absValue=1, # un-remark this if you are using log2-fold changes
   	level1=0.1, # FDR threshold for plotting. Specify level1=1 to plot all GO categories containing genes exceeding the absValue.
   	level2=0.05, # FDR cutoff to print in regular (not italic) font.
   	level3=0.01, # FDR cutoff to print in large bold font.
   	txtsize=1.2,    # decrease to fit more on one page, or increase (after rescaling the plot so the tree fits the text) for better "word cloud" effect
   	treeHeight=0.5, # height of the hierarchical clustering tree
   colors=c("dodgerblue2","firebrick1","skyblue2","lightcoral") # these are default colors, un-remar and change if needed
   )
   # manually rescale the plot so the tree matches the text
   # if there are too many categories displayed, try make it more stringent with level1=0.05,level2=0.01,level3=0.001.

   # text representation of results, with actual adjusted p-values
   results[[1]]
  # 
  # 
  # # ------- extracting representative GOs
  # 
  # this module chooses GO terms that best represent *independent* groups of significant GO terms

  pcut=1e-2 # adjusted pvalue cutoff for representative GO
  hcut=0.9 # height at which cut the GO terms tree to get "independent groups".

  # plotting the GO tree with the cut level (un-remark the next two lines to plot)
  # plot(results[[2]],cex=0.6)
  # abline(h=hcut,col="red")

  # cutting
  ct=cutree(results[[2]],h=hcut)
  annots=c();ci=1
  for (ci in unique(ct)) {
    message(ci)
  	rn=names(ct)[ct==ci]
  	obs=grep("obsolete",rn)
  	if(length(obs)>0) { rn=rn[-obs] }
  	if (length(rn)==0) {next}
  	rr=results[[1]][rn,]
  	bestrr=rr[which(rr$pval==min(rr$pval)),]
  	best=1
  	if(nrow(bestrr)>1) {
  		nns=sub(" .+","",row.names(bestrr))
  		fr=c()
  		for (i in 1:length(nns)) { fr=c(fr,eval(parse(text=nns[i]))) }
  		best=which(fr==max(fr))
  	}
  	if (bestrr$pval[best]<=pcut) { annots=c(annots,sub("\\d+\\/\\d+ ","",row.names(bestrr)[best]))}
  }

  mwus=read.table(paste("MWU",goDivision,input,sep="_"),header=T)
  bestGOs=mwus[mwus$name %in% annots,]
  bestGOs
#Zoox###################################################################################
  meta=read.csv("regionalInds2PopsNoClones.csv") # Reads in population data for each sample
  zoox = read.delim("zooxGenomeAlignmentRate", header = FALSE, check.names = FALSE)
  head(zoox)
  
  zoox$V2[is.na(zoox$V2)] <- as.character(zoox$V1[is.na(zoox$V2)])
  zoox$V1 = gsub(pattern = "*.trim.zoox.zooxOnly.bt2.bam", "chr", zoox$V1)
  zoox$V2 = gsub(".trim.*", "", zoox$V2)
  
  zoox = zoox %>% filter(zoox$V1 != "*")
  
  zooxLst = split(zoox$V2, as.integer(gl(length(zoox$V2), 20, length(zoox$V2))))
  
  zooxMaps = NULL
  for(i in zooxLst){
    zooxMaps = rbind(zooxMaps, data.frame(t(i)))
  }
  colnames(zooxMaps) = c("sample",zoox$V1[c(2:20)])
  
  for(i in c(2:20)){
    zooxMaps[,i] = as.numeric(levels(zooxMaps[,i]))[zooxMaps[,i]]
  }
  
  str(zooxMaps)
  
  zooxMaps$Symbiodinium = rowSums(zooxMaps[2:6])
  zooxMaps$Breviolum = rowSums(zooxMaps[7:10])
  zooxMaps$Cladocopium = rowSums(zooxMaps[11:16])
  zooxMaps$Durusdinium = rowSums(zooxMaps[17:20])
  
  zooxMaps = zooxMaps[,c(1, 21:24)]
  
  zooxMapsSums=zooxMaps
  zooxMapsSums$totalAlignments=rowSums(zooxMapsSums[2:5])
  mean(zooxMapsSums$Cladocopium)/mean(zooxMapsSums$totalAlignments)
  
  zooxProp = zooxMaps
  
  zooxProp$sum = apply(zooxProp[, c(2:length(zooxProp[1,]))], 1, function(x) {
    sum(x, na.rm = T)
  })
  
  zooxProp = cbind(zooxProp[, c(1)], (zooxProp[, c(2:(ncol(zooxProp)-1))]
                                      / zooxProp$sum))
  head(zooxProp)
  
  apply(zooxProp[, c(2:(ncol(zooxProp)))], 1, function(x) {
    sum(x, na.rm = T)
  })
  
  head(meta)
  
  dfZoox = cbind("sample" = zooxProp[,1], "region" = meta[,4], "depthZone" = meta[,6], "regionDepth" = meta[,10], zooxProp[,c(2:5)])
  #dfZoox$regionDepth=as.factor(dfZoox$regionDepth)
  #dfZoox$regionDepth = factor(dfZoox$regionDepth, levels = levels(dfZoox$regionDepth)[c(6,5,4,3,2,1,8,7)])
  
  
  dfZoox = dfZoox[order(dfZoox$regionDepth),]
  dfZoox$Order = c(1:nrow(dfZoox))
  
  zDat = melt(dfZoox, id.vars = c("sample", "region", "depthZone", "regionDepth", "Order"), variable.name = "Symbiont", value.name = "Fraction")
  colPalZoox = brewer.pal(4, "BrBG")
  names(colPalZoox) = levels(zDat$Symbiont)
  
  zooxPlotA = ggplot(data = zDat, aes(x = Order, y = Fraction, fill = Symbiont, order = Order)) +
    geom_bar(stat="identity", position="fill", width=1) +
    #geom_bar(stat = "identity", position = "stack", colour = "grey25", width = 1) +
    xlab("Population") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x*100, "%")) +
    scale_fill_manual(values = colPalZoox, name = "Symbiodiniaceae genus") +
    coord_cartesian(ylim = c(-.01,1.01)) +
    facet_grid(.~fct_inorder(regionDepth), drop=TRUE, scales = "free", switch = "x", space = "free") +
    theme_bw()
  
  zooxPlot = zooxPlotA + theme(plot.title = element_text(hjust = 0.5),
                               panel.grid=element_blank(),
                               panel.background=element_rect(fill=NA, colour="grey25"),
                               panel.spacing.x=grid:::unit(0, "lines"),
                               panel.border = element_rect(color="black", size=2, linetype="solid"),
                               axis.text.y=element_text(size=14),
                               axis.text.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.x=element_blank(),
                               strip.background=element_blank(),
                               strip.text=element_text(size=12, angle=90),
                               legend.key=element_blank(),
                               legend.position = "right",
                               #legend.title = element_blank(),
                               legend.text = element_text(face = "italic"))
  
  zooxPlot
  
  #PERMANOVA####################
  zooxGen = cbind("sample" = zooxMaps[,1], "region" = meta[,4], "depthZone" = meta[,6], "regionDepth" = meta[,10], sqrt(zooxMaps[,c(2:5)]))
  set.seed(694) #setting seed allows repetition of randomized processes
  
  zooxGenS = betadisper(vegdist(zooxGen[, c(5:ncol(zooxGen))]), zooxGen$region)
  
  anova(zooxGenS)
  #Significant effect of site on beta diversity
  
  set.seed(694)
  
  zooxGenD = betadisper(vegdist(zooxGen[, c(5:ncol(zooxGen))]), zooxGen$depthZone)
  
  anova(zooxGenD)
  #No significant effect of depth on beta diversity
  
  #PERMANOVA
  set.seed(694)
  its2Adonis = adonis(zooxGen[, c(5:ncol(zooxGen))] ~ region * depthZone, 
                      data = zooxGen, permutations = 99, method = "bray")
  
  its2Adonis
  
  set.seed(694)
  
  its2PWAdonis = pairwise.adonis(zooxGen[, c(5:ncol(zooxGen))],
                                 factors = zooxGen$region,
                                 sim.method = "bray", p.adjust.m = "BH", perm = 99)
  
  its2PWAdonis
  
########indicator spp.#################
  install.packages("indicspecies")
  indval = multipatt(wetland, groups, 
                     control = how(nperm=999)) 
#NMDS###################################
  #make community matrix - extract columns with abundance information
  com = (zooxMaps[,2:ncol(zooxMaps)]) #extracting community matrix
  m_com = as.matrix(com) #making it a matric
  set.seed(123)
  nmds = metaMDS(m_com, distance = "bray", try = 100) #Running the NMDS
  nmds
data.scores = as.data.frame(scores(nmds)) #extracting these values
data.scores = cbind("sample" = zooxMaps[,1], "region" = meta[,4], "depthZone" = meta[,6], "regionDepth" = meta[,10], data.scores) #binding metadata together
set.seed(694) #setting seed allows repetition of randomized processes
head(data.scores)
  
  zooxGenFit <- envfit(nmds, m_com, permutations = 999) #Adding indicator species vectors
  head(zooxGenFit)
  zooxSppScores <- as.data.frame(scores(zooxGenFit, display = "vectors")) #extracts relevant scores from envifit
  A <- as.list(zooxGenFit$vectors)
  pvals<-as.data.frame(A$pvals)
  arrows<-as.data.frame(A$arrows*sqrt(A$r))
  C<-cbind(arrows, pvals)
  Cred<-subset(C,pvals<0.01)
  Cred <- cbind(Cred, Species = rownames(Cred))
  
  data.scores = merge(data.scores, aggregate(cbind(mean.x = NMDS1, mean.y = NMDS2)~regionDepth, data.scores, mean), by="regionDepth") 
  
  #%>% merge(., aggregate(cbind(mean.x.K = dbRDA1, mean.y.K = dbRDA2)~K, regionalDbrdaData1, mean), by="K")
  
  data.scores$depthZone = factor(data.scores$depthZone)
  data.scores$depthZone = factor(data.scores$depthZone, levels(data.scores$depthZone)[c(2,1)])
  data.scores$region = factor(data.scores$region)
  data.scores$region = factor(data.scores$region, levels = levels(data.scores$region)[c(7, 4, 3, 6, 2, 5, 8, 1)])
  
  head(data.scores)
  
  colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
  zooxNMDSA = ggplot() +
    geom_hline(yintercept = 0, color = "gray90", size = 0.5) +
    geom_vline(xintercept = 0, color = "gray90", size = 0.5) +
    geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, fill = region, shape = depthZone), size = 2, stroke = NA, alpha = 0.5, show.legend = FALSE) +
    geom_segment(data = Cred,
                 aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "black") +
    scale_shape_manual(values = c(24,25), name = "Depth Zone") +
    # geom_segment(data = regionalDbrdaData, aes(x = mean.x, y = mean.y, xend = dbRDA1, yend = dbRDA2, color = pop), size = 0.5, alpha = 0.5) +
    #geom_segment(data = envLoad, aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2), 
                 #color = "black", arrow = arrow(length = unit(0.25, "cm"), type = "open"), size = 0.65) +
    #geom_text(data = envLoad, aes(x = dbRDA1, y = dbRDA2+0.1, label = var), color = "black", size = 3) +
    geom_point(data = subset(data.scores, region !=("NWGOM")), aes(x = mean.x, y = mean.y, fill = region, shape = depthZone), size = 2, stroke = NA, alpha = 0.5) +
    geom_point(data = subset(data.scores, region ==("NWGOM")), aes(x = mean.x, y = mean.y, fill = region, shape = depthZone), size = 5, color = "black") + #population centroids indicated by triangles
    #geom_text(data = Cred, aes(x = NMDS1, y = NMDS2, label = Species), size = 3)+
    scale_fill_manual(values = colPal, name = "Site") +
    scale_color_manual(values = colPal, name = "Site") +
    guides(shape = guide_legend(override.aes = list(size = 3.5, stroke = 0.5, alpha = 0.7), order = 2), fill = guide_legend(override.aes = list(shape = 22, size = 5, color = NA, alpha = NA), order = 1))+
    theme_bw()
  
zooxNMDS = zooxNMDSA +
    theme(axis.title.x = element_text(color = "black", size = 10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_text(color = "black", size = 10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "right",
          panel.border = element_rect(color = "black", size = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  zooxNMDS
  ggsave("zooxNMDSNwgom.tiff", plot = zooxNMDS, height = 5, width = 7, units = "in", dpi = 300)
  #Zoox PCoA
  zooxCom =(zooxMaps[,2:ncol(zooxMaps)])
  zooxDist=vegdist(zooxCom, method="bray")
  
  zooxMds = cmdscale(zooxDist, eig = TRUE, x.ret = TRUE)
  # Determine percent variation captured on each axis
  # Calculate the eigenvalues so later we can figure out % variation shown on each Principal Coordinate
  zooxPcoaVar = round(zooxMds$eig/sum(zooxMds$eig)*100, 1)
  zooxPcoaVar
  
  # Format data to plot
  zooxPcoaValues = zooxMds$points
  zooxPcoaValues
  zooxPcoaValues=data.frame(zooxPcoaValues)
  zooxPcoaValues = cbind("sample" = zooxMaps[,1], "region" = meta[,4], "depthZone" = meta[,6], "regionDepth" = meta[,10], zooxPcoaValues)
  colnames(zooxPcoaValues)[5] <- "PCo1"
  colnames(zooxPcoaValues)[6] <- "PCo2"
  
  zooxGenFitPcoa <- envfit(zooxMds, zooxCom, permutations = 999)
  head(zooxGenFitPcoa)
  zooxSppScoresPcoa <- as.data.frame(scores(zooxGenFitPcoa, display = "vectors")) #extracts relevant scores from envifit
  APcoa <- as.list(zooxGenFitPcoa$vectors)
  pvalsPcoa<-as.data.frame(APcoa$pvals)
  arrowsPcoa<-as.data.frame(APcoa$arrows*sqrt(APcoa$r))
  CPcoa<-cbind(arrowsPcoa, pvalsPcoa)
  CredPcoa<-subset(CPcoa,pvalsPcoa<0.01)
  CredPcoa <- cbind(CredPcoa, Species = rownames(CredPcoa))
  
  zooxPcoaValues = merge(zooxPcoaValues, aggregate(cbind(mean.x = PCo1, mean.y = PCo2)~regionDepth, zooxPcoaValues, mean), by="regionDepth") 
  
  #%>% merge(., aggregate(cbind(mean.x.K = dbRDA1, mean.y.K = dbRDA2)~K, regionalDbrdaData1, mean), by="K")
  
  zooxPcoaValues$depthZone = factor(zooxPcoaValues$depthZone)
  zooxPcoaValues$depthZone = factor(zooxPcoaValues$depthZone, levels(zooxPcoaValues$depthZone)[c(2,1)])
  zooxPcoaValues$region = factor(zooxPcoaValues$region)
  
  head(zooxPcoaValues)
  
  colPal = c(paletteer_d("vapeplot::vaporwave")[c(7,10,11,14,1,5,3)], "slateblue4")
  zooxPcoaA = ggplot() +
    geom_hline(yintercept = 0, color = "gray90", size = 0.5) +
    geom_vline(xintercept = 0, color = "gray90", size = 0.5) +
    geom_point(data = zooxPcoaValues, aes(x = PCo1, y = PCo2, fill = region, shape = depthZone), 
               size = 3, stroke = NA, alpha = 0.7, show.legend = FALSE) +
    scale_shape_manual(values = c(24,25), name = "Depth Zone") +
    # geom_segment(data = regionalDbrdaData, aes(x = mean.x, y = mean.y, xend = dbRDA1, yend = dbRDA2, color = pop), size = 0.5, alpha = 0.5) +
    #geom_segment(data = envLoad, aes(x = 0, y = 0, xend = dbRDA1, yend = dbRDA2), 
    #color = "black", arrow = arrow(length = unit(0.25, "cm"), type = "open"), size = 0.65) +
    #geom_text(data = envLoad, aes(x = dbRDA1, y = dbRDA2+0.1, label = var), color = "black", size = 3) +
    geom_point(data = zooxPcoaValues, aes(x = mean.x, y = mean.y, fill = region, shape = depthZone),
               size = 5, color = "black") + #population centroids indicated by triangles
    geom_segment(data = CredPcoa,
                 aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
                 arrow = arrow(length = unit(0.25, "cm")), colour = "grey") +
    geom_text(data = CredPcoa, aes(x = Dim1, y = Dim2, label = Species),
              size = 3)+
    scale_fill_manual(values = colPal, name = "Region") +
    scale_color_manual(values = colPal, name = "Region") +
    guides(shape = guide_legend(override.aes = list(size = 3.5, stroke = 0.5, alpha = 0.7), order = 2), fill = guide_legend(override.aes = list(shape = 22, size = 5, color = NA, alpha = NA), order = 1))+
    theme_bw()
  
  zooxPcoa = zooxPcoaA +
    theme(axis.title.x = element_text(color = "black", size = 10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_text(color = "black", size = 10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "right",
          panel.border = element_rect(color = "black", size = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  zooxPcoa
  
#Cuba Dendrogram#####################################################
  cubaCloneBams = read.table("cubaBams") # list of bam files
  
  cubaCloneMa = as.matrix(read.table("cubaREMcavClones.ibsMat")) # reads in IBS matrix produced by ANGSD 
  
  dimnames(cubaCloneMa) = list(cubaCloneBams[,1],cubaCloneBams[,1])
  cubaClonesHc = hclust(as.dist(cubaCloneMa),"ave")
  
  #cubaClonePops = cubaCloneBams$region
  #cubaCloneDepth = cubaCloneBams$depthZone
  
  cubaCloneDend = cubaCloneMa %>% as.dist() %>% hclust(.,"ave") %>% as.dendrogram()
  cubaCloneDData = cubaCloneDend %>% dendro_data()
  
  # Making the branches hang shorter so we can easily see clonal groups
  cubaCloneDData$segments$yend2 = cubaCloneDData$segments$yend
  for(i in 1:nrow(cubaCloneDData$segments)) {
    if (cubaCloneDData$segments$yend2[i] == 0) {
      cubaCloneDData$segments$yend2[i] = (cubaCloneDData$segments$y[i] - 0.01)}}
  
  cubaCloneDendPoints = cubaCloneDData$label
  #cubaCloneDendPoints$pop = cubaClonePops[order.dendrogram(cubaCloneDend)]
  #cubaCloneDendPoints$depth=cubaCloneDepth[order.dendrogram(cubaCloneDend)]
  rownames(cubaCloneDendPoints) = cubaCloneDendPoints$label
  
  
  # Making points at the leaves to place symbols for populations
  point = as.vector(NA)
  for(i in 1:nrow(cubaCloneDData$segments)) {
    if (cubaCloneDData$segments$yend[i] == 0) {
      point[i] = cubaCloneDData$segments$y[i] - 0.01
    } else {
      point[i] = NA}}
  
  cubaCloneDendPoints$y = point[!is.na(point)]
  factor(cubaCloneDendPoints$label)
  
  techReps = c("cuba_018-2", "cuba_018-3", "cuba_018RE", "cuba_018RS", "cuba_018-1", "cuba_044-2", "cuba_044-3", "cuba_044RE", "cuba_044RS", "cuba_044-1", "cuba_061-2", "cuba_061-3", "cuba_061RE", "cuba_061RS", "cuba_061-1")
  #cubaCloneDendPoints$depth = factor(cubaCloneDendPoints$depth,levels(cubaCloneDendPoints$depth)[c(2,1)])
  
  #cubaCloneDendPoints$pop = factor(cubaCloneDendPoints$pop,levels(cubaCloneDendPoints$pop)[c(4, 1, 3, 2)])
  
  flPal = paletteer_d("vapoRwave::jazzCup")[c(2:5)]
  
  cubaCloneDendA = ggplot() +
    geom_segment(data = segment(cubaCloneDData), aes(x = x, y = y, xend = xend, yend = yend2), size = 0.5) +
    geom_point(data = cubaCloneDendPoints, aes(x = x, y = y), size = 4, stroke = 0.25, fill = "#94D0FFFF", shape = 24) +
    #scale_fill_brewer(palette = "Dark2", name = "Population") +
    #scale_fill_manual(values = flPal, name= "Population")+
    #scale_shape_manual(values = c(24, 25), name = "Depth Zone")+
    #geom_hline(yintercept = 0.12, color = "red", lty = 5, size = 0.75) + # creating a dashed line to indicate a clonal distance threshold
    geom_text(data = subset(cubaCloneDendPoints, subset = label %in% techReps), aes(x = x, y = (y - .015), label = label), angle = 90) + # spacing technical replicates further from leaf
    geom_text(data = subset(cubaCloneDendPoints, subset = !label %in% techReps), aes(x = x, y = (y - .012), label = label), angle = 90) +
    labs(y = "Genetic distance (1 - IBS)") +
    guides(fill = guide_legend(override.aes = list(shape = 22)))+
    theme_classic()
  
  cubaCloneDend = cubaCloneDendA + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 12, color = "black", angle = 90),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.ticks.y = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill="white"),
    legend.key = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom")
  
  cubaCloneDend
  
  ggsave("cubaCloneDend.png", plot = cubaCloneDend, height = 10, width = 35, units = "in", dpi = 300)
  
  
