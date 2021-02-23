setwd("D:/IPS Amundsen 2018/UVP5")
library(data.table)
library(scatterpie)
library(tidyverse)
ecotax.df <-as.data.frame(fread("ecotaxa_export_1165_20190131_1439.tsv"))

#convert taxa to factors for subsetting and cleaning
data.cop <-subset(dat.df, object_annotation_category == "Copepoda")

ecotax.df$object_annotation_category <- as.factor(ecotax.df$object_annotation_category)

dat.df <- droplevels( ecotax.df[-which(ecotax.df$object_annotation_category %in% c("artefact", "badfocus<artefact", "darksphere", "detritus", "duplicate", "fiber<detritus", "othertocheck")),])



#add copepoda and copepod-like
levels(dat.df$object_annotation_category)[levels(dat.df$object_annotation_category) ==  "like<Copepoda"] <- "Copepoda"
#change ctenophora<metazoa to ctenophora
levels(dat.df$object_annotation_category)[levels(dat.df$object_annotation_category) ==  "Ctenophora<Metazoa"] <- "Ctenophora"
#add house to appendicularia
levels(dat.df$object_annotation_category)[levels(dat.df$object_annotation_category) ==  "house"] <- "Appendicularia"

#simple histogram by station
ggplot(dat.df, aes(sample_stationid, fill=object_annotation_category)) + geom_histogram(stat="count")

#remove station 6 -- no data
dat.df <- droplevels( dat.df[-which(dat.df$sample_stationid == "006"),])

#plot all samples
ggplot(dat.df, aes(object_annotation_category)) + geom_histogram(stat="count")  + theme(axis.text.x = element_text(angle = 45, hjust = 1)
#remove species with low sample size
dat.df <- droplevels( dat.df[-which(dat.df$object_annotation_category %in% c("Acantharea", "Crustacea", "Mollusca", "Siphonophorae", "Gnathostomata", "Cladocera")),])

#rearrange the site order so that they plot fron inshore to offshore

dat.df$station = factor(dat.df$sample_stationid, levels=c('005','004','003','002','001','float'))


#vertical distribution 

dat.df %>% filter(object_annotation_category %in% c("Copepoda", "Ostracoda", "Appendicularia")) %>%

ggplot(., aes(x=object_depth_max, fill= object_annotation_category)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=50,
                 colour="black", fill="white") + 
  geom_density(alpha=.2, fill="blue", bw=50) +  
  labs(x="Depth") +              
  facet_grid(station~object_annotation_category) +
                                              coord_flip() +
  theme_bw() +
                                                  scale_x_continuous(breaks=c(0,500,1000)) +
                                                          scale_x_reverse() +
                                                              scale_y_continuous(labels=function(n){format(n, scientific = T)}) +
                                                                  theme(strip.text.y = element_text(angle = 90),
                                                                   #     axis.text.x = element_text(angle=45, vjust=0.5)
                                                                        axis.text.x=element_blank()) 

#individual vertical plots

#copepod
{
  
data.cop <-subset(dat.df, object_annotation_category == "Copepoda")

  #histogram  
ggplot(data.cop, aes(x=object_depth_max)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=10,
                 colour="black", fill="#00BD5C") + 
                      geom_density(alpha=.2, fill="#00BD5C", bw=10) +  
                        labs(x="Depth", title= "Copepoda Vertical Density") +              
                          facet_grid(~station) +
                            coord_flip() +
                              scale_x_reverse() +
                                scale_y_continuous(labels=function(n){format(n, scientific = T)}) +
                                  theme_bw() +
                                    theme(axis.text.x = element_text(angle = 45, vjust=0.5)) 




ggplot(data.cop, aes(x=object_depth_max, y=bio_vol, col=station)) +  #stat_ecdf(pad=F, n=1000) +
 geom_col(width=10, col="black", fill="#00BD5C", alpha=0.7) +  
  labs(x="Depth (m)", y=expression(paste("Biovolume (", mu, "L)")), title= "Copepoda Biovolume") +             
  facet_grid(~station) +
  coord_flip() +
  scale_x_reverse()  +theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5)) 

y=expression(paste("Biovolume (", mu, "L)"))
                                                                  }
#appendicularia
{
data.app <-subset(dat.df, object_annotation_category == "Appendicularia")
  
  ggplot(data.app, aes(x=object_depth_max)) + 
    geom_histogram(aes(y=..density..),      
                   binwidth=10,
                   colour="black", fill="#00C1A7") + 
    geom_density(alpha=.2, fill="#00C1A7", bw=10) +  
    labs(x="Depth", title= "Appendicularia Vertical Density") +              
    facet_grid(~station) +
    coord_flip() +
    scale_x_reverse() +
    scale_y_continuous(labels=function(n){format(n, scientific = T)}) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust=0.5)) 
  
#Chaetognatha
  {
data.che <-subset(dat.df, object_annotation_category == "Chaetognatha")

    ggplot(data.che, aes(x=object_depth_max)) + 
      geom_histogram(aes(y=..density..),      
                     binwidth=10,
                     colour="black", fill="#AEA200") + 
      geom_density(alpha=.2, fill="#AEA200", bw=10) +  
      labs(x="Depth", title= "Chaetognatha Vertical Density") +              
      facet_grid(~station) +
      coord_flip() +
      scale_x_reverse() +
      scale_y_continuous(labels=function(n){format(n, scientific = T)}) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, vjust=0.5)) 
    }
#Ostracoda
  {
data.ost <-subset(dat.df, object_annotation_category == "Ostracoda")

ggplot(data.ost, aes(x=object_depth_max)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=10,
                 colour="black", fill="#00BADE") + 
  geom_density(alpha=.2, fill="#00BADE", bw=10) +  
  labs(x="Depth", title= "Ostracoda Vertical Density") +              
  facet_grid(~station) +
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(labels=function(n){format(n, scientific = T)}) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5)) 
  }
 
  
#Phaeosphaerida 
data.pha <-subset(dat.df, object_annotation_category == "Phaeosphaerida")
ggplot(data.pha, aes(x=object_depth_max)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=10,
                 colour="black", fill="#FF63B6") + 
  geom_density(alpha=.2, fill="#FF63B6", bw=10) +  
  labs(x="Depth", title= "Phaeosphaerida Vertical Density") +              
  facet_grid(~station) +
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(labels=function(n){format(n, scientific = T)}) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))  

#Diversity index

library(vegan)
dat.div <- diversity(df_2[,4:21], base=10)
 
#create a tallied df -- numbers of occurence (freq) and biovolume (bv) of each taxa
count.df <- dat.df %>%
            group_by(station, object_annotation_category, object_lon, object_lat) %>%
              dplyr::summarize(freq = n() , bv = sum(bio_vol))

#spread the frequency across taxa for geom_scatterpie() plotting
df_2 <- count.df %>% 
  tidyr::spread(key=object_annotation_category, value= freq)

#spread the biovolume across taxa for geom_scatterpie() plotting 
df_2 <- count.df %>% 
  tidyr::spread(key=object_annotation_category, value= bv)
  
bv_total <- count.df %>%
        group_by(station) %>%
          dplyr::summarize(bv_tot = sum(bv))

df_2 <- df_2 %>%
        left_join(., bv_total, by = c("station"))


df_2[is.na(df_2)] <- 0
df_2$sample_stationid <- as.factor(df_2$sample_stationid) # not need for dat.df
df_2[,2:15] <- lapply(df_2[,2:15], as.numeric)

#Diversity index

library(vegan)
dat.div <- diversity(df_2[,4:15], base=exp(1))
df_2$div <- dat.div
#plot div
ggplot(df_2, aes(x=station, y=div)) + geom_col()

df_2$Total <- rowSums(df_2[,4:15])

#move two stations slightly to make the pies more visible

df_2$object_lon[df_2$object_lon == -63.7835] <- -64 #move station 5 west
df_2$object_lon[df_2$object_lon == -63.56400] <- -63.4 #move station 4 east

require(raster)
require(marmap)
require(rgdal)
bath <- readGEBCO.bathy("C:/GIS/RN-8929_1548982716590/GEBCO_2014_2D_-68.4704_64.7194_-60.8023_69.7138.nc")
bath.f <- fortify.bathy(bath)

coast <- readOGR("C:/GIS/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")
#define extent (raster)
coast.trim <- crop(coast, extent(-65, -59, 66, 70))

brk <- c(-50,-100,-250,-500,-750,-1000,-1250,-1500,-2000,-2500)   #define bathymetry breaks


ggplot() +

  geom_contour(data = bath.f,                       #bathymetry
               aes(x=x, y=y, z=z),
               breaks=brk,
               size=c(0.3),
               colour="lightsteelblue2") +
    
    geom_polygon(data= coast.trim,                      #coast
                 aes(x=long, 
                     y=lat,
                     group=group),
                 fill= "navajowhite1", 
                 colour ="dodgerblue4",
                 inherit.aes = F) +
  
         geom_scatterpie(data = df_2, 
                      aes(object_lon, object_lat, r=log(1+bv_tot)/20), #r=log(1+bv_tot)/20 <- for biovol Total/1000 <- for abundance
                      cols = c("Copepoda","Ctenophora", "Ostracoda", "Chaetognatha",
                               "Appendicularia", "Aulacanthidae", "Coelodendridae", 
                               "Collodaria", "Hydrozoa", "Phaeosphaerida","Rhizaria"), 
                      alpha = 0.8) + 
                            scale_fill_manual( values = c("Appendicularia" = "#00C1A7",
                                                           "Aulacanthidae" = "#B385FF",
                                                           "Chaetognatha" = "#AEA200",
                                                           "Coelodendridae" = "#F8766D",
                                                           "Collodaria" = "#DB8E00",
                                                           "Copepoda" = "#00BD5C",
                                                           "Ctenophora" = "#00A6FF",
                                                           "Hydrozoa" = "#EF67EB",
                                                           "Ostracoda" = "#00BADE",
                                                           "Phaeosphaerida" = "#FF63B6",
                                                           "Rhizaria" = "#64B200")) +
#  geom_scatterpie_legend(df_2$Total/1000, x=-64.25, y=69.25) +
   #     annotate("text", x= -63.5, y=68.9, label= expression(paste("Abundance x", 10^-3))) +
  
           geom_scatterpie_legend(log(1+df_2$bv_tot)/20, x=-64.25, y=69.25) +
                            annotate("text", x= -63.35, y=68.8, label= expression(paste("Log Biovolume (",mu,"L/20)"))) +
  
            ylim(66, 69.75) + xlim(-65, -60) +
  
            labs(x="Longitude", y="Latitude", fill = "Taxa", title = "UVP5 Zooplankton Biodiversity") +
    
            coord_equal() +

                        theme(panel.grid.major =element_line(colour = "snow3",size=0.5),         #theme
                              panel.grid.minor = element_line(colour = "snow3",size=0.5), 
                              plot.title = element_text(hjust=0.5),
                              text = element_text(size=15), 
                              legend.direction = "horizontal",
                              legend.position = "bottom",
                              legend.background = element_blank(),
                              legend.text = element_text(size=10),
                              legend.title = element_text(face="bold"),
                              panel.background = element_blank(),
                              axis.line = element_line(colour = "white"),
                              axis.text.y = element_text(size=10), 
                              axis.text.x = element_text(size=10),
                              axis.title.x = element_text(size=10),
                              axis.title.y = element_text(size=10),
                              panel.ontop = F) 

                               
1    #00C1A7  Appendicularia
148  #B385FF  Aulacanthidae
184  #AEA200  Chaetognatha
190  #F8766D  Coelodendridae
221  #DB8E00  Collodaria
234  #00BD5C  Copepoda
247  #00A6FF  Ctenophora
358  #EF67EB  Hydrozoa
458  #00BADE  Ostracoda
743  #FF63B6  Phaeosphaerida
1075 #64B200  Rhizaria
  


  library(wesanderson)
install.packages("wesanderson")

#Biovolume

#pixel size in micrometer (um)
pix.um <- unique(ecotax.df$process_pixel)

#Ellipsoid biovolume
#Spherical Volume = V (mm3) = 4/3 x ??? x [ (Major(mm)/2) x (Minor(mm)/2) x (Minor(mm)/2) ] units (mm^3)
data.cop$sphere_vol <- 4/3 * pi * ((data.cop$object_major*pix.um/2)*(data.cop$object_minor*pix.um/2)*(data.cop$object_minor*pix.um/2))
data.cop$bio_vol <- (data.cop$sphere_vol)/(data.cop$acq_volimage) #mm^3/L
plot(data.cop$object_area, data.cop$object_depth_min)

#same but with whole dataset
dat.df$sphere_vol <- 4/3 * pi * ((dat.df$object_major*pix.um/2)*(dat.df$object_minor*pix.um/2)*(dat.df$object_minor*pix.um/2))
dat.df$bio_vol <- (dat.df$sphere_vol)/(dat.df$acq_volimage) #mm^3/L
plot(dat.df$object_area, dat.df$object_depth_min)


#trim the two outliers
data.cop <- subset(data.cop, object_area < 1000)

ggplot(data.cop, aes(x=bio_vol, y=object_depth_max)) + geom_point() +  + facet_wrap(~station) + scale_y_reverse()


+ geom_path(aes(group=object_depth_max)) 

ggplot(data.cop, aes(x=bio_vol, y=object_depth_min)) + geom_bar()

#calculate distance between points
install.packages("geosphere")
library(geosphere)

data.cop <- data.cop %>%
              arrange(factor(station)) %>%
                    mutate(.,
                         distance = distHaversine(cbind(object_lon, object_lat),  
                                                  cbind(lag(object_lon), lag(object_lat))))
data.cop$max <- data.cop %>%
              group_by(station) %>%
              summarise_all(funs(max))

library(data.table)
max <- setDT(data.cop)[, list(max=max(distance)), by = station] 
temp <- merge(data.cop, max, by='station')

cum.dist <- unique(temp$max.y)
cum.dist[is.na(cum.dist)] <- 0
cum.dist <- cumsum(cum.dist)


temp <- temp %>% mutate(cum.dist = cummax(as.numeric(factor(max.y, levels = unique(max.y)))))

lat <- unique(data.cop$object_lat)
lon <- unique(data.cop$object_lon)
pos.dat <- cbind(lon, lat)

dist.mat <- as.data.frame(distm(x=(pos.dat), y=(pos.dat), fun=distHaversine)/1000)
station <- unique(data.cop$station)
dist.mat <- cbind(dist.mat, station)

dist.mat <- dist.mat[,-c(2:6)]
colnames(dist.mat)[1] <- "dist.km"

temp <- merge(data.cop, dist.mat, by='station')

#log transform bio_vol to normalize
mean(data.cop$bio_vol)
temp$bio_vol_log <- log(1+temp$bio_vol/mean(temp$bio_vol))

#Multilevel B-spline interpolation
library(colorRamps)
library(MBA)
mba <- mba.surf(temp[,c('dist.km', 'object_depth_min', 'bio_vol')], 250, 250)
dimnames(mba$xyz.est$z) <- list(mba$xyz.est$x, mba$xyz.est$y)
df3 <- melt(mba$xyz.est$z, varnames = c('dist.km', 'object_depth_min'), value.name = 'bio_vol')

  ggplot(data=df3, aes(dist.km, object_depth_min))+
     geom_raster(aes(fill = bio_vol), interpolate = F, hjust = 0.5, vjust = 0.5) +
        geom_contour(aes(z = bio_vol)) + 
          geom_point(data = temp, aes(dist.km, object_depth_min), size=0.1, colour = 'white') +
            scale_y_reverse() +
              scale_fill_gradientn(colours = matlab.like(8)) +
                  labs(x="Distance (km)", y= "Depth (m)", fill="Biovolume (log microliters)", title="Copepod Biovolume") +
                    theme(panel.grid.major =element_line(colour = "snow3",size=0.5),         #theme
                          panel.grid.minor = element_line(colour = "snow3",size=0.5), 
                          plot.title = element_text(hjust=0.5),
                          text = element_text(size=15), 
                          legend.direction = "horizontal",
                          legend.position = "bottom",
                          legend.background = element_blank(),
                          legend.text = element_text(size=10),
                          legend.title = element_text(size=10, face="bold"),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "white"),
                          axis.text.y = element_text(size=10), 
                          axis.text.x = element_text(size=10),
                          axis.title.x = element_text(size=10),
                          axis.title.y = element_text(size=10),
                          panel.ontop = F) +
                              annotate("text", fontface="bold", x=3, y=500, label= "Green Edge", angle=270, col="black") + 
                              annotate("text", fontface="bold",x=17, y=500, label = "Station 004", angle=270, col="black") +
                              annotate("text", fontface="bold",x=80, y=500, label = "Station 003", angle=270, col="black") +
                              annotate("text", fontface="bold",x=120, y=500, label = "Station 002", angle=270, col="black") +
                              annotate("text", fontface="bold",x=175, y=500, label = "Station 001", angle=270, col="black") +
                              annotate("text", fontface="bold",x=233, y=500, label = "Argo Float", angle=270, col="black")
                              
    
    

#cubic spline interpolation
library(akima)
  interpdf <- interp2xyz(interp(x=temp$dist.km, 
                                y=temp$object_depth_max,
                                z=temp$bio_vol_log, 
                                  duplicate = "mean",
                                  data.frame = T) 
                         
                         interpdf %>%
                           tbl_df() %>%
                           ggplot(aes(x = x, y = y, z = z, fill = z)) +               #interpolation
                           geom_tile(aes(fill=z)) + 
                           scale_fill_distiller(name= "Biovolume (log)", #change 
                                                type="seq", #change type (div, seq) 
                                                palette="Spectral", #change palette (RdBu, Spectral)
                                                direction =-1, 
                                                na.value = "transparent") +
                         
                                  geom_point(data = temp, aes(dist.km, object_depth_max), size=0.1, colour = 'white') +
                                      scale_y_reverse() 
                           
                                
#DMS vertical plot
                         
dms.dat <- read.csv("IPS_2018_DMS.csv")


ggplot(dms.dat, aes(x=DMS.concentration..nM., y=depth..m., color=Station)) + 
          geom_point(size=2) + 
            geom_line(size=1) + 
              scale_y_reverse() +
                  scale_color_brewer(palette ="Spectral") + 
                      labs(y="Depth (m)", x= "DMS Concentration (nM)", title="DMS by Station") +
                            theme(panel.grid.major =element_line(colour = "snow3",size=0.5),         #theme
                                  panel.grid.minor = element_line(colour = "snow3",size=0.5), 
                                  plot.title = element_text(hjust=0.5),
                                  text = element_text(size=15), 
                                  legend.direction = "vertical",
                                  legend.position = "right",
                                  legend.background = element_blank(),
                                  legend.text = element_text(size=10),
                                  legend.title = element_text(face="bold"),
                                  panel.background = element_blank(),
                                  axis.line = element_line(colour = "white"),
                                  axis.text.y = element_text(size=10), 
                                  axis.text.x = element_text(size=10),
                                  axis.title.x = element_text(size=10),
                                  axis.title.y = element_text(size=10),
                                  panel.ontop = F) 



  