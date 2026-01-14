require(plyr)
library(tidyverse)
library(ggplot2)
library(data.table)
#library(ggcorrplot)
library(plotly)

library(sp)
###library(geoR)
#library(RandomFields)
###
library(gstat)
library(cluster)
# library(dplyr)
library(raster)   
library(e1071)
# library(geosphere)
#library(rgdal)
# library(rgeos)
# library(reshape2)
library(RColorBrewer)
# library(rasterVis)
#library(png)
#library(zoo)
library(fpc)
library(entropy)
library(RANN)
#library(stringr)
library(latex2exp)
library(sf)

unloadNamespace("ggtern") # for compitabity

options(scipen=999)

#uploadDir <- 'uploads/'
uploadDir <- 'uploads/'
polygonsDir <- 'polygons/'
stagingDir <- 'staging/'
projectsDir <- 'projects/'

h.theme <- theme(plot.subtitle = element_text(hjust=1,size=12),plot.caption = element_text(hjust=1,size=12),panel.background = element_rect(NA),panel.grid = element_line(size=0.1,colour = "grey80"),panel.border = element_rect(fill = NA))


# xFile="/home/assafi/Desktop/gen_7 raw blank - TEST.csv"
# df <- data.table(read.delim(xFile, header = T, sep=","))
# df.clean<-df[rowSums(is.na(df[,3])) == 0,]
# x=data.table(xdat)
# x<-x[rowSums(is.na(x[,3])) == 0,]
# print(x,topn=5,row.names = FALSE)

### settings
# coordinates reference system codes
wgs84 <- "+proj=longlat + ellps=WGS84"
#utm <- "+init=epsg:32629" #"+proj=utm +zone=36N +datum=WGS84"
utm <- "+init=epsg:32636"




ECaMaps <- function(xFile) {
  #xFile=fFile='30.12gadsh.h.xyz'
  x <- loadECa(xFile)

  
  xl <- x %>% 
  #  filter(tolower(Sample) != "blank") %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "wavelength",
      names_prefix = "",
      values_to = "value" #,
      #values_drop_na = TRUE
    ) %>% dplyr::mutate(wavelength = as.numeric(str_replace(wavelength, "X", "")))

  
  if(group=="sample"){
    xl$Sample <- factor(xl$Sample, levels = unique(xl$Sample))  # preserve order in plot    
    xl<-xl %>% 
      group_by(Sample,wavelength) %>%
      summarize(value = mean(value))
    legendtitle <- list(yref='paper',xref="paper",y=0.98,x=0.99, text="Sample",showarrow=F)
    return(xl %>% plot_ly(x = ~wavelength, y = ~value, color = ~Sample) %>% add_lines() %>% layout(title ="Samples - Spectrum", annotations=legendtitle))
  }
  
  # plot individual samples
  xl$Well <- factor(xl$Well, levels = unique(xl$Well))  # preserve order in plot    
  legendtitle <- list(yref='paper',xref="paper",y=0.98,x=0.99, text="Well",showarrow=F)
  return (xl %>% group_by(Well) %>% plot_ly(x = ~wavelength, y = ~value, color = ~Well) %>% add_lines() %>%
          layout(title ="Replicates - Spectrum", annotations=legendtitle ))
}



getFiles <- function(dir) {
  flist <- as.list(list.files(dir))
  txt=''
  j=1
  # <a href='' onclick='downloadCSV(this);return false;'><i class='fas fa-download'></i></a>
  for(f in flist){
    txt <- paste(txt, paste0("<p id='a",j,"'>",
                                
                              # <a href='' onclick='editName(this);return false;'><i class='far fa-edit'></i></a>  
                              # <a href='' onclick='deleteFile(this);return false;'><i class='fas fa-minus-circle delicon'></i></a> 
                              # <span class='newname'>
                              #   <input type='text' class='newval' value='",f,"'/>
                              #   <button onclick='cancelEdit()'>Cancel</button>
                              #   <button onclick='renameFile(this)'>Rename</button>
                              # </span>
                              # <span class='delfile'>
                              #  <button onclick='cancelEdit()'>Cancel</button>
                              #  <button onclick='approveDelete(this)' class='fred'>Delete</button>
                              # </span>
                              "<span class='fname'>",f,"</span>
                              <span class='fpath'>",paste0(uploadDir,"/",f),"</span>
                             </p>"))
    j=j+1
  }
  return(txt)
}




getPolygonFiles <- function(dir) {
  flist <- as.list(list.files(dir,pattern="*.shp"))
  txt=''
  j=1
  for(f in flist){
    txt <- paste(txt, paste0("<p id='a",j,"'>",
                              # <a href='' onclick='deleteFile(this);return false;'><i class='fas fa-minus-circle delicon'></i></a> 
                              # <span class='delfile'>
                              #  <button onclick='cancelEdit()'>Cancel</button>
                              #  <button onclick='approveDelete(this)' class='fred'>Delete</button>
                              # </span>
                             "<span class='fname'>",f,"</span>
                              <span class='fpath'>",paste0(polygonsDir,f),"</span>
                             </p>"))
    j=j+1
  }
  return(txt)
}




getProjects <- function(dir) {
  plist <- as.list(list.dirs(projectsDir, recursive=FALSE,full.names = F))
  txt=''
  j=1
  for(pr in plist){
    flist <- list.files(paste0(projectsDir,pr,"/"))
    txt <- paste(txt, paste("<p id='p",j,"'>",
                              # <a href='' onclick='editName(this);return false;'><i class='far fa-edit'></i></a>  
                              # <span class='newname'>
                              #   <input type='text' class='newval' value='",pr,"'/>
                              #   <button onclick='cancelEdit()'>Cancel</button>
                              #   <button onclick='renameFile(this)'>Rename</button>
                              # </span>
                              "<span class='pname' onclick='toggleExpand(this);return false;'>",pr,"</span>"))
     for(fl in flist){
       txt <- paste(txt,"<p class='flname'>",
                              # <a href='' onclick='deleteFile(this);return false;'><i class='fas fa-minus-circle delicon'></i></a> 
                              # <span class='delfile'>
                              #   <button onclick='cancelEdit()'>Cancel</button>
                              #   <button onclick='approveDelete(this)' class='fred'>Delete</button>
                              # </span>
                              "<span class='fname'>",fl,"</span>
                              <span class='fpath'>",paste0(projectsDir,pr,"/",fl),"</span>
                    </p>")
     }
    txt <- paste(txt,"</p>")
    
    j=j+1
    
  }
  return(txt)
}



### js functions in /srv/proexp/www/app.js






compactEca <- function(df, c){
  c<-as.numeric(c)
  df<-as.data.frame(df)
  df %>% dplyr::group_by(G=(0:(n()-1))%/%c) %>%  dplyr::summarise(
                    x=min(x),y=min(y),
                    ECa.1m=mean(eval(parse(text=names(df[3])))),
                    ECa.05m=mean(eval(parse(text=names(df[5])))),
                    Elevation=min(Elevation) ) %>% 
    dplyr::select(-G)
}
#compactEca(df,10)




ecaHist <- function(rawFile,compactFactor,trimLeft,trimRight,log=F) {
  #rawFile="30.12gadsh.v.csv"
  df <- loadECa(rawFile,"space")
  
  # Trim by lower and upper bound
  trimRight<-as.numeric(trimRight)
  trimLeft<-as.numeric(trimLeft)
  df <- as.data.frame(df[df[,3]<quantile(df[,3],trimRight) & df[,3]>quantile(df[,3],trimLeft),])
  
  if(log){ # log transform
    df[3]<-log(df[3])
    df[5]<-log(df[5])
  }
  
  # Compact data
  df<-compactEca(df,compactFactor)
  sk1<-round(skewness(df[[3]]),3)
  sk05<-round(skewness(df[[5]]),3)
  N=nrow(df)

  h.theme <- theme(plot.subtitle = element_text(hjust=1,size=12),plot.caption = element_text(hjust=1,size=12),panel.background = element_rect(NA),panel.grid = element_line(size=0.1,colour = "grey80"),panel.border = element_rect(fill = NA))
  
  if(log){ binw=0.02 } else { binw=0.5 }

 hist.eca1<-ggplot(df, aes_string(names(df)[3])) + geom_histogram(binwidth=binw,fill="lightsalmon",col="mediumpurple4") + 
      geom_vline(xintercept=mean(df[[3]]),lty=3)+
    geom_density(aes(y=..count../2),fill="wheat",col="purple4", alpha=0.3,lwd=1) + 
    labs(subtitle=paste("skewness ",sk1),caption = paste0("N=",N),y="Count") + h.theme +
      stat_function(
        fun = function(x, mean, sd, n){n * dnorm(x = x, mean = mean, sd = sd)},
        args = with(df, c(mean = mean(df[[3]]), sd = 3, n=(N/2))),lty=3) +
      xlim(min(df[3])-(min(df[3])*0.1) ,max(df[3])+(max(df[3])*0.1))
  
    hist.eca05<-
      ggplot(df, aes_string(names(df)[4])) + geom_histogram(binwidth=binw,fill="lightsalmon",col="mediumpurple4") + 
      geom_vline(xintercept=mean(df[[4]]),lty=3)+
      geom_density(aes(y=..count../2),fill="wheat",col="purple4", alpha=0.3,lwd=1) + 
      labs(subtitle=paste("skewness ",sk05),caption = paste0("N=",N),y="Count") + h.theme +
      stat_function(
        fun = function(x, mean, sd, n){n * dnorm(x = x, mean = mean, sd = sd)},
        args = with(df, c(mean = mean(df[[4]]), sd = 3, n=(N/2))),lty=3) +
      xlim(min(df[4])-(min(df[4])*0.1) ,max(df[4])+(max(df[4])*0.1))
    
#  hist.eca05<-ggplot(df, aes_string(names(df)[5])) + geom_histogram(bins=100) + labs(subtitle=paste("skewness ",sk05),caption = paste0("N=",N)) + h.theme
  multiplot(hist.eca1,hist.eca05,cols=2)
}



ecaPoints <- function(rawFile,compactFactor,trimLeft,trimRight,log=F) {
  #rawFile="30.12gadsh.h.csv"
  df <- loadECa(rawFile,"space")
  
  # Trim by lower and upper bound
  trimRight<-as.numeric(trimRight)
  trimLeft<-as.numeric(trimLeft)
  df <- as.data.frame(df[df[,3]<quantile(df[,3],trimRight) & df[,3]>quantile(df[,3],trimLeft),])
  
  if(log){ # log transform
    df[3]<-log(df[3])
    df[5]<-log(df[5])
  }
  
  # Compact data
  df<-compactEca(df,compactFactor)
  N=nrow(df)
  
  ###
  sp<-df
  coordinates(sp) = ~x + y
  crs(sp) <- CRS(utm)
  
  # Define the grid extent (perimeter + 5m)
  x.range <- as.numeric(c(round(extent(sp)[1]-50), round(extent(sp)[2]+50)))
  y.range <- as.numeric(c(round(extent(sp)[3]-50), round(extent(sp)[4]+50)))
  
  # Expand points to grid
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 1), y = seq(from = y.range[1], to = y.range[2], by = 1))  
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  crs(grd) <- CRS(utm)
  
  palp <- colorRampPalette(brewer.pal(9,"OrRd"))(20)
  
  points.eca.1 <- ggplot(df,aes(x,y)) +
    geom_point(aes_string(col=names(df[3])),alpha=1,size=1) + coord_equal() + theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5,size=12)) + 
    labs( y="Northing", x="Easting", title=paste0(names(df[3])), caption = "") + 
    scale_color_gradientn(name="mS/m",colors=palp) + #,limits=c(min.eca,max.eca)) +
    theme(legend.position="right",plot.title = element_text(hjust = 0.5,size=12),
          plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=10),
          panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey30"),
          panel.grid.minor = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.2, fill = NA),
          panel.ontop = TRUE, axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank()) 
  
  points.eca.05 <- ggplot(df,aes(x,y)) +
    geom_point(aes_string(col=names(df[4])),alpha=1,size=1) + coord_equal() + theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5,size=12)) + 
    labs( y="Northing", x="Easting", title=paste0(names(df[4])), caption = "") + 
    scale_color_gradientn(name="mS/m",colors=palp) + #,limits=c(min.eca,max.eca)) +
    theme(legend.position="right",plot.title = element_text(hjust = 0.5,size=12),
          plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=10),
          panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey30"),
          panel.grid.minor = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.2, fill = NA),
          panel.ontop = TRUE, axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank()) 
  
  multiplot(points.eca.1,points.eca.05,cols=2)
}





ecaKrige <- function(rawFile,compactFactor=30,trimLeft=0,trimRight=1,plotType="variogram",perimeterFile="potato_2025",log=F) {

  #rawFile="30.12gadsh.h.csv"
  #rawFile="TEST  EM38 h.csv"
  df.full <- loadECa(rawFile,"space")
  
  # Trim by lower and upper bound
  trimRight<-as.numeric(trimRight)
  trimLeft<-as.numeric(trimLeft)
  df.full <- as.data.frame(df.full[df.full[,3]<quantile(df.full[,3],trimRight) & df.full[,3]>quantile(df.full[,3],trimLeft),])
  
  if(log){ # log transform
    df.full[3]<-log(df.full[3])
    df.full[5]<-log(df.full[5])
  }
  
  # Compact data
  df<-compactEca(df.full,compactFactor)
  N=nrow(df)
  
  sp<-df
  coordinates(sp) = ~x + y
  crs(sp) <- CRS(utm)
  
    # Semi-Variogram
  v.eca1 = variogram(eval(parse(text=names(df[3])))~1, sp)
  v.eca05 = variogram(eval(parse(text=names(df[4])))~1, sp)
  
  # Fit function to variogram
  fit.eca1<-fit.variogram(v.eca1, vgm(c("Exp", "Mat", "Sph")))
  fit.eca05<-fit.variogram(v.eca05, vgm(c("Exp", "Mat", "Sph")))
  
  if(plotType=="variogram"){
  range.eca1<-round(fit.eca1[2,3],2)
  range.eca05<-round(fit.eca05[2,3],2)
  
  # Plot each variogram
  params1<-paste0("model: ",fit.eca1[2,1],"    nugget: ",round(fit.eca1[1,2],2),"   sill: ",round(fit.eca1[2,2],2),"   range: ",range.eca1," meters")
  params05<-paste0("model: ",fit.eca05[2,1],"    nugget: ",round(fit.eca05[1,2],2),"   sill: ",round(fit.eca05[2,2],2),"   range: ",range.eca05," meters")

  Fitted1 <- data.frame(dist = seq(0.01, max(v.eca1$dist), length = 101))
  Fitted05 <- data.frame(dist = seq(0.01, max(v.eca05$dist), length = 101))
  Fitted1$gamma <- variogramLine(fit.eca1, dist_vector = Fitted1$dist)$gamma
  Fitted05$gamma <- variogramLine(fit.eca05, dist_vector = Fitted05$dist)$gamma

  p.v1 <- 
    ggplot(v.eca1, aes(x = dist, y = gamma)) + 
    geom_point(shape=1,size=2,col="blue") + 
    geom_line(data = Fitted1,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = names(df[3])) + 
    scale_x_continuous(limits = c(0, max(v.eca1$dist+(max(v.eca1$dist)/20))),expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(c(v.eca1$gamma+(max(v.eca1$gamma)/5), Fitted1$gamma))),expand = c(0, 0))+
    annotate("text", x = (max(v.eca1$dist)), y=(max(v.eca1$gamma)+(max(v.eca1$gamma)/10)), label = params1,hjust=1)
  
  p.v05 <- 
    ggplot(v.eca05, aes(x = dist, y = gamma)) + 
    geom_point(shape=1,size=2,col="blue") + 
    geom_line(data = Fitted05,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title=names(df[4])) + 
    scale_x_continuous(limits = c(0, max(v.eca05$dist+(max(v.eca05$dist)/20))),expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(c(v.eca05$gamma+(max(v.eca05$gamma)/5), Fitted05$gamma))),expand = c(0, 0))+
    annotate("text", x = (max(v.eca05$dist)), y=(max(v.eca05$gamma)+(max(v.eca05$gamma)/10)), label = params05,hjust=1)

  multiplot(p.v1,p.v05,cols=2)
  
  } else { #if(plotType=="krige") { # Kriging
    
    # Define the grid extent (all points perimeter + 5m)
    sp.full<-df.full
    coordinates(sp.full) = ~x + y
    crs(sp.full) <- CRS(utm)
    x.range <- as.numeric(c(round(extent(sp.full)[1]-30), round(extent(sp.full)[2]+30)))
    y.range <- as.numeric(c(round(extent(sp.full)[3]-30), round(extent(sp.full)[4]+30)))
    
    # Expand points to grid
    grid.red<-1
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = grid.red), y = seq(from = y.range[1], to = y.range[2], by = grid.red))  
    coordinates(grd) <- ~x + y
    gridded(grd) <- TRUE
    crs(grd) <- CRS(utm)
    
    print(paste0("Start kriging ",names(df[3])," 1x1 ~~~",Sys.time()))
    kriged.eca1 = krige(eval(parse(text=names(df[3])))~1, sp, grd, model = fit.eca1)
    #saveRDS(kriged.ecv1, 'gf_ecv1_1x1_2021_12_30.RData')
    print(paste0("Start kriging ",names(df[4])," 1x1 ~~~",Sys.time()))
    kriged.eca05 = krige(eval(parse(text=names(df[4])))~1, sp, grd, model = fit.eca05)
    #saveRDS(kriged.ecv05, 'gf_ecv05_1x1_2021_12_30.RData')
    print(paste("Done kriging ~~~",Sys.time()))
    
    r.kriged.eca1 <- raster(kriged.eca1[1])
    r.kriged.eca05 <- raster(kriged.eca05[1])
    r.var.eca1 <- raster(kriged.eca1[2])
    r.var.eca05 <- raster(kriged.eca05[2])
    
    if(perimeterFile!="none"){
      #perimeterFile="perimeter.shpeerimer
      # get field perimeter area from shapefile
      # perimeter <- readOGR(dsn = polygonsDir, layer = substr(perimeterFile,0,str_length(perimeterFile)-4))
      # perimeter.df <- SpatialPolygonsDataFrame(perimeter,data=data.frame(row.names=row.names(perimeter)))
      perimeter<<-sf::st_read(paste0(polygonsDir,perimeterFile))
      perimeter.df<<- as(st_zm(perimeter), 'Spatial')

      # extent(r.kriged.eca1)<-
      # extent(r.kriged.eca05)<-
      # extent(r.var.eca1)<-
      # extent(r.var.eca05)<-extent(perimeter)
      # 
      # print(extent(perimeter))
      # print(extent(r.kriged.eca1))
      
      # Crop using extent, rasterize polygon and finally, create poly-raster
      cr.eca.1 <- raster::crop(r.kriged.eca1, extent(perimeter))
      fr.eca.1 <- rasterize(perimeter.df, cr.eca.1)
      lr.eca.1 <- mask(x=cr.eca.1, mask=fr.eca.1)
  
      cr.eca.05 <- raster::crop(r.kriged.eca05, extent(perimeter))
      fr.eca.05 <- rasterize(perimeter.df, cr.eca.05)
      lr.eca.05 <- mask(x=cr.eca.05, mask=fr.eca.05)
      
      cr.eca.1.var <- raster::crop(r.var.eca1, extent(perimeter))
      fr.eca.1.var <- rasterize(perimeter.df, cr.eca.1.var)
      lr.eca.1.var <- mask(x=cr.eca.1.var, mask=fr.eca.1.var)
      
      cr.eca.05.var <- raster::crop(r.var.eca05, extent(perimeter))
      fr.eca.05.var <- rasterize(perimeter.df, cr.eca.05.var)
      lr.eca.05.var <- mask(x=cr.eca.05.var, mask=fr.eca.05.var)
      
      df.eca1=as.data.frame(as(lr.eca.1, "SpatialPixelsDataFrame"))
      df.eca05=as.data.frame(as(lr.eca.05, "SpatialPixelsDataFrame"))
      df.eca1.var=as.data.frame(as(lr.eca.1.var, "SpatialPixelsDataFrame"))
      df.eca05.var=as.data.frame(as(lr.eca.05.var, "SpatialPixelsDataFrame"))

      eca.stack<-stack(lr.eca.1,lr.eca.05,lr.eca.1.var,lr.eca.05.var)
      # zmin=floor(min(cellStats(eca.1,min),cellStats(eca.05,min)))
      # zmax=ceiling(max(cellStats(eca.1,max),cellStats(eca.05,max)))      
      #eca.1<-r.kriged.eca1
      #eca.05<-r.kriged.eca05
      #eca.stack<-stack(eca.1,eca.05)
    } else {
      df.eca1=as.data.frame(kriged.eca1[1])
      df.eca05=as.data.frame(kriged.eca05[1])
      df.eca1.var=as.data.frame(kriged.eca1[2])
      df.eca05.var=as.data.frame(kriged.eca05[2])
      
      eca.stack<-stack(r.kriged.eca1,r.kriged.eca05,r.var.eca1,r.var.eca05)
    }
    
    names(eca.stack)<-c("ECa_1m","ECa_05m","variance_1m","variance_05m")
    unlink(paste0(stagingDir,names(eca.stack),".tif"))
    unlink(paste0(stagingDir,names(eca.stack),".tif.aux.xml"))
    #writeRaster(eca.stack, filename=paste0(stagingDir,names(eca.stack)), bylayer=TRUE,format="GTiff",overwrite=TRUE)
    writeRaster(eca.stack$ECa_1m, filename=paste0(stagingDir,names(eca.stack)[1]),format="GTiff",overwrite=TRUE)
    writeRaster(eca.stack$ECa_05m, filename=paste0(stagingDir,names(eca.stack)[2]),format="GTiff",overwrite=TRUE)
    writeRaster(eca.stack$variance_1m, filename=paste0(stagingDir,names(eca.stack)[3]),format="GTiff",overwrite=TRUE)
    writeRaster(eca.stack$variance_05m, filename=paste0(stagingDir,names(eca.stack)[4]),format="GTiff",overwrite=TRUE)
    #print(paste0("updated files with perimeter", names(eca.stack)))
    files<-paste0(stagingDir,names(eca.stack),".tif")
    zip(zipfile=paste0(stagingDir,"ECa_kriged.zip"), files=files,flags = "-FSj")

    pal.eca <- colorRampPalette(brewer.pal(9,"OrRd"))(9)
    pal.var <- colorRampPalette(brewer.pal(9,"Blues"))(9)
    theme.eca <- theme(legend.position="right",plot.title = element_text(hjust = 0,size=14),plot.subtitle = element_text(size=12,hjust = 0),plot.caption = element_text(size=12),axis.text=element_text(size=10),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey30"),panel.grid.minor = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.2, fill = NA)) 

    
    p.k1 <- ggplot(df.eca1,aes(x,y)) + geom_tile(aes(fill=var1.pred),alpha = 0.9) + coord_equal() + theme.eca +
      labs( y="Northing", x="Easting", title=paste(names(df[3]),"Kriged"), subtitle="", caption="",fill = "mS/m") + 
      scale_fill_gradientn(colours = pal.eca) + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0))
    
    p.k05 <- ggplot(df.eca05,aes(x,y)) + geom_tile(aes(fill=var1.pred),alpha = 0.9) + coord_equal() + theme.eca +
      labs( y="Northing", x="Easting", title=paste(names(df[4]),"Kriged"), subtitle="", caption="",fill = "mS/m") + 
      scale_fill_gradientn(colours = pal.eca) + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0))
    
    p.v1 <- ggplot(df.eca1.var,aes(x,y)) + geom_tile(aes(fill=var1.var),alpha = 0.9) + coord_equal() + theme.eca +
      labs( y="Northing", x="Easting", title=paste(names(df[3]),"- Kriging Variance"), subtitle="", caption=paste0("mean: ",round(mean(df.eca1.var[,1],na.rm=T),2)),fill = "Variance [mS/m]") + 
      scale_fill_gradientn(colours = pal.var) + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0))
    
    p.v05 <- ggplot(df.eca05.var,aes(x,y)) + geom_tile(aes(fill=var1.var),alpha = 0.9) + coord_equal() + theme.eca +
      labs( y="Northing", x="Easting", title=paste(names(df[4]),"- Kriging Variance"), subtitle="", caption=paste0("mean: ",round(mean(df.eca05.var[,1],na.rm=T),2)),fill = "Variance [mS/m]") + 
      scale_fill_gradientn(colours = pal.var) + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0))
    
    multiplot(p.k1,p.v1,p.k05,p.v05,cols=2)
  }
  


}




clusters <- function(nclust=1,project,layers){
  if(nclust==1){
    text(x = 0.5, y = 0.5, paste("Select number of clusters...\n"), cex = 1.6, col = "black")
    return();
  }
  
  if(length(layers)<2){
    text(x = 0.5, y = 0.5, paste("Select att least 2 input layers...\n"), cex = 1.6, col = "black")
    return();
  }
  
  #layers<-c("ECa_05m220130_153232.tif", "ECa_1m_220130_153940.tif"), "ECa_1m220130_153225.tif")
  em.stack<-stack()
  for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
    em.stack<-stack(em.stack,r)
  }
#  names(em.stack)<-c("ECaV 1.5m 25-2","ECaV 0.75m 25-2","ECaH 1.0m 25-2","ECaH 0.5m 25-2")

  field.df <- as.data.frame(rasterToPoints(em.stack))
  
  #nclust=3
  nclust <- as.numeric(nclust)
  d.cols <- 3:ncol(field.df)
  #fc <- cmeans(field.df[d.cols],nclust,1000,verbose=F,m=2,method="cmeans") # ECa + NDVI (26/2)
  set.seed(1234)
  kc <- kmeans(field.df[d.cols],nclust,100000, algorithm="MacQueen")
  
  field.df$kc <- kc$cluster
  #field.df$fc <- mapvalues(field.df$fc, from=c(unique(field.df$fc)), to=c(unique(field.df$fc)))
  
  anc.r<-rasterFromXYZ(field.df[c("x","y","kc")])
  writeRaster(anc.r, filename=paste0(stagingDir,"clusters_",nclust), format="GTiff",overwrite=TRUE)  
  
  palj <- brewer.pal(nclust,"Spectral")
  ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc))) + coord_equal() + theme_bw() + theme(plot.title = element_text(hjust = 0.5,size=10)) + labs( y="Northing", x="Easting", title=paste(nclust,"clusters - by",toString(layers),collapse=" "), caption = "") + scale_fill_manual(values=c(palj),name="cluster",labels=c("1", "2", "3", "4", "5", "6","7","8"))
  
}




# clustering source layers
clusterSourcePlot <- function(project,layers){

  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select project and input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  em.stack<-stack()
  for(layer in layers) {
    if (file.exists(paste0(projectsDir,project,"/",layer))) {
      r<-raster(paste0(projectsDir,project,"/",layer))
      em.stack<-stack(em.stack,r)
    } else {
      text(x = 0.5, y = 0.5, paste("file does not exist.\n"), cex = 1.6, col = "black")
      return();
    }
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  layer.cols<-names(field.df[-c(1:2)])
  n=length(layers)
  
  palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
  
  for (i in 1:n) {
    pl <-
      ggplot(field.df,aes(x,y)) + geom_tile(aes_string(fill=layer.cols[i]),alpha = 1) + coord_equal() + 
      theme(legend.position="bottom",plot.title = element_text(hjust = 0.5,size=10),
            axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey30"),
            panel.grid.minor = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.2, fill = NA)) + 
      labs( y="", x="", title=layers[i])+
      scale_fill_gradientn(colours = palc,name="")
    
    assign(paste0("p",i),pl)
  }
  
  params <- as.list(parse(text=paste0("p", 1:n)))
  params$cols<-n
  #par(mfrow=c(1,1))
  do.call(multiplot,params)
}




# Fuzzy clusters validitiy indices
clustersValidity <- function(project,layers,max.k=6){
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select project and input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  em.stack<-stack()
  for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
    em.stack<-stack(em.stack,r)
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  layer.cols<-names(field.df[-c(1:2)])
  
  cat("Calculating fuzzy clusters validitiy indices...
  Partition Entropy (PE)
  Partition Coefficient (PC)
  Fukuyama Sugeno Index (FS)
  Calinski-Harbaz Criterion (CHC)
  Xie Beni (XB)")
  cat(paste(c("Input layers:", layers), collapse=" "))
  cat(paste0("Scope: k in 2:",max.k))
  
  fuzzy.indices<-data.frame(k=2:max.k)
  pe.v<-pc.v<-fs.v<-chc.v<-xb.v<-list()
  x <- field.df[layer.cols]
  #
  for (k in 2:max.k){
    fc <- cmeans(x,k,1000,verbose=F,m=2,method="cmeans")
    pe.v <- append(pe.v,fclustIndex(fc,x, index="partition.entropy"))
    pc.v <- append(pc.v,fclustIndex(fc,x, index="partition.coefficient"))
    fs.v <- append(fs.v,fclustIndex(fc,x, index="fukuyama.sugeno"))  
    chc.v <- append(chc.v,calinhara(x,fc$cluster))
    xb.v <- append(xb.v,fclustIndex(fc,x, index="xie.beni"))
    #pre.v <- append(xb.v,fclustIndex(fc,x, index="proportion.exponent"))
    print(paste0("done k=",k))
  }
  fuzzy.indices<-cbind(fuzzy.indices,pe=unlist(pe.v),pc=unlist(pc.v),fs=unlist(fs.v),chc=unlist(chc.v),xb=unlist(xb.v))
  
  caption <- "" #paste(anclayers, collapse=", ")
  
  theme.fvi <- theme(plot.subtitle = element_text(hjust=1,size=11),panel.background = element_rect(NA),panel.grid = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.1, fill = NA))
  
  fvi.1 <- ggplot(fuzzy.indices,aes(k,pe)) + geom_point(size=1) + geom_line(linewidth=1) + 
    geom_vline(aes(xintercept=fuzzy.indices[which.max(pe),"k"]),col="Red2",alpha=0.5) +
    theme.fvi + labs(title="Partition entropy (max)",caption=caption) + scale_x_discrete("k", 1:max.k, 1:max.k, 1:max.k)
  fvi.2 <- ggplot(fuzzy.indices,aes(k,pc)) + geom_point(size=1) + geom_line(linewidth=1) + 
    geom_vline(aes(xintercept=fuzzy.indices[which.min(pc),"k"]),col="Red2",alpha=0.5) +
    theme.fvi + labs(title="Partition coefficient (min)",caption=caption) + scale_x_discrete("k", 1:max.k, 1:max.k, 1:max.k)
  fvi.3 <- ggplot(fuzzy.indices,aes(k,fs)) + geom_point(size=1) + geom_line(linewidth=1) + 
    geom_vline(aes(xintercept=fuzzy.indices[which.min(fs),"k"]),col="Red2",alpha=0.5) +
    theme.fvi + labs(title="Fukuyama-Sugeno (min)",caption=caption) + scale_x_discrete("k", 1:max.k, 1:max.k, 1:max.k)
  fvi.4 <- ggplot(fuzzy.indices,aes(k,chc)) + geom_point(size=1) + geom_line(linewidth=1) + 
    geom_vline(aes(xintercept=fuzzy.indices[which.max(chc),"k"]),col="Red2",alpha=0.5) +
    theme.fvi + labs(title="Calinski-Harbaz criterion (max)",caption=caption) + scale_x_discrete("k", 1:max.k, 1:max.k, 1:max.k)
  fvi.5<-ggplot(fuzzy.indices,aes(k,xb)) + geom_point(size=1) + geom_line(linewidth=1) + 
    geom_vline(aes(xintercept=fuzzy.indices[which.min(xb),"k"]),col="Red2",alpha=0.5) +
    theme.fvi + labs(title="Xie-Beni (min)") + scale_x_discrete("k\nnumber of clusters", 1:max.k, 1:max.k, 1:max.k)
  
  multiplot(fvi.1,fvi.2,fvi.3,fvi.4,fvi.5,cols = 1)
  #multiplot(fvi.3,fvi.4)
}




clusterBox <- function(project,layers,nclust=1){
  # project<-"Example"
  # layers<-c("dugma 28 4 v_deep_c1_0-95_example_perimeter.tif", "dugma 28 4 v_shallow_c1_0-95_example_perimeter.tif")
    if(nclust==1){
      return();
    }
    
    if(length(layers)<2){
      text(x = 0.5, y = 0.5, paste("Select att least 2 input layers...\n"), cex = 1.6, col = "black")
      return();
    }
    
    em.stack<-stack()
    for(layer in layers) {
      if (file.exists(paste0(projectsDir,project,"/",layer))) {
        r<-raster(paste0(projectsDir,project,"/",layer))
        em.stack<-stack(em.stack,r)
      } else {
        text(x = 0.5, y = 0.5, paste("file does not exist.\n"), cex = 1.6, col = "black")
        return();
      }
    }
    field.df <- as.data.frame(rasterToPoints(em.stack))
    
    nclust <- as.numeric(nclust)
    d.cols <- 3:ncol(field.df)
    N=nrow(field.df)

    set.seed(1234)
    kc <- kmeans(field.df[d.cols],nclust,100000, algorithm="MacQueen")
    field.df$kc <- factor(kc$cluster)
  
    theme.box<-theme(axis.text=element_text(size=12),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                     panel.grid.minor = element_blank(),panel.grid.major.y = element_line(size=0.1),panel.border = element_blank(),
                     axis.line=element_line(size=0.1),axis.ticks.x = element_blank()) 
    palc <-brewer.pal(9,"Set1")
    ggplot(field.df, aes_string("kc",names(field.df)[3],group="kc"),fill="steelblue3") + theme_bw() + theme.box + 
      #geom_violin(fill="grey80",adjust = 0.7,draw_quantiles = c(0.5),scale="count") + 
      geom_boxplot(width=0.2,outlier.size = 0.8) + 
      geom_point(size=0.7,alpha=0.7,col="grey20") +
      stat_summary(fun=mean, geom="point", size=1.5, color="red") + 
      labs(title=paste0("ECa by Cluster"),x="Cluster",y="ECa",caption=paste0("n=",N))
}



loadECa <- function(rawFile,rawFileType="space") {
  #rawFile<-"30.12gadsh.v.csv"
  #rawFile<-"Sasa_04-23_H.csv"
  x.path <- paste0(uploadDir,rawFile)
  
  if(rawFileType=="csv"){
    xdat <- data.frame(read.csv(x.path,header = T,sep = ',',quote = '"'))
  } else {
    if(rawFileType=="space"){
      xdat <- data.frame(read.csv(x.path,header = F,sep = '',quote = '"'))
    } else {
      # tab delimeted
      xdat <- data.frame(read.csv(x.path,header = F,sep = '\t',quote = '"'))
    }
  }
  #names(xdat)<-as.character(xdat[1,])
  #names(xdat)[6]<-paste0(names(xdat)[6],"05")
  if(is.na(as.numeric(xdat[1,1]))){xdat<-xdat[-1,]}
  xdat<-xdat[!grepl("//", xdat[,1]),] # remove comments
  xdat <- as.data.frame(apply(xdat,2, as.numeric))
  #xdat <- xdat %>% discard(~all(is.na(.) | . ==""))
  xdat <- xdat %>% discard(~any(is.na(.) | . ==""))
  
  names(xdat)<-c("x","y","ECa.1m","I.1m","ECa.05m","I.05m","Elevation")
  
  #print(paste("min y",min(xdat$y), "max y",max(xdat$y)))
  return(xdat)
}




######################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

normal <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


###
#################### 
# Sampling plan design

sampleDesign <- function(project,layers,n.points=c(10),s.type="quantiles",outlier.rm=T,seed="deterministic") {
  #layers=c("ECa_1m_220306-180417.tif","ECa_05m_220306-180417.tif")
  #layers=c("conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 v_1m_c1_0-99_conqueiros_site1.tif","conqueiros site1 v_05m_c1_0-99_conqueiros_site1.tif")
  # project="Gadash_farm"
  # layers=c("30.12gadsh.h_deep_c1_0-98_sample_area.tif","30.12gadsh.h_shallow_c1_0-98_sample_area.tif")#,"30.12gadsh.v_deep_c1_1-98_sample_area.tif","30.12gadsh.v_shallow_c1_1-98_sample_area.tif")
  # project="ulisboa"
  # layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif")
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  

  em.stack<-stack()
  for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
    em.stack<-stack(em.stack,r)
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-names(field.df[-c(1:2)])
  #n.layers=length(layers)
  
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  sampling.schemes<-list()
  centroids<-list()
  q<-1
  
  n.points<-seq(min(n.points),max(n.points),1)
    
  for(N in n.points){
      n.clust<-N
      
      # Clusters by Geometry
      if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
      kc <- kmeans(field.df[1:2],n.clust,100000, algorithm="MacQueen")
      field.df$kc <- kc$cluster
      field.df[[paste0("kc.",N)]] <- field.df$kc
      field.df$outlier <- 0
      
      centers <- as.data.frame(kc$centers)
      centers$kc<-1:N
      centroids<-c(centroids,list(centers)) # add to list of plans by N
      
      # palcm<-colorRampPalette(brewer.pal(9,"Spectral"))(n.clust)
      # ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc))) + geom_point(data=centroids[[1]],aes(x,y),color="black",size=1.2) +
      #   coord_equal() + theme_bw() + theme(plot.title = element_text(hjust = 0.5,size=12)) + labs( y="Northing", x="Easting", title=paste0("Geometric Clusters"), caption = "") + scale_fill_manual(values=palcm,name="cluster",labels=c(1:n.clust))
      
      
      ### Stratification method
      if(s.type=="clusters"){
        ### Clusters by Feature Space - ECa
        if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
        kc.eca <- kmeans(field.df[d.cols],n.clust,100000, algorithm="MacQueen")
        field.df$kc.eca <- kc.eca$cluster
        field.df[[paste0("kc.eca.",N)]] <- field.df$kc.eca
        s.type.str="cluster"
      } else {
        ### Quantiles by Feature Space - ECa first layer
        field.df$kc.eca <-  ntile(field.df[[3]],N)
        s.type.str="quantile"

        field.temp<-data.frame()
        for(o in 1:length(unique(field.df$kc.eca))){
          v.quantile <- field.df[field.df$kc.eca==o,]
          # mark outliers
          v.quantile[is.na(remove_outliers(v.quantile[[3]])),"outlier"]<-1
          field.temp<-rbind(field.temp,v.quantile)
        }
        field.df<-field.temp
        
        
      }
      
      ### sort geo clusters by number of feature clusters - ASC
      c.rank <- field.df %>% dplyr::count(kc, kc.eca, sort = F)
      num.features <- c.rank %>% dplyr::count(kc, sort = T)
      
      valid=0
      j=1
      # 
      while(valid==0){
        sampling.n<-data.frame()
        unsampled <- 1:N
        #  print(paste("iter",j))
        valid=1
        cls.all.df<-data.frame()
        
        for(i in rev(num.features$kc)){
          field.i <- field.df[field.df$kc==i,] # geo cluster i

          if(!is.null(outlier.rm) & outlier.rm==T){ field.i=field.i[field.i$outlier==0,] } # remove outliers
          feasible <- intersect(unique(field.i$kc.eca),unsampled) # intersection set of un-sampled feature space points in zone i
          
          cent.i<-t(as.matrix(as.numeric(centers[i,]))) # centroid i coordinates
          points.i<-as.matrix(field.i[,1:2]) # coordinates of points in zone i
          diff.center <- sweep(points.i, 2, cent.i, check.margin=F) # subtract point aa from each point in bb
          field.i$cent.dist <- sqrt(rowSums(diff.center^2))
          center.idx <- which.min(field.i$cent.dist) # index of nearest point in i to centroid
          
          if(length(feasible) < 1){
            print(paste("centroid feature cluster:",field.i[center.idx,"kc.eca"]))
            print(paste0("no unique options... N=",N," | geo-cluster=",i," --> choosing centroid..."))
            pnt <- field.i[center.idx,]

          } else {
            
            # select centroid point first
            feature.cls<-field.i[center.idx,]$kc.eca
            
            field.cls <- field.i[field.i$kc.eca %in% feasible,]
            if(feature.cls %in% feasible){ # centroid is not sampled yet
              pnt <- field.i[center.idx,] #c(colnames(field.cls)[1:6],"kc","kc.eca")] # centroid point
            } else { # select closest point among unsampled clusters
              nearest.idx <- which.min(field.cls$cent.dist)
              pnt <- field.cls[nearest.idx,] #c(colnames(field.cls)[1:6],"kc","kc.eca")] # nearest point row
              feature.cls<-pnt$kc.eca
            }
            
            unsampled <- unsampled[!unsampled %in% feature.cls] # update unsampled
            
          }
          sampling.n<-rbind(sampling.n,pnt) # add to sampling plan
          cls.all.df<-rbind(cls.all.df,field.cls) # 
        }
        
        j<-j+1 
      }
      
      sampling.schemes<-c(sampling.schemes,list(sampling.n)) # add to list of plans by N
      
      pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(N)
      theme.map <- theme(legend.position="left",plot.title = element_text(hjust = 0,size=10),axis.title = element_text(size=9),axis.text = element_text(size=8),axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
                         legend.title = element_text(size=9),legend.text = element_text(size=5),legend.key.width = unit(0.6,"cm"),legend.key.height = unit(0.1,"cm"),legend.spacing.y = unit(0,"cm"))
      
      assign(paste0("pk.",N),
             ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc)),alpha=0.7) + 
               geom_point(data=sampling.n,aes(x,y),size=1.2,shape=21,fill="white")+
               geom_point(data=centroids[[q]],aes(x,y),size=0.6,shape=3,alpha=0.7) +
               coord_equal() + theme_bw() + theme.map +
               labs( y="Northing", x="Easting", title=paste(N," Geo clusters")) + 
               scale_fill_manual(values=pal.eca,name="cluster") 
             #title=paste("Geo clusters -",N,"points"))
             #geom_text(data=centroids[[q]],aes(x-2,y-2),label=centroids[[q]]$kc,size=2)
      )
      
      assign(paste0("pk.eca.",N),
             ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc.eca)),alpha=0.7) +
               #geom_point(data=centroids[[q]],aes(x,y),size=1.5,shape=3) +
               geom_point(data=sampling.n,aes(x,y),size=1.1,shape=21,fill="white",stroke=0.5)+
               coord_equal() + theme_bw() + theme.map +
               labs( y="Northing", x="Easting", title=paste0(N," ECa ",s.type.str,"s")) + 
               scale_fill_manual(values=pal.eca,name="quantile") #+
              # title=paste0("ECa ",s.type.str,"s - ",N," points"))
              #geom_text(data=sampling.n,aes(x-1,y-1),label=sampling.n$id,size=2)
      )
      
      # boxplot - ECa by feature cluster
      pal.box <- c("slateblue2",brewer.pal(length(d.cols)+2,"Set2"))
      p.box <- ggplot(field.df, aes(x=reorder(kc.eca, d.cols[i]), mean)) + theme_bw() + scale_x_continuous(breaks = 1:10) +
        theme(plot.title = element_text(size=10),axis.title = element_text(size=9))
      for(i in length(d.cols):1){
        p.box <- p.box + geom_boxplot(aes_string("kc.eca",d.cols[i],group="kc.eca"),lwd=0.3,outlier.size=0.5,width=0.25,outlier.alpha = 0.5,col=pal.box[i],fill="transparent")
      }
      p.box <- p.box + labs(title=paste("Distribution by",s.type.str),x=paste(s.type.str),y="ECa (normalized)") + #,subtitle=paste0("n=",N))+
          geom_point(data=sampling.n,aes_string("kc.eca",names(sampling.n)[3]),col="black",size=1)
      #p.box
      assign(paste0("p.sample.",N),p.box)
      
      print("***")
      print(paste0("Done N=",N," rows=",nrow(sampling.n)))
      print("***")
      
      q<-q+1
      
    } # end sample-size loop
  
    
    ##### Plots selected samples
    ### Geo clusters
    plot.list=list()
    for(p in n.points){ # plot geometric clusters
      pl<-list(eval(parse(text=paste0("pk.",p))))
      plot.list<-c(plot.list,pl)
    }
    for(p in n.points){ # plot Feature space clusters
      pl<-list(eval(parse(text=paste0("pk.eca.",p))))
      plot.list<-c(plot.list,pl)
    }
    
    for(p in n.points){ # box plot - ECa distribution
      pl<-list(eval(parse(text=paste0("p.sample.",p))))
      plot.list<-c(plot.list,pl)
    }

    sampling.csv=paste0(stagingDir,"sampling_plans_results.csv") #paste0("sampling_plans_",min(n.points),"-",max(n.points),".csv")
    unlink(sampling.csv)
    lapply(sampling.schemes, function(x) write.table( data.frame(x), sampling.csv , append= T, sep=',', row.names=F))
    #Sys.chmod(sampling.csv, "777", use_umask = FALSE)
    
    saveRDS(sampling.schemes, paste0(stagingDir,'current_samples.RData'))
    Sys.chmod(paste0(stagingDir,'current_samples.RData'), "777", use_umask = FALSE)

    do.call(multiplot,c(plot.list,cols=3))
}
  


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  #H <- 1.5 * IQR(x, na.rm = na.rm)
  H=0
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

  
  

sampleMetrics <- function(project,layers,npoints="",s.type="q",outlier.rm=T,seed="d") {
  # project="beeri"  
  # layers=c("Beeri 9 24 H_1m_c30_0-95_potato.tif")
  # layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif")
  

  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  
  em.stack<-stack()
  for(layer in layers) {
    if (file.exists(paste0(projectsDir,project,"/",layer))) {
      r<-raster(paste0(projectsDir,project,"/",layer))
      em.stack<-stack(em.stack,r)
    } else {
      text(x = 0.5, y = 0.5, paste("file does not exist.\n"), cex = 1.6, col = "black")
      return();
    }
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-3:ncol(field.df)
  n.layers=length(layers)
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  
  
  if(file.exists(paste0(stagingDir,'current_samples.RData'))){
    samples<-readRDS(paste0(stagingDir,'current_samples.RData'))
  } else {
    text(x = 0.5, y = 0.5, paste("...\n"), cex = 1.2, col = "black")
    return();
  }
  

  ### AIC - by sample size ###
  aic.names<-c(paste0("aic.",names(samples[[1]]))[d.cols],"n") #c("aic.ecv1","aic.ecv05","aic.ech1","aic.ech05","n")
  aic.res <- data.frame()#aic.ecv1=numeric(),aic.ecv05=numeric(),aic.ech1=numeric(),aic.ech05=numeric(),sid=character(),n=integer())
  
  for (i in 1:length(samples)){
    aic.row<-list()
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
  
    for(j in d.cols){
      if(length(d.cols)>1){
        lm <- lm(as.formula(paste(colnames(samp)[j], "~",
                                  paste(colnames(samp)[d.cols[!d.cols %in% j]], collapse = "+"),
                                  sep = "")), 
                 data=samp)
        aic<-AIC(lm)
      } else {
        aic<-0
      }
      aic.row<-c(aic.row,aic)
    }
    
    newrow<-data.frame(c(aic.row,nrow(samp)))
    names(newrow)<-aic.names
    aic.res<-rbind(aic.res,newrow)
  }
  
  aic.res$aic.mean<-apply(aic.res[1:length(d.cols)],1,mean)
  aic.res$aic.max<-apply(aic.res[1:length(d.cols)],1,max)
  aic.res$aic.min<-apply(aic.res[1:length(d.cols)],1,min)
  aic.res$n<-as.factor(aic.res$n)
  
  pl.aic.all<-
  ggplot(aic.res) + 
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black")) +
    scale_x_discrete() +
    labs( x="n",y="AIC", title='AIC by sample size n') +
    #geom_point(aes_string("n",aic.names[1]),col="purple4",alpha=0.5,size=2) + 
    #geom_point(aes_string("n",aic.names[2]),col="navyblue",alpha=0.5,size=2) + 
    geom_point(aes(n,aic.mean),size=4,shape=1) + 
    geom_line(aes(n,aic.mean,group=1),linewidth=1,lty=1) +
    geom_vline(aes(xintercept=aic.res[which.min(aic.mean),"n"]),col="Red2",alpha=0.5) 

###




  ### D-KL - by sample size ###
  dkl.names<-c(paste0("dkl.",names(samples[[1]])[d.cols]),"n")
  dkl.res <- data.frame()
  bins=9
  
  # compute the entire field distribution
  dkl.full<-list()
  for(j in d.cols){
    field.j<-field.df[[j]]
    h.full <- hist(field.j, breaks = bins ,plot=FALSE)
    h.full$counts[h.full$counts==0]<-0.01
    d.full=h.full$counts/sum(h.full$counts)
    dkl.full<-c(dkl.full,list(d.full))
  }
  
  # compute samples distributions
  for (i in 1:length(samples)){
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    #bins<-nrow(samp)
    dkl.samp<-list()
    for(j in d.cols){
      samp.j<-samp[[j]]
      h.samp <- hist(samp.j, breaks = bins, plot=FALSE)
      h.samp$counts[h.samp$counts==0]<-0.01
      dkl.samp.j=h.samp$counts/sum(h.samp$counts)
      dkl.samp<-c(dkl.samp,list(dkl.samp.j))
    }
    
    kl.val<-list()
    for(k in 1:length(d.cols)){
      # compute d-KL of sample and field distributions
      kl <- signif(KL.plugin(dkl.full[[k]],dkl.samp[[k]]),3)
      kl.val <- c(kl.val,kl)
    }
    
    newrow<-data.frame(kl.val,nrow(samp))
    names(newrow)<-dkl.names
    #newrow<-data.frame(kl.ecv,kl.ech,kl.tir,sid,fid,n)
    dkl.res<-rbind(dkl.res,newrow)
  }
  
  dkl.res$dkl.mean<-apply(dkl.res[1:2],1,mean)
  dkl.res$n<-as.factor(dkl.res$n)
  
  pl.dkl.all<-
  ggplot(dkl.res,aes(n,kl.ecv1)) + 
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black")) +
    scale_x_discrete() +
    labs( x="n",y="D-KL", title='D-KL by sample size n') +
    #geom_point(aes_string("n",dkl.names[1]),col="purple4",alpha=0.5,size=2) + 
    #geom_point(aes_string("n",dkl.names[2]),col="navyblue",alpha=0.5,size=2) +
    geom_point(aes(n,dkl.mean),size=4,shape=1) + 
    geom_line(aes(n,dkl.mean,group=1),linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=dkl.res[which.min(dkl.mean),"n"]),col="Red2",alpha=0.5) 



### cLHS - by sample size ###
  
  clhs.res <- data.frame()
  data_continuous <- field.df[d.cols] # only ancillary data columns
  n_cols <- ncol(data_continuous)
  n_data <- nrow(data_continuous)
  
  # compute samples cLHS
  for (i in 1:length(samples)){
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    N <- nrow(samp)
    
    # Edge of the strata
    continuous_strata <- apply(data_continuous, 2, function(x) {quantile(field.df, probs = seq(0, 1, length.out = N + 1), na.rm = TRUE)})
    continuous_samp <- samp[d.cols]
    cont_data_strata <- lapply(1:n_cols, function(i) list(continuous_samp[, i], continuous_strata[, i]) )
    cont_obj_sampled <- lapply(cont_data_strata, function(x) hist(x[[1]], breaks = x[[2]], plot = FALSE)$counts)
    cont_obj_sampled <- matrix(unlist(cont_obj_sampled), ncol = n_cols, byrow = FALSE)
    
    delta_obj_continuous <- rowSums(abs(cont_obj_sampled - 1)) 
    clhs.val <- sum(delta_obj_continuous)/(N*n_cols) # cLHS - normalized 
  
    newrow<-data.frame(clhs.val,nrow(samp))
    names(newrow)<-c("cLHS","n")
    clhs.res<-rbind(clhs.res,newrow)
  }
  
  clhs.res$n<-as.factor(clhs.res$n)
  
  pl.clhs.all<-
    ggplot(clhs.res,aes(n,cLHS)) + 
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black")) +
    scale_x_discrete() +
    labs( x="n",y="cLHS", title='cLHS (normalized) by sample size n') +
    geom_point(aes(n,cLHS),col="purple4",alpha=0.5,size=2) + 
    geom_line(aes(n,cLHS,group=1),linewidth=1.2,lty=1) + 
    geom_vline(aes(xintercept=clhs.res[which.min(cLHS),"n"]),col="Red2",alpha=0.5) 

###

  
  
  ### Distance metric - by sample size ###
  dist.names<-c("cent.dist","n")
  dist.res <- data.frame()

  # compute samples average distance from centroids
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    
    newrow<-data.frame(mean(samp$cent.dist),nrow(samp))
    names(newrow)<-dist.names
    #newrow<-data.frame(kl.ecv,kl.ech,kl.tir,sid,fid,n)
    dist.res<-rbind(dist.res,newrow)
  }
  
  #dist.res$dkl.mean<-apply(dkl.res[1:2],1,mean)
  dist.res$n<-as.factor(dist.res$n)
  #print(dist.res)
  
  pl.dist.all<-
    ggplot(dist.res,aes(n,cent.dist)) + 
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black")) +
    scale_x_discrete() +
    labs( x="n",y="meters", title='Mean distance to centroid by sample size n') +
    geom_point(aes_string("n",dist.names[1]),col="purple4",alpha=0.5,size=2) +
    geom_line(aes(n,cent.dist,group=1),linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=dist.res[which.min(cent.dist),"n"]),col="Red2",alpha=0.5) 
  

  
  
  ### Variogram R^2 - by sample size ###
  vgm.names<-c("R2","n")
  vgm.res <- data.frame()
  
  # compute samples average distance from centroids
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    
    samp.sp<-samp
    coordinates(samp.sp) = ~x + y
    crs(samp.sp) <- CRS(utm)
    
    
    for(k in d.cols){
      v.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp)  # Semi-Variogram
      fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))  # Fit function to variogram
      vgm.R2 <- attr(fit.eca,"SSErr")
      
      newrow<-data.frame(vgm.R2,nrow(samp))
      names(newrow)<-vgm.names
      vgm.res<-rbind(vgm.res,newrow)
    }
    # params1<-paste0("model: ",fit.eca1[2,1],"    nugget: ",round(fit.eca1[1,2],2),"   sill: ",round(fit.eca1[2,2],2),"   range: ",range.eca1," meters")
    # 
    # Fitted1 <- data.frame(dist = seq(0.01, max(v.eca1$dist), length = 101))
    # Fitted1$gamma <- variogramLine(fit.eca1, dist_vector = Fitted1$dist)$gamma
    # 
    # ggplot(v.eca1, aes(x = dist, y = gamma)) + 
    #   geom_point(shape=1,size=2,col="blue") + 
    #   geom_line(data = Fitted1,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = names(df[3])) + 
    #   scale_x_continuous(limits = c(0, max(v.eca1$dist+(max(v.eca1$dist)/20))),expand = c(0, 0)) +
    #   scale_y_continuous(limits = c(0, max(c(v.eca1$gamma+(max(v.eca1$gamma)/5), Fitted1$gamma))),expand = c(0, 0))+
    #   annotate("text", x = (max(v.eca1$dist)), y=(max(v.eca1$gamma)+(max(v.eca1$gamma)/10)), label = params1,hjust=1)
    
    #vgm.R2 <- R2variogram(v.eca1,fit.eca1)
    
  }
  
  #dist.res$dkl.mean<-apply(dkl.res[1:2],1,mean)
  vgm.res$n<-as.factor(vgm.res$n)
  vgm.df <- as.data.frame(vgm.res %>%
    group_by(n) %>%
    dplyr::summarize(Mean = mean(R2, na.rm=TRUE))
  )
  
  pl.vgm.all<-
    ggplot(vgm.df,aes(n,Mean)) + 
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black")) +
    scale_x_discrete() +
    labs( x="n",y="", title='Variogram model fit - Mean Sum of Squared Errors (R^2) by sample size') +
    geom_point(aes_string("n","Mean"),col="purple3",alpha=0.5,size=2) +
    geom_line(aes(n,Mean,group=1),linewidth=1.2,lty=1,col="navy")+
    geom_vline(aes(xintercept=vgm.df[which.min(Mean),"n"]),col="Red2",alpha=0.5) 
  
  

  
## n-fold cross validation:
  
  cv.names<-c("n","ME","MSPE","MSNE","corObsPred","corObsResidual")
  cv.res <- data.frame()
  
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    
    samp.sp<-samp
    coordinates(samp.sp) = ~x + y
    crs(samp.sp) <- CRS(utm)
    
    for(k in d.cols){
      v.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp)  # Semi-Variogram
      fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))  # Fit function to variogram
      
      x <- krige.cv(eval(parse(text=names(samp[k])))~1, samp.sp, fit.eca, nmax = 40, nfold=20)
      #bubble(x, "residual", main = "ECa: 5-fold CV residuals")

      # mean error, ideally 0:
      me<-mean(x$residual)
      # MSPE, ideally small
      mspe<-mean(x$residual^2)
      # Mean square normalized error, ideally close to 1
      msne<-mean(x$zscore^2)
      # correlation observed and predicted, ideally 1
      cor.obs.pred<-cor(x$observed, x$observed - x$residual)
      # correlation predicted and residual, ideally 0
      cor.obs.residual<-cor(x$observed - x$residual, x$residual)
      
      #vgm.R2 <- attr(fit.eca,"SSErr")
      
      newrow<-data.frame(nrow(samp),me,mspe,msne,cor.obs.pred,cor.obs.residual)
      names(newrow)<-cv.names
      cv.res<-rbind(cv.res,newrow)
    }
  }
  
    cv.res$n<-as.factor(cv.res$n)
    cv.df <- as.data.frame(cv.res %>%
                            group_by(n) %>%
                            dplyr::summarize(me = mean(ME, na.rm=TRUE),
                                             mspe = mean(MSPE, na.rm=TRUE),
                                             msne = mean(MSNE, na.rm=TRUE),
                                             corObsPred = mean(corObsPred, na.rm=TRUE),
                                             corObsResidual = mean(corObsResidual, na.rm=TRUE))
  )
  
    theme.cv<-
      theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black"))
  
    pl.cv.me<-
    ggplot(cv.df,aes(n,Mean)) + 
    theme.cv +
    scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation - Mean Prediction Error') +
      geom_point(aes(n,me),col="blue3",alpha=0.5,size=2) +
      geom_line(aes(n,me,group=1),col="blue4",linewidth=1.2,lty=1)+
     geom_vline(aes(xintercept=cv.df[which.min(me),"n"]),col="Red2",alpha=0.5)
  
    pl.cv.msne<-
      ggplot(cv.df,aes(n,msne)) + 
      theme.cv +
      scale_x_discrete() +
      labs( x="n",y="", title='Cross Validation MSNE - Mean Squared Normalized Error') +
      geom_point(aes(n,msne),col="blue3",alpha=0.5,size=2) +
      geom_line(aes(n,msne,group=1),col="blue4",linewidth=1.2,lty=1) +
      geom_vline(aes(xintercept=cv.df[which.min(msne),"n"]),col="Red2",alpha=0.5)
    
    
    pl.cv.mspe<-
      ggplot(cv.df,aes(n,mspe)) + 
      theme.cv +
      scale_x_discrete() +
      labs( x="n",y="", title='Cross Validation MSPE - Mean Squared Prediction Error') +
      geom_point(aes(n,mspe),col="blue3",alpha=0.5,size=2) +
      geom_line(aes(n,mspe,group=1),col="blue4",linewidth=1.2,lty=1) +
      geom_vline(aes(xintercept=cv.df[which.min(mspe),"n"]),col="Red2",alpha=0.5)
    
    
    pl.cv.cor<-
      ggplot(cv.df,aes(n,msne)) + 
      theme.cv +
      scale_x_discrete() +
      labs( x="n",y="", title='Cross Validation - Correlation: Observed ~ Predicted') +
      geom_point(aes(n,corObsPred),col="blue3",alpha=0.5,size=2) +
      geom_line(aes(n,corObsPred,group=1),col="blue4",linewidth=1.2,lty=1) +
      geom_vline(aes(xintercept=cv.df[which.max(corObsPred),"n"]),col="Red2",alpha=0.5)
    
    # geom_point(aes(n,msne),col="red3",alpha=0.5,size=2) +
    # geom_line(aes(n,msne,group=1),col="red4",size=1.2,lty=1) +
    #  geom_point(aes(n,mspe),col="blue3",alpha=0.5,size=2) +
    #  geom_line(aes(n,mspe,group=1),col="blue4",size=1.2,lty=1) +
    #   geom_point(aes(n,corObsPred),col="green3",alpha=0.5,size=2) +
    #   geom_line(aes(n,corObsPred,group=1),col="green4",size=1.2,lty=1) 
    # geom_point(aes(n,corObsResidual),col="darkgoldenrod1",alpha=0.5,size=2) +
    # geom_line(aes(n,corObsResidual,group=1),col="darkgoldenrod3",size=1.2,lty=1)
  

    
    
    ### Variogram R^2 - by sample size ###
    chi.names<-c("X","n")
    chi.res <- data.frame()
    
    # compute samples average distance from centroids
    for (i in 1:length(samples)){
      samp<-samples[[i]] 
      
      samp.sp<-samp
      coordinates(samp.sp) = ~x + y
      crs(samp.sp) <- CRS(utm)
      
      
      for(k in d.cols){
        sp.cloud = variogram(eval(parse(text=names(samp[k])))~1, samp.sp, cloud=T)  # Semi-Variogram
        X.hat = 
          sum(
            abs(
              1-(sp.cloud$gamma+cov(sp.cloud$dist,sp.cloud$gamma)) / 
                var(sp.cloud$gamma))*sp.cloud$np)/sum(sp.cloud$np)
        
        newrow<-data.frame(X.hat,nrow(samp))
        names(newrow)<-chi.names
        chi.res<-rbind(chi.res,newrow)
      }
      
    }
    #dist.res$dkl.mean<-apply(dkl.res[1:2],1,mean)
    chi.res$n<-as.factor(chi.res$n)
    chi.df <- as.data.frame(chi.res %>%
                              group_by(n) %>%
                              dplyr::summarize(X = mean(X, na.rm=TRUE))
    )
    
    pl.chi.all<-
      ggplot(chi.df,aes(n,X)) + 
      theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black")) +
      scale_x_discrete() +
      labs( x="n",y="", title='Variogram quality (X) by sample size') +
      geom_point(aes_string("n","X"),col="navy",alpha=0.5,size=2) +
      geom_line(aes(n,X,group=1),linewidth=1.2,lty=1,col="navy")+
      geom_vline(aes(xintercept=chi.df[which.min(X),"n"]),col="Red2",alpha=0.5) 
    
    
    
      
  multiplot(pl.aic.all,pl.dkl.all,pl.clhs.all,pl.dist.all,pl.vgm.all,pl.chi.all,pl.cv.me,pl.cv.msne,pl.cv.mspe,pl.cv.cor)

}





plotSamplingPlan <- function(project,layers,sample.size="",n.zones=1,seed="deterministic",QC=T) {
  # project="Newe_yaar"  
  # layers=c("ny 10 04 22 h_deep_c5_t0-98_perimeter_100_act_21.tif","ny 10 04 22 h_shallow_c5_t0-98_perimeter_100_act_21.tif")
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  if(sample.size==""){
    text(x = 0.5, y = 0.5, paste("Select sample size...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  
  em.stack<-stack()
  for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
    em.stack<-stack(em.stack,r)
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-3:ncol(field.df)

  if(QC==T)  {
    rdata.name = 'current_QC_samples.RData'
  } else {
    rdata.name = 'current_samples.RData'
  }
  
  if(file.exists(paste0(stagingDir,rdata.name))){
    samples<-readRDS(paste0(stagingDir,rdata.name))
  } else {
    text(x = 0.5, y = 0.5, paste("...\n"), cex = 1.2, col = "black")
    return();
  }
  
  found=0
  for(i in 1:length(samples)){
   n=nrow(samples[[i]])
   if(n==sample.size){
     
     pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(sample.size)
     zones.label=""
     found=1
     
     #field.df$eca.steps<-plyr::round_any(field.df[[3]], sample.size)# round((max(as.numeric(field.df[[3]]))-min(as.numeric(field.df[[3]])))/n)),2)
     #field.df[3]<-as.numeric(field.df[3])
     field.df$eca.steps <-  dplyr::ntile(field.df[[3]],n)
     field.df$eca.steps<-as.factor(field.df$eca.steps)
     
     d.cols.sample<-3:match("kc",names(samples[[i]]))
     sample<-samples[[i]][c("x","y",names(samples[[i]][d.cols.sample]),"kc","cent.dist")]
     sample$cent.dist<-round(sample$cent.dist,2)
     sample<-sample[order(sample$y),]
     sample$id<-1:n
     
     sample<-sample[c("x","y","id")]

     sample.csv=paste0(stagingDir,"sampling_plan_selected.csv") #paste0("sampling_plans_",min(n.points),"-",max(n.points),".csv")
     unlink(sample.csv)
     write.table( sample, sample.csv , sep=',', row.names=F)
#     saveRDS(sample, paste0(stagingDir,'selected_sampling_plan.RData'))
#     Sys.chmod(paste0(stagingDir,'selected_sampling_plan.RData'), "777", use_umask = FALSE)
     
     
       # Clusters by Geometry
     if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
       kc <- kmeans(field.df[1:2],sample.size,100000, algorithm="MacQueen")
       field.df$kc <- kc$cluster

       centroids <- as.data.frame(kc$centers)
       centroids$kc<-1:sample.size
       
       if(n.zones>1){
         if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
         kc.eca <- kmeans(field.df[d.cols],n.zones,100000, algorithm="MacQueen")
         field.df$kc.eca <- kc.eca$cluster
         
         zones.label=paste("on",n.zones,"Management Zones")
       } else {
         field.df$kc.eca = 1
       }
     
       sample<-merge(field.df,sample, by = c("x","y"))
       pal.eca <- colorRampPalette(brewer.pal(11,"Spectral"))(n.zones)
     
       sample$kc.eca<-factor(sample$kc.eca)
       field.df$kc.eca<-factor(field.df$kc.eca)
       
     p.sample <- 
       ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc.eca)),alpha=0.4) +
     #     geom_point(data=centroids,aes(x,y),size=1,shape=3) +
          geom_point(data=sample,aes(x,y),size=1.2)+
          coord_equal() + theme_bw() + theme(legend.position="left",plot.title = element_text(hjust = 0.5,size=11),axis.title = element_text(size=10),panel.grid = element_blank()) + 
          labs( y="Northing", x="Easting", title=paste("Sampling plan",zones.label,n," - points")) + 
         scale_fill_manual(values=pal.eca,name="MZ",labels=c(1:n)) +
          geom_text(data=sample,aes(x-9,y-9),label=sample$id,size=2)
     
     
     # boxplot - ECa by feature cluster
     p.sample.box <- ggplot(field.df, aes(x=reorder(kc.eca, names(field.df[3]), mean))) + #theme.ch +
              geom_boxplot(aes_string("kc.eca",names(field.df[3]),group="kc.eca"),width=0.25,lwd=0.3,outlier.size=0.5,outlier.alpha = 0.5) + theme_bw() + 
              labs(title=paste("Distribution"),x=paste("MZ"),y="ECa 1m",caption=paste0("n=",n))+
              geom_point(data=sample,aes_string("kc.eca",names(sample[3])),col="blue2",size=1.2)

     

     # p.feature <- ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=eca.steps),alpha=0.7) +
     #   #geom_point(data=centroids[[q]],aes(x,y),size=1.5,shape=3) +
     #   geom_point(data=sample,aes(x,y),size=1.2)+
     #   coord_equal() + theme_bw() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=11),axis.title = element_text(size=10)) +
     #   labs( y="Northing", x="Easting", title=paste("Sampling plan -",sample.size,"points"), caption = "") + scale_fill_manual(values=pal.eca,name="geo cluster",labels=c(1:10)) +
     #   geom_text(data=sample,aes(x-2,y-2),label=sample$id,size=2.5)
     # 
    
     multiplot(p.sample,p.sample.box,cols=2)
     
   }
  }
    if(found==0){
        text(x = 0.5, y = 0.5, paste("Sample size not in current samples...\n"), cex = 1.2, col = "purple4")
        return();
  }
}











######################
#  QC Sample design  #

QCsampleDesign <- function(project,layers,n.points.min=10,n.points.max=20,s.type="quantiles",outlier.rm=T,seed="deterministic") {
  # project="ulisboa"
  # layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif")
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  if(n.points.min>=n.points.max){
    text(x = 0.5, y = 0.9, paste("Select positive points range...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  em.stack<-stack()
  for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
    em.stack<-stack(em.stack,r)
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-names(field.df[-c(1:2)])
  #n.layers=length(layers)
  
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  sampling.schemes<-list()
  centroids<-list()
  q<-1
  
  points.range <- as.numeric(n.points.max) - as.numeric(n.points.min)
  step.size <- 1
  if(points.range>10) { step.size <- 2 }
  if(points.range>20) { step.size <- 4 }
  if(points.range>30) { step.size <- 6 }
  if(points.range>50) { step.size <- 10 }
  n.points<-seq(n.points.min,n.points.max,step.size)
  
  
  for(N in n.points){
    n.clust<-N
    
    # Clusters by Geometry
    if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
    kc <- kmeans(field.df[1:2],n.clust,100000, algorithm="MacQueen")
    field.df$kc <- kc$cluster
    field.df[[paste0("kc.",N)]] <- field.df$kc
    field.df$outlier <- 0
    
    centers <- as.data.frame(kc$centers)
    centers$kc<-1:N
    centroids<-c(centroids,list(centers)) # add to list of plans by N
    
    # palcm<-colorRampPalette(brewer.pal(9,"Spectral"))(n.clust)
    # ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc))) + geom_point(data=centroids[[1]],aes(x,y),color="black",size=1.2) +
    #   coord_equal() + theme_bw() + theme(plot.title = element_text(hjust = 0.5,size=12)) + labs( y="Northing", x="Easting", title=paste0("Geometric Clusters"), caption = "") + scale_fill_manual(values=palcm,name="cluster",labels=c(1:n.clust))
    
    
    ### Stratification method
    if(s.type=="clusters"){
      ### Clusters by Feature Space - ECa
      if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
      kc.eca <- kmeans(field.df[d.cols],n.clust,100000, algorithm="MacQueen")
      field.df$kc.eca <- kc.eca$cluster
      field.df[[paste0("kc.eca.",N)]] <- field.df$kc.eca
      s.type.str="cluster"
    } else {
      ### Quantiles by Feature Space - ECa first layer
      field.df$kc.eca <-  ntile(field.df[[3]],N)
      s.type.str="quantile"
      
      field.temp<-data.frame()
      for(o in 1:length(unique(field.df$kc.eca))){
        v.quantile <- field.df[field.df$kc.eca==o,]
        # mark outliers
        v.quantile[is.na(remove_outliers(v.quantile[[3]])),"outlier"]<-1
        field.temp<-rbind(field.temp,v.quantile)
      }
      field.df<-field.temp
      
      
    }
    
    ### sort geo clusters by number of feature clusters - ASC
    c.rank <- field.df %>% dplyr::count(kc, kc.eca, sort = F)
    num.features <- c.rank %>% dplyr::count(kc, sort = T)
    
    valid=0
    j=1
    # 
    while(valid==0){
      sampling.n<-data.frame()
      unsampled <- 1:N
      #  print(paste("iter",j))
      valid=1
      cls.all.df<-data.frame()
      
      for(i in rev(num.features$kc)){
        field.i <- field.df[field.df$kc==i,] # geo cluster i
        
        if(!is.null(outlier.rm) & outlier.rm==T){ field.i=field.i[field.i$outlier==0,] } # remove outliers
        feasible <- intersect(unique(field.i$kc.eca),unsampled) # intersection set of un-sampled feature space points in zone i
        
        cent.i<-t(as.matrix(as.numeric(centers[i,]))) # centroid i coordinates
        points.i<-as.matrix(field.i[,1:2]) # coordinates of points in zone i
        diff.center <- sweep(points.i, 2, cent.i, check.margin=F) # subtract point aa from each point in bb
        field.i$cent.dist <- sqrt(rowSums(diff.center^2))
        center.idx <- which.min(field.i$cent.dist) # index of nearest point in i to centroid
        
        if(length(feasible) < 1){
          print(paste("centeroid feature cluster:",field.i[center.idx,"kc.eca"]))
          print(paste0("no unique options... N=",N," | geo-cluster=",i," --> choosing centroid..."))
          pnt <- field.i[center.idx,]
          
        } else {
          
          # select centroid point first
          feature.cls<-field.i[center.idx,]$kc.eca
          
          field.cls <- field.i[field.i$kc.eca %in% feasible,]
          if(feature.cls %in% feasible){ # centroid is not sampled yet
            pnt <- field.i[center.idx,] #c(colnames(field.cls)[1:6],"kc","kc.eca")] # centroid point
          } else { # select closest point among unsampled clusters
            nearest.idx <- which.min(field.cls$cent.dist)
            pnt <- field.cls[nearest.idx,] #c(colnames(field.cls)[1:6],"kc","kc.eca")] # nearest point row
            feature.cls<-pnt$kc.eca
          }
          
          unsampled <- unsampled[!unsampled %in% feature.cls] # update unsampled
          
        }
        sampling.n<-rbind(sampling.n,pnt) # add to sampling plan
        cls.all.df<-rbind(cls.all.df,field.cls) # 
      }
      
      j<-j+1 
    }
    
    sampling.schemes<-c(sampling.schemes,list(sampling.n)) # add to list of plans by N
    
    pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(N)
    theme.map <- theme(legend.position="left",plot.title = element_text(hjust = 0,size=10),axis.title = element_text(size=9),axis.text = element_text(size=8),axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
                       legend.title = element_text(size=9),legend.text = element_text(size=8),legend.key.width = unit(0.6,"cm"),legend.key.height = unit(0.2,"cm"),legend.spacing.y=unit(0,"cm"))
    
    assign(paste0("pk.",N),
           ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc)),alpha=0.7) + 
             geom_point(data=sampling.n,aes(x,y),size=1.2,shape=21,fill="white")+
             geom_point(data=centroids[[q]],aes(x,y),size=0.6,shape=3,alpha=0.7) +
             coord_equal() + theme_bw() + theme.map +
             labs( y="Northing", x="Easting", title=paste(N, "Geo clusters")) + 
             scale_fill_manual(values=pal.eca,name="cluster") 
           #title=paste("Geo clusters -",N,"points"))
           #geom_text(data=centroids[[q]],aes(x-2,y-2),label=centroids[[q]]$kc,size=2)
    )
    
    assign(paste0("pk.eca.",N),
           ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc.eca)),alpha=0.7) +
             #geom_point(data=centroids[[q]],aes(x,y),size=1.5,shape=3) +
             geom_point(data=sampling.n,aes(x,y),size=1.1,shape=21,fill="white",stroke=0.5)+
             coord_equal() + theme_bw() + theme.map +
             labs( y="Northing", x="Easting", title=paste0(N, " ECa ",s.type.str,"s")) + 
             scale_fill_manual(values=pal.eca,name="quantile") #+
           # title=paste0("ECa ",s.type.str,"s - ",N," points"))
           #geom_text(data=sampling.n,aes(x-1,y-1),label=sampling.n$id,size=2)
    )
    
    # boxplot - ECa by feature cluster
    pal.box <- c("slateblue2",brewer.pal(length(d.cols)+1,"Set2"))
    p.box <- ggplot(field.df, aes(x=reorder(kc.eca, d.cols[i]), mean)) + theme_bw() + scale_x_continuous(breaks = n.points) +
      theme(plot.title = element_text(size=10),axis.title = element_text(size=9))
    for(i in length(d.cols):1){
      p.box <- p.box + geom_boxplot(aes_string("kc.eca",d.cols[i],group="kc.eca"),lwd=0.3,outlier.size=0.5,width=0.25,outlier.alpha = 0.5,col=pal.box[i],fill="transparent")
    }
    p.box <- p.box + labs(title=paste("Distribution by",s.type.str),x=paste(s.type.str),y="ECa (normalized)") + #,subtitle=paste0("n=",N))+
      geom_point(data=sampling.n,aes_string("kc.eca",names(sampling.n)[3]),col="black",size=1)
    #p.box
    assign(paste0("p.sample.",N),p.box)
    
    print("***")
    print(paste0("Done N=",N," rows=",nrow(sampling.n)))
    print("***")
    
    q<-q+1
    
  } # end sample-size loop
  
  
  # Plot selected samples
  ### Geo clusters
  plot.list=list()
  
  for(p in n.points){ # plot geometric clusters
    pl<-list(eval(parse(text=paste0("pk.",p))))
    plot.list<-c(plot.list,pl)
  }
  
  for(p in n.points){ # plot Feature space clusters
    pl<-list(eval(parse(text=paste0("pk.eca.",p))))
    plot.list<-c(plot.list,pl)
  }
  
  for(p in n.points){ # box plot - ECa distribution
    pl<-list(eval(parse(text=paste0("p.sample.",p))))
    plot.list<-c(plot.list,pl)
  }
  
  sampling.csv=paste0(stagingDir,"QC_sampling_plans_results.csv") #paste0("sampling_plans_",min(n.points),"-",max(n.points),".csv")
  unlink(sampling.csv)
  
  #sampling.schemes <- lapply(sampling.schemes,function(x) as.character(x)) # convert to char
  suppressWarnings(lapply(sampling.schemes, function(x) write.table( data.frame(x), sampling.csv , append= T, sep=',', row.names=F)))
  #Sys.chmod(sampling.csv, "777", use_umask = FALSE)
  
  saveRDS(sampling.schemes, paste0(stagingDir,'current_QC_samples.RData'))
  Sys.chmod(paste0(stagingDir,'current_QC_samples.RData'), "777", use_umask = FALSE)
  
  do.call(multiplot,c(plot.list,cols=3))
}




QCsampleMetrics <- function(project,layers,npoints="",s.type="q",outlier.rm=T,seed="d") {
  # project="ulisboa"  
  # project="Gadash_farm"
  # layers="30.12gadsh.h_deep_c1_0-98_sample_area.tif"
  # layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif")
  #layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif","conqueiros site1 v_05m_c1_0-99_conqueiros_site1.tif","conqueiros site1 v_1m_c1_0-99_conqueiros_site1.tif")
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  
  em.stack<-stack()
  for(layer in layers) {
    if (file.exists(paste0(projectsDir,project,"/",layer))) {
      r<-raster(paste0(projectsDir,project,"/",layer))
      em.stack<-stack(em.stack,r)
    } else {
      text(x = 0.5, y = 0.5, paste("file does not exist.\n"), cex = 1.6, col = "black")
      return();
    }
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-3:ncol(field.df)
  n.layers=length(layers)
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  
  
  if(file.exists(paste0(stagingDir,'current_QC_samples.RData'))){
    samples<-readRDS(paste0(stagingDir,'current_QC_samples.RData'))
  } else {
    text(x = 0.5, y = 0.5, paste("...\n"), cex = 1.2, col = "black")
    return();
  }
  
  
  
  theme.metrics <- theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black"))
  
    
  
  
  ### AIC - by sample size ###
  aic.names<-c(paste0("aic.",names(samples[[1]]))[d.cols],"n") #c("aic.ecv1","aic.ecv05","aic.ech1","aic.ech05","n")
  aic.res <- data.frame()#aic.ecv1=numeric(),aic.ecv05=numeric(),aic.ech1=numeric(),aic.ech05=numeric(),sid=character(),n=integer())
  
  for (i in 1:length(samples)){
    aic.row<-list()
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    
    for(j in d.cols){
      if(length(d.cols)>1){
        lm <- lm(as.formula(paste(colnames(samp)[j], "~",
                                  paste(colnames(samp)[d.cols[!d.cols %in% j]], collapse = "+"),
                                  sep = "")), 
                 data=samp)
        aic<-AIC(lm)
      } else {
        aic<-0
      }
      aic.row<-c(aic.row,aic)
    }
    
    newrow<-data.frame(c(aic.row,nrow(samp)))
    names(newrow)<-aic.names
    aic.res<-rbind(aic.res,newrow)
  }
  
  aic.res$aic.mean<-apply(aic.res[1:length(d.cols)],1,mean)
  aic.res$aic.max<-apply(aic.res[1:length(d.cols)],1,max)
  aic.res$aic.min<-apply(aic.res[1:length(d.cols)],1,min)
  aic.res$n<-as.factor(aic.res$n)
  
  pl.aic.all<-
    ggplot(aic.res) +  theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="AIC", title='AIC by sample size n') +
    #geom_point(aes_string("n",aic.names[1]),col="purple4",alpha=0.5,size=2) + 
    #geom_point(aes_string("n",aic.names[2]),col="navyblue",alpha=0.5,size=2) + 
    geom_point(aes(n,aic.mean),size=4,shape=1) + 
    geom_line(aes(n,aic.mean,group=1),linewidth=1,lty=1) +
    geom_vline(aes(xintercept=aic.res[which.min(aic.mean),"n"]),col="Red2",alpha=0.5) 
  
  ###
  
  
  
  
  ### D-KL - by sample size ###
  dkl.names<-c(paste0("dkl.",names(samples[[1]])[d.cols]),"n")
  dkl.res <- data.frame()
  bins=9
  
  # compute the entire field distribution
  dkl.full<-list()
  for(j in d.cols){
    field.j<-field.df[[j]]
    h.full <- hist(field.j, breaks = bins ,plot=FALSE)
    h.full$counts[h.full$counts==0]<-0.01
    d.full=h.full$counts/sum(h.full$counts)
    dkl.full<-c(dkl.full,list(d.full))
  }
  
  # compute samples distributions
  for (i in 1:length(samples)){
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    #bins<-nrow(samp)
    dkl.samp<-list()
    for(j in d.cols){
      samp.j<-samp[[j]]
      h.samp <- hist(samp.j, breaks = bins, plot=FALSE)
      h.samp$counts[h.samp$counts==0]<-0.01
      dkl.samp.j=h.samp$counts/sum(h.samp$counts)
      dkl.samp<-c(dkl.samp,list(dkl.samp.j))
    }
    
    kl.val<-list()
    for(k in 1:length(d.cols)){
      # compute d-KL of sample and field distributions
      kl <- signif(KL.plugin(dkl.full[[k]],dkl.samp[[k]]),3)
      kl.val <- c(kl.val,kl)
    }
    
    newrow<-data.frame(kl.val,nrow(samp))
    names(newrow)<-dkl.names
    #newrow<-data.frame(kl.ecv,kl.ech,kl.tir,sid,fid,n)
    dkl.res<-rbind(dkl.res,newrow)
  }
  
  dkl.res$dkl.mean<-apply(dkl.res[1:2],1,mean)
  dkl.res$n<-as.factor(dkl.res$n)
  
  pl.dkl.all<-
    ggplot(dkl.res,aes(n,kl.ecv1)) +  theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="D-KL", title='D-KL by sample size n') +
    #geom_point(aes_string("n",dkl.names[1]),col="purple4",alpha=0.5,size=2) + 
    #geom_point(aes_string("n",dkl.names[2]),col="navyblue",alpha=0.5,size=2) +
    geom_point(aes(n,dkl.mean),size=4,shape=1) + 
    geom_line(aes(n,dkl.mean,group=1),linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=dkl.res[which.min(dkl.mean),"n"]),col="Red2",alpha=0.5) 
  
  
  
  ### cLHS - by sample size ###
  
  clhs.res <- data.frame()
  data_continuous <- field.df[d.cols] # only ancillary data columns
  n_cols <- ncol(data_continuous)
  n_data <- nrow(data_continuous)
  
  # compute samples cLHS
  for (i in 1:length(samples)){
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    N <- nrow(samp)
    
    # Edge of the strata
    continuous_strata <- apply(data_continuous, 2, function(x) {quantile(field.df, probs = seq(0, 1, length.out = N + 1), na.rm = TRUE)})
    continuous_samp <- samp[d.cols]
    cont_data_strata <- lapply(1:n_cols, function(i) list(continuous_samp[, i], continuous_strata[, i]) )
    cont_obj_sampled <- lapply(cont_data_strata, function(x) hist(x[[1]], breaks = x[[2]], plot = FALSE)$counts)
    cont_obj_sampled <- matrix(unlist(cont_obj_sampled), ncol = n_cols, byrow = FALSE)
    
    delta_obj_continuous <- rowSums(abs(cont_obj_sampled - 1)) 
    clhs.val <- sum(delta_obj_continuous)/(N*n_cols) # cLHS - normalized 
    
    newrow<-data.frame(clhs.val,nrow(samp))
    names(newrow)<-c("cLHS","n")
    clhs.res<-rbind(clhs.res,newrow)
  }
  
  clhs.res$n<-as.factor(clhs.res$n)
  
  pl.clhs.all<-
    ggplot(clhs.res,aes(n,cLHS)) +  theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="cLHS", title='cLHS (normalized) by sample size n') +
    geom_point(aes(n,cLHS),col="purple4",alpha=0.5,size=2) + 
    geom_line(aes(n,cLHS,group=1),linewidth=1.2,lty=1) + 
    geom_vline(aes(xintercept=clhs.res[which.min(cLHS),"n"]),col="Red2",alpha=0.5) 
  
  ###
  
  
  
  ### Distance metric - by sample size ###
  dist.names<-c("cent.dist","n")
  dist.res <- data.frame()
  
  # compute samples average distance from centroids
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    newrow<-data.frame(mean(samp$cent.dist),nrow(samp))
    names(newrow)<-dist.names
    dist.res<-rbind(dist.res,newrow)
  }
  
  dist.res$n<-as.factor(dist.res$n)
  #print(dist.res)
  
  pl.dist.all<-
    ggplot(dist.res,aes(n,cent.dist)) + 
    
    scale_x_discrete() +
    labs( x="n",y="meters", title='Mean distance to centroid by sample size n') +
    geom_point(aes_string("n",dist.names[1]),col="purple4",alpha=0.5,size=2) +
    geom_line(aes(n,cent.dist,group=1),linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=dist.res[which.min(cent.dist),"n"]),col="Red2",alpha=0.5) 
  
  
  
  
  ### Variogram R^2 - by sample size ###
  vgm.names<-c("R2","n")
  vgm.res <- data.frame()
  
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    
    samp.sp<-samp
    coordinates(samp.sp) = ~x + y
    crs(samp.sp) <- CRS(utm)
    
    for(k in d.cols){
      v.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp)  # Semi-Variogram
      fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))  # Fit function to variogram
      vgm.R2 <- attr(fit.eca,"SSErr")
      
      newrow<-data.frame(vgm.R2,nrow(samp))
      names(newrow)<-vgm.names
      vgm.res<-rbind(vgm.res,newrow)
    }
    # params1<-paste0("model: ",fit.eca1[2,1],"    nugget: ",round(fit.eca1[1,2],2),"   sill: ",round(fit.eca1[2,2],2),"   range: ",range.eca1," meters")
    # 
    # Fitted1 <- data.frame(dist = seq(0.01, max(v.eca1$dist), length = 101))
    # Fitted1$gamma <- variogramLine(fit.eca1, dist_vector = Fitted1$dist)$gamma
    # 
    # ggplot(v.eca1, aes(x = dist, y = gamma)) + 
    #   geom_point(shape=1,size=2,col="blue") + 
    #   geom_line(data = Fitted1,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = names(df[3])) + 
    #   scale_x_continuous(limits = c(0, max(v.eca1$dist+(max(v.eca1$dist)/20))),expand = c(0, 0)) +
    #   scale_y_continuous(limits = c(0, max(c(v.eca1$gamma+(max(v.eca1$gamma)/5), Fitted1$gamma))),expand = c(0, 0))+
    #   annotate("text", x = (max(v.eca1$dist)), y=(max(v.eca1$gamma)+(max(v.eca1$gamma)/10)), label = params1,hjust=1)
    
    #vgm.R2 <- R2variogram(v.eca1,fit.eca1)
    
  }
  
  #dist.res$dkl.mean<-apply(dkl.res[1:2],1,mean)
  vgm.res$n<-as.factor(vgm.res$n)
  vgm.df <- as.data.frame(vgm.res %>%
                            group_by(n) %>%
                            dplyr::summarize(Mean = mean(R2, na.rm=TRUE))
  )
  
  pl.vgm.all<-
    ggplot(vgm.df,aes(n,Mean)) +  theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="", title='Variogram model fit - Mean Sum of Squared Errors (R^2) by sample size') +
    geom_point(aes_string("n","Mean"),col="purple3",alpha=0.5,size=2) +
    geom_line(aes(n,Mean,group=1),linewidth=1.2,lty=1,col="navy")+
    geom_vline(aes(xintercept=vgm.df[which.min(Mean),"n"]),col="Red2",alpha=0.5) 
  
  
  
  
  ## five-fold cross validation:
  
  cv.names<-c("n","ME","MSPE","MSNE","corObsPred","corObsResidual","X")
  cv.res <- data.frame()
  
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    
    samp.sp<-samp
    coordinates(samp.sp) = ~x + y
    crs(samp.sp) <- CRS(utm)
    
    for(k in d.cols){
      v.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp)  # Semi-Variogram
      fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))  # Fit function to variogram
      cv.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp,covariogram=T) # Co-variogram
      
      x <- krige.cv(eval(parse(text=names(samp[k])))~1, samp.sp, fit.eca, nmax = 40, nfold=5)
      #bubble(x, "residual", main = "ECa: 5-fold CV residuals")
      
      # mean error, ideally 0:
      me<-mean(x$residual)
      # MSPE, ideally small
      mspe<-mean(x$residual^2)
      # Mean square normalized error, ideally close to 1
      msne<-mean(x$zscore^2)
      # correlation observed and predicted, ideally 1
      cor.obs.pred<-cor(x$observed, x$observed - x$residual)
      # correlation predicted and residual, ideally 0
      cor.obs.residual<-cor(x$observed - x$residual, x$residual)
      
      #vgm.R2 <- attr(fit.eca,"SSErr")
      
      ## X (Glass, 2003)
      X = signif(
        sum(abs(1 - (v.eca$gamma + cv.eca[cv.eca$dist>0,"gamma"]) / var(samp[[3]])) * v.eca$np) / 
          sum(v.eca$np)
        ,2)
      
      newrow<-data.frame(nrow(samp),me,mspe,msne,cor.obs.pred,cor.obs.residual,X)
      names(newrow)<-cv.names
      cv.res<-rbind(cv.res,newrow)
    }
  }
  
  cv.res$n<-as.factor(cv.res$n)
  cv.df <- as.data.frame(cv.res %>%
                           group_by(n) %>%
                           dplyr::summarize(me = mean(ME, na.rm=TRUE),
                                            mspe = mean(mspe, na.rm=TRUE),
                                            msne = mean(MSNE, na.rm=TRUE),
                                            corObsPred = mean(corObsPred, na.rm=TRUE),
                                            corObsResidual = mean(corObsResidual, na.rm=TRUE),
                                            X = mean(X, na.rm=TRUE))
  )
  
  theme.cv<-
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black"))
  
  pl.cv.me<-
    ggplot(cv.df,aes(n,Mean)) + theme.cv + scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation - Mean Prediction Error') +
    geom_point(aes(n,me),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,me,group=1),col="blue4",linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=cv.df[which.min(me),"n"]),col="Red2",alpha=0.5)
  
  pl.cv.msne<-
    ggplot(cv.df,aes(n,msne)) + theme.cv + scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation MSNE - Mean Squared Normalized Error') +
    geom_point(aes(n,msne),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,msne,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(msne),"n"]),col="Red2",alpha=0.5)
  
  
  pl.cv.mspe<-
    ggplot(cv.df,aes(n,msne)) + 
    theme.cv +
    scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation MSPE - Mean Squared Prediction Error') +
    geom_point(aes(n,mspe),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,mspe,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(mspe),"n"]),col="Red2",alpha=0.5)
  
  
  pl.cv.cor<-
    ggplot(cv.df,aes(n,msne)) + theme.cv +  scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation - Correlation: Observed ~ Predicted') +
    geom_point(aes(n,corObsPred),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,corObsPred,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(corObsPred),"n"]),col="Red2",alpha=0.5)
  
  pl.cv.X<-
    ggplot(cv.df,aes(n,X)) + theme.cv +  scale_x_discrete() +
    labs( x="n",y="", title='X - variogram quality index') +
    geom_point(aes(n,X),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,X,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(X),"n"]),col="Red2",alpha=0.5)
  
  # geom_point(aes(n,msne),col="red3",alpha=0.5,size=2) +
  # geom_line(aes(n,msne,group=1),col="red4",size=1.2,lty=1) +
  #  geom_point(aes(n,mspe),col="blue3",alpha=0.5,size=2) +
  #  geom_line(aes(n,mspe,group=1),col="blue4",size=1.2,lty=1) +
  #   geom_point(aes(n,corObsPred),col="green3",alpha=0.5,size=2) +
  #   geom_line(aes(n,corObsPred,group=1),col="green4",size=1.2,lty=1) 
  # geom_point(aes(n,corObsResidual),col="darkgoldenrod1",alpha=0.5,size=2) +
  # geom_line(aes(n,corObsResidual,group=1),col="darkgoldenrod3",size=1.2,lty=1)
  
  
  

  multiplot(pl.cv.X,pl.aic.all,pl.dkl.all,pl.clhs.all,pl.dist.all,pl.vgm.all,pl.cv.me,pl.cv.msne,pl.cv.mspe,pl.cv.cor)
}








######################
#  QC2023 - Sample design  #

QC23sampleDesign <- function(project,layers,n.points.min=10,n.points.max=20,outlier.rm=T,seed="deterministic",low.var=10,extra.points=T) {
  # project="ulisboa"
  # layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif")
  
  # project="sasa"
  # layers="ECa H 05m.tif"
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  if(n.points.min==0 | n.points.max==0){
    text(x = 0.5, y = 0.9, paste("Select points range...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  if(n.points.min>=n.points.max){
    text(x = 0.5, y = 0.9, paste("Select positive points range...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  
  em.stack<-stack()
  for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
    em.stack<-stack(em.stack,r)
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-names(field.df[-c(1:2)])
  #n.layers=length(layers)
  
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  sampling.schemes<-list()
  centroids<-list()
  q<-1
  
  points.range <- as.numeric(n.points.max) - as.numeric(n.points.min)
  step.size <- 1
  if(points.range>14) { step.size <- 2 }
  if(points.range>28) { step.size <- 3 }
  if(points.range>42) { step.size <- 4 }
  if(points.range>63) { step.size <- 10 }
  n.points<-seq(n.points.min,n.points.max,step.size)
  
  
  for(N in n.points){
    n.clust<-N
    
    # Clusters by Geometry
    if(seed!="random") { set.seed(1234) } else { set.seed(NULL) }
    kc <- kmeans(field.df[1:2],n.clust,100000, algorithm="MacQueen")
    field.df$kc <- kc$cluster
    field.df[[paste0("kc.",N)]] <- field.df$kc
    field.df$outlier <- 0
    
    centers <- as.data.frame(kc$centers)
    centers$kc<-1:N
    centroids<-c(centroids,list(centers)) # add to list of plans by N
    
    # palcm<-colorRampPalette(brewer.pal(9,"Spectral"))(n.clust)
    # ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc))) + geom_point(data=centroids[[1]],aes(x,y),color="black",size=1.2) +
    #   coord_equal() + theme_bw() + theme(plot.title = element_text(hjust = 0.5,size=12)) + labs( y="Northing", x="Easting", title=paste0("Geometric Clusters"), caption = "") + scale_fill_manual(values=palcm,name="cluster",labels=c(1:n.clust))
    
    
    ### Feature Space Quantiles - by ECa first layer
    field.df$kc.eca <-  ntile(field.df[[3]],N)
    s.type.str="quantile"
    
    field.temp<-data.frame()
    for(o in 1:length(unique(field.df$kc.eca))){
      v.quantile <- field.df[field.df$kc.eca==o,]
      # mark outliers
      v.quantile[is.na(remove_outliers(v.quantile[[3]])),"outlier"]<-1 # keep only inter-quantile range (0.25,0.75)
      field.temp<-rbind(field.temp,v.quantile)
    }
    field.df<-field.df.uni<-field.temp

    # filter by low ECa variance 
    if(low.var!=0){
      r.var<-raster(paste0(projectsDir,project,"/variance/",substring(layer,1,nchar(layer)-4),"_variance.tif"))
      var.limit <- raster::quantile(r.var,  probs = as.numeric(low.var)/100)
      #r.var[r.var[] > var.limit ] = NA
      #typeof(r.var[])
      #plot(r.var)
      df.var<-as.data.frame(rasterToPoints(r.var))
      names(df.var)[3]<-"var"
      field.df.uni <- join(field.df,df.var,by=c("x","y"))
      field.df.uni <- field.df.uni[as.numeric(field.df.uni$var)<var.limit,]
    } else {
      field.df.uni$var<-NA
    }
        
    ### sort geo clusters by number of feature clusters - ASC
    c.rank <- field.df.uni %>% dplyr::count(kc, kc.eca, sort = F)
    num.features <- c.rank %>% dplyr::count(kc, sort = T)
    
    valid=0
    j=1
    # 
    while(valid==0){
      sampling.n<-data.frame()
      unsampled <- 1:N
      #  print(paste("iter",j))
      valid=1
      #cls.all.df<-data.frame()
      
      for(i in rev(num.features$kc)){
        field.i <- field.df.uni[field.df.uni$kc==i,] # geo cluster i
        if(!is.null(outlier.rm) & outlier.rm==T){ field.i=field.i[field.i$outlier==0,] } # remove outliers
        feasible <- intersect(unique(field.i$kc.eca),unsampled) # intersection set of un-sampled feature space points in zone i
        
        cent.i<-t(as.matrix(as.numeric(centers[i,]))) # centroid i coordinates
        points.i<-as.matrix(field.i[,1:2]) # coordinates of points in zone i
        diff.center <- sweep(points.i, 2, cent.i[1:2], check.margin=F) # subtract point aa from each point in bb
        field.i$cent.dist <- sqrt(rowSums(diff.center^2))
        center.idx <- which.min(field.i$cent.dist) # index of nearest point in i to centroid
        
        if(length(feasible) < 1){ # if no unsampled quantiles in current cluster
          print(paste("centeroid feature cluster:",field.i[center.idx,"kc.eca"]))
          print(paste0("no unique options... N=",N," | geo-cluster=",i," --> choosing centroid..."))
          pnt <- field.i[center.idx,]
        } else {
          field.cls <- field.i[field.i$kc.eca %in% feasible,] # include only unsampled quantiles
          # select closest point among unsampled clusters
          nearest.idx <- which.min(field.cls$cent.dist)
          pnt <- field.cls[nearest.idx,] # nearest point row
          feature.cls<-pnt$kc.eca
          unsampled <- unsampled[!unsampled %in% feature.cls] # update unsampled
        }
        sampling.n<-rbind(sampling.n,pnt) # add to sampling plan
        #cls.all.df<-rbind(cls.all.df,field.cls) # 
      }
      
      j<-j+1 
    }
    
    if(extra.points==T){ # Additional random points at close range
      max.dist <- ceiling(min(dist(centers[1:2]),na.rm = T)/3)
      min.dist <- ifelse(max.dist<10, 2, 5) 
      n.extra <- ceiling(N/10)

      pvts <-sample(1:nrow(sampling.n),n.extra)
      for(ep in pvts){
        pvt <- sampling.n[ep,]
        x.seq <- c(seq(pvt$x-max.dist,pvt$x-min.dist,1),seq((pvt$x+min.dist),pvt$x+max.dist,1))
        y.seq <- c(seq(pvt$y-max.dist,pvt$y-min.dist,1),seq((pvt$y+min.dist),pvt$y+max.dist,1))
        
        options.grid <- expand.grid(x=x.seq,y=y.seq)
        supp.coords <- options.grid[sample(1:nrow(options.grid),1),]
        supp.point <- join(supp.coords,field.df.uni,by=c("x","y"))
        diff.center <- sweep(points.i, 2, cent.i, check.margin=F) # subtract point aa from each point in bb
        field.i$cent.dist <- sqrt(rowSums(diff.center^2))
        supp.point[3] <- field.df[field.df$x==supp.point$x&field.df$y==supp.point$y,3]
        if(is.na(supp.point[3])){supp.point[3] <- mean(field.df[,3],na.rm=T)}
        supp.point$cent.dist <- NA
        supp.point$var <- NA
        sampling.n <- rbind(sampling.n,supp.point)
      }
    }
    
    Ns <- nrow(sampling.n)
    
    sampling.schemes<-c(sampling.schemes,list(sampling.n)) # add to list of plans by N
    
    pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(N)
    theme.map <- theme(legend.position="left",plot.title = element_text(hjust = 0,size=10),axis.title = element_text(size=9),axis.text = element_text(size=8),axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
                       legend.title = element_text(size=9),legend.text = element_text(size=8),legend.key.width = unit(0.6,"cm"),legend.key.height = unit(0.2,"cm"),legend.spacing.y=unit(0,"cm"))
    
    assign(paste0("pk.",N),
           ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=factor(kc)),alpha=0.7) + 
             geom_point(data=sampling.n,aes(x,y),size=1.2,shape=21,fill="white")+
             geom_point(data=centroids[[q]],aes(x,y),size=0.6,shape=3,alpha=0.7) +
             coord_equal() + theme_bw() + theme.map +
             labs( y="Northing", x="Easting", title=paste(Ns,"points,",N, "Geo clusters")) + 
             scale_fill_manual(values=pal.eca,name="cluster") 
           #title=paste("Geo clusters -",N,"points"))
           #geom_text(data=centroids[[q]],aes(x-2,y-2),label=centroids[[q]]$kc,size=2)
    )
    
    assign(paste0("pk.eca.",N),
           ggplot(field.df.uni,aes(x,y)) + geom_tile(aes(fill=factor(kc.eca)),alpha=0.7) +
             #geom_point(data=centroids[[q]],aes(x,y),size=1.5,shape=3) +
             geom_point(data=sampling.n,aes(x,y),size=1.1,shape=21,fill="white",stroke=0.5)+
             coord_equal() + theme_bw() + theme.map +
             labs( y="Northing", x="Easting", title=paste0(Ns," points, ",N, " ECa ",s.type.str,"s")) + 
             scale_fill_manual(values=pal.eca,name="quantile") #+
           # title=paste0("ECa ",s.type.str,"s - ",N," points"))
           #geom_text(data=sampling.n,aes(x-1,y-1),label=sampling.n$id,size=2)
    )
    
    # boxplot - ECa by feature cluster
    pal.box <- c("slateblue2",brewer.pal(length(d.cols)+1,"Set2"))
    p.box <- ggplot(field.df, aes(x=reorder(kc.eca, d.cols[i]), mean)) + theme_bw() + scale_x_continuous(breaks = 1:N) +
      theme(plot.title = element_text(size=10),axis.title = element_text(size=9))
    for(i in length(d.cols):1){
      p.box <- p.box + geom_boxplot(aes_string("kc.eca",d.cols[i],group="kc.eca"),lwd=0.3,outlier.size=0.5,width=0.25,outlier.alpha = 0.5,col=pal.box[i],fill="transparent")
    }
    p.box <- p.box + labs(title=paste("Distribution by",s.type.str),x=paste(s.type.str),y="ECa (normalized)") + #,subtitle=paste0("n=",N))+
      geom_point(data=sampling.n,aes_string("kc.eca",names(sampling.n)[3]),col="black",size=1)
    #p.box
    assign(paste0("p.sample.",N),p.box)
    
    print("***")
    print(paste0("Done N=",N," rows=",nrow(sampling.n)))
    print("***")
    
    q<-q+1
    
  } # end sample-size loop
  
  
  # Plot selected samples
  ### Geo clusters
  plot.list=list()
  
  for(p in n.points){ # plot geometric clusters
    pl<-list(eval(parse(text=paste0("pk.",p))))
    plot.list<-c(plot.list,pl)
  }
  
  for(p in n.points){ # plot Feature space clusters
    pl<-list(eval(parse(text=paste0("pk.eca.",p))))
    plot.list<-c(plot.list,pl)
  }
  
  for(p in n.points){ # box plot - ECa distribution
    pl<-list(eval(parse(text=paste0("p.sample.",p))))
    plot.list<-c(plot.list,pl)
  }
  
  sampling.csv=paste0(stagingDir,"QC23_sampling_plans_results.csv") #paste0("sampling_plans_",min(n.points),"-",max(n.points),".csv")
  unlink(sampling.csv)
  
  #sampling.schemes <- lapply(sampling.schemes,function(x) as.character(x)) # convert to char
  suppressWarnings(lapply(sampling.schemes, function(x) write.table( data.frame(x), sampling.csv , append= T, sep=',', row.names=F)))
  #Sys.chmod(sampling.csv, "777", use_umask = FALSE)
  
  saveRDS(sampling.schemes, paste0(stagingDir,'current_QC_samples.RData'))
  Sys.chmod(paste0(stagingDir,'current_QC_samples.RData'), "777", use_umask = FALSE)
  
  # metrics.plots <- QC23sampleMetrics(project,layers,"",outlier.rm,seed,low.var,extra.points)
  # plot.list <- c(plot.list,metrics.plots)
  do.call(multiplot,c(plot.list,cols=3))
}




QC23sampleMetrics <- function(project,layers) {
  # project="ulisboa"  
  # project="Gadash_farm"
  # layers="30.12gadsh.h_deep_c1_0-98_sample_area.tif"
  # layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif")
  #layers=c("conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif","conqueiros site1 h_1m_c1_0-98_conqueiros_site1.tif","conqueiros site1 v_05m_c1_0-99_conqueiros_site1.tif","conqueiros site1 v_1m_c1_0-99_conqueiros_site1.tif")
  
  # project="sasa"
  # layers="Sasa 04-23 H_05m_c5_0-100_perimeter_sasa_log.tif"
  
  if(length(layers)<1){
    text(x = 0.5, y = 0.5, paste("Select input layers...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  
  em.stack<-stack()
  for(layer in layers) {
    if (file.exists(paste0(projectsDir,project,"/",layer))) {
      r<-raster(paste0(projectsDir,project,"/",layer))
      em.stack<-stack(em.stack,r)
    } else {
      text(x = 0.5, y = 0.5, paste("file does not exist.\n"), cex = 1.6, col = "black")
      return();
    }
  }
  
  field.df <- as.data.frame(rasterToPoints(em.stack))
  d.cols<-3:ncol(field.df)
  n.layers=length(layers)
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  
  
  if(file.exists(paste0(stagingDir,'current_QC_samples.RData'))){
    samples<-readRDS(paste0(stagingDir,'current_QC_samples.RData'))
  } else {
    text(x = 0.5, y = 0.5, paste("...\n"), cex = 1.2, col = "black")
    return();
  }
  
  theme.metrics <- theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black"))
  
  
  
  
  
  
  
  ### D-KL - by sample size ###
  dkl.names<-c(paste0("dkl.",names(samples[[1]])[d.cols]),"n")
  dkl.res <- data.frame()
  bins=50
  
  
  # compute the entire field distribution
  dkl.full<-list()
  for(j in d.cols){
    field.j<-field.df[[j]]
    h.full <- hist(field.j, breaks = seq(0, 1, length.out = bins) ,plot=FALSE)
    h.full$counts[h.full$counts==0]<-0.0001
    d.full=h.full$counts/sum(h.full$counts)
    dkl.full<-c(dkl.full,list(d.full))
  }
  
  # compute samples distributions
  for (i in 1:length(samples)){
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    bins<-round(nrow(samp)/2)
    dkl.samp<-list()
    for(j in d.cols){
      samp.j<-samp[[j]]
      h.samp <- hist(samp.j, breaks = seq(0, 1, length.out = bins), plot=FALSE)
      h.samp$counts[h.samp$counts==0]<-0.0001
      dkl.samp.j=h.samp$counts/sum(h.samp$counts)
      dkl.samp<-c(dkl.samp,list(dkl.samp.j))
    }
    
    kl.val<-list()
    for(k in 1:length(d.cols)){
      # compute d-KL of sample and field distributions
      a.full<-dkl.full[[k]]
      a.samp<-dkl.samp[[k]]
      kl <- signif(KL.plugin(dkl.full[[k]],dkl.samp[[k]]),3)
      kl.val <- c(kl.val,kl)
    }
    
    newrow<-data.frame(kl.val,nrow(samp))
    names(newrow)<-dkl.names
    #newrow<-data.frame(kl.ecv,kl.ech,kl.tir,sid,fid,n)
    dkl.res<-rbind(dkl.res,newrow)
  }
  
  dkl.res$dkl.mean<-apply(dkl.res[1:length(d.cols)],1,mean)
  dkl.res$n<-as.factor(dkl.res$n)
  
  pl.dkl.all<-
    ggplot(dkl.res,aes(n,kl.ecv1)) +  theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="D-KL", title='D-KL') +
    #geom_point(aes_string("n",dkl.names[1]),col="purple4",alpha=0.5,size=2) + 
    #geom_point(aes_string("n",dkl.names[2]),col="navyblue",alpha=0.5,size=2) +
    geom_point(aes(n,dkl.mean),size=4,shape=1) + 
    geom_line(aes(n,dkl.mean,group=1),linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=dkl.res[which.min(dkl.mean),"n"]),col="Red2",alpha=0.5) 
  
  
  
  ### cLHS - by sample size ###
  
  clhs.res <- data.frame()
  data_continuous <- field.df[d.cols] # only ancillary data columns
  n_cols <- ncol(data_continuous)
  n_data <- nrow(data_continuous)
  
  # compute samples cLHS
  for (i in 1:length(samples)){
    samp<-samples[[i]] #eval(parse(text=paste0("sample",all.fronts[i,"sampleID"])))
    N <- nrow(samp)
    
    # Edge of the strata
    continuous_strata <- apply(data_continuous, 2, function(x) {quantile(field.df[d.cols], probs = seq(0, 1, length.out = N + 1), na.rm = TRUE)})
    continuous_samp <- samp[d.cols]
    cont_data_strata <- lapply(1:n_cols, function(i) list(continuous_samp[, i], continuous_strata[, i]) )
    cont_obj_sampled <- lapply(cont_data_strata, function(x) hist(x[[1]], breaks = x[[2]], plot = FALSE)$counts)
    cont_obj_sampled <- matrix(unlist(cont_obj_sampled), ncol = n_cols, byrow = FALSE)
    
    delta_obj_continuous <- rowSums(abs(cont_obj_sampled - 1)) 
    clhs.val <- sum(delta_obj_continuous)/(N*n_cols) # cLHS - normalized 
    
    newrow<-data.frame(clhs.val,nrow(samp))
    names(newrow)<-c("cLHS","n")
    clhs.res<-rbind(clhs.res,newrow)
  }
  
  clhs.res$n<-as.factor(clhs.res$n)
  
  pl.clhs.all<-
    ggplot(clhs.res,aes(n,cLHS)) +  theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="cLHS", title='cLHS') +
    geom_point(aes(n,cLHS),col="purple4",alpha=0.5,size=2) + 
    geom_line(aes(n,cLHS,group=1),linewidth=1.2,lty=1) + 
    geom_vline(aes(xintercept=clhs.res[which.min(cLHS),"n"]),col="Red2",alpha=0.5) 
  
  ###
  
  
  
  ### Distance metric - by sample size ###
  dist.names<-c("cent.dist","n")
  dist.res <- data.frame()
  
  # compute samples average distance from centroids
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    newrow<-data.frame(mean(samp$cent.dist,na.rm=T),nrow(samp))
    names(newrow)<-dist.names
    dist.res<-rbind(dist.res,newrow)
  }
  
  dist.res$n<-as.factor(dist.res$n)
  #print(dist.res)
  
  pl.dist.all<-
    ggplot(dist.res,aes(n,cent.dist)) + theme.metrics +
    scale_x_discrete() +
    labs( x="n",y="meters", title='Mean distance to centroid') +
    geom_point(aes_string("n",dist.names[1]),col="purple4",alpha=0.5,size=2) +
    geom_line(aes(n,cent.dist,group=1),linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=dist.res[which.min(cent.dist),"n"]),col="Red2",alpha=0.5) 
  
  
  ## five-fold cross validation:
  
  cv.names<-c("n","ME","MSPE","MSNE","corObsPred","corObsResidual","X")
  cv.res <- data.frame()
  
  for (i in 1:length(samples)){
    samp<-samples[[i]] 
    
    samp.sp<-samp
    coordinates(samp.sp) = ~x + y
    crs(samp.sp) <- CRS(utm)
    
    for(k in d.cols){
      v.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp)  # Semi-Variogram
      fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))  # Fit function to variogram
      if(fit.eca$range[2]<0){fit.eca$range[2]=1}
      cv.eca = variogram(eval(parse(text=names(samp[k])))~1, samp.sp,covariogram=T) # Co-variogram
      
      x <- krige.cv(eval(parse(text=names(samp[k])))~1, samp.sp, fit.eca, nmax = 40, nfold=5)
      #bubble(x, "residual", main = "ECa: 5-fold CV residuals")
      
      # mean error, ideally 0:
      me<-mean(x$residual)
      # MSPE, ideally small
      mspe<-mean(x$residual^2)
      # Mean square normalized error, ideally close to 1
      msne<-mean(x$zscore^2)
      # correlation observed and predicted, ideally 1
      cor.obs.pred<-cor(x$observed, x$observed - x$residual)
      # correlation predicted and residual, ideally 0
      cor.obs.residual<-cor(x$observed - x$residual, x$residual)
      
      #vgm.R2 <- attr(fit.eca,"SSErr")
      
      ## X (Glass, 2003)
      X = signif(
        sum(abs(1 - (v.eca$gamma + cv.eca[cv.eca$dist>0,"gamma"]) / var(samp[[3]])) * v.eca$np) / 
          sum(v.eca$np)
        ,2)
      
      newrow<-data.frame(nrow(samp),me,mspe,msne,cor.obs.pred,cor.obs.residual,X)
      names(newrow)<-cv.names
      cv.res<-rbind(cv.res,newrow)
    }
  }
  
  cv.res$n<-as.factor(cv.res$n)
  cv.df <- as.data.frame(cv.res %>%
                           group_by(n) %>%
                           dplyr::summarize(me = mean(ME, na.rm=TRUE),
                                            mspe = mean(MSPE, na.rm=TRUE),
                                            msne = mean(MSNE, na.rm=TRUE),
                                            corObsPred = mean(corObsPred, na.rm=TRUE),
                                            corObsResidual = mean(corObsResidual, na.rm=TRUE),
                                            X = mean(X, na.rm=TRUE))
  )
  
  theme.cv<-
    theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle = element_text(size=10,hjust = 0.5),axis.text=element_text(size=12),panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey50"),panel.border = element_rect(size=0.3, fill = NA,colour = "black"))
  
  pl.cv.me<-
    ggplot(cv.df,aes(n,Mean)) + theme.cv + scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation - Mean Prediction Error') +
    geom_point(aes(n,me),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,me,group=1),col="blue4",linewidth=1.2,lty=1)+
    geom_vline(aes(xintercept=cv.df[which.min(me),"n"]),col="Red2",alpha=0.5)
  
  pl.cv.msne<-
    ggplot(cv.df,aes(n,msne)) + theme.cv + scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation MSNE - Mean Squared Normalized Error') +
    geom_point(aes(n,msne),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,msne,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(msne),"n"]),col="Red2",alpha=0.5)
  
  pl.cv.mspe<-
    ggplot(cv.df,aes(n,mspe)) + 
    theme.cv +
    scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation MSPE - Mean Squared Prediction Error') +
    geom_point(aes(n,mspe),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,mspe,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(mspe),"n"]),col="Red2",alpha=0.5)
  
  pl.cv.cor<-
    ggplot(cv.df,aes(n,msne)) + theme.cv +  scale_x_discrete() +
    labs( x="n",y="", title='Cross Validation - Correlation: Observed ~ Predicted') +
    geom_point(aes(n,corObsPred),col="blue3",alpha=0.5,size=2) +
    geom_line(aes(n,corObsPred,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.max(corObsPred),"n"]),col="Red2",alpha=0.5)
  
  pl.cv.X<-
    ggplot(cv.df,aes(n,X)) + theme.cv +  scale_x_discrete() +
    labs( x="n",y="", title='X - variogram quality index') +
    geom_point(aes(n,X),col="blue3",alpha=0.5,linewidth=2) +
    geom_line(aes(n,X,group=1),col="blue4",linewidth=1.2,lty=1) +
    geom_vline(aes(xintercept=cv.df[which.min(X),"n"]),col="Red2",alpha=0.5)
  
  # geom_point(aes(n,msne),col="red3",alpha=0.5,size=2) +
  # geom_line(aes(n,msne,group=1),col="red4",size=1.2,lty=1) +
  #  geom_point(aes(n,mspe),col="blue3",alpha=0.5,size=2) +
  #  geom_line(aes(n,mspe,group=1),col="blue4",size=1.2,lty=1) +
  #   geom_point(aes(n,corObsPred),col="green3",alpha=0.5,size=2) +
  #   geom_line(aes(n,corObsPred,group=1),col="green4",size=1.2,lty=1) 
  # geom_point(aes(n,corObsResidual),col="darkgoldenrod1",alpha=0.5,size=2) +
  # geom_line(aes(n,corObsResidual,group=1),col="darkgoldenrod3",size=1.2,lty=1)

  
#  plots <- list(pl.cv.X,pl.dkl.all,pl.clhs.all,pl.dist.all,pl.cv.me,pl.cv.msne,pl.cv.mspe,pl.cv.cor)
  #return(plots)
  multiplot(pl.cv.X,pl.dkl.all,pl.clhs.all,pl.dist.all,pl.cv.me,pl.cv.msne,pl.cv.mspe,pl.cv.cor)
}






















vgm.q <- function(rawFile,compactFactor=10,trimLeft=0,trimRight=1) {
    #rawFile="30.12gadsh.h.csv"
    df.full <- loadECa(rawFile,"space")
    
    # Trim by lower and upper bound
    trimRight<-as.numeric(trimRight)
    trimLeft<-as.numeric(trimLeft)
    df.full <- as.data.frame(df.full[df.full[,3]<quantile(df.full[,3],trimRight) & df.full[,3]>quantile(df.full[,3],trimLeft),])
    
    # Compact data
    df<-compactEca(df.full,compactFactor)
    N=nrow(df)
    
    sp<-df
    coordinates(sp) = ~x + y
    crs(sp) <- CRS(utm)
    
    # Semi-Variogram
v.eca1 = variogram(eval(parse(text=names(df[3])))~1, sp)

# Fit function to variogram
fit.eca1<-fit.variogram(v.eca1, vgm(c("Exp", "Mat", "Sph")))

  range.eca1<-round(fit.eca1[2,3],2)
  
  # Plot each variogram
  params1<-paste0("model: ",fit.eca1[2,1],"    nugget: ",round(fit.eca1[1,2],2),"   sill: ",round(fit.eca1[2,2],2),"   range: ",range.eca1," meters")
  
  
  Fitted1 <- data.frame(dist = seq(0.01, max(v.eca1$dist), length = 101))
  Fitted1$gamma <- variogramLine(fit.eca1, dist_vector = Fitted1$dist)$gamma
  
    ggplot(v.eca1, aes(x = dist, y = gamma)) + 
    geom_point(shape=1,size=2,col="blue") + 
    geom_line(data = Fitted1,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = names(df[3])) + 
    scale_x_continuous(limits = c(0, max(v.eca1$dist+(max(v.eca1$dist)/20))),expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(c(v.eca1$gamma+(max(v.eca1$gamma)/5), Fitted1$gamma))),expand = c(0, 0))+
    annotate("text", x = (max(v.eca1$dist)), y=(max(v.eca1$gamma)+(max(v.eca1$gamma)/10)), label = params1,hjust=1)
  
    
    # GLS residual variogram:
    
    
    R2variogram(v.eca1,fit.eca1)
    
    
    
}


# GLS residual variogram
R2variogram<-function(s.v,vr.fit){
  # s.v  - sample variogram
  
  SSErr<-attr(vr.fit,"SSErr")
  summary(vr.fit)
  ## SStot total sum of weighted squares
  
  weig<-s.v$np/s.v$dist^2
  
  #fit.method  fitting method, used by gstat. The default method uses
  #weights $N_h/h^2$ with $N_h$ the number of point pairs and $h$ the
  #distance.
  
  (SStot<- sum(weig*(s.v$gamma-mean(s.v$gamma))^2))
  R2<-1-SSErr/SStot
  
  return(R2)
}






###
#################### 
# X - Sampling plan design

variogramSample <- function(project,layer,n.points=120,v.model="auto", nugget=0.01, sill=0.06,range=30,samp.method="clusters",data.type="eca",to.plot="sample") {
  #n.points=120
  # project="ulisboa"  
  # layer="conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif"
  # project="Gadash_farm"
  # layer="30.12gadsh.h_deep_c1_0-98_sample_area.tif"
  
  if(is.null(layer) || layer==""){
    text(x = 0.5, y = 0.5, paste("Select input layer...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  #em.stack<-stack()
  #for(layer in layers) {
    r<-raster(paste0(projectsDir,project,"/",layer))
   # em.stack<-stack(em.stack,r)
  #}
    
  if(data.type=="uniform"){ # syntetic data
    r<-setValues(r,runif(ncell(r)))
  }
  
  if(data.type=="normal"){ # syntetic data
    r<-setValues(r,rnorm(ncell(r)))
  }
  
  field.df <- as.data.frame(rasterToPoints(r))
  d.cols<-names(field.df[-c(1:2)])

  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  field.df$eca <- field.df[[3]]
  field.df$eca.q <- as.factor(ntile(field.df[[3]],as.numeric(n.points)))
  
  
  xss<-data.frame(matrix(ncol=2,nrow=0))
  names(xss)<-c("sample.size","X")

  if(samp.method=="grid"){ # Grid sampling

    field.r <- rasterFromXYZ(field.df[1:3])    
    #plot(field.r)
    
    n.factor <- round(as.numeric(n.points)/12)
    new.res <- raster(nrow=n.factor, ncol=n.factor,ext=extent(field.r)) 
    resampled <- resample(field.r, new.res)
    
    points.all <- rasterToPoints(resampled, na.rm = TRUE,spatial=T)
    points <- raster::intersect(points.all,resampled)
    #plot(resampled)
    #points(points,pch=19,cex=0.4)
    num.points=n.points.max=nrow(points)
    field.df$eca <- as.factor(ntile(field.df[[3]],as.numeric(num.points)))
    
    points.df <- as.data.frame(rasterToPoints(resampled,spatial = F)) %>% arrange(x,y)
    points.df$id <- 1:num.points
    
    pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(n.points.max)
    
    denoms <- c(1,1.5,2,3,4,5,7,9)
    for(m in 1:8) {
      #sub.points.df <- sub.points <- points.df[points.df$id%%(2^m)==0,]
      if(denoms[m]==1.5){
        sub.points.df <- sub.points <- points.df[points.df$id%%denoms[m]!=0,]
      } else {
        sub.points.df <- sub.points <- points.df[points.df$id%%denoms[m]==0,]
      }
      num.points=nrow(sub.points.df)
      print(num.points)
    
    p.sample <- 
      ggplot(field.df ,aes(x,y)) + geom_tile(aes(fill=eca),lwd=0,alpha=0.5) +
      geom_point(data=sub.points.df,aes(x,y),size=0.8,shape=21,fill="white",alpha=0.9)+
      coord_equal() + theme_bw() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=11),axis.title = element_text(size=10)) + 
      labs( y="Northing", x="Easting", title=paste0("Sample - ",num.points," points")) + scale_fill_manual(values=pal.eca,name="",labels=c(1:10)) #+
    #geom_text(data=sampling.n,aes(x-1,y-1),label=sampling.n$id,size=2)
    
    coordinates(sub.points) = ~x + y
    crs(sub.points) <- CRS(utm)
    
    # Co-variogram
    cv.eca = variogram(eval(parse(text=names(sub.points)[1]))~1, sub.points,covariogram=T)
    
    # Semi-Variogram
    v.eca = variogram(eval(parse(text=names(sub.points)[1]))~1, sub.points)
    if(sill==0){sill=NA}
    if(is.na(range) || range==0){range=NA}
    if(v.model[1]=="auto"){v.model=c("Exp", "Mat", "Sph")}
    
    # Fit function to variogram
    #fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))
    fit.eca<-fit.variogram(v.eca, vgm(model=v.model, nugget=nugget, psill=sill, range=range))
    range.eca<-round(fit.eca$range,2)
    params<-paste0("model: ",fit.eca$model,"    nugget: ",nugget,"   sill: ",round(fit.eca$psill,2),"   range: ",range.eca," meters")
    
    Fitted <- data.frame(dist = seq(0.01, max(v.eca$dist), length = 101))
    Fitted$gamma <- variogramLine(fit.eca, dist_vector = Fitted$dist)$gamma
    
    # Variogram cloud - X.hat
    v.eca.cloud = variogram(eval(parse(text=names(sub.points)[1]))~1, sub.points, cloud=T)

    # Eq 12
    X.hat = signif(
      sum(abs(1 - (v.eca$gamma + cv.eca[cv.eca$dist>0,"gamma"]) / var(sub.points.df[[3]])) * v.eca$np) / 
        sum(v.eca$np)
      ,2)
    
    # # Eq 13 - old
    # X.hat = round(
    #   sum(
    #     abs(
    #       cov(v.eca$dist,v.eca$gamma) / var(v.eca$gamma)
    #     ) * v.eca$np
    #   ) / 
    #     sum(v.eca$np)
    #   ,2)
    # 
    # 
    
    p.v <- 
      ggplot(v.eca, aes(x = dist, y = gamma)) + 
      geom_point(data=v.eca.cloud,aes(x=dist, y=gamma),shape=1,size=1,col="skyblue2",alpha=0.6) + 
      geom_point(shape=21,size=2,col="blue",fill="blue") + 
      geom_line(data = Fitted,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = paste0("ECa variogram - ",num.points," points")) + 
                                                                #caption = paste0("N=",n.points,"\n")) + 
      scale_x_continuous(limits = c(0, max(v.eca$dist+(max(v.eca$dist)/20))),expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, max(c(v.eca$gamma+(max(v.eca$gamma)/5), Fitted$gamma))),expand = c(0, 0))+
      annotate("text", x = (max(v.eca$dist)), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = params,hjust=1) +
      #annotate("text", x = (2), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = paste0("X = ",X.hat) ,hjust=0)
      annotate("text", x = (1), y=(max(v.eca$gamma)), label = unname(TeX(paste0("$\\bar{\\chi}$ = ",X.hat))), parse=TRUE ,hjust=0)
    
    assign(paste0("p.sample.",(m)),p.sample)
    assign(paste0("p.v.",(m)),p.v)
    
    
    new.xss.row<-data.frame(sample.size=num.points,X=X.hat)
    xss<-rbind(xss,new.xss.row)
    }
    plots.list <- as.list(1:8)
    
  
  } else { # clusters centroids
  
  sample.size <- as.numeric(n.points)
  plots.list<-list()
  while(sample.size>1){
    set.seed(1234)  
    kc <- kmeans(field.df[1:2],sample.size,100000, algorithm="MacQueen")
    field.df$kc <- kc$cluster
    field.df$eca.q <- as.factor(ntile(field.df[[3]],as.numeric(sample.size)))
  
    centers <- as.data.frame(kc$centers)
    centers <- arrange(centers,y,x)
    centers$id<-paste0("o",1:sample.size)
    
    centers.nearest<-centers
    centers.nearest$x <-round(centers$x)
    centers.nearest$y <-round(centers$y)
    
    sampled <- centers.ref <- centers
    pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(sample.size)
  
  # for(q in 0:3){
  #   new.points = which(1:n.points%%(2^q) == 0)
  #   new.size = length(new.points)
    
    # if(new.size!=n.points){
      
      # set.seed(1234)
      # kc <- kmeans(field.df[1:2],new.size,100000, algorithm="MacQueen")
      # field.df$kc <- kc$cluster
      # new.centers <- as.data.frame(kc$centers)
      # new.centers <- arrange(new.centers,y,x)
      # new.centers$id <-paste0("c",1:new.size)
      
      # closest <- nn2(centers.ref[,1:2],new.centers[,1:2], 1)
      # sampled <- centers.ref[closest$nn.idx,]
      # centers.ref <- new.centers
  
    
    N <- nrow(sampled)
    sampled$eca<-merge(field.df,centers.nearest, by = c("x","y"))[,3]
    sample.df<-as.data.frame(sampled)
    
     p.sample <- 
       ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=eca.q),lwd=0,alpha=0.5) +
       geom_point(data=sample.df,aes(x,y),size=0.8,shape=21,fill="white",alpha=0.9)+
       coord_equal() + theme_bw() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=11),axis.title = element_text(size=10)) + 
       labs( y="Northing", x="Easting", title=paste0("Sample - ",N," points"), caption = "") + scale_fill_manual(values=pal.eca,name="",labels=c(1:10)) #+
       #geom_text(data=sampling.n,aes(x-1,y-1),label=sampling.n$id,size=2)

     coordinates(sampled) = ~x + y
     crs(sampled) <- CRS(utm)
     
     # Semi-Variogram
     v.eca = variogram(eca~1, sampled)
     if(sill==0){sill=NA}
     if(is.na(range) || range==0){range=NA}
     if(v.model[1]=="auto"){v.model=c("Exp", "Mat", "Sph")}
     
     # Fit function to variogram
     #fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))
     fit.eca<-fit.variogram(v.eca, vgm(model=v.model, nugget=nugget, psill=sill, range=range))
     range.eca<-round(fit.eca$range,2)
     params<-paste0("model: ",fit.eca$model,"    nugget: ",nugget,"   sill: ",round(fit.eca$psill,2),"   range: ",range.eca," meters")

     Fitted <- data.frame(dist = seq(0.01, max(v.eca$dist), length = 101))
     Fitted$gamma <- variogramLine(fit.eca, dist_vector = Fitted$dist)$gamma
     
     # Variogram cloud 
     v.eca.cloud = variogram(eca~1, sampled, cloud=T)

     
     # Co-variogram
     cv.eca = variogram(eval(parse(text=names(sampled)[2]))~1, sampled,covariogram=T)
     
     # Eq 12 - X.hat
     X.hat = signif(
       sum(abs(1 - (v.eca$gamma + cv.eca[cv.eca$dist>0,"gamma"]) / var(sampled[[2]])) * v.eca$np) / 
         sum(v.eca$np)
       ,2)
     
     # X.hat = round(sum(abs(1-(v.eca.cloud$gamma+cov(v.eca.cloud$dist,v.eca.cloud$gamma)) / var(v.eca.cloud$gamma)) * v.eca.cloud$np) / sum(v.eca.cloud$np) ,2)
     
     p.v <- 
       ggplot(v.eca, aes(x = dist, y = gamma)) + 
       geom_point(data=v.eca.cloud,aes(x=dist, y=gamma),shape=1,size=1,col="skyblue2",alpha=0.6) + 
       geom_point(shape=21,size=2,col="blue",fill="blue") + 
       geom_line(data = Fitted,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = paste0("ECa variogram - ",N," points\n")) + 
       scale_x_continuous(limits = c(0, max(v.eca$dist+(max(v.eca$dist)/20))),expand = c(0, 0)) +
       scale_y_continuous(limits = c(0, max(c(v.eca$gamma+(max(v.eca$gamma)/5), Fitted$gamma))),expand = c(0, 0))+
       annotate("text", x = (max(v.eca$dist)), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = params,hjust=1) +
        annotate("text", x = (2), y=(max(v.eca$gamma)/10), label = paste0("X = ",X.hat),hjust=0)
        #annotate("text", x = (2), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = unname(TeX(paste0("$\\bar{\\chi}$ = ",X.hat))), parse=TRUE ,hjust=0)
     
     #unname(TeX(paste0("$\\bar{\\chi}$ = ",X.hat)))
       
       
     
     
     # p.v.c <- 
     #   ggplot(v.eca.cloud, aes(x = dist, y = gamma)) + 
     #   geom_point(shape=1,size=2,col="skyblue2",alpha=0.8) + 
     #   geom_line(data = Fitted,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = "ECa (normaliezd)",caption = paste0("N=",N,"\n")) + 
     #   scale_x_continuous(limits = c(0, max(v.eca$dist+(max(v.eca$dist)/20))),expand = c(0, 0)) +
     #   scale_y_continuous(limits = c(0, max(c(v.eca$gamma+(max(v.eca$gamma)/5), Fitted$gamma))),expand = c(0, 0))+
     #   annotate("text", x = (max(v.eca$dist)), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = params,hjust=1) +
     #   annotate("text", x = (2), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = paste0("X = ",X.hat) ,hjust=0)
     
     assign(paste0("p.sample.",N),p.sample)
     assign(paste0("p.v.",N),p.v)
     # assign(paste0("p.v.c",q),p.v.c)
     
     new.xss.row<-data.frame(sample.size=N,X=X.hat)
     xss<-rbind(xss,new.xss.row)
     plots.list<-c(plots.list,N)
     
     sample.size=sample.size-10
  }
  
  } # end clusters centroids
  
  
  
  if(to.plot=="xss"){ ### Plot X vs sample-size
    theme.fvi <- theme(plot.subtitle = element_text(hjust=1,size=11),panel.background = element_rect(NA),panel.grid = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.1, fill = NA))
    max.X<-max(xss$X)
    
    p <- 
      ggplot(xss,aes(sample.size,X)) + geom_line(linewidth=0.6,col="grey30",lty=2) + geom_point(size=2,col="forestgreen") +
      #geom_vline(aes(xintercept=fuzzy.indices[which.max(pe),"k"]),col="Red2",alpha=0.5) +
        
       theme.fvi + labs(title=unname(TeX("$\\bar{\\chi}$ vs sample-size"))) + scale_x_continuous("number of samples",expand = c(0,10)) + scale_y_continuous(unname(TeX("$\\bar{\\chi}$"))) 
    # +
    #     annotate("text", x=(xss$sample.size+1), y=(xss$X+0.03*max.X), label = paste0(xss$sample.size," / ",xss$X),hjust=0,size=3.5)
    
    return(p)
    
  } else { ### plot variogram and sample map

     plot.list=list()
     for(p.N in plots.list[1:(round(length(plots.list)/2))]){ 
       pl.v<-list(eval(parse(text=paste0("p.v.",p.N))))
       plot.list<-c(plot.list,pl.v)
     }
     # for(t in 0:3){ 
     #   pl.v.c<-list(eval(parse(text=paste0("p.v.c",t))))
     #   plot.list<-c(plot.list,pl.v.c)
     # }
     for(w in plots.list[1:(round(length(plots.list)/2))]){ 
       pl.s<-list(eval(parse(text=paste0("p.sample.",w))))
       plot.list<-c(plot.list,pl.s)
     }
     
     
     for(p.N in plots.list[(round(length(plots.list)/2)+1):length(plots.list)]){ 
       pl.v<-list(eval(parse(text=paste0("p.v.",p.N))))
       plot.list<-c(plot.list,pl.v)
     }
     for(w in plots.list[(round(length(plots.list)/2)+1):length(plots.list)]){ 
       pl.s<-list(eval(parse(text=paste0("p.sample.",w))))
       plot.list<-c(plot.list,pl.s)
     }
     
     do.call(multiplot,c(plot.list,cols=4))
  }

}




###
#################### 
# Sampling plan design

variogramSamples <- function(project,layer,n.points=120,data.type="eca",to.plot="xss",iterations=10) {
  #n.points=120
  # project="ulisboa"  
  # layer="conqueiros site1 h_05m_c1_0-98_conqueiros_site1.tif"
  # project="Gadash_farm"
  # layer="30.12gadsh.h_deep_c1_0-98_sample_area.tif"
  
  if(is.null(layer) || layer==""){
    text(x = 0.5, y = 0.5, paste("Select input layer...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
  r<-raster(paste0(projectsDir,project,"/",layer))
  
  if(data.type=="uniform"){ # syntetic data
    r<-setValues(r,runif(ncell(r)))
  }
  
  if(data.type=="normal"){ # syntetic data
    r<-setValues(r,rnorm(ncell(r)))
  }
  
  field.df <- as.data.frame(rasterToPoints(r))
  d.cols<-names(field.df[-c(1:2)])
  
  field.df <-data.frame(x=field.df$x,y=field.df$y,lapply(field.df[d.cols], normal)) # normalize ECa values to (0,1)
  field.df$eca <- field.df[[3]]
  field.df$eca.q <- as.factor(ntile(field.df[[3]],as.numeric(n.points)))
  
  
  xss<-data.frame(matrix(ncol=2,nrow=0))
  names(xss)<-c("sample.size","X")
  
  # sample by clusters centroids
    for(z in 1:iterations) {
      sample.size <- as.numeric(n.points)
    while(sample.size>1){
      
      #set.seed(1234)  
      kc <- kmeans(field.df[1:2],sample.size,100000, algorithm="MacQueen")
      field.df$kc <- kc$cluster
      field.df$eca.q <- as.factor(ntile(field.df[[3]],as.numeric(sample.size)))
      
      centers <- as.data.frame(kc$centers)
      centers <- arrange(centers,y,x)
      centers$id<-paste0("o",1:sample.size)
      
      centers.nearest<-centers
      centers.nearest$x <-round(centers$x)
      centers.nearest$y <-round(centers$y)
      
      sampled <- centers.ref <- centers
      pal.eca <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(sample.size)
      
      N <- nrow(sampled)
      sampled$eca<-merge(field.df,centers.nearest, by = c("x","y"))[,3]
      sample.df<-as.data.frame(sampled)
      
      # p.sample <- 
      #   ggplot(field.df,aes(x,y)) + geom_tile(aes(fill=eca.q),lwd=0,alpha=0.5) +
      #   geom_point(data=sample.df,aes(x,y),size=0.8,shape=21,fill="white",alpha=0.9)+
      #   coord_equal() + theme_bw() + theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=11),axis.title = element_text(size=10)) + 
      #   labs( y="Northing", x="Easting", title=paste0("Sample - ",N," points"), caption = "") + scale_fill_manual(values=pal.eca,name="",labels=c(1:10)) #+
      # #geom_text(data=sampling.n,aes(x-1,y-1),label=sampling.n$id,size=2)
      
      coordinates(sampled) = ~x + y
      crs(sampled) <- CRS(utm)
      
      # Semi-Variogram
      v.eca = variogram(eca~1, sampled)
      
      # Fit function to variogram
      fit.eca<-fit.variogram(v.eca, vgm(c("Exp", "Mat", "Sph")))
      range.eca<-round(fit.eca$range[2],2)
      model<-fit.eca$model[2]
      sill<-round(fit.eca$psill[2],2)
      nugget<-round(fit.eca$psill[1],2)
      params<-paste0("model: ",model,"    nugget: ",nugget,"   sill: ",sill,"   range: ",range.eca," meters")
      
      Fitted <- data.frame(dist = seq(0.01, max(v.eca$dist), length = 101))
      Fitted$gamma <- variogramLine(fit.eca, dist_vector = Fitted$dist)$gamma
      
      # Variogram cloud 
      #v.eca.cloud = variogram(eca~1, sampled, cloud=T)
      
      
      # Co-variogram
      cv.eca = variogram(eval(parse(text=names(sampled)[2]))~1, sampled,covariogram=T)
      
      # Eq 12 - X.hat
      X.hat = signif(
        sum(abs(1 - (v.eca$gamma + cv.eca[cv.eca$dist>0,"gamma"]) / var(sampled[[2]])) * v.eca$np) / 
          sum(v.eca$np)
        ,2)
      
      # p.v <- 
      #   ggplot(v.eca, aes(x = dist, y = gamma)) + 
      #   geom_point(data=v.eca.cloud,aes(x=dist, y=gamma),shape=1,size=1,col="skyblue2",alpha=0.6) + 
      #   geom_point(shape=21,size=2,col="blue",fill="blue") + 
      #   geom_line(data = Fitted,aes(dist,gamma)) + h.theme + labs(x="Distance [m]",y="semivariance",title = paste0("ECa variogram - ",N," points\n")) + 
      #   scale_x_continuous(limits = c(0, max(v.eca$dist+(max(v.eca$dist)/20))),expand = c(0, 0)) +
      #   scale_y_continuous(limits = c(0, max(c(v.eca$gamma+(max(v.eca$gamma)/5), Fitted$gamma))),expand = c(0, 0))+
      #   annotate("text", x = (max(v.eca$dist)), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = params,hjust=1) +
      #   annotate("text", x = (2), y=(max(v.eca$gamma)/10), label = paste0("X = ",X.hat),hjust=0)
      # #annotate("text", x = (2), y=(max(v.eca$gamma)+(max(v.eca$gamma)/10)), label = unname(TeX(paste0("$\\bar{\\chi}$ = ",X.hat))), parse=TRUE ,hjust=0)
      
      # assign(paste0("p.sample.",N),p.sample)
      # assign(paste0("p.v.",N),p.v)
  
      new.xss.row<-data.frame(sample.size=N,X=X.hat)
      xss<-rbind(xss,new.xss.row)
      #plots.list<-c(plots.list,N)
      
      sample.size=sample.size-10
    }
    
  } # end clusters centroids
  
    #if(to.plot=="xss"){ ### Plot X vs sample-size
    theme.fvi <- theme(plot.subtitle = element_text(hjust=1,size=11),panel.background = element_rect(NA),panel.grid = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.1, fill = NA))
    max.X<-max(xss$X)
    
    mean.xss <- xss %>% 
      group_by(sample.size) %>%
      dplyr::summarize(X.mean = mean(X, na.rm=TRUE))
                       
    p <- 
      ggplot(xss,aes(sample.size,X)) + 
        geom_point(size=1,col="forestgreen",alpha=0.4) +
        geom_line(data=mean.xss,aes(sample.size,X.mean),linewidth=0.6,col="grey30",lty=2) + 
        geom_point(data=mean.xss,aes(sample.size,X.mean),size=2,col="royalblue3",alpha=1) +
      #geom_vline(aes(xintercept=fuzzy.indices[which.max(pe),"k"]),col="Red2",alpha=0.5) +
          
      theme.fvi + labs(title=unname(TeX("$\\bar{\\chi}$ vs sample-size"))) + scale_x_continuous("number of samples",expand = c(0,10)) + scale_y_continuous(unname(TeX("$\\bar{\\chi}$"))) 
    # +
    #     annotate("text", x=(xss$sample.size+1), y=(xss$X+0.03*max.X), label = paste0(xss$sample.size," / ",xss$X),hjust=0,size=3.5)
    
    return(p)
  }
    #}
    
#  } else { ### plot variogram and sample map
    
    # plot.list=list()
    # for(p.N in plots.list[1:(round(length(plots.list)/2))]){ 
    #   pl.v<-list(eval(parse(text=paste0("p.v.",p.N))))
    #   plot.list<-c(plot.list,pl.v)
    # }
    # # for(t in 0:3){ 
    # #   pl.v.c<-list(eval(parse(text=paste0("p.v.c",t))))
    # #   plot.list<-c(plot.list,pl.v.c)
    # # }
    # for(w in plots.list[1:(round(length(plots.list)/2))]){ 
    #   pl.s<-list(eval(parse(text=paste0("p.sample.",w))))
    #   plot.list<-c(plot.list,pl.s)
    # }
    # 
    # 
    # for(p.N in plots.list[(round(length(plots.list)/2)+1):length(plots.list)]){ 
    #   pl.v<-list(eval(parse(text=paste0("p.v.",p.N))))
    #   plot.list<-c(plot.list,pl.v)
    # }
    # for(w in plots.list[(round(length(plots.list)/2)+1):length(plots.list)]){ 
    #   pl.s<-list(eval(parse(text=paste0("p.sample.",w))))
    #   plot.list<-c(plot.list,pl.s)
    # }
    # 
    # do.call(multiplot,c(plot.list,cols=4))
  
  




# clustering source layers
vSourcePlot <- function(project,layer){

  if(is.null(layer) || layer==""){
    text(x = 0.5, y = 0.5, paste("Select project and layer...\n"), cex = 1.2, col = "purple4")
    return();
  }
  
    if (file.exists(paste0(projectsDir,project,"/",layer))) {
      r<-raster(paste0(projectsDir,project,"/",layer))
    } else {
      text(x = 0.5, y = 0.5, paste("file does not exist.\n"), cex = 1.6, col = "black")
      return();
    }
  
  field.df <- as.data.frame(rasterToPoints(r))
  layer.cols<-names(field.df[-c(1:2)])
  n=length(layer)
  palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
  
  pl <-
      ggplot(field.df,aes(x,y)) + geom_tile(aes_string(fill=layer.cols[1]),alpha = 1) + coord_equal() + 
      theme(legend.position="bottom",plot.title = element_text(hjust = 0.5,size=10),
            axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
            panel.background = element_rect(fill = NA),panel.grid.major = element_line(size=0.1,colour = "grey30"),
            panel.grid.minor = element_line(size=0.1,colour = "grey30"),panel.border = element_rect(size=0.2, fill = NA)) + 
      labs( y="", x="", title=layer.cols[1])+
      scale_fill_gradientn(colours = palc,name="")
  
  pl
}