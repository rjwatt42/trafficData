#' @export
showHTML<-function(data,new=FALSE) {
  dir <- tempfile()
  dir.create(dir)
  if (is.character(data)) {
    if (new) {
      for (i in 1:100) {
        htmlFile <- file.path(dir, paste0("index",i,".html"))
        if (!file.exists(htmlFile)) break;
      }
    } else
      htmlFile <- file.path(dir, "index.html")
    writeLines(data, con = htmlFile)
    rstudioapi::viewer(htmlFile)
  } else {
    assign("graphHTML",TRUE,braw.env)
    show<-data()
    
    htmlFile <- file.path(dir, "index.html")
    writeLines(show, con = htmlFile)
    rstudioapi::viewer(htmlFile)
    
    assign("graphHTML",FALSE,braw.env)
  }
  return(invisible(NULL))
}


mapping3D<-function(azimuth,elevation,distance,xlim,ylim,zlim) {
  azimuth <- azimuth*pi/180
  elevation <- elevation*pi/180
  caz <- cos(azimuth)
  saz <- sin(azimuth)
  cel <- cos(elevation)
  sel <- sin(elevation)
  rot.mat<-matrix(c(caz, 0, saz, 0,
                    saz*sel,cel,-caz*sel, 0,
                    -cel*saz, sel,caz*cel, 0,
                    0,0,0,1
  ),4,4,byrow=TRUE)
  
  corners<-matrix(c(1,1,1,1,
                    -1,1,1,1,
                    -1,-1,1,1,
                    1,-1,1,1,
                    1,1,-1,1,
                    -1,1,-1,1,
                    -1,-1,-1,1,
                    1,-1,-1,1
  ),ncol=8)
  scale<-rot.mat%*%corners
  scale[1,]<-scale[1,]/scale[4,]
  scale[2,]<-scale[2,]/scale[4,]
  scale[3,]<-scale[3,]/scale[4,]
  
  if (distance != 0)  
  {
    scale[1,]<-scale[1,]/(distance-scale[3,])
    scale[2,]<-scale[2,]/(distance-scale[3,])
  }
  scalex<-max(abs(scale[1,]))*1.25
  scaley<-max(abs(scale[2,]))*1.25
  
  return(list(rot.mat=rot.mat,
              minx=xlim[1],
              rangex=diff(xlim),
              miny=ylim[1],
              rangey=diff(ylim),
              minz=zlim[1],
              rangez=diff(zlim),
              distance=distance,
              scalex=scalex,
              scaley=scaley
  ))
}

rotate3D <- function(data,mapping)
{
  
  data$x<-((data$x-mapping$minx)/mapping$rangex-0.5)*2
  data$y<-((data$y-mapping$miny)/mapping$rangey-0.5)*2
  data$z<-((data$z-mapping$minz)/mapping$rangez-0.5)*2
  
  tdata <- mapping$rot.mat %*% rbind(data$y,data$z,data$x, 1)
  tdata[1,] <- tdata[1,]/tdata[4,]
  tdata[2,] <- tdata[2,]/tdata[4,]
  tdata[3,] <- tdata[3,]/tdata[4,]
  
  if (mapping$distance != 0)  
  {
    tdata[1,] <- tdata[1,] / (mapping$distance - tdata[3,])
    tdata[2,] <- tdata[2,] / (mapping$distance - tdata[3,])
  }
  tdata[1,]<-tdata[1,]/mapping$scalex
  tdata[2,]<-tdata[2,]/mapping$scaley
  return(data.frame(x=tdata[1,],y=tdata[2,]))
}

start3dPlot<-function(xlim,ylim,zlim,
                      xtick,ytick,ztick,
                      xlabel,ylabel,zlabel,
                      azimuth=45,elevation=40,distance=10) {
  
  BoxColFloor<-"grey"
  boxed<-FALSE
  boxedDash<-FALSE
  
  braw.env$plotArea<-c(0,0,1,1)
  mapping<-mapping3D(azimuth,elevation,distance,xlim,ylim,zlim)
  g<-startPlot(xlim=c(-1,1),ylim=c(-1,1),
               box="none",backC=braw.env$plotColours$graphC)
  pts<-rotate3D(data.frame(x=xlim[c(1,2,2,1)],y=ylim[c(1,1,2,2)],z=c(0,0,0,0)+zlim[1]),mapping)
  g<-addG(g,dataPolygon(pts,colour=BoxColFloor,fill=BoxColFloor))
  # outside box            
  if (boxed)
    g<-addG(g,
            dataPolygon(rotate3D(data.frame(x=xlim[c(1,1,1,1)],
                                            y=ylim[c(1,1,2,2)],
                                            z=zlim[c(1,2,2,1)]
            ),
            mapping),
            colour=BoxColSamples,
            fill=BoxColSamples
            ),
            dataPolygon(rotate3D(data.frame(x=xlim[c(1,1,2,2)],
                                            y=ylim[c(2,2,2,2)],
                                            z=zlim[c(1,2,2,1)]
            ),
            mapping),
            colour=BoxColPopulations,
            fill=BoxColPopulations
            )
    )
  
  if (boxedDash)
    g<-addG(g,
            dataPath(rotate3D(data.frame(x=xlim[c(1,1,2)],
                                         y=ylim[c(1,2,2)],
                                         z=zlim[c(2,2,2)]
            ),
            mapping),
            colour=BoxCol
            ),
            dataPath(rotate3D(data.frame(x=xlim[c(1,1)],
                                         y=ylim[c(1,1)],
                                         z=zlim[c(1,2)]
            ),
            mapping),
            colour=BoxCol
            ),
            dataPath(rotate3D(data.frame(x=xlim[c(1,1)],
                                         y=ylim[c(2,2)],
                                         z=zlim[c(1,2)]
            ),
            mapping),
            colour=BoxCol
            ),
            dataPath(rotate3D(data.frame(x=xlim[c(2,2)],
                                         y=ylim[c(2,2)],
                                         z=zlim[c(1,2)]
            ),
            mapping),
            colour=BoxCol
            )
    )
  
  
  tick_grow<-3
  tick_more<-2
  xtick_length<-0.03*diff(xlim)
  ytick_length<-0.03*diff(ylim)
  
  if (!is.null(zlabel)) {
    # z-axis 
    zx<-xlim[1]
    zy<-ylim[1]
    zyt<- -1
    zxt<-  0
    g<-addG(g,dataPath(rotate3D(data.frame(x=c(zx,zx),
                                             y=c(zy,zy),
                                             z=zlim),mapping),colour="#000000")
      )
      # long ticks
      long_ticks<-ztick
      tick.z.start <- rotate3D(data.frame(x=zx,y=zy,z=long_ticks), mapping)
      tick.z.end <- rotate3D(data.frame(x=zx+zxt*xtick_length,y=zy+zyt*ytick_length,z=long_ticks), mapping)
      for (i in 1:length(tick.z.start$x))
        g<-addG(g,dataPath(data.frame(x=c(tick.z.start$x[i],tick.z.end$x[i]),y=c(tick.z.start$y[i],tick.z.end$y[i]))))
      g<-addG(g,dataText(data.frame(x=tick.z.end$x,y=tick.z.end$y),long_ticks,hjust=1,vjust=0.5,size=0.7))
      # label
      pos.z<-rotate3D(data.frame(x=zx+zxt*diff(xlim)*0.16,y=zy+zyt*diff(ylim)*0.16,z=mean(zlim)),mapping)
      rotate.z=rotate3D(data.frame(x=c(zx,zx),
                                   y=c(zy,zy),
                                   z=zlim),mapping)
      q1<-atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296
      q2<-atan(diff(rotate.z$y)/diff(rotate.z$x))*57.296
      rotate.z<- 180-zyt*q1+zxt*q2
      g<-addG(g,dataText(data.frame(x=pos.z$x,y=pos.z$y),label=zlabel,angle=rotate.z,hjust=0.5,size=0.7,fontface="bold"))
    }

  # x ticks
  g<-addG(g,dataPath(rotate3D(data.frame(x=xlim,
                                         y=c(ylim[1],ylim[1]),
                                         z=c(zlim[1],zlim[1])),mapping),colour="#000000")
  )
  long_ticks<-xtick

  # long ticks
  tick.x.start <- rotate3D(data.frame(x=long_ticks, y=ylim[1], z=zlim[1]), mapping)
  tick.x.end <- rotate3D(data.frame(x=long_ticks, y=ylim[1]-ytick_length*tick_more, z=zlim[1]), mapping)
  for (i in 1:length(tick.x.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.x.start$x[i],tick.x.end$x[i]),y=c(tick.x.start$y[i],tick.x.end$y[i]))))
  # tick labels
  ticks.x<-rotate3D(data.frame(x=long_ticks,y=ylim[1]-ytick_length*tick_grow,z=zlim[1]),mapping)
  g<-addG(g,dataText(data.frame(x=ticks.x$x,y=ticks.x$y),long_ticks,hjust=1,vjust=0.5,size=0.7))
  
  # y ticks
  g<-addG(g,dataPath(rotate3D(data.frame(x=c(xlim[2],xlim[2]),
                                         y=ylim,
                                         z=c(zlim[1],zlim[1])),mapping),colour="#000000")
  )
  long_ticks<-ytick
  # long ticks
  tick.y.start <- rotate3D(data.frame(x=xlim[2], y=long_ticks, z=zlim[1]), mapping)
  tick.y.end <- rotate3D(data.frame(x=xlim[2]+xtick_length*tick_more, y=long_ticks, z=zlim[1]), mapping)
  for (i in 1:length(tick.y.start$x))
    g<-addG(g,dataPath(data.frame(x=c(tick.y.start$x[i],tick.y.end$x[i]),y=c(tick.y.start$y[i],tick.y.end$y[i]))))
  # tick labels
  ticks.y<-rotate3D(data.frame(x=xlim[2]+xtick_length*tick_grow,y=long_ticks,z=zlim[1]),mapping)
  g<-addG(g,dataText(data.frame(x=ticks.y$x,y=ticks.y$y),long_ticks,hjust=0.5,vjust=0.5,size=0.7))
  
  pos.x<-rotate3D(data.frame(x=sum(xlim)/2,y=ylim[1]-ytick_length*tick_grow*2,z=zlim[1]-0.1),mapping)
  g<-addG(g,dataText(pos.x,xlabel,hjust=0.5,vjust=0.5,fontface="bold"))
  
  pos.y<-rotate3D(data.frame(x=xlim[2]+xtick_length*tick_grow*2,y=sum(ylim)/2,z=zlim[1]-0.1),mapping)
  g<-addG(g,dataText(pos.y,ylabel,hjust=0.5,vjust=0.5,fontface="bold"))
  
  return(g)
}

