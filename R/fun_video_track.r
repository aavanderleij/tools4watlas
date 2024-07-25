#' Create a video with tracking data
#'
#' Creates a video or video frames with tracking data
#'
#' @author Allert Bijleveld
#' @param l_tracks list of tracks to plot
#' @param IDs dataframe with tag information, default is Null
#' @param zero_ts whether to calculate relative time by subtracting start time from time of the track
#' (to sync them between differen tracks), default is FALSE.
#' @param dt timestep for aggregating locations (seconds)
#' @param bbox bonding box for underlying map, default = NULL. If bbox is NULL bbox wil be estimateted from data
#' @param frames_s frames per second, default is 15.
#' @param LINES whether or not plot a line for the track where it has been before, default is TRUE.
#' @param PAST_LINES add line for the whole past track, default is TRUE.
#' @param trail lenght of trail, multipued by dt. Default is 10.
#' @param MAP openmap object, default is NULL. If MAP is NULL the map will be loaded from the bbox.
#' @param tg_cols colors of each tag, default is NULL.
#' @param species dataframe with columns for name (species) and colour (COL) per species used in legend.
#' Default is NULL. If species is NULL, legend and colour will be done per individulal.
#' @param Save whether to save the video, default is FALSE
#' @param pad video save location, default is current working direcory.
#' @param name video name
#' @param fr fraction of plot range to move scale bar up and left. #TODO get clarivication
#' @param ppi pixel per inch, default is NULL #TODO why null default?
#' @param LEGEND position of legend
#' @param plot_LEGEND_IDs plot tag id's in legend, default is TRUE
#' @param legend_line interspacing between lines, default is 1.
#' @param legend_cex size of points in legend, default is 1.
#' @param legend_text size of text in legend, default is 1.
#' @param scale_bar size of scalebar, default is 1. #TODO @ get clarity
#' @param scale_dt Default is 1. #TODO get clarity
#' @param SCALE_DIST scale bar sistance in meters, default is 5000.
#' @param codec encoder used for FFmpeg, default is "libx264".
#' @param towers data of tower locations, plot tower locations in video. Default is NULL, don't plot towers
#' @param water tidal data, plot tilal data in video. Default is NULL, don't plot tidal data.
#' @param height height data, plot height data in video. Default is NULL. don't plot height data.
#' @param tmp_PNGs
#' @return
#'
#' @examples
#' \dontrun{
#' video_tracks(ldf_osm, dt=Dt, trail=Trail, IDs=tag_list_all, PAST_LINES=Past_Lines, MAP=MAP,frames_s=Frames_s, ppi=96,
#' Save=TRUE, pad="C:/path/to/videos/", name=Name, tg_cols=COL, species=species_col, LEGEND=LEGEND,
#' plot_LEGEND_IDs=PLOT_LEGEND_IDs, legend_line=SCALE_LEGEND_LINE, legend_cex=SCALE_LEGEND_CEX,
#' legend_text=SCALE_LEGEND_TXT, bbox=BBox, Scale=SCALE, SCALE_DIST=SCALE_DIST, scale_bar=SCALE_BAR, scale_dt=SCALE_DT,
#' codec="libx264", towers=towers, water=Water, height=height, tmp_PNGs=FALSE)
#' }
#'
#' @export
#'

## It would be useful to be able to plot tracks in sequence (let's say per day).
# For this the timestap needs to be from the minum overall to the maximum overall time.

video_tracks<-function(l_tracks, IDs=NULL,day=NULL, zero_ts=FALSE, dt, bbox=NULL, frames_s=15, LINES=TRUE,
					   PAST_LINES=TRUE, trail=10, MAP=NULL, tg_cols=NULL, species=NULL, Save=FALSE, pad=getwd(), name,
					   fr=0.015,ppi=NULL,LEGEND=LEGEND, plot_LEGEND_IDs=TRUE, legend_line=1, legend_cex=1,legend_text=1,
					   scale_bar=1, scale_dt=1, Scale=1, SCALE_DIST=5000, codec="libx264", towers=NULL, water=NULL,
					   height=NULL, tmp_PNGs=FALSE){

## load libraries


	library("raster")
	library("plyr")
	library("OpenStreetMap")
	library("RColorBrewer")
	library("animation")
  	library("stringr")


	## catch possible errors or non-logical combinations of settings
	if(Save==FALSE&tmp_PNGs==TRUE){stop("Writing PNG files without saving a movie is wasting time")}

	## setup functions

	## which track has the max Tinterval
	whichmax<-function(x,colName){
		max(x@data[,c(colName)])
		}
	## which track has the min Tinterval
	whichmin<-function(x,colName){
		min(x@data[,c(colName)])
		}
	## which track has the max Tinterval
	BBOX<-function(x, buffer=1000){
		# buffer is in meters x en dan y
		bbox<-as.data.frame(t(bbox(x)+matrix(c(-buffer,buffer,-buffer,buffer),nrow=2,byrow=TRUE)))
		names(bbox)<-c("X","Y")
		### make spatial bbox
		coordinates(bbox) <- ~ X+Y
		proj4string(bbox) <- osm()
		## transform to LL
		LL<- "+init=epsg:4326"
		bbox<-spTransform(bbox,LL)
		bbox<-bbox@bbox
		# rbind(c(bbox[2,2], bbox[1,1]), c(bbox[2,1],bbox[1,2]))
		# list(X = bbox[1,], Y=bbox[2,])
		}
		## get integers for timeinteravls
	getIntervals<-function(x, Tsteps){
			x$Tinterval<- findInterval(x$time, Tsteps) + 1
			x
			}
	## aggregate intervals into spatial dfram
	aggregate_steps <- function(d1){
		d1$x<-coordinates(d1)[,1]
		d1$y<-coordinates(d1)[,2]
		## aggregate locations per Tstep window
		D1<-ddply(as.data.frame(d1), .(Tinterval), summarise, time = median(time), X= median(x), Y= median(y), N=length(x))
		D1<-D1[order(D1$Tinterval),]
		# make aggregation spatial
		coordinates(D1) = ~X + Y
		proj4string(D1) <- osm()
		D1
		}
	## plot tracks
	plot_tracks<-function(d, COL){
		d$col <- t_col(COL, seq(80,0,length.out=nrow(d)),NULL)
		d$size<-seq(0.25,2,length.out=nrow(d))
		points(coordinates(d), col=NULL,bg=d$col, cex= d$size, pch=21)
		}
	## plot tracks
	plot_lines<-function(d, COL){
		lines(coordinates(d), col=t_col(COL, 50, NULL))
		}
	## make colors transparent
		t_col <- function(color, percent = 50, name = NULL) {
		#	  color = color name
		#	percent = % transparency
		#	   name = an optional name for the color
		## Get RGB values for named color
		  rgb.val <- col2rgb(color)
		## Make new color using input color as base and alpha set by transparency
		  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],max = 255,alpha = (100-percent)*255/100, names = name)
		## Save the color
		  # invisible(t.col)
		  t.col
		  }

### work the data

	## omzetten initial values
	frames_s<-1/(frames_s)


	## create timesteps
		Tsteps<-seq(min(l_tracks[[which.min(unlist(lapply(l_tracks, whichmin, colName="time")))]]$time) + dt, max(l_tracks[[which.max(unlist(lapply(l_tracks, whichmax, colName="time")))]]$time), dt)
			## 60*dt omdat dt in minumten is en de steps in sec
		# Tsteps<-seq(min(l_tracks[[which.min(unlist(lapply(l_tracks, whichmin, colName="time")))]]$time)+60*dt, max(l_tracks[[which.max(unlist(lapply(l_tracks, whichmax, colName="time")))]]$time), (60*dt))
		# original  Tsteps<-seq(min(l_tracks[[which.max(unlist(lapply(l_tracks, whichmax, colName="time")))]]$time)+(60*dt), max(l_tracks[[which.max(unlist(lapply(l_tracks, whichmax, colName="time")))]]$time), (60*dt))
		## get time intervals for each track
		l_tracks<-lapply(l_tracks, getIntervals,Tsteps=Tsteps)

	## aggregate the locations into the timesteps
		l_steps<-lapply(l_tracks, aggregate_steps)

	# get real time per interval (why for latest track)?
		maxID<-which.max(unlist(lapply(l_steps, whichmax, colName="Tinterval")))
		maxTint<-max(l_tracks[[maxID]]$Tinterval)
		realTime<-data.frame(Tinterval=1:maxTint)
		# for maximum time
		# realTime$time<-l_tracks[[maxID]]$time[which.min(l_tracks[[maxID]]$time)] + realTime$Tinterval * dt
		# realTime$TIME<-l_tracks[[maxID]]$TIME[which.min(l_tracks[[maxID]]$TIME)] + realTime$Tinterval * dt
		## for minimum
		realTime$time<-min(l_tracks[[which.min(unlist(lapply(l_tracks, whichmin, colName="time")))]]$time) + realTime$Tinterval * dt
		realTime$TIME<-min(l_tracks[[which.min(unlist(lapply(l_tracks, whichmin, colName="TIME")))]]$TIME) + realTime$Tinterval * dt


### initialise map

		## get bounding box
		if(is.null(bbox)){
			Bbox<-lapply(l_tracks,BBOX,buffer=500)
			xrange<-range(unlist(lapply(Bbox, `[`,1,)))
			yrange<-range(unlist(lapply(Bbox, `[`,2,)))
			bbox<-cbind(rev(yrange), xrange)
			}
		if(is.null(MAP)){
			map <- openmap(bbox[1,],bbox[2,],type='bing')
			}
		# plot(map)
		# enlarge scale om mooie high quality vodeo te krijgen
		px_width  <- map$tiles[[1]]$yres[1] * Scale
		px_height <- map$tiles[[1]]$xres[1]	* Scale

		### for video px_width and px_height need to be even numbers
		if(px_width %% 2 != 0){px_width<-px_width-1}
		if(px_height %% 2 != 0){px_height<-px_height-1}

		## get number of pixels per inch on screen
		# if(is.null(ppi)){
			# pix<-dev.size(units = "px")
			# inch<-dev.size(units = "in")
			# ppi<-pix[1]/inch[1]
			# dev.off()
			# }


		##initialise plot
		if(tmp_PNGs==FALSE){
			# set win graph with required dimensions in inches
			win.graph(width=px_width/ppi, height=px_height/ppi)
			par(bg="black")
			par(xpd=TRUE)
			}

## match tidal data with Tintervals
	if(!is.null(water)){
		## set color of water
			collint<-"blue"
			RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)

		## crop tidal data to tracks
			water<-water[water$datetime >= (min(Tsteps) - 601) & water$datetime <= (max(Tsteps)  + 601) ,]
			if(nrow(water)==0){print("no tidal data found in selected time window")}

		## error log if NAP data is not specified
		if(is.null(height)){ print("Tidal data is specified but NAP NULL")}
		}


## start recording for video
	if(Save){
		if(tmp_PNGs==FALSE){
			## initialise recording from memory with animate
			ani.record(reset=TRUE)
		}else{
			##create directory for temporary output of frames
			dir.create(file.path(pad, "tmp_animate"),  recursive = TRUE, showWarnings = TRUE)
			## delete any file that is there
			files<-list.files(file.path(pad, "tmp_animate"))
			if(length(files)>0){
				file.remove(file.path(pad, "tmp_animate", files))
				}
			}
		}


	#loop over all time steps
	##	find max step also
	## + trailInt to set length of visible trail
	for (i in 1:(maxTint+trail+1)){

		l_subset<-lapply(l_steps, function(x)x[x$Tinterval<=i&x$Tinterval>(i-trail),])

		#plot background map
		if(Save&tmp_PNGs){
			## create png
			png(file = paste(file.path(pad, "tmp_animate"), "/track", str_pad(as.character(i), 5, pad = "0"), ".png", sep=""), width=px_width, height=px_height, bg="black")
			par(xpd=TRUE)
			}

		## initialise map
		plot(map)

		## add water
		if(!is.null(water)){
			## calculate inundated area
			if(i<=maxTint){# prevents error if trails extens outside of mesured times
				waterlevel<-water$waterlevel[which.min(abs(difftime(water$datetime, realTime$time[realTime$Tinterval==i])))]
				dry_land <- height > waterlevel
				#plot(dry_land)
				dry_land[dry_land>=0.5]<-NA
				}
			## overlay water
				# image omdat met plot de plotting region verandert
			image(dry_land, col=RGBi, interpolate=FALSE, add=TRUE, legend = FALSE)
			}

		# add towers
		if(!is.null(towers)){
			points(towers$X,towers$Y,pch=23, cex=2,col=2,bg=1)
			}

		# add timestamp
		Time<- paste(realTime$time[realTime$Tinterval==i],"s")
		if(i>maxTint){Time<- paste(realTime$time[realTime$Tinterval==maxTint],"s")}
		mtext(Time, side = 1, line = 3.7, adj = 0.90, cex = scale_dt, col = "white")

		## add scalebar
		# xy_scale<-cbind(-131120, 6765510)
		# xy_scale<-click()
			xdiff<-diff(par('usr')[1:2])
			ydiff<-diff(par('usr')[3:4])
			xy_scale<-c(par('usr')[1]+xdiff*fr, par('usr')[3] + ydiff*fr)
			scalebar(SCALE_DIST, xy_scale,type='line', divs=4, lwd=3, col="white", label=paste(SCALE_DIST/1000, "km"), cex=scale_bar)

		# set colors
		if(is.null(tg_cols)){
			COL=rainbow(length(l_subset))
			}else{
			COL=tg_cols
			}

		## legend
		if(!is.null(IDs) & plot_LEGEND_IDs ){ #limit legend to 20 tags
			# single or multiple species
			if(is.null(species) | length(unique(species))==1){ ## plot legend per tag
					legend(LEGEND, c(IDs) , title="www.nioz.nl/watlas", col=c(COL), pt.bg=c(COL),pch=c(rep(21,length(COL))),text.col="white", cex=legend_text,pt.cex=legend_cex,bty = "n", x.intersp=legend_line,y.intersp=legend_line, ncol=1)
					# legend(LEGEND, c("receiver stations",IDs) , col=c("red",COL), pt.bg=c("black",COL),pch=c(23,rep(21,length(COL))),text.col="white", cex=legend_text,pt.cex=legend_cex,bty = "n", x.intersp=legend_line,y.intersp=legend_line, ncol=1) # with receivers in legend
				}else{ # plot per species
					legend("topleft", c(spec$species), title="www.nioz.nl/watlas", title.adj=, title.cex=1.4*legend_text, col=c(spec$COL), pt.bg=c(spec$COL),pch=c(rep(21,length(spec$COL))),text.col="white", cex=legend_text, pt.cex=legend_cex, bty = "n", x.intersp=legend_line,y.intersp=legend_line*0.75, ncol=1)
					# legend("topleft", c("receiver stations",spec$species) , col=c("red",spec$COL), pt.bg=c("black", spec$COL),pch=c(23,rep(21,length(spec$COL))),text.col="white", cex=legend_text, pt.cex=legend_cex, bty = "n", x.intersp=legend_line,y.intersp=legend_line, ncol=1) # with receivers in legend
				}

			}else{ # plot most simple legend with only the receiver stations
				# legend(LEGEND, c("receiver stations") , col=c("red"), pt.bg=c("black"), pch=c(23), text.col="white", cex=legend_text,pt.cex=legend_cex,bty = "n", x.intersp=legend_line,y.intersp=legend_line)
				legend(LEGEND, c("www.nioz.nl/watlas"), text.col="white", cex=legend_text, pt.cex=legend_cex, bty = "n", x.intersp=legend_line,y.intersp=legend_line)
			}


		## plot line as well
		if(LINES==TRUE){
		mapply(plot_lines, d=l_subset, COL=COL)
			}

		## add line for whole previous part of track
		if(PAST_LINES==TRUE){
			l_subset_lines<-lapply(l_steps, function(x)x[x$Tinterval<=i,])
			mapply(plot_lines, d=l_subset_lines, COL=COL)
			}

		## plot tracks
		mapply(plot_tracks, d=l_subset, COL=COL)

		if(Save){
			if(tmp_PNGs==FALSE){
				## indicate next frame
				ani.record()
			}else{
				## close PNG
				dev.off()
				}
			}

		}	# end for i

## save the	videofile

		if (Save==TRUE){
			if(tmp_PNGs==FALSE){
			## set path for exporting
				setwd(pad)
				try(dev.off(), silent =TRUE)

				cat(paste("Saving video ... ","\n", sep=""))


				saveVideo(ani.replay(),
						  video.name = paste(name, ".mp4", sep=""),
						  ffmpeg = "C:\\ffmpeg\\bin\\ffmpeg.exe",
						  interval=frames_s,
						  ani.width=px_width/Scale,
						  ani.height=px_height/Scale,
						  ani.dev="png",
						  other.opts = paste("-pix_fmt yuv420p -c:v ",
											 codec,
											 " -hide_banner -loglevel warning -stats -crf 18 ",
											 sep=""))

				### old code with diffrent arguments like equal bitrate
				# saveVideo(ani.replay(), video.name = paste(name, ".mp4", sep=""),ffmpeg = "C:\\ffmpeg\\bin\\ffmpeg.exe", interval=frames_s, ani.width=px_width, ani.height=px_height,ani.dev="png", other.opts = "-pix_fmt yuv420p -c:v libx264 -b:v 1000k")
				} else {
				cat(paste("PNG files are ready in: ","\n", file.path(pad, "tmp_animate"),"\n", sep=""))
				}
			}
} # end of function