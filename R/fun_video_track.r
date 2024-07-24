
##### create movie
## created by allert bijleveld 03 July 2017

### test
# l_tracks<-ldf_osm
# IDs=NULL
# day=NULL
# zero_ts=TRUE
# dt=30
# bbox=BBox
# frames_s=10
# LINES=TRUE
# PAST_LINES=TRUE
# trail=5
# tg_cols=NULL
# species=NULL
# Save=FALSE
# pad=getwd()
# name=paste(Sys.Date(), "pegerine", sep="-")
# fr=0.015
# ppi=96
# LEGEND=LEGEND
# plot_LEGEND_IDs=TRUE
# legend_line=1
# legend_cex=2
# legend_text=1
# scale_bar=SCALE_BAR
# scale_dt=SCALE_DT
# Scale =2
# codec="libx264"
# towers=NULL
# water=NULL
# hoogte=NULL
# tmp_PNGs=FALSE
# SCALE_DIST=5000

## It would be useful to be able to plot tracks in sequence (let's say per day).
# For this the timestap needs to be from the minum overall to the maximum overall time.

video_tracks<-function(l_tracks, IDs=NULL,day=NULL, zero_ts=FALSE, dt, bbox=NULL, frames_s=15, LINES=TRUE, PAST_LINES=TRUE, trail=10, MAP=NULL, tg_cols=NULL, species=NULL, Save=FALSE, pad=getwd(), name, fr=0.015,ppi=NULL,LEGEND=LEGEND, plot_LEGEND_IDs=TRUE, legend_line=1, legend_cex=1,legend_text=1, scale_bar=1, scale_dt=1, Scale=1, SCALE_DIST=5000, codec="libx264", towers=NULL, water=NULL, hoogte=NULL, tmp_PNGs=FALSE){

	# ltracks: list of tracks to plot
	# IDs: names for legend
	# zero_ts: whether to calculate relative time by subtracting start time from time of the track (to sync them between differen tracks )
	# dt: timestep for aggregating locations (s)
	# bbox (LL): bounding box for underlying map
	# pxwidth and height: dimensions of map
	# frames_s: aantal frames per seconde
	# LINES: whether or not plot a loine for the track where it has been before
	#PAST_LINES: add line for the whole past track
	# trail: trail van de track in aantal kaer Dt
	# tg_cols: colors of each tag
	# species: dataframe with columns for name (species) and colour (COL) per species used in legend
	# Save: whether to save the video
	# pad: waar je de video wilt saven
	# name: video name
	# fr: fractie van plot range dat de scale bar naar links en naar boven gaat
	# LEGEND	# position of legend
	# legend_line: interspacing between lines
	# legend_cex: size of the points  in legend
	# legend_text: size of the text in legend
	# Scale: to increase fonts and ces from final video output
	# codec="libx264" # codec to use for video utput
	# towers	# tower data ((NULL = not plot))
	# water		# water data to plot (NULL = not plot)
	# hoogte	# hoogte data (NULL = not plot)
	# tmp_PNGs	# False means plots are stored in memory; if true a PNG file is stored per frame in directory pad/tmp_animate
	# SCALE_DIST size of scalebar in m

## load libraries


	library("raster")
	library("plyr")
	library("OpenStreetMap")
	library("RColorBrewer")
	library("animation")
  	library("stringr")
	# library("rgdal")
	# library("data.table")


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
		## aggregate  locations per Tstep blokje
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

		### voor film export moeten het even getallen zijn
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

## match waterstanden met Tintervals
	if(!is.null(water)){
		## set color of water
			collint<-"blue"
			RGBi<-rgb(col2rgb(collint)[1,1],col2rgb(collint)[2,1],col2rgb(collint)[3,1], alpha=90, max=255)

		## crop waterdata data naar tracks
			water<-water[water$datetime >= (min(Tsteps) - 601) & water$datetime <= (max(Tsteps)  + 601) ,]
			if(nrow(water)==0){print("geen waterhoogtes in geselecteerde time window")}

		## error log if NAP data is not specified
		if(is.null(hoogte)){ print("wel waterhoogte gespecificeerd, maar geen NAP")}
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
	## + trailInt zodat de trail kan verdwijnen
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
			if(i<=maxTint){# anders error met de trail buiten de gemeten tijden
				waterlevel<-water$waterlevel[which.min(abs(difftime(water$datetime, realTime$time[realTime$Tinterval==i])))]
				droog <- hoogte > waterlevel
				#plot(droog)
				droog[droog>=0.5]<-NA
				}
			## overlay water
				# image omdat met plot de plotting region verandert
			image(droog, col=RGBi, interpolate=FALSE, add=TRUE, legend = FALSE)
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

				### oude code met andere commando's voor bijv. equal bitrate
				# saveVideo(ani.replay(), video.name = paste(name, ".mp4", sep=""),ffmpeg = "C:\\ffmpeg\\bin\\ffmpeg.exe", interval=frames_s, ani.width=px_width, ani.height=px_height,ani.dev="png", other.opts = "-pix_fmt yuv420p -c:v libx264 -b:v 1000k")
				} else {
				cat(paste("PNG files are ready in: ","\n", file.path(pad, "tmp_animate"),"\n", sep=""))
				}
			}
} # end of function