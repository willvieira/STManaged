##########################################
# Calculate real distance from climate data
# Use this information to create a virtual landscape but with real dimensions
# Will Vieira
# September, 3 2019
##########################################

# Steps
  # load real climate data
  # filter to the Québec region
  # calculate mean temperature for each latitude of the landscape
  # create a vector of distance (in Km) for the latitudinal gradient
  # save this information as a dataframe in the sysdata file


library(raster)


# load clim data

  clim <- raster::raster('simulations/data/will_clim2010_averaged5years.grd')

#



# filter to the Québec region

  clim = raster::crop(clim, extent(1e6, 2.8e6, 0, 2.2e6))

#



# Calculate mean temperature

  stepsize = (clim@extent@ymax - clim@extent@ymin) / clim@nrows
  yvals = seq(clim@extent@ymax - stepsize / 2, clim@extent@ymin, -stepsize)

  meanTemp = rowMeans(as.matrix(clim), na.rm = TRUE)

#



# Test raster distance with google maps distance

  plot(clim)
  points(2.4e6, 1e5, pch = 19)
  points(1.87e6, 2.17e6, pch = 19)
  lines(c(2.4e6, 1.87e6), c(1.1e5, 2.17e6))
  # get distance between points: google distance is about 2190 meters
  raster::pointDistance(c(2.4e6, 1e5), c(1.87e6, 2.17e6), lonlat = FALSE)/1000

#



# plot annual mean temperature map with the mean for each row

  par(mfrow = c(1, 2), mar = c(3,3,1,0.5), mgp = c(1.5, 0.3, 0), tck = -.008)
  plot(clim)
  plot(meanTemp, yvals, xlab = 'Annual mean temperature', ylab = 'Latitude')

#



# create data frame with latitude, mean temperature for each latitude, and cumulated distance

  df <- data.frame(lat = yvals, tp = meanTemp)
  df$cumDist <- rep(0, nrow(df))

  # calculate distance
  long = 2.3e6
  for(i in 2:nrow(df))
  {
    df[i, 3] <- raster::pointDistance(c(long, df[1, 1]), c(long, df[i, 1]), lonlat = FALSE)/1000
  }

#



# save file

  realDistance <- df
  #  load('R/sysdata.rda')
  #  save(realDistance, envProb, land.r, land.rBio, pars, neighbor, params, vars.means, vars.sd, file = 'R/sysdata.rda')

#
