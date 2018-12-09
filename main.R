## get functions
library(parallel)
file.sources <- dir('R/')
invisible(sapply(paste0('R/', file.sources), source))

## get initial lanscape
initLand <- create_landscape(climRange = c(-2.3, 0.55), cellSize = 0.8)
plot_landscape(initLand, Title = 'initial_landscape')

# load pars
params = read.table("data/pars.txt", row.names = 1)


# run the model
  # (Land = 50 x 60; 1000 years = 7MB; ~ 42 seconds)
  # (Land = 80 x 240; 750 years = 25MB; ~ 3.2 minutes)
lands <- run_model(steps = 150, initLand,
                   plantInt = 0, # for all management practice: [0-1]
                   harvInt = 0,
                   thinInt = 0,
                   enrichInt = 0,
                   RCP = 4.5, # either 0, 2.6, 4.5, 6.0 and 8.5
                   stoch = T,
                   saveOutput = F
)

# load option
load('initLand.Rdata')
load('lands.Rdata')

# outputs
  ## plot some landscapes
  par(mfrow = c(3, 3))
  invisible(sapply(1:9, function(x) plot_landscape(lands[[x]], Title = names(lands)[x], rmBorder = T)))

  ## create a gif (http://imagemagick.org must be installed)
    # (5 minutes with 9MB file size for a gif with 150 steps)
  make_gif(lands, steps = 4, years = NULL, fps = 5, gifName = 'RCP6')
  make_gif(lands, steps = NULL, years = 1:100, fps = 5, gifName = 'RCP6', rangeLimit = TRUE, occup = 0.7)

  ## State occupancy for each year
  par(mfrow = c(1, 2))
  plot_occupancy(lands, years = c(1, 2))

  ## State occupancy over time
  plot_occ_time(lands)

  ## Migration
  par(mar = c(3,3,1.5,0.8), mgp = c(1.5, 0.3, 0), tck = -.008, xaxs='i', yaxs='i')
  plot(c(0, 1200), c(0, 1200), type = 'l', xlab = 'limit of B', ylab = 'limit of T')
  for(i in seq(0.3, 0.9, 0.1)) {
    mig <- lapply(lands[grep('land', names(lands))], function(x) range_limit(x, occup = i))
    mig <- setNames(data.frame(matrix(unlist(mig), nc = 2, byrow = T)), c('limitB', 'limitT'))
    points(mig, xlim = c(0, 200), ylim = c(0, 1200), col = i*10, pch = 20)
  }
  legend('bottomright', legend = seq(0.3, 0.9, 0.1), pch = 20, col = seq(0.3, 0.9, 0.1)*10, bty = 'n')

  ## Migration 2
  mig <- lapply(lands[grep('land', names(lands))], function(x) range_limit(x, occup = 0.7))
  mig <- setNames(data.frame(matrix(unlist(mig), nc = 2, byrow = T)), c('limitB', 'limitT'))
  par(mar = c(3,3,1.5,0.8), mgp = c(1.5, 0.3, 0), tck = -.008, xaxs='i', yaxs='i')
  plot(1:dim(mig)[1], mig[, 1], type = 'l', ylim = c(1200, 0), xlab = 'Time (year * 5)', ylab = 'Range limit (latitudinal gradient)', col = 'darkcyan')
  lines(mig[, 2], col = 'orange')
  abline(v = 20, lty = 2); #mtext('Climate change stops', side = 3, at = 20)
  legend('topright', legend = c('Boreal', 'Temperate'), lty = 1, col = c('darkcyan', 'orange'), bty = 'n')

  ## plot parameters increase
  climDiff <- clim_diff(env1 = initLand[['env1']], RCP = 8.5, params)
  climIncLin <- clim_increase(steps = 100, climDiff, growth = 'linear')
  climInc <- clim_increase(steps = 100, climDiff, growth = 'exponential')

  par(mfrow = c(3, 3))
  for(i in 1:9) {
    plot(1:121, unlist(lapply(climInc, function(x) x[i, 100])), type = 'l', xlab = 'steps', ylab = 'parameter')
    points(1:121, unlist(lapply(climIncLin, function(x) x[i, 100])), type = 'l', xlab = 'steps', ylab = 'parameter', col = 2)
  }
