## get functions
file.sources <- dir('R/')
invisible(sapply(paste0('R/', file.sources), source))


## get initial lanscape
initLand <- create_landscape(climRange = c(-1.9, 0.6), cellSize = 5)
plot_landscape(initLand, Title = 'initial_landscape')

# load pars
params = read.table("data/pars.txt", row.names = 1)

# run the model
  # (Land = 50 x 60; 1000 years = 7MB; ~ 42 seconds)
  # (Land = 80 x 240; 750 years = 25MB; ~ 3.2 minutes)
lands <- run_model(steps = 150, initLand, params,
                   plantInt = 0, # for all management practice: [0-1]
                   harvInt = 0,
                   thinInt = 0,
                   enrichInt = 0,
                   RCP = 6, # either 0, 2.6, 4.5, 6.0 and 8.5
                   stoch = T)

# load option
load('initLand.Rdata')
load('lands.Rdata')

# outputs
  ## plot some landscapes
  par(mfrow = c(3, 3))
  invisible(sapply(1:9, function(x) plot_landscape(lands[[x]], Title = names(lands)[x], rmBorder = T)))

  ## create a gif (http://imagemagick.org must be installed)
    # (5 minutes with 9MB file size for a gif with 150 steps)
  make_gif(lands, steps = 4, years = NULL, fps = 5, gifName = 'RCP4.5')
  make_gif(lands, steps = NULL, years = 1:100, fps = 5, gifName = 'test', rangeLimit = TRUE, occup = 0.7)

  ## State occupancy for each year
  par(mfrow = c(1, 2))
  plot_occupancy(lands, years = c(1, 2))

  ## State occupancy over time
  plot_occ_time(lands)

  ## Migration
  par(mar = c(3,3,1.5,0.8), mgp = c(1.5, 0.3, 0), tck = -.008, xaxs='i', yaxs='i')
  plot(c(0, 200), c(0, 200), type = 'l', xlab = 'limit of B', ylab = 'limit of T')
  for(i in seq(0.3, 0.9, 0.1)) {
    mig <- lapply(lands[grep('land', names(lands))], function(x) range_limit(x, occup = i))
    mig <- setNames(data.frame(matrix(unlist(mig), nc = 2, byrow = T)), c('limitB', 'limitT'))
    points(mig, xlim = c(0, 200), ylim = c(0, 200), col = i*10, pch = 20)
  }
  legend('bottomright', legend = seq(0.3, 0.9, 0.1), pch = 20, col = seq(0.3, 0.9, 0.1)*10, bty = 'n')
