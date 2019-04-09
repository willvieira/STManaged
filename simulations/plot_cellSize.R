###############################
# Plot cell size simulation
# Will Vieira
# April 8, 2019
##############################

##############################
# Steps:
  # getting data
  # calculate col proportion (mean and sd)
  # plot landscape proportion
  # organize range limit
  # plot range limit in function of time
##############################



# getting data (it takes about 2 minutes to load all 900 simulations)
  cellSize = c(0.3, 0.5, 0.8, 1, 2.5, 5)
  manag = c('noManag', 'plant', 'harv', 'thin', 'enrich')
  managInt = 0.15
  reps = 1:30
  steps = 150

  mainFolder = 'output/cellSizeSim/'
  for(cs in cellSize) {
    for(mg in manag) {
      folderName = paste0('cellSize_', cs, '_', mg, '_', ifelse(mg == 'noManag', 0, managInt))
      for(rp in reps) {
        fileName = paste0('cellSize', cs, '_', mg, ifelse(mg == 'noManag', 0, managInt), '_rep_', rp, '.RDS')
        assign(sub('\\.RDS$', '', fileName), readRDS(paste0(mainFolder, folderName, '/', fileName)))
      }
    }
  }
#



# calculate col proportion (mean and sd)

  # function to get summary for each col
  getProp <- function(x, nRow) {
     B <- sum(x == 1)/nRow
     T <- sum(x == 2)/nRow
     M <- sum(x == 3)/nRow
     R <- 1 - sum(B, T, M)
     return(setNames(c(B, T, M, R), c('B', 'T', 'M', 'R')))
  }

  # Confidence interval function
  ci = function(x) qt(0.975, df=length(x)-1)*sd(x)/sqrt(length(x))


  # get 30 summary data frames (6 cellSizes * 5 managements)
  for(cs in cellSize) {
    # list to store all different managements results
    listCellSize = list()

    # for each management practice, save the mean and sd of all 30 replications
    for(mg in manag) {
      # name of files for each replication
      fileNames = paste0('cellSize', cs, '_', mg, ifelse(mg == 'noManag', 0, managInt), '_rep_', reps)
      # data frames to store nCol's proportion for each forest state (to be used latter for mean and IC)
      dfB = dfT = data.frame(matrix(1:(30 * get(fileNames[1])[['nCol']]), ncol = reps[length(reps)]))

      for(rp in reps) {
        land = matrix(get(fileNames[rp])[['land_T150']], ncol = get(fileNames[rp])[['nCol']], byrow = T)
        props = apply(land, 2, getProp, nRow = get(fileNames[rp])[['nRow']])
        dfB[, rp] = props["B", ]
        dfT[, rp] = props["T", ]
      }

      # calculate mean and sd
      df = data.frame(meanB = apply(dfB, 1, mean))
      df$ciB = apply(dfB, 1, ci)
      df$meanT = apply(dfT, 1, mean)
      df$ciT = apply(dfT, 1, ci)

      # add a x axis value to standardize for all cell size
      df$x = (1:get(fileNames[rp])[['nCol']])/get(fileNames[rp])[['nCol']]

      # remove border

      df = df[c(-1, -nrow(df)), ]

      # append in the list
      listCellSize[[mg]] <- df
    }

    # save specific cellSize list
    assign(paste0('listCellSize', cs), listCellSize)
  }
#



# plot landscape proportion
  # define cellSize colors
  cols = rainbow(length(cellSize))
  colsT = rainbow(length(cellSize), alpha = 0.2)
  titleLine = 1 + 15 * 0:4

  pdf('simulations/landProp_cellSize.pdf', height = 12)
  par(mfrow = c(5, 2), mar = c(2.1, 2.5, 1.1, 0.5), mgp = c(1.2, 0.2, 0), tck = -.01, cex = 0.8)
  for(mg in 1:length(manag)) {
    # boreal
    plot(0, pch = '', xlim = c(0, 1), ylim = c(0, 1), xlab = '', ylab = 'Boreal occupancy')
    for(cs in 1:length(cellSize)) {
      df = get(paste0('listCellSize', cellSize[cs]))[[manag[mg]]]
      polygon(c((1:nrow(df))/nrow(df), rev((1:nrow(df))/nrow(df))),
            c(smooth.spline(df$meanB + df$ciB, spar = 0.4)$y, rev(smooth.spline(df$meanB - df$ciB, spar = 0.4)$y)), col = colsT[cs], border = FALSE)
      lines(smooth.spline(x = df$x, y = df$meanB, spar = 0.4), col = cols[cs])
    }
    if(mg == 1) legend('topright', legend = cellSize, lty = 1, col = cols, bty = 'n')
    if(mg == 5) mtext('Latitudinal gradient', 1, line = 1.1, cex = 0.85)
    # temperate
    plot(0, pch = '', xlim = c(0, 1), ylim = c(0, 1), xlab = '', ylab = 'Temperate occupancy')
    for(cs in 1:length(cellSize)) {
      df = get(paste0('listCellSize', cellSize[cs]))[[manag[mg]]]
      polygon(c((1:nrow(df))/nrow(df), rev((1:nrow(df))/nrow(df))),
            c(smooth.spline(df$meanT + df$ciT, spar = 0.4)$y, rev(smooth.spline(df$meanT - df$ciT, spar = 0.4)$y)), col = colsT[cs], border = FALSE)
      lines(smooth.spline(x = df$x, y = df$meanT, spar = 0.4), col = cols[cs])
    }
    if(mg == 5) mtext('Latitudinal gradient', 1, line = 1.1, cex = 0.85)
    mtext(manag[mg], side = 3, line = - titleLine[mg], outer = T, cex = 0.9)
  }
  dev.off()
#



# organize range limit
  # get 30 summary data frames (6 cellSizes * 5 managements)
  for(cs in cellSize) {
    # list to store all different managements results
    listCellSize = list()

    # for each management practice, save the mean and sd of all 30 replications
    for(mg in manag) {
      # name of files for each replication
      fileNames = paste0('cellSize', cs, '_', mg, ifelse(mg == 'noManag', 0, managInt), '_rep_', reps)
      # data frames to store nCol's proportion for each forest state (to be used latter for mean and IC)
      dfB = dfT = data.frame(matrix(rep(NA, 30 * (steps + 1)), ncol = reps[length(reps)]))

      for(rp in reps) {
        rg = get(fileNames[rp])[['rangeLimit']]
        dfB[, rp] = rg[, "limitB"]
        dfT[, rp] = rg[, "limitT"]
      }

      # calculate mean and sd
      df = data.frame(meanB = apply(dfB, 1, mean))
      df$ciB = apply(dfB, 1, ci)
      df$meanT = apply(dfT, 1, mean)
      df$ciT = apply(dfT, 1, ci)

      # append in the list
      listCellSize[[mg]] <- df
    }

    # save specific cellSize list
    assign(paste0('listCellSize', cs), listCellSize)
  }
#



# plot range limit in function of time
  pdf('simulations/rangeLimit_cellSize.pdf', height = 9)
  par(mfrow = c(3, 2), mar = c(2.5, 2.5, 1, 0.5), mgp = c(1, 0.2, 0), tck = -.01, cex = 0.8)

  for(mg in 1:length(manag)) {
    plot(0, pch = '', xlim = c(0, 151), ylim = c(1, 0), xlab = '', ylab = '', yaxt = 'n')
    for(cs in 1:length(cellSize)) {
      df = get(paste0('listCellSize', cellSize[cs]))[[manag[mg]]]
      # Boreal
      polygon(c((1:nrow(df)), rev((1:nrow(df)))),
            c(df$meanB + df$ciB, rev(df$meanB - df$ciB)), col = colsT[cs], border = FALSE)
      lines(df$meanB, col = cols[cs])
      # Temperate
      polygon(c((1:nrow(df)), rev((1:nrow(df)))),
            c(df$meanT + df$ciT, rev(df$meanT - df$ciT)), col = colsT[cs], border = FALSE)
      lines(df$meanT, col = cols[cs])
    }
    # Boreal
    abline(h = max(df[, 'meanB']), lty = 3, col = 'grey', lwd = 2)
    mtext('Boreal', 2, at = max(df[, 'meanB']), cex = 0.7)
    # Temperate
    abline(h = max(df[, 'meanT']), lty = 3, col = 'grey', lwd = 2)
    mtext('Temperate', 2, at = max(df[, 'meanT']), cex = 0.7)
    # Coordinates
    if(mg == 1) {mtext('South', 1, line = -1, cex = 0.75); mtext('North', 3, line = -1, cex = 0.75)}
    # legend
    if(mg == 1) legend('topleft', legend = cellSize, lty = 1, col = cols, bty = 'n', cex = 0.8)
    # Management practice
    mtext(manag[mg], 3, cex = 0.9)
    # labs
    if(mg == 4 | mg == 5) mtext('Time (years * 5)', 1, line = 1.2, cex = 0.9)
    if(mg == 3) mtext('Latitudinal gradient', 2, line = 1.2, cex = 0.9)
  }
  dev.off()
#
