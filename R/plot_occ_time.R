# Plot to summary the landscape occupancy over time
 ## Input:
  # - lands
 ## Output:
  # - Scatterplot

plot_occ_time <- function(lands) {

  cells = seq(lands[['nCol']] + 1, 2 * lands[['nCol']])

  # data frame
  df <- setNames(data.frame(matrix(1:4, nrow = 1)), c('B', 'T', 'M', 'R'))

  # lands
  for(i in 1:lands[['steps']]) {
    ld <- lands[[i]][cells]
    df[i, ] <- as.vector(table(ld)/length(ld))
  }

  # plot
  col <- c("darkcyan","orange","palegreen3","black")
  manag <- lands[['manag']][[1]]

  main <- paste0('Plant = ', manag[1], '; Harv = ', manag[2],
                 '; Thin = ', manag[3], '; Enrich = ', manag[4],
                 '\nsteps =', lands[['steps']], '; RCP = ', lands[['RCP']])

  par(mar = c(3,2.5,3,0.5), mgp = c(1.5, 0.3, 0), tck = -.008)
  plot(1:dim(df)[1], df[, 1], type = 'l', lwd = 1.3, xlab = "Time (*5 = years)", ylab = "State occupancy", ylim = c(0, 1), col = col[1], main = main)
  invisible(sapply(2:4, function(x) points(1:dim(df)[1], df[, x], type = 'l', lwd = 1.3, col = col[x])))
  legend('topright', legend = c('Boreal', 'Temperate', 'Mixed', 'Regeneration'), lty = 1, col = col, bty = 'n', cex = 0.9)
}
