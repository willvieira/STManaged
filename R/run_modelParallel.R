# Function to run the model over time
 ## Input:
  # - steps (nb of steps; step * 5 = years)
  # - initLand (output of the `create_landscape` function)
  # - params (parameters)
  # - Forest management intensity [0-1]
  # - RCP (three options of climate change: RCP = 2.6, RCP = 4.5, RCP = 6, RCP = 8.5)
 ## Output:
  # - land (a list for every step)
  # - Each step (or list) of land have a list of state occupancy and the respective env1 vector
 ## Extra functions:
  # - neighbor_prop (calculates the proportion between states for each neigbor area - 9 cells)

# function to return neighborhood proportion considering the 8 neighbors
neighbor_prop <- function(neighbor) {
  B = sum(neighbor == 1)/9
  T = sum(neighbor == 2)/9
  M = sum(neighbor == 3)/9

  return(setNames(c(B, T, M), c("B", "T", "M")))
}

# function to be used in the lapply (TODO: figure out how to use ... here)
cellRun <- function(cell, neighbor, land0, pars, parCell, i, plantInt, harvInt, thinInt, enrichInt, stoch, nCol) {

  y0 <- neighbor_prop(land0[neighbor[[cell]]])
  y1 <- model_fm(t = 1, y = y0, params = pars[[i]][, parCell[cell]], plantInt, harvInt, thinInt, enrichInt)
  y1 <- y0 + unlist(y1) # update cell
  y1['R'] <- 1 - sum(y1)
  if(stoch == T) {
    cell <- which(rmultinom(n = 1, size = 1, prob = y1) == 1) # get a state depending on the probability `p`
  }else {
    cell <- which(y1 == max(y1)) # update cell
  }

  return(cell)
}

# Main function to run the model over time
run_model_parallel <- function(steps, initLand,
                              plantInt = 0,
                              harvInt = 0,
                              thinInt = 0,
                              enrichInt = 0,
                              RCP = 0,
                              stoch = T,
                              cores = 2,
                              outputLand = NA, # if NA, everything is out, otherwise specify a vector of timeSteps values [1 - steps]
                              saveOutput = F,
                              fileOutput = NULL, # name of the file
                              foulderOutput = NULL) # name of the output file, if NULL will just save in the mail `output` folder
{

  # climate change
  climDiff <- clim_diff(initLand[['env1']], RCP = RCP, params)
  pars <- clim_increase(steps = steps, climDiff, growth = 'linear')

  # lands
  lands <- list(land_T0 = initLand[['land']])
  land0 <- initLand[['land']]

  # lands information
  nRow = initLand[['nRow']]
  nCol = initLand[['nCol']]
  position = initLand[['position']]
  neighbor = initLand[['neighbor']]
  parCell = (seq(1, (nRow * nCol - 2 * nRow)) - 1) %% (nCol - 2) + 2
  parCell = parCell[((nCol - 2) * 2 + 1):length(parCell)]

  # create a border vector with states from land0 to be added at each time step (the border will not be updated TODO: find a way to consider the border)
  indexToAdd = which(!(1:(nCol * nRow) %in% position))
  border = land0[indexToAdd]

  for(i in 1:steps) {

    # run for all cells
    land = parallel::mcmapply(function(cell) cellRun(cell, neighbor, land0, pars, parCell, i, plantInt, harvInt, thinInt, enrichInt, stoch, nCol), seq_along(neighbor), mc.cores = cores)

    land1 = setNames(land, position)

    # add border to keep same size at each time step
    #land1[(length(land1) + 1):(nCol * nRow)] = border TODO: this does not keep the name
    land1 = c(land1, border)
    land1 = land1[match(1:length(land1), as.numeric(names(land1)))] # sort to keep same order

    land0 <- land1 # update land0 for next time step

    # save the land i step
    lands[[paste0('land_T', i)]] <- land1

    # print progress
    cat("==>", format(100*i/steps, digits = 4), "%", "\r")
  }

  # keep all output lands or just a part of it?
  if(!is.na(outputLand)) {
    y = paste0('land_T', outputLand)
    lands <- lands[y]
  }

  # add steps, management and RCP information
  lands[['env1']] <- initLand[['env1']]
  lands[['steps']] <- steps
  lands[['manag']] <- list(plantInt = plantInt, harvInt = harvInt, thinInt = thinInt, enrichInt = enrichInt)
  lands[['RCP']] <- RCP
  lands[['nCol']] <- nCol
  lands[['nRow']] <- nRow

  # save or simply return the output
  if(saveOutput == TRUE) {
      # define fileName
      if(is.null(fileOutput)) {
        fileName <- paste0('RCP', RCP, paste(c(plantInt, harvInt, thinInt, enrichInt), collapse = ''))
      }else {
        fileName = fileOutput
      }
      # define directory
      if(is.null(foulderOutput)) {
        directoryName <- paste0('output/', fileName, '.RDS')
      }else {
        if(!dir.exists(foulderOutput)) dir.create(foulderOutput) # ckeck if directory exists and if not, create it
        directoryName <- paste0(foulderOutput, '/', fileName, '.RDS')
      }
    saveRDS(lands, file = directoryName)
  }else {
    return(lands)
  }
}
