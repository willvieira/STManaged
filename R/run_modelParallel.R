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

# Function to get a vector of cell position (ignoring border first and last line and column)
getPosition <- function(nRow, nCol) {
  nLines = nRow - 2
  pos = numeric((nCol - 2) * (nRow - 2))
  i = 1
  j = nCol - 2

  for(line in 1:nLines) {
    pos[i:j] = (nCol * line + 2):((nCol * line) + nCol - 1)
    i = j + 1; j = j + nCol - 2
  }

  return(pos)
}

# Function to get a list of neighborhood index for each cell
getNeighbor <- function(nRow, nCol) {
  jump = nCol - 3
  cellPerRow = nCol - 2
  lineNeighbor <- c(1:3, (4:6 + jump), (7:9 + (jump * 2)))
  totalCells = (nCol - 2) * (nRow - 2)

  neighList = rep(list(NA), totalCells)
  count = 0; count2 = 0
  for(i in 1:totalCells) {
    neighList[[i]] = lineNeighbor + count + (nCol * count2)
    count = count + 1
    if(count == cellPerRow) {
      count = 0
      count2 = count2 + 1
    }
  }
  return(neighList)
}

# function to be used in the lapply (TODO: figure out how to use ... here)
cellRun <- function(cell, neighbor, unLand0, pars, parCell, i, plantInt, harvInt, thinInt, enrichInt, stoch, nCol) {

  y0 <- neighbor_prop(unLand0[neighbor[[cell]]])
  y1 <- model_fm(t = 1, y = y0, params = pars[[i]][, parCell[cell]], plantInt, harvInt, thinInt, enrichInt)
  y1 <- y0 + unlist(y1) # update cell
  y1['R'] <- 1 - sum(y1)
  if(stoch == T) {
    cell <- which(rmultinom(n = 1, size = 1, prob = y1) == 1) # get a state depending on the probability `p`
  }else {
    cell <- which(y1 == max(y1)) # update landStep
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
  lands <- setNames(list(initLand), 'land_T0')
  land0 <- initLand[['land']]

  # lands information
  nRow = length(land0)
  nCol = length(land0[[1]])
  position = getPosition(nRow = nRow, nCol = nCol)
  neighbor = getNeighbor(nRow = nRow, nCol = nCol)
  parCell = (seq(1, (nRow * nCol - 2 * nRow)) - 1) %% (nCol - 2) + 2
  parCell = parCell[((nCol - 2) * 2 + 1):length(parCell)]

  # unlist land0 in a vector
  unLand0 = setNames(unlist(land0), 1:(nCol * nRow))

  # create a border vector with states from land0 to be added at each time step (the border will not be updated TODO: find a way to consider the border)
  indexToAdd = which(!(1:(nCol * nRow) %in% position))
  border = unLand0[indexToAdd]

  for(i in 1:steps) {

    # run for all cells
    land = parallel::mclapply(seq_along(neighbor), function(cell) cellRun(cell, neighbor, unLand0, pars, parCell, i, plantInt, harvInt, thinInt, enrichInt, stoch, nCol), mc.cores = cores)
    unLand1 = setNames(unlist(land), position)

    # add border to keep same size at each time step
    unLand1 = c(unLand1, border)
    unLand1 = unLand1[match(1:length(unLand1), as.numeric(names(unLand1)))] # sort to keep same order

    unLand0 <- unLand1 # update land0 for next time step

    # list unLand1
    landStep = split(unLand1, cut(seq_along(unLand1), nRow, labels = FALSE))
    # save the land i step
    lands[[paste0('land_T', i)]] <- setNames(list(landStep), 'land')

    # print progress
    cat("==>", format(100*i/steps, digits = 4), "%", "\r")
  }

  # keep all output lands or just a part of it?
  if(!is.na(outputLand)) {
    y = paste0('land_T', outputLand)
    lands <- lands[y]
  }

  # add steps, management and RCP information
  lands[['steps']] <- steps
  lands[['manag']] <- list(plantInt = plantInt, harvInt = harvInt, thinInt = thinInt, enrichInt = enrichInt)
  lands[['RCP']] <- RCP

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
