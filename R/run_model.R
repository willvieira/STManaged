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
  return(c(B = sum(neighbor == 1), T = sum(neighbor == 2), M = sum(neighbor == 3))/9)
}

# Main function to run the model over time
run_model <- function(steps, initLand,
                      managInt = c(0, 0, 0, 0), # for plant, harv, thin and enrich
                      RCP = 0,
                      stoch = T,
                      outputLand = NA, # if NA, everything is out, otherwise specify a vector of timeSteps values [1 - steps]
                      saveRangeLimit = FALSE,
                      occup = 0.75,
                      saveOutput = F,
                      fileOutput = NULL, # name of the file
                      folderOutput = NULL) # name of the output file, if NULL will just save in the mail `output` folder
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
  states = 1:4

  # range limit table (divided by nCol so I can compare with different landcape cell size)
  rangeLimitDF = data.frame(step = 1:(steps + 1), limitB = numeric(steps + 1), limitT = numeric(steps + 1))
  rangeLimitDF[1, 2:3] = range_limit(land0, nRow = nRow, nCol = nCol, occup = occup)/nCol

  for(i in 1:steps) {

    land1 <- land0

    for(cell in 1:length(neighbor)) {

      # get neighborhood
      y0 <- neighbor_prop(land0[neighbor[[cell]]])
      # run the model
      y1 <- model_fm(t = 1, y0, params = pars[[i]][, parCell[cell]], managInt)
      y1 <- y0 + unlist(y1) # update cell
      y1['R'] <- 1 - sum(y1)

      if(stoch == T) {
        land1[neighbor[[cell]][5]] <- states[rmultinom(n = 1, size = 1, prob = y1) == 1] # get a state depending on the probability `p`
      }else {
        land1[neighbor[[cell]][5]] <- states[y1 == max(y1)] # update landStep
      }
    }

    # calculate range limit
    rangeLimitDF[i + 1, 2:3] = range_limit(land1, nRow = nRow, nCol = nCol, occup = occup)/nCol

    land0 <- land1 # update land0 for next time step
    lands[[paste0('land_T', i)]] <- land1 # save land time step

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
  lands[['manag']] <- list(managInt = managInt)
  lands[['RCP']] <- RCP
  lands[['nCol']] <- nCol
  lands[['nRow']] <- nRow
  lands[['rangeLimit']] <- rangeLimitDF

  # save or simply return the output
  if(saveOutput == TRUE) {
    # define fileName
    if(is.null(fileOutput)) {
      fileName <- paste0('RCP', RCP, paste(managInt, collapse = ''))
    }else {
      fileName = fileOutput
    }
    # define directory
    if(is.null(folderOutput)) {
      directoryName <- paste0('output/', fileName, '.RDS')
    }else {
      fo = paste0('output/', folderOutput)
      if(!dir.exists(fo)) dir.create(fo) # ckeck if directory exists and if not, create it
      directoryName <- paste0(fo, '/', fileName, '.RDS')
    }
    saveRDS(lands, file = directoryName)
  }else {
    return(lands)
  }
}
