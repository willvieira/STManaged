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

# Main function to run the model over time
run_model <- function(steps, initLand,
                      plantInt = 0,
                      harvInt = 0,
                      thinInt = 0,
                      enrichInt = 0,
                      RCP = 0,
                      stoch = T,
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

  for(i in 1:steps) {

    landStep <- land0

    for(c in 2:(length(land0)-1)) {
      for(r in 2:(length(land0[[1]])-1)) {

        # get neighborhood
        neighbor <- c(land0[[c - 1]][r - 1], land0[[c - 1]][r], land0[[c - 1]][r + 1],
                      land0[[c]][r - 1], land0[[c]][r], land0[[c]][r + 1],
                      land0[[c + 1]][r - 1], land0[[c + 1]][r], land0[[c + 1]][r + 1])
        y0 <- neighbor_prop(neighbor)

        # run the model
        y1 <- model_fm(t = 1, y0, params = pars[[i]][,r], plantInt, harvInt, thinInt, enrichInt)
        y1 <- y0 + unlist(y1) # update cell
        y1['R'] <- 1 - sum(y1)

        if(stoch == T) {
          landStep[[c]][r] <- which(rmultinom(n = 1, size = 1, prob = y1) == 1) # get a state depending on the probability `p`
        }else {
          landStep[[c]][r] <- which(y1 == max(y1)) # update landStep
        }
      }
    }

    land0 <- landStep # update land0 for next time step
    landStep <- setNames(list(landStep, initLand[['env1']]), c('land', 'env1')) # add temperature info
    lands[[paste0('land_T', i)]] <- landStep # save the land i step

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
