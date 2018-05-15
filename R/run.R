# Function to run the model over time
 ## Input:
  # - steps (nb of steps; step * 5 = years)
  # - initLand (output of the `create_landscape` function)
  # - params (parameters)
 ## Output:
  # - lands (a list for every step)
  # - Each step (or list) of lands have a list of state occupancy and the respective env1 vector
 ## Extra functions:
  # - neighbor_prop (calculates the proportion between states for each neigbor area - 9 cells)

# function to return neighborhood proportion considering the 8 neighbors
neighbor_prop <- function(neighbor) {
  B = sum(neighbor == "B")/9
  T = sum(neighbor == "T")/9
  M = sum(neighbor == "M")/9

  return(setNames(c(B, T, M), c("B", "T", "M")))
}

# Main function to run the model over time
run_model <- function(steps, initLand, params)
{

  # list of parameters for each row cell (temperature gradient)
  pars <- lapply(as.list(initLand[['env1']]), function(x) get_pars(ENV1 = x, ENV2 = 0, params, int = 5))

  # lands
  lands <- setNames(list(initLand), 'landT0')
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
        y1 <- model_fm(t = 1, y0, params = pars[[r]], plantInt = 0, harvInt = 0, thinInt = 0, enrichInt = 0)
        y1 <- y0 + unlist(y1) # update cell
        landStep[[c]][r] <- names(y1)[which(y1 == max(y1))] # update landStep

      }
    }

    land0 <- landStep # update land0 for next time step
    landStep <- setNames(list(landStep, initLand[['env1']]), c('land', 'env1')) # add temperature info
    lands[[paste0('landT', i)]] <- landStep # save the land i step

  }
  return(lands)
}
