# Function to run the model over time
 ## Input:
  # - steps (nb of steps; step * 5 = years)
  # - initLand (output of the `create_landscape` function)
  # - params (parameters)
  # - Forest management intensity [0-1]
  # - RCP (three options of climate change: RCP = 2.6, RCP = 4.5, RCP = 6, RCP = 8.5)
 ## Output:
  # - lands (a list for every step)
  # - Each step (or list) of lands have a list of state occupancy and the respective env1 vector
 ## Extra functions:
  # - neighbor_prop (calculates the proportion between states for each neigbor area - 9 cells)
  # - cc_diff (get the params difference between env1 before and after climate change)

# function to return neighborhood proportion considering the 8 neighbors
neighbor_prop <- function(neighbor) {
  B = sum(neighbor == "B")/9
  T = sum(neighbor == "T")/9
  M = sum(neighbor == "M")/9

  return(setNames(c(B, T, M), c("B", "T", "M")))
}

# function to define the temperature increase over the time steps based in RCP scenarios
cc_diff <- function(env1, # pars as a list for each row of the lanscape
                    RCP) # RCP either 0, 2.6, 4.5, 6 and 8.5
{
  load('data/scale_info.robj')

  # unscale temperature to add climate change
  tempSc0 <- env1
  tempUn0 <- tempSc0 * vars.sd['annual_mean_temp'] + vars.means['annual_mean_temp']

  # add climate change
  if(RCP == 2.6) tempUn1 <- tempUn0 + 1 # increase of 1 degree
  if(RCP == 4.5) tempUn1 <- tempUn0 + 1.8 # 1.8 degrees
  if(RCP == 6) tempUn1 <- tempUn0 + 2.2 # 1.8 degrees
  if(RCP == 8.5) tempUn1 <- tempUn0 + 3.7 # 1.8 degrees
  if(RCP == 0) tempUn1 <- tempUn0

  # scale future temperature
  tempSc1 <- (tempUn1 - vars.means['annual_mean_temp'])/vars.sd['annual_mean_temp']

  # list of parameters for each row cell before and after climate change (temperature gradient)
  pars0 <- lapply(as.list(tempSc0), function(x) get_pars(ENV1 = x, ENV2 = 0, params, int = 5))
  pars1 <- lapply(as.list(tempSc1), function(x) get_pars(ENV1 = x, ENV2 = 0, params, int = 5))

  # list of difference between before and after climate change
  parsDiff <- mapply("-", pars1, pars0, SIMPLIFY = FALSE)

  rer <- setNames(list(pars0, parsDiff), c('pars0', 'parsDiff'))

  return(rer)
}

# Main function to run the model over time
run_model <- function(steps, initLand, params,
                      plantInt = 0,
                      harvInt = 0,
                      thinInt = 0,
                      enrichInt = 0,
                      RCP = 0)
{

  # climate change (define a list of parameters difference between before and after climate change)
  pars0Diff <- cc_diff(initLand[['env1']], RCP = RCP)
  # divide the difference by time steps - 20
  pars0Diff[['parsInc']] <- lapply(pars0Diff[['parsDiff']], function(x) x/(20))

  # lands
  lands <- setNames(list(initLand), 'landT0')
  land0 <- initLand[['land']]

  for(i in 1:steps) {

    landStep <- land0

    # get param with climate change
    if(i == 1) {
      pars <- pars0Diff[['pars0']] # first time step with original env1
    }else if(i > 1 && i < 20) { # climate change increasing from 2:20 steps (= 100 years)
      pars <- mapply("+", pars, pars0Diff[['parsInc']], SIMPLIFY = FALSE)
    }else { # keep climate stable for last steps
      pars <- pars
    }

    for(c in 2:(length(land0)-1)) {
      for(r in 2:(length(land0[[1]])-1)) {

        # get neighborhood
        neighbor <- c(land0[[c - 1]][r - 1], land0[[c - 1]][r], land0[[c - 1]][r + 1],
                      land0[[c]][r - 1], land0[[c]][r], land0[[c]][r + 1],
                      land0[[c + 1]][r - 1], land0[[c + 1]][r], land0[[c + 1]][r + 1])
        y0 <- neighbor_prop(neighbor)

        # run the model
        y1 <- model_fm(t = 1, y0, params = pars[[r]], plantInt, harvInt, thinInt, enrichInt)
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
