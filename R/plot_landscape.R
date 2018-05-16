# Function to plot the landscape grid
  ## Input:
   # - land (list of 2: - List of landscape occupancy and vector of env1)
   # - title if needed
  ## Output:
   # - an image()
  ## Extra function:
   # - covertState (function to convert state character to numeric)

 # function to convert state letters in values (needed to plot image)
 convertState <- function(state) {
   if(state == "B") {
     state <- 1
   }else if(state == "T") {
     state <- 2
   }else if(state == "M") {
     state <- 3
   }else{
     state <- 4
   }
   state <- as.numeric(state)
   return(state)
 }

plot_landscape <- function(land, title = NULL, rmBorder = TRUE)
{
  # define coordinates
  nc <- length(land[['land']])
  nr <- length(land[['land']][[1]])

  coordx <- seq(0, nc)
  coordy <- seq(0, nr)

  # convert states in values and then in a matrix
  landConverted <- sapply(unlist(land[['land']], use.names = F), convertState)
  landM <- matrix(landConverted, ncol = length(land[[1]]))

  # remove border of landscape that are not updated
  if(rmBorder == TRUE) {
    landM <- landM[c(-1, -nr), c(-1, -nc)]
    coordx <- seq(1, dim(landM)[2])
    coordy <- seq(1, dim(landM)[1])
  }

  # plot
  col <- c("darkcyan", "orange", "palegreen3", "black")
  main <- paste0(title, '\nPlant = ', land[['manag']][[1]], '; Harv = ',     land[['manag']][[2]], '; Thin = ', land[['manag']][[3]], '; Enrich = ', land[['manag']][[4]], '\nsteps = ', land[['steps']], '; RCP = ', land[['RCP']])
  par(mar = c(0.5,0.5,3.5,0.5))
  image(x = coordy, y = coordx, xaxt='n', yaxt = 'n', z = landM, xlab = "", ylab = "",
      col = col, main = main, breaks = c(0, 1, 2, 3, 4))

  # add north arrow
  north.arrow = function(x, y, h) {
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y + h, y), col = "black", border = NA)
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y - h, y))
    #text(x, y, "N", adj = c(7, 0.4), cex = 2.5)
  }
  north.arrow(max(coordy) - 1, min(coordx) + 2, 1.5)
}
