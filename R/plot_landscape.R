# Function to plot the landscape grid
  ## Input:
   # - land (list of 2: - List of landscape occupancy and vector of env1)
   # - title if needed
   # - rmBorder (remove border of plot)
   # - rangeLimit (if TRUE will add a line in the plot with the range limit of Boreal and Temperate)
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

plot_landscape <- function(land, Title = NULL, rmBorder = TRUE, rangeLimit = NULL)
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
  # title
  if(!exists('lands')) {
    main <- Title
  }else {
    main <- paste0(Title, '\nPlant = ', lands[['manag']][[1]], '; Harv = ', lands[['manag']][[2]], '; Thin = ', lands[['manag']][[3]], '; Enrich = ', lands[['manag']][[4]], '\nsteps = ', lands[['steps']], '; RCP = ', lands[['RCP']])
  }

  # change par() if rangeLimit is TRUE
  if(!is.null(rangeLimit)) {
    par(mar = c(.5,0.5,3.5,0.5), cex.main = 1, xpd = T)
  }else {
    par(mar = c(.5,0.5,3.5,0.5), cex.main = 1)
  }

  image(x = coordy, y = coordx, xaxt='n', yaxt = 'n', z = landM, xlab = "", ylab = "",
      col = col, main = main, breaks = c(0, 1, 2, 3, 4))

  # add rangeLimit line
  if(!is.null(rangeLimit)) {
    lines(c(rangeLimit[1], rangeLimit[1]), c(dim(landM)[2], -2), lwd = 2)
    lines(c(rangeLimit[2], rangeLimit[2]), c(dim(landM)[2], -2), lwd = 2)
  }

  # add north arrow
  north.arrow = function(x, y, h) {
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y + h/2.4, y), col = "black", border = NA)
    polygon(c(x - h, x, x - (1 + sqrt(3)/2) * h), c(y, y - h/2.4, y))
    #text(x, y, "N", adj = c(7, 0.4), cex = 2.5)
  }
  north.arrow(par("usr")[1]+0.995*diff(par("usr")[1:2]), par("usr")[3]+0.12*diff(par("usr")[3:4]), diff(par("usr")[1:2]) * 0.013)
}
