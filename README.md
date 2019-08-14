# STManaged: State and transition model for the eastern North American forest

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis build status](https://travis-ci.org/willvieira/STManaged.svg?branch=noAnimation)](https://travis-ci.org/willvieira/STManaged) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/mypax31p5fr0uf44/branch/noAnimation?svg=true)](https://ci.appveyor.com/project/WillianVieira/stmanaged/branch/master) [![codecov](https://codecov.io/gh/willvieira/STManaged/branch/master/graph/badge.svg)](https://codecov.io/gh/willvieira/STManaged)

The `{STManaged}` R package runs the State and transition model for the eastern North American forest, with integrated forest management practices. This package allows you to spatially-explicit model the dynamics of four forest states (Boreal, Temperate, Mixed and Regeneration) over space and time. You will be able to set the intensity of four management practices (plantation, harvest, thinning and enrichment) that aim to increase the northward range shift of forest.

## Installation

```r
devtools::install_github("willvieira/STManaged", ref = 'noAnimation')
```

## Quick start

```r
library(STManaged)

# Create the initial landscape defining the range of annual mean temperature and the cell size:
initLand <- create_landscape(climRange = c(-2.61, 5.07), cellSize = 2)

# Print the initial landscape
plot_landscape(initLand, Title = 'initial_landscape')

# Run the model for 200 years with temperature increase of 1.8 degrees
lands <- run_model(steps = 40,
                   initLand,
                   managInt = c(0, 0, 0, 0),
                   RCP = 4.5)

# Some functions are already built in to check the model output
## Forest state occupancy for first and last year
par(mfrow = c(2, 1))
plot_occupancy(lands, step = 0, spar = 0.4)
plot_occupancy(lands, step = 40, spar = 0.4)

## Range limit shift of Boreal and Temperate states over time
plot_rangeShift(lands, rangeLimitOccup = 0.7)
