###############################
# Automate the creation of R script and bash files to run simulation
# Simulation to test for cell size
# Will Vieira
# March 30, 2019
##############################

##############################
# Steps:
  # define simulation variants
  # create initial landscapes (30 for each cell size)
  # create all subfolders for the simulation output
  # create 6 x 5 run files for simulation (R script + bash)
  # create Rscript to run all simulations using submit bash
##############################

file.sources <- dir('R/')
invisible(sapply(paste0('R/', file.sources), source))
params = read.table("data/pars.txt", row.names = 1)

set.seed(42)

# define simulation variants

  # 6 different sizes interacting with 5 management scenarios = 30 simulations
  # Each simulation will be repeated 30 times following a different initial landscape
  cellSize = c(0.3, 0.5, 0.8, 1, 2.5, 5)
  manag = c(noManag = 0, plant = .15, harv = .15, thin = .15, enrich = .15)
  reps = 30

#



# create initial landscapes (30 for each cell size)

  # create folder to store all landscapes
  initLandFoder = 'initLandscape'
  if(!dir.exists(initLandFoder)) dir.create(initLandFoder)

  # file names
  initLandFiles = do.call(paste, c(expand.grid(paste0("initLand_cellSize_", cellSize), paste0("_rep_", 1:reps), ".RDS"), sep = ""))

  # check if initial landscapes are already created (very time consuming)
  files = dir(initLandFoder)
  run = ifelse(!all(initLandFiles %in% files), T, F)

  # 1 land for each repetion x 6 different cell size = 180 initLand objects
  if(run == TRUE) {
  count = 0
    for(cellSz in cellSize) {
      for(rep in 1:reps) {
        saveRDS(create_landscape(climRange = c(-2.5, 0.35), cellSize = cellSz), file = paste0(initLandFoder, '/initLand_cellSize_', cellSz, '_rep_', rep, '.RDS'))
        cat('    creating initial landscapes', round((rep + count)/(length(cellSize) * reps) * 100, 1), '%\r')
      }
      count = count + reps
    }
  }

#



# create all subfolders for the simulation output

  mainFolder = 'output'
  cs <- paste0('cellSize_', cellSize)
  mg <- paste0(names(manag), '_', manag)
  folders <- do.call(paste, c(expand.grid(cs, mg), sep = "_"))
  invisible(sapply(paste0(mainFolder, '/', folders), dir.create))

#



# create 6 x 5 run files for simulation (R script + bash)
for(cellSz in cellSize) {
  for(mg in seq_along(manag)) {

    # define simulation name
    simName = paste0('simCellSize_', cellSz, '_', names(manag)[mg], '_', manag[mg])
    # define initLand file
    initFile = paste0(initLandFoder, '/initLand_cellSize_', cellSz, '_rep_')
    # define fileOutput
    fOutput = paste0('cellSize', cellSz, '_', names(manag)[mg], manag[mg], '_rep_')
    # define folderOutput
    fdOutput = paste0('cellSize_', cellSz, '_', names(manag)[mg], '_', manag[mg])
    # management
    if(names(manag)[mg] == "noManag") {
      management = c(0, 0, 0, 0)
    }else if(names(manag)[mg] == "plant") {
      management = c(manag['plant'], 0, 0, 0)
    }else if(names(manag)[mg] == "harv") {
      management = c(0, manag['harv'], 0, 0)
    }else if(names(manag)[mg] == "thin") {
      management = c(0, 0, manag['thin'], 0)
    }else if(names(manag)[mg] == "enrich") {
      management = c(0, 0, 0, manag['enrich'])
    }else{
      stop('Management not available')
    }
    # memory usage
    if(cellSz == 0.3) {
      mem = 7000
    }else if(cellSz == 0.5) {
      mem = 3000
    }else{
      mem = 1500
    }

# R script with cluster running
Rscript <- paste0("## Script to run 30 rep simulation

# functions
file.sources <- dir(\"R/\")
invisible(sapply(paste0(\"R/\", file.sources), source))
params = read.table(\"data/pars.txt\", row.names = 1)

# run function to be used multiple times
run <- function(i) {

  initLand <- readRDS(paste0(\"", initFile, "\", i, \".RDS\"))

  run_model(steps = 150, initLand,
             plantInt = ", management[1], ",
             harvInt = ", management[2], ",
             thinInt = ", management[3], ",
             enrichInt = ", management[4], ",
             RCP = 4.5, # either 0, 2.6, 4.5, 6.0 and 8.5
             stoch = T,
             saveRangeLimit = TRUE,
             occup = 0.75,
             outputLand = 150,
             saveOutput = T,
             fileOutput = paste0(\"", fOutput, "\", i), # name of the file
             folderOutput = \"", fdOutput, "\"
  )
  print(i)
}

library(parallel)

# Define the number of cores
no_cores <- ", reps, "

# Initiate cluster
cl <- makeCluster(no_cores, type = \"FORK\")

# run in parellel
parLapply(cl, X = 1:no_cores, function(X) run(X))

# stop cluster
stopCluster(cl)")


# bash submit file
bash <- paste0("#!/bin/bash

#SBATCH --account=def-dgravel
#SBATCH -t 1-00:00:00
#SBATCH --mem-per-cpu=", mem, "
#SBATCH --ntasks=", reps, "
#SBATCH --job-name=cs", cellSz, names(manag)[mg], manag[mg], "
#SBATCH --mail-user=willian.vieira@usherbrooke.ca
#SBATCH --mail-type=ALL

NCORES=$SLURM_CPUS_PER_TASK R --vanilla <<code", "\n",
Rscript, "
code")

  # Save bash file
  system(paste0("echo ", "'", bash, "' > ", simName, '.sh'))
  }
}

#



# Rscript to run all simulations
  runSim <- "
# list all bash runs
bashFiles <- list.files(pattern = \".sh\")

for(sh in bashFiles) {
  system(paste0(\"sbatch \", sh))
}
"

  # save run R script
  system(paste0("echo ","'", runSim, "' > ", 'runSimulation.R'))
#
#
