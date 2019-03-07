# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense",
  description = "A landscape fire model, sensitive to environmental changes (e.g.
                 weather and land-cover).",
  keywords = c("fire", "percolation", "environmental control", "feedback", 
                "weather", "vegetation", "land-cover"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense.Rmd"),
  reqdPkgs = list("data.table", "raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "mapping", class = "character, list", default = NULL,
                    desc = "optional named vector or list of character strings 
                            mapping one or more inputs required by the module to
                            objects loaded in the simList environment."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(name = ".plotInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start plotting."),
    defineParameter(name = ".plotInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between plot events.")
  ),
  inputObjects = rbind(
    expectsInput(
      objectName = "ignitionProb",
      objectClass = "RasterLayer",
      sourceURL = NA_character_,
      desc = "A RasterLayer or RasterStack (time series) describing spatial
              variations in ignition probabilities."
    ),
    expectsInput(
      objectName = "escapeProb",
      objectClass = "RasterLayer",
      sourceURL = NA_character_,
      desc = "A RasterLayer or RasterStack (time series) describing spatial
              variations in escape probabilities."
    ),
    expectsInput(
      objectName = "spreadProb",
      objectClass = "RasterLayer",
      sourceURL = NA_character_,
      desc = "A RasterLayer or RasterStack describing spatial variations in the
              spread probabilities."
    )
  ),
  outputObjects = rbind(
    createsOutput(
      objectName = "burnMap",
      objectClass = "RasterLayer",
      desc = "A RasterLayer describing how which pixels burned this timestep."
    ),
    createsOutput(
      objectName = "burnMapCumul",
      objectClass = "RasterLayer",
      desc = "A RasterLayer describing how many times each pixel burned over the
              course of the simulation."
    )
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense = function(sim, eventTime, eventType, debug = FALSE) 
{
  switch(
    eventType,
    init = { sim <- init(sim) },
    burn = { sim <- burn(sim) },
    save = { sim <- save(sim) },
    plot = { sim <- plot(sim) },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  invisible(sim)
}


init <- function(sim) 
{
  moduleName <- current(sim)$moduleName
  
  sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "burn")
  
  if (!is.na(P(sim)$.saveInitialTime))
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last() + 1)
  
  if (!is.na(P(sim)$.plotInitialTime))
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "plot", .last())
  
  sim
}

burn <- function(sim) 
{
  moduleName <- current(sim)$moduleName
  currentTime <- time(sim, timeunit(sim))

  ## Mapping
    if (!is.null(P(sim)[["mapping"]][["ignitionProb"]]))
      sim[["ignitionProb"]] <- sim[[P(sim)[["mapping"]][["ignitionProb"]]]]
  
    if (!is.null(P(sim)[["mapping"]][["escapeProb"]]))
      sim[["escapeProb"]] <- sim[[P(sim)[["mapping"]][["escapeProb"]]]]
    
    if (!is.null(P(sim)[["mapping"]][["spreadProb"]]))
      sim[["spreadProb"]] <- sim[[P(sim)[["mapping"]][["spreadProb"]]]]
    
  ## Ignite
  ignitionProb <- sim[["ignitionProb"]][]
  isNA <- is.na(ignitionProb)
  ignitionProb <- ignitionProb[!isNA]
    
  ignited <- which(
    rbinom(n = length(ignitionProb),
           size = 1,
           prob = pmin(ignitionProb, 1)
    ) > 0
  )
  
  rm(ignitionProb)
  
  ## Escape
  loci <- ignited[sim[["escapeProb"]][!isNA][ignited] > runif(length(ignited))]
  rm(ignited)
  
  if (length(loci) > 0L)
  {
    ## Spread
    fires <- SpaDES.tools::spread(
      sim[["spreadProb"]],
      loci = loci, 
      spreadProb = sim[["spreadProb"]],
      returnIndices = TRUE
    )
    
    sim$burnMap <- raster(sim[["spreadProb"]])
    sim$burnMap[fires$indices] <- 1
    
    if (is.null(sim[["burnMapCumul"]]))
    {
      sim$burnMapCumul <- sim[["spreadProb"]]
      sim$burnMapCumul[!is.na(sim$burnMapCumul[])] <- 0
    }
    
    sim$burnMapCumul[fires$indices] <- sim$burnMapCumul[fires$indices] + 1
    
    #sim$fireSize[[time(sim) - start(sim) + 1L]] <- tabulate(fires[["id"]])
  }
  
  if (!is.na(P(sim)$.runInterval))
    sim <- scheduleEvent(sim, currentTime + P(sim)$.runInterval, moduleName, "burn")
  
  invisible(sim)
}


save <- function(sim) 
{
  sim <- saveFiles(sim)
  
  if (!is.na(P(sim)$.saveInterval))
    sim <- scheduleEvent(sim, time(sim, timeunit(sim)) + P(sim)$.saveInterval, current(sim)$moduleName, "save")
  
  invisible(sim)
}


plot <- function(sim) 
{
  Plot(sim$burnMap, sim$burnMapCumul, title = c("Burn map", "Cumulative burn map"))
  
  if (!is.na(P(sim)$.plotInterval))
    sim <- scheduleEvent(sim, time(sim, timeunit(sim)) + P(sim)$.plotInterval, current(sim)$moduleName, "plot")
  
  invisible(sim)
}
