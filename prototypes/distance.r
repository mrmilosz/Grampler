source('wavebank.r')
source('spectrum.r')

sampleDirectory='DrumSamples'
samplePaths <- list.files(sampleDirectory, pattern='wav$', recursive=TRUE)
distanceFilePath <- 'cache.dat'
distances <- NULL

computeDistanceMatrix <- function(numBands=11, windowLength=260, windowEasing=130, numSamples=22050) {
  numWindows <- floor((numSamples - windowEasing) / (windowLength - windowEasing))
  soundwaveLength <- numWindows * (windowLength - windowEasing) + windowEasing

  samplePaths <- list.files(sampleDirectory, pattern='wav$', recursive=TRUE)
  numSamples <- length(samplePaths)

  spectra <- list()
  for (i in 1:numSamples) {
    spectra[[i]] <- spectrum(
      wave.fromWav(paste(sampleDirectory, samplePaths[i], sep='/'), numSamples=soundwaveLength),
      windowLength=windowLength, windowEasing=windowEasing, numBands=numBands, numWindows=numWindows,
      normalize=TRUE
    )
    print(paste('Computed spectrograph for', samplePaths[i]))
  }

  distances <- matrix(0, nrow=numSamples, ncol=numSamples)

  print('Comparing and sorting...')
  for (i in 1:numSamples) {
    for (j in 1:numSamples) {
      distances[i,j] <- sqrt(mean((spectra[[i]]$energies - spectra[[j]]$energies)^2))
    }
  }
  print('Database is ready!')

  save(distances, file=distanceFilePath)
}

sortedNeighbors <- function(path, amount=NULL, ...) {
  if (is.null(amount)) {
    amount=length(samplePaths)
  }
  sampleDistances <- distances[which(samplePaths == path),]
  t(vapply(sort(sampleDistances, index.return=TRUE, ...)$ix, function(i) { c(samplePaths[i], sampleDistances[i]) }, c('', 0))[,1:amount])
}

if (file.exists(distanceFilePath)) {
  load(distanceFilePath)
} else {
  computeDistanceMatrix()
}
