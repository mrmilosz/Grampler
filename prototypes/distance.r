source('wavebank.r')
source('spectrum.r')

sampleDirectory='DrumSamples'
samplePaths <- list.files(sampleDirectory, pattern='wav$', recursive=TRUE)
distanceFilePath <- 'distances.dat'

computeDistanceMatrix <- function(numBands=11, windowLength=260, windowEasing=130, numSamples=22050) {
  numWindows <- floor((numSamples - windowEasing) / (windowLength - windowEasing))
  soundwaveLength <- numWindows * (windowLength - windowEasing) + windowEasing

  samplePaths <- list.files(sampleDirectory, pattern='wav$', recursive=TRUE)
  numSamples <- length(samplePaths)

  bandMatrices <- list()
  for (i in 1:numSamples) {
    bandMatrices[[i]] <- spectrum(
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
      distances[i,j] <- sqrt(sum((bandMatrices[[i]]$energies - bandMatrices[[j]]$energies)^2) / numSamples)
    }
  }
  print('Database is ready!')

  save(distances, file=distanceFilePath)
}

sortedNeighbors <- function(path, ...) {
  sampleDistances <- distances[which(samplePaths == path),]
  vapply(sort(sampleDistances, index.return=TRUE, ...)$ix, function(i) { c(samplePaths[i], sampleDistances[i]) }, c('', 0))
}

if (file.exists(distanceFilePath)) {
  load(distanceFilePath)
} else {
  computeDistanceMatrix()
}

