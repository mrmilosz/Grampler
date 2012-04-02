source('wavebank.r')
source('spectrum.r')
source('graphs.r')

sampleDirectory='DrumSamples'
samplePaths <- list.files(sampleDirectory, pattern='wav$', recursive=TRUE)
distanceFilePath <- 'cache.dat'
distances <- NULL

# This function takes the n samples in samplePaths and builds an n by n matrix of comparisons.
# Obviously, there should be zeros on the diagonal! They are computed anyway out of paranoia.
# The matrix is stored in the "distances" variable, and also saved to disk cache.
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

# Returns the first few neighbors, and their distances, of a given sound file.
# e.g. sortedNeighbors('Tambourines/tambourine1.wav', 20)
sortedNeighbors <- function(path, amount=NULL, ...) {
  if (is.null(amount)) {
    amount=length(samplePaths)
  }
  sampleDistances <- distances[which(samplePaths == path),]
  t(vapply(sort(sampleDistances, index.return=TRUE, ...)$ix, function(i) { c(samplePaths[i], sampleDistances[i]) }, c('', 0))[,1:amount])
}

# Graphs the given sample
# e.g. graphSample('Tambourines/tambourine1.wav')
graphSample <- function(path, ...) {
  soundwave <- wave.fromWav(paste(sampleDirectory, path, sep='/'))
  par(mfrow=c(2, 1))
  oscillograph(soundwave)
  spectrograph(soundwave, ...)
}

# Compute and cache the distance matrix if no cache file is present.
# Otherwise, load the cached matrix.
if (file.exists(distanceFilePath)) {
  load(distanceFilePath)
} else {
  computeDistanceMatrix()
  load(distanceFilePath)
}
