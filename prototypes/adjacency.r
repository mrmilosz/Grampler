source('wavemanip.r')
source('wavebank.r')
source('bandmatrix.r')
source('heatgraph.r')

# Settable
numBands <- 11
windowLength <- 260
windowEasing <- 130
numSamples <- 22050
# End settable

numWindows <- floor((numSamples - windowEasing) / (windowLength - windowEasing))
soundwaveLength <- numWindows * (windowLength - windowEasing) + windowEasing

sampleDirectory <- 'DrumSamples/Tambourines'
samplePaths <- list.files(sampleDirectory, pattern='wav$', recursive=TRUE)
numSamples <- length(samplePaths)

bandMatrices <- list()
for (i in 1:numSamples) {
  bandMatrices[[i]] <- bandMatrix(
    wave.fromWav(paste(sampleDirectory, samplePaths[i], sep='/'), samples=soundwaveLength),
    windowLength=windowLength, windowEasing=windowEasing, numBands=numBands, numWindows=numWindows
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

closestNeighbors <- function(path) {
  vapply(sort(distances[which(samplePaths == path),], index.return=TRUE)$ix, function(i) { samplePaths[i] }, '')
}
