numBands <- 16
numFreqs <- 100
expTicks <- numFreqs * 2^(1:numBands) / 2^numBands
expWidths <- expTicks - c(0, expTicks)[1:numBands]
freqEnergies <- cos(1:numFreqs)+1+rnorm(numFreqs)^2

allTicks <- sort(union(1:numFreqs, expTicks))

bandEnergies <- numeric(numBands)
k <- 1
l <- 1
for (j in 1:numFreqs) {
  while (allTicks[k] < j) {
    bandEnergies[l] <- bandEnergies[l] + energies[j] * (allTicks[k+1] - allTicks[k]) / (expWidths[l]) 
    if (allTicks[k] == expTicks[l]) {
      l <- l + 1
    }
    k <- k + 1
  }
}
