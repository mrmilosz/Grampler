tukeyWindow <- function(length, easing=floor(length/2)) {
  if (length < 1 || easing < 0 || length < 2 * easing) { NULL }
  else { vapply(0:(length - 1), function(t) {
    if      (t < easing)           { (-cos(pi * t / easing) + 1) * 0.5 }
    else if (t <= length - easing) { 1 }
    else if (t <= length)          { (cos(pi * (t - (length - easing)) / easing) + 1) * 0.5 }
  }, 0) }
}

spectrum <- function(soundwave, numBands=11, sampleRate=44100, windowLength=260, windowEasing=floor(windowLength/2), numWindows=NULL, normalize=FALSE) {
  soundwaveLength <- length(soundwave)
  window <- tukeyWindow(windowLength, windowEasing)
  if (is.null(numWindows)) {
    numWindows <- floor((soundwaveLength - windowEasing) / (windowLength - windowEasing))
  }
  soundwaveLength <- min(soundwaveLength, numWindows * (windowLength - windowEasing))

  numFourierBands <- floor(windowLength / 2)
  expTicks <- numFourierBands * 2^(1:numBands) / 2^numBands
  expWidths <- expTicks - c(0, expTicks)[1:numBands]
  allTicks <- sort(union(1:numFourierBands, expTicks))

  bandEnergies <- matrix(0, nrow=numWindows, ncol=numBands)
  times <- numeric(numWindows)

  i <- 1
  pos <- windowLength
  while (pos <= soundwaveLength) {
    times[i] <- (pos - windowEasing) / sampleRate
    fourierEnergies <- Mod(fft(soundwave[(pos - windowLength + 1):pos] * window)[2:(numFourierBands + 1)])

    k <- 1
    l <- 1
    for (j in 1:numFourierBands) {
      while (allTicks[k] < j) {
        bandEnergies[i,l] <- bandEnergies[i,l] + fourierEnergies[j] * (allTicks[k+1] - allTicks[k]) / expWidths[l]
        if (allTicks[k] == expTicks[l]) {
          l <- l + 1
        }
        k <- k + 1
      }
    }

    if (normalize) {
      bandEnergies[i,l] <- bandEnergies[i,l] / mean(bandEnergies[i,l])
    }

    pos <- pos + windowLength - windowEasing
    i <- i + 1
  }

  output <- list()
  output$energies <- bandEnergies
  output$frequencies <- sampleRate * expTicks / windowLength
  output$times <- times

  output
}
