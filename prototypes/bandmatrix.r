tukeyWindow <- function(length, easing=length/2) {
  if (length < 1 || easing < 0 || length < 2 * easing) { NULL }
  else { vapply(0:(length - 1), function(t) {
    if      (t < easing)           { (-cos(pi * t / easing) + 1) * 0.5 }
    else if (t <= length - easing) { 1 }
    else if (t <= length)          { (cos(pi * (t - (length - easing)) / easing) + 1) * 0.5 }
  }, 0) }
}

bandmatrix <- function(soundwave, numBands=16, windowLength=1024, windowEasing=124, sampleRate=44100) {
  soundwaveLength <- length(soundwave)
  numFourierBands <- floor(windowLength / 2)
  window <- tukeyWindow(windowLength, windowEasing)
  logScalingVector <- numBands * log(1:numFourierBands, 2) / log(numFourierBands, 2)
  cumulativeBandwidths <- vapply(1:numBands, function(bandNumber) { length(which(logScalingVector <= bandNumber)) }, 0)
  numWindows <- floor((soundwaveLength - windowEasing) / (windowLength - windowEasing))
  meanEnergies <- matrix(0, nrow=numWindows, ncol=numBands)
  times <- numeric(numWindows)

  i <- 1
  pos <- windowLength
  while (pos <= soundwaveLength) {
    times[i] <- (pos - windowEasing) / sampleRate
    energies <- Mod(fft(soundwave[(pos - windowLength + 1):pos] * window)[2:(numFourierBands + 1)])
    start <- 1
    for (j in 1:numBands) {
      end <- cumulativeBandwidths[j]
      if (start <= end) {
        meanEnergies[i,j] <- mean(energies[start:end])
      }
      start <- end + 1
    }
    pos <- pos + windowLength - windowEasing
    i <- i + 1
  }

  output <- list()
  output$energies <- meanEnergies
  output$frequencies <- sampleRate * (cumulativeBandwidths) / windowLength
  output$times <- times

  output
}
