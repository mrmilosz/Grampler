# Waves are in R floating point format. Conversion methods (including to binary) in wavemanip.r.

wave.fromWav <- function(path, ...) {
  readToFloat(path, ...)
}

wave.snare <- function(numSamples=22050) {
  wave <- rnorm(1:numSamples)
  waveFadeFactor <- 10
  waveMax <- max(abs(wave))
  if (waveMax > 0) { wave <- wave / waveMax }
  wave <- wave * ((numSamples / waveFadeFactor) / (0:(numSamples - 1) + numSamples / waveFadeFactor)) * ((numSamples - 1):0 / numSamples)
  wave
}

wave.sineramp <- function(numSamples=22050, sampleRate=44100, f0=22050, f1=30, log=TRUE) {
  env <- 1:numSamples/numSamples
  if (log) {
    freq <- f0 * (f1 / f0) ^ env
  }
  else {
    freq <- f0 + (f1 - f0) * env
  }
  phase <- numeric(numSamples)
  for (i in 2:numSamples) {
    phase[i] <- phase[i-1] + i * (freq[i-1] - freq[i])
  }
  cos(2 * pi * (freq * 1:numSamples + phase) / sampleRate)
}

wave.sinevibrato <- function(numSamples=22050, sampleRate=44100, f=440, amp=10, speed=8, log=TRUE) {
  env <- cos(2 * pi * speed * 1:numSamples / numSamples)
  if (log) {
    freq <- f * (1 + amp / f) ^ env
  }
  else {
    freq <- f + amp * env
  }
  phase <- numeric(numSamples)
  for (i in 2:numSamples) {
    phase[i] <- phase[i-1] + i * (freq[i-1] - freq[i])
  }
  cos(2 * pi * (freq * 1:numSamples + phase) / sampleRate)
}

wave.weirdsaw <- function(numSamples=88200, sampleRate=44100, f=440) {
  wave <- numeric(numSamples)
  i <- 1
  h <- f
  while(2 * h < sampleRate) {
    wave <- wave + wave.sinevibrato(numSamples=numSamples, sampleRate=sampleRate, f=h, amp=20*runif(1), speed=10*runif(1)) / i
    i <- i + 1
    h <- f * i
  }
  wave / max(abs(wave))
}
