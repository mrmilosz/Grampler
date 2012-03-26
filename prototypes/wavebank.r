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
  sin(2 * pi * (freq * 1:numSamples + phase) / sampleRate)
}

wave.sinevibrato <- function(numSamples=22050, sampleRate=44100, f=440, amp=0.5, speed=4) {
  freq <- f * 2 ^ (amp * sin(2 * pi * speed * 1:numSamples / numSamples) / 12)
  phase <- numeric(numSamples)
  for (i in 2:numSamples) {
    phase[i] <- phase[i-1] + i * (freq[i-1] - freq[i])
  }
  sin(2 * pi * (freq * 1:numSamples + phase) / sampleRate)
}

wave.weirdsaw <- function(numSamples=22050, sampleRate=44100, f=440, ampvar=0.3, speedvar=8) {
  wave <- numeric(numSamples)
  i <- 1
  h <- f
  while(2 * h < sampleRate) {
    wave <- wave + wave.sinevibrato(numSamples=numSamples, sampleRate=sampleRate, f=h, amp=runif(1)*ampvar, speed=runif(1)*speedvar) / i
    i <- i + 1
    h <- f * i
  }
  wave / max(abs(wave))
}

wave.weirdsaw2 <- function(numSamples=22050, sampleRate=44100, f=440, ampvar=0.35, amperror=0.15, speedvar=2, speederror=1) {
  wave <- numeric(numSamples)
  i <- 1
  h <- f
  while(2 * h < sampleRate) {
    wave <- wave + sin(2 * pi * h * 1:numSamples / sampleRate) / i * (ampvar + (runif(1)*2-1) * amperror) * sin(2 * pi * (speedvar + (runif(1)*2-1) * speederror) * 1:numSamples / sampleRate)
    i <- i + 1
    h <- f * i
  }
  wave / max(abs(wave))
}

wave.weirdsaw3 <- function(numSamples=22050, sampleRate=44100, f=440, fampvar=0.1, fspeedvar=8, ampvar=0.35, amperror=0.15, speedvar=2, speederror=1) {
  wave <- numeric(numSamples)
  i <- 1
  h <- f
  while(2 * h < sampleRate) {
    wave <- wave + wave.sinevibrato(numSamples=numSamples, sampleRate=sampleRate, f=h, amp=runif(1)*fampvar, speed=runif(1)*fspeedvar) / i * (ampvar + (runif(1)*2-1) * amperror) * sin(2 * pi * (speedvar + (runif(1)*2-1) * speederror) * 1:numSamples / sampleRate)
    i <- i + 1
    h <- f * i
  }
  wave / max(abs(wave))
}
