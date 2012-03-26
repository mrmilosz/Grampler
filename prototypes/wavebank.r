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

wave.sine <- function(numSamples=22050, f=440, sampleRate=44100) {
  wave <- sin(2 * pi * f * 1:numSamples / sampleRate)
}

wave.sine.mod <- function(numSamples=22050, f=440, sampleRate=44100, pm=rep(0, numSamples), fm=rep(0, numSamples), rm=rep(1, numSamples)) {
  freqs <- f + fm # Frequency modulation (unit = 1 oscillation per second)
  freqs <- freqs * 2 ^ (pm / 12) # Pitch modulation (unit = 1 note)
  phase <- c(0, cumsum(2:numSamples * (freqs[1:(numSamples-1)] - freqs[2:numSamples]))) # Phase correction
  wave <- sin(2 * pi * (freqs * 1:numSamples + phase) / sampleRate) * rm # Ring modulation (unit = original amplitude)
  wave / max(abs(wave))
}

wave.series <- function(numSamples=22050, f=440, sampleRate=44100, detune=1) { # detune 1 = saw, 2 = square, etc...
  wave <- numeric(numSamples)
  harmonic.numbers <- seq(1, (sampleRate / 2) %/% 440, detune)
  wave <- rowSums(vapply(harmonic.numbers, function(harmonic.number) { wave.sine(f=f*harmonic.number, numSamples=numSamples, sampleRate=sampleRate) / harmonic.number }, rep(0, numSamples)))
  wave / max(abs(wave))
}

wave.series.phasey <- function(numSamples=22050, f=440, sampleRate=44100, detune=1, speedIncrease=0, rectifySpeed=TRUE) {
  wave <- numeric(numSamples)
  harmonic.numbers <- seq(1, (sampleRate / 2) %/% f, detune)
  tremoloRamp <- 1:numSamples * speedIncrease * 12 / numSamples
  if (rectifySpeed) {
    wave <- rowSums(vapply(harmonic.numbers, function(harmonic.number) { wave.sine.mod(f=f*harmonic.number, numSamples=numSamples, sampleRate=sampleRate, rm=abs(wave.sine.mod(numSamples, harmonic.number / 2, pm=tremoloRamp))) / harmonic.number }, rep(0, numSamples)))
  } else {
    wave <- rowSums(vapply(harmonic.numbers, function(harmonic.number) { wave.sine.mod(f=f*harmonic.number, numSamples=numSamples, sampleRate=sampleRate, rm=wave.sine.mod(numSamples, harmonic.number / 2, pm=tremoloRamp)) / harmonic.number }, rep(0, numSamples)))
  }
  wave / max(abs(wave))
}
