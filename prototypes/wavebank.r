# Waves are in R floating point format. Conversion methods (including to binary) in wavemanip.r.

wave.fromPcm <- function(path, ...) {
  int16ToFloat(pcmToInt16(readPcm(path, ...)))
}

wave.fromWav <- function(path, ...) {
  int16ToFloat(pcmToInt16(stripWavHeader(readPcm(path, ...))))
}

wave.snare <- function(samples=22050) {
  wave <- rnorm(1:samples)
  waveFadeFactor <- 10
  waveMax <- max(abs(wave))
  if (waveMax > 0) { wave <- wave / waveMax }
  wave <- wave * ((samples / waveFadeFactor) / (0:(samples - 1) + samples / waveFadeFactor)) * ((samples - 1):0 / samples)
  wave
}

wave.sineramp <- function(samples=22050, sampleRate=44100, f0=22050, f1=30, ramp=1:samples/samples, log=TRUE) {
  if (log) {
    freq <- f0 * (f1 / f0) ^ ramp
  }
  else {
    freq <- f0 + (f1 - f0) * ramp
  }
  phase <- numeric(samples)
  for (i in 2:samples) {
    phase[i] <- phase[i-1] + i * (freq[i-1] - freq[i])
  }
  cos(2 * pi * (freq * 1:samples + phase) / sampleRate)
}
