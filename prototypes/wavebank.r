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

# Convenience functions
writeFloat <- function(float, path) {
  writePcm(int16ToPcm(floatToInt16(float)), path)
}
wave.fromPcm <- function(path) {
  int16ToFloat(pcmToInt16(readPcm(path)))
}
wave.fromWav <- function(path) {
  int16ToFloat(pcmToInt16(stripWavHeader(readPcm(path))))
}

stripWavHeader <- function(binary) {
  binary[45:length(binary)]
}

floatToInt16 <- function(float) {
  as.integer(floor(float * (2^(15) - 0.5)))
}
int16ToPcm <- function(int16) {
  as.raw(as.vector(vapply(int16, function(int) { if (int < 0) { int <- int + 65536 }; c(int %% 256, int %/% 256) }, c(0, 0))))
}
writePcm <- function(pcm, path) {
  to.write <- file(path, 'wb')
  writeBin(pcm, to.write)
  close(to.write)
}

readPcm <- function(path, samples=22050) {
  to.read <- file(path, 'rb')
  pcm <- readBin(to.read, what='raw', n=samples * 2)
  close(to.read)
  pcm
}
pcmToInt16 <- function(bin) {
  pcmLength <- length(bin) %/% 2
  out <- numeric(pcmLength)
  for (i in 1:pcmLength) {
    out[i] <- 256 * as.integer(bin[2 * i]) + as.integer(bin[2 * i + 1])
    if (out[i] >= 32768) {
      out[i] <- out[i] - 65536
    }
  }
  out
}
int16ToFloat <- function(wave) {
  (wave + 0.5) / (2^15 - 0.5)
}
