# Convenience function
writeFloat <- function(float, path) {
  writePcm(int16ToPcm(floatToInt16(float)), path)
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

pcmToInt16 <- function(pcm) {
  pcmLength <- length(pcm) %/% 2
  out <- numeric(pcmLength)
  for (i in 1:pcmLength) {
    out[i] <- 256 * as.integer(pcm[2 * i]) + as.integer(pcm[2 * i + 1])
    if (out[i] >= 32768) {
      out[i] <- out[i] - 65536
    }
  }
  out
}

int16ToFloat <- function(int16) {
  (int16 + 0.5) / (2^15 - 0.5)
}
