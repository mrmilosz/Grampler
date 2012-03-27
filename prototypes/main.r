source('wavebank.r')
source('graphs.r')

soundwave <- NULL

graph <- function(...) {
  par(mfrow=c(2, 1))
  oscillograph(soundwave)
  spectrograph(soundwave, ...)
}

soundwave <- wave.snare()
