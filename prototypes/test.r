source('bandmatrix.r');
source('myImagePlot.R');

soundwaveLength <- 22050

# Some kind of electronic snare thing
soundwave <- rnorm(1:soundwaveLength)
waveFadeFactor <- 20
soundwaveMax <- max(abs(soundwave))
if (soundwaveMax > 0) { soundwave <- soundwave / soundwaveMax }
soundwave <- soundwave * ((soundwaveLength / waveFadeFactor) / (0:(soundwaveLength - 1) + soundwaveLength / waveFadeFactor))

plot(1:soundwaveLength, soundwave, type='l')

run <- function(...) {
  data <- bandmatrix(soundwave, ...)
  myImagePlot(t(data$energies), xLabels=round(data$times, 4), yLabels=round(data$frequencies, 2), title='spectrograph')
}
