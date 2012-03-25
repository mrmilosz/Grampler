# Convenience methods

readToFloat <- function(path, ...) {
  int16ToFloat(wavToInt16(readWav(path, ...)))
}

writeFromFloat <- function(wave, path, ...) {
  writeWav(int16ToBytes(floatToInt16(wave)), path, ...)
}

#
# PCM input methods
#

# int16 to float
int16ToFloat <- function(int16) {
  (int16 + 0.5) / (2^15 - 0.5)
}

# Bytes to char
bytesToChar <- function(bytes) {
  rawToChar(bytes)
}

# Little endian bytes to int16
bytesToInt16 <- function(bytes) {
  int16Length <- length(bytes) %/% 2
  out <- numeric(int16Length)
  for (i in 0:(int16Length-1)) {
    out[i+1] <- as.integer(bytes[2 * i + 1]) + 256 * as.integer(bytes[2 * i + 2])
    if (out[i+1] >= 32768) {
      out[i+1] <- out[i+1] - 65536
    }
  }
  out
}

# Little endian bytes to int32
bytesToInt32 <- function(bytes) {
  int32Length <- length(bytes) %/% 4
  out <- numeric(int32Length)
  for (i in 0:(int32Length-1)) {
    out[i+1] <- as.integer(bytes[4 * i + 1]) + 256 * as.integer(bytes[4 * i + 2]) + 65536 * as.integer(bytes[4 * i + 3]) + 16777216 * as.integer(bytes[4 * i + 4])
    if (out[i+1] >= 2147483648) {
      out[i+1] <- out[i+1] - 4294967296
    }
  }
  out
}

# Read a wave file into a structure containing all wav metadata
readWav <- function(path, numSamples=NULL) {
  to.read <- file(path, 'rb')

  bytes <- readBin(to.read, what='raw', n=44)

  ChunkID <- bytesToChar(bytes[1:4])

  if (ChunkID != 'RIFF') {
    close(to.read)
    stop('File being parsed does not have ChunkID == "RIFF" (got ', ChunkID, ' instead)')
  }

  ChunkSize <- bytesToInt32(bytes[ 5: 8])
  Format    <- bytesToChar( bytes[ 9:12])

  if (Format != 'WAVE') {
    close(to.read)
    stop('RIFF file being parsed does not have format == "WAVE" (got ', Format, ' instead)')
  }

  Subchunk1ID <- bytesToChar(bytes[13:16])

  if (Subchunk1ID != 'fmt ') {
    close(to.read)
    stop('WAVE file being parsed does not have Subchunk1ID == "fmt " (got ', Subchunk1ID, ' instead)')
  }

  Subchunk1Size <- bytesToInt32(bytes[17:20])

  if (Subchunk1Size != 16) {
    close(to.read)
    stop('WAVE file being parsed does not have Subchunk1Size == 16 (got ', Subchunk1Size, ' instead)')
  }

  AudioFormat <- bytesToInt16(bytes[21:22])

  if (AudioFormat != 1) {
    close(to.read)
    stop('WAVE file being parsed does not have AudioFormat == 1 (PCM) (got ', AudioFormat, ' instead)')
  }

  NumChannels <- bytesToInt16(bytes[23:24])
  SampleRate  <- bytesToInt32(bytes[25:28])

  if (SampleRate != 44100) { # Potentially eliminate one day
    close(to.read)
    stop('WAVE file being parsed does not have SampleRate == 44100 (only 44100 is supported at this time, got ', SampleRate, ' instead)')
  }

  ByteRate      <- bytesToInt32(bytes[29:32])
  BlockAlign    <- bytesToInt16(bytes[33:34])
  BitsPerSample <- bytesToInt16(bytes[35:36])

  if (BitsPerSample != 16) { # Potentially eliminate one day
    close(to.read)
    stop('WAVE file being parsed does not have BitsPerSample == 16 (only 16 is supported at this time, got ', BitsPerSample, ' instead)')
  }

  if (BlockAlign != NumChannels * BitsPerSample/8) {
    close(to.read)
    stop('WAVE file being parsed does not have BlockAlign == NumChannels * BitsPerSample/8 (got ', BlockAlign, ' instead)')
  }

  if (ByteRate != SampleRate * BlockAlign) {
    close(to.read)
    stop('WAVE file being parsed does not have ByteRate == SampleRate * BlockAlign (got ', ByteRate, ' instead)')
  }

  Subchunk2ID <- bytesToChar(bytes[37:40])

  if (Subchunk2ID != 'data') {
    close(to.read)
    stop('WAVE file being parsed does not have Subchunk2ID == "data" (got ', Subchunk2ID, ' instead)')
  }

  Subchunk2Size <- bytesToInt32(bytes[41:44])

  if (Subchunk2Size != ChunkSize - 36) {
    close(to.read)
    stop('WAVE file being parsed does not have Subchunk2Size + 36 == ChunkSize - 36 (got ', Subchunk2Size, ' instead)')
  }

  if (!is.null(numSamples)) {
    Subchunk2Size <- numSamples * BlockAlign
    ChunkSize <- Subchunk2Size + 36
  }

  bytes <- readBin(to.read, what='raw', n=Subchunk2Size)
  bytes <- c(bytes, as.raw(rep(0, Subchunk2Size - length(bytes))))

  data <- bytesToInt16(bytes)

  close(to.read)

  list(
    ChunkID=ChunkID, ChunkSize=ChunkSize, Format=Format,
    Subchunk1ID=Subchunk1ID, Subchunk1Size=Subchunk1Size,
    AudioFormat=AudioFormat, NumChannels=NumChannels,
    SampleRate=SampleRate, ByteRate=ByteRate, BlockAlign=BlockAlign,
    BitsPerSample=BitsPerSample, Subchunk2ID=Subchunk2ID,
    Subchunk2Size=Subchunk2Size, data=data
  )
}

# WAVE to int16 (discards metadata and mixes to mono)
wavToInt16 <- function(wav) {
  rowSums(matrix(wav$data, ncol=wav$NumChannels)) / wav$NumChannels
}

#
# PCM output methods
#

# float to int16
floatToInt16 <- function(float) {
  as.integer(floor(float * (2^(15) - 0.5)))
}

writeWav <- function(bytes, path) {
  bytesLength <- length(bytes)
  to.write <- file(path, 'wb')
  writeBin(to.write, c(
    charToBytes('RIFF'),            # ChunkID       4
    int32ToBytes(36 + bytesLength), # ChunkSize     4
    charToBytes('WAVE'),            # Format        4
    charToBytes('fmt '),            # Subchunk1ID   4
    int32ToBytes(16),               # Subchunk1Size 4
    int16ToBytes(1),                # AudioFormat   2
    int16ToBytes(1),                # NumChannels   2
    int32ToBytes(44100),            # SampleRate    4
    int32ToBytes(88200),            # ByteRate      4
    int16ToBytes(2),                # BlockAlign    2
    int16ToBytes(16),               # BitsPerSample 2
    charToBytes('data'),            # Subchunk2ID   4
    int32ToBytes(bytesLength),      # Subchunk2Size 4
    int16ToBytes(bytes)             # data          Subchunk2Size
  ))
  close(to.write)
}

# Big endian char to bytes
charToBytes <- function(char) {
  charToRaw(char)
}

# int16 to little endian bytes
int16ToBytes <- function(int16) {
  as.raw(as.vector(vapply(int16, function(int) { if (int < 0) { int <- int + 65536 }; c(int %% 256, int %/% 256) }, c(0, 0))))
}

# int32 to little endian bytes
int32ToBytes <- function(int32) {
  as.raw(as.vector(vapply(int16, function(int) { if (int < 0) { int <- int + 4294967296 }; c(int %% 256, int %/% 256, int %/% 65536, int %/% 16777216) }, c(0, 0))))
}
