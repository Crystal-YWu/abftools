#Fundamental functions for ABF2 file I/O. These functions do not do sanity check
#and read/write data as is.

#Simple support for C typed I/O
.ctypes.supported <- c("char", "uchar", "int8", "uint8", "int16", "uint16",
                       "int32", "uint32", "int64", "float", "double", "string",
                       "unused")
#and their corresponding size (in bytes)
.ctypes.bytesize <- c(1L, 1L, 1L, 1L, 2L, 2L, 4L, 4L, 8L, 4L, 8L, 0L, 0L)

sizeof <- .ctypes.bytesize
names(sizeof) <- .ctypes.supported

#Read C-styled struct, as defined by struct.def:
#  struct.def$field: fields of the struct
#  struct.def$type:  type of the field
#  struct.def$ssize: skip (or string) size (in byte) of the field if type is unused/string
#Basically does fread(&result, sizeof(a_struct), n, fptr)
read_struct <- function(fp, struct.def, fptr = 0) {

  result <- list()
  seek(fp, where = fptr, origin = "start")

  byteread <- 0
  byteskip <- 0

  for (i in seq_along(struct.def$field)) {

    fd <- struct.def$field[i]
    tp <- struct.def$ctype[i]
    ss <- struct.def$ssize[i]
    sz <- sizeof[tp]

    #We do not check the sanity of tp or verify sz, because read_struct is only
    #supposed to take struct.def from predefined ABF2 structs only, which are all
    #validated.

    if ( startsWith(tp, "int") | (tp == "char") ) {
      #read a signed integer: char, int8, int16, int32, int64
      result[[fd]] <- readBin(fp, what = "integer", size = sz, signed = TRUE)
      byteread <- byteread + sz
    }
    else if ( (tp == "float") | (tp == "double") ) {
      #read a floating point numbers: float, double
      result[[fd]] <- readBin(fp, what = "numeric", size = sz)
      byteread <- byteread + sz
    }
    else if ( startsWith(tp, "uint") | (tp == "uchar") ) {
      #read an unsigned integer:
      #this gets a little bit tricky because R does not support unsigned long
      if ( sz == 4 ) {
        #uint32: use numeric to store an unsigned long
        b1 <- readBin(fp, what = "integer", size = 2, signed = FALSE)
        b2 <- readBin(fp, what = "integer", size = 2, signed = FALSE)
        #little-endian should be the norm
        if ( .Platform$endian == "little" ) {
          result[[fd]] <- 65536.0 * b2 + b1
        } else {
          result[[fd]] <- 65536.0 * b1 + b2
        }
      } else {
        #uchar, uint8, uint16
        result[[fd]] <- readBin(fp, what = "integer", size = sz, signed = FALSE)
      }
      byteread <- byteread + sz
    }
    else if ( tp == "string" ) {
      #Read strings: if size is given, readBin/rawToChar are called, otherwise
      #read until \0
      if ( ss != 0L ) {
        #In case of multi-byte characters, readBin instead of readChar
        rawstr <- readBin(fp, what = "raw", n = ss)
        result[[fd]] <- rawToChar(rawstr)
        byteread <- length(rawstr)
      } else {
        # string character size is not defined, read into a raw byte vector with until first 0 byte
        rawstr <- c()
        idx <- 0
        repeat {
          tmpchar <- readBin(fp, what = "raw")
          idx <- idx + 1
          rawstr[idx] <- tmpchar
          byteread <- byteread + 1
          if ( tmpchar == 0 ) break
        }
        result[[fd]] <- rawToChar(rawstr)
      }
    }
    else if ( tp == "unused" ) {
      # simply skip ss bytes of data
      seek(fp, where = ss, origin = "current")
      byteskip <- byteskip + ss
      result[[fd]] <- 0
    }

  }
  result$byte.total <- byteread + byteskip

  return(result)
}

#Calculate total size of a struct for verification purpose if needed.
#This function is unsafe since there could be string of undefined length and we
#would never know the correct size of the string before reading. Currently
#this function always yield correct results because the only string field is
#fFileSignature of a well-defined size of 4. However, features could be broken
#if we introduce parsing of Tag sections etc. in the future.
sizeof_struct <- function(struct.def) {

  size <- 0
  for (i in seq_along(struct.def$field)) {
    tp <- struct.def$ctype[i]
    ss <- struct.def$ssize[i]
    size <- size + sizeof[tp] + ss
  }

  return(size)
}

#Calculate actual file pointer from section info
get_fptr <- function(section.info) section.info$uBlockIndex * ABF2.BlockSize

#Read an ABF2 section
#read_section do not check sanity of section.info, this should be done in caller
read_section <- function(fp, section.info, section.def) {

  fptr <- get_fptr(section.info)
  #pre-allocate result data.frame
  n <- section.info$llNumEntries
  m <- length(section.def$field)
  result <- data.frame(matrix(ncol = m, nrow = n))
  colnames(result) <- section.def$field

  if (n == 0) return(result)
  for (i in 1:n) {
    tmp <- read_struct(fp, section.def, fptr)
    fptr <- fptr + tmp$byte.total
    for (j in 1:m)
    {
      result[i, j] <- tmp[[j]]
    }
  }

  return(result)
}

#Read the special string section
#The string section in ABF2 is just a bunch of zero-seperated strings. Total size
#of the section is recorded in uBytes and number of strings is recorded in llNumEntries.
read_str_section <- function(fp, section.info) {

  fptr <- get_fptr(section.info)
  seek(fp, where = fptr, origin = "start")
  rawdata <- readBin(fp, what = "raw", n = section.info$uBytes)
  parsed <- parse_str_section(rawdata)

  return(parsed)
}

#Parse the string section from rawdata
#String sectio is basically a set of \0 seperated strings.
parse_str_section <- function(rawdata) {

  result <- list()
  idx <- 0
  ptr <- ABF2.StringOffset
  if (ptr >= length(rawdata)) return(result)
  for (i in seq(from = ABF2.StringOffset, to = length(rawdata))) {
    if (rawdata[i] == 0) {
      idx <- idx + 1
      result[[idx]] <- rawToChar(rawdata[ptr:(i - 1)])
      ptr <- i + 1
    }
  }

  return(result)
}

#But what's the point of sync array section?
read_synch_arr_section <- function(fp, section.info) {

  n_entries <- section.info$llNumEntries
  n_element <- n_entries * 2

  fptr <- get_fptr(section.info)
  seek(fp, where = fptr, origin = "start")
  data <- array(data = readBin(fp, "integer", n = n_element, size = sizeof["int32"]),
                dim = c(2, n_entries))
  synch_arr <- data.frame(t(data))
  colnames(synch_arr) <- c("lStart", "lLength")

  return(synch_arr)
}

#Read raw data section
read_data_section <- function(fp, section.info) {

  datasize <- section.info$uBytes
  datatype <- ifelse(datasize == 2, "integer", "numeric")
  datalen <- section.info$llNumEntries

  fptr <- get_fptr(section.info)
  seek(fp, where = fptr, origin = "start")
  rawdata <- readBin(fp, what = datatype, size = datasize, n = datalen, signed = TRUE)

  return(rawdata)
}
