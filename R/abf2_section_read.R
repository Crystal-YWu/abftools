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
uint <- function(b1, b2) {
  #we have no way to know what endian the file is stored. assume to be same as .Platform
  if ( .Platform$endian == "little" )
    65536.0 * b2 + b1
  else
    65536.0 * b1 + b2
}
assign_struct <- function(struct.def, n = 1L) {

  ans <- list()
  for (i in seq_along(struct.def$field)) {

    fd <- struct.def$field[i]
    tp <- struct.def$ctype[i]

    if ((tp == "int16") || (tp == "int32") || (tp == "int8") ||
        (tp == "int64") || (tp == "char") || (tp == "unused")) {
      ans[[fd]] <- vector(mode = "integer", length = n)
    }
    else if ((tp == "float") || (tp == "double")) {
      ans[[fd]] <- vector(mode = "numeric", length = n)
    }
    else if (startsWith(tp, "uint") || (tp == "uchar")) {
      if (sizeof[tp] == 4) {
        ans[[fd]] <- vector(mode = "numeric", length = n)
      } else {
        ans[[fd]] <- vector(mode = "integer", length = n)
      }
    }
    else if (tp == "string") {
      ans[[fd]] <- vector(mode = "character", length = n)
    }
    else {
      err_ctype(tp)
    }
  }

  ans
}
read_struct_n <- function(fp, struct.def, fptr = NULL, n = 1L) {

  result <- assign_struct(struct.def, n)
  if (!is.null(fptr)) {
    seek(fp, where = fptr, origin = "start")
  }

  for (idx in seq_len(n)) {
    for (i in seq_along(struct.def$field)) {

      fd <- struct.def$field[i]
      tp <- struct.def$ctype[i]
      ss <- struct.def$ssize[i]
      sz <- sizeof[tp]

      if ((tp == "int16") || (tp == "int32") || (tp == "int8") ||
          (tp == "int64") || (tp == "char")) {
        #read a signed integer: char, int8, int16, int32, int64
        result[[fd]][idx] <- readBin(fp, what = "integer", size = sz, signed = TRUE)
      }
      else if ((tp == "float") || (tp == "double")) {
        #read a floating point numbers: float, double
        result[[fd]][idx] <- readBin(fp, what = "numeric", size = sz)
      }
      else if (startsWith(tp, "uint") || (tp == "uchar")) {
        #read an unsigned integer:
        #this gets a little bit tricky because R does not support unsigned long
        if (sz == 4) {
          #uint32: use numeric to store an unsigned long
          b1 <- readBin(fp, what = "integer", size = 2, signed = FALSE)
          b2 <- readBin(fp, what = "integer", size = 2, signed = FALSE)
          result[[fd]][idx] <- uint(b1, b2)
        } else {
          #uchar, uint8, uint16
          result[[fd]][idx] <- readBin(fp, what = "integer", size = sz, signed = FALSE)
        }
      }
      else if (tp == "string") {
        #Read strings: if size is given, readBin/rawToChar are called, otherwise
        #read until \0
        if (ss != 0L) {
          #In case of multi-byte characters, readBin instead of readChar
          rawstr <- readBin(fp, what = "raw", n = ss)
          result[[fd]][idx] <- rawToChar(rawstr)
        } else {
          # string character size is not defined, read into a raw byte vector with until first 0 byte
          result[[fd]][idx] <- readBin(fp, what = "character")
        }
      }
      else if (tp == "unused") {
        # simply skip ss bytes of data
        seek(fp, where = ss, origin = "current")
      }
    }
  }

  result
}

#Calculate actual file pointer from section info
get_fptr <- function(section.info) section.info$uBlockIndex * ABF2.BlockSize

read_section <- function(fp, section.info, section.def, ret.df = TRUE) {

  fptr <- get_fptr(section.info)
  #pre-allocate result data.frame
  n <- section.info$llNumEntries
  ans <- read_struct_n(fp, section.def, fptr, n = n)
  if (ret.df) {
    as.data.frame(do.call(cbind, ans))
  } else {
    ans
  }
}

#Read the special string section
#The string section in ABF2 is just a bunch of zero-seperated strings. Total size
#of the section is recorded in uBytes and number of strings is recorded in llNumEntries.
read_str_section <- function(fp, section.info) {

  fptr <- get_fptr(section.info)
  seek(fp, where = fptr, origin = "start")
  rawdata <- readBin(fp, what = "raw", n = section.info$uBytes)
  parsed <- parse_str_section(rawdata)

  parsed
}
#Parse the string section from rawdata
#String sectio is basically a set of \0 seperated strings.
parse_str_section <- function(rawdata) {

  result <- c()
  ptr <- ABF2.StringOffset
  if (ptr >= length(rawdata)) {
    return(result)
  }
  for (i in seq(from = ABF2.StringOffset, to = length(rawdata))) {
    if (rawdata[i] == 0) {
      result <- c(result, rawToChar(rawdata[ptr:(i - 1)]))
      ptr <- i + 1
    }
  }

  result
}

read_synch_arr_section <- function(fp, section.info) {

  n_entries <- section.info$llNumEntries
  n_element <- n_entries * 2

  fptr <- get_fptr(section.info)
  seek(fp, where = fptr, origin = "start")
  data <- array(data = readBin(fp, what = "integer", size = sizeof["int32"], n = n_element),
                dim = c(2, n_entries))
  synch_arr <- t(data)
  colnames(synch_arr) <- c("lStart", "lLength")

  data.frame(synch_arr)
}

#Read raw data section
read_data_section <- function(fp, section.info) {

  datasize <- section.info$uBytes
  datatype <- ifelse(datasize == 2, "integer", "numeric")
  datalen <- section.info$llNumEntries

  fptr <- get_fptr(section.info)
  seek(fp, where = fptr, origin = "start")
  rawdata <- readBin(fp, what = datatype, size = datasize, n = datalen, signed = TRUE)

  rawdata
}
