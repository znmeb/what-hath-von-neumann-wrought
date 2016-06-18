# constants
.bits <- 40
.words = 4096
.lorder <- 1:8
.laddress <- 9:20
.rorder <- 21:28
.raddress <- 29:40
.bytes <- .bits / binaryLogic::byte()
.multiplier <- 2 ^ (.bits - 1)
.minint <- -.multiplier
.maxint <- .multiplier - 1

#' @title ILLIAC hexadecimal encode
#' @name illiac_hex_encode
#' @description Most ILLIAC docs use the term "sexadecimal" for base 16.
#' Moreover, the digits greater than 9 aren't the ones we normally use, but
#' "K", "S", "N", "J", "F", and "L". which had the right bit patterns on the
#' five-hole paper tape ILLIAC used.
#' @param digit an integer from 0 to 15
#' @return the ILLIAC sexadecimal code for that digit
#' @export illiac_hex_encode

illiac_hex_encode <- function(digit) {
  illiac_hex_codes <-
    c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "K", "S", "N", "J", "F", "L")
  return(illiac_hex_codes[digit + 1])
}

#' @export encode_integer
encode_integer <- function(value) {

  # silently enforce bounds
  integer <- max(.minint, value)
  integer <- min(.maxint, integer)
  binaryLogic::as.binary(integer, signed = TRUE, size = .bytes)
}

#' @export decode_integer
decode_integer <- function(binary) {
  as.numeric(binary)
}

#' @export encode_fraction
encode_fraction <- function(value) {
  encode_integer(.multiplier * value)
}

#' @export decode_fraction
decode_fraction <- function(binary) {
  decode_integer(binary) / .multiplier
}

# internals for memory display
.decode_left_order <- function(binary) as.numeric(binaryLogic::as.binary(
  binary[.lorder], signed = FALSE))
.decode_left_address <- function(binary) as.numeric(binaryLogic::as.binary(
  binary[.laddress], signed = FALSE))
.decode_right_order <- function(binary) as.numeric(binaryLogic::as.binary(
  binary[.rorder], signed = FALSE))
.decode_right_address <- function(binary) as.numeric(binaryLogic::as.binary(
  binary[.raddress], signed = FALSE))
.format_order <- function(order) {
  type <- illiac_hex_encode(order %/% 16)
  variant <- illiac_hex_encode(order %% 16)
  return(paste(type, variant, sep = ""))
}

#' @title Create Memory
#' @name create_memory
#' @description create a list of binaryLogic::binary elements
#' @export create_memory
#' @param num_words number of words
#' @param num_bits number of bits in a word - must be a multiple of 8
#' @return a list of binaryLogic::binary elements, initialized to 0

create_memory <- function(num_words = .words, num_bits = .bits) {
  bytes <- num_bits / binaryLogic::byte()
  zeroes <- array(data = 0, dim = num_words)
  memory <- binaryLogic::as.binary(zeroes, signed = TRUE, size = bytes)
  return(memory)
}

#' @export display_memory
display_memory <- function(memory) {
  address <- seq(0, length(memory) - 1)
  integer <- sapply(memory, decode_integer)
  fraction <- sapply(memory, decode_fraction)
  left_order <- .format_order(sapply(memory, .decode_left_order))
  left_address <- sapply(memory, .decode_left_address)
  right_order <- .format_order(sapply(memory, .decode_right_order))
  right_address <- sapply(memory, .decode_right_address)

  return(as.data.frame(cbind(
    address, integer, fraction, left_order, left_address, right_order, right_address)))
}
