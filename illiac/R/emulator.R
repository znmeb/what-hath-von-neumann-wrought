#' @title Order Code Table
#' @name order_code_table
#' @description create and return empty order code table
#' @export order_code_table
#' @return an order code table - a matrix with the type and variant digits as
#' row and column names
#'
#' @examples
#' octable <- order_code_table()

order_code_table <- function() {
  result <- matrix(nrow = 16, ncol = 16)
  rownames(result) <- paste("T", hex_digits(), sep = "")
  colnames(result) <- paste("V", hex_digits(), sep = "")
  return(result)
}

#' @title Hexadecimal Digits
#' @name hex_digits
#' @description Most ILLIAC docs use the term "sexadecimal" for base 16.
#' Moreover, the digits greater than 9 aren't the ones we normally use, but
#' "K", "S", "N", "J", "F", and "L". which had the right bit patterns on the
#' five-hole paper tape ILLIAC used.
#' @export hex_digits
#' @return a character vector containing the ILLIAC hexadecimal digits
#'
#' @examples
#' digits <- hex_digits()

hex_digits <- function() {
  return(c("0", "1", "2", "3", "4", "5", "6", "7",
           "8", "9", "K", "S", "N", "J", "F", "L"))
}

#' @title ILLIAC Word Length
#' @name illiac_word_length
#' @description Number of bits in an ILLIAC word - 40
#' @export illiac_word_length
#' @return the length of an ILLIAC word (40 bits)
#'
#' @examples
#' N <- illiac_word_length()

illiac_word_length <- function() {
  return(40)
}

#' @title ILLIAC word
#' @name illiac_word
#' @description ILLIAC used a 40-bit word. There are numerous ways we could deal
#' with this, but since ILLIAC's "native" arithmetic is based on two's-complement
#' fractions, there's nothing native to R that deals with it easily.
#'
#' So we might as well be pedagocial and just use a 40-element vector of
#' individual bits. And as a bonus we can name them and use matrix and vector
#' operations on them.
#' @export illiac_word
#' @return a 40-element vector with named elements
#'
#' @examples
#' accumulator <- illiac_word()
#' quotient <- illiac_word()

illiac_word <- function() {
  N <- illiac_word_length()
  result <- vector(mode = "logical", length = N)
  names(result) <- paste("w", seq(0, N - 1), sep = "")
  return(result)
}

#' @title ILLIAC AQ Doubleword
#' @name illiac_aq
#' @description The ILLIAC arithmetic unit had two main 40-bit registers, A (for
#' accumulator) and Q (for quotient). So we create an 80-element vector.
#' @export illiac_aq
#' @return an 80-element vector with named elements
#'
#' @examples
#' aq <- illiac_aq()

illiac_aq <- function() {
  N <- illiac_word_length()
  result <- vector(mode = "logical", length = N * 2)
  names(result)[1:N] <- paste("a", seq(0, N - 1), sep = "")
  names(result)[(N + 1):(2 * N)] <- paste("q", seq(0, N - 1), sep = "")
  return(result)
}

#' @title ILLIAC AQ Right Shift one place
#' @name illiac_aq_rshift
#' @description An ILLIAC right shift
#' @param illiac_aq the number (sign and 78 fraction bits) to be shifted
#' @export illiac_aq_rshift
#' @return the one-place-right-shift of illiac_aq
#'
#' @examples
#' aq <- illiac_aq()
#' shift <- illiac_aq_rshift(aq)

illiac_aq_rshift <- function(illiac_aq) {
  N <- illiac_word_length()
  result <- illiac_aq
  result[(N + 3):(2 * N)] <- result[(N + 2):(2 * N - 1)]
  result[N + 2] <- result[N]
  result[2:N] <- result[1:(N - 1)]
  return(result)
}

#' @title ILLIAC AQ Left Shift one place
#' @name illiac_aq_lshift
#' @description An ILLIAC left shift
#' @param illiac_aq the number (sign and 78 fraction bits) to be shifted
#' @export illiac_aq_lshift
#' @return the one-place-left-shift of illiac_aq
#'
#' @examples
#' aq <- illiac_aq()
#' shift <- illiac_aq_lshift(aq)

illiac_aq_lshift <- function(illiac_aq) {
  N <- illiac_word_length()
  result <- illiac_aq
  result[1:(N - 1)] <- result[2:N]
  result[N] <- result[N + 2]
  result[(N + 2):(2 * N - 1)] <- result[(N + 3):(2 * N)]
  result[(2 * N)] <- FALSE
  return(result)
}

#' @title ILLIAC Add
#' @name illiac_add
#' @description ILLIAC addition is a straightforward binary addition of
#' two's-complement numbers.
#' @export illiac_add
#' @param augend the number to be augmented
#' @param addend the number to be added to the augend
#' @return the sum of the augend and the addend

illiac_add <- function(augend, addend) {
}

#' @title ILLIAC Start
#' @name illiac_start
#' @description The emulator itself -given an address, left/right start position
#' and memory state, emulates the machine until it stops. Yes, if you give it an
#' unending loop that's what it will do, as would the system it's emulating.
#'
#' When it stops, you get a list with what you need to start it again and the
#' contents of memory when it stopped. You also get a decent stop message.
#' @export illiac_start
#' @param program_counter the address at which to start
#' @param right_hand FALSE: left-hand order, TRUE: right-hand order
#' @param aq the contents of the AQ register
#' @param memory the state of the memory at start time
#' @return a list containing
#' \itemize{
#' \item program_counter the address to restart
#' \item right_hand FALSE: restart at left-hand order,
#' TRUE: restart at right-hand order
#' \item aq the contents of the AQ register at stop time
#' \item memory the state of the memory when it stopped
#' \item reason for stop (character)}

illiac_start <- function(program_counter, right_hand, aq, memory) {
  N <- illiac_word_length()
  pc <- program_counter
  rh <- right_hand
  AQ <- aq
  RAM <- memory
  running <- TRUE
  while (running) {
    inst_reg <- RAM(pc - 1)
    if (rh) {
      order <- inst_reg * order_shift
    } else {
      order <- inst_reg & lhmask
    }
  opcode <- (order / op_shift) & op_mask
  address <- order & address_mask
  step_list <- illiac_execute()
  }
}

#' @title Create Memory
#' @name create_memory
#' @description create a boolean matrix representing a computer memory. The rows
#' are words (addressable entities) and the columns are bits.
#' @export create_memory
#' @param num_words number of rows (words, bytes, etc.)
#' @param num_bits number of bits in a word - must be a multiple of 8
#' @return a num_words by num_bits array filled with FALSE

create_memory <- function(num_words, num_bits) {
  bytes <- num_bits / 8
  zeroes <- array(data = 0, dim = num_words)
  memory <- binaryLogic::as.binary(zeroes, signed = TRUE, size = bytes)
  return(memory)
}

.ias_to_integer <- function(binary) {
  as.numeric(binary)
}

.ias_to_fraction <- function(binary) {
  divisor <- 2 ^ (length(binary) - 1)
  as.numeric(binary) / divisor
}

.store <- function(memory, value, address) {
  memory[[address - 1]] <- value
  return(memory)
}

.load <- function(memory, address) {
  value <- memory[[address - 1]]
  return(value)
}

# constants
.bits <- 40
.bytes <- .bits / 8
.multiplier <- 2 ^ (.bits - 1)
.minint <- -.multiplier
.maxint <- .multiplier - 1
.minfrac <- -1
.maxfrac <- 1 - 1 / .multiplier

# low-level functions
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
