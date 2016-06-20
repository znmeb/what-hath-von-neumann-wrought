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

#' @title Encode Integer
#' @name encode_integer
#' @description Convert an R numeric as an ILLIAC signed integer
#' @param value numeric the number to encode
#' @return the binaryLogic::binary equivalent. Note that any fractional part is
#' discarded and the bounds are silenty enforced
#' @export
encode_integer <- function(value) {

  # silently enforce bounds
  integer <- max(.minint, value)
  integer <- min(.maxint, integer)
  binaryLogic::as.binary(integer, signed = TRUE, size = .bytes)
}

#' @title Decode Integer
#' @name decode_integer
#' @description Convert an ILLIAC signed integer to an R numeric
#' @param binary a binaryLogic::binary ILLIAC word
#' @return the R numeric equivalent assuming the word is a signed integer
#' @export
decode_integer <- function(binary) {
  as.numeric(binary)
}

#' @title Encode Fraction
#' @name encode_fraction
#' @description Convert an R numeric to an ILLIAC signed fraction
#' @param value numeric the number to encode
#' @return the binaryLogic::binary equivalent. Note that the bounds are silenty enforced
#' @export
#' @export
encode_fraction <- function(value) {
  encode_integer(.multiplier * value)
}

#' @title Decode Fraction
#' @name decode_fraction
#' @description Convert an ILLIAC fraction to an R numeric
#' @param binary a binaryLogic::binary ILLIAC word
#' @return the R numeric equivalent assuming the word is a signed fraction
#' @export
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

#' @title Display State
#' @name display_state
#' @description This is the main mechanism for the emulator to present its result
#' to the user. It displays the accumulator and quotient registers, the program
#' counter and its contents, and the memory as a data frame. Each word is
#' shown as a decimal fraction, a decimal integer, and as an order pair.
#' @param a the accumulator register
#' @param q the quotient register
#' @param program_counter 2 * the actual program counter + 0 for left or 1 for right
#' @param memory the memory
#' @return a data frame with the state as decimal fractions, integers and order pairs
#' @examples
#' \dontrun{
#' williams <- create_memory()
#' williams[[1]] <- encode_fraction(-1)
#' williams[[2]] <- encode_fraction(1)
#' a <- williams[[1]]
#' q <- williams[[2]]
#' program_counter <- 3
#' state <- display_state(a, q, program_counter, williams)
#' View(state)}
#' @export
display_state <- function(a, q, program_counter, memory) {
  pc <- program_counter %/% 2
  pcd <- paste(pc, ifelse(program_counter %% 2 == 0, "L", "R"), sep = "")
  memx <- c(list(a), list(q), list(memory[[pc + 1]]), memory)
  address <- c(
    "a", "q", pcd, as.character(seq(0, length(memory) - 1)))
  integer <- sapply(memx, decode_integer)
  fraction <- sapply(memx, decode_fraction)
  left_order <- .format_order(sapply(memx, .decode_left_order))
  left_address <- sapply(memx, .decode_left_address)
  right_order <- .format_order(sapply(memx, .decode_right_order))
  right_address <- sapply(memx, .decode_right_address)
  result <- as.data.frame(cbind(
    address, integer, fraction, left_order, left_address, right_order, right_address))
  return(result)
}

#' @title Emulate
#' @name emulate
#' @description The emulator itself. The user calls it with an initial state.
#' It proceeds order by order until a stopping condition happens. At termination
#' the emulator returns the reason for stopping and the state at termination.
#' @param a the accumulator register
#' @param q the quotient register
#' @param program_counter 2 * the actual program counter + 0 for left or 1 for right
#' @param memory the memory
#' @return a list containing
#' \itemize{
#' \item display a data frame from "display_state" with the terminal state
#' \item a the accumulation
#' \item q the quotient
#' \item program_counter the coded program counter
#' \item memory the memory}
#' @export

emulate <- function(a, q, program_counter, memory) {
  # unpack arguments
  # fetch first order pair
  # main loop: while "running" do
  #     decode current instruction
  #     return stop code if it's a stop or hangup
  #     execute it if it's a transfer; next
  #     execute non-transfer; return stop code if it's a divide hangup
  #     increment program counter
  #     next:
  #
}
