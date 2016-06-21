# constants
.bits <- 40
.words = 1024
.ltype <- 1:4
.lvariant <- 5:8
.laddress <- 9:20
.rtype <- 21:24
.rvariant <- 25:28
.raddress <- 29:40
.bytes <- .bits / binaryLogic::byte()
.frac2int <- 2 ^ (.bits - 1)
.int2frac <- 2 ^ -(.bits - 1)
.minint <- -.frac2int
.maxint <- .frac2int - 1

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
#' @description We store ILLIAC words as integers in a double. So all we need to
#' do to encode an integer is enforce bound.
#' @param value numeric the number to encode
#' @return a double truncated to ILLIAC signed integer range
#' @export
encode_integer <- function(value) {

  # silently enforce bounds
  return(min(.maxint, max(.minint, round(value, 0))))
  }

#' @title Decode Integer
#' @name decode_integer
#' @description ILLIAC words are stored in the emulator as integers in a
#' double, since R integers are on 32 bits. As long as nothing creates a non-
#' integer we can just treat these as words.
#' @param word a double representing an ILLIAC word as an integer
#' @return the input word
#' @export
decode_integer <- function(word) {
 return(word)
}

#' @title Encode Fraction
#' @name encode_fraction
#' @description Convert an R numeric to an ILLIAC signed fraction
#' @param value numeric the number to encode
#' @return the fraction scaled to an integer and rounded
#' @export
#' @export
encode_fraction <- function(value) {
  encode_integer(round(.frac2int * value), 0)
}

#' @title Decode Fraction
#' @name decode_fraction
#' @description Convert an ILLIAC fraction to an R numeric
#' @param value a numeric representing a scaled fraction
#' @return the R numeric equivalent assuming the word is a signed fraction
#' @export
decode_fraction <- function(value) {
  return(.int2frac * value)
}

# internals for bit manipulation
.int2bits <- function(integer) {
  return(binaryLogic::as.binary(integer, signed = TRUE, size = .bytes))
}
.decode_order_pair <- function(integer) {
  bits <- .int2bits(integer)
  ltype <- as.numeric(bits[.ltype])
  lvariant <- as.numeric(bits[.lvariant])
  laddress <- as.numeric(bits[.laddress])
  rtype <- as.numeric(bits[.rtype])
  rvariant <- as.numeric(bits[.rvariant])
  raddress <- as.numeric(bits[.raddress])
}

.format_order <- function(type, variant) {
  type <- illiac_hex_encode(type)
  variant <- illiac_hex_encode(variant)
  return(paste(type, variant, sep = ""))
}

#' @title Create Memory
#' @name create_memory
#' @description create a vector of integers
#' @export create_memory
#' @param num_words number of words
#' @return a numeric vector initialized to 0

create_memory <- function(num_words = .words) {
  return(vector(mode = numeric, length = num_words))
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
  memx <- c(a, q, memory[[pc + 1]], memory)
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

#' @title SADOI
#' @name sadoi
#' @description A re-implementation of the ILLIAC Symbolic Address Decimal
#' Order Input (SADOI) assembler.
#' @param source_file path to a file with directives and orders in SADOI format
#' @return a list of
#' \itemize{
#' \item memory a "memory" object containing the assembled orders
#' \item start the program counter for the last start directive encountered}
#' @export

sadoi <- function(source_file) {

}
