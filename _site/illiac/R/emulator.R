# constants
.bits <- 40
.words <- 1024
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
.address_divisor <- 2 ^ 12
.order_divisor <- 2 ^ 8
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
  encode_integer(round(.frac2int * value, 0))
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
  return(array(0, dim = num_words))
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

#' @title Interpret as ILLIAC
#' @name interpret_as_illiac
#' @description Interpret a vector of ILLIAC words in doubles as fractions and order pairs
#' @param illiac_words a vector of ILLIAC words, stored as integers in doubles
#' @return a list of
#' \itemize{
#' \item fraction double - the input vector scaled to print as fractions
#' \item left_order integer the left order
#' \item left_address integer the left 12-bit address
#' \item right_order integer the right order
#' \item right_address integer the left 12-bit address}
#' @export

interpret_as_illiac <- function(illiac_words) {
  fraction <- illiac_words * .int2frac
  right_address <- illiac_words %% .address_divisor
  work <- illiac_words %/% .address_divisor
  right_order <- work %% .order_divisor
  work <- work %/% .order_divisor
  left_address <- work %% .address_divisor
  work <- work %/% .address_divisor
  left_order <- work %% .order_divisor
  return(list(
    fraction = fraction,
    left_order = left_order,
    left_address = left_address,
    right_order = right_order,
    right_address = right_address))
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

  integer <- memx
  w <- interpret_as_illiac(memx)
  fraction <- w$fraction
  left_order <- .hex_order(w$left_order)
  left_address <- w$left_address
  right_order <- .hex_order(w$right_order)
  right_address <- w$right_address
  return(as.data.frame(cbind(
    address, integer, fraction, left_order, left_address, right_order, right_address)))
}

.hex_order <- function(order) {
  type <- illiac_hex_encode(order %/% 16)
  variant <- illiac_hex_encode(order %% 16)
  return(paste(type, variant, sep = ""))
}

#' @title ILLIAC multiply
#' @name illiac_multiply
#' @description Emulate an ILLIAC multiply
#' @param a binaryLogic::binary the accumulator register
#' @param q binaryLogic::binary the quotient register (multiplier)
#' @param binaryLogic::binary r3 the r3 register (multiplicand)
#' @return a list with a and q = 78-bit product
#' @examples
#' \dontrun{
#' r3 <- as.binary(encode_fraction(1/2), size = 5, signed = TRUE)
#' print(r3)
#' q <- r3
#' print(q)
#' a <- as.binary(encode_fraction(0), size = 5, signed = TRUE)
#' print(a)
#' product <- illiac_multiply(a, q, r3)
#' print(product$a)
#' print(product$q)}
#' @export
illiac_multiply <- function(a, q, r3) {
  for (i in 1:(.bits - 1)) {
    if (q[.bits]) {
      a <- a + r3
    }
    q[3:.bits] <- q[2:(.bits - 1)]
    q[2] <- a[.bits]
    a[2:.bits] <- a[1:(.bits - 1)]
  }
  return(list(a = a, q = q))
}

#' @title ILLIAC divide
#' @name illiac_divide
#' @description Emulate an ILLIAC divide
#' @param a binaryLogic::binary the accumulator register (upper dividend)
#' @param q binaryLogic::binary the quotient register (lower dividend)
#' @param binaryLogic::binary r3 the r3 register (divisor)
#' @return a list with a and q = residue and quotient
#' @examples
#' \dontrun{
#' r3 <- as.binary(encode_fraction(1/2), size = 5, signed = TRUE)
#' print(r3)
#' q <- r3
#' print(q)
#' a <- as.binary(encode_fraction(0), size = 5, signed = TRUE)
#' print(a)
#' product <- illiac_multiply(a, q, r3)
#' print(product$a)
#' print(product$q)
#' division <- illiac_divide(a, q, r3)
#' print(division$a)
#' print(division$q)}
#' @export
illiac_divide <- function(a, q, r3) {
  ax <- a
  qx <- q
  for (i in 1:(.bits - 1)) {
    diff <- ax - r3
    if (diff >= 0) {
      ax <- diff
      qx[.bits] <- 1
    }
    ax[1:(.bits - 1)] <- ax[2:.bits]
    ax[.bits] <- qx[2]
    qx[1] <- qx[2]
    qx[2:(.bits - 1)] <- qx[3:.bits]
    qx[.bits] <- 0
  }
  qx[.bits] <- 1
  return(list(a = ax, q = qx))
}
