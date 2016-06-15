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