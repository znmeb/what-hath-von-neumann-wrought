% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emulator.R
\name{display_state}
\alias{display_state}
\title{Display State}
\usage{
display_state(a, q, program_counter, memory)
}
\arguments{
\item{a}{the accumulator register}

\item{q}{the quotient register}

\item{program_counter}{2 * the actual program counter + 0 for left or 1 for right}

\item{memory}{the memory}
}
\value{
a data frame with the state as decimal fractions, integers and order pairs
}
\description{
This is the main mechanism for the emulator to present its result
to the user. It displays the accumulator and quotient registers, the program
counter and its contents, and the memory as a data frame. Each word is
shown as a decimal fraction, a decimal integer, and as an order pair.
}
\examples{
\dontrun{
williams <- create_memory()
williams[[1]] <- encode_fraction(-1)
williams[[2]] <- encode_fraction(1)
a <- williams[[1]]
q <- williams[[2]]
program_counter <- 3
state <- display_state(a, q, program_counter, williams)
View(state)}
}

