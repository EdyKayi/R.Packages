#' @title Prime factorisation
#' In this work we take a number and give all the prime factors
#'
#' @param n
#'
#' @return n]
#' @export
#'
#' @examples prime_numbers(72)
#'
#'
# Define a function 'prime_numbers' that takes a single argument 'n'
prime_numbers <- function(n) {

  if(is.character(n)){
    n <- as.numeric(stringr::str_extract_all(n,"\\d+"))
  }

  # Check if 'n' is greater than or equal to 2
  if (n >= 2) {

    # Create a sequence from 2 to 'n'
    x = seq(2, n)

    # Initialize an empty vector to store prime numbers
    prime_nums = c()

    # Loop through each number in the sequence from 2 to 'n'
    for (i in seq(2, n)) {

      # Check if 'i' is in the sequence 'x'
      if (any(x == i)) {

        # Add 'i' to the 'prime_nums' vector
        prime_nums = c(prime_nums, i)

        # Remove multiples of 'i' from the sequence 'x'
        x = c(x[(x %% i) != 0], i)
      }
    }

    # Return the vector of prime numbers
    return(prime_nums)
  }
  else {
    # Stop the function execution and display an error message if 'n' is less than 2
    stop("Input number should be at least 2.")
  }
}



#' @title multiples of 2
#' This function take a number and and mutiply it by 2
#'
#' @param x
#'
#' @return x*2]
#' @export
#'
#' @examples times4(3)
#'
#'
# Define a function 'prime_numbers' that takes a single argument 'n'
times4 <- function(x){
  x*2
}
