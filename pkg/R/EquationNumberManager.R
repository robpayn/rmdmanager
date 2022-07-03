# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class EquationNumberManager ####

#' @export
#'
#' @title
#'   R markdown manager for automating equation numbers
#'
#' @description
#'   An R6 class for the management of automated equation numbers,
#'   including a feature for cross-referencing previously defined equations
#'   by number.
#'
EquationNumberManager <- R6::R6Class(
  classname = "EquationNumberManager",
  public = list(

    namedeqs = NULL,
    counter = NULL,
    parens = NULL,

    initialize = function
    (
      counter = 0,
      parens = TRUE
    )
    {
      self$namedeqs <- integer()
      self$counter <- counter
      self$parens <- parens
    },

    addEq = function
    (
      name = NULL,
      counter = NULL,
      parens = self$parens
    )
    {
      if (!is.null(counter)) {
        self$counter <- counter
      }

      self$counter = self$counter + 1

      if (!is.null(name)) {
        self$namedeqs[name] <- self$counter
      }

      if (parens) {
        return(sprintf(
          "(%d)",
          self$counter
        ))
      } else {
        return(as.character(self$counter))
      }
    },

    getNum = function(name)
    {
      rval <- self$namedeqs[name]
      if (is.na(rval)) {
        return("**EquationNumberManager Error: Equation name not found**")
      } else {
        return(rval)
      }
    }

  )
)
