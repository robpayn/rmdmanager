# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class SectionNumberManager ####

#' @export
#'
#' @title
#'   R markdown manager for automating hierarchical section numbers
#'
#' @description
#'   An R6 class for the management of automated hierarchical section numbers.
#'
SectionNumberManager <- R6::R6Class(
  classname = "SectionNumberManager",
  public = list(

    counters = NULL,

    initialize = function
    (
      counters = 0
    )
    {
      self$counters <- counters
    },

    addSection = function
    (
      level,
      reset = 0,
      counters = NULL
    )
    {
      if (!is.null(counters)) {
        self$counters <- counters
      }

      if (level <= length(self$counters)) {
        self$counters[level] <- self$counters[level] + 1
        self$counters[level + 1] = reset
        if(length(self$counters) > level + 1) {
          self$counters <- self$counters[1:(level + 1)]
        }
      } else if (level > length(self$counters)) {
        stop(sprintf(
          "Heading level %d is more than one level deeper than previous heading level %d.",
          level,
          length(self$counters) - 1
        ))
      }

      return(paste0(
        self$getSectionNumber(),
        "."
      ))
    },

    getSectionNumber = function()
    {
      if(length(self$counters) > 1) {
        return(
          paste(
            as.character(self$counters[1:(length(self$counters) - 1)]),
            collapse = "."
          )
        )
      } else {
        return(NULL)
      }
    }

  )
)
