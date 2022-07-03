# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class FigureManager ####

#' @export
#'
#' @title
#'   R markdown manager for automating figure references
#'
#' @description
#'   An R6 class for the management of figures, including: figure numbering,
#'   cross-references of figure numbers, and exporting figures with captions
#'   to R markdown.
#'
FigureManager <- R6Class(
  classname = "FigureManager",
  public = list(

    ## FigureManager attributes ####

    #' @field figCount
    #'   An integer counter for the total count of citable figures
    figCount = NULL,

    #' @field figList
    #'   A list where each element is information about a citable figure
    figList = NULL,

    figOrder = NULL,

    ## FigureManager.initialize() ####

    initialize = function()
    {
      self$figCount <- 0;
      self$figList <- vector(mode = "list", length = 0)
      self$figOrder <- integer(length = 0)
    },

    ## FigureManager.addFig() ####

    #' @description
    #'   Adds the information for a figure to the citable list
    #'
    #' @param name
    #'   Character string with the unique name of the figure
    #' @param caption
    #'   Character string with the figure caption.
    #'   Markdown formatting is allowed.
    #' @param path
    #'   Character string with the path to the image file for the figure.
    #' @param fun
    #'   Optional function for generating the markdown for rendering the
    #'   figure and its caption.
    #'   Default value is NULL.
    #'
    #' @return
    #'   Invisibly returns the index of the figure in the list of
    #'   citable figures
    #'
    addFig = function(name, caption, path, fun = NULL)
    {
      self$figList[[name]] <- list(
        number = -1,
        caption = caption,
        path = path,
        fun = fun
      )
      invisible(length(self$figList))
    },

    ## FigureManager.citeFig() ####

    citeFig = function(name)
    {
      fignum <- self$figList[[name]]$number
      if (fignum != -1) {
        return(fignum)
      } else {
        self$figCount <- self$figCount + 1
        self$figList[[name]]$number <- self$figCount
        self$figOrder[self$figCount] <<- which(names(self$figList) == name)
        return(self$figCount)
      }
    },

    ## FigureManager.catMarkdownAll() ####

    catMarkdownAll = function(
    newpage = TRUE,
    as.string = FALSE,
    type = c("output", "message"),
    split = FALSE,
    md.heading = "##"
    )
    {
      if (as.string) {
        file <- textConnection("rval", "w", local = TRUE)
        sink(file, type = type, split = split)
        on.exit({
          sink(type = type, split = split)
          close(file)
        })
      }
      fig <- self$figList[[self$figOrder[1]]]
      if (is.null(fig$fun)) {
        cat(
          sprintf(
            "%s Figure %d\n\n",
            md.heading,
            fig$number
          )
        )
        cat(
          sprintf(
            "![**Figure %d.** %s](%s)\n",
            fig$number,
            fig$caption,
            fig$path
          )
        )
      } else {
        fig$fun(fig)
      }
      for (index in 2:length(self$figOrder)) {
        cat(
          paste0(
            "\n",
            if (newpage) "\\newpage"
          )
        )
        fig <- self$figList[[self$figOrder[index]]]
        if (is.null(fig$fun)) {
          cat(
            sprintf(
              "%s Figure %d\n\n",
              md.heading,
              fig$number
            )
          )
          cat(
            sprintf(
              "![**Figure %d.** %s](%s)\n",
              fig$number,
              fig$caption,
              fig$path
            )
          )
        } else {
          fig$fun(fig)
        }
      }
      if (as.string) {
        on.exit()
        sink(type = type, split = split)
        close(file)
        return(
          paste0(rval, collapse = "\n")
        )
      } else {
        invisible(NULL)
      }
    },

    ## FigureManager.catCaptionsAll() ####

    catCaptionsAll = function
    (
      as.string = FALSE,
      type = c("output", "message"),
      split = FALSE
    )
    {
      if (as.string) {
        file <- textConnection("rval", "w", local = TRUE)
        sink(file, type = type, split = split)
        on.exit({
          sink(type = type, split = split)
          close(file)
        })
      }
      for (index in 1:length(self$figOrder)) {
        fig <- self$figList[[self$figOrder[index]]]
        cat(
          sprintf(
            "**Figure %d.** %s\n\n",
            fig$number,
            fig$caption
          )
        )
      }
      if (as.string) {
        on.exit()
        sink(type = type, split = split)
        close(file)
        return(
          paste0(rval, collapse = "\n")
        )
      } else {
        invisible(NULL)
      }
    }
  )
)
