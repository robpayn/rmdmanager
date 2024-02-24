# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class TableManager ####

#' @export
#'
#' @title
#'   R markdown manager for automating table references
#'
#' @description
#'   An R6 class for the management of tables, including: table numbering,
#'   cross-references of table numbers, and exporting tables with captions
#'   to R markdown.
#'
TableManager <- R6Class(
  classname = "TableManager",
  public = list(

    ## Table attributes ####

    #' @field count
    #'   An integer counter for the total count of citable tables
    count = NULL,

    #' @field list
    #'   A list where each element is information about a citable table
    list = NULL,

    order = NULL,

    ## TableManager.initialize() ####

    initialize = function()
    {
      self$count <- 0;
      self$list <- vector(mode = "list", length = 0)
      self$order <- integer(length = 0)
    },

    ## TableManager.add() ####

    #' @description
    #'   Adds the information for a table to the citable list
    #'
    #' @param name
    #'   Character string with the unique name of the table
    #' @param caption
    #'   Character string with the table caption.
    #'   Markdown formatting is allowed.
    #' @param df
    #'   Character string with the data frame with table data
    #' @param fun
    #'   Optional function for generating the markdown for rendering the
    #'   figure and its caption.
    #'   Default value is NULL.
    #'
    #' @return
    #'   Invisibly returns the index of the table in the list of
    #'   citable figures
    #'
    add = function(name, caption, df, fun = NULL)
    {
      self$list[[name]] <- list(
        number = -1,
        caption = caption,
        df = df,
        fun = fun
      )
      invisible(length(self$list))
    },

    ## TableManager.cite() ####

    cite = function(name)
    {
      tabnum <- self$list[[name]]$number
      if (tabnum != -1) {
        return(tabnum)
      } else {
        self$count <- self$count + 1
        self$list[[name]]$number <- self$count
        self$order[self$count] <<- which(names(self$list) == name)
        return(self$count)
      }
    },

    ## TableManager.catMarkdownAll() ####

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
      tab <- self$list[[self$order[1]]]
      if (is.null(tab$fun)) {
        cat(
          sprintf(
            "%s Table %d\n\n",
            md.heading,
            tab$number
          )
        )
        cat(
          sprintf(
            "**Table %d.** %s\n\n",
            tab$number,
            tab$caption
          )
        )
        print(
          knitr::kable(
            tab$df[2:nrow(tab$df), ],
            row.names = FALSE,
            align = "l",
            col.names = unlist(
              tab$df[1, ]
            )
          )
        )
      } else {
        tab$fun(tab)
      }
      for (index in 2:length(self$order)) {
        cat(
          paste0(
            "\n",
            if (newpage) "\\newpage"
          )
        )
        tab <- self$list[[self$order[index]]]
        if (is.null(tab$fun)) {
          cat(
            sprintf(
              "%s Table %d\n\n",
              md.heading,
              tab$number
            )
          )
          cat(
            sprintf(
              "**Table %d.** %s\n\n",
              tab$number,
              tab$caption
            )
          )
          print(
            knitr::kable(
              tab$df[2:nrow(tab$df), ],
              row.names = FALSE,
              align = "l",
              col.names = unlist(
                tab$df[1, ]
              )
            )
          )
        } else {
          tab$fun(tab)
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

    ## TableManager.catCaptionsAll() ####

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
      for (index in 1:length(self$order)) {
        tab <- self$list[[self$order[index]]]
        cat(
          sprintf(
            "**Table %d.** %s\n\n",
            tab$number,
            tab$caption
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
