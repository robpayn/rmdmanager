# Dependencies for ROxygen ####

#' @importFrom R6 R6Class

# Class CommentManager ####

#' @export
#'
#' @title
#'   R markdown manager for automating annotated commenting on text
#'
#' @description
#'   An R6 class for the management of comments. Includes option for checking
#'   for pandoc compilation to Word docx files to use the Word feature
#'   of comments in margins.
#'
CommentManager = R6Class(
  classname = "CommentManager",
  public = list(

    commentCount = NULL,
    author = NULL,
    timeDefault = NULL,
    checkWord = NULL,

    initialize = function
    (
      commentCount = 0,
      author = "Anonymous",
      timeDefault = "1970-01-01T00:00:00Z",
      checkWord = TRUE
    )
    {
      self$commentCount = commentCount
      self$author = author
      self$timeDefault = timeDefault
      self$checkWord = checkWord
    },

    comment = function
    (
      text,
      comment,
      author = self$author,
      time = NULL,
      id = NULL,
      checkWord = self$checkWord
    )
    {
      self$commentCount <- self$commentCount + 1

      if (checkWord && isTRUE(knitr:::pandoc_to() == "docx")) {
        if (is.null(time))
        {
          time <- self$timeDefault
        }
        if (is.null(id)) {
          id = as.character(self$commentCount)
        }
        return(
          sprintf(
            '[%s]{.comment-start id="%s" author="%s" date="%s"}%s[]{.comment-end id="%s"}',
            comment,
            id,
            author,
            time,
            text,
            id
          )
        )
      } else {
        if (is.null(time)) {
          timePhrase <- ""
        } else {
          timePhrase <- sprintf(" at time %s", time)
        }
        if (is.null(id)) {
          idPhrase <- ""
        } else {
          idPhrase <- sprintf(" id %s", id)
        }
        return(
          sprintf(
            "*%s* **[Comment%s by %s%s: %s]**",
            text,
            idPhrase,
            author,
            timePhrase,
            comment
          )
        )
      }
    }

  )
)
