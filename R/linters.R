#' Miscellaneous Linters
#'
#' Additional linters
#'
#' @param source_file returned by \code{\link[lintr]{get_source_expressions}}
#' @return An \code{lints} object, which is a list of \code{lint} objects.
#' @name linters
NULL

#' @describeIn linters check that all left parentheses in a function call
#'    do not have spaces before them.
#' @rdname linters
#' @export
function_left_parentheses_linter <- function(source_file) {
  one_of <- NULL
  space <- NULL
  lapply(ids_with_token(source_file, "'('"),
         function(id) {

           parsed <- source_file$parsed_content[id, ]

           family_ids <- family(source_file$parsed_content, parsed$id)

           types <- source_file$parsed_content[
             source_file$parsed_content$id %in% family_ids,
             "token"]

           is_function <- length(family_ids) %!=% 0L &&
             any(types %in% c("SYMBOL_FUNCTION_CALL", "FUNCTION"))

           if (is_function) {

             line <- source_file$lines[as.character(parsed$line1)]

             before_operator <- substr(line, parsed$col1 - 1L, parsed$col1 - 1L)

             space_before <- re_matches(before_operator, rex(space))

             if (space_before) {
               Lint(
                 filename = source_file$filename,
                 line_number = parsed$line1,
                 column_number = parsed$col1,
                 type = "style",
                 message = "Remove spaces before the left parenthesis in a function call.",
                 line = line,
                 linter = "function_left_parentheses"
               )
             }
           }

         })
}

