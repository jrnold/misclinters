#' @import lintr
NULL

LINTR_OBJECTS <-
  c("settings", "clear_settings", "read_settings", "flatten_lints",
    "reorder_lints", "ids_with_token", "family", "%!=%")
for (x in LINTR_OBJECTS) {
  assign(x, getFromNamespace(x, "lintr"))
}
