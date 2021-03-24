# loadData <- function(fields,
#                      table,
#                      WhereCls = '') {
#   # Construct the fetching query, pool cannot use dbSendQuery
#   if (WhereCls == '') {
#     dataDB <- pool %>% dbplyr::tbl(table) %>% dplyr::select(dplyr::all_of(fields))
#   }
#   else {
#     # dataDB <- pool %>% tbl(table) %>% filter(Org_Name == WhereCls)
#   }
# }



#' Save data to SQL table depending
#' @param data all input data to save
#' @param table table
#'
#' @noRd
save_data_in_roast_profiles <-
  function(data, table) { # Data is matched via ID_Key
    # print(get_where_clause())
    if (length(data != 0)) { # verify there is data to be changed
      if (.env$input[["primary_key"]] == 0 |
          .env$input[["primary_key"]] == '') {   # If no primary key, insert record instead
        sql <- paste0(
          "INSERT INTO ?table (",
          paste0(names(data), collapse = ", "),
          ") VALUES (",
          paste0("?", names(data), collapse = ", "),
          ")"
        )
        query <- pool::sqlInterpolate(pool, sql, .dots = c(list(table = table),
                                                     data))
        # print(query)
        record_status <- "inserted."
      } else {
        # Construct the update query
        sql <- paste0(
          "UPDATE ?table SET ",
          paste0(names(data), " = ?", names(data), collapse = ", "),
          " WHERE ",
          "primary_key",  # what to match with ?>?
          " = ?idVal;"
        )
        query <- pool::sqlInterpolate(pool, sql, .dots = c(
          list(table = table),
          as.list(data),
          list(idVal = .env$input[["primary_key"]])
        ))
        record_status <- "updated."
      }
      # Submit query and disconnect
      pool::dbExecute(pool, query)
    } else {
      record_status <- "not updated." # if no data?
    }
    record_status # Return if either inserted or updated
  }

#' Upload data
#'
#' @param data data selected to upload
#' @param table table to insert data into
#'
#' @noRd
upload_roast_profiles <-
  function(data, table) {

    if (length(data != 0)) { # verify there is data to be changed
      sql <- paste0(
        "INSERT INTO ?table (",
        paste0(names(data), collapse = ", "),
        ") VALUES (",
        paste0("?", names(data), collapse = ", "),
        ")"
      )
      query <- pool::sqlInterpolate(pool, sql, .dots = c(list(table = table),
                                                   data))
      # Submit query and disconnect
      pool::dbExecute(pool, query)
    }
    record_status <- "inserted."
  }
