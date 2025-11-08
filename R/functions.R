#' pasteurize() — for cleaning data
#'
#' - Cleans and standardizes column names (lowercase, alphanumeric with underscores)
#' - Converts `"NULL"` strings and empty cells to `NA`
#' - Title-cases character columns
#' - Removes completely empty rows and duplicates
#' 
#' @param df A data frame to clean
#' @importFrom stringr str_to_title
#' @return A cleaned data frame
#' @examples 
#' pasteurize(data.frame(" First Name " = c("john", "JANE", "NULL")))
#' @export
pasteurize <- function(df) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame", call. = FALSE)
  }
  
  if (nrow(df) == 0) {
    return(df)
  }
  
  # 1. Clean column names
  names(df) <- tolower(names(df))
  names(df) <- gsub("[^a-z0-9_]", "_", names(df))
  names(df) <- gsub("_+", "_", trimws(names(df)))
  names(df) <- sub("^_|_$", "", names(df))
  
  # 2. Clean character columns
  char_cols <- vapply(df, is.character, logical(1))
  
  for (col in names(df)[char_cols]) {
    # Convert "NULL" strings or empty strings to NA
    df[[col]][toupper(df[[col]]) == "NULL"] <- NA
    df[[col]][nchar(trimws(df[[col]])) == 0] <- NA
    
    # Convert to proper case (title case)
    df[[col]] <- stringr::str_to_title(df[[col]])

    # Trim whitespace
    df[[col]] <- trimws(df[[col]], which = "both")
  }
  
  # 3. Remove completely empty rows
  non_empty_rows <- rowSums(!is.na(df)) > 0
  df <- df[non_empty_rows, , drop = FALSE]
  
  # 4. Remove duplicate rows
  if (nrow(df) > 0) {
    df <- df[!duplicated(df), , drop = FALSE]
  }
  
  return(df)
}

#' fix_date_ymd() — for converting YYYYMMDD date fields to YYYY-MM-DD
#' 
#' Converts date strings in `YYYYMMDD` format to ISO `YYYY-MM-DD` format
#' and parses them into `Date` objects.
#'
#' @param date A character vector or numeric vector containing dates in `YYYYMMDD` format.
#'
#' @importFrom stringr str_replace
#' @importFrom lubridate ymd
#'
#' @return A `Date` vector, with invalid values returned as `NA`.
#'
#' @examples
#' fix_date_ymd("20250319")
#'
#' @export
fix_date_ymd <- function(date) {
  date <- as.character(date)

  fixed_date <- ifelse(
    test = grepl("^\\d{8}$", date),
    yes = str_replace(date, 
                      pattern = "(\\d{4})(\\d{2})(\\d{2})", 
                      replacement = "\\1-\\2-\\3"), 
    no = NA_character_
  )
  
  fixed_date <- suppressWarnings(lubridate::ymd(fixed_date))
}

#' fix_date_mdy() — for converting MM/DD/YYYY date fields to YYYY-MM-DD
#' 
#' Converts date strings in `MM/DD/YYYY` format to ISO `YYYY-MM-DD` format.
#' Returns a character vector in the standardized form.
#'
#' @param date A character vector containing dates in `MM/DD/YYYY` format.
#' 
#' @importFrom lubridate mdy
#'
#' @return A character vector of reformatted dates. Invalid values are returned unchanged.
#'
#' @examples
#' fix_date_mdy("03/19/2025")
#'
#' @export
fix_date_mdy <- function(date) {
  parsed <- suppressWarnings(lubridate::mdy(date))
  
  ifelse(
    test = !is.na(parsed), 
    yes = format(parsed, "%Y-%m-%d"), 
    no = date  
  )
}

#' affiche() — for displaying an aesthetic table
#' 
#' Prints a formatted table with borders and color, similar to a newspaper style.
#' Useful for visually inspecting small data frames in the console.
#'
#' @param df A data frame or tibble to display.
#' @param align Character string specifying text alignment: `"left"`, `"center"`, or `"right"`.
#' @param na_color ANSI color escape code for highlighting `NA` values (default is red italics).
#' @param theme Character specifying the border style. Currently supports `"newspaper"`.
#'
#' @importFrom tibble rownames_to_column
#' 
#' @return Invisibly returns the input data frame.
#'
#' @examples
#' df <- head(mtcars, 5) 
#' affiche(df)
#'
#' @export
affiche <- function(df,
                    align = "left",
                    na_color = "\033[91;3m",
                    theme = "newspaper") {

  if (ncol(df) == 0 || nrow(df) == 0) { 
    msg <- "That table doesn't exist!"
    width <- nchar(msg)
    top <- paste0("╔", strrep("═", width + 2), "╗")
    mid <- paste0("║ ", msg, " ║")
    bot <- paste0("╚", strrep("═", width + 2), "╝")
    cat(top, "\n", mid, "\n", bot, "\n", sep = "")
    return(invisible(df))
  }

  if (!is.null(rownames(df)) && !any(names(df) == " ")) {
    df <- tibble::rownames_to_column(df, var = "row")
  }

  border <- switch(theme,
    "newspaper" = list(
      h = "═", v = "║",
      tl = "╔", tr = "╗",
      bl = "╚", br = "╝",
      jn = "╬",
      l = "╠", r = "╣",
      t = "╦", b = "╩"
    ),
    stop("Theme not supported. Try 'newspaper'")
  )

  reset <- "\033[0m"
  color_na <- function(x) paste0(na_color, x, reset)

  display_width <- function(s) {
    if (is.na(s)) return(2) 
    clean <- gsub("\033\\[[0-9;]*[mK]", "", as.character(s))
    nchar(clean, type = "width")
  }

  df_display <- as.data.frame(
    lapply(df, function(col) {
      ifelse(is.na(col), color_na("NA"), as.character(col))
    }),
    stringsAsFactors = FALSE
  )
  col_names <- names(df_display)

  # Create type abbreviations
  type_map <- c(
    "integer" = "int",
    "numeric" = "num",
    "character" = "str",
    "logical" = "bool",
    "factor" = "fct",
    "Date" = "date",
    "POSIXct" = "dttm",
    "POSIXlt" = "dttm"
  )
  
  # Get column types with abbreviations
  col_types <- sapply(df, function(col) {
    type <- class(col)[1]
    abbrev <- type_map[type]
    if (is.na(abbrev)) abbrev <- type
    tolower(abbrev)
  })
  
  # Apply italic formatting to types
  italic <- "\033[3m"
  col_types_display <- paste0(italic, col_types, reset)

  col_widths <- sapply(seq_along(col_names), function(i) {
    max(display_width(col_names[i]),
        display_width(col_types[i]),
        sapply(df_display[[i]], display_width),
        na.rm = TRUE)
  })

  draw_hline <- function(connector_left, connector_right, cross) {
    line <- paste0(
      connector_left,
      paste0(sapply(col_widths, function(w) {
        paste0(strrep(border$h, w + 2), cross)
      }), collapse = "")
    )
    gsub(paste0(cross, "$"), connector_right, line)
  }

  top_line <- draw_hline(border$tl, border$tr, border$t)
  mid_line <- draw_hline(border$l, border$r, border$jn)
  bot_line <- draw_hline(border$bl, border$br, border$b)

  header <- paste0(
    border$v,
    paste0(sapply(seq_along(col_names), function(i) {
      name <- col_names[i]
      width <- col_widths[i]
      pad_total <- width - display_width(name)
      pad_left <- switch(align,
        "left" = 0,
        "center" = floor(pad_total / 2),
        "right" = pad_total
      )
      paste0(" ",
             strrep(" ", pad_left),
             name,
             strrep(" ", pad_total - pad_left),
             " ", border$v)
    }), collapse = "")
  )

  type_row <- paste0(
    border$v,
    paste0(sapply(seq_along(col_types_display), function(i) {
      content <- col_types_display[i]
      width <- col_widths[i]
      pad_total <- width - display_width(col_types[i])
      pad_left <- switch(align,
        "left" = 0,
        "center" = floor(pad_total / 2),
        "right" = pad_total
      )
      paste0(" ",
             strrep(" ", pad_left),
             content,
             strrep(" ", pad_total - pad_left),
             " ", border$v)
    }), collapse = "")
  )

  data_rows <- sapply(1:nrow(df_display), function(i) {
    paste0(
      border$v,
      paste0(sapply(seq_along(col_names), function(j) {
        content <- df_display[i, j]
        width <- col_widths[j]
        pad_total <- width - display_width(content)
        pad_left <- switch(align,
          "left" = 0,
          "center" = floor(pad_total / 2),
          "right" = pad_total
        )
        paste0(" ",
               strrep(" ", pad_left),
               content,
               strrep(" ", pad_total - pad_left),
               ifelse(j == length(col_names), "", paste0(" ", border$v)))
      }), collapse = ""),
      " ", border$v 
    )
  })

  cat(top_line, "\n")
  cat(header, "\n")
  cat(type_row, "\n")
  cat(mid_line, "\n")
  cat(paste0(data_rows, collapse = "\n"), "\n")
  cat(bot_line, "\n")

  invisible(df)
}

#' describe() — for summarizing dataset statistics
#' 
#' Generates basic summary statistics (min, max, median, mean, standard deviation, and count)
#' for all numeric columns in a data frame.
#'
#' @param df A data frame containing numeric variables.
#' 
#' @importFrom dplyr select
#' @importFrom stats median sd
#'
#' @return A data frame of summary statistics, with each column representing
#' a numeric variable from the input.
#'
#' @examples
#' describe(mtcars)
#'
#' @export
describe <- function(df) {
  numeric_df <- df |> 
    select(where(is.numeric))
  
  summary_list <- list()
  
  for (col in names(numeric_df)) {
    summary_list[[col]] <- c(
      min = min(numeric_df[[col]], na.rm = TRUE),
      max = max(numeric_df[[col]], na.rm = TRUE),
      median = median(numeric_df[[col]], na.rm = TRUE),
      mean = mean(numeric_df[[col]], na.rm = TRUE),
      sd = round(sd(numeric_df[[col]], na.rm = TRUE), 2),
      n = sum(!is.na(numeric_df[[col]]))
    )
  }
  
  result <- as.data.frame(do.call(rbind, summary_list))
  
  result <- as.data.frame(t(result))
  
  colnames(result) <- names(numeric_df)
  
  return(result)
}

#' count_table() — for generating a count table
#' 
#' Produces a frequency table showing the counts and percentages
#' of values in a specified column of a data frame.
#'
#' @param df A data frame or tibble.
#' @param column An unquoted column name to count values of.
#' 
#' @importFrom dplyr group_by summarize arrange mutate across where n desc if_else
#' 
#' @return A tibble with columns: the grouping variable, `count`, and `percent`.
#'
#' @examples
#' mtcars |> count_table(mpg)
#'
#' @export
count_table <- function(df, column) {
  df |> 
    group_by({{ column }}) |> 
    summarize(count = n()) |>
    arrange(desc(count)) |>  
    mutate(percent = paste0(round(count / sum(count) * 100, 0), "%")) |> 
    mutate(percent = if_else(percent == "0%" & count >= 1, true = "<1%", false = percent))
}

#' count_na() — for generating a count table of NAs
#' 
#' Returns a table showing the number and percentage of `NA` values
#' for each column in a data frame.
#'
#' @param df A data frame or tibble.
#' 
#' @importFrom dplyr filter count add_row arrange mutate case_when pull n desc
#' @importFrom rlang sym
#' @importFrom tibble tibble
#'
#' @return A tibble with columns: `col`, `na_count`, and `na_percent`.
#'
#' @examples
#' mtcars |> count_na()
#'
#' @export
count_na <- function(df) {
  result <- tibble(col = character(), na_count = integer())

  for (col in names(df)) {
    na_count <- df |> filter(is.na(!!sym(col))) |> count() |> pull(n)
    result <- result |> add_row(col = col, na_count = na_count)
  }

  result |>
    mutate(
      na_percent = case_when(
        na_count == 0 ~ "0%",
        (na_count / nrow(df)) <= 0.0099 ~ "<1%",
        TRUE ~ paste0(round(na_count / nrow(df) * 100, 0), "%")
      )
    ) |>
    arrange(desc(na_count))
}