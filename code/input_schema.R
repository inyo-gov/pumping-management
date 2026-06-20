# Canonical input schema for the on/off pumping management reports.
#
# These helpers define the workbook structure the Quarto reports should
# eventually read from directly. The goal is to keep staff-entered data simple:
# use real dates, stable lowercase column names, and put site metadata in tables
# instead of hardcoding it in the report body.

required_onoff_input_schema <- function() {
  list(
    site_metadata = c(
      "site", "region", "report_page", "display_order", "display_name",
      "site_type", "photo_file", "dtw_label", "include_on_off", "notes"
    ),
    linked_wells = c("site", "well_id"),
    awc_vwr = c("site", "date", "awc", "vwr"),
    dtw = c("site", "date", "dtw"),
    on_off_history = c("site", "date", "status", "status_code"),
    current_status = c("site", "current_status", "awc_req_turnon")
  )
}

read_sheet_header <- function(path, sheet) {
  names(readxl::read_excel(path, sheet = sheet, n_max = 0))
}

validate_onoff_input_workbook <- function(path) {
  schema <- required_onoff_input_schema()
  sheets <- readxl::excel_sheets(path)
  problems <- character()

  missing_sheets <- setdiff(names(schema), sheets)
  if (length(missing_sheets) > 0) {
    problems <- c(
      problems,
      paste("Missing required sheet(s):", paste(missing_sheets, collapse = ", "))
    )
  }

  for (sheet in intersect(names(schema), sheets)) {
    actual <- read_sheet_header(path, sheet)
    required <- schema[[sheet]]
    missing_cols <- setdiff(required, actual)

    if (length(missing_cols) > 0) {
      problems <- c(
        problems,
        paste0(
          "Sheet '", sheet, "' is missing required column(s): ",
          paste(missing_cols, collapse = ", ")
        )
      )
    }

    legacy_cols <- intersect(actual, c("Date_Julian", "date_real", "Date", "Site"))
    if (length(legacy_cols) > 0) {
      problems <- c(
        problems,
        paste0(
          "Sheet '", sheet, "' includes legacy/non-canonical column name(s): ",
          paste(legacy_cols, collapse = ", "),
          ". Use lowercase canonical names and real dates."
        )
      )
    }
  }

  if (length(problems) > 0) {
    stop(paste(problems, collapse = "\n"), call. = FALSE)
  }

  invisible(TRUE)
}

read_onoff_input_workbook <- function(path, validate = TRUE) {
  if (validate) {
    validate_onoff_input_workbook(path)
  }

  list(
    site_metadata = readxl::read_excel(path, sheet = "site_metadata"),
    linked_wells = readxl::read_excel(path, sheet = "linked_wells"),
    awc_vwr = readxl::read_excel(path, sheet = "awc_vwr"),
    dtw = readxl::read_excel(path, sheet = "dtw"),
    on_off_history = readxl::read_excel(path, sheet = "on_off_history"),
    current_status = readxl::read_excel(path, sheet = "current_status")
  )
}

as_report_inputs <- function(input) {
  list(
    awc1 = input$awc_vwr %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(!is.na(awc), !is.na(vwr), !is.na(date)) %>%
      dplyr::select(site, date, awc, vwr),

    dtw1 = input$dtw %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(!is.na(dtw), !is.na(site), !is.na(date)) %>%
      dplyr::select(site, date, dtw),

    on.off1 = input$on_off_history %>%
      dplyr::mutate(
        date = as.Date(date),
        on.off = status,
        on.off.1 = status_code
      ) %>%
      dplyr::select(site, date, on.off, on.off.1),

    awc.req.turnon = input$current_status %>%
      dplyr::rename(
        current.status = current_status,
        AWC.req.turnon = awc_req_turnon
      ),

    linked_wells = input$linked_wells %>%
      dplyr::rename(Site = site, Linked_Well = well_id),

    site_metadata = input$site_metadata
  )
}
