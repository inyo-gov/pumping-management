On/Off Report Input Template
============================

input_template_2026.xlsx is the proposed canonical input workbook for the
Quarto on/off pumping management reports.

Design Goals
------------

- Use real Excel dates, not Julian/decimal-year dates.
- Keep staff-entered field data in simple, narrow tables.
- Move static report metadata out of the Quarto files.
- Keep linked well assignments in a table instead of hardcoded R vectors.
- Make annual updates easier to validate before rendering the site.

Required Sheets
---------------

site_metadata

Static report information for each monitoring/control site.

Required columns:

- site
- region
- report_page
- display_order
- display_name
- site_type
- photo_file
- dtw_label
- include_on_off
- notes

linked_wells

One row per site/well relationship.

Required columns:

- site
- well_id

awc_vwr

Available water content and vegetation water requirement data.

Required columns:

- site
- date
- awc
- vwr

dtw

Depth-to-water measurements.

Required columns:

- site
- date
- dtw

on_off_history

Historical on/off status observations.

Required columns:

- site
- date
- status
- status_code

current_status

Current on/off status and AWC threshold summary.

Required columns:

- site
- current_status
- awc_req_turnon

Date Policy
-----------

Use real dates in every date column. Do not enter or maintain Julian,
decimal-year, or SigmaPlot-era numeric date fields. If a numeric plotting date
is ever needed, generate it downstream in R.

Validation
----------

The helper functions in code/input_schema.R can validate this workbook:

source(here::here("code", "input_schema.R"))
validate_onoff_input_workbook(here::here("data", "input_template_2026.xlsx"))

The current report still reads the legacy workbook format. This template is a
proposed target for the next cleanup pass.
