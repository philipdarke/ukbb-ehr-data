# Global constants and helper functions
# Author: Philip Darke <p.a.darke2@newcastle.ac.uk>

# File paths
source("file_paths.R")

# Data cleaning etc ------------------------------------------------------------

# Missing dates
# from https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=591 section 4.6
na_dates <- list(pre_dob = dmy("01/01/1901"),
                 dob = dmy("02/02/1902"),
                 yob = dmy("03/03/1903"),
                 future = dmy("07/07/2037"))

# Data collection period
record_start <- dmy("01/01/1985")  # assumed start of primary care EHR adoption
min_gap <- 1  # join periods with gaps less than min_gap year(s)

# Extract date range
# from https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=591 section 4.2
extract_date <- data.table(data_provider = 1:4,
                           extract_start = dmy(c("25/05/2017",
                                                 "19/04/2017",
                                                 "14/06/2016",
                                                 "18/09/2017")),
                           extract_end = dmy(c("14/06/2017",
                                               "04/05/2017",
                                               "16/06/2017",
                                               "27/09/2017")))

# Diabetes phenotyping ---------------------------------------------------------

# NICE diagnostic thresholds
nice_hba1c <- list(pre = 42, diabetes = 48)  # mmol/mol
nice_fpg <- list(pre = 5.5, diabetes = 7)  # mmol/l
nice_ogtt <- list(pre = 7.8, diabetes = 11.1)  # mmol/mol

# Other constants
prescription_duration <- days(90)  # assumed time covered by a prescription
gestational_duration <- months(9)  # assumed maximum length of gestational diabetes
remission_gap <- 6  # gap between sub-diabetic tests for remission (months)
diagnosis_lag <- months(6)  # assumed maximum period from diabetic test result to diagnosis

# Helper functions -------------------------------------------------------------

#' Identify gaps between registration periods
#'
#' @param dates Registration periods in data table with from/to dates in
#'   columns 1/2 respectively.
#'
#' @return Data table with gaps between periods and reg = FALSE indicator.
#' 
#' @export
#'
add_gaps <- function (dates) {
  out <- data.table(dates[-nrow(dates), 2] + 1, dates[-1, 1] - 1, FALSE)
  names(out) <- c("reg_date", "deduct_date", "reg")
  out
}

#' Combine UK Biobank EHR and visit data
#'
#' @description Join EHR and visit data for a specific observation/test. Only
#'   one observation is permitted on each date in the input data. The EHR value
#'   is dropped if a visit value is recorded on the same date. Inputs must have
#'   columns \code{eid}, \code{date}, \code{value}, \code{variable} and
#'   \code{source}.
#'
#' @param ukbb Data table with visit observations/test results.
#' @param ehr Data table with EHR observations/test results.
#'
#' @return Data table with combined results.
#' 
#' @export
#'
combine_data <- function (ukbb, ehr) {
  if (nrow(ehr) == 0) {
    return(ukbb[, .(eid, date, variable, source, value)])
  }
  # Check inputs
  ukbb_obs <- ukbb[, unique(variable)]
  ehr_obs <- ehr[, unique(variable)]
  if (ukbb_obs != ehr_obs) {
    stop("Check the data. Must provide the same observation/test in visit and EHR data.")
  }
  if (length(ukbb_obs) != 1 | length(ehr_obs) != 1) {
    stop("Check the data. Must only provide results for one observation/test.")
  }
  # Combine EHR and UK Biobank values
  ukbb <- ukbb[, .(eid, date, variable, source, value)]
  ehr <- ehr[, .(eid, date, variable, source, value)]
  combined <- rbind(ukbb, ehr)[order(eid, date)]
  # Drop EHR value if UK Biobank measurement taken on same date
  drop_values <- combined[, .N, by = c("eid", "date")
  ][N > 1, .(eid, date, drop = TRUE)]
  combined <- merge(combined, drop_values, by = c("eid", "date"), all = TRUE)
  combined <- combined[is.na(drop) | (drop == TRUE & source == "ukbb")]
  combined[, drop := NULL]
  combined[]
}

#' Convert between HbA1c units
#' 
#' @description See conversion factors here
#' \code{https://www.soc-bdr.org/content/rds/authors/unit_tables_conversions_and_genetic_dictionaries/e14718/index_en.html}.
#'
#' @param value Value to convert.
#' @param out_unit "ifcc" = convert from DCCT to IFCC units (default),
#'   "dctt" = convert from IFCC to DCTT units.
#'
#' @return Converted value.
#' 
#' @export
#'
conv_hba1c <- function (value, out_unit = "ifcc") {
  if (out_unit == "ifcc") {
    out <- value * 10.93 - 23.5  # DCCT/NGSP (%) to IFCC mmol/mol
  } else if (out_unit == "dcct") {
    out <- value * 0.0915 + 2.15  # IFCC mmol/mol to DCCT/NGSP (%)
  } else {
    stop("out_unit must be 'ifcc' or 'dcct'")
  }
  out[which(out < 0)] <- NA
  out
}

#' Get formatted diagnoses
#'
#' @param sr_data Self-reported data.
#' @param codes Codes for condition (see https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6).
#' @param variable Name for variable column.
#' @param level Optional name for level column (default NA).
#'
#' @return Formatted data table.
#' 
#' @export
#'
get_condition <- function (sr_data, codes, variable, level = NA) {
  sr_data[condition %in% codes,
          .(eid,
            date,
            source = "ukbb",
            variable = variable,
            level = level,
            reported)]
}

#' Identify periods of medication from prescription records
#'
#' @param data Data table with eid = participant identifier and
#'   issue_date = date of prescription.
#' @param period Assumed length of each prescription (days period object).
#' @param category Optional description of medication.
#'
#' @return Data table with periods of medication.
#' 
#' @export 
#'
presc_history <- function (data, period, category = NULL) {
  # Drop multiple prescriptions on same day
  data <- unique(data[, .(eid, issue_date)])[order(eid, issue_date)]
  # Determine continuous periods of medication
  periods <- data[, .(eid, from = issue_date, to = issue_date %m+% period - 1)]
  periods[, from_next := c(from[-1], NA_Date_), by = eid]
  periods[, period := cumsum(c(TRUE, (to < from_next)[-.N])), by = eid]
  # Return medication periods in a data table
  if (is.null(category)) {
    periods <- periods[,
                       .(from = head(from, 1), to = tail(to, 1)),
                       by = c("eid", "period")]
  } else {
    periods <- periods[,
                       .(from = head(from, 1), to = tail(to, 1), category = category),
                       by = c("eid", "period")]
  }
  periods[, -c("period")]
}

#' Calculate C, R squared and D for a survival risk score
#' 
#' R squared and D are based on https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.1621
#' Concordance calculation is not computationally efficient!
#'
#' @param score Model scores (0-1 inclusive).
#' @param time Time to event.
#' @param event Event indicator (TRUE/FALSE).
#'
#' @return Named list with performance metrics.
#' 
#' @export
#'
eval_scores <- function(score, time, event) {
  message("Calculating scores - long run time for large data sets...")
  # C-index
  X <- data.table(score, time, event)
  X[time > 10, c("time", "event") := .(10, FALSE)]
  pair <- 0
  concordant <- 0
  for (i in 1:(nrow(X) - 1)) {
    X[, c("score_i", "time_i", "event_i") := .SD[1, 1:3]]
    X <- X[-1]
    if (X[1, event_i]) {
      X[event == TRUE & time_i < time, concordant := score_i > score]
      X[event == TRUE & time < time_i, concordant := score > score_i]
      X[event == FALSE & time_i < time, concordant := score_i > score]
    } else {
      X[event == TRUE & time < time_i, concordant := score > score_i]
    }
    pair <- pair + X[!is.na(concordant), .N]
    concordant <- concordant + X[, sum(concordant, na.rm = TRUE)]
  }
  # R2 and D
  y2 <- Surv(time, event)
  qhat <- qnorm((frank(score) - 3/8) / (length(score) + 0.25))
  rfit <- coxph(y2 ~ qhat)
  beta <- unname(coef(rfit))
  # Results
  list(C = round(concordant / pair, 3),
       R2 = round(beta^2 / (pi^2/6 + beta^2), 3),
       D = round(beta * sqrt(8/pi), 3))
}

# Plotting functions -----------------------------------------------------------

#' Plot data collection/registration period and record summary
#'
#' Colour-blind safe palette based on https://personal.sron.nl/~pault/.
#'
#' @param period_data Data table with data collection/registration period(s) i.e.
#'   `data_period.rds` or `gp_reg_raw.rds`.
#' @param record_data `all_records.rds`
#' @param labels `ggplot2` `labs` object with plot title/subtitle etc.
#' @param x_limits Limits for x-axis i.e. vector with minimum and maximum dates.
#' @param y_title Title of y-axis.
#' @param x_axis Display x-axis? Optional, default `TRUE`.
#' @param legend Display legend? Optional, default `FALSE`.
#' @param top_margin Top margin of plot. Optional, default 5.
#' @param bottom_margin Bottom margin of plot. Optional, default 5.
#'
#' @return `ggplot` object.
#' 
#' @export
#'
data_plot <- function (
  period_data,
  record_data,
  labels,
  x_limits,
  y_title,
  x_axis = TRUE,
  legend = FALSE,
  top_margin = 5,
  bottom_margin = 5
) {
  # Constants
  col_palette <- c("#0077bb",  # blue
                   "#33bbee",  # cyan
                   "#009988",  # teal
                   "#ee7733",  # orange
                   "#cc3311",  # red
                   "#ee3377",  # magenta
                   "#dddddd",  # light grey
                   "#555555")  # off-black
  # Format legend and x-axis
  theme_legend <- if (legend) {
    theme(legend.position = "bottom",
          legend.margin = margin(t = 0, l = 1, r = 1, b = 0))
  } else {
    theme(legend.position = "none")
  }
  theme_x_axis <- if (x_axis) {
    NULL
  } else {
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  }
  # Return plot
  ggplot() +
    geom_rect(data = period_data,
              aes(xmin = from, xmax = to, fill = "Data collection"),
              ymin = -0.3, ymax = 3.3) +
    geom_linerange(data = record_data,
                   aes(x = date, y = 0, ymin = ymin, ymax = ymax, colour = type)) +
    scale_x_date(limits = x_limits) +
    scale_y_continuous(name = y_title, limits = c(-0.3, 3.3)) +
    scale_colour_manual(name = NULL,
                        limits = c("event", "test", "presc"),
                        labels = c("Diagnosis/event", "Test/observation", "Prescription"),
                        values = c(col_palette[1], col_palette[5], col_palette[4])) +
    scale_fill_manual(name = NULL,
                      limits = "Data collection",
                      values = col_palette[7]) +
    labels +
    theme_minimal() +
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(t = top_margin, l = 5, r = 5, b = bottom_margin)) +
    theme_x_axis +
    theme_legend
}

#' Plot registration periods and output from data collection algorithm
#' 
#' Must have access to following objects:
#'   `reg_data <- readRDS(paste0(output_path, "gp_reg_raw.rds"))`
#'   `data_period <- readRDS(paste0(output_path, "data_period.rds"))`
#'   `records <- readRDS(paste0(output_path, "all_records.rds"))`
#'
#' @param id UK Biobank id number.
#' @param save Optional file path to save plot.
#'
#' @return `ggplot` object.
#' 
#' @export
#'
algo_plot <- function (id, save = NULL) {
  # Constants
  provider_labels <- c("1 (England Vision)",
                       "2 (Scotland)",
                       "3 (England TPP)",
                       "4 (Wales)")
  # Participant data
  reg_data <- reg_data[eid == id]
  setnames(reg_data, c("reg_date", "deduct_date"), c("from", "to"))
  data_period <- data_period[eid == id]
  records <- records[eid == id]
  records[type == "event", c("ymin", "ymax") := .(2, 3)]
  records[type == "test", c("ymin", "ymax") := .(1, 2)]
  records[type == "presc", c("ymin", "ymax") := .(0, 1)]
  # Plot limits
  x_limits <- c(min(reg_data$from, data_period$from, records$date),
                max(reg_data$to, data_period$to, records$date))
  
  # Summary plot for each data provider
  plot_i <- c(sort(unique(reg_data$data_provider)), 0)
  all_plots <- lapply(plot_i, function (provider) {
    if (provider > 0) {
      labels <- if (provider == min(reg_data$data_provider)) {
        labs(title = paste("Participant summary: ID", id),
             subtitle = "Registration periods by provider")
      } else {
        NULL
      }
      y_title <- provider_labels[provider]
      top_margin <- ifelse(provider == min(reg_data$data_provider), 5, 0)
      x_axis <- provider == max(reg_data$data_provider)
      data_plot(reg_data[data_provider == provider],
                records[data_provider == provider],
                labels, x_limits, x_axis = x_axis, y_title,
                top_margin = top_margin, bottom_margin = 0)
    } else {
      labels <- labs(subtitle = "Inferred period of data collection under algorithm A1")
      data_plot(data_period, records, labels, x_limits, y_title = NULL,
                legend = TRUE, top_margin = 10)
    }
  })
  # Composite plot
  n_plots <- length(plot_i)
  rel_heights <- c(1.2, rep(1, n_plots - 2), 1.5)
  out <- plot_grid(plotlist = all_plots, ncol = 1, align = "hv", axis = "lr", rel_heights = rel_heights)
  if (!is.null(save)) {
    ggsave(save, height = 50 * n_plots, width = 210, unit = "mm")
  }
  out
}

#' Plot output of diabetes phenotyping algorithm
#'
#' Colour-blind safe palette based on https://personal.sron.nl/~pault/.
#' 
#' Must have access to following objects:
#'   `data_period <- readRDS(paste0(output_path, "data_period.rds"))`
#'   `phenotype <- readRDS(paste0(output_path, "diabetes_phenotype.rds"))`
#'   `biomarkers <- readRDS(paste0(output_path, "biomarkers.rds"))`
#'   `prescriptions <- readRDS(paste0(output_path, "prescriptions.rds"))`
#'   `records <- readRDS(paste0(output_path, "all_records.rds"))`
#'
#' @param id UK Biobank id number.
#' @param save Optional file path to save plot.
#'
#' @return `ggplot` object.
#' 
#' @export
#'
pheno_plot <- function (id, save = NULL) {
  # Constants
  drug_types <- c("insulin" = "Insulin",
                  "metformin" = "Metformin",
                  "non_insulin" = "Other anti-diabetic",
                  "anti_hypertensives" = "Anti-hypertensive",
                  "antipsychotics" = "Atypical anti-psychotic",
                  "statins" = "Statin",
                  "steroids" = "Steroid")
  col_palette <- c("#0077bb",  # blue
                   "#33bbee",  # cyan
                   "#009988",  # teal
                   "#ee7733",  # orange
                   "#cc3311",  # red
                   "#ee3377",  # magenta
                   "#dddddd",  # light grey
                   "#555555")  # off-black
  # Participant data
  data_period <- data_period[eid == id]
  biomarkers <- biomarkers[eid == id]
  phenotype <- phenotype[eid == id]
  prescriptions <- prescriptions[eid == id]
  prescriptions <- prescriptions[!(category == "diabetes" & type == "any")]
  records <- records[eid == id]
  records[type == "event", c("ymin", "ymax") := .(2, 3)]
  records[type == "test", c("ymin", "ymax") := .(1, 2)]
  records[type == "presc", c("ymin", "ymax") := .(0, 1)]
  # Plot limits
  x_limits <- c(min(data_period$from) - 365, max(data_period$to) + 365)
  # Plot theme
  theme_set(theme_minimal())
  theme_update(text = element_text(size = 14),
               plot.title = element_text(size = 12, face = "bold"),
               plot.subtitle = element_text(size = 12),
               axis.title.y = element_text(size = 12),
               panel.grid.minor.x = element_blank(),
               panel.grid.minor.y = element_blank(),
               legend.position = "bottom",
               legend.margin = margin(t = 0, l = 1, r = 1, b = 0),
               plot.margin = margin(t = 0, l = 5, r = 5, b = 0))
  # EHR data plot
  ehr_plot <- data_plot(data_period, records,
                        labs(title = paste("Participant summary: ID", id),
                             subtitle = "Inferred period of data collection under algorithm A1"),
                        x_limits, y_title = NULL, legend = TRUE, bottom_margin = 0)
  # Phenotyping plot
  state_limits <- c("gestational", "pre", "type2", "remission")
  state_labels <- c("Gestational", "Pre-diabetes", "Type 2 diabetes", "Remission")
  state_labels <- state_labels[state_limits %in% phenotype$state]
  state_limits <- state_limits[state_limits %in% phenotype$state]
  pheno_plot <- ggplot() +
    geom_rect(data = phenotype, aes(xmin = from, xmax = to, fill = state), min = -Inf, ymax = Inf, alpha = 0.35) +
    geom_point(data = biomarkers[variable == "hba1c"], aes(x = date, y = value), colour = col_palette[8]) +
    geom_line(data = biomarkers[variable == "hba1c"], aes(x = date, y = value), colour = col_palette[8]) +
    scale_x_date(name = NULL, limits = x_limits, date_labels = "%Y") +
    scale_y_continuous(name = "HbA1c (mmol/mol)") +
    scale_fill_manual(name = NULL,
                      limits = state_limits,
                      labels = state_labels,
                      values = c("gestational" = col_palette[1],
                                 "pre" = col_palette[4],
                                 "type2" = col_palette[5],
                                 "remission" = col_palette[3])) +
    labs(subtitle = "Diabetes phenotyping") +
    theme(plot.margin = margin(t = 5, l = 5, r = 5, b = 0))
  # Prescriptions plot
  prescriptions[, name := category]
  prescriptions[category == "diabetes", name := type]
  prescriptions[, name := revalue(name, drug_types, warn_missing = FALSE)]
  presc_categories <- prescriptions[, unique(name)]
  lapply(1:length(presc_categories), function (i) {
    prescriptions[name == presc_categories[i], c("ymin", "ymax") := .(i, i + 1)]
    NULL
  })
  drug_labels <- unique(prescriptions[, .(name, ymin)])
  drug_plot <- ggplot() +
    geom_rect(data = prescriptions, aes(xmin = from, xmax = to, ymin = ymin, ymax = ymax, fill = category)) +
    geom_text(data = drug_labels, aes(y = ymin + 0.5, label = name), x = x_limits[1], hjust = 0) +
    scale_x_date(name = NULL, limits = x_limits, date_labels = "%Y") +
    scale_y_continuous(name = NULL) +
    scale_fill_discrete(type = col_palette) +
    labs(subtitle = "Prescription history") +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  # Biomarker plots
  bmi_plot <- ggplot() +
    geom_point(data = biomarkers[variable == "bmi"], aes(x = date, y = value), colour = col_palette[8]) +
    geom_line(data = biomarkers[variable == "bmi"], aes(x = date, y = value), colour = col_palette[8]) +
    scale_x_date(name = NULL, limits = x_limits, date_labels = "%Y") +
    scale_y_continuous(name = bquote(BMI~(kg/m^2))) +
    labs(subtitle = "Biomarkers") +
    theme(plot.margin = margin(t = 10, l = 5, r = 5, b = 5))
  # Composite plot
  all_plots <- list(ehr_plot, pheno_plot, drug_plot, bmi_plot)
  out <- plot_grid(plotlist = all_plots, ncol = 1, align = "hv", axis = "lr", rel_heights = c(1.45, 1.25, 1, 1.1))
  if (!is.null(save)) {
    ggsave(save, plot = out, height = 200, width = 210, unit = "mm")
  }
  out
}
