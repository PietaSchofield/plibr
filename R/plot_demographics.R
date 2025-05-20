#' Plot demographics with optional layers
#'
#' Creates a histogram of age by sex, faceted by group. Optionally,
#' adds color or alpha layers for ethnicity and socioeconomic status (SES).
#'
#' @param data A data frame with one row per individual.
#' @param age_col Column name for age.
#' @param sex_col Column name for sex or gender.
#' @param group_col Column name for group (e.g., case/control).
#' @param ethnicity_col Optional. Column name for ethnicity.
#' @param ses_col Optional. Column name for socioeconomic status.
#'
#' @return A ggplot object.
#' @export
plot_demographics <- function(data,
                              age_col = "age",
                              sex_col = "gender",
                              group_col = "group",
                              ethnicity_col = NULL,
                              ses_col = NULL) {
  
  p <- ggplot(data, aes_string(x = sex_col, y=age_col, fill = sex_col)) +
    geom_boxplot(binwidth = 5, position = "identity", alpha = 0.6) +
    facet_wrap(as.formula(paste("~", group_col))) +
    labs(
      title = "Age distribution by Sex and Group",
      x = "Age",
      y = "Count",
      fill = "Sex"
    ) +
    theme_minimal()
  
  # Add ethnicity as outline color if provided
  if (!is.null(ethnicity_col) && ethnicity_col %in% names(data)) {
    p <- p + aes_string(color = ethnicity_col)
  }

  # Add SES as alpha level if provided
  if (!is.null(ses_col) && ses_col %in% names(data)) {
    p <- p + aes_string(alpha = ses_col)
  }
  
  return(p)
}

