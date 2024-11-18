#' Function to calculate CDR Jobs
#'
#' This function estimates the number of jobs created by CDR technologies based on a given GCAM database.
#'
#' @param db_path Path to the GCAM database.
#' @param db_name Name of the GCAM database.
#' @param dat_file Name of the .dat file to load.
#' @param scenario_list List of scenarios to include.
#' @param region_list List of regions to filter, default is all regions.
#' @param output_path Path to save output files.
#' @param output_type Output format, either "csv" or "list".
#' @param create_plots Logical, if TRUE, generates plots for the results.
#' @param job_metric The metric to visualize in plots. One of "mean_Jobs", "min_Jobs", "max_Jobs".
#' @param ncol Number of columns for the facets in the plots.
#' @param nrow Number of rows for the facets in the plots.
#' @param selected_years For "total_year", a vector of years to include, or NULL for all.
#' @return A list containing the estimated jobs.
#' @import dplyr rgcam ggplot2
#' @export
calculate_cdr_jobs <- function(db_path,
                               db_name,
                               dat_file,
                               scenario_list,
                               region_list = NULL,
                               output_path,
                               output_type = c("csv", "list"),
                               create_plots = TRUE,
                               job_metric = "mean_Jobs",
                               ncol = 2,
                               nrow = NULL,
                               selected_years = NULL) {

  # Validate output_type
  output_type <- match.arg(output_type, several.ok = TRUE)

  # Validate output path
  if (!dir.exists(output_path)) {
    stop("The specified output_path does not exist.")
  }

  # Define the CDR query directly in the function
  CDR_query <- "<?xml version='1.0'?>
  <queries>
    <aQuery>
      <all-regions/>
      <supplyDemandQuery title='CDR by tech'>
        <axis1 name='technology'>technology</axis1>
        <axis2 name='Year'>physical-output[@vintage]</axis2>
        <xPath buildList='true' dataName='output' group='false' sumAll='false'>
          *[@type='sector' and @name='CDR_regional']/*[@type='subsector']/*[@type='technology' and not(@name='unsatisfied CDR demand')]/
          *[@type='output']/physical-output/node()
        </xPath>
        <comments>Excludes unsatisfied CDR demand</comments>
      </supplyDemandQuery>
    </aQuery>
  </queries>"

  # Create a temporary XML file for the query
  query_file <- tempfile(fileext = ".xml")
  writeLines(CDR_query, query_file)

  # Establish database connection and add scenario
  CDR_tech <- addScenario(localDBConn(db_path, db_name), paste0(dat_file, ".dat"), scenario_list, query_file)

  # Query the database for CDR outputs
  CDR_Output <- getQuery(CDR_tech, "CDR by tech")

  # Filter by region if region_list is provided
  if (!is.null(region_list)) {
    CDR_Output <- CDR_Output %>% filter(region %in% region_list)
  }

  # Load the CDR_Job_Inten dataset directly from the package
  data("CDR_Job_Inten", package = "CDRJOBS4")

  # Helper function to calculate job estimates
  calculate_jobs <- function(data) {
    data %>%
      group_by(across(-value)) %>%
      summarize(
        mean_Jobs = sum(mean_int * 3.667 * 10^6 * value, na.rm = TRUE),
        min_Jobs = sum(min_int * 3.667 * 10^6 * value, na.rm = TRUE),
        max_Jobs = sum(max_int * 3.667 * 10^6 * value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Join the CDR Output with job intensity values
  joined_data <- dplyr::left_join(CDR_Output, CDR_Job_Inten, by = c("technology" = "CDR"))

  # Calculate jobs by year and technology
  Job_total_year <- calculate_jobs(joined_data %>% group_by(scenario, region, year))
  Job_by_tech_year <- calculate_jobs(joined_data %>% group_by(scenario, region, technology, year))
  Job_cum_tech <- calculate_jobs(joined_data %>% group_by(scenario, region, technology))
  Job_cum_total <- calculate_jobs(joined_data %>% group_by(scenario, region))

  # Save CSVs if specified
  if ("csv" %in% output_type) {
    write.csv(Job_total_year, file.path(output_path, "Job_total_year.csv"), row.names = FALSE)
    write.csv(Job_by_tech_year, file.path(output_path, "Job_by_tech_year.csv"), row.names = FALSE)
    write.csv(Job_cum_tech, file.path(output_path, "Job_cum_tech.csv"), row.names = FALSE)
    write.csv(Job_cum_total, file.path(output_path, "Job_cum_total.csv"), row.names = FALSE)
  }

  results <- list(
    Job_total_year = Job_total_year,
    Job_by_tech_year = Job_by_tech_year,
    Job_cum_tech = Job_cum_tech,
    Job_cum_total = Job_cum_total
  )

  # Create plots if requested
  if (create_plots) {
    message("Generating and saving visualizations...")

    # Visualize cumulative jobs by technology
    visualize_results(
      data = Job_cum_tech,
      type = "cum_tech",
      job_metric = job_metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )

    # Visualize total jobs by year
    visualize_results(
      data = Job_total_year,
      type = "total_year",
      job_metric = job_metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      selected_years = selected_years,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )
  }

  # Return results as a list if specified
  if ("list" %in% output_type || length(output_type) == 0) {
    return(results)
  }
}
