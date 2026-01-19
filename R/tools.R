#' Get Tool Path
#'
#' Locates the correct platform-specific binary for a genetic analysis tool.
#'
#' @param tool_name Character string specifying the tool name: "blupf90", "plink", or "renumf90".
#'
#' @return Character string containing the full path to the tool binary.
#' @export
#'
#' @details
#' This function automatically detects the user's operating system and returns
#' the path to the corresponding pre-compiled binary included with the package.
#'
#' @examples
#' \dontrun{
#'   get_tool_path("blupf90")
#'   get_tool_path("plink")
#' }
#'
get_tool_path <- function(tool_name) {
  # Validate tool name
  valid_tools <- c("blupf90", "plink", "renumf90", "inbupgf90")
  if (!tool_name %in% valid_tools) {
    stop("Invalid tool name. Must be one of: ", paste(valid_tools, collapse = ", "))
  }

  # Detect operating system
  sys_info <- Sys.info()["sysname"]
  
  # Map system name to directory name
  os_map <- c(
    "Linux" = "linux",
    "Darwin" = "macos",
    "Windows" = "windows"
  )
  
  os_dir <- os_map[sys_info]
  
  if (is.na(os_dir)) {
    stop("Unsupported operating system: ", sys_info)
  }
  
  # Construct the actual binary filename
  # blupf90+ uses "+" in the filename on Unix systems
  if (tool_name == "blupf90") {
    if (os_dir == "windows") {
      tool_file <- "blupf90+.exe"
    } else {
      tool_file <- "blupf90+"
    }
  } else {
    # plink and renumf90 don't have special characters
    if (os_dir == "windows") {
      tool_file <- paste0(tool_name, ".exe")
    } else {
      tool_file <- tool_name
    }
  }
  
  # Try to find the tool in the installed package first
  pkg_dir <- system.file(package = "linkbreedeR")
  
  if (pkg_dir != "") {
    tool_path <- file.path(pkg_dir, "extbin", os_dir, tool_file)
    if (file.exists(tool_path)) {
      return(tool_path)
    }
  }
  
  # Fallback for development mode: look in current working directory structure
  # This allows testing during development with devtools::load_all()
  dev_path <- file.path(getwd(), "inst", "extbin", os_dir, tool_file)
  if (file.exists(dev_path)) {
    return(dev_path)
  }
  
  # Also try parent directory (if running from subdirectory)
  dev_path_alt <- file.path(dirname(getwd()), "inst", "extbin", os_dir, tool_file)
  if (file.exists(dev_path_alt)) {
    return(dev_path_alt)
  }
  
  # If tool not found, provide helpful error message
  stop(
    "Tool binary '", tool_name, "' not found.\n",
    "Searched locations:\n",
    "  1. ", file.path(pkg_dir, "extbin", os_dir, tool_file), "\n",
    "  2. ", dev_path, "\n",
    "  3. ", dev_path_alt, "\n",
    "Please ensure the package is properly installed or you are in the development directory."
  )
}


#' Run BLUPF90+
#'
#' Execute BLUPF90+ for genomic BLUP analysis.
#'
#' @param par_file Character string specifying the parameter file path.
#' @param data_file Optional character string specifying the data file path.
#' @param ... Additional arguments to pass to the BLUPF90+ executable.
#'
#' @return Invisible list containing exit code, stdout, and stderr from the process.
#' @export
#'
#' @details
#' This function runs the BLUPF90+ tool which is used for Best Linear Unbiased 
#' Prediction (BLUP) analysis in genomic prediction and quantitative genetics.
#'
#' @examples
#' \dontrun{
#'   run_blupf90("parameter_file.par")
#' }
#'
run_blupf90 <- function(par_file, data_file = NULL, ...) {
  tool_path <- get_tool_path("blupf90")
  
  # Check if parameter file exists
  if (!file.exists(par_file)) {
    stop("Parameter file not found: ", par_file)
  }
  
  # Build command arguments
  args <- c(par_file)
  
  if (!is.null(data_file)) {
    if (!file.exists(data_file)) {
      stop("Data file not found: ", data_file)
    }
    args <- c(args, data_file)
  }
  
  # Add any additional arguments
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    args <- c(args, unlist(extra_args))
  }
  
  # Execute tool
  message("Running BLUPF90+ with parameter file: ", par_file)
  result <- processx::run(tool_path, args = args, error_on_status = FALSE)
  
  return(invisible(list(
    exit_code = result$status,
    stdout = result$stdout,
    stderr = result$stderr
  )))
}


#' Run PLINK
#'
#' Execute PLINK for genome-wide association studies and quality control.
#'
#' @param cmd Character string containing PLINK command arguments.
#'
#' @return Exit code from the PLINK process.
#' @export
#'
#' @details
#' This function runs PLINK, a comprehensive association analysis toolset designed 
#' to perform a range of basic, large-scale analyses in a computationally efficient manner.
#'
#' @examples
#' \dontrun{
#'   run_plink("--bfile mydata --freq")
#'   run_plink("--bfile 1 --make-bed --out 1")
#' }
#'
run_plink <- function(cmd) {
  tool_path <- get_tool_path("plink")
  
  if (missing(cmd) || nchar(cmd) == 0) {
    stop("Please provide arguments for PLINK. Use --help for available options.")
  }
  
  # Build complete command
  full_cmd <- paste(shQuote(tool_path), cmd)
  
  # Execute tool using system()
  message("Running PLINK: ", full_cmd)
  exit_code <- system(full_cmd)
  
  return(invisible(exit_code))
}


#' Run RENUMF90
#'
#' Execute RENUMF90 for renumbering and converting genetic data formats.
#'
#' @param par_file Character string specifying the parameter file path.
#' @param ... Additional arguments to pass to the RENUMF90 executable.
#'
#' @return Invisible list containing exit code, stdout, and stderr from the process.
#' @export
#'
#' @details
#' This function runs RENUMF90, a utility program for renumbering animals and 
#' converting between different genetic data formats.
#'
#' @examples
#' \dontrun{
#'   run_renumf90("parameter_file.par")
#' }
#'
run_renumf90 <- function(par_file, ...) {
  tool_path <- get_tool_path("renumf90")
  
  # Check if parameter file exists
  if (!file.exists(par_file)) {
    stop("Parameter file not found: ", par_file)
  }
  
  # Build command arguments
  args <- c(par_file)
  
  # Add any additional arguments
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    args <- c(args, unlist(extra_args))
  }
  
  # Execute tool
  message("Running RENUMF90 with parameter file: ", par_file)
  result <- processx::run(tool_path, args = args, error_on_status = FALSE)
  
  return(invisible(list(
    exit_code = result$status,
    stdout = result$stdout,
    stderr = result$stderr
  )))
}


#' Run INBUPGF90
#'
#' Execute INBUPGF90 for inbreeding coefficient calculation and pedigree analysis.
#'
#' @param cmd Character string containing INBUPGF90 command arguments.
#'
#' @return Exit code from the INBUPGF90 process.
#' @export
#'
#' @details
#' This function runs INBUPGF90, a program for calculating inbreeding coefficients
#' and performing pedigree analysis in genetic evaluations.
#'
#' @examples
#' \dontrun{
#'   run_inbupgf90("--pedfile myped.txt")
#'   run_inbupgf90("--pedfile pedigree.txt --out results")
#' }
#'
run_inbupgf90 <- function(cmd) {
  tool_path <- get_tool_path("inbupgf90")
  
  if (missing(cmd) || nchar(cmd) == 0) {
    stop("Please provide arguments for INBUPGF90. Use --help for available options.")
  }
  
  # Build complete command
  full_cmd <- paste(shQuote(tool_path), cmd)
  
  # Execute tool using system()
  message("Running INBUPGF90: ", full_cmd)
  exit_code <- system(full_cmd)
  
  return(invisible(exit_code))
}
