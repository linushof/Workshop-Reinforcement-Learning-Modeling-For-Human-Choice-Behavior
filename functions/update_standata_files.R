update_standata_files <- function() {
  # Define the directory path
  dir_path <- "data/stan_ready_data_files"
  
  # Read all file names in the directory
  files_in_dir <- list.files(path = dir_path)
  
  # Attempt to load the existing standata_files list
  tryCatch({
    load(file.path(dir_path, "standata_files.Rdata"))
  }, error = function(e) {
    # If file doesn't exist or other loading error, initialize an empty list
    standata_files <- character()
  })
  
  # Ensure standata_files is treated as a vector, even if empty or not found
  if (!exists("standata_files")) {
    standata_files <- character()
  }
  
  # Files in directory without the list file itself
  files_in_dir <- setdiff(files_in_dir, "standata_files.Rdata")
  
  # Add new files to the list (those in the directory but not in the list)
  new_files <- setdiff(files_in_dir, standata_files)
  standata_files <- c(standata_files, new_files)
  
  # Remove non-existing files from the list (those in the list but not in the directory)
  standata_files <- standata_files[standata_files %in% files_in_dir]
  
  # Save the updated list back to the .Rdata file
  save(standata_files, file = file.path(dir_path, "standata_files.Rdata"))
}
