# This function allows the user to remove multiple working models from the list
# of available models. It presents a GUI dialog box allowing the user to select
# multiple models to remove, and then updates the list of available models accordingly.

remove_multiple_workingmodels <- function() {
    # Load the list of available models
    load('functions/working_model.rdata')
    
    # Present a GUI select list for the user to choose multiple models
    selected_models <- dlg_list(title = 'Select models to remove:',
                                mymodels_list,
                                multiple = TRUE)$res
    
    # Update mymodels_list by removing the selected models
    mymodels_list <- mymodels_list[!(mymodels_list %in% selected_models)]
    
    # Print a message indicating which models have been removed
    cat(paste(selected_models, 'were removed from the models list.\n'))
    
    # Save the updated mymodels_list back to the file
    save(mymodels_list, file = 'functions/working_model.rdata')
}