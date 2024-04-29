## Version 1.1.2

### Added Functionality
* Added more options for setting time intervals in time-dependent fossil sampling
* Added buttons to clear specific parts of the simulation
* Simulation functions now print information about the time spent and events simulated

### UI changes
* Font size and colour of the time and depth axis have been updated to make the axis more visible.

### Bug fixing
* Saving the plot no longer breaks when no data is present (instead a blank plot is saved)
* Taxonomy colours no longer change every time the plot is updated

## Version 1.1.1

### UI changes
* "Environment-dependent" model is now called "Depth-dependent" for clarity
* Better and more detailed error messages

### Bug fixing
* Invalid Newick strings now give an error message instead of silently failing
* Simulating with tip number = 1 now works
* Plot is no longer cut at the bottom on smaller screens

## Version 1.1.0

#### Added Functionality
* Plots can now be saved in PDF format
* The simulated can now be downloaded as RData for future reference

### UI changes
* Clarified names of fossil input parameters

### Bug fixing
* Added missing tooltips on fossil simulation functions
* Time intervals are now plotted according to the correct simulation model used


## Version 1.0.0

#### General Functionalities
* Ability to generate fossil, taxonomy and phylogenetic tree data.
* User friendly interface.
* Fully deployable web app.
