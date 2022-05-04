# t.o.a.d. = Tab manager
# set of functions that help us manage our tabs

# Creates a new tab manager
toadCreate <- function(keys) {
  toad = reactiveValues()
  
  for (key in keys) {
    toad[[key]] = list(tree = NULL, fossils = FossilSim::fossils())
  }
  
  toad$currentTab = "v"
  return(toad)
}

# Returns the current tab
# only used for debugging
toadGetCurrentTab <- function(toad) {
  return(toad[[toad$currentTab]])
}

# Returns the next tab's key
# only used for debugging
toadNextTabKey <- function(toad, keys) {
  for (i in 1:(length(keys)-1)) {
    if (toad$currentTab == keys[i]) {
      return(keys[i + 1])
    }
  }
  
  toad$currentTab = keys[2]
  return(toad$currentTab)
}

# Return the new tab's key
toadNewTabKey <- function(keys, Ckeys) {
  for (k in keys) {
    if (!(k %in% Ckeys)) {
      return(k)
    }
  }
}