f = function(settings, ...) {
  # Here's how I envisioned a typical use case within a standard PEcAn function
  if(is.MultiSettings(settings)) {
    return(papply(settings, f, ...))
  }
  
  # Don't worry about the beolow, it's just some guts to make the function do something we can see
  l <- list(...)
  for(i in seq_along(l)) {
    ind <- length(settings) + 1
    settings[[ind]] <- l[[i]]
    if(!is.null(names(l))) {
      names(settings)[ind] <- names(l)[i]
    }
  }
  return(settings)
}

# Example
settings1 <- Settings(list(a="aa", b=1:3, c="NA"))
settings2 <- Settings(list(a="A", b=4:5, c=paste))
multiSettings <- MultiSettings(settings1, settings2)

# The fucntion should add element $d = D to either a Settings, or each entry in a MultiSettings
f(settings1, d="D")
print(f(multiSettings, d="D"), TRUE)

