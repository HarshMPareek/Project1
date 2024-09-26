# Load the styler package
library(styler)

# Style all R scripts in the 'scripts' folder within the working directory
styler::style_dir(path = "scripts")
