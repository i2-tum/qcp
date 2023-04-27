requiredPackages <- c(
    "tidyverse",
    "sqldf",
    "hrbrthemes",
    "ggpubr",
    "ggridges",
    "viridis"
)

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
}

ipak(requiredPackages)