# This script will install most of the packages used in this book.
# The list of packages to be installed can be found in the file listpackages.txt located in the same directory as this script.
# This script will not install keras. Instructions on how to install keras are included in the book appendix.

# IMPORTANT: The intention of this script is to ease the installation of the required packages.
# This script was tested on R 4.0.5 on Windows 10.
# This script may not work with your platform.


# Read list of packages to install.
df <- read.table("listpackages.txt")

listPackages <- unique(df$V1)

installedPackages <- NULL

# Iterate through packages and install.
for(p in listPackages){
  
  res <- require(p, character.only = T, quietly = T, warn.conflicts = F)
  if(res == F){
    install.packages(p)
    installedPackages <- c(installedPackages, p)
  }
}

# Print installed packages.
print("The following packages were installed: ")
print(installedPackages)

# Print the packages that were not installed (if any).
listPackages[which(!listPackages %in% installedPackages)]
