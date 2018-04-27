# .libPaths()[1]
# instPackages <- list.files(.libPaths()[1])
# ?install.packages
# getwd()
# library(nothing)
# library(base)
# library(stats)
# install.packages(pkgs = instPackages, lib = "R_Packages")
# 
# .libPaths()[2]
# .libPaths(new = "R_Packages")

R.home()
Sys.getenv("HOME")
.libPaths()

# check if there's even a site-wide RProfile
site_path = R.home(component = "home")
fname = file.path(site_path, "etc", "Rprofile.site")
file.exists(fname)

file.exists("~/.Rprofile")


# edit the R Profile
user_rprofile = path.expand(file.path("~", ".Rprofile"))
file.edit(user_rprofile)

file.edit("~/.Rprofile")

# find the environment
user_renviron = path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron)

install.packages()


#### how to update r and reinstall all packages ============================================================================

# first update the location for your packages in your .Renviron. It's probably just changing 3.4 to 3.5 or something 
user_renviron = path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron)

# Restart R and then use libPaths to check and make sure you're good here.
.libPaths()

# now, get the names of all the packages from the OLD .libPaths folder
x <- installed.packages(lib.loc = "/Users/MJ/R_Packages_3.4")

# extract the names of those packages
packages <- x[,1]

# reinstall all those packages in your new location, and they'll build with the new R installation
install.packages(packages, .libPaths()[1])



# check any packages that you don't have now
old_packages <- installed.packages(lib.loc = "/Users/MJ/R_Packages_3.4")[,1]
new_packages <- installed.packages(lib.loc = "/Users/MJ/R_Packages_3.5")[,1]

in_both <- old_packages %in% packages
not_installed_now <- old_packages[!in_both]
not_installed_now
