R.home()
Sys.getenv("HOME")
.libPaths()

# check if there's even a site-wide RProfile
site_path = R.home(component = "home")
fname = file.path(site_path, "etc", "Rprofile.site")
file.exists(fname)

file.exists("~/.Rprofile")


# edit the user R Profile
file.edit("~/.Rprofile")

# find the environment
user_renviron = path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron)



#### how to update r and reinstall all packages ============================================================================

# first update the location for your packages in your .Renviron. It's probably just changing 3.4 to 3.5 or something 
user_renviron = path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron)

# Restart R and then use libPaths to check and make sure your new path appears
.libPaths()

# now, get the names of all the packages from the OLD .libPaths folder
packages <- list.files("/Users/MJ/R_Packages_3.4")

# reinstall all those packages in your new location, and they'll build with the new R installation (this will take a while)
install.packages(packages, .libPaths()[1])


# check any packages that you don't have now, such as packages that you got from github

packages_compare <- function(old_path, new_path){
  old_packages <- list.files(old_path)
  new_packages <- list.files(new_path)
  
  in_both <- old_packages %in% new_packages
  not_installed_now <- old_packages[!in_both]
  return(not_installed_now)
}

still_need <- packages_compare("/Users/MJ/R_Packages_3.4", "/Users/MJ/R_Packages_3.5")
still_need

# the package githubinstall will find packages in github without the username and ask you if they're right, then install them
library(githubinstall)

githubinstall(still_need)

# one last check to see if there are any you didn't get
packages_compare("/Users/MJ/R_Packages_3.4", "/Users/MJ/R_Packages_3.5")

