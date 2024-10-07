# README


This repo contains the source function to generate clean codebooks from REDCap for Hope Home projects.

# Installation

Make sure you have `devtools`, `credentials` and `remotes` already
installed and loaded. Also be sure to be active on the **UCSF VPN**.

- `install.packages("devtools")`
- `install.packages("credentials")`
- `install.packages("remotes"`
- `library(devtools)`
- `library(credentials)`
- `library(remotes)`

Steps are commented out below beneath its description.

``` r
#set config
# usethis::use_git_config(user.name = "YourName", user.email = "your@mail.com")

#Go to github page to generate token
# usethis::create_github_token() 

#paste your PAT into pop-up that follows...
# credentials::set_github_pat()

#now remotes::install_github() will work
# remotes::install_github("https://github.com/ucsf-bhhi/hope_home_tools")
```
