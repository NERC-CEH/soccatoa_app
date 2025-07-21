# SOCCATOA shiny app
This runs on [Datalabs](https://datalab.datalabs.ceh.ac.uk/projects).

## Getting started
1. Open the SOCCATOA project.
2. Open the Notebook currently called "madda's notebook" (should be renamed 'SOCCATOA WP5 app'?.
3. Open app.R and click on the "Run app" button or just enter `golem::run_dev()` at the terminal. The app will open in a new tab.
4. Click on login and enter user:test pass:test or make an account.
5. Currently we need to upload data each time, but this is work in progress. Test data is available [here](https://github.com/NERC-CEH/soccatoa_app/blob/main/data-raw/soccatoa_input_test.csv). Select file and click on Submit.
6. After uploading, click on Run.

## Connect to GitHub from Datalabs
Connecting to GitHub needs to use the http url, as ssh is not available on Datalabs.
This requires using a personal token in place of a password - see [Generate a personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).
Each user's live [here](https://github.com/settings/tokens).
Thereafter, this can be used in http authentication in place of a password, from the command line or via the Rstudio dialogues.

## Code formatting using air
To convert a project
```{r}
# to get the dev version of usethis
pak::pak("r-lib/usethis")
library(usethis)
usethis::use_air()

# create a github action to check every pull request
usethis::use_github_action(url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml")
```

Thereafter in VS code use Ctrl+Shift+P: `Air: Format Workspace Folder`
For Rstudio, see [here](https://posit-dev.github.io/air/editor-rstudio.html).


## Variable naming
I suggest using a consistent naming convention throughout the code as far as possible, where names are constructed as `noun_modifier1_modifier2`.
- "_" means subscript i.e modifier
- following the [tidyverse style guide](https://style.tidyverse.org/) which is the default for 'lintr' and 'styler'
- i.e. snake_case not camelCase, so no capital letters except where these are need for disambiguation in maths ('F' for "flux not 'f(x)')
Where possible, define the maths in LaTeX/markdown and use the same in the R code
- e.g. $\theta_{obs}$, is written as 'theta_obs' in both LaTeX/markdown and R
- Use "Hungarian notation" to make the object type explicit - many/most coding errors arise through confusion over the object type
    - add a systematic prefix / suffix denoting type / class
    - `df_` for data frames, `r_` for rasters ...

Suggested prefixes for common R data structures

df_ for data frame (or tibble)
dt_ for data table
spdf_ for spatial point data frame
v_ for vector
m_ for matrix (note potential confusion with model fit object)
a_ for array
l_ for list
t_ for table
r_ for raster layer
s_ for raster stack
b_ for raster brick