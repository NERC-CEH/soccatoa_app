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
