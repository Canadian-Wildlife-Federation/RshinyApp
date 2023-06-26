# RshinyApp
This is a repository for accompanying WCRPs in Canadian watersheds in a convenient RShiny dashboard app form. In order to create a similar web app, copy this repository and change the writing necessary by following the code comments in app.R while also changing specific paths included in other files within the repository. Styling choices can be changed through the www/simple_app_styling.css file.

# Requirements
- [R](https://cran.r-project.org/bin/windows/base/) installed on your local machine or [RStudio](https://posit.co/download/rstudio-desktop/)
- Priority and intermediate barrier lists for the watershed you wish to build the WCRP Dashboard app (see data folder in current repository for examples).
- Mapbox access [token](https://www.mapbox.com/) if wishing to use mapbox basemaps in app (free to create account and token).
- Follow code and read comments which show how to alter code to suit specific watershed area.

# Instuctions
1. Start by cloning this repository (forking it first if necessary). Use the cloned repository link to initialize the repository on your local machine in order to make changes to the code later. [See here how to clone and fork](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/adding-and-cloning-repositories/cloning-and-forking-repositories-from-github-desktop)
2. Based on the watershed you are working in, change all of the API calls to reflect the watershed within the calls. For example, if working in the Bulkley (BULK), change the following line from _crossings_res <- read_sf("http://159.89.114.239:9002/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27**HORS**%27%20AND%20all_spawningrearing_km%3e0")_ to _crossings_res <- read_sf("http://159.89.114.239:9002/collections/bcfishpass.crossings/items.json?filter=watershed_group_code%20=%20%27**BULK**%27%20AND%20all_spawningrearing_km%3e0")_
3. Follow any links to logos or csvs within the code in order to reflect your own data (i.e., make sure the links point to the right places).
4. Change colours and styling of the app using the www/style.css file in the repository to your liking.
