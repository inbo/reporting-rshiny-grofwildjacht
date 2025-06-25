
## Installation

As the `reportingGrofWild` package is not available on CRAN, the easiest way of installing the package is using the `remotes` package and referring to the git repository holding the package code:

```
remotes::install_github("inbo/reporting-rshiny-grofwildjacht", subdir = "reporting-grofwild")
```

Remark that the package itself is stored in a subfolder of the repository, which is tackled by the `subdir` command.

Once installed, the functionalities can be derived by loading the package:

``` r
library("reportingGrofwild")
```

## Development information

### R package update

When adding elements to the code or ui and you want to test if your local adaptations are effectively working, make sure to build and install your local adaptations:

1. When working in a Rstudio project

* In Rstudio, File > New project... > Version control > Git > 
* in the field Repository URL, add URL github repo: `https://github.com/inbo/reporting-rshiny-grofwildjacht`
* This clones the repo to your computer and creates an Rstudio project linked to it

2. To test and try out your adaptations, load the latest version of the package functions and start the app:

```
devtools::load_all(path = "~/git/reporting-rshiny-grofwildjacht/reporting-grofwild")
reportingGrofwild::runWildApp()
```

### Add new graph for public app

Steps for adding a new output (plot/table) to the public app.

**Step 1**: Create output function(s)

If needed, create a file (e.g. `R/countYearAge.R`) for the plot that contains:
(1) plot function, 
(2) Shiny server module function and 
(3) Shiny UI module function. 
Often, existing scripts can be recycled or updated for multi-purpose use. 

**Step 2**: Define when & where to show the output

In file `app_choices.R`: 

* For function `getSubcategoryOutput()`: specify the subcategory for the new output; this output should be a unique name e.g. Shiny UI module function name.
* For function `getOutputSpecie()`: specify minimal data requirements for the new output; use same unique output name as above.

**Step 3**: Include output in shiny app

In file `app_category_*.R` (update for relevant category):

* For function *OutputServer(): Specify the Shiny UI & server module function, using the same unique output name as defined in step 2.

**Step 4**: Customize images and text

* Image for tile: Add file /inst/ui/www/category-*-output.png: where you fill out the relevant category and 'output' is the unique output name as defined in step 2.
* Custom text: Add row to file /inst/extdata/uiText.csv: 
  - plotFunction: unique output name as defined in step 2. Next one can define
  - title: to be shown above graph in UI
  - summary: empty (""); short description only used in the tiles
  - description: description to be shown with the graph in UI (public app)
  - wbe: description to be shown with the graph in UI (private app)
  
