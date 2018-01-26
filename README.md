# westcoastseas-gp
West Coast ResilienSeas Group Project 

## Build website

Using [Rmarkdown websites](http://rmarkdown.rstudio.com/rmarkdown_websites.html) framework, website can be knitted from individual files and if menu changed in `_site.yml`, then go to Build tab in RStudio and "Build Website".

## Process

### Analysis

**Paths**. Work in` scripts/*.R` files and reference paths using the `here` R package with the `here()` function.

**Functions**. You can pull in functions from your `oatools` library locally, using:

```r
devtools::load_all(here("../oatools"))
```

### Publish

Then visualize outputs in your website `./*.Rmd`
