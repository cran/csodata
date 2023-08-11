## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# # Install or update the package:
# install.packages("csodata")

library(csodata)

## -----------------------------------------------------------------------------
toc <- cso_get_toc()
head(toc)

## -----------------------------------------------------------------------------
tbl1 <- cso_get_data("PEA19")

## -----------------------------------------------------------------------------
meta1 <- cso_get_meta("CDP06")
cso_disp_meta("CDP06")

## -----------------------------------------------------------------------------
shp <- cso_get_geo("County Councils")

## ---- fig.width = 5, fig.height=6, eval= !is.null(shp)------------------------
# install.packages("leaflet")
library(leaflet)


leaflet(shp) %>% 
  addTiles() %>% 
  addPolygons()



## -----------------------------------------------------------------------------
cso_clear_cache()

