## ----setup, echo=FALSE---------------------------------------------------
suppressPackageStartupMessages({
library(rhdf5client)
})

## ----dsmeta--------------------------------------------------------------
library(rhdf5client)
bigec2 = H5S_source("http://54.174.163.77:5000")
bigec2
dsmeta(bigec2)[1:2,]      # two groups
dsmeta(bigec2)[1,2][[1]]  # all dataset candidates in group 1

## ----doso----------------------------------------------------------------
mys = H5S_source(serverURL="http://54.174.163.77:5000")
mys

## ----groups--------------------------------------------------------------
groups(mys)

## ----links---------------------------------------------------------------
lks = links(mys,1)
lks

## ----dataset-------------------------------------------------------------
dta = bigec2[["tenx_100k_sorted"]] # arbitrary name assigned long ago
dta

## ----access--------------------------------------------------------------
x = dta["15:20", "1904:1906"]
x

