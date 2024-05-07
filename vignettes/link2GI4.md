---
author: "Chris Reudenbach"
title: "OTB Wrapper"
date: "2024-05-06"
editor_options:
  chunk_output_type: console
output:
  html_document: 
    theme: united
    toc: yes
  rmarkdown: default
  pdf_document:
    latex_engine: xelatex
    toc: yes
urlcolor: blue
vignette: >
  %\VignetteIndexEntry{OTB Wrapper}
  %\VignetteEncoding{UTF-8}{inputenc}\
  %\VignetteEngine{knitr::knitr}
---


# A typical use case for the Orfeo Toolbox wrapper
link2GI supports the use of the Orfeo Toolbox with a simple list-based wrapper function. Actually, two functions parse the module and function syntax dumps and generate a command list that can be easily modified with the necessary arguments.

Usually you have to get the module list first:




```r
# link to the installed OTB 
otblink<-link2GI::linkOTB()


# get the list of modules from the linked version
algo<-parseOTBAlgorithms(gili = otblink)
```

Based on the modules of the current version of `OTB', you can then select the module(s) you want to use.



```r
## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)

## print the current command
print(cmd)
```

Admittedly this is a very straightforward and preliminary approach. Nevertheless it provids you a valid list of all `OTB` API calls that can easily manipulated for your needs. The following working example will give you an idea how to use it.



```r
require(link2GI)
require(raster)
require(listviewer)

otblink<-link2GI::linkOTB()
 root_dir<-tempdir()
 
fn <- system.file("ex/elev.tif", package = "terra")

## for the example we use the edge detection, 
algoKeyword<- "EdgeExtraction"

## extract the command list for the choosen algorithm 
cmd<-parseOTBFunction(algo = algoKeyword, gili = otblink)

## get help using the convenient listviewer
listviewer::jsonedit(cmd$help)

## define the mandantory arguments all other will be default
cmd$input_in  <- fn
cmd$filter <- "touzi"
cmd$channel <- 1
cmd$out <- paste0(root_dir,paste0("\\out",cmd$filter,".tif"))

## run algorithm
retStack<-runOTB(cmd,gili = otblink)

## plot filter raster on the green channel
plot(retStack)
```

