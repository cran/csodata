csodata
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## An R package for downloading CSO data.

The csodata package allows for easily downloading CSO (Central
Statistics Office, the statistics agency of Ireland) StatBank data into
R. It also includes multiple functions for examining the metadata of
tables in the StatBank, as well as a function to download geographic
data in the ESRI vector format from the cso website.

StatBank is the Central Statistics Office’s (CSO) online database of
Official Statistics. This database contains current and historical data
series compiled from CSO statistical releases and is accessed
[here](http://www.cso.ie/px/pxeirestat/statire/SelectTable/Omrade0.asp?Planguage=0).
A tutorial explaining and illustrating how to navigate to the StatBank
and how to search the StatBank by keyword or theme can be found
[here:](https://www.cso.ie/en/interactivezone/youtubevideos/statbanktutorial/).
This tutorial also demonstrates how a user can create, edit and download
a table from the StatBank. The CSO StatBank Application Programming
Interface (API), which is accessed in this package, provides access to
StatBank data in [JSON-stat
format](https://statbank.cso.ie/webserviceclient/). This dissemination
tool allows developers machine to machine access to CSO StatBank data.

### References

Graeme Walsh (2018). statbanker R package version 6.2.0. *For
inspiration and code used for reshaping tables.*
