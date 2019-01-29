# skossxml
reads and parses skoss xml and rdf files


## Installation

Installing `skossxml` requires the `devtools` package:

```R
install.packages("devtools")
devtools::install_github("Daphnisd/skossxml") 
```


## Usage

Read xml file

```R
getskossxmldata("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/")
getskossxmldata(c("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/","http://vocab.nerc.ac.uk/collection/P06/current/ULAA/")
getskossxmldata("http://vocab.nerc.ac.uk/collection/S10/current/")
getskossxmldata("http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/rdf")

```


get the identifiers linked to a certain term

```R
getrelatedterms(vocid = "http://vocab.nerc.ac.uk/collection/S25/current/BE007117/", relation = "narrower", vocab = "P01" )
```


Read P01 vocabterms and their related units
```R
getunitsandparams("http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL03/")
getunitsandparams(getrelatedterms(vocid = "http://vocab.nerc.ac.uk/collection/S25/current/BE007117/", relation = "narrower", vocab = "P01" ))
```
