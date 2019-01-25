# skossxml
reads and parses skoss xml and rdf files


## Installation

Installing `skossxml` requires the `devtools` package:

```R
install.packages("devtools")
devtools::install_github("Daphnisd/skossxml") 
```


## Usage

```R
getskossxmldata("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/")
getskossxmldata(c("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/","http://vocab.nerc.ac.uk/collection/P06/current/ULAA/")
getskossxmldata("http://vocab.nerc.ac.uk/collection/S10/current/")
getskossxmldata("http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/rdf")
```

