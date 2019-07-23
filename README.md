# Spanish census exploration

This repository contains the R code used to explore the data of the Spanish population census from
1874 to 2011. Data is available in Spanish National Institute of Statistics [INE](https://www.ine.es/intercensal/intercensal.do?search=1&cmbTipoBusq=0&textoMunicipio=Ponferrada&btnBuscarDenom=Consultar+selecci%F3n).


## Data

Census data is not available in INE to be downloaded as a whole file, so we developed a [scrapper](https://gist.github.com/ferblape/e34c6cc07b7096c86f9f8ba27349d01b) and loaded it into [Populate
Data](https://data.populate.tools) to consume it as an API. The rest of data required is included in the `data/` repository.

We combined the census data with Rafael del Pino [Contabilidad Nacional Histórica](https://espacioinvestiga.org/bbdd-chne/) database, which is a very interesting dataset of almost 200 years of social and economic statistics of Spain.


## Analysis content

The file `historic-data-analysis.R` contains both code to explore the data (although most of it has been removed for the shake of clarity) and code to generate the final charts


## Some charts generated with this code

![](http://gobierto-public-resources.s3.amazonaws.com/censo-ine-201907/censo-ine-201907-ciudades.png)

![](http://gobierto-public-resources.s3.amazonaws.com/censo-ine-201907/censo-ine-201907-mapa.png)


## Feedback and comments

Please, write to fernando@populate.tools if you have any comments or questions about this code. I'll be happy to discuss and to hear your feedback.


## Special thanks

- [Robin Lovelace](https://twitter.com/robinlovelace) for the book [Geocomputation with R](https://geocompr.robinlovelace.net/index.html)
- [Beatriz Martínez](https://visualizados.com) for providing the Spanish IGN map features parsing code
