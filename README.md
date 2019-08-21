<p align="center">
  <img src="https://github.com/ordanovich/images/blob/master/portadaWlogos.png?raw=true">
</p>

## Interactive application for programmatic data retrieval from [WorldBank](https://www.worldbank.org/)

This appication is based on the [**wbstats** package](https://github.com/GIST-ORNL/wbstats). To learn more about the functionality of the package please refer to the [tutorial](https://cran.r-project.org/web/packages/wbstats/vignettes/Using_the_wbstats_package.html#fn2).

:books: [WorldBank knowledge base](https://datahelpdesk.worldbank.org/knowledgebase)
:shipit: [WorldBank developer information](https://datahelpdesk.worldbank.org/knowledgebase/topics/125589)

### Nomenclature

- :rocket: [app.R](https://github.com/ordanovich/downloadWORLDBANK/blob/master/app.R): Shiny app combining **UI** and **server** parts.
- :bar_chart: [report.Rmd](https://github.com/ordanovich/downloadWORLDBANK/blob/master/report.Rmd): template markdown file to generate and download an HTML report through the application interface

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/worldbank_download/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to consult the contents of the data base, retrieve and visualize the desired datasets in a quick and easy-to-manipulate manner. 
