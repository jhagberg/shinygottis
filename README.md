# R-Shiny provparningsprogram för Föreningen Gotlandskaninen
R shiny Gottis provparning

# Testa
Du behöver en registerfil denna kan fås av genbankansvarig.
Spara denna fil i samma katalog namnge den med datum enligt maill i app.r
tex G20191125.xlsx

Uppdatera då app.R till 
```
filename<-"G20191125"
```
För att köra behöver du installera R samt biblioteken i R genom att köra tex.

```
R -e "options(rgl.useNULL = TRUE);install.packages(c('shiny', 'rmarkdown', 'optiSel', 'shinyjs','readxl', 'anytime', 'shinydashboard','data.table','devtools'), repos='https://ftp.acc.umu.se/mirror/CRAN'); \
          library(devtools);devtools::install_github(c('datastorm-open/visNetwork','luansheng/visPedigree'))"
```


We are using the [Browserstack](http://browserstack.com/) application for testing the webpage so it works for everyone
![Browserstack](https://bstacksupport.zendesk.com/attachments/token/LfDPvQkWhH5GSKf2dMAZDIYLG/?name=browserstack-logo-600x315.png)


