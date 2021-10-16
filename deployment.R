library(shiny)
library(rsconnect)

rsconnect::setAccountInfo(name='cwu377',
                          token='8D72A24FAB99803513740DA7A1716657',
                          secret='9MAs9sFwdZFb9BiXsxRtt8k7tYXtMYBGUmfGEr+f')


deployApp("app", appName = "Test", appTitle = "Test")
