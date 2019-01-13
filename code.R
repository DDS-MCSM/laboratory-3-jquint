#******************************************************************************#
#                                                                              #
#                    Lab 3 - Data Acquisition & Analysis                       #
#                                                                              #
#                     Jonathan Quintana - Data Driven Securty                  #
#                                                                              #
#******************************************************************************#

## Crawling y Scrapping
#install.packages("httr")
#nstall.packages("XML")
#install.packages("xml2")
library(httr)
library(XML)
library(xml2)
library(dplyr)
library(ggplot2)


### 1.1 Obtención de la página web
get_title <- function(web){
  html_code2 <- read_html(web)
  return(html_code2)
}

#html_code <- get_title("https://www.mediawiki.org/wiki/MediaWiki")

### 1.2 Analisis de el contenido de la web
GetParsed <- function(html_code){
  htmlParse(html_code, asText = T)
}

GetWebTitle <- function(parsed){
  xpathApply(parsed, "//head/title")
}

web_content <- function(html_code){
  parsed <- GetParsed(html_code)
  title <- GetWebTitle(parsed)
  return(title)
}

#parsed <- GetParsed(html_code2)
#titleWebPage <- GetWebTitle(parsed)

### 1.3.	Extracción de enlaces
get_links <- function(parsed){
  a <- xpathApply(parsed, "//a")
  href <- sapply(a , function(x){ xmlGetAttr(x, "href") })
  final <- a[!sapply(href,is.null)]
  text <- sapply(final, xmlValue)
  href <- sapply(final , function(x){ xmlGetAttr(x, "href") })
  df <- data.frame(text = as.character(text), link = as.character(href), stringsAsFactors = F)
  df <- df[-grep(x = df$link, pattern = "^#"),]
  return(df)
}

#df <- GetLinks(parsed)


### 1.4 Exploración de enlaces
expl_links <- function(data){
  wiki <- handle("https://www.mediawiki.org/wiki/MediaWiki")
  results <- sapply(data$link, function(x) { 
    if (grepl("^/", x)) 
      HEAD(handle = wiki, path = x)$status 
    else 
      HEAD(url = x)$status 
    #Sys.sleep(4) 
    })
  status <- data.frame(link = data, status = results)
  return(status)
}

#dfStatus <- LinkStatus(df$link)


### Gráficos en R

### 2.1 Histograma
#relativas <- df$link[-grep(pattern = "http*", x = df$link)]
#absolutas <- df$link[grep(pattern = "http*", x = df$link)]
GetHist <- function(df){
  df$isabs <- grepl(pattern = "http*", x = df$link)
  hist <- ggplot(df, aes(x=link, color=isabs)) + geom_histogram(fill="white", stat="count") + theme(axis.text.x = element_text(angle=90, hjust = 1)) + coord_flip()
  return(hist)
}
#df$isabs <- grepl(pattern = "http*", x = df$link)

#ggplot(df, aes(x=link, color=isabs)) + geom_histogram(fill="white", stat="count") + theme(axis.text.x = element_text(angle=90, hjust = 1))
       
#qplot(df$link, geom = "histogram", stat = "count")
#summary(df$isabs)

### 2.2 Un gráfico de barras
GetBarPlot <- function(remote_links) {
#remote_links <- df
  remote_links$isexternal <- sapply(remote_links$link, function(x) { 
    if (grepl(x, pattern = "https://www.mediawiki.org*")) 
        x <- "external"
    else
      x <- "internal"
      } )
  summary(remote_links)
  bar_plot <- ggplot(remote_links, aes(x=isexternal, fill = isexternal)) + geom_bar(stat = "count")
  return(bar_plot)
}

#bar <- GetBarPlot(df)

### 2.3 Pie Chart
#pie <- ggplot(dfStatus, aes(x="", y="", fill=as.factor(status))) + geom_bar(width=1, stat="identity") + coord_polar("y", start=0)

