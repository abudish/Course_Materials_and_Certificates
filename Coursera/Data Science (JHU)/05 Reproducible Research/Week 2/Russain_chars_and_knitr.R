fileName <- "Practice_knitr.rmd"
knit(fileName, encoding = "utf-8")
markdownToHTML("test.md", "test.html")
browseURL(paste("file://", file.path(getwd(), "test.html"), sep = ""))