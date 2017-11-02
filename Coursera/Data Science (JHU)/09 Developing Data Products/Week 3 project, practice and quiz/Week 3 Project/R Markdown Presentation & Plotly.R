library(plotly)
library(datasets)
library(ggplot2)

df <- as.data.frame(UCBAdmissions)


p <- ggplot(data=df, aes(x = Gender, y = Freq, fill = Admit)) +
        geom_bar(stat="identity") + facet_grid(~ Dept) +
        labs(title = "Student Admissions for the six largest departments in 1973",
             x = "Departments", y = "") +
        scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0))
ggplotly(p)
