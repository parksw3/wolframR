## ----load, message = FALSE-----------------------------------------------
library(wolframR)
library(ggplot2); theme_set(theme_classic())
library(plyr)
library(dplyr)

## ----collatz, fig.width = 4, fig.height = 4, dpi = 300-------------------
set.seed(101)
n <- sample(1e7, 2000)

collatz_test <- function(x){
    ifelse(x %% 2 == 0,
        x/2,
        ifelse(x == 1, NA, 3 * x  + 1)
    )
}

collatz_seq <- n %>%
    nest_while(collatz_test, any(!is.na(x)), m = "all")

collatz_list <- do.call("rbind", collatz_seq) %>%
    split(rep(1:ncol(.), each = nrow(.))) %>%
    lapply(function(x) rev(x[!is.na(x)]))

convert_collatz <- function(x, c = 0.4 , h = 0.8, a = 1.5){
    list(
        theta = c * (h - 2 * (x %% 2)),
        length = x/(1+x^a))
}

collatz_angle <- collatz_list %>%
    lapply(convert_collatz) %>%
    lapply(function(x){
        args <- x
        args$plot <- FALSE
        res <- do.call("angle_path", args)
    }) %>%
    bind_rows(.id = "run")

ggplot(collatz_angle, aes(x, y, group = run)) +
    geom_path(alpha = 0.04)

## ----fancy_collatz, fig.width = 4, fig.height= 4, dpi = 300--------------
collatz_angle2 <- collatz_list %>%
    lapply(function(x){
        data.frame(level = 1/length(x) * 0:length(x))
    }) %>%
    bind_rows %>%
    cbind(collatz_angle) %>%
    as_data_frame

collatz_theme <- theme(
    panel.background = element_rect(fill = "black",
        colour = "black"),
    plot.margin = unit(c(0, 0, 0, 0), "in"),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "null"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
)

ggplot(collatz_angle2, aes(x, y, group = run)) +
    geom_path(aes(col = level), lwd = 0.15, alpha = 0.07) +
    scale_colour_gradientn(
        colours = c("#89505C", "#2F3665", "#E4DD7C")) +
    collatz_theme

