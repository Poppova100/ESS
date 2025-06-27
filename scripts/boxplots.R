lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'assertthat')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dta_boxplots <- readRDS("./data/dta_boxplots.rds")

position <- as.factor(c('HWG', 'HWL', 'HWH', 'HWO', 'June-Sep.'))
f <- function(x) {
  r <- quantile(dta_boxplots, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

### whisker definition function
stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge2",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  assert_that(
    length(qs) == 5 && is.numeric(qs),
    msg = "`qs` should be a numeric vector with 5 values."
  )
  
  assert_that(
    all(qs == sort(qs)), 
    msg = "`qs` should be provided in ascending order."
  )
  
  assert_that(
    all(qs <= 1) && all(qs >= 0),
    msg = "`qs` should only span values [0, 1]."
  )
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      orientation = orientation,
      qs = qs,
      ...
    )
  )
}

StatBoxplotCustom <- ggplot2::ggproto("StatBoxplotCustom", ggplot2::Stat,
                                      required_aes = c("y|x"),
                                      non_missing_aes = "weight",
                                      dropped_aes = c("x", "y", "weight"),
                                      setup_data = function(self, data, params) {
                                        data <- ggplot2::flip_data(data, params$flipped_aes)
                                        data$x <- ggplot2:::"%||%"(data$x, 0)
                                        data <- ggplot2::remove_missing(
                                          data,
                                          na.rm = params$na.rm,
                                          vars = "x",
                                          name = "stat_boxplot_custom"
                                        )
                                        ggplot2::flip_data(data, params$flipped_aes)
                                      },
                                      
                                      setup_params = function(self, data, params) {
                                        params$flipped_aes <- ggplot2::has_flipped_aes(data, params, 
                                                                                       main_is_orthogonal = TRUE,
                                                                                       group_has_equal = TRUE,
                                                                                       main_is_optional = TRUE)
                                        data <- ggplot2::flip_data(data, params$flipped_aes)
                                        
                                        has_x <- !(is.null(data$x) && is.null(params$x))
                                        has_y <- !(is.null(data$y) && is.null(params$y))
                                        if (!has_x && !has_y) {
                                          abort("stat_boxplot() requires an x or y aesthetic.")
                                        }
                                        
                                        params$width <- ggplot2:::"%||%"(
                                          params$width, 
                                          (ggplot2::resolution(ggplot2:::"%||%"(data$x, 0) * 0.75))
                                        ) 
                                        
                                        if (!ggplot2:::is_mapped_discrete(data$x) && is.double(data$x) && 
                                            !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                          rlang::warn(glue::glue(
                                            "Continuous {flipped_names(params$flipped_aes)$x} aesthetic -- did you forget aes(group=...)?"
                                          ))
                                        }
                                        
                                        params
                                      },
                                      
                                      extra_params = c("na.rm", "orientation"),
                                      
                                      compute_group = function(data, scales, width = NULL, na.rm = FALSE, 
                                                               qs = c(.05, .25, 0.5, 0.75, 0.95), flipped_aes = FALSE) {
                                        
                                        data <- ggplot2::flip_data(data, flipped_aes)
                                        
                                        if (!is.null(data$weight)) {
                                          mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                          stats <- as.numeric(stats::coef(mod))
                                        } else {
                                          stats <- as.numeric(stats::quantile(data$y, qs))
                                        }
                                        names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                                        iqr <- diff(stats[c(2, 4)])
                                        
                                        outliers <- (data$y < stats[1]) | (data$y > stats[5])
                                        
                                        if (vctrs::vec_unique_count(data$x) > 1)
                                          width <- diff(range(data$x)) * 0.9
                                        
                                        df <- ggplot2:::data_frame0(!!!as.list(stats))
                                        df$outliers <- list(data$y[outliers])
                                        
                                        if (is.null(data$weight)) {
                                          n <- sum(!is.na(data$y))
                                        } else {
                                          n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                                        }
                                        
                                        df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                                        df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                                        
                                        df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                                        df$width <- width
                                        df$relvarwidth <- sqrt(n)
                                        df$flipped_aes <- flipped_aes
                                        ggplot2::flip_data(df, flipped_aes)
                                      }
)

###

boxplots<-ggplot(data = dta_boxplots, aes(x = factor(Type, levels = position), y = value, fill = variable)) + 
  theme_bw()+
  scale_y_continuous(breaks = c(-5, -3, -1, 1, 3, 5))+
  theme(panel.grid.major.x = element_blank())+
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), color='grey')+
  coord_cartesian(ylim=c(-3.5,5))+ 
  scale_fill_manual(name= '                   ', values = c("3_days_before" = "#D84315", 
                                                            "during_HW" = "#4CAF50", 
                                                            "3_days_after" = "#0288D1", 
                                                            "June-Sep." = "#9E9E9E"),
                    labels = c("3_days_before"="3 days before     ", "during_HW"="during HW     ", "3_days_after"="3 days after     ", "June-Sep."="June-Sep.     "), 
                    breaks = c('3_days_before', 'during_HW', '3_days_after', 'June-Sep.'))+
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.50, 0.75, 0.9), 
                      outlier.shape = NA, 
                      notch = FALSE, 
                      width = 0.67, 
                      position = position_dodge2(width = 0.67, preserve = "single"))+
  labs(y= "PET-P [mm/d]")+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())

#ggsave(boxplots, file="./figures/boxplots.png", height = 10 , width = 15, units = "cm", bg='white')

