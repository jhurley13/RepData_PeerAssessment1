
library(data.table)
library(ggplot2)

dir_proj <- rprojroot::find_root(rprojroot::criteria$is_rstudio_project, path = ".")

#dir.script <- dirname(sys.frame(1)$ofile)
data_path <- paste(dir_proj, "activity.csv", sep="/")

data <- as.data.table(read.csv(data_path, header = TRUE, sep = ","))

# Add a column with the "date" column converted to a POSIXlt object
data[, pdate := as.POSIXct(date, tz = "", format = "%Y-%m-%d"), by = interval]

# What is mean total number of steps taken per day?
#
# For this part of the assignment, you can ignore the missing values in the
# dataset.
#
# Calculate the total number of steps taken per day. If you do not understand the
# difference between a histogram and a barplot, research the difference between
# them. Make a histogram of the total number of steps taken each day. Calculate
# and report the mean and median of the total number of steps taken per day

steps_by_day <- copy(data[,c("steps", "date")])
setkey(steps_by_day, date)
steps_by_day_t <- steps_by_day[, .(total = sum(steps, na.rm = TRUE)), by = date]

hist(steps_by_day_t$total, xlab = "Steps per day", probability = FALSE, breaks = 20, main = "Histogram of Steps per Day")

print(paste0("total number of steps taken per day: Mean: ", mean(steps_by_day_t$total), ", Median: ", median(steps_by_day_t$total)))

# What is the average daily activity pattern?
#     
# Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all days
# (y-axis) Which 5-minute interval, on average across all the days in the
# dataset, contains the maximum number of steps?
    
steps_by_interval <- data[, .(mean_steps = mean(steps, na.rm = TRUE)), by = interval]
plot(steps_by_interval$interval, steps_by_interval$mean_steps, type="l")

max_interval <- unique(steps_by_interval[mean_steps==max(steps_by_interval$mean_steps)]$interval)
print(paste0("Interval with maximum average number of steps: ", max_interval, " (", max(steps_by_interval$mean_steps), " steps)"))

#
## Imputing missing values
#
# Note that there are a number of days/intervals where there are missing values
# (coded as 𝙽𝙰). The presence of missing days may introduce bias into some
# calculations or summaries of the data.
#
# Calculate and report the total number of missing values in the dataset (i.e.
# the total number of rows with 𝙽𝙰s) Devise a strategy for filling in all of
# the missing values in the dataset. The strategy does not need to be
# sophisticated. For example, you could use the mean/median for that day, or the
# mean for that 5-minute interval, etc. Create a new dataset that is equal to
# the original dataset but with the missing data filled in. Make a histogram of
# the total number of steps taken each day and Calculate and report the mean and
# median total number of steps taken per day. Do these values differ from the
# estimates from the first part of the assignment? What is the impact of
# imputing missing data on the estimates of the total daily number of steps?


# Are there differences in activity patterns between weekdays and weekends?
#
# For this part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use
# the dataset with the filled-in missing values for this part.
#
# Create a new factor variable in the dataset with two levels – “weekday” and
# “weekend” indicating whether a given date is a weekday or weekend day. Make a
# panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis). See the README file in the
# GitHub repository to see an example of what this plot should look like using
# simulated data.

is_weekend_day <- function(xdatetime) {
    as.factor(weekdays(xdatetime)) %in% c("Saturday", "Sunday")
}

data[, weekend := is_weekend_day(pdate)]

steps_by_interval_weekend <- data[data$weekend, .(mean_steps = mean(steps, na.rm = TRUE)), by = interval]
steps_by_interval_weekday <- data[!data$weekend, .(mean_steps = mean(steps, na.rm = TRUE)), by = interval]

library(ggpubr)

## scale_y_continuous(position = "right") +

max_y_value <- max(max(steps_by_interval_weekend$mean_steps), max(steps_by_interval_weekday$mean_steps))

weekend_plot <- 
    ggplot(steps_by_interval_weekend, aes(interval, mean_steps)) + geom_line() +
    xlab("") + ylab("") + ggtitle("Weekend") +
    coord_cartesian(ylim = c(0, max_y_value)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5))

weekday_plot <- 
    ggplot(steps_by_interval_weekday, aes(interval, mean_steps)) + geom_line() +
    xlab("Interval") + ylab("") + ggtitle("Weekday") +
    coord_cartesian(ylim = c(0, max_y_value)) +
    theme(plot.title = element_text(hjust = 0.5))

panel_plot <- ggarrange(weekend_plot, weekday_plot, ncol = 1, nrow = 2)
panel_plot <- annotate_figure(panel_plot,
                left = text_grob("Number of Steps", color = "black", rot = 90)
)
show(panel_plot)

##  data[, is_weekday := weekday_factor(date)]

##  http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/#using-1-n-setkey-and-by-for-within-group-subsetting
