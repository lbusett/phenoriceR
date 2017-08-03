# ggplot(insos5kdata_filt, aes(x = date_fake, y = season, fill = season)) +
#   geom_joy(stat = "binline", binwidth = 8) +
#   scale_x_date(limits = as.Date(c("2002-10-01", "2003-10-01")),
#                date_labels = "%b %d") + theme_joy() +
#    scale_fill_cyclical(values = c("gray80", "gray50"))
#
# cuts <- as.Date('2001-08-29') + lubridate::years(1:15)
#
#
# ggplot(insos5kdata_filt, aes(x = date_fake))  +
#   geom_histogram(binwidth = 8) +
#   scale_x_date(date_breaks = "2 months", date_labels = "%b %d") +
#   theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#   facet_wrap(~groups) +
#   scale_x_date(limits = as.Date(c("2002-10-01", "2003-10-01")))
#
