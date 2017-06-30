# Calculates basic statistical measures for the given data.
#
# Arguments:
#   data: Array of values for which statistical measures will be applied.
#   digits: Number of decimal places to be used.
basic_measures = function(data, digits=2) {
    avg = mean(df$Velocidade)
    stdev = sd(df$Velocidade)

    # Note: Algorithm used for quantile() depends on 'type' attribute!
    quants = quantile(df$Velocidade)

    measures = c("Min.", "1st Qu.", "Median", "Mean", "Std. Dev.", "3rd Qu.", "Max")
    # Force values to use the desired number of decimal places
    values = round(c(quants[1:3], avg, stdev, quants[4:5]), digits)
    values = format(values, nsmall=digits)

    # Join arrays in a matrix and print the result
    table = format(rbind(measures, values), justify="right")
    write.table(table, row.names=F, col.names=F, quote=F)
}
