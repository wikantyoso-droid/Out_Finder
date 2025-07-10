detect_outlier = function(data,column) {
  values = data[[column]]
  q1 = quantile(values, 0.25)
  q3 = quantile(values, 0.75)
  iqr = q3 - q1
  lower = q1 - 1.5 * iqr
  upper = q3 + 1.5 * iqr
  outliers = values[values < lower | values > upper]
  return(outliers)
}
