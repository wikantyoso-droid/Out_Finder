#' Deteksi Outlier
#'
#' Packages digunakan untuk mencari variabel yang tergolong outlier dalam data.
#'
#' @param data Data frame yang akan digunakan untuk mencari outlier.
#' @param column Kolom dari data (dalam bentuk karakter) yang akan dicari outlier-nya.
#'
#' @return Data frame yang hanya berisi baris-baris outlier.
#'
#' @examples
#' # example
#' data(iris)
#' detect_outlier(iris, "Sepal.Length")
#'
#' tes_data = data.frame(nilai=c(10,12,13,15,99))
#' detect_outlier(tes_data, "nilai")
#'
#' @export
detect_outlier=function(data,column){
  if(!is.data.frame(data)) stop("Data harus berupa data frame.")
  if(!column %in% names(data)) stop("Kolom tidak ditemukan dalam data.")
  if(!is.numeric(data[[column]])) stop("Kolom harus bertipe numerik")

  q1 = quantile(data[[column]], 0.25, na.rm = TRUE)
  q3 = quantile(data[[column]], 0.75, na.rm = TRUE)
  iqr = q3 - q1
  lower = q1 - 1.5 * iqr
  upper = q3 + 1.5 * iqr

  outliers = data[data[[column]] < lower | data[[column]] > upper, ]
  return(outliers)
}2
