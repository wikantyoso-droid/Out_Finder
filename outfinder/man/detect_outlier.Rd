\name{detect_outlier}
\alias{detect_outlier}
\title{deteksi outlier}
\description{
  Package digunakan untuk mencari variabel yang tergolong Outlier dalam data.
}
\usage{
detect_outlier(data,cloumn)
}
\arguments{
  \item{data}{Data yang akan digunakan untuk mencari outlier.}
  \item{coloumn}{Kolom dari data yang akan dicari outliernya.}
}
\examples{
  tes_data = data.frame(
    kolom1 = c(150,153,151,157,154,156,165,159,240,151)
  )
  detect_outlier(tes_data, "kolom1")
}
