library(rhdf5client)
system("rm -f profile.out")
fname <- "profile.out"

bigec2 <- H5S_source("http://54.174.163.77:5000")
txdat <- bigec2[["tenx_100k_sorted"]]

cat("-------------------------------------------------------------------------",
  file=fname, sep="\n", append=TRUE)
cat("2000 x 2000 JSON transfer", file=fname, sep="\n", append=TRUE)
Rprof()
for (i in 1:5)  {
  M <- submatrix(txdat, 1:2000, 1:2000)
}
Rprof(NULL)
out <- capture.output(summaryRprof())
cat(out, file=fname, sep="\n", append=TRUE)

cat("-------------------------------------------------------------------------",
  file=fname, sep="\n", append=TRUE)
transfermode(txdat) <- "binary"
cat("2000 x 2000 binary transfer", file=fname, sep="\n", append=TRUE)
Rprof()
for (i in 1:5)  {
  M <- submatrix(txdat, 1:2000, 1:2000)
}
Rprof(NULL)
out <- capture.output(summaryRprof())
cat(out, file=fname, sep="\n", append=TRUE)

cat("-------------------------------------------------------------------------",
  file=fname, sep="\n", append=TRUE)
transfermode(txdat) <- "JSON"

cat("20000 x 200 JSON transfer", file=fname, sep="\n", append=TRUE)
Rprof()
for (i in 1:5)  {
  M <- submatrix(txdat, 1:20000, 1:200)
}
Rprof(NULL)
out <- capture.output(summaryRprof())
cat(out, file=fname, sep="\n", append=TRUE)

cat("-------------------------------------------------------------------------",
  file=fname, sep="\n", append=TRUE)
transfermode(txdat) <- "binary"
cat("20000 x 200 binary transfer", file=fname, sep="\n", append=TRUE)
Rprof()
for (i in 1:5)  {
  M <- submatrix(txdat, 1:20000, 1:200)
}
Rprof(NULL)
out <- capture.output(summaryRprof())
cat(out, file=fname, sep="\n", append=TRUE)

cat("-------------------------------------------------------------------------",
  file=fname, sep="\n", append=TRUE)
transfermode(txdat) <- "JSON"

cat("200 x 20000 JSON transfer", file=fname, sep="\n", append=TRUE)
Rprof()
for (i in 1:5)  {
  M <- submatrix(txdat, 1:200, 1:20000)
}
Rprof(NULL)
out <- capture.output(summaryRprof())
cat(out, file=fname, sep="\n", append=TRUE)

cat("-------------------------------------------------------------------------",
  file=fname, sep="\n", append=TRUE)
transfermode(txdat) <- "binary"
cat("200 x 20000 binary transfer", file=fname, sep="\n", append=TRUE)
Rprof()
for (i in 1:5)  {
  M <- submatrix(txdat, 1:200, 1:20000)
}
Rprof(NULL)
out <- capture.output(summaryRprof())
cat(out, file=fname, sep="\n", append=TRUE)
