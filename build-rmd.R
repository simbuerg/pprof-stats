if (!require(knitr)) {
  install.packages("knitr")
}
if (!require(rmarkdown)) {
  install.packages("rmarkdown")
}

devtools::load_all()
c <- pprof::login()
exps <- pprof::get_experiments(c)

render_runtime_comparison <- function(g) {
  rmarkdown::render("runtime-comparison.Rmd", params = list(group = g),
    output_file = paste("html_out/runtime.comparison.", g, ".html",
      sep = ""))
}

render_single_experiment <- function(e, prefix, name) {
  rmarkdown::render("single-experiment.Rmd", params = list(experiment = e),
    output_file = paste("html_out/single.exp.", prefix, ".", gsub(" ",
      "_", name), ".html", sep = ""))
}

render_runtime_comparison("polybench")
render_runtime_comparison("lnt")
render_runtime_comparison("pprof")
render_runtime_comparison("gentoo")

rt_exps <- c("pj-raw", "raw", "polly-openmp", "polly-vectorize", "polly-openmpvect", "polly")
apply(subset(exps, exps$name %in% rt_exps),
  1,
  function(exp_obj) {
  render_single_experiment(exp_obj[2], exp_obj[1], exp_obj[3])
})
