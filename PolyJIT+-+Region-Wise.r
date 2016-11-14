
devtools::load_all()
library(RPostgres)
library(DT)
library(gridExtra)
library(ggplot2)
if (!require(reshape2)) { install.packages("reshape2") }
library(reshape2)
if (!require(rpivotTable)) { install.packages("rpivotTable")}
library(rpivotTable)

c <- benchbuild::login("bb")
library(repr)
options(repr.plot.width=12, repr.plot.height=8)

exps <- benchbuild::get_experiments(c)

plot_comparison_data <- function(selected) {
  exps <- exps[exps$id %in% selected,]$id
  plot_data <- benchbuild::region_wise_comparison(c, exps)
  return(plot_data)
}

plot_comparison <- function(plot_data) {
  plot <- ggplot(data = plot_data, aes(x=cores, y=speedup, color = cores)) +
    geom_point() +
    coord_cartesian(ylim = c(-10,10)) +
    facet_wrap(~ project)
      theme(axis.text = element_text(size = 8), axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position="none")
  return(plot)
}

scatter_plot <- function(plot_data) {
  plot <- ggplot(data = plot_data, aes(y=speedup, x=runtime_jit, color=cores)) +
    scale_x_log10() +
    geom_point(size=0.5) +
    geom_hline(yintercept=0) +
    coord_cartesian(ylim = c(-10,10)) +
    facet_wrap(~ cores) +
    geom_smooth(method=lm, se=TRUE, fullrange=TRUE, size=0.5) +
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot)
}

project_plot <- function(plot_data) {
  plot <- ggplot(data = plot_data, aes(y=speedup, x=project, color=cores)) +
    geom_point(size=0.2) +
    geom_jitter() +
    geom_hline(yintercept=0) +
    coord_cartesian(ylim = c(-10,10)) +
    #facet_wrap(~ t) +
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot)
}

bar_plot <- function(plot_data) {
  plot <- ggplot(data = plot_data, aes(y=speedup, x=region_name, fill=cores)) +
    geom_bar(stat="identity", position="dodge") +
    geom_hline(yintercept=0) +
    facet_wrap(~ cores) +
    coord_cartesian(ylim = c(-10,10)) +
    scale_x_discrete() +
    scale_y_discrete(limits = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10))
    theme(axis.text = element_text(size = 8))
    #,
    #      axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot)
}

box_plot <- function(plot_data) {
  plot <- ggplot(data = plot_data, aes(y=speedup, x=cores)) +
    geom_violin(trim= TRUE, adjust = 1) +
    geom_boxplot(width=.1, outlier.size = 0) +
    #geom_jitter(height=0, size=0.5) +
    geom_hline(yintercept=0) +
    scale_y_discrete(limits = c(-40,-30,-20,-10,-5,-4,-3,-2,-1,0,1,2,3,4,5,10,20,30,40)) +
    coord_cartesian(ylim = c(min(plot_data$speedup),max(plot_data$speedup))) +
    xlab("Number of cores") +
    ylab("Speedup: Recompilation over baseline") +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10))
  return(plot)
}

#data <- plot_comparison_data(c(
#    '0f42573f-973d-4cd4-bfac-b71961d100ed',
#    '09de35a6-f0af-4577-91a2-a564b63ef900'
#))

#data <- rbind(data, plot_comparison_data(c(
#    '917f3844-063a-45ef-8397-c55818c37dd4',
#    'a1116f0d-81c5-456e-a9de-6232c51df483',
#    'cab8443e-1172-4246-8bed-59ac62a133ec'
#)))


# PLDI 2017 Current
#polybench <- plot_comparison_data(c('9fd28f1b-8a9e-4d99-b495-e3b18fe9d347'))
#llvm_nt <-plot_comparison_data(c('45d076f4-52b7-4d38-8cea-4e1d64714aed'))
#benchbuild <- plot_comparison_data(c('d5c856a3-4f78-4988-b43f-a349d246eab2'))
#data <- rbind(polybench, llvm_nt)
#data <- rbind(data, benchbuild)

# New
#data <- plot_comparison_data(c('05461bd4-92ca-43c8-8f2c-4bbd26167ee8'))

data <- plot_comparison_data(
  c('395e0449-4e7f-4366-9928-eaae699be388',
    '4f74a94b-0176-4345-a9b5-c63ab1205cfe',
    '3d979f16-8d8c-4d5e-b51a-f2a6c963cfd3',
    '71ff99e8-d65c-4a36-8ee4-e6068a290eb5',
    '6005160b-fb89-4538-a982-d891f5a9f323',
    'd8fba4ac-1afb-4121-a8c0-f4c5ffd9ac16',
    '058b87a9-4e18-4dc6-a3cb-5cde41329d4d',
    '3cb17d02-4a6c-40d2-8896-f9f357e1bd9d'))

spec <- plot_comparison_data(c('058b87a9-4e18-4dc6-a3cb-5cde41329d4d'))
lnt_ssb <- plot_comparison_data(c('3cb17d02-4a6c-40d2-8896-f9f357e1bd9d'))
polybench <- plot_comparison_data(c('6005160b-fb89-4538-a982-d891f5a9f323'))
lnt_msb <- plot_comparison_data(c('71ff99e8-d65c-4a36-8ee4-e6068a290eb5'))
lnt_msa <- plot_comparison_data(c('3d979f16-8d8c-4d5e-b51a-f2a6c963cfd3'))
scimark <- plot_comparison_data(c('d8fba4ac-1afb-4121-a8c0-f4c5ffd9ac16'))
benchbuild <- plot_comparison_data(c('395e0449-4e7f-4366-9928-eaae699be388'))

print_regions <- function(cls, data) {
  projects <- levels(as.factor(data$project))
  for (p in projects) {
    p.data <- data[data$project == p,]
    cat("Class:", cls, " - ", p, " Regions: ", length(levels(as.factor(p.data$region_name))), "\n")
  }
  cat("Total: ", length(levels(as.factor(data$region_name))), "\n")
}

print_regions("spec", spec)
print_regions("lnt_ssb", lnt_ssb)
print_regions("lnt_msb", lnt_msb)
print_regions("lnt_msa", lnt_msa)
print_regions("scimark", scimark)
print_regions("polybench", polybench)
print_regions("benchbuild", benchbuild)

print_runs <- function(cls, data) {

}

print_tone <- function(cls, data) {
  projects <- levels(as.factor(data$project))
  for (p in projects) {
    p.data <- data[data$project == p,]
    cat("Class:", cls, " - ", p, " Regions: ", length(levels(as.factor(p.data$runtime_no_recomp))), "\n")
  }
  cat("Total: ", length(levels(as.factor(data$region_name))), "\n")
}

print_regions("spec", spec)
print_regions("lnt_ssb", lnt_ssb)
print_regions("lnt_msb", lnt_msb)
print_regions("scimark", scimark)
print_regions("polybench", polybench)

data$region_name <- as.numeric(factor(data$region_name))
data <- data[data$runtime_jit > 1000,]
data <- transform(data, speedup = ifelse(speedup >= 1, speedup, -1/speedup))
data <- data[complete.cases(data),]
data <- data[!is.infinite(data$speedup),]

pos <- data[(as.numeric(data$cores) %in% c(1,2,3,4,5,6,7,8)),]
pos <- pos[pos$speedup < 30,]
pos <- pos[pos$speedup > 1.1,]
neg <- data[(as.numeric(data$cores) %in% c(1,2,3,4,5,6,7,8)),]
neg <- neg[neg$speedup < -1.1,]
neg <- neg[neg$speedup > -30,]
#data_pos <- data_filter[(data_filter$speedup > 0.50),]

neg$t <- "bad"
pos$t <- "good"

all <- rbind(neg, pos)

options(repr.plot.width=11, repr.plot.height=4)
box_plot(all)
box_plot(pos)
box_plot(neg)

#pdf(file = "./box-plot_all.pdf", family = "ComputerModern")
pdf(file = "./box-plot_all.pdf", width = 11, height=8)
box_plot(all)
dev.off()

pdf(file = "./box-plot_pos.pdf")
box_plot(pos)
dev.off()

pdf(file = "./box-plot_neg.pdf")
box_plot(neg)
dev.off()

length(levels(as.factor(pos$project)))
length(levels(as.factor(neg$project)))

length(levels(as.factor(neg$region_name)))
length(levels(as.factor(pos$region_name)))
length(levels(as.factor(all$region_name)))

csv <-read.csv("/home/simbuerg/Documents/2016/2017-PLDI-polyjit/sql/data.csv")
mean(csv$dyncov)
min(
min(csv$s2),
min(csv$s3),
min(csv$s4[!is.na(csv$s4)]),
min(csv$s5[!is.na(csv$s5)]))

max(
max(csv$s2),
max(csv$s3),
max(csv$s4[!is.na(csv$s4)]),
max(csv$s5[!is.na(csv$s5)]))

box_plot_small <- function(plot_data, ylims, scale_limits) {
  plot <- ggplot(data = plot_data, aes(y=speedup, x=cores)) +
    geom_violin(trim= TRUE, adjust = 1) +
    geom_boxplot(width=.1, outlier.size = 0) +
    #geom_jitter(height=0, size=0.5) +
    scale_y_discrete(limits = scale_limits) +
    coord_cartesian(ylim = ylims) +
    xlab("Number of cores") +
    ylab("Speedup: Recompilation over baseline") +
    theme(axis.title = element_text(size = 10),
          axis.text = element_text(size = 10))
  return(plot)
}

options(repr.plot.width=4, repr.plot.height=2)
pos_small_data <- pos[pos$speedup<=10 & pos$cores %in% c(1,5),]
neg_small_data <- neg[neg$speedup>=-10 & neg$cores %in% c(1,5),]

pos_small <- box_plot_small(pos_small_data,
                            ylims=c(1, 10),
                            scale_limits=c(1,2,3,4,5,10))
neg_small <- box_plot_small(neg_small_data,
                            ylims=c(-5, -1),
                            scale_limits=c(-5,-4,-3,-2,-1))

length(levels(as.factor(pos_small_data$region_name)))

pdf(file = "./box-plot_small_pos.pdf", width = 6, height=6)
pos_small
dev.off()
pdf(file = "./box-plot_small_neg.pdf", width = 6, height=6)
neg_small
dev.off()
