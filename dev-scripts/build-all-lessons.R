# Build all production-level lessons (those with numbers)
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-12-01

library(rmarkdown)

# Rmarkdown files that have a number (i.e. are out of beta)
lesson_files <- list.files(pattern = "[0-9]{3}.*\\.Rmd$")

# Loop over all Rmarkdown files and create html + pdfs
for (rmd in lesson_files) {
  message(paste0("Rendering ", rmd))
  rmarkdown::render(input = rmd,
                    output_format = "all")
}
