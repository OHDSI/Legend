# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of Legend
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' OHDSI-formatted LEGEND report
#'
#' @inheritParams  rmarkdown::pdf_document
#' @param ... Additional arguments to \code{rmarkdown::pdf_document}
#' @param keep_tex Boolean toggle as to whether intermediate LaTex file is saved
#' @return R Markdown output format to pass to \code{\link[rmarkdown:render]{render}}
#'
#' @section Document options:
#'
#' \describe{
#'   \item{\code{databaseId}}{TODO}
#' }
#' @export
legend_report <- function(..., keep_tex = TRUE) {
    template <- system.file("latex", "blank_template.tex",
                            package = "Legend")
    base <- inherit_pdf_document(...,
                                 template = template,
                                 keep_tex = keep_tex)

    # base$knitr$opts_chunk$prompt <- FALSE 	# changed from TRUE
    # base$knitr$opts_chunk$comment <- '# '	# default to one hashmark
    # base$knitr$opts_chunk$highlight <- TRUE  	# changed as well
    #
    # base$knitr$opts_chunk$collapse <- collapse 	# allow override
    #
    # base$knitr$opts_chunk$dev.args <- list(pointsize = 9)  # from 11
    # base$knitr$opts_chunk$fig.width <- 3.5 	# from 4.9 # 6.125" * 0.8, as in template
    # base$knitr$opts_chunk$fig.height <- 3.5	# from 3.675 # 4.9 * 3:4
    # base$knitr$opts_chunk$fig.align <- "center"
    #
    # hook_output <- function(x, options) {
    #     paste('\\begin{ShadedResult}\n\\begin{verbatim}\n', x,
    #           '\\end{verbatim}\n\\end{ShadedResult}\n', sep = '')
    # }
    # if (!collapse) base$knitr$knit_hooks$output  <- hook_output
    # base$knitr$knit_hooks$message <- hook_output
    # base$knitr$knit_hooks$warning <- hook_output

    base
}

inherit_pdf_document <- function(...) {
    fmt <- rmarkdown::pdf_document(...)
    fmt$inherits <- "pdf_document"
    fmt
}
