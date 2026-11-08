#' @importFrom SummarizedExperiment `assays<-` rbind cbind
NULL

# Names describe identities, not annotation columns. Keep validation separate
# from alignment so both provided sources are checked before subsetting.
.se_name_problem <- function(value, size, label) {
  if (is.null(value) && size == 0L) {
    return(NULL)
  }
  if (is.null(value) || length(value) != size) {
    return(paste0(label, " must have one name per position"))
  }
  if (anyNA(value) || any(value == "")) {
    return(paste0(label, " must not contain missing or empty names"))
  }
  if (anyDuplicated(value)) {
    return(paste0(label, " must be unique"))
  }
  NULL
}

.assert_se_names <- function(value, size, label) {
  problem <- .se_name_problem(value, size, label)
  if (!is.null(problem)) {
    cli::cli_abort(problem)
  }
  invisible(value)
}

.align_se_axis <- function(matrix_names, info, size, prefix, label) {
  if (is.null(info)) {
    info <- S4Vectors::make_zero_col_DFrame(size)
  } else if (!methods::is(info, "DataFrame")) {
    info <- methods::as(info, "DataFrame")
  }
  if (nrow(info) != size) {
    cli::cli_abort(
      "{.arg {label}} must have {size} rows, one per abundance position."
    )
  }
  info_names <- rownames(info)
  if (!is.null(matrix_names)) {
    .assert_se_names(matrix_names, size, paste0("abundance ", prefix, " names"))
  }
  if (!is.null(info_names)) {
    .assert_se_names(info_names, size, paste0(label, " row names"))
  }
  if (!is.null(matrix_names) && !is.null(info_names)) {
    if (!setequal(matrix_names, info_names)) {
      cli::cli_abort(
        "Abundance names and {.arg {label}} row names must match exactly."
      )
    }
    info <- info[match(matrix_names, info_names), , drop = FALSE]
  }
  resolved <- matrix_names %||% info_names %||% paste0(prefix, seq_len(size))
  if (size == 0L) {
    resolved <- character()
  }
  rownames(info) <- resolved
  list(names = resolved, data = info)
}

.construct_named_se <- function(
  abundance,
  rowData = NULL,
  colData = NULL,
  ...
) {
  if (length(dim(abundance)) != 2L) {
    cli::cli_abort("{.arg abundance} must be two-dimensional.")
  }
  rows <- .align_se_axis(
    rownames(abundance),
    rowData,
    nrow(abundance),
    "V",
    "rowData"
  )
  cols <- .align_se_axis(
    colnames(abundance),
    colData,
    ncol(abundance),
    "S",
    "colData"
  )
  dimnames(abundance) <- list(rows$names, cols$names)
  SummarizedExperiment::SummarizedExperiment(
    list(abundance = abundance),
    rowData = rows$data,
    colData = cols$data,
    ...
  )
}

# SE exposes its authoritative top-level names through public accessors. A
# conversion may receive an assay whose stored names predate a top-level rename.
.prepare_named_se <- function(x) {
  dn <- list(rownames(x), colnames(x))
  sizes <- dim(x)
  for (i in seq_len(2L)) {
    if (is.null(dn[[i]])) {
      dn[i] <- list(
        if (sizes[i]) paste0(c("V", "S")[i], seq_len(sizes[i])) else character()
      )
    }
    .assert_se_names(dn[[i]], sizes[i], c("row names", "column names")[i])
  }
  # Call the public parent method explicitly to permit repairing older objects
  # whose other axis has not yet been named, without touching S4 slots.
  parent <- methods::getMethod("dimnames<-", c("SummarizedExperiment", "list"))
  x <- parent(x, dn)
  x <- .sync_se_assay_names(x)
  methods::validObject(x)
  x
}

.sync_se_assay_names <- function(x) {
  values <- SummarizedExperiment::assays(x, withDimnames = FALSE)
  for (i in seq_along(values)) {
    dimnames(values[[i]]) <- dimnames(x)
  }
  SummarizedExperiment::assays(x, withDimnames = FALSE) <- values
  x
}

.validate_se_names <- function(x) {
  dn <- list(rownames(x), colnames(x))
  msg <- c(
    .se_name_problem(dn[[1L]], nrow(x), "row names"),
    .se_name_problem(dn[[2L]], ncol(x), "column names")
  )
  values <- SummarizedExperiment::assays(x, withDimnames = FALSE)
  for (j in seq_along(values)) {
    a <- values[[j]]
    # R matrices represent a zero-length axis with NULL dimnames.
    if (
      any(vapply(
        seq_len(2L),
        function(i) {
          dim(x)[i] > 0L &&
            !identical(list(rownames(a), colnames(a))[[i]], dn[[i]])
        },
        logical(1)
      ))
    ) {
      msg <- c(msg, "underlying assay names must match object names")
      break
    }
  }
  msg
}

.replace_glyco_dimnames <- function(x, value) {
  if (!is.list(value) || length(value) != 2L) {
    cli::cli_abort(
      "Replacement dimnames must be a list of row and column names."
    )
  }
  value <- lapply(value, function(v) if (is.null(v)) NULL else as.character(v))
  .assert_se_names(value[[1L]], nrow(x), "row names")
  .assert_se_names(value[[2L]], ncol(x), "column names")
  parent <- methods::getMethod("dimnames<-", c("SummarizedExperiment", "list"))
  x <- parent(x, value)
  x <- .sync_se_assay_names(x)
  methods::validObject(x)
  x
}

#' @rdname GlycomicSE
#' @export
methods::setMethod(
  "dimnames<-",
  c("GlycomicSE", "list"),
  .replace_glyco_dimnames
)
#' @rdname GlycoproteomicSE
#' @export
methods::setMethod(
  "dimnames<-",
  c("GlycoproteomicSE", "list"),
  .replace_glyco_dimnames
)
#' @rdname GlycomicSE
#' @export
methods::setMethod(
  "dimnames<-",
  c("GlycomicSE", "NULL"),
  .replace_glyco_dimnames
)
#' @rdname GlycoproteomicSE
#' @export
methods::setMethod(
  "dimnames<-",
  c("GlycoproteomicSE", "NULL"),
  .replace_glyco_dimnames
)

.replace_glyco_rownames <- function(x, value) {
  .replace_glyco_dimnames(x, list(value, colnames(x)))
}
.replace_glyco_colnames <- function(x, value) {
  .replace_glyco_dimnames(x, list(rownames(x), value))
}
#' @rdname GlycomicSE
#' @export
methods::setMethod("rownames<-", "GlycomicSE", .replace_glyco_rownames)
#' @rdname GlycoproteomicSE
#' @export
methods::setMethod("rownames<-", "GlycoproteomicSE", .replace_glyco_rownames)
#' @rdname GlycomicSE
#' @export
methods::setMethod("colnames<-", "GlycomicSE", .replace_glyco_colnames)
#' @rdname GlycoproteomicSE
#' @export
methods::setMethod("colnames<-", "GlycoproteomicSE", .replace_glyco_colnames)

# Public SE mutators can bypass subclass validity, so check their returned
# objects before exposing them. Name synchronization uses the public assays API.
.finish_se_mutation <- function(x) {
  .assert_se_names(rownames(x), nrow(x), "row names")
  .assert_se_names(colnames(x), ncol(x), "column names")
  x <- .sync_se_assay_names(x)
  methods::validObject(x)
  x
}

.replace_glyco_names <- function(x, value) {
  .replace_glyco_rownames(x, value)
}

.replace_glyco_coldata <- function(x, ..., value) {
  x <- methods::callNextMethod()
  .finish_se_mutation(x)
}

.replace_glyco_rowdata <- function(x, ..., value) {
  x <- methods::callNextMethod()
  .finish_se_mutation(x)
}

.replace_glyco_assays <- function(x, withDimnames = TRUE, ..., value) {
  for (j in seq_along(value)) {
    a <- value[[j]]
    supplied <- list(rownames(a), colnames(a))
    expected <- list(rownames(x), colnames(x))
    for (i in seq_len(2L)) {
      if (!is.null(supplied[[i]]) && !identical(supplied[[i]], expected[[i]])) {
        cli::cli_abort(
          "Replacement assay names must match object names in the same order."
        )
      }
    }
    dimnames(a) <- expected
    value[[j]] <- a
  }
  x <- methods::callNextMethod(
    x = x,
    withDimnames = withDimnames,
    ...,
    value = value
  )
  # Do not use .finish_se_mutation() here: it calls assays<- again.
  methods::validObject(x)
  x
}

.subset_glyco_se <- function(x, i, j, ..., drop = TRUE) {
  x <- methods::callNextMethod()
  methods::validObject(x)
  x
}

.replace_subset_glyco_se <- function(x, i, j, ..., value) {
  x <- methods::callNextMethod()
  .finish_se_mutation(x)
}

.rbind_glyco_se <- function(..., deparse.level = 1) {
  x <- methods::callNextMethod()
  methods::validObject(x)
  x
}
.cbind_glyco_se <- function(..., deparse.level = 1) {
  x <- methods::callNextMethod()
  methods::validObject(x)
  x
}

#' Keep glyco-omics names valid during SE operations
#'
#' These methods preserve the complete, unique axis-name contract of
#' [GlycomicSE()] and [GlycoproteomicSE()] during common SE operations.
#' Repeated row or column selections, and binding objects with overlapping
#' names on the bound axis, now error instead of returning an invalid object.
#' Returned glyco-omics objects must also retain their required annotation fields.
#' Use distinct observation identifiers before binding; keep repeated biological
#' labels in annotation columns. Subsetting to zero rows or columns is supported.
#'
#' `names(x) <- value` is equivalent to replacing row names. Replacing `colData`
#' follows SE's positional semantics and synchronizes its row names to the
#' assay; supplied names must be complete and unique. To align differently
#' ordered components by name, use the constructors. Replacement assays may be
#' unnamed or have names identical to the object in the same order, even when
#' `withDimnames = FALSE`; resolved names are stored on the replacement assay.
#'
#' `combineRows()` and `combineCols()` retain their upstream behavior: they
#' return an ordinary `SummarizedExperiment`, outside this subclass contract.
#' Convert the result explicitly to restore glyco-omics validation.
#'
#' @param x A glyco-omics SE object.
#' @param value Replacement names, annotations, assays, or a subexperiment.
#' @param i,j Row and column indices.
#' @param ... Additional arguments passed to the parent SE method, or objects
#'   to bind.
#' @param drop Passed to the parent SE method.
#' @param withDimnames Whether the parent SE method checks assay names.
#' @param deparse.level Passed to the parent binding method.
#' @return A valid glyco-omics SE object.
#' @name se-name-operations
NULL

#' @rdname se-name-operations
#' @export
methods::setMethod("names<-", "GlycomicSE", .replace_glyco_names)

#' @rdname se-name-operations
#' @export
methods::setMethod("rowData<-", "GlycomicSE", .replace_glyco_rowdata)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "colData<-",
  c("GlycomicSE", "DataFrame"),
  .replace_glyco_coldata
)

#' @rdname se-name-operations
#' @export
methods::setMethod("colData<-", c("GlycomicSE", "NULL"), .replace_glyco_coldata)

#' @rdname se-name-operations
#' @export
methods::setMethod("assays<-", c("GlycomicSE", "list"), .replace_glyco_assays)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "assays<-",
  c("GlycomicSE", "SimpleList"),
  .replace_glyco_assays
)

#' @rdname se-name-operations
#' @export
methods::setMethod("[", c("GlycomicSE", "ANY", "ANY", "ANY"), .subset_glyco_se)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "[<-",
  c("GlycomicSE", "ANY", "ANY", "SummarizedExperiment"),
  .replace_subset_glyco_se
)

#' @rdname se-name-operations
#' @export
methods::setMethod("rbind", "GlycomicSE", .rbind_glyco_se)

#' @rdname se-name-operations
#' @export
methods::setMethod("cbind", "GlycomicSE", .cbind_glyco_se)

#' @rdname se-name-operations
#' @export
methods::setMethod("names<-", "GlycoproteomicSE", .replace_glyco_names)

#' @rdname se-name-operations
#' @export
methods::setMethod("rowData<-", "GlycoproteomicSE", .replace_glyco_rowdata)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "colData<-",
  c("GlycoproteomicSE", "DataFrame"),
  .replace_glyco_coldata
)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "colData<-",
  c("GlycoproteomicSE", "NULL"),
  .replace_glyco_coldata
)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "assays<-",
  c("GlycoproteomicSE", "list"),
  .replace_glyco_assays
)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "assays<-",
  c("GlycoproteomicSE", "SimpleList"),
  .replace_glyco_assays
)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "[",
  c("GlycoproteomicSE", "ANY", "ANY", "ANY"),
  .subset_glyco_se
)

#' @rdname se-name-operations
#' @export
methods::setMethod(
  "[<-",
  c("GlycoproteomicSE", "ANY", "ANY", "SummarizedExperiment"),
  .replace_subset_glyco_se
)

#' @rdname se-name-operations
#' @export
methods::setMethod("rbind", "GlycoproteomicSE", .rbind_glyco_se)

#' @rdname se-name-operations
#' @export
methods::setMethod("cbind", "GlycoproteomicSE", .cbind_glyco_se)
