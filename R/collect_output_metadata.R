#' Collect reproducibility metadata for a generated output
#'
#' Gathers provenance and environment metadata for any generated artefact
#' (figure, table, file). Designed for auditable exports (TikZ, CSV, etc.).
#'
#' @param output_path Optional path to the generated file (used for hashing).
#' @param project_root Optional project root; if NULL, inferred via heuristics
#'   (here::here(), Git root, or working directory).
#' @param include_session Logical; include compact session info (R version, OS).
#' @param include_packages Logical; include attached package versions (can be verbose).
#' @param extra Optional named list of extra fields to append (e.g., params).
#'
#' @return A named list of metadata fields (character scalars).
#' @export
collect_output_metadata <- function(
  output_path = NULL,
  project_root = NULL,
  include_session = TRUE,
  include_packages = FALSE,
  extra = NULL
) {
  norm <- function(p) {
    if (is.null(p) || !nzchar(p)) {
      NA_character_
    } else {
      normalizePath(p, winslash = "/", mustWork = FALSE)
    }
  }

  # tiny helper for NULL-coalescing
  `%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && nzchar(a)) a else b

  # -- Project root ----------------------------------------------------------
  root <- if (!is.null(project_root)) {
    norm(project_root)
  } else {
    pr <- tryCatch(
      if (requireNamespace("here", quietly = TRUE)) here::here(),
      error = function(e) ""
    )
    if (!nzchar(pr)) {
      pr <- tryCatch(
        system("git rev-parse --show-toplevel", intern = TRUE),
        error = function(e) ""
      )
    }
    if (!nzchar(pr)) {
      pr <- getwd()
    }
    norm(pr)
  }

  # -- Script path (absolute + relative to project root) ---------------------
  script_abs <- tryCatch(get_script_path(), error = function(e) NA_character_)
  script_rel <- tryCatch(
    {
      if (!is.na(script_abs)) {
        if (requireNamespace("fs", quietly = TRUE)) {
          fs::path_rel(script_abs, start = root)
        } else {
          sub(
            paste0(
              "^",
              gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", root),
              "/?"
            ),
            "",
            script_abs
          )
        }
      } else {
        NA_character_
      }
    },
    error = function(e) NA_character_
  )

  # -- Git status (or OneDrive note) ----------------------------------------
  is_onedrive <- function(p) {
    env_roots <- unique(na.omit(c(
      Sys.getenv("OneDrive"),
      Sys.getenv("OneDriveCommercial"),
      Sys.getenv("OneDriveConsumer"),
      Sys.getenv("OneDriveBusiness")
    )))
    any(
      nzchar(env_roots) &
        sapply(env_roots, function(r) startsWith(tolower(p), tolower(norm(r))))
    ) ||
      grepl("onedrive", p, ignore.case = TRUE)
  }

  git_branch <- git_commit <- git_status <- NA_character_
  # Try Git first
  git_top <- tryCatch(
    system("git rev-parse --show-toplevel", intern = TRUE),
    error = function(e) ""
  )
  if (
    nzchar(git_top) &&
      startsWith(
        normalizePath(root, winslash = "/"),
        normalizePath(git_top, winslash = "/")
      )
  ) {
    git_branch <- tryCatch(
      system("git rev-parse --abbrev-ref HEAD", intern = TRUE),
      error = function(e) NA_character_
    )
    git_commit <- tryCatch(
      system("git rev-parse --short HEAD", intern = TRUE),
      error = function(e) NA_character_
    )
    dirty <- tryCatch(
      system("git status --porcelain", intern = TRUE),
      error = function(e) character()
    )
    git_status <- if (length(dirty) == 0) "clean" else "dirty"
  } else {
    # Not a Git repo
    if (is_onedrive(root)) {
      git_branch <- "not tracked (OneDrive sync)"
      git_commit <- "not tracked (OneDrive sync)"
      git_status <- "n/a"
    } else {
      git_branch <- "not a Git repository"
      git_commit <- "not a Git repository"
      git_status <- "n/a"
    }
  }

  # -- Session / environment -------------------------------------------------
  si <- if (include_session) {
    os <- paste(Sys.info()[c("sysname", "release")], collapse = " ")
    list(
      r_version = as.character(getRversion()),
      os = os,
      user = Sys.info()[["user"]],
      host = Sys.info()[["nodename"]],
      datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
  } else {
    list()
  }

  pkgs <- if (include_packages) {
    # attached packages + versions
    si0 <- utils::sessionInfo()
    at <- si0$otherPkgs
    if (is.null(at)) {
      at <- list()
    }
    paste(
      sprintf("%s=%s", names(at), vapply(at, function(x) x$Version, "")),
      collapse = "; "
    )
  } else {
    NA_character_
  }

  # -- Output file hash ------------------------------------------------------
  file_hash <- tryCatch(
    {
      if (
        !is.null(output_path) &&
          requireNamespace("digest", quietly = TRUE) &&
          file.exists(output_path)
      ) {
        digest::digest(file = output_path, algo = "sha256")
      } else {
        NA_character_
      }
    },
    error = function(e) NA_character_
  )

  # -- Assemble --------------------------------------------------------------
  meta <- c(
    list(
      project_root = root,
      script_path = script_rel %||% script_abs,
      script_path_abs = script_abs,
      output_path = norm(output_path),
      git_branch = git_branch,
      git_commit = git_commit,
      git_status = git_status,
      file_hash = file_hash
    ),
    si,
    list(attached_packages = pkgs)
  )

  if (!is.null(extra) && length(extra)) {
    stopifnot(is.list(extra), !is.null(names(extra)))
    extra_chr <- lapply(extra, function(x) {
      if (length(x) <= 1) as.character(x) else paste(x, collapse = ", ")
    })
    meta <- c(meta, extra_chr)
  }

  # Coerce everything to single-length character
  meta <- lapply(meta, function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x)[1]
  })
  return(meta)
}
