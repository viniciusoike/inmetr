source(here::here("data-raw", "pipeline", "parse.R"))
source(here::here("data-raw", "pipeline", "manifest.R"))

testthat::test_that("build provenance rejects dirty pipeline code", {
  root <- tempfile("provenance-")
  dir.create(file.path(root, "data-raw", "pipeline"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("initial", file.path(root, "data-raw", "build.R"))
  system2("git", c("-C", shQuote(root), "init", "-q"))
  system2("git", c("-C", shQuote(root), "add", "data-raw/build.R"))
  system2(
    "git",
    c(
      "-C",
      shQuote(root),
      "-c",
      "user.name=Test",
      "-c",
      "user.email=test@example.com",
      "commit",
      "-qm",
      "Initial"
    )
  )
  sha <- get_build_commit(root)
  testthat::expect_match(sha, "^[0-9a-f]{40}$")
  writeLines("changed", file.path(root, "data-raw", "build.R"))
  testthat::expect_error(get_build_commit(root), "uncommitted")
  testthat::expect_equal(get_build_commit(root, allow_dirty = TRUE), sha)
})
