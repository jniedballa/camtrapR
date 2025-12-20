
# --- Helper to create mock TSN objects ---
# Mimics taxize::as.tsn behavior:
# 1. Preserves input length (including 0 for empty vectors)
# 2. Sets class to "tsn"
# 3. Sets "uri" attribute (required by checkSpeciesNames)
fake_as_tsn_impl <- function(x, check = FALSE) {
  if (length(x) == 0) {
    res <- character(0)
    class(res) <- "tsn"
    attr(res, "uri") <- character(0)
    return(res)
  }
  res <- x
  class(res) <- "tsn"
  # Provide a dummy URI for every element
  attr(res, "uri") <- rep("http://itis.gov/mock", length(x))
  return(res)
}

test_that("Input validation works", {
  
  testthat::skip_if_not_installed("taxize")
  
  expect_error(checkSpeciesNames(speciesNames = "Cat", searchtype = "invalid"),
               "should be one of")
  
  expect_error(checkSpeciesNames(speciesNames = 123, searchtype = "common"),
               "is.character")
  expect_error(checkSpeciesNames(speciesNames = "Cat", searchtype = "common", accepted = "yes"),
               "is.logical")
})

test_that("Happy Path: Valid Scientific Name found", {
  
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("taxize")
  
  # --- Mocks ---
  # get_tsn returns a TSN object
  m_get_tsn <- mockery::mock(fake_as_tsn_impl("123456"))
  
  m_sci_name <- mockery::mock(data.frame(tsn = "123456", combinedname = "Panthera leo", stringsAsFactors = FALSE))
  m_com_name <- mockery::mock(data.frame(tsn = "123456", commonName = "Lion", stringsAsFactors = FALSE))
  m_author   <- mockery::mock(data.frame(tsn = "123456", authorship = "Linnaeus", stringsAsFactors = FALSE))
  m_rank     <- mockery::mock(data.frame(tsn = "123456", rankname = "Species", stringsAsFactors = FALSE))
  
  # --- Stubs ---
  mockery::stub(checkSpeciesNames, "taxize::get_tsn", m_get_tsn)
  mockery::stub(checkSpeciesNames, "taxize::as.tsn", fake_as_tsn_impl)
  mockery::stub(checkSpeciesNames, "ritis::scientific_name", m_sci_name)
  mockery::stub(checkSpeciesNames, "ritis::common_names", m_com_name)
  mockery::stub(checkSpeciesNames, "ritis::taxon_authorship", m_author)
  mockery::stub(checkSpeciesNames, "ritis::rank_name", m_rank)
  
  # --- Execution ---
  result <- checkSpeciesNames(speciesNames = "Panthera leo", 
                              searchtype = "scientific", 
                              accepted = TRUE)
  
  # --- Assertions ---
  expect_equal(nrow(result), 1)
  expect_equal(result$tsn, 123456)
  expect_equal(result$scientificName, "Panthera leo")
  expect_equal(result$commonName, "Lion")
  expect_equal(result$taxon_status, "valid") 
  expect_false(is.na(result$itis_url))
})

test_that("Logic: Multiple Common Names are collapsed", {
  
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("taxize")
  
  # --- Mocks ---
  m_get_tsn <- mockery::mock(fake_as_tsn_impl("123"))
  
  m_sci_name <- mockery::mock(data.frame(tsn = "123", combinedname = "Simba", stringsAsFactors = FALSE))
  m_com_name <- mockery::mock(data.frame(tsn = c("123", "123"), 
                                commonName = c("NameA", "NameB"), 
                                stringsAsFactors = FALSE))
  m_author   <- mockery::mock(data.frame(tsn = "123", authorship = "Disney", stringsAsFactors = FALSE))
  m_rank     <- mockery::mock(data.frame(tsn = "123", rankname = "Species", stringsAsFactors = FALSE))
  
  # --- Stubs ---
  mockery::stub(checkSpeciesNames, "taxize::get_tsn", m_get_tsn)
  mockery::stub(checkSpeciesNames, "taxize::as.tsn", fake_as_tsn_impl)
  mockery::stub(checkSpeciesNames, "ritis::scientific_name", m_sci_name)
  mockery::stub(checkSpeciesNames, "ritis::common_names", m_com_name)
  mockery::stub(checkSpeciesNames, "ritis::taxon_authorship", m_author)
  mockery::stub(checkSpeciesNames, "ritis::rank_name", m_rank)
  
  # --- Execution ---
  result <- checkSpeciesNames("Simba", "common")
  
  # --- Assertions ---
  sep <- .Platform$file.sep
  expected_common <- paste("NameA", "NameB", sep = sep)
  expect_equal(result$commonName, expected_common)
})

test_that("Error Handling: No Matches Found (All NA)", {
  
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("taxize")
  
  # --- Mocks ---
  # taxize::get_tsn returns NA
  m_get_tsn <- mockery::mock(NA)
  
  # Important: When checkSpeciesNames gets NA, it filters the list.
  # The resulting list is empty. fake_as_tsn_impl correctly returns length 0.
  # This prevents the loop from running and calling ritis functions with bad data.
  
  mockery::stub(checkSpeciesNames, "taxize::get_tsn", m_get_tsn)
  mockery::stub(checkSpeciesNames, "taxize::as.tsn", fake_as_tsn_impl)
  
  # --- Execution & Assertion ---
  # Since the filtered list is empty, the function hits the `else { stop(...) }` block
  expect_error(
    suppressWarnings(checkSpeciesNames("Unicorn", "common")),
    "found no TSNs for speciesNames"
  )
})

test_that("Partial Success: One match, One mismatch", {
  
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("taxize")
  
  input_names <- c("Lion", "Unicorn")
  
  # --- Mocks ---
  # taxize::get_tsn returns vector: "123" for Lion, NA for Unicorn
  tsn_raw <- c("123", NA)
  m_get_tsn <- mockery::mock(tsn_raw)
  
  # Mock ritis functions (only called for valid TSN "123")
  m_sci_name <- mockery::mock(data.frame(tsn = "123", combinedname = "Leo", stringsAsFactors = FALSE))
  m_com_name <- mockery::mock(data.frame(tsn = "123", commonName = "Lion", stringsAsFactors = FALSE))
  m_author   <- mockery::mock(data.frame(tsn = "123", authorship = "L.", stringsAsFactors = FALSE))
  m_rank     <- mockery::mock(data.frame(tsn = "123", rankname = "Spp", stringsAsFactors = FALSE))
  
  mockery::stub(checkSpeciesNames, "taxize::get_tsn", m_get_tsn)
  mockery::stub(checkSpeciesNames, "taxize::as.tsn", fake_as_tsn_impl)
  mockery::stub(checkSpeciesNames, "ritis::scientific_name", m_sci_name)
  mockery::stub(checkSpeciesNames, "ritis::common_names", m_com_name)
  mockery::stub(checkSpeciesNames, "ritis::taxon_authorship", m_author)
  mockery::stub(checkSpeciesNames, "ritis::rank_name", m_rank)
  
  # --- Execution ---
  expect_warning(
    result <- checkSpeciesNames(input_names, "common"),
    "found no matches for 1 name"
  )
  
  # --- Assertions ---
  expect_equal(nrow(result), 2)
  
  # Check Lion row
  lion_row <- result[result$user_name == "Lion", ]
  expect_equal(lion_row$tsn, 123)
  
  # Check Unicorn row
  unicorn_row <- result[result$user_name == "Unicorn", ]
  expect_true(is.na(unicorn_row$tsn))
})

test_that("Error Handling: taxize network error", {
  
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("taxize")
  
  # --- Mock ---
  err <- structure("Network Error", class = "try-error")
  m_get_tsn <- mockery::mock(err)
  
  mockery::stub(checkSpeciesNames, "taxize::get_tsn", m_get_tsn)
  
  # --- Execution ---
  expect_message(
    result <- checkSpeciesNames("Lion", "common"),
    "error in get_tsn"
  )
  
  expect_null(result)
})

test_that("Logic: Accepted = FALSE fetches usage rating", {
  
  testthat::skip_if_not_installed("mockery")
  testthat::skip_if_not_installed("taxize")
  
  # --- Mocks ---
  m_get_tsn <- mockery::mock(fake_as_tsn_impl("999"))
  
  m_sci_name <- mockery::mock(data.frame(tsn = "999", combinedname = "OldName", stringsAsFactors = FALSE))
  m_com_name <- mockery::mock(data.frame()) 
  m_author   <- mockery::mock(data.frame()) 
  m_rank     <- mockery::mock(data.frame())
  m_core_meta <- mockery::mock(data.frame(tsn = "999", taxonUsageRating = "invalid", stringsAsFactors = FALSE))
  
  mockery::stub(checkSpeciesNames, "taxize::get_tsn", m_get_tsn)
  mockery::stub(checkSpeciesNames, "taxize::as.tsn", fake_as_tsn_impl)
  mockery::stub(checkSpeciesNames, "ritis::scientific_name", m_sci_name)
  mockery::stub(checkSpeciesNames, "ritis::common_names", m_com_name)
  mockery::stub(checkSpeciesNames, "ritis::taxon_authorship", m_author)
  mockery::stub(checkSpeciesNames, "ritis::rank_name", m_rank)
  mockery::stub(checkSpeciesNames, "ritis::core_metadata", m_core_meta)
  
  # --- Execution ---
  result <- checkSpeciesNames("OldName", "scientific", accepted = FALSE)
  
  # --- Assertions ---
  expect_equal(result$tsn, 999)
  expect_equal(result$taxonUsageRating, "invalid")
})
