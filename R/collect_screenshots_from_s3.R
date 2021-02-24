#' Bulk-download (and compress) screenshots stored on S3
#'
#' Download screenshots previously stored on S3 through ScrapeBot instances. The function will collect those cases in the data that refer to S3-stored screenshots and download them to a local output directory. While doing so, the funtion can also resize images to save local disk space.
#'
#' @param scrapebot_connection A connection object, as retrieved from [connect_scrapebot()].
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#' @param run_uid Optional numeric UID or a vector of numeric UIDs of a specific run to collect data from. If `NULL`, either `instance_uid` or `recipe_uid` (or both) has to be provided. Defaults to `NULL`.
#' @param instance_uid Optional numeric UID or a vector of numeric UIDs of the instance to filter data for. If `NULL`, either `run_uid` or `recipe_uid` (or both) has to be provided. Defaults to `NULL`.
#' @param recipe_uid Optional numeric UID or a vector of numeric UIDs of the recipe to filter data for. If `NULL`, either `instance_uid` or `run_uid` (or both) has to be provided. Defaults to `NULL`.
#' @param include_inactive If `TRUE`, inactive recipes are included along active recipes; defaults to `FALSE`.
#' @param resize If `TRUE`, downloaded screenshots will be resized keeping their aspect ratio to the maximum width/height as provided. Use for file-size compression.
#' @param resize_max_width Integer indicating the maximum width images should be resized to (if `resize` is `TRUE`). Aspect ratio with `resize_max_height` will be respected.
#' @param resize_max_height Integer indicating the maximum height images should be resized to (if `resize` is `TRUE`). Aspect ratio with `resize_max_width` will be respected.
#' @param output_directory Character string holding the (relative) path to the directory into which the screenshot files should be downloaded.
#' @param verbose If `TRUE`, the function will show download progress bar to indicate how far it has come.
#'
#' @return A [tibble][tibble::tibble-package] listing all matching run-data entries according to which screenshots should be found on S3. As such, it contains the same amount of rows as received from `get_run_data` when filtering for the respective parameters and S3 links for "screenshot"-containing recipe steps (i.e., `get_recipes` first, then `get_recipe_steps` and filter for "screenshot," then `get_run_data` and filter for S3 links). For each row, then, the local filename, width, height, and filesize (in bytes) as well as their respective counterparts on S3 (note that, without resizing, width/height/filesize should be practically the same).
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' collect_screenshots_from_s3(
#'   scrapebot_connection, aws_connection,
#'   run_uid = 42
#' )
#' collect_screenshots_from_s3(
#'   scrapebot_connection, aws_connection,
#'   run_uid = 42,
#'   resize = TRUE, resize_max_width = 800
#' )
#' collect_screenshots_from_s3(
#'   scrapebot_connection, aws_connection,
#'   run_uid = 42,
#'   output_directory = 'download_dir/'
#' )
#' disconnect(connection)
#' }
#'
#' @importFrom magrittr %>%
#' @export
collect_screenshots_from_s3 <- function(scrapebot_connection,
                                        aws_connection,
                                        run_uid = NULL, instance_uid = NULL, recipe_uid = NULL,
                                        include_inactive = FALSE,
                                        resize = FALSE,
                                        resize_max_width = NULL, resize_max_height = NULL,
                                        output_directory = '',
                                        verbose = TRUE) {

  # Test input
  if(is.null(scrapebot_connection$db)) {
    stop('Connection needs to be a valid ScrapeBot connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(!is.character(aws_connection$s3_bucket)) {
    stop('AWS connection needs to be set up, initiated through ScrapeBotR::connect_aws.')
  }
  if(is.null(run_uid) & is.null(instance_uid) & is.null(recipe_uid)) {
    stop('Either run_uid or instance_uid or recipe_uid (or a combination thereof) need(s) to be provided.')
  }
  if(output_directory != '' & !stringr::str_detect(output_directory, '.+/$')) {
    output_directory <- paste0(output_directory, '/')
  }

  # Find recipes
  recipes <- get_recipes(scrapebot_connection, instance_uid, include_inactive)
  if(!is.null(recipe_uid)) {
    recipes <-
      recipes %>%
      dplyr::filter(uid %in% c(recipe_uid))
  }
  if(nrow(recipes) == 0) {
    warning('No recipes match your combined criteria of instance_uid, recipe_uid, and include_inactive.')
    return(tibble::tibble(
      created = as.POSIXct(character()),
      run_uid = integer(),
      instance_uid = integer(),
      recipe_uid = integer(),
      step_uid = integer(),
      filename = character(),
      width = integer(),
      height = integer(),
      filesize = integer(),
      filename_s3 = character(),
      width_s3 = integer(),
      height_s3 = integer(),
      filesize_s3 = integer()
    ))
  }

  # Identify recipe steps with screenshots
  steps <-
    get_recipe_steps(scrapebot_connection, recipes$uid, include_inactive) %>%
    dplyr::filter(stringr::str_detect(type, stringr::fixed('screenshot')))
  if(nrow(steps) == 0) {
    warning('No recipe steps match your combined criteria of instance_uid, recipe_uid, and include_inactive.')
    return(tibble::tibble(
      created = as.POSIXct(character()),
      run_uid = integer(),
      instance_uid = integer(),
      recipe_uid = integer(),
      step_uid = integer(),
      filename = character(),
      width = integer(),
      height = integer(),
      filesize = integer(),
      filename_s3 = character(),
      width_s3 = integer(),
      height_s3 = integer(),
      filesize_s3 = integer()
    ))
  }

  # Find data of respective steps
  data <-
    get_run_data(scrapebot_connection, run_uid, instance_uid, recipes$uid, steps$uid) %>%
    dplyr::filter(stringr::str_detect(value, '^s3:\\/\\/.+$'))
  if(nrow(data) == 0) {
    warning('No data matches your combined criteria of run_uid, instance_uid, recipe_uid, and include_inactive.')
    return(tibble::tibble(
      created = as.POSIXct(character()),
      run_uid = integer(),
      instance_uid = integer(),
      recipe_uid = integer(),
      step_uid = integer(),
      filename = character(),
      width = integer(),
      height = integer(),
      filesize = integer(),
      filename_s3 = character(),
      width_s3 = integer(),
      height_s3 = integer(),
      filesize_s3 = integer()
    ))
  }

  if(verbose) {
    print(paste0(nrow(data), ' images identified in ScrapeBot data'))
  }

  # Download (and resize) images
  s3_region <- NULL
  s3_buckets <- aws.s3::bucketlist(add_region = TRUE, key = aws_connection$aws_access_key, secret = aws_connection$aws_secret)
  if(is.data.frame(s3_buckets)) {
    s3_buckets <-
      tibble::as_tibble(s3_buckets) %>%
      dplyr::filter(Bucket == aws_connection$s3_bucket)
    if(nrow(s3_buckets) == 1) {
      s3_region <- s3_buckets[[1, 'Region']]
    }
  }
  if(is.null(s3_region)) {
    warning('S3 bucket not found or could not be reached. Double-check aws_connection (connect_aws()) and particularly its region.')
    return(tibble::tibble(
      created = as.POSIXct(character()),
      run_uid = integer(),
      instance_uid = integer(),
      recipe_uid = integer(),
      step_uid = integer(),
      filename = character(),
      width = integer(),
      height = integer(),
      filesize = integer(),
      filename_s3 = character(),
      width_s3 = integer(),
      height_s3 = integer(),
      filesize_s3 = integer()
    ))
  }

  images <- NULL
  n_images <- nrow(data)
  n_images_10p <- floor(n_images*.1)
  for(i in 1:n_images) {
    s3_head <- aws.s3::head_object(data[[i, 'value']], key = aws_connection$aws_access_key, secret = aws_connection$aws_secret, region = s3_region)
    if(!s3_head) {
      images <-
        images %>%
        dplyr::bind_rows(
          tibble::tibble(
            created = data[[i, 'created']],
            run_uid = data[[i, 'run_uid']],
            instance_uid = data[[i, 'instance_uid']],
            recipe_uid = data[[i, 'recipe_uid']],
            step_uid = data[[i, 'step_uid']],
            filename = NA_character_,
            width = NA_integer_,
            height = NA_integer_,
            filesize = NA_integer_,
            filename_s3 = NA_character_,
            width_s3 = NA_integer_,
            height_s3 = NA_integer_,
            filesize_s3 = NA_integer_
          )
        )
    } else {
      image_s3 <- aws.s3::get_object(data[[i, 'value']],
                                     key = aws_connection$aws_access_key, secret = aws_connection$aws_secret, region = s3_region,
                                     show_progress = verbose)
      image_filename <- stringr::str_remove(data[[i, 'value']], stringr::fixed(paste0('s3://', aws_connection$s3_bucket, '/')))

      image_magick <- magick::image_read(image_s3)
      image_info <- magick::image_info(image_magick)

      if(resize) {
        image_rescaled <- magick::image_scale(image_magick, paste0(resize_max_width, 'x', resize_max_height))
      } else {
        image_rescaled <- image_magick
      }

      image_rescaled_path <- magick::image_write(image_rescaled, path = paste0(output_directory, image_filename))
      image_rescaled_read <- magick::image_read(image_rescaled_path)
      image_info_rescaled <- magick::image_info(image_rescaled_read)

      images <-
        images %>%
        dplyr::bind_rows(
          tibble::tibble(
            created = data[[i, 'created']],
            run_uid = data[[i, 'run_uid']],
            instance_uid = data[[i, 'instance_uid']],
            recipe_uid = data[[i, 'recipe_uid']],
            step_uid = data[[i, 'step_uid']],
            filename = image_filename,
            width = image_info_rescaled[[1, 'width']],
            height = image_info_rescaled[[1, 'height']],
            filesize = image_info_rescaled[[1, 'filesize']],
            filename_s3 = data[[i, 'value']],
            width_s3 = image_info[[1, 'width']],
            height_s3 = image_info[[1, 'height']],
            filesize_s3 = image_info[[1, 'filesize']]
          )
        )
    }

    if(verbose) {
      if(n_images_10p > 0 & i %% n_images_10p == 0) {
        print(paste0(i, '/', n_images, ' images done (', round(100*i/n_images), '%)'))
      }
    }
  }
  return(images)
}
