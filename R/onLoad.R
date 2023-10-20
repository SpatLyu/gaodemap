.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) {
        v = packageVersion("gaodemap")
        message('gaodeumap ', v)
        message(Notification)
    }
}

Notification <- paste('Apply an application from here: https://lbs.amap.com/api/webservice/guide/create-project/get-key',
                      "Then register you key by running `options(gaode.key = 'xxx')`",
                      sep = '\n')
