context("Test zzz")

test_that("onLoad", {
    setup(require(aseshms))
    expect_equal(Sys.getlocale("LC_CTYPE"), "Chinese (Simplified)_China.936")
    expect_true(all(c("op", "A", "EE", "END_TAG", "XM_PI", "START_TAG", "CHAR2NUM_SCI",
                      "CHAR2NUM_NOSCI") %in% names(aseshms_env)))
    expect_equal(names(.pkgPara()), 
                 c("init.dir", "toolkit.dir", "init.pal", "na.string",
                   "mach.arch", "remote.pkg.dir", "aseshms.version",
                   "aseshms.latest.version", "guiToolkit" ))
    teardown(unload_pkgs(aseshms))
})

test_that("on Unload", {
    setup({
        unload_pkgs("aseshms")
        expect_false(file.exists(paste0(Sys.getenv("HOME"), "/acckey.pem")))
    })
    teardown(library(aseshms))
})