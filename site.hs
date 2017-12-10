--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "img/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "imginfo/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/et-book/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "contact.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "about.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= applyAsTemplate defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return (take 10 posts)) `mappend`
                    constField "title" "home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    version "redirects" $ createRedirects preHakyllLinks


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


preHakyllLinks =
    [ ("2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.html", "posts/2016-06-22_parsing_text_with_erlang_pattern_matching_and_guards.html")
    , ("2016-07-06_what_is_cqrs.html", "posts/2016-07-06_what_is_cqrs.html")
    , ("2016-07-20_my_first_erlang_patch.html", "posts/2016-07-20_my_first_erlang_patch.html")
    , ("2016-08-03_the_essence_of_otp.html", "posts/2016-08-03_the_essence_of_otp.html")
    , ("2016-08-17_erlang_error_handling_primitives.html", "posts/2016-08-17_erlang_error_handling_primitives.html")
    , ("2016-08-31_why_so_many_lambda_tshirts.html", "posts/2016-08-31_why_so_many_lambda_tshirts.html")
    , ("2016-09-14_simple_gen_event_example.html", "posts/2016-09-14_simple_gen_event_example.html")
    , ("2016-10-27_cqrs_versus_oop.html", "posts/2016-10-27_cqrs_versus_oop.html")
    , ("2016-11-09_a_simple_erlang_application.html", "posts/2016-11-09_a_simple_erlang_application.html")
    , ("2016-11-23_a_simple_erlang_application_with_prometheus.html", "posts/2016-11-23_a_simple_erlang_application_with_prometheus.html")
    , ("2016-12-07_a_closure_with_erlang.html", "posts/2016-12-07_a_closure_with_erlang.html")
    , ("2016-12-21_a_production_gen_event_application.html", "posts/2016-12-21_a_production_gen_event_application.html")
    , ("2017-01-04_how_to_return_json_from_an_erlang_web_service.html", "posts/2017-01-04_how_to_return_json_from_an_erlang_web_service.html")
    , ("2017-01-18_how_to_read_utf8_encoded_file_with_erlang.html", "posts/2017-01-18_how_to_read_utf8_encoded_file_with_erlang.html")
    , ("2017-02-01_so_long_erlang_its_been_great.html", "posts/2017-02-01_so_long_erlang_its_been_great.html")
    , ("2017-02-21_hello_elm_and_haskell.html", "posts/2017-02-21_hello_elm_and_haskell.html")
    , ("2017-03-07_how_to_build_snap_with_stack.html", "posts/2017-03-07_how_to_build_snap_with_stack.html")
    , ("2017-03-22_some_cqrs_notes.html", "posts/2017-03-22_some_cqrs_notes.html")
    , ("2017-04-05_haskell_on_alpine_linux.html", "posts/2017-04-05_haskell_on_alpine_linux.html")
    , ("2017-04-19_haskell_operators.html", "posts/2017-04-19_haskell_operators.html")
    ]
