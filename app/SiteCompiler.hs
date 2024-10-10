{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Hakyll.Core.Configuration
import           System.FilePath (joinPath, splitPath)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do

    -- All our "just serve these files" content.
    match "site/static/**" $ do
        route   (dropInitialComponents 2)
        compile copyFileCompiler

    match "site/template/**" $ compile templateBodyCompiler

    -- Top-level stand-alone pages
    match ("site/pages/*.md" .&&. complement "site/pages/index.md") $ do
        route   $ composeRoutes
                    (dropInitialComponents 2) (setExtension "html")
        compile $ do
            pandocCompiler >>= loadAndApplyTemplate
                "site/template/default.tmpl" defaultContext

    -- Draft posts are under a separate path component and have no date.
    match "site/pages/draft/*.md" $ do
        route   $ composeRoutes
                    (dropInitialComponents 2) (setExtension "html")
        compile $ do
            -- XXX wrong template, but good enough for the moment
            pandocCompiler >>= loadAndApplyTemplate
                "site/template/default.tmpl" defaultContext

    match "site/pages/index.md" $ do
        route   $ composeRoutes
                    (dropInitialComponents 2) (setExtension "html")
        compile $ do
            pandocCompiler >>= loadAndApplyTemplate
                "site/template/default.tmpl" defaultContext

    where
        config = defaultConfiguration
            { storeDirectory = ".build/hakyll/cache"
            , destinationDirectory = ".build/site"
            }

--------------------------------------------------------------------------------
-- Custom code for our site

-- | Drop the given number of leading path components
dropInitialComponents :: Int -> Routes
dropInitialComponents n = customRoute $
    joinPath . drop n . splitPath . toFilePath

dropInitialComponent = dropInitialComponents 1
