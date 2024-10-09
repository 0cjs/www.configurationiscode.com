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
