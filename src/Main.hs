{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
    ( main
    ) where

import qualified Data.Map     as M
import qualified Data.Set     as S
import           Data.Text    (Text)
import           Miso
import           Miso.String
import           Text.Madlibs (madFile, run)

randomText :: IO Text
randomText = run $(madFile "mad-src/hot-takes.mad")

type Model = Text

data Action
  = Regenerate
  | Write Text
  | NoOp
  deriving (Show, Eq)

main :: IO ()
main = startApp App {..}
  where
    mountPoint = Nothing
    initialAction = NoOp
    model  = ""
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ keyboardSub keypress ]

backgroundStyle :: [Attribute action]
backgroundStyle = [ style_ $ M.fromList [("color", "#4d4d4d"), ("margin-left", "15%"), ("margin-top", "15%") ] ]

largeFont :: [Attribute action]
largeFont = [ style_ $ M.fromList [("font", "20px \"Comic Sans MS\", Helvetica, sans-serif")] ]

buttonFont :: [Attribute action]
buttonFont = [ style_ $ M.fromList [("font", "50px \"Comic Sans MS\", Helvetica, sans-serif")] ]

buttonTraits :: [Attribute action]
buttonTraits = class_ "button" : buttonFont

fontStyles :: [Attribute action]
fontStyles = [ style_ $ M.fromList [("font", "30px \"Comic Sans MS\", Helvetica, sans-serif")] ]

updateModel :: Action -> Model -> Effect Action Model
updateModel Regenerate m = m <# fmap Write randomText
updateModel (Write t) _  = noEff t
updateModel NoOp m       = noEff m

keypress :: S.Set Int -> Action
keypress keys = if 82 `elem` S.toList keys then Regenerate else NoOp

viewModel :: Model -> View Action
viewModel x = div_ backgroundStyle
    [
      p_ largeFont [ text "Press 'more' or hit 'r' for another hot-takes" ]
    , p_ [] [ div_ (onClick Regenerate : buttonTraits) [ text "more" ] ]
    , p_ fontStyles [ text (toMisoString x) ]
    , p_ [] [ footer ]
    ]

footerParagraph :: [Attribute action]
footerParagraph = [ style_ $ M.fromList [("align", "bottom"), ("position", "absolute"), ("bottom", "200px")] ]

footer :: View Action
footer = footer_ [ class_ "info" ]
    [ p_ footerParagraph
        [ a_ [ href_ "https://github.com/vmchale/hot-takes" ] [ text "source" ] ] ]
