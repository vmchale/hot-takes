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

type Model = MisoString

data Action
  = Regenerate
  | Write MisoString
  | NoOp

main :: IO ()
main = startApp App {..}
  where
    mountPoint = Nothing
    initialAction = NoOp
    model  = mempty
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ keyboardSub keypress ]

backgroundStyle :: [Attribute action]
backgroundStyle = [ style_ $ M.fromList [("color", "#4d4d4d"), ("margin-left", "15%"), ("margin-top", "15%") ] ]

mkFont :: Word -> [Attribute action]
mkFont i = [ style_ $ M.fromList [("font", toMisoString (show i) <> "px \"Comic Sans MS\", Helvetica, sans-serif")] ]

largeFont :: [Attribute action]
largeFont = mkFont 20

fontStyles :: [Attribute action]
fontStyles = mkFont 30

buttonTraits :: [Attribute action]
buttonTraits = class_ "button" : buttonFont
    where buttonFont = mkFont 50

updateModel :: Action -> Model -> Effect Action Model
updateModel Regenerate m = m <# fmap (Write . toMisoString) randomText
updateModel (Write t) _  = noEff t
updateModel NoOp m       = noEff m

keypress :: S.Set Int -> Action
keypress keys =
    if 82 `elem` S.toList keys
        then Regenerate
        else NoOp

viewModel :: Model -> View Action
viewModel x = div_ backgroundStyle
    [
      p_ largeFont [ text "Press 'more' or hit 'r' for another hot take" ]
    , p_ [] [ div_ (onClick Regenerate : buttonTraits) [ text "more" ] ]
    , p_ fontStyles [ text x ]
    , p_ [] [ footer ]
    ]

footerParagraph :: [Attribute action]
footerParagraph = [ style_ $ M.fromList [("align", "bottom"), ("position", "absolute"), ("bottom", "200px")] ]

footer :: View Action
footer = footer_ [ class_ "info" ]
    [ p_ footerParagraph
        [ a_ [ href_ "https://github.com/vmchale/hot-takes" ] [ text "source" ] ] ]
