-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import GHC.Generics
import Text.Read (readMaybe)
import Syokugan (solve)

-- | Type synonym for an application model
data Model = Model {
  n :: Int
  , s :: Int
  , r :: Int
  , result :: Double
  } deriving (Show, Generic, Eq)

-- | Sum type for application events
data Action
  = NoOp
  | SayHelloWorld
  | UpdateN Int
  | UpdateS Int
  | UpdateR Int
  | Calc
  deriving (Show, Eq)

initModel = Model {n = 0, s = 0, r = 0, result = 0.0}

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = initModel            -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only applies to `miso` function)

factorial :: Int -> Int
factorial n = product [1..n]

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Calc m@Model{..} =
  noEff m {result = 100.0 * result'}
    where
      result' = fromRational $ solve nn ss rr nn
      nn = toInteger n
      ss = toInteger s
      rr = toInteger r

updateModel action m@Model{..} =
  case action of
    NoOp
      -> noEff m 
    SayHelloWorld
      -> m <# do consoleLog "Hello World" >> pure NoOp
    UpdateN n'
      -> noEff m {n = n'}
    UpdateS s'
      -> noEff m {s = s'}
    UpdateR r'
      -> noEff m {r = r'}

readStrAsInt action s = case readMaybe (fromMisoString s) of
                          Just n -> action n
                          Nothing -> NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model{..} = 
  div_ [] [
    header_ [] [
      h1_ [] ["ランダムパック販売の確率計算するやつ"]
    ]
    , p_ [] [
      text "KAMITSUBAKI STUDIOの"
      , a_ [
          href_ "https://findmestore.thinkr.jp/products/kts-300-0245?_pos=50&_sid=126beefb6&_ss=r"
        ] ["この商品"]
      , text "のように、18種類の商品を3個パック単位で販売している場合などの購入するパック数とコンプリートする確率が気になったので作りました。ご自由にお使い下さい。KAMITSUBAKI STUDIOの場合は1パック内で種類が重複している例を見ていないため、その分補正した計算式になっています。詳しくは以下の解説ページをご覧ください。"
    ]
    , p_ [] [
      a_ [
        href_ "https://zenn.dev/4ergfbv547uezdf/articles/da849d2dd2e8a9"
      ] ["解説ページ(zenn)"]
    ]
    , p_ [] [
      a_ [
        href_ "https://github.com/yosukeueda33/syokugan"
      ] ["ソースコード(github)"]
    ]
    , p_ [] [
      text "種類数(例:18):"
      , input_ [
          type_ "number"
        , min_ "1"
        , onInput $ readStrAsInt UpdateN
        ]
    ]
    , p_ [] [
      text "パックに封入されている個数(例:3):"
      , input_ [
          type_ "number"
        , min_ "1"
        , onInput $ readStrAsInt UpdateS
        ]
    ]
    , p_ [] [
      text "購入するパック数(例:15):"
      , input_ [
          type_ "number"
        , min_ "1"
        , onInput $ readStrAsInt UpdateR
        ]
    ]
    , p_ [] [
      button_ [ onClick Calc ] [ text "計算する" ]
    ]
    , p_ [] [
      text "コンプリートする確率[%]:", text (ms result)
    ]
  ]
