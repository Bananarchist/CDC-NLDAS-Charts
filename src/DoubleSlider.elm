module DoubleSlider exposing (..)

import Html as Tag exposing (Html, Attribute, text)
import Html.Attributes as Hats exposing (type_, step, style, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as D exposing (Decoder)
import Basics.Extra exposing (flip)

view : 
  { max : Int
  , min : Int
  , step: Int
  , val1 : Int
  , val2 : Int
  , updateMin : Int -> msg
  , updateMax : Int -> msg
  } -> Html msg
view data =
  let
      rangeAttrs : Int -> Int -> Int -> Float -> (Int -> msg) -> List (Attribute msg)
      rangeAttrs val minimum maximum xscale msg =
        [ type_ "range"
        , step <| String.fromInt data.step
        , Hats.min <| String.fromInt minimum
        , Hats.max <| String.fromInt maximum
        , value <| String.fromInt val
        , style "width" ((String.fromFloat xscale) ++ "%")
        , style "display" "inline-block"
        , style "margin" "0"
        , on "input" (D.map msg eventDecoder)
        ]
      v1 = data.val1 |> toFloat
      v2 = data.val2 |> toFloat
      diff = v2 - v1
      v1Max = floor (diff / 2) |> (+) data.val1 |> Debug.log "v1Max"
      v2Min = ceiling (diff / 2) |> (+) data.val1 |> Debug.log "v2Min"
      maxRange = data.max - data.min |> toFloat
      percentV1 = (v1Max - data.min) * 100 |> toFloat |> flip (/) maxRange
      percentV2 = (data.max - v2Min) * 100 |> toFloat |> flip (/) maxRange
      
  in 
  Tag.div []
    [ Tag.input 
      ( rangeAttrs data.val1 data.min v1Max percentV1 data.updateMin )
      []
    , Tag.input 
      ( rangeAttrs data.val2 v2Min data.max percentV2 data.updateMax )
      []
    ]
     -- transofmr wont work because it warps the slider
     -- must use style:width%%%


eventDecoder : Decoder Int
eventDecoder =
  D.at ["event", "target", "value"] D.int 

