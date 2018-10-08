module PStyle exposing (Styles(..), Variation(..), sansSerif, stylesheet)

import Browser
import Element exposing (..)
import Element.Attributes exposing (..)
import MyColor
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition


type Styles
    = None
    | Main
    | Page
    | Logo
    | NavOption
    | Box
    | Container
    | Label


type Variation
    = Disabled


sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]


{-| First, we create a stylesheet.

Styles only deal with properties that are not related to layout, position, or size.

Generally all properties only have one allowed unit, which is usually px.

If you want to use something like em, you should check out the `Style.Scale` module, which will show how to make something similar to `em`.

-}
stylesheet : StyleSheet Styles Variation
stylesheet =
    Style.styleSheet
        [ style None [] -- It's handy to have a blank style
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text MyColor.darkCharcoal
            , Color.background MyColor.white
            , Color.border MyColor.lightGrey
            , Font.typeface sansSerif
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style Page
            [ Border.all 5
            , Border.solid
            , Color.text MyColor.darkCharcoal
            , Color.background MyColor.white
            , Color.border MyColor.lightGrey
            ]
        , style Label
            [ Font.size 25 -- set font size to 25 px
            , Font.center
            ]
        , style Logo
            [ Font.size 25
            , Font.typeface sansSerif
            ]
        , style NavOption
            [ Font.size 16
            , Font.typeface sansSerif
            ]
        , style Box
            [ Transition.all
            , Color.text MyColor.white
            , Color.background MyColor.blue
            , Color.border MyColor.blue
            , Border.rounded 3 -- round all borders to 3px
            , variation Disabled
                [ Color.background MyColor.red
                ]
            , hover
                [ Color.text MyColor.white
                , Color.background MyColor.red
                , Color.border MyColor.red
                , cursor "pointer"
                ]
            ]
        , style Container
            [ Color.text MyColor.black
            , Color.background MyColor.lightGrey
            , Color.border MyColor.lightGrey
            ]
        ]
