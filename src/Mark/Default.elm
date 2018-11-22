module Mark.Default exposing (default)

{-|

@docs default

-}

import Element
import Mark.Custom


default =
    Mark.Custom.block "Header"
        (\onOff ->
            \model ->
                if onOff then
                    Element.text "on"

                else
                    Element.text "off"
        )
        Mark.Custom.bool
