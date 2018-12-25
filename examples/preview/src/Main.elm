module Main exposing (main)

{-| -}

import Document
import Element
import Element.Font as Font
import Preview


main =
    Preview.program Document.document
        (\view ->
            Element.layout
                [ Font.family [ Font.typeface "EB Garamond" ]
                ]
                (view ())
        )
