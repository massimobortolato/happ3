module Pages where

import Lucid.Base
import Lucid.Html5

import Coordinates
import Prelude hiding (for_)

--------------------------------------------------------------------------------
pageRoot :: Maybe Coordinates -> Html ()
pageRoot maybeCoordinates =
  template $
    h1_ [class_ "text-center"] "Convert coordinates to UTM"
      <> (resultContents maybeCoordinates)
      <> form_
        [action_ "", method_ "get", class_ "row g-3"]
        ( div_
            [class_ "col-md-6"]
            ( label_ [for_ latitude_, class_ "form-label"] latitudeLabel
                <> input_ [type_ "number", name_ latitude_, class_ "form-control"]
            )
            <> div_
              [class_ "col-md-6"]
              ( label_ [for_ longitude_, class_ "form-label"] longitudeLabel
                  <> input_ [type_ "number", name_ longitude_, class_ "form-control"]
              )
            <> div_
              [class_ "col-12"]
              ( button_ [type_ "submit", class_ "btn btn-primary"] submitLabel
              )
        )
 where
  latitude_ = "latitude"
  longitude_ = "longitude"
  latitudeLabel = "Latitude"
  longitudeLabel = "Longitude"
  submitLabel = "Converto to UTM"
  resultContents :: Maybe Coordinates -> Html ()
  resultContents r
    | (Just coords) <- r =
        div_
          [class_ "container-sm border rounded-3 p-1"]
          ( div_
              [class_ "row"]
              ( div_ [class_ "col"] "Latitude"
                  <> div_ [class_ "col"] (show $ latitude coords)
              )
              <> div_
                [class_ "row"]
                ( div_ [class_ "col"] "Longitude"
                    <> div_ [class_ "col"] (show $ longitude coords)
                )
              <> div_
                [class_ "row"]
                ( div_ [class_ "col"] "UTM Zone"
                    <> div_ [class_ "col"] (show (zone coords) <> show (latitudeBand coords))
                )
              <> div_
                [class_ "row"]
                ( div_ [class_ "col"] "Easting"
                    <> div_ [class_ "col"] (show $ easting coords)
                )
              <> div_
                [class_ "row"]
                ( div_ [class_ "col"] "Northing"
                    <> div_ [class_ "col"] (show $ northing coords)
                )
          )
    | otherwise =
        pure ()

-- div_
--   [class_ "toast", role_ "alert"]
--   ( div_
--       [class_ "toast-header"]
--       ( strong_ [class_ "me-auto"] "Warning"
--       )
--       <> div_
--         [class_ "toast-body"]
--         "Need to insert coordinates"
--   )

----------------------------------------s----------------------------------------
template :: Html () -> Html ()
template content =
  html_
    ( head_
        ( link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css", rel_ "stylesheet", integrity_ "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC", crossorigin_ "anonymous"]
            <> link_ [rel_ "manifest", href_ "manifest.json"]
            <> meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        )
        <> body_
          [class_ "bg-light pt-4"]
          ( div_ [class_ "container-sm shadow p-4 bg-white rounded-3"] content
          )
    )
