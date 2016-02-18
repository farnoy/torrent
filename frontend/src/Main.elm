import Effects exposing (Effects)
import Effects as Effects
import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http exposing (getString)
import Http
import Json.Decode exposing(decodeString, (:=), string, float, Decoder)
import Json.Decode as Json
import Task as Task
import StartApp as StartApp
import Time

app =
  StartApp.start { init = init
                 , view = view
                 , update = update
                 , inputs =
                   [ Time.every (5 * Time.second)
                     |> Signal.map (always ReloadActive)
                   ]
                 }


main = app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


type alias Model = { activeTorrents : List ActiveTorrent }


init : (Model, Effects Action)
init = ({ activeTorrents = [] },
        Task.succeed ReloadActive |> Effects.task)


view address model =
  div []
    [ button [ onClick address ReloadActive ] [ text "query" ]
    , text <| toString model
    ]


type Action = ReloadActive | SetActiveTorrents (List ActiveTorrent) | Noop


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ReloadActive -> (model, getString "http://localhost:8036/active"
                     |> Task.toResult
                     |> Task.map parseResponse
                     |> Effects.task)
    Noop -> (model, Effects.none)
    SetActiveTorrents list -> ({ model | activeTorrents = list }, Effects.none)

type alias ActiveTorrent = { infoHash : String, progress: Float }

activeTorrentDecoder : Decoder ActiveTorrent
activeTorrentDecoder =
  Json.object2 ActiveTorrent
    ("infoHash" := string)
    ("progress" := float)

parseResponse : Result Http.Error String -> Action
parseResponse res =
  case res of
    Ok str -> case decodeString (Json.list activeTorrentDecoder) str of
      Ok val -> SetActiveTorrents val
      Err err -> Noop
    Err err -> Noop
