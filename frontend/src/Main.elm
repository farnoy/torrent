import AnimationFrame exposing (times)
import Bits exposing (split)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (getString, post, multipart, stringData)
import Http
import Humanize exposing (humanize)
import Json.Decode exposing(..)
import Json.Decode as Json
import Json.Encode as Json
import Json.Encode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options
import Material.Table as Table
import Material.Typography as Typo
import Set exposing (Set)
import Set
import String
import Svg
import Svg.Attributes as Svg
import Task as Task
import Time
import Material.Scheme as Scheme
import Material.Color as Color

import Native.Test

type alias DataSet = {
    label : String
  , data : List Float
  , pointRadius : Int
  }

type alias ChartOptions = {
    type_ : String
  , data : {
      labels: List String
    , datasets: List DataSet
    }
  }

encodeChartOptions : ChartOptions -> Json.Encode.Value
encodeChartOptions {type_, data} =
  let
      datasets = List.map makeDataset data.datasets
      makeDataset s = Json.object
                        [ ("label", Json.Encode.string s.label)
                        , ("data",
                              List.map Json.Encode.float s.data
                           |> Json.Encode.list)
                        , ("pointRadius", Json.Encode.int s.pointRadius)
                        ]
  in Json.Encode.object
    [ ("type", Json.Encode.string type_)
    , ("data", Json.Encode.object
      [ ("labels",
            List.map Json.Encode.string data.labels
         |> Json.Encode.list)
      , ("datasets", Json.Encode.list datasets)
      ]
    )
    ]


chart : String -> ChartOptions -> Task.Task x ()
chart id opt = Native.Test.chart id (encodeChartOptions opt |> Json.encode 0)

main =
  Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

subscriptions : Model -> Sub Msg
subscriptions model =
  let
      baseTimer = [Time.every (5 * Time.second) (always ReloadActive)]
      graphChart = if model.pendingAnimFrameForChart then [times (always UpdateCharts)] else []
      mdlSubs = [Layout.subscriptions model.layoutMdl]
             |> List.map (Sub.map LayoutMdl)
  in Sub.batch <| baseTimer ++ graphChart ++ mdlSubs


type alias Model =
  { activeTorrents : List ActiveTorrent
  , expandedTorrents : Set String
  , connectionLive : Bool
  , pendingAnimFrameForChart : Bool
  , mdl : Material.Model
  , layoutMdl : Layout.Model
  }


init : (Model, Cmd Msg)
init =
  let
      mdlModel = Material.model
      (layoutModel, layoutCmd) = Layout.init
  in ({ activeTorrents = [], expandedTorrents = Set.empty, connectionLive = False, pendingAnimFrameForChart = False, mdl = mdlModel, layoutMdl = layoutModel },
        Cmd.batch [Task.succeed ReloadActive |> Task.perform (\_ -> Noop) (\a -> a), Cmd.map LayoutMdl layoutCmd])


view model =
  Scheme.topWithScheme Color.Brown Color.Indigo <|
  Layout.render Mdl model.mdl
    [ Layout.fixedHeader
    ]
    { header = header model
    , drawer = []
    , tabs = ([], [])
    , main = [ viewMain model ]
    }

header model =
  [ Layout.row []
        [ Layout.title [] [text "Torrent"]
        , Layout.spacer
        , Layout.navigation []
          [ Button.render Mdl [0, 0] model.mdl
            [ Button.icon
            , Button.ripple
            , Button.onClick ReloadActive
            ]
            [ Icon.i "refresh"]
          , Layout.link []
            [ Icon.i (if model.connectionLive then "cloud" else "cloud_off") ]
          ]
        ]
      ]

viewMain model =
  Grid.grid [Grid.maxWidth "900px"]
    [ Grid.cell [ Grid.size Grid.All 12 ]
        [ div [] (List.indexedMap (viewTorrent model.mdl model.expandedTorrents) model.activeTorrents)
        ]
    ]

viewTorrent mdl expandedTorrents idx torrent =
  let
      isExpanded = Set.member torrent.infoHash expandedTorrents
  in Card.view
    [ if isExpanded then Elevation.e8 else Elevation.e2
    , Options.css "width" "100%"
    , if isExpanded then Options.css "margin-bottom" "20px" else Options.nop
    ]
    [ Card.title [] [ Card.head [] [ text torrent.name ] ]
    , Card.actions [ Card.border ]
      [ if torrent.status == "active"
        then Button.render Mdl [idx, 0] mdl
              [ Button.icon
              , Button.ripple
              , Button.onClick (StopTorrent torrent.infoHash)
              ]
              [ Icon.i "stop"]
        else Button.render Mdl [idx, 1] mdl
              [ Button.icon
              , Button.ripple
              , Button.onClick (StartTorrent torrent.infoHash)
              ]
              [ Icon.i "play_arrow"]
      , if isExpanded
        then Button.render Mdl [idx, 2] mdl
              [ Button.icon
              , Button.ripple
              , Button.onClick (ShrinkTorrent torrent.infoHash)
              ]
              [ Icon.i "expand_less"]
        else Button.render Mdl [idx, 3] mdl
              [ Button.icon
              , Button.ripple
              , Button.onClick (ExpandTorrent torrent.infoHash)
              ]
              [ Icon.i "expand_more"]
      ]
    , Card.text [ Card.border ]
      [ text <| toString (toFloat (round (torrent.progress * 10000)) / 100) ++ "%"
      , Options.styled p
        [ Typo.subheading ]
        [ Icon.view "file_download" [ Options.css "vertical-align" "bottom" ]
        , torrent.downloadSpeed |> List.filter (\a -> a /= 0) |> List.reverse |> List.take 5 |> List.map toFloat |> List.sum |> \a -> a / 5 |> humanize |> \s -> s ++ "/s" |> text
        ]
      , div []
        (viewExpandedTorrent isExpanded torrent)
      ]
    ]

viewExpandedTorrent isExpanded torrent =
  if isExpanded
  then
    [ canvas [id <| "bitfield-" ++ torrent.infoHash, width 600, height 200] []
    , canvas [id <| "download-speed-" ++ torrent.infoHash, width 600, height 200] []
    ]
  else []

viewBitField : Int -> Int -> Svg.Svg a
viewBitField index bar =
  let
      xStr = toString index
      y2 = 80 - toFloat bar / 255 * 80
      y2Str = toString y2
  in Svg.line [Svg.x1 xStr, Svg.y1 "80", Svg.x2 xStr, Svg.y2 y2Str, Svg.stroke "red"] []

viewDownloadHistory : List Int -> Svg.Svg a
viewDownloadHistory hist =
  let
      points =  List.reverse hist
             |> List.take 60
             |> List.reverse
             |> List.indexedMap (\ix hist -> [toString ix, toString <| 1000 - (toFloat hist) / 1000])
             |> List.map (String.join ",")
             |> String.join " "
  in Svg.polyline [Svg.points points, Svg.stroke "blue", Svg.fill "none"] []


type Msg =
    ReloadActive
  | SetActiveTorrents (List ActiveTorrent)
  | ExpandTorrent String
  | ShrinkTorrent String
  | Noop
  | LoseConnection
  | StartTorrent String
  | StopTorrent String
  | UpdateCharts
  | Mdl Material.Msg
  | LayoutMdl Layout.Msg


showChart : String -> ChartOptions -> Cmd Msg
showChart id opt = chart id opt |> Task.perform (always Noop) (always Noop)

showDownloadCharts : Model -> List (Cmd Msg)
showDownloadCharts model =
  let
    chartOps torrent =
      {
        type_ = "line",
        data = {
          labels = List.indexedMap (\ix _ -> if (ix - 60) % 10 == 0 then toString (ix - 60) ++ "s" else "") torrent.downloadSpeed
        , datasets = [{
            label = "KiB/s"
          , data = List.map (\val -> toFloat val / 1024) torrent.downloadSpeed
          , pointRadius = 1
          }]
        }
      }
    makeId torrent = "download-speed-" ++ torrent.infoHash
  in List.map (\t -> showChart (makeId t) (chartOps t)) model.activeTorrents

showBitfieldCharts : Model -> List (Cmd Msg)
showBitfieldCharts model =
  let
    chartOps torrent =
      let
          pieces = List.concatMap split torrent.bitField
      in
        { type_ = "line"
        , data =
          { labels = List.indexedMap (\ix _ -> if ix % 10 == 0 then toString ix else "") pieces
          , datasets = [{
              label = "piece progress"
            , data = List.map (\a -> if a then 1 else 0) pieces
            , pointRadius = 0
            }]
          }
        }
    makeId torrent = "bitfield-" ++ torrent.infoHash
  in List.map (\t -> showChart (makeId t) (chartOps t)) model.activeTorrents

fetchData : Cmd Msg
fetchData = getString "http://localhost:8036/active"
         |> Task.toResult
         |> Task.perform (\_ -> Noop) parseResponse

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ReloadActive -> (model, fetchData)
    Noop -> (model, Cmd.none)
    SetActiveTorrents list -> ({ model | activeTorrents = list, pendingAnimFrameForChart = True, connectionLive = True }, Cmd.none)
    ExpandTorrent infoHash -> ({ model | expandedTorrents = Set.insert infoHash model.expandedTorrents, pendingAnimFrameForChart = True }, Cmd.none)
    ShrinkTorrent infoHash -> ({ model | expandedTorrents = Set.remove infoHash model.expandedTorrents, pendingAnimFrameForChart = True }, Cmd.none)
    LoseConnection -> ({ model | activeTorrents = [], connectionLive = False }, Cmd.none)
    StartTorrent infoHash -> (model, post (keyValuePairs string) "http://localhost:8036/start" (multipart [stringData "infoHash" infoHash]) |> Task.toResult |> Task.perform (\_ -> Noop) (\_ -> Noop))
    StopTorrent infoHash -> (model, post (keyValuePairs string) "http://localhost:8036/stop" (multipart [stringData "infoHash" infoHash]) |> Task.toResult |> Task.perform (\_ -> Noop) (\_ -> Noop))
    UpdateCharts -> ({ model | pendingAnimFrameForChart = False }, Cmd.batch <| showDownloadCharts model ++ showBitfieldCharts model)
    Mdl msg -> let (model', cmd) = Material.update Mdl msg model in (model', cmd)
    LayoutMdl msg -> let (model', cmd) = Layout.update msg model.layoutMdl in ({ model | layoutMdl = model' }, Cmd.map LayoutMdl cmd)

type alias ActiveTorrent =
  {
    infoHash : String
  , name : String
  , progress : Float
  , status : String
  , peerCount : Int
  , bitField : List Int
  , downloadSpeed : List Int
  }

activeTorrentDecoder : Decoder ActiveTorrent
activeTorrentDecoder =
  Json.object7 ActiveTorrent
    ("infoHash" := string)
    ("name" := string)
    ("progress" := float)
    ("status" := string)
    ("peers" := int)
    ("bitField" := Json.Decode.list int)
    ("downloadSpeed" := Json.Decode.list int)

parseResponse : Result Http.Error String -> Msg
parseResponse res =
  case res of
    Ok str -> case decodeString (Json.Decode.list activeTorrentDecoder) str of
      Ok val -> SetActiveTorrents val
      Err err -> LoseConnection
    Err err -> LoseConnection
