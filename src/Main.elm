module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes as A
import Html.Events as E
import Http
import Iso8601
import Json.Decode as Dec
import Json.Decode.Pipeline as Pipe
import Time


baseUrl =
    "https://versatileapi.herokuapp.com/api"


type alias DescriptionFormat =
    { description : String
    , type_ : String
    , minLength : Int
    , maxLength : Int
    }


type alias Text =
    DescriptionFormat


type alias InReplyToUserId =
    DescriptionFormat


type alias InReplyToTextId =
    DescriptionFormat


type alias Properties =
    { text : Text
    , inReplyToUserId : InReplyToUserId
    , inReplyToTextId : InReplyToTextId
    }


type alias PostMessage =
    { additionalProperties : Bool
    , type_ : String
    , properties : Properties
    , required : List String
    }


type alias Message =
    { id : String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , userId : String
    , text : String
    }


type alias Model =
    { postForm : String
    , messages : List Message
    , error : Maybe Http.Error
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { postForm = ""
      , messages = []
      , error = Nothing
      }
    , Http.get
        { url = baseUrl ++ "/text/all"
        , expect = Http.expectJson GotMessages (Dec.list messageDecoder)
        }
    )


messageDecoder : Dec.Decoder Message
messageDecoder =
    Dec.succeed Message
        |> Pipe.required "id" Dec.string
        |> Pipe.required "_created_at" Iso8601.decoder
        |> Pipe.required "_updated_at" Iso8601.decoder
        |> Pipe.required "_user_id" Dec.string
        |> Pipe.required "text" Dec.string


descriptionFormatDecoder : Dec.Decoder DescriptionFormat
descriptionFormatDecoder =
    Dec.succeed DescriptionFormat
        |> Pipe.required "description" Dec.string
        |> Pipe.required "type" Dec.string
        |> Pipe.required "minLength" Dec.int
        |> Pipe.required "maxLength" Dec.int


propertiesDecoder : Dec.Decoder Properties
propertiesDecoder =
    Dec.succeed Properties
        |> Pipe.required "text" descriptionFormatDecoder
        |> Pipe.required "in_reply_to_user_id" descriptionFormatDecoder
        |> Pipe.required "in_reply_to_text_id" descriptionFormatDecoder


postMessageDecoder : Dec.Decoder PostMessage
postMessageDecoder =
    Dec.succeed PostMessage
        |> Pipe.required "addtionalProperties" Dec.bool
        |> Pipe.required "type" Dec.string
        |> Pipe.required "properties" propertiesDecoder
        |> Pipe.required "required" (Dec.list Dec.string)



-- view


viewPostForm : String -> Html Msg
viewPostForm postForm =
    div []
        [ Html.input [ A.value postForm, E.onInput UpdatePostForm ] [] ]


viewMessage : Message -> Html msg
viewMessage message =
    div [ A.class "message" ]
        [ Html.text message.text ]


viewMessages : List Message -> Html msg
viewMessages messages =
    div []
        (List.map viewMessage messages)


viewError : Maybe Http.Error -> Html msg
viewError maybeError =
    case maybeError of
        Nothing ->
            Html.text ""

        Just error ->
            case error of
                Http.BadUrl url ->
                    Html.text ("Bad URL" ++ url)

                Http.Timeout ->
                    Html.text "timeout"

                Http.NetworkError ->
                    Html.text "network error"

                Http.BadStatus code ->
                    Html.text <| String.fromInt code

                Http.BadBody body ->
                    Html.text body


view : Model -> Html Msg
view model =
    div []
        [ viewPostForm model.postForm
        , viewMessages model.messages
        , viewError model.error
        ]



-- update


type Msg
    = UpdatePostForm String
    | GotMessages (Result Http.Error (List Message))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePostForm text ->
            ( { model | postForm = text }
            , Cmd.none
            )

        GotMessages result ->
            case result of
                Ok messages ->
                    ( { model | messages = messages }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = Just error }
                    , Cmd.none
                    )


subs : Model -> Sub Msg
subs model =
    Sub.none


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }
