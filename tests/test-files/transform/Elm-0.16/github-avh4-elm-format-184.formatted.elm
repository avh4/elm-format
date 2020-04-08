module Main exposing (..)


update : CotwData.Msg -> Model -> ( Model, Cmd CotwData.Msg )
update msg model =
  case msg of
    SplashMsg NewGame ->
      ( { model | currentPage = CharCreationPage }, Cmd.none )

    SplashMsg _ ->
      ( { model | currentPage = NotImplementedPage }, Cmd.none )

    CharCreationMsg StartGame ->
      ( { model | currentPage = GamePage }, Cmd.none )

    CharCreationMsg msg ->
      ( { model | character = CharCreation.update msg model.character }, Cmd.none )

    GameMsg msg ->
      ( { model | game = Game.update msg model.game }, Cmd.none )

    Keyboard (Just a) ->
      ( model, Cmd.none )

    Keyboard Maybe.Nothing ->
      ( model, Cmd.none )
