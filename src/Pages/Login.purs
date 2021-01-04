module Pages.Login
  ( Message
  , Page(..)
  , State
  , LoginState
  , RegisterState
  , init
  , update
  , view
  , route
  ) where

import Prelude

import Elmish (DispatchMsgFn, ReactElement, Transition, handleMaybe)
import Elmish.HTML.Styled as H
import Router as Router
import Utils.Html (eventTargetValue)

data State
  = Login LoginState
  | Register RegisterState

type LoginState =
  { email :: String
  , password :: String
  }

type RegisterState =
  { email :: String
  , password :: String
  , username :: String
  }

data Message
  = EditEmail String
  | EditPassword String
  | EditUsername String

data Page
  = LoginPage
  | RegisterPage

init :: forall m. Page -> Transition m Message State
init = case _ of
  LoginPage -> pure $ Login { email: "", password: "" }
  RegisterPage -> pure $ Register { email: "", password: "", username: "" }

update :: forall m. State -> Message -> Transition m Message State
update state message = case message, state of
  EditEmail email, Login loginState ->
    pure $ Login loginState { email = email }
  EditEmail email, Register registerState ->
    pure $ Register registerState { email = email }
  EditPassword password, Login loginState ->
    pure $ Login loginState { password = password }
  EditPassword password, Register registerState ->
    pure $ Register registerState { password = password }
  EditUsername username, Register registerState ->
    pure $ Register registerState { username = username }
  EditUsername username, _ ->
    pure state

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch =
  H.div "auth-page" $
    H.div "container page" $
      H.div "row" $
        H.div "col-md-6 offset-md-3 col-xs-12"
        [ H.h1 "text-xs-center" case state of
            Login _ -> "Sign in"
            Register _ -> "Sign up"
        , H.p "text-xs-center" $
            H.a_ ""
              { href: Router.print case state of
                  Login _ -> Router.Register
                  Register _ -> Router.Login
              }
              case state of
                Login _ -> "Need an account?"
                Register _ -> "Have an account?"
        -- , H.ul "error-messages"
        --   [ H.li "" "That email is already taken"
        --   ]
        , H.form ""
          [ case state of
              Login _ ->
                H.empty
              Register { username } -> H.fieldset "form-group" $
                H.input_ "form-control form-control-lg"
                  { placeholder: "Username"
                  , type: "text"
                  , value: username
                  , onChange: handleMaybe dispatch (map EditUsername <<< eventTargetValue)
                  }
          , H.fieldset "form-group" $
              H.input_ "form-control form-control-lg"
                { placeholder: "Email"
                , type: "text"
                , value: case state of
                    Login { email } -> email
                    Register { email } -> email
                , onChange: handleMaybe dispatch (map EditEmail <<< eventTargetValue)
                }
          , H.fieldset "form-group" $
              H.input_ "form-control form-control-lg"
                { placeholder: "Password"
                , type: "password"
                , value: case state of
                    Login { password } -> password
                    Register { password } -> password
                , onChange: handleMaybe dispatch (map EditPassword <<< eventTargetValue)
                }
          , H.button "btn btn-lg btn-primary pull-xs-right" case state of
              Login _ -> "Sign in"
              Register _ -> "Sign up"
          ]
        ]

route :: State -> Router.Route
route = case _ of
  Login _ -> Router.Login
  Register _ -> Router.Register
