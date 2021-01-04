module Pages.Login
  ( Message
  , Page(..)
  , State(..)
  , LoginState
  , RegisterState
  , init
  , update
  , view
  ) where

import Prelude

import Elmish (DispatchMsgFn, ReactElement, Transition)
import Elmish.HTML.Styled as H
import Router as Router

data State
  = Login LoginState
  | Register RegisterState

type LoginState = Unit

type RegisterState = Unit

type Message = Void

data Page
  = LoginPage
  | RegisterPage

init :: forall m. Page -> Transition m Message State
init = case _ of
  LoginPage -> pure $ Login unit
  RegisterPage -> pure $ Register unit

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  _ -> pure state

view :: State -> DispatchMsgFn Message -> ReactElement
view state _ =
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
              Register _ -> H.fieldset "form-group" $
                H.input_ "form-control form-control-lg"
                  { placeholder: "Username"
                  , type: "text"
                  }
          , H.fieldset "form-group" $
              H.input_ "form-control form-control-lg"
                { placeholder: "Email"
                , type: "text"
                }
          , H.fieldset "form-group" $
              H.input_ "form-control form-control-lg"
                { placeholder: "Password"
                , type: "password"
                }
          , H.button "btn btn-lg btn-primary pull-xs-right" case state of
              Login _ -> "Sign in"
              Register _ -> "Sign up"
          ]
        ]
