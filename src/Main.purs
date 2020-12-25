module Main where

import Prelude

import Article (Article)
import Effect (Effect)
import Elmish (ComponentDef, DispatchMsgFn, ReactElement, Transition)
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H
import Home as Home
import Utils.Html as Html

main :: Effect Unit
main =
  defaultMain
    { elementId: "app"
    , def
    }

type State =
  { articles :: Array Article
  }

type Message = Void

def :: forall m. ComponentDef m Message State
def =
  { init
  , update: \state _ -> pure state
  , view
  }
  where
    init :: Transition m Message State
    init = pure
      { articles:
          [ { image: "http://i.imgur.com/Qr71crq.jpg"
            , author: "Eric Simons"
            , date: "January 20th"
            , likes: 29
            , title: "How to build webapps that scale"
            , description: "This is the description for the post"
            }
          , { image: "http://i.imgur.com/N4VcUeJ.jpg"
            , author: "Albert Pai"
            , date: "January 20th"
            , likes: 32
            , title: "The song you won't ever stop singing. No matter how hard you try."
            , description: "This is the description for the post."
            }
          ]
      }

    view :: State -> DispatchMsgFn Message -> ReactElement
    view state _ = H.fragment
      [ H.nav "navbar navbar-light" $
          H.div "container"
          [ H.a_ "navbar-brand" { href: "index.html" } "conduit"
          , H.ul "nav navbar-nav pull-xs-right" $
            [ H.li "nav-item" $
                H.a_ "nav-link active" { href: "" } "Home"
            , H.li "nav-item" $
                H.a_ "nav-link" { href: "" }
                  [ H.i "ion-compose" H.empty
                  , Html.nbsp
                  , H.text "New Post"
                  ]
            , H.li "nav-item" $
                H.a_ "nav-link" { href: "" }
                  [ H.i "ion-gear-a" H.empty
                  , Html.nbsp
                  , H.text "Settings"
                  ]
            , H.li "nav-item" $
                H.a_ "nav-link" { href: "" }
                  [ H.i "ion-gear-a" H.empty
                  , Html.nbsp
                  , H.text "Sign up"
                  ]
            ]
          ]
      , Home.view state
      , H.footer "" $
          H.div "container"
            [ H.a_ "logo-font" { href: "/" } "conduit"
            , H.span "attribution"
                [ H.text "An interactive learning project from "
                , H.a_ "" { href: "https://thinkster.io" } "Thinkster"
                , H.text ". Code "
                , Html.amp
                , H.text " design licensed under MIT."
                ]
            ]
      ]
