module Backend.Effects.Interpret where

import Prelude

import Backend.App.Types (RMailTransporter)
import Backend.Effects (MAILER, MailerF(..), _mailer)
import Effect.Class (liftEffect)
import Effect.Console (log)
import NodeMailer as Mail
import Run (AFF, Run(..), interpret, liftAff, on, send)
import Run.Reader (READER, ask)

-- Mailer Effect

handleMailer ∷
  ∀ eff r
  .  MailerF
  ~> Run 
      ( aff ∷ AFF
      , reader ∷ READER { | RMailTransporter r }
      | eff
      )
handleMailer = case _ of
  SendMail { from, to, subject, text } cnt → do
    let 
      msg = 
        { from
        , to: [ to ]
        , cc: []
        , bcc: []
        , subject
        , text
        , attachments: []
        }
    { mailTransporter } ← ask
    url ← liftAff do
      msgInfo ← Mail.sendMail_ msg mailTransporter
      pure $ show $ Mail.getTestMessageUrl msgInfo
    pure $ cnt url

runMailer
  ∷ ∀ eff r
  .  Run (aff ∷ AFF, reader ∷ READER { | RMailTransporter r }, mailer ∷ MAILER | eff)
  ~> Run (aff ∷ AFF, reader ∷ READER { | RMailTransporter r } | eff)
runMailer = interpret (on _mailer handleMailer send)
