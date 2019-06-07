## purescript-httpure-registration-example

### Simple registration / sign-up flow implemented in PureScript with: 

Cookies and session management (memory store)

Postgresql db to store registered users

Pure SQL operations (inserts & queries with aggregation) using [purescript-selda](https://github.com/Kamirus/purescript-selda/tree/experimental)

Send registration confirmation email

Used purescript HTTP server framework: [HTTPure](https://github.com/cprussin/purescript-httpure) 


## Endpoints

- **default**
  - serves register.html file with email input form
  - submitting the form makes POST request to /register
- **POST /register**
  - with email in body 
  - signes (using hmac) email concatenated with session id and uses it to create confirmation link
  - sends email with confirmation link (which leads to next endpoint)
- **GET /confirm/<signed_email_with_session_id>**
  - check signed email and session id
  - serves register-password.html file with password input form
  - submitting the form makes POST request to /confirm
- **POST /confirm/<signed_email_with_session_id>**
  - with password in body
  - check signed email and session id
  - tries to register the user in the database (email may be taken)
