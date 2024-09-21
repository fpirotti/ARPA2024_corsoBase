require(bcrypt)
require(shinyvalidate) ### remotes::install_github("rstudio/shinyvalidate")

#### FUNZIONE per accedere alle app con autenticazione minima
#### Crea due file
####    users.pwds.rda - una list con chiave la email dell'utente e valore la password in hash (crittografata)
####    users.logs.rda - una list con chiave la email dell'utente e vome valore un vettore con il timestamp di accesso
#### PER ATTIVARE: aggiungere sotto  server.R (dentro la funzione server() ):
####               source("functions_auth.R", local=T)

ra<- reactiveValues( LOGGED.USER=NULL, IS.LOGGED=F, IS.UNIPD=F)

##  if this is null, a "send password" button
## will appear allowing anyone to have a password.
## It this is not the desired behavious, eg. you want only a predefined list of user<->password, then
## change this to list("username"="password") - password can be clear or hashed
predefined.users<-NULL


iv <- InputValidator$new()
iv$add_rule("password", sv_required())
iv$add_rule("email", sv_required())
iv$add_rule("email", sv_email())
iv$enable()

if(file.exists("users.pwds.rda")) load("users.pwds.rda") else users.pwds<-list()
if(file.exists("users.logs.rda")) load("users.logs.rda") else users.logs<-list()

taglist.auth <- list(
  div(
    textInput("email", "Email address"),
    passwordInput("password", "Password"),
    div( style="margin: 0 auto; ",
         actionButton("ok_pw", "Log IN", icon = icon("user"), style="color:black;",
                      class="btn btn-success action-button shiny-bound-input"),
         div(style=" color:black; font-weight:500;   border-radius: 5px;
                       cursor: pointer;",
             class="btn btn-warning action-button shiny-bound-input",
             onclick="Shiny.setInputValue('resetpwd', true,  {priority: 'event'});",
             title="A mail with a password will be sent - can also be used to reset your password if you forget it",
         HTML( as.character(icon("envelope")), "  Send password" ) )
    )
  )
)


isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$\\>", as.character(x), ignore.case=TRUE)
}


#' setPassword
#'
#' @param x username (email address of user)
#'
#' @return
#' @export
#'
#' @examples #
setPassword<-function(x){

  pw<-fun::random_password(12, extended = F)
  if(!exists("users.pwds")) {
    users.pwds<<-list()
    warning("here in users.pd")
  }

  users.pwds[[x]]<<- bcrypt::hashpw(pw)
  save(users.pwds, file="users.pwds.rda")
  Sys.chmod("users.pwds.rda", "777", use_umask = FALSE)

  comm<-sprintf('echo "Your password is: %s\\nDo not reply to this mail.\\n\\nCIRGEO Interdepartmenta Research Center in Geomatics" | mailx -s "InForSat password" --append "From: CIRGEO InForSAT <donotreply@unipd.it>" --append="BCC:<francesco.pirotti@unipd.it>"  %s',
                pw, input$email  )

  res<-system(comm, intern = T)

}



#### check submitted form data
observeEvent(input$ok_pw, {
  # ra$IS.LOGGED<-T
  # ra$LOGGED.USER<-"sss"
  #
  # removeModal()
  # return()
  browser()
  if( !isValidEmail(input$email ) ){
    title<-"Authentication error"
    message<-sprintf("Email address \"%s\" is not recognized as a valid email address.", input$email)
  }   else if(is.null(users.pwds[[ input$email ]]) ){
    title<-"Authentication error"
    message<-sprintf("Email  \"%s\" is not enabled. Use the 'Send password' button to sent a password to that address. <br>
                    Access will be monitored and blacklisted if abused.", input$email)
  } else {
    is.correct.pw<-F
    if(is.character(users.pwds[[ input$email ]]) &&
       nchar(input$password)>2 ) {
      ## verifica password, va bene memorizzata hashed o clear
      is.correct.pw <- bcrypt::checkpw(input$password,   users.pwds[[ input$email ]]) ||
                                      input$password == users.pwds[[ input$email ]]

    }
    if( is.correct.pw ){
      ra$IS.LOGGED<-T
      ra$LOGGED.USER<-input$email

      users.logs[[ input$email ]]<-c(users.logs[[ input$email ]], Sys.time())

      save(users.logs, file="users.logs.rda")
      Sys.chmod("users.logs.rda", "777", use_umask = FALSE)

      removeModal()
      return()
    }

    title<-"Authentication error"
    message<-sprintf("Email recognized but <b>WRONG PASSWORD</B>! If you forgot your password click the \"Send password\" button to send again." )
  }
  showAlert(title, message )

  })



### SEND PASSWORD to user ----
observeEvent(input$resetpwd, {

  req(input$resetpwd)

  if(!isValidEmail(input$email)){
    shinyjs::alert(sprintf("%s ... not a recognized email address.", input$email))
  } else {
    setPassword(input$email)
    shinyjs::alert(sprintf("An email was sent to %s, check mailbox.", input$email))
  }

})


showAlert<-function(title= HTML( as.character(icon("user")), "Authenticate"), message="<b>Email and password</b><br>if your email is not enabled push the button
                   'Send password' - a password will be sent to the email address."){

  showModal(modalDialog(
    title = HTML(title),
    tagList(taglist.auth) ,
    easyClose = T,
    footer =  HTML(message)
  ))

}

showAlert()
