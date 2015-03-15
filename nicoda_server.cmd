SET wdir=%cd%
SET wdir=%wdir:\=/%
"C:\Program Files\R\R-3.1.1\bin\x64\R.exe" -e "shiny::runApp('%wdir%', port = 12345, launch.browser=TRUE)"
pause