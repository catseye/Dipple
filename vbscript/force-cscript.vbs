' Force this WSH script to run in a console window,
' even if it is started by double-cliking the .vbs file
' in Windows Explorer.

' I don't know if this will actually work or not yet :/

Option Explicit
Dim WshShell

If InStr(UCase(WScript.FullName), "CSCRIPT") = 0 Then
    Set WshShell = WScript.CreateObject("WScript.Shell")
    WshShell.Run "CScript //Nologo " & WScript.ScriptFullName, 1, True
Else
    WScript.Echo "This should appear in a Console window"
End If
