' This technique forces this WSH script to run in a console window,
' even if it is started by double-clicking the .vbs file
' in Windows Explorer.

Option Explicit
Dim WshShell
Dim I
Dim Pause

If InStr(UCase(WScript.FullName), "CSCRIPT") = 0 Then
    Set WshShell = WScript.CreateObject("WScript.Shell")
    WshShell.Run "CScript //Nologo " & Chr(34) & WScript.ScriptFullName & Chr(34) & " /Pause", 1, True
Else
    WScript.Echo "This should appear in a Console window"
    'Remainder of your script goes here
    Pause = False
    For I = 0 to WScript.Arguments.Count - 1
        If WScript.Arguments(I) = "/Pause" Then
	    Pause = True
	End If
    Next
    If Pause Then
        WScript.Echo "[Press Enter to close this window]"
        WScript.StdIn.ReadLine
    End If
End If
