Option Explicit

Dim a

Private Sub Hello(x)
    msgbox "Welcome to " + x + "!"
End Sub

Class Foo
    private tape()

    Public Sub Class_Initialize()
        ReDim tape(100)
    End Sub

    Public Property Let Identity(x)
        tape(99) = x
    End Property

    Public Property Let Marf(num,x)
        tape(num) = x
    End Property

    Public Property Get Identity()
        Identity = tape(99)
    End Property

    Public Function GetId()
        GetId = tape(99)
    End Function
End Class

Set a = New Foo
a.Identity = "foobar"
Hello a.GetId
a.Marf(99) = "marf"
Hello a.Identity
