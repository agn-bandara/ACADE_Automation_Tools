
Sub ReadTableValues()
    ' This example adds a table in model space and sets and gets a column name
    Dim MyModelSpace As AcadModelSpace
    Set MyModelSpace = ThisDrawing.ModelSpace
    Dim MyTable As AcadTable
    Dim CellVal As Variant
    Dim CellContent As String
    ' Select a table
    Dim pickedEntity As AcadEntity
    Dim pickedPoint As Variant
    ThisDrawing.Utility.GetEntity pickedEntity, pickedPoint
    If TypeOf pickedEntity Is AcadTable Then
        Set MyTable = pickedEntity
    Else
        MsgBox "You did not select a table."
        Exit Sub
    End If
    
    CellVal = MyTable.GetCellValue(6, 1)
    CellContent = Split(CellVal, ";")(2)
    'Call MyTable.SetCellValue(6, 1, CellVal)
    'Call MyTable.SetCellValueFromText(6, 1, CellVal, acSetDefaultFormat)
    MsgBox CellContent & " is the test cell's value "

End Sub