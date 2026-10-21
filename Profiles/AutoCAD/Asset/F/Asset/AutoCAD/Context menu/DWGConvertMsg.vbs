' DWGConvertMsg.vbs
' Called as:  wscript.exe "F:\Asset\AutoCAD\Context menu\DWGConvertMsg.vbs" "<path-to-dwg>"

Option Explicit

Dim srcDwg, tgtDxf, exe, cmd
Dim fso, shell

' --- 1. Grab the source file from the registry %1 argument ------------
If WScript.Arguments.Count < 1 Then
    MsgBox "No DWG file was passed to the script.", vbExclamation, "Convert to dxf"
    WScript.Quit 1
End If

srcDwg = WScript.Arguments(0)

Set fso = CreateObject("Scripting.FileSystemObject")

If Not fso.FileExists(srcDwg) Then
    MsgBox "Source file not found:" & vbCrLf & srcDwg, vbExclamation, "Convert to dxf"
    WScript.Quit 1
End If

' --- 2. Build the target DXF path (same folder + base name, .dxf) -----
tgtDxf = fso.BuildPath( _
             fso.GetParentFolderName(srcDwg), _
             fso.GetBaseName(srcDwg) & ".dxf" )

' --- 3. Locate Acme CAD Converter -----------------------------------
exe = "D:\Program Files\Acme CAD Converter\AcmeCADConverter.exe"
If Not fso.FileExists(exe) Then
    MsgBox "Acme CAD Converter was not found in the standard locations." & vbCrLf & _
           "Edit the 'exe' path inside DWGConvertMsg.vbs.", vbExclamation, "Convert to dxf"
    WScript.Quit 1
End If

' --- 4. Build and run the Acme command line -------------------------
cmd = """" & exe & """ /r /e /x2010 """ & srcDwg & """ """ & tgtDxf & """"

Set shell = CreateObject("WScript.Shell")
On Error Resume Next
shell.Run cmd, 0, True          ' 0 = hidden console, True = wait for exit
If Err.Number <> 0 Then
    MsgBox "Failed to launch Acme CAD Converter." & vbCrLf & _
           "Error " & Err.Number & ": " & Err.Description, vbCritical, "Convert to dxf"
    WScript.Quit 1
End If
On Error GoTo 0

' --- 5. Report the result -------------------------------------------
' If fso.FileExists(tgtDxf) Then
'    MsgBox "Conversion complete:" & vbCrLf & tgtDxf, vbInformation, "Convert to dxf"
' Else
'    MsgBox "Acme finished, but the DXF was not produced:" & vbCrLf & tgtDxf & vbCrLf & _
'           "Check the Acme switches (/r /e /x2010) for your version.", vbExclamation, "Convert to dxf"
' End If
