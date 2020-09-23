VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   5265
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   7260
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5265
   ScaleWidth      =   7260
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox p3 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   735
      Left            =   6120
      ScaleHeight     =   705
      ScaleWidth      =   825
      TabIndex        =   4
      Top             =   4200
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.PictureBox p2 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   735
      Left            =   4800
      Picture         =   "Form1.frx":0000
      ScaleHeight     =   705
      ScaleWidth      =   1185
      TabIndex        =   3
      Top             =   4200
      Visible         =   0   'False
      Width           =   1215
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   735
      Left            =   240
      ScaleHeight     =   705
      ScaleWidth      =   4305
      TabIndex        =   2
      Top             =   4200
      Width           =   4335
   End
   Begin VB.ListBox l1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H0000FF00&
      Height          =   2760
      ItemData        =   "Form1.frx":2982
      Left            =   240
      List            =   "Form1.frx":2989
      TabIndex        =   1
      Top             =   360
      Width           =   6735
   End
   Begin VB.TextBox t1 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FF00&
      Height          =   330
      Left            =   240
      MultiLine       =   -1  'True
      TabIndex        =   0
      Top             =   3480
      Width           =   6735
   End
   Begin VB.Frame Frame1 
      Caption         =   "Commands"
      ForeColor       =   &H0000FF00&
      Height          =   3135
      Left            =   120
      TabIndex        =   5
      Top             =   120
      Width           =   6975
   End
   Begin VB.Frame Frame2 
      Caption         =   "Enter Command"
      ForeColor       =   &H0000FF00&
      Height          =   735
      Left            =   120
      TabIndex        =   6
      Top             =   3240
      Width           =   6975
   End
   Begin VB.Frame Frame3 
      Caption         =   "Description"
      ForeColor       =   &H0000FF00&
      Height          =   1095
      Left            =   120
      TabIndex        =   7
      Top             =   3960
      Width           =   6975
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Dim COM As String, COM1 As String, COM2 As String, CRACK(1 To 10) As Boolean, DRIV


Private Sub Form_Load()
    For i = 1 To 10
        CRACK(i) = False
    Next
    Form1.BackColor = vbBlack
    Frame1.BackColor = vbBlack
    Frame2.BackColor = vbBlack
    Frame3.BackColor = vbBlack
    ENT = False
    DRIV = "c:"
    l1.ListIndex = l1.ListCount - 1
End Sub

Private Sub t1_KeyDown(KeyCode As Integer, Shift As Integer)
    COM = (t1.Text)
    COM2 = (t1.Text)
    If KeyCode = vbKeySpace Then
        If COM = "cd" Or COM = "view" Or COM = "portconnect" Then
            COM1 = COM
        End If
    End If
End Sub

Private Sub t1_KeyUp(KeyCode As Integer, Shift As Integer)
    If KeyCode = vbKeyReturn Then
        If COM1 <> "" Then
            COM = COM1
        End If
        l1.RemoveItem (l1.ListCount - 1)
        l1.AddItem UCase(DRIV) & "  " & COM2
        CO
        t1.Text = ""
        COM1 = ""
        l1.AddItem UCase(DRIV)
        l1.ListIndex = l1.ListCount - 1
    End If
    
    
End Sub
Sub CO()

Select Case LCase(COM)
    Case "dir"
        CLEAR
        p1.Print "Contents Of " & UCase(DRIV)
        Select Case LCase(DRIV)
            Case "c:"
                l1.AddItem "view"
                l1.AddItem "help.exe"
                l1.AddItem "exit.exe"
                l1.AddItem "text1.txt"
                l1.AddItem "c:\secret"
                CRACK(1) = True
            Case "c:\secret"
                l1.AddItem "pass.txt"
                l1.AddItem "c:\secret\pic"
                CRACK(2) = True
            Case "c:\secret\pic"
                l1.AddItem "passpic.bmp"
                CRACK(4) = True
            Case "e:"
                l1.AddItem "e:\ports"
                l1.AddItem "portconnect"
            Case "e:\ports"
                l1.AddItem "121.312.12.441"
                l1.AddItem "433.122.123.11"
                l1.AddItem "101.501.3.821"
                CRACK(6) = True
            Case "a:"
                l1.AddItem "a:\encrypted_files"
                l1.AddItem "decrypt.exe"
            Case "a:\encrypted_files"
                If CRACK(9) = True Then
                    l1.AddItem "Main Computer"
                    l1.AddItem "Access Key"
                Else
                    COI
                End If
        End Select
        
    Case "cd"
        CLEAR
        Select Case LCase(COM2)
            Case "cd c:"
                DRIV = "c:"
            Case "cd c:\secret"
                DRIV = "c:\secret"
            Case "cd c:\secret\pic"
                DRIV = "c:\secret\pic"
            Case "cd e:"
                DRIV = "e:"
            Case "cd e:\ports"
                DRIV = "e:\ports"
            Case "cd a:"
                DRIV = "a:"
            Case "cd a:\encrypted_files"
                DRIV = "a:\encrypted_files"
            Case Else
                COI
        End Select
    Case "cls"
        l1.CLEAR
    Case "exit.exe"
        End
    Case "help.exe"
        CLEAR
        p1.Print "The Commands are similar to DOS prompt, 'dir', 'cls', 'cd'"
        p1.Print "If you can improve the game, go ahead"

    Case "view"
        CLEAR
        Select Case LCase(COM2)
            Case "view text1.txt"
                If CRACK(1) = True Then
                    p1.Print "Can you gain access to the main computer"
                End If
            Case "view pass.txt"
                CLEAR
                If CRACK(3) = True Then
                    p1.Print "The password is PASSCRYPT"
                Else
                    COI
                End If
            Case "view passpic.bmp"
                CLEAR
                If CRACK(4) = True Then
                    p1.Picture = p2.Picture
                    CRACK(3) = True
                Else
                    COI
                End If
            Case Else
                p1.Print "View text and picture"
                p1.Print "Ex1. ' view text1.txt '"
        End Select
    

    Case "boss"
        CLEAR
        If CRACK(4) = True Then
            p1.Print "e: drive available"
            CRACK(5) = True
        Else
            COI
        End If

    Case "portconnect"
        CLEAR
        Select Case LCase(COM2)
            Case "portconnect 121.312.12.441"
                p1.Print "Connecting";
                Sleep 100
                DoEvents
                For x = 1 To 20
                    p1.Print ".";
                    Sleep 100
                    DoEvents
                Next
                p1.Print "Connection Lost";
                Sleep 100
                DoEvents
            Case "portconnect 433.122.123.11"
                p1.Print "Connecting";
                Sleep 100
                DoEvents
                For x = 1 To 20
                    p1.Print ".";
                    Sleep 100
                    DoEvents
                Next
                p1.Print "Connection Lost";
                Sleep 100
                DoEvents
            Case "portconnect 101.501.3.821"
                p1.Print "Connecting";
                Sleep 100
                DoEvents
                For x = 1 To 20
                    p1.Print ".";
                    Sleep 100
                    DoEvents
                Next
                p1.Print "Connection Complete"
                p1.Print "a: drive available"
                DoEvents
                CRACK(7) = True
            Case Else
                p1.Print "Connects to port"
                p1.Print "Ex1. portconnect ???.???.???.???"
                If CRACK(6) = True Then
                    l1.AddItem "121.312.12.441"
                    l1.AddItem "433.122.123.11"
                    l1.AddItem "101.501.3.821"
                ElseIf CRACK(6) = False Then
                    p1.Print "No ports available"
                End If
        End Select
    Case "decrypt.exe"
        CLEAR
        p1.Print "Decrypt Files"
        If CRACK(7) = True Then
            p1.Print "Enter decryption code"
            CRACK(8) = True
        Else
            COI
        End If
    Case "passcrypt"
        CLEAR
        If CRACK(8) = True Then
            p1.Print "Files Decrypted"
            CRACK(9) = True
        Else
            COI
        End If
    
    Case "access key"
        CLEAR
        If CRACK(9) = True Then
            p1.Print "Decode to unlock the Main Computer"
            p1.Print "%1001,%0111,%0001"
            CRACK(10) = True
        Else
            COI
        End If
    Case "9,7,1"
        CLEAR
        If CRACK(10) = True Then
            p1.Print "Access Granted, Good Hacking"
        Else
            COI
        End If
    Case "main computer"
        CLEAR
        If CRACK(10) = True Then
            p1.Print "Welcome"
        Else
            p1.Print "Access Denied"
        End If
    Case Else
        CLEAR
        COI
     
        
End Select

ENT = False
End Sub
Sub COI()
    p1.Print "Command Invalid"
End Sub
Sub CLEAR()
    p1.Cls
    p1.Picture = p3.Picture
End Sub



