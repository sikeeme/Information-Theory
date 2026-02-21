object Form3: TForm3
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Form3'
  ClientHeight = 477
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 40
    Top = 16
    Width = 86
    Height = 42
    Caption = 'Label1'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -31
    Font.Name = 'Segoe UI Semibold'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 80
    Width = 156
    Height = 25
    Caption = #1048#1089#1093#1086#1076#1085#1072#1103' '#1089#1090#1088#1086#1082#1072': '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 222
    Width = 51
    Height = 25
    Caption = #1050#1083#1102#1095':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 360
    Top = 209
    Width = 88
    Height = 25
    Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object KeySecondEd: TEdit
    Left = 16
    Top = 312
    Width = 305
    Height = 33
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TextHint = #1050#1083#1102#1095' 2'
  end
  object KeyThirdEd: TEdit
    Left = 16
    Top = 368
    Width = 305
    Height = 33
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TextHint = #1050#1083#1102#1095' 3'
  end
  object EncryptBt: TButton
    Left = 352
    Top = 149
    Width = 137
    Height = 33
    Caption = #1064#1080#1092#1088#1086#1074#1072#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = EncryptBtClick
  end
  object DecipherBt: TButton
    Left = 495
    Top = 149
    Width = 154
    Height = 33
    Caption = #1044#1077#1096#1080#1092#1088#1086#1074#1072#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = DecipherBtClick
  end
  object KeyFirstEd: TEdit
    Left = 16
    Top = 256
    Width = 305
    Height = 33
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    TextHint = #1050#1083#1102#1095' 1'
  end
  object KeyFourthEd: TEdit
    Left = 16
    Top = 424
    Width = 305
    Height = 33
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    TextHint = #1050#1083#1102#1095' 4'
  end
  object OpenBt: TButton
    Left = 352
    Top = 110
    Width = 297
    Height = 33
    Caption = #1054#1090#1082#1088#1099#1090#1100' '#1092#1072#1081#1083' '#1076#1083#1103' '#1095#1090#1077#1085#1080#1103
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = OpenBtClick
  end
  object SaveBt: TButton
    Left = 352
    Top = 367
    Width = 297
    Height = 33
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090' '#1074' '#1092#1072#1081#1083
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = SaveBtClick
  end
  object ClearBt: TButton
    Left = 352
    Top = 423
    Width = 297
    Height = 33
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = ClearBtClick
  end
  object ResM: TMemo
    Left = 352
    Top = 240
    Width = 297
    Height = 105
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'ResM')
    ParentFont = False
    TabOrder = 9
  end
  object OrM: TMemo
    Left = 16
    Top = 111
    Width = 305
    Height = 105
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'OrM')
    ParentFont = False
    TabOrder = 10
  end
  object OpenDialog1: TOpenDialog
    Left = 536
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 448
    Top = 8
  end
end
