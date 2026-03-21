object LFSR: TLFSR
  Left = 0
  Top = 0
  Caption = 'LFSR'
  ClientHeight = 585
  ClientWidth = 659
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 88
    Width = 244
    Height = 23
    Caption = #1042#1093#1086#1076#1085#1086#1081' '#1092#1072#1081#1083' ('#1076#1074#1086#1080#1095#1085#1099#1081' '#1074#1080#1076'):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 404
    Width = 80
    Height = 23
    Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 268
    Width = 135
    Height = 23
    Caption = #1050#1083#1102#1095#1077#1074#1086#1081' '#1087#1086#1090#1086#1082':'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 25
    Width = 200
    Height = 23
    Caption = #1057#1090#1072#1088#1090#1086#1074#1099#1081' '#1082#1083#1102#1095' (30 '#1073#1080#1090'):'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object CipherM: TMemo
    Left = 8
    Top = 433
    Width = 643
    Height = 105
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object FileM: TMemo
    Left = 8
    Top = 117
    Width = 643
    Height = 105
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'Memo2')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object AlgoritmBtn: TButton
    Left = 8
    Top = 228
    Width = 643
    Height = 34
    Caption = #1064#1080#1092#1088#1086#1074#1072#1090#1100' / '#1044#1077#1096#1080#1092#1088#1086#1074#1072#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = AlgoritmBtnClick
  end
  object ClearBtn: TButton
    Left = 336
    Top = 544
    Width = 315
    Height = 33
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = ClearBtnClick
  end
  object KeyEd: TEdit
    Left = 8
    Top = 54
    Width = 643
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    TextHint = #1042#1074#1086#1076#1080#1090#1100' '#1090#1086#1083#1100#1082#1086' 0 '#1080' 1 '
    OnKeyPress = KeyEdKeyPress
  end
  object LoadBtn: TButton
    Left = 442
    Top = 8
    Width = 209
    Height = 33
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1092#1072#1081#1083
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = LoadBtnClick
  end
  object ResKeyM: TMemo
    Left = 8
    Top = 297
    Width = 643
    Height = 105
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    Lines.Strings = (
      'ResKeyM')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object SaveBtn: TButton
    Left = 8
    Top = 544
    Width = 313
    Height = 33
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = SaveBtnClick
  end
  object OpenDialog: TOpenDialog
    Left = 352
    Top = 8
  end
end
