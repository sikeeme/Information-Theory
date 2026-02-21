object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1064#1080#1092#1088#1099
  ClientHeight = 346
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 184
    Top = 72
    Width = 254
    Height = 45
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1096#1080#1092#1088'?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object PlayfairBt: TButton
    Left = 32
    Top = 176
    Width = 233
    Height = 81
    Caption = #1055#1083#1077#1081#1092#1077#1081#1088#1072
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = PlayfairBtClick
  end
  object VigenereBt: TButton
    Left = 344
    Top = 176
    Width = 233
    Height = 81
    Caption = #1042#1080#1078#1077#1085#1077#1088#1072
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = VigenereBtClick
  end
end
