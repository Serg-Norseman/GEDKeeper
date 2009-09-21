object fmSourceEdit: TfmSourceEdit
  Left = 494
  Top = 214
  BorderStyle = bsDialog
  Caption = #1048#1089#1090#1086#1095#1085#1080#1082
  ClientHeight = 337
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 93
    Height = 13
    Caption = #1050#1088#1072#1090#1082#1086#1077' '#1085#1072#1079#1074#1072#1085#1080#1077
  end
  object Label2: TLabel
    Left = 8
    Top = 120
    Width = 48
    Height = 13
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077
  end
  object Label3: TLabel
    Left = 8
    Top = 32
    Width = 31
    Height = 13
    Caption = #1040#1074#1090#1086#1088
  end
  object Label4: TLabel
    Left = 8
    Top = 208
    Width = 74
    Height = 13
    Caption = #1054#1087#1091#1073#1083#1080#1082#1086#1074#1072#1085#1086
  end
  object btnAccept: TBitBtn
    Left = 184
    Top = 304
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 4
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 272
    Top = 304
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 5
    Kind = bkCancel
  end
  object EditShortTitle: TEdit
    Left = 120
    Top = 8
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object EditAuthor: TMemo
    Left = 120
    Top = 32
    Width = 233
    Height = 81
    TabOrder = 1
  end
  object EditTitle: TMemo
    Left = 120
    Top = 120
    Width = 233
    Height = 81
    TabOrder = 2
  end
  object EditPublication: TMemo
    Left = 120
    Top = 208
    Width = 233
    Height = 81
    TabOrder = 3
  end
end
