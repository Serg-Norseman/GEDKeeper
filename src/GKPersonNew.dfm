object fmPersonNew: TfmPersonNew
  Left = 713
  Top = 177
  BorderStyle = bsDialog
  Caption = #1053#1086#1074#1072#1103' '#1087#1077#1088#1089#1086#1085#1072#1083#1100#1085#1072#1103' '#1079#1072#1087#1080#1089#1100
  ClientHeight = 153
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 44
    Height = 13
    Caption = #1060#1072#1084#1080#1083#1080#1103
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 19
    Height = 13
    Caption = #1048#1084#1103
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 49
    Height = 13
    Caption = #1054#1090#1095#1077#1089#1090#1074#1086
  end
  object Label4: TLabel
    Left = 8
    Top = 88
    Width = 19
    Height = 13
    Caption = #1055#1086#1083
  end
  object EditFamily: TEdit
    Left = 64
    Top = 8
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object EditName: TEdit
    Left = 64
    Top = 32
    Width = 185
    Height = 21
    TabOrder = 1
  end
  object EditPatronymic: TComboBox
    Left = 64
    Top = 56
    Width = 185
    Height = 21
    ItemHeight = 13
    TabOrder = 2
  end
  object EditSex: TComboBox
    Left = 64
    Top = 80
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object btnAccept: TBitBtn
    Left = 48
    Top = 120
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 4
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 136
    Top = 120
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 5
    Kind = bkCancel
  end
end
