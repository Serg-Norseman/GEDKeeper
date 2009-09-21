object fmFilter: TfmFilter
  Left = 529
  Top = 250
  BorderStyle = bsDialog
  Caption = #1060#1080#1083#1100#1090#1088
  ClientHeight = 186
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 96
    Width = 64
    Height = 13
    Caption = #1052#1072#1089#1082#1072' '#1080#1084#1077#1085#1080
  end
  object btnAccept: TBitBtn
    Left = 120
    Top = 152
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = btnAcceptClick
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 208
    Top = 152
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object rgLife: TRadioGroup
    Left = 8
    Top = 8
    Width = 137
    Height = 73
    Items.Strings = (
      #1074#1089#1077
      #1090#1086#1083#1100#1082#1086' '#1078#1080#1074#1099#1077
      #1090#1086#1083#1100#1082#1086' '#1091#1084#1077#1088#1096#1080#1077)
    TabOrder = 0
  end
  object EditName: TEdit
    Left = 8
    Top = 112
    Width = 281
    Height = 21
    TabOrder = 1
    Text = '*'
  end
  object rgSex: TRadioGroup
    Left = 152
    Top = 8
    Width = 137
    Height = 73
    Items.Strings = (
      #1074#1089#1077
      #1090#1086#1083#1100#1082#1086' '#1084#1091#1078#1095#1080#1085#1099
      #1090#1086#1083#1100#1082#1086' '#1078#1077#1085#1097#1080#1085#1099)
    TabOrder = 4
  end
end
