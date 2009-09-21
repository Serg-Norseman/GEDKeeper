object fmNameEdit: TfmNameEdit
  Left = 452
  Top = 269
  BorderStyle = bsDialog
  Caption = #1048#1084#1103
  ClientHeight = 105
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnAccept: TBitBtn
    Left = 16
    Top = 72
    Width = 81
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 104
    Top = 72
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
  object EditKind: TComboBox
    Left = 8
    Top = 8
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = #1044#1088#1091#1075#1086#1077' '#1080#1084#1103
    Items.Strings = (
      #1044#1088#1091#1075#1086#1077' '#1080#1084#1103
      #1055#1089#1077#1074#1076#1086#1085#1080#1084
      #1055#1088#1086#1079#1074#1080#1097#1077
      #1044#1091#1093#1086#1074#1085#1086#1077' '#1080#1084#1103)
  end
  object EditName: TEdit
    Left = 8
    Top = 40
    Width = 177
    Height = 21
    TabOrder = 1
  end
end
