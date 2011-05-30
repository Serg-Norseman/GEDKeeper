object fmNamesBook: TfmNamesBook
  Left = 397
  Top = 112
  BorderStyle = bsToolWindow
  Caption = #1057#1087#1088#1072#1074#1086#1095#1085#1080#1082' '#1080#1084#1077#1085
  ClientHeight = 353
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object cbNames: TComboBox
    Left = 8
    Top = 8
    Width = 257
    Height = 169
    Style = csSimple
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnSelect = cbNamesSelect
  end
  object mmDesc: TMemo
    Left = 8
    Top = 184
    Width = 257
    Height = 161
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
