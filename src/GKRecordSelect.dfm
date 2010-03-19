object fmRecordSelect: TfmRecordSelect
  Left = 548
  Top = 199
  BorderStyle = bsDialog
  Caption = #1042#1099#1073#1086#1088' '#1079#1072#1087#1080#1089#1080
  ClientHeight = 369
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object ListRecords: TBSListView
    Left = 8
    Top = 8
    Width = 369
    Height = 313
    Columns = <>
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    SortColumn = 0
    SortDirection = sdAscending
  end
  object btnSelect: TBitBtn
    Left = 200
    Top = 336
    Width = 81
    Height = 25
    Caption = #1042#1099#1073#1088#1072#1090#1100
    TabOrder = 2
    OnClick = btnSelectClick
    Kind = bkOK
  end
  object btnCreate: TBitBtn
    Left = 104
    Top = 336
    Width = 81
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 1
    OnClick = btnCreateClick
  end
  object btnCancel: TBitBtn
    Left = 296
    Top = 336
    Width = 81
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    Kind = bkCancel
  end
end
