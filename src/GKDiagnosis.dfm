object fmDiagnosis: TfmDiagnosis
  Left = 357
  Top = 167
  BorderStyle = bsDialog
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1076#1072#1085#1085#1099#1093
  ClientHeight = 420
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
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
  object ListErrors: TBSListView
    Left = 0
    Top = 0
    Width = 636
    Height = 420
    Align = alClient
    Columns = <
      item
        Caption = #1047#1072#1087#1080#1089#1100
        Width = 100
      end
      item
        Caption = #1044#1080#1072#1075#1085#1086#1089#1090#1080#1082#1072
        Width = 500
      end>
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    SortColumn = 0
    SortDirection = sdAscending
  end
end
