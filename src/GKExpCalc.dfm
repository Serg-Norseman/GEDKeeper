object fmCalcWidget: TfmCalcWidget
  Left = 582
  Top = 151
  ActiveControl = edExpression
  BorderStyle = bsToolWindow
  Caption = #1050#1072#1083#1100#1082#1091#1083#1103#1090#1086#1088' '#1074#1099#1088#1072#1078#1077#1085#1080#1081
  ClientHeight = 201
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListOutput: TListBox
    Left = 8
    Top = 8
    Width = 257
    Height = 105
    TabStop = False
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 0
  end
  object edExpression: TEdit
    Left = 8
    Top = 120
    Width = 257
    Height = 21
    TabOrder = 1
    OnKeyDown = edExpressionKeyDown
  end
  object chkPutToClipboard: TCheckBox
    Left = 8
    Top = 176
    Width = 257
    Height = 17
    Caption = #1055#1086#1084#1077#1089#1090#1080#1090#1100' '#1088#1077#1079#1091#1083#1100#1090#1072#1090' '#1074' '#1073#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
    TabOrder = 3
  end
  object edCalcResult: TEdit
    Left = 8
    Top = 144
    Width = 257
    Height = 21
    TabStop = False
    Color = clBtnFace
    DragMode = dmAutomatic
    ReadOnly = True
    TabOrder = 2
    OnDragOver = edCalcResultDragOver
    OnKeyDown = edExpressionKeyDown
  end
end
