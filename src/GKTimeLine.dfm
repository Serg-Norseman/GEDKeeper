object fmTimeLine: TfmTimeLine
  Left = 339
  Top = 427
  BorderStyle = bsToolWindow
  Caption = #1051#1080#1085#1080#1103' '#1074#1088#1077#1084#1077#1085#1080
  ClientHeight = 50
  ClientWidth = 524
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbTimeLine: TTrackBar
    Left = 0
    Top = 0
    Width = 524
    Height = 31
    Align = alClient
    Min = 5
    PageSize = 1
    Position = 5
    TabOrder = 0
    OnChange = tbTimeLineChange
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 31
    Width = 524
    Height = 19
    Panels = <
      item
        Width = 300
      end
      item
        Width = 50
      end>
  end
end
