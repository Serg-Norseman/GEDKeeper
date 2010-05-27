object fmMediaView: TfmMediaView
  Left = 306
  Top = 106
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1055#1088#1086#1089#1084#1086#1090#1088
  ClientHeight = 573
  ClientWidth = 792
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 792
    Height = 573
    ActivePage = SheetText
    Align = alClient
    TabOrder = 0
    object SheetText: TTabSheet
      Caption = 'SheetText'
      TabVisible = False
      object mText: TMemo
        Left = 0
        Top = 0
        Width = 784
        Height = 563
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object SheetRTF: TTabSheet
      Caption = 'SheetRTF'
      ImageIndex = 1
      TabVisible = False
      object RichEdit: TRichEdit
        Left = 0
        Top = 0
        Width = 784
        Height = 563
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object SheetImage: TTabSheet
      Caption = 'SheetImage'
      ImageIndex = 2
      TabVisible = False
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 825
        Height = 526
        Align = alClient
        Center = True
        Proportional = True
        Stretch = True
      end
    end
    object SheetPlayer: TTabSheet
      Caption = 'SheetPlayer'
      ImageIndex = 3
      TabVisible = False
    end
    object SheetHTML: TTabSheet
      Caption = 'SheetHTML'
      ImageIndex = 4
      TabVisible = False
      object WebBrowser1: TWebBrowser
        Left = 0
        Top = 0
        Width = 825
        Height = 526
        Align = alClient
        TabOrder = 0
        ControlData = {
          4C000000445500005D3600000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
end
