object fmMediaView: TfmMediaView
  Left = 277
  Top = 103
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = #1055#1088#1086#1089#1084#1086#1090#1088
  ClientHeight = 536
  ClientWidth = 833
  Color = clBtnFace
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
    Width = 833
    Height = 536
    ActivePage = SheetHTML
    Align = alClient
    TabOrder = 0
    object SheetText: TTabSheet
      Caption = 'SheetText'
      TabVisible = False
      object mText: TMemo
        Left = 0
        Top = 0
        Width = 825
        Height = 526
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
        Width = 825
        Height = 526
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
      OnResize = SheetPlayerResize
      object WindowsMediaPlayer1: TWindowsMediaPlayer
        Left = 0
        Top = 0
        Width = 240
        Height = 245
        TabOrder = 0
        ControlData = {
          000300000800000000000500000000000000F03F030000000000050000000000
          0000000008000200000000000300010000000B00FFFF0300000000000B00FFFF
          08000200000000000300320000000B00000008000A000000660075006C006C00
          00000B0000000B0000000B0000000B00FFFF0B00000008000200000000000800
          020000000000080002000000000008000200000000000B000000CE1800005219
          0000}
      end
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
