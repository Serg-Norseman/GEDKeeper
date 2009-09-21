object fmChart: TfmChart
  Left = 263
  Top = 107
  Width = 757
  Height = 590
  Caption = #1044#1080#1072#1075#1088#1072#1084#1084#1072
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 749
    Height = 30
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 27
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    Images = fmGEDKeeper.ImageList1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tbImageSave: TToolButton
      Left = 0
      Top = 0
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077
      Caption = 'tbImageSave'
      ImageIndex = 6
      OnClick = tbImageSaveClick
    end
    object ToolButton1: TToolButton
      Left = 27
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object Label1: TLabel
      Left = 35
      Top = 0
      Width = 126
      Height = 26
      Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1087#1086#1082#1086#1083#1077#1085#1080#1081': '
      Layout = tlCenter
    end
    object ListDepthLimit: TComboBox
      Left = 161
      Top = 2
      Width = 110
      Height = 21
      Style = csDropDownList
      DropDownCount = 10
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = #1085#1077#1086#1075#1088#1072#1085#1080#1095#1077#1085#1085#1086
      OnChange = ListDepthLimitChange
      Items.Strings = (
        #1085#1077#1086#1075#1088#1072#1085#1080#1095#1077#1085#1085#1086
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8'
        '9')
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 30
    Width = 749
    Height = 533
    Align = alClient
    TabOrder = 1
    OnResize = ScrollBox1Resize
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 400
      Height = 300
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'jpg'
    Filter = #1060#1072#1081#1083#1099' JPEG (*.jpg)|*.jpg'
    Left = 128
    Top = 120
  end
end
