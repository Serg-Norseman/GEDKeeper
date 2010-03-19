object fmChart: TfmChart
  Left = 266
  Top = 103
  Width = 757
  Height = 590
  Caption = #1044#1080#1072#1075#1088#1072#1084#1084#1072
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
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
      Top = 0
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
    object ToolButton2: TToolButton
      Left = 271
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 8
      Style = tbsSeparator
    end
    object tbGotoPerson: TToolButton
      Left = 279
      Top = 0
      Caption = 'tbGotoPerson'
      ImageIndex = 24
      OnClick = tbGotoPersonClick
    end
    object ToolButton4: TToolButton
      Left = 306
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object tbPrev: TToolButton
      Left = 314
      Top = 0
      Caption = 'tbPrev'
      ImageIndex = 22
      OnClick = tbPrevClick
    end
    object tbNext: TToolButton
      Left = 341
      Top = 0
      Caption = 'tbNext'
      Enabled = False
      ImageIndex = 23
      OnClick = tbNextClick
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
