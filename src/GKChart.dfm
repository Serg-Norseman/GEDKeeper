object fmChart: TfmChart
  Left = 359
  Top = 150
  Width = 838
  Height = 491
  Caption = #1044#1080#1072#1075#1088#1072#1084#1084#1072
  Color = clBtnFace
  DefaultMonitor = dmMainForm
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
    Width = 830
    Height = 30
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 27
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    Images = fmGEDKeeper.ImageList_Buttons
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
    object ToolButton2: TToolButton
      Left = 271
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 8
      Style = tbsSeparator
    end
    object tbPrev: TToolButton
      Left = 279
      Top = 0
      Caption = 'tbPrev'
      ImageIndex = 22
      OnClick = tbPrevClick
    end
    object tbNext: TToolButton
      Left = 306
      Top = 0
      Caption = 'tbNext'
      Enabled = False
      ImageIndex = 23
      OnClick = tbNextClick
    end
    object ToolButton3: TToolButton
      Left = 333
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 24
      Style = tbsSeparator
    end
    object TrackBar1: TTrackBar
      Left = 341
      Top = 0
      Width = 153
      Height = 26
      Hint = #1052#1072#1089#1096#1090#1072#1073
      LineSize = 5
      Min = 5
      PageSize = 1
      Position = 10
      TabOrder = 1
      OnChange = TrackBar1Change
    end
    object ToolButton5: TToolButton
      Left = 494
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 25
      Style = tbsSeparator
    end
    object tbFilter: TToolButton
      Left = 502
      Top = 0
      Hint = #1060#1080#1083#1100#1090#1088#1072#1094#1080#1103' '#1076#1088#1077#1074#1072
      Caption = 'tbFilter'
      ImageIndex = 16
      OnClick = tbFilterClick
    end
    object ToolButton6: TToolButton
      Left = 529
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 17
      Style = tbsSeparator
    end
    object tbMode: TToolButton
      Left = 537
      Top = 0
      Hint = #1056#1077#1078#1080#1084#1099
      Caption = 'tbMode'
      DropdownMenu = PopupMenu2
      ImageIndex = 21
      Style = tbsDropDown
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'tga'
    Filter = 
      #1060#1072#1081#1083#1099' BMP (*.bmp)|*.bmp|'#1060#1072#1081#1083#1099' JPEG (*.jpg)|*.jpg|'#1060#1072#1081#1083#1099' EMF (*.em' +
      'f)|*.emf'
    FilterIndex = 2
    Left = 128
    Top = 120
  end
  object PopupMenu1: TPopupMenu
    Left = 128
    Top = 166
    object miEdit: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
      Default = True
      OnClick = miEditClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miFamilyAdd: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1089#1077#1084#1100#1102
      OnClick = miFamilyAddClick
    end
    object miSpouseAdd: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1089#1091#1087#1088#1091#1075#1072'('#1091')'
      OnClick = miSpouseAddClick
    end
    object miSonAdd: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1089#1099#1085#1072
      OnClick = miSonAddClick
    end
    object miDaughterAdd: TMenuItem
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1076#1086#1095#1100
      OnClick = miDaughterAddClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100
      OnClick = miDeleteClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object miRebuildTree: TMenuItem
      Caption = #1055#1077#1088#1077#1089#1090#1088#1086#1080#1090#1100' '#1076#1088#1077#1074#1086
      ShortCut = 117
      OnClick = miRebuildTreeClick
    end
    object miRebuildKinships: TMenuItem
      Caption = #1055#1077#1088#1077#1089#1090#1088#1086#1080#1090#1100' '#1086#1090#1085#1086#1096#1077#1085#1080#1103
      ShortCut = 118
      OnClick = miRebuildKinshipsClick
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 128
    Top = 216
    object miModeBoth: TMenuItem
      Caption = #1042#1089#1105
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = miModeDescendantsClick
    end
    object miModeAncestors: TMenuItem
      Caption = #1058#1086#1083#1100#1082#1086' '#1087#1088#1077#1076#1082#1080
      GroupIndex = 1
      RadioItem = True
      OnClick = miModeDescendantsClick
    end
    object miModeDescendants: TMenuItem
      Caption = #1058#1086#1083#1100#1082#1086' '#1087#1086#1090#1086#1084#1082#1080
      GroupIndex = 1
      RadioItem = True
      OnClick = miModeDescendantsClick
    end
    object N7: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object miTraceRoot: TMenuItem
      Caption = #1040#1074#1090#1086#1089#1084#1077#1085#1072' '#1094#1077#1085#1090#1088#1072
      GroupIndex = 2
      OnClick = miTraceRootClick
    end
  end
end
