unit GKLifeSettings;

interface

uses
  Buttons, Classes, Graphics, ComCtrls, Controls, ExtCtrls, Forms, StdCtrls;

type
  TfmLifeSettings = class(TForm)
    pagMain: TPageControl;
    tabGeneral: TTabSheet;
    grpGridSize: TGroupBox;
    lblGridSizeAcross: TLabel;
    lblGridSizeDown: TLabel;
    edtGridSizeAcross: TEdit;
    arwGridSizeAcross: TUpDown;
    edtGridSizeDown: TEdit;
    arwGridSizeDown: TUpDown;
    grpColour: TGroupBox;
    lblColourBackground: TLabel;
    lblColourLivingCells: TLabel;
    grpDisplayedInformation: TGroupBox;
    chkDisplayedInformationGeneration: TCheckBox;
    chkDisplayedInformationLivingCells: TCheckBox;
    grpAnimation: TGroupBox;
    lblAnimationDelay: TLabel;
    edtAnimationDelay: TEdit;
    arwAnimationDelay: TUpDown;
    btnRestoreGeneralDefaults: TBitBtn;
    tabRules: TTabSheet;
    grpLiveCells: TGroupBox;
    chkLiveCell0: TCheckBox;
    chkLiveCell1: TCheckBox;
    chkLiveCell2: TCheckBox;
    chkLiveCell3: TCheckBox;
    chkLiveCell4: TCheckBox;
    chkLiveCell5: TCheckBox;
    chkLiveCell6: TCheckBox;
    chkLiveCell7: TCheckBox;
    chkLiveCell8: TCheckBox;
    grpDeadCells: TGroupBox;
    chkDeadCell0: TCheckBox;
    chkDeadCell1: TCheckBox;
    chkDeadCell2: TCheckBox;
    chkDeadCell3: TCheckBox;
    chkDeadCell4: TCheckBox;
    chkDeadCell5: TCheckBox;
    chkDeadCell6: TCheckBox;
    chkDeadCell7: TCheckBox;
    chkDeadCell8: TCheckBox;
    btnRestoreRuleDefaults: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure btnRestoreGeneralDefaultsClick(Sender: TObject);
    procedure btnRestoreRuleDefaultsClick(Sender: TObject);
    procedure DisplayedInformationChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ReadOptions;
    procedure ReadRules;
  end;

implementation

uses
  GKLifeMain, UOptions;

{$R *.DFM}

procedure TfmLifeSettings.btnOKClick(Sender: TObject);
begin
  with Options do begin
    GridHeight := arwGridSizeDown.Position;
    GridWidth := arwGridSizeAcross.Position;
    BackgroundColor := clBlack;//btnColourBackground.Color;
    LivingCellColor := clLime;//btnColourLivingCells.Color;
    DisplayGeneration := chkDisplayedInformationGeneration.Checked;
    DisplayedInformationColor := clYellow;//btnDisplayedInformationColour.Color;
    DisplayLivingCells := chkDisplayedInformationLivingCells.Checked;
    AnimationDelay := arwAnimationDelay.Position;

    if Modified then
      SaveToRegistry
  end;

  with Rules do begin
    LiveCells[0] := chkLiveCell0.Checked;
    LiveCells[1] := chkLiveCell1.Checked;
    LiveCells[2] := chkLiveCell2.Checked;
    LiveCells[3] := chkLiveCell3.Checked;
    LiveCells[4] := chkLiveCell4.Checked;
    LiveCells[5] := chkLiveCell5.Checked;
    LiveCells[6] := chkLiveCell6.Checked;
    LiveCells[7] := chkLiveCell7.Checked;
    LiveCells[8] := chkLiveCell8.Checked;

    DeadCells[0] := chkDeadCell0.Checked;
    DeadCells[1] := chkDeadCell1.Checked;
    DeadCells[2] := chkDeadCell2.Checked;
    DeadCells[3] := chkDeadCell3.Checked;
    DeadCells[4] := chkDeadCell4.Checked;
    DeadCells[5] := chkDeadCell5.Checked;
    DeadCells[6] := chkDeadCell6.Checked;
    DeadCells[7] := chkDeadCell7.Checked;
    DeadCells[8] := chkDeadCell8.Checked;

    if Modified then
      SaveToRegistry
  end
end;

procedure TfmLifeSettings.btnRestoreGeneralDefaultsClick(Sender: TObject);
begin
  Options.RestoreDefaults;
  ReadOptions
end;

procedure TfmLifeSettings.btnRestoreRuleDefaultsClick(Sender: TObject);
begin
  Rules.RestoreDefaults;
  ReadRules
end;

procedure TfmLifeSettings.DisplayedInformationChanged(Sender: TObject);
begin
  //btnDisplayedInformationColour.Enabled := chkDisplayedInformationGeneration.Checked or
    //                                       chkDisplayedInformationLivingCells.Checked
end;

procedure TfmLifeSettings.FormCreate(Sender: TObject);
begin
  //btnColourBackground.CustomColorsKey := RegistryKey;
  //btnColourLivingCells.CustomColorsKey := RegistryKey;
  //btnDisplayedInformationColour.CustomColorsKey := RegistryKey;
  ReadOptions;
  ReadRules
end;

procedure TfmLifeSettings.ReadOptions;
begin
  with Options do begin
    arwGridSizeAcross.Position := GridWidth;
    arwGridSizeDown.Position := GridHeight;
    //btnColourBackground.Color := BackgroundColor;
    //btnColourLivingCells.Color := LivingCellColor;
    chkDisplayedInformationGeneration.Checked := DisplayGeneration;
    //btnDisplayedInformationColour.Color := DisplayedInformationColor;
    chkDisplayedInformationLivingCells.Checked := DisplayLivingCells;
    arwAnimationDelay.Position := AnimationDelay;
  end
end;

procedure TfmLifeSettings.ReadRules;
begin
  with Rules do begin
    chkLiveCell0.Checked := LiveCells[0];
    chkLiveCell1.Checked := LiveCells[1];
    chkLiveCell2.Checked := LiveCells[2];
    chkLiveCell3.Checked := LiveCells[3];
    chkLiveCell4.Checked := LiveCells[4];
    chkLiveCell5.Checked := LiveCells[5];
    chkLiveCell6.Checked := LiveCells[6];
    chkLiveCell7.Checked := LiveCells[7];
    chkLiveCell8.Checked := LiveCells[8];

    chkDeadCell0.Checked := DeadCells[0];
    chkDeadCell1.Checked := DeadCells[1];
    chkDeadCell2.Checked := DeadCells[2];
    chkDeadCell3.Checked := DeadCells[3];
    chkDeadCell4.Checked := DeadCells[4];
    chkDeadCell5.Checked := DeadCells[5];
    chkDeadCell6.Checked := DeadCells[6];
    chkDeadCell7.Checked := DeadCells[7];
    chkDeadCell8.Checked := DeadCells[8]
  end
end;

end.
