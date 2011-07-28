unit GKScriptDaemon; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.ComponentModel, System.Windows.Forms,
  GKBase, GKUtils, GKEngine, GKEngineAPI, GKMain, GKLangs;

type
  TfmScriptDaemon = class(System.Windows.Forms.Form)
  strict private
    ToolBar1: System.Windows.Forms.ToolBar;
    btnLoadScript: System.Windows.Forms.ToolBarButton;
    ToolButton2: System.Windows.Forms.ToolBarButton;
    btnRun: System.Windows.Forms.ToolBarButton;
    mmDebugOutput: System.Windows.Forms.TextBox;
    mmScriptText: System.Windows.Forms.TextBox;
    OpenDialog1: System.Windows.Forms.OpenFileDialog;
    btnSaveScript: System.Windows.Forms.ToolBarButton;
    SaveDialog1: System.Windows.Forms.SaveFileDialog;
    btnNewScript: System.Windows.Forms.ToolBarButton;

    FBase: TfmBase;
    FFileName: string;
    FModified: Boolean;

    function CheckModified(): Boolean;
    procedure SetFileName(const Value: string);
    procedure SetModified(const Value: Boolean);
    procedure SetTitle();
    procedure InitializeComponent;
    procedure NewScript();
    procedure LoadScript();
    procedure SaveScript();
    procedure Run();

    procedure TfmScriptDaemon_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
    procedure mmScriptText_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure ToolBar1_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property FileName: string read FFileName write SetFileName;
    property Modified: Boolean read FModified write SetModified;

    procedure SetLang();
  end;

implementation

procedure TfmScriptDaemon.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ToolBarButton = array of System.Windows.Forms.ToolBarButton;
begin
  Self.ToolBar1 := System.Windows.Forms.ToolBar.Create;
  Self.btnNewScript := System.Windows.Forms.ToolBarButton.Create;
  Self.btnLoadScript := System.Windows.Forms.ToolBarButton.Create;
  Self.btnSaveScript := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolButton2 := System.Windows.Forms.ToolBarButton.Create;
  Self.btnRun := System.Windows.Forms.ToolBarButton.Create;
  Self.mmDebugOutput := System.Windows.Forms.TextBox.Create;
  Self.mmScriptText := System.Windows.Forms.TextBox.Create;
  Self.OpenDialog1 := System.Windows.Forms.OpenFileDialog.Create;
  Self.SaveDialog1 := System.Windows.Forms.SaveFileDialog.Create;
  Self.SuspendLayout;
  // 
  // ToolBar1
  // 
  Self.ToolBar1.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  Self.ToolBar1.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(Self.btnNewScript, 
          Self.btnLoadScript, Self.btnSaveScript, Self.ToolButton2, Self.btnRun));
  Self.ToolBar1.DropDownArrows := True;
  Self.ToolBar1.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar1.Name := 'ToolBar1';
  Self.ToolBar1.ShowToolTips := True;
  Self.ToolBar1.Size := System.Drawing.Size.Create(712, 28);
  Self.ToolBar1.TabIndex := 0;
  Include(Self.ToolBar1.ButtonClick, Self.ToolBar1_ButtonClick);
  // 
  // btnNewScript
  // 
  Self.btnNewScript.ImageIndex := 0;
  Self.btnNewScript.ToolTipText := 'Новый скрипт';
  // 
  // btnLoadScript
  // 
  Self.btnLoadScript.ImageIndex := 1;
  Self.btnLoadScript.ToolTipText := 'Загрузить скрипт';
  // 
  // btnSaveScript
  // 
  Self.btnSaveScript.ImageIndex := 2;
  Self.btnSaveScript.ToolTipText := 'Сохранить скрипт';
  // 
  // ToolButton2
  // 
  Self.ToolButton2.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // btnRun
  // 
  Self.btnRun.ImageIndex := 33;
  Self.btnRun.ToolTipText := 'Выполнить';
  // 
  // mmDebugOutput
  // 
  Self.mmDebugOutput.Location := System.Drawing.Point.Create(0, 286);
  Self.mmDebugOutput.Multiline := True;
  Self.mmDebugOutput.Name := 'mmDebugOutput';
  Self.mmDebugOutput.ReadOnly := True;
  Self.mmDebugOutput.Size := System.Drawing.Size.Create(712, 148);
  Self.mmDebugOutput.TabIndex := 1;
  Self.mmDebugOutput.Text := '';
  // 
  // mmScriptText
  // 
  Self.mmScriptText.Location := System.Drawing.Point.Create(0, 30);
  Self.mmScriptText.Multiline := True;
  Self.mmScriptText.Name := 'mmScriptText';
  Self.mmScriptText.Size := System.Drawing.Size.Create(712, 253);
  Self.mmScriptText.TabIndex := 2;
  Self.mmScriptText.Text := '';
  Include(Self.mmScriptText.TextChanged, Self.mmScriptText_TextChanged);
  // 
  // OpenDialog1
  // 
  Self.OpenDialog1.Filter := 'Скрипты|*.lua';
  // 
  // SaveDialog1
  // 
  Self.SaveDialog1.DefaultExt := 'lua';
  Self.SaveDialog1.Filter := 'Скрипты|*.lua';
  // 
  // TfmScriptDaemon
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(712, 434);
  Self.Controls.Add(Self.ToolBar1);
  Self.Controls.Add(Self.mmDebugOutput);
  Self.Controls.Add(Self.mmScriptText);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.Name := 'TfmScriptDaemon';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'ScriptDaemon';
  Include(Self.Closing, Self.TfmScriptDaemon_Closing);
  Self.ResumeLayout(False);
end;

constructor TfmScriptDaemon.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  ToolBar1.ImageList := fmGEDKeeper.ImageList_Buttons;

  //OnClose = FormClose

  NewScript();
  SetLang();
end;

procedure TfmScriptDaemon.SetLang();
begin
  //
end;

procedure TfmScriptDaemon.ToolBar1_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  if (e.Button = btnNewScript) then NewScript();
  if (e.Button = btnLoadScript) then LoadScript();
  if (e.Button = btnSaveScript) then SaveScript();
  if (e.Button = btnRun) then Run();
end;

procedure TfmScriptDaemon.TfmScriptDaemon_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
begin
  if not(CheckModified())
  then e.Cancel := True
  else e.Cancel := False;
end;

procedure TfmScriptDaemon.mmScriptText_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  Modified := True;
end;

procedure TfmScriptDaemon.SetFileName(const Value: string);
begin
  FFileName := Value;
  SetTitle();
end;

procedure TfmScriptDaemon.SetModified(const Value: Boolean);
begin
  FModified := Value;
  SetTitle();
end;

procedure TfmScriptDaemon.SetTitle();
begin
  Text := System.IO.Path.GetFileName(FFileName);

  if FModified
  then Text := '* ' + Text;
end;

procedure TfmScriptDaemon.NewScript();
begin
  if CheckModified() then begin
    mmScriptText.Clear;
    FileName := 'unknown.lua';
    Modified := False;
  end;
end;

procedure TfmScriptDaemon.LoadScript();
var
  strd: System.IO.StreamReader;
begin
  if CheckModified() and (OpenDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK) then begin
    strd := StreamReader.Create(System.IO.File.OpenRead(SaveDialog1.FileName));
    mmScriptText.Text := strd.ReadToEnd;
    FileName := OpenDialog1.FileName;
    Modified := False;
  end;
end;

procedure TfmScriptDaemon.SaveScript();
var
  strd: System.IO.StreamWriter;
begin
  SaveDialog1.FileName := FileName;
  if (SaveDialog1.ShowDialog = System.Windows.Forms.DialogResult.OK) then begin
    strd := StreamWriter.Create(SaveDialog1.FileName, False);
    strd.Write(mmScriptText.Text);
    FileName := SaveDialog1.FileName;
    Modified := False;
  end;
end;

procedure TfmScriptDaemon.Run();
var
  scr_engine: TScriptEngine;
begin
  mmDebugOutput.Clear;
  try
    scr_engine := TScriptEngine.Create;
    try
      scr_engine.lua_run(mmScriptText.Text, Base, mmDebugOutput);
    finally
      scr_engine.Free;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKScriptDaemon.Run(): ' + E.Message);
  end;
end;

function TfmScriptDaemon.CheckModified(): Boolean;
begin
  Result := True;

  if Modified then begin
    case MessageBox.Show(LSList[LSID_FileSaveQuery], TGenEngine.AppName, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning) of
      System.Windows.Forms.DialogResult.Yes: SaveScript();
      System.Windows.Forms.DialogResult.No: {dummy};
      System.Windows.Forms.DialogResult.Cancel: Result := False;
    end;
  end;
end;

end.
