unit GKNamesBook; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.ComponentModel, System.Windows.Forms,
  System.Reflection, System.Resources, System.Text, VCLStub, GedCom551, GKCtrls;

type
  TfmNamesBook = class(System.Windows.Forms.Form)
  strict private
    cbNames: System.Windows.Forms.ComboBox;
    mmDesc: System.Windows.Forms.TextBox;

    procedure InitializeComponent;
    procedure TfmNamesBook_Closed(sender: System.Object; e: System.EventArgs);
    procedure cbNames_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  strict private
    type
      TNameRecord = class(System.Object)
      public
        Name: string;
        Desc: string;
        Sex: TGEDCOMObject.TGEDCOMSex;
        ChIndex: Integer;
      end;

    var
      FNames: TObjectList;
      FChurchFNames, FChurchMNames: TStringList;

    procedure PrepareList();
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create;
  end;

implementation

{$R .\res\names.res}

uses
  GKUtils, GKMain, GKLangs;

procedure TfmNamesBook.InitializeComponent;
begin
  Self.cbNames := System.Windows.Forms.ComboBox.Create;
  Self.mmDesc := System.Windows.Forms.TextBox.Create;
  Self.SuspendLayout;
  //
  // cbNames
  // 
  Self.cbNames.DropDownStyle := System.Windows.Forms.ComboBoxStyle.Simple;
  Self.cbNames.Location := System.Drawing.Point.Create(8, 8);
  Self.cbNames.Name := 'cbNames';
  Self.cbNames.Size := System.Drawing.Size.Create(257, 168);
  Self.cbNames.Sorted := True;
  Self.cbNames.TabIndex := 0;
  Include(Self.cbNames.SelectedIndexChanged, Self.cbNames_SelectedIndexChanged);
  // 
  // mmDesc
  // 
  Self.mmDesc.BackColor := System.Drawing.SystemColors.Control;
  Self.mmDesc.Location := System.Drawing.Point.Create(8, 184);
  Self.mmDesc.Multiline := True;
  Self.mmDesc.Name := 'mmDesc';
  Self.mmDesc.ReadOnly := True;
  Self.mmDesc.ScrollBars := System.Windows.Forms.ScrollBars.Vertical;
  Self.mmDesc.Size := System.Drawing.Size.Create(257, 161);
  Self.mmDesc.TabIndex := 1;
  Self.mmDesc.Text := '';
  // 
  // TfmNamesBook
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(274, 353);
  Self.Controls.Add(Self.cbNames);
  Self.Controls.Add(Self.mmDesc);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedToolWindow;
  Self.Name := 'TfmNamesBook';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'TfmNamesBook';
  Self.TopMost := True;
  Include(Self.Closed, Self.TfmNamesBook_Closed);
  Self.ResumeLayout(False);
end;

constructor TfmNamesBook.Create;
begin
  inherited Create;
  InitializeComponent;

  FNames := TObjectList.Create(True);
  FChurchFNames := TStringList.Create;
  FChurchMNames := TStringList.Create;
  PrepareList();

  // SetLang
  Text := LSList[LSID_MINamesBook];
end;

procedure TfmNamesBook.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FChurchFNames.Free;
    FChurchMNames.Free;
    FNames.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmNamesBook.TfmNamesBook_Closed(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.miNamesBook.Checked := False;
  fmGEDKeeper.fmNamesBook := nil;
end;

procedure TfmNamesBook.PrepareList();

  function ExtractFlags(var st: string): Boolean;
  begin
    Result := (Length(st) >= 2) and (st[1] = '[') and (st[Length(st)] = ']');
    if Result
    then st := Copy(st, 2, Length(st) - 2);
  end;

var
  fs: TResourceStream;
  ns, st: string;
  rec: TNameRecord;
  i, k: Integer;
  lst: TStringList;
  strd: System.IO.StreamReader;
begin
  fs := TResourceStream.Create(HInstance, 'NAMES_DATA', RT_RCDATA);
  strd := StreamReader.Create(TStreamToCLRStream.GetStream(fs), Encoding.GetEncoding(1251));
  try
    while not(strd.Peek = -1) do begin
      ns := strd.ReadLine().Trim();

      if (ns <> '') and (TGKUtils.GetTokensCount(ns, '/') >= 3) then begin
        rec := TNameRecord.Create;
        rec.Name := TGKUtils.GetToken(ns, '/', 1).Trim();
        rec.Desc := TGKUtils.GetToken(ns, '/', 3).Trim();

        st := TGKUtils.GetToken(ns, '/', 2).Trim();
        if ExtractFlags(st) then begin
          case st[1] of
            'm': rec.Sex := svMale;
            'f': rec.Sex := svFemale;
          end;
        end;

        FNames.Add(rec);
      end;
    end;
  finally
    strd.Free;
    fs.Free;
  end;

  fs := TResourceStream.Create(HInstance, 'CHURCH_FNAMES_DATA', RT_RCDATA);
  try
    FChurchFNames.LoadFromStream(fs);
  finally
    fs.Free;
  end;

  fs := TResourceStream.Create(HInstance, 'CHURCH_MNAMES_DATA', RT_RCDATA);
  try
    FChurchMNames.LoadFromStream(fs);
  finally
    fs.Free;
  end;

  cbNames.BeginUpdate;
  try
    cbNames.Items.Clear;
    for i := 0 to FNames.Count - 1 do begin
      rec := TNameRecord(FNames[i]);
      ns := rec.Name;
      cbNames.Items.Add(TComboItem.Create(ns, rec));

      rec.ChIndex := -1;
      ns := ns.ToUpper();
      if (rec.Sex = svMale) then lst := FChurchMNames
      else
      if (rec.Sex = svFemale) then lst := FChurchFNames;

      for k := 0 to lst.Count - 1 do begin
        st := lst[k];
        if (st[1] = '-') and (Pos(ns, st) > 0) then begin
          rec.ChIndex := k;
          Break;
        end;
      end;
    end;
  finally
    cbNames.EndUpdate;
  end;
end;

procedure TfmNamesBook.cbNames_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
var
  idx, k: Integer;
  rec: TNameRecord;
  lst: TStringList;
  st: string;
begin
  idx := cbNames.SelectedIndex;
  if (idx < 0) or (idx >= cbNames.Items.Count) then Exit;
  rec := TNameRecord(TComboItem(cbNames.Items[idx]).Data);

  mmDesc.Text := '';
  mmDesc.AppendText(rec.Name+#13#10);
  mmDesc.AppendText(rec.Desc+#13#10);

  if (rec.ChIndex >= 0) then begin
    mmDesc.AppendText(''+#13#10);
    mmDesc.AppendText('Святцы:'+#13#10);

    if (rec.Sex = svMale) then lst := FChurchMNames
    else
    if (rec.Sex = svFemale) then lst := FChurchFNames;

    for k := rec.ChIndex + 1 to lst.Count - 1 do begin
      st := lst[k].Trim();

      if (st[1] = '-')
      then Break
      else begin
        StrDelete(st, 1, 1);
        mmDesc.AppendText(st+#13#10);
      end;
    end;
  end;
end;

end.
