unit GKLocationEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKBase, GKEngine, GKCtrls, GKLists, GKMapBrowser, GKLangs;

type
  TfmLocationEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    PagesData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    SheetCommon: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    EditName: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    EditLatitude: System.Windows.Forms.TextBox;
    Label3: System.Windows.Forms.Label;
    EditLongitude: System.Windows.Forms.TextBox;
    GroupBox1: System.Windows.Forms.GroupBox;
    ListGeoCoords: System.Windows.Forms.ListView;
    btnSearch: System.Windows.Forms.Button;
    btnSelect: System.Windows.Forms.Button;
    btnSelectName: System.Windows.Forms.Button;
    btnShowOnMap: System.Windows.Forms.Button;
    panMap: System.Windows.Forms.Panel;
    ColumnHeader1: System.Windows.Forms.ColumnHeader;
    ColumnHeader2: System.Windows.Forms.ColumnHeader;
    ColumnHeader3: System.Windows.Forms.ColumnHeader;

    FBase: TfmBase;
    FLocationRecord: TGEDCOMLocationRecord;
    FMapBrowser: TMapBrowser;
    FMediaList: TSheetList;
    FNotesList: TSheetList;
    FSearchPoints: TObjectList;

    procedure ControlsRefresh();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure SetLocationRecord(const Value: TGEDCOMLocationRecord);

    procedure InitializeComponent;
    procedure EditName_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSearch_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelect_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSelectName_Click(sender: System.Object; e: System.EventArgs);
    procedure EditName_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure btnShowOnMap_Click(sender: System.Object; e: System.EventArgs);
    procedure ListGeoCoords_Click(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property LocationRecord: TGEDCOMLocationRecord read FLocationRecord write SetLocationRecord;

    procedure SetLang();
  end;

implementation

procedure TfmLocationEdit.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ColumnHeader = array of System.Windows.Forms.ColumnHeader;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.PagesData := System.Windows.Forms.TabControl.Create;
  Self.SheetCommon := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.EditName := System.Windows.Forms.TextBox.Create;
  Self.EditLatitude := System.Windows.Forms.TextBox.Create;
  Self.EditLongitude := System.Windows.Forms.TextBox.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.ListGeoCoords := System.Windows.Forms.ListView.Create;
  Self.ColumnHeader1 := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnHeader2 := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnHeader3 := System.Windows.Forms.ColumnHeader.Create;
  Self.btnSearch := System.Windows.Forms.Button.Create;
  Self.btnSelect := System.Windows.Forms.Button.Create;
  Self.btnSelectName := System.Windows.Forms.Button.Create;
  Self.panMap := System.Windows.Forms.Panel.Create;
  Self.btnShowOnMap := System.Windows.Forms.Button.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.PagesData.SuspendLayout;
  Self.SheetCommon.SuspendLayout;
  Self.GroupBox1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(384, 440);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 2;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(472, 440);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 1;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // PagesData
  // 
  Self.PagesData.Controls.Add(Self.SheetCommon);
  Self.PagesData.Controls.Add(Self.SheetNotes);
  Self.PagesData.Controls.Add(Self.SheetMultimedia);
  Self.PagesData.Location := System.Drawing.Point.Create(0, 0);
  Self.PagesData.Name := 'PagesData';
  Self.PagesData.SelectedIndex := 0;
  Self.PagesData.Size := System.Drawing.Size.Create(561, 425);
  Self.PagesData.TabIndex := 0;
  // 
  // SheetCommon
  // 
  Self.SheetCommon.Controls.Add(Self.Label1);
  Self.SheetCommon.Controls.Add(Self.Label2);
  Self.SheetCommon.Controls.Add(Self.Label3);
  Self.SheetCommon.Controls.Add(Self.EditName);
  Self.SheetCommon.Controls.Add(Self.EditLatitude);
  Self.SheetCommon.Controls.Add(Self.EditLongitude);
  Self.SheetCommon.Controls.Add(Self.GroupBox1);
  Self.SheetCommon.Controls.Add(Self.btnShowOnMap);
  Self.SheetCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommon.Name := 'SheetCommon';
  Self.SheetCommon.Size := System.Drawing.Size.Create(553, 399);
  Self.SheetCommon.TabIndex := 0;
  Self.SheetCommon.Text := 'Основное';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(16, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(60, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Название';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(287, 8);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(50, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Широта';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(375, 8);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(50, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Долгота';
  // 
  // EditName
  // 
  Self.EditName.Location := System.Drawing.Point.Create(16, 24);
  Self.EditName.Name := 'EditName';
  Self.EditName.Size := System.Drawing.Size.Create(265, 21);
  Self.EditName.TabIndex := 0;
  Self.EditName.Text := '';
  Include(Self.EditName.KeyDown, Self.EditName_KeyDown);
  Include(Self.EditName.TextChanged, Self.EditName_TextChanged);
  // 
  // EditLatitude
  // 
  Self.EditLatitude.Location := System.Drawing.Point.Create(287, 24);
  Self.EditLatitude.Name := 'EditLatitude';
  Self.EditLatitude.Size := System.Drawing.Size.Create(81, 21);
  Self.EditLatitude.TabIndex := 1;
  Self.EditLatitude.Text := '';
  // 
  // EditLongitude
  // 
  Self.EditLongitude.Location := System.Drawing.Point.Create(375, 24);
  Self.EditLongitude.Name := 'EditLongitude';
  Self.EditLongitude.Size := System.Drawing.Size.Create(81, 21);
  Self.EditLongitude.TabIndex := 2;
  Self.EditLongitude.Text := '';
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.ListGeoCoords);
  Self.GroupBox1.Controls.Add(Self.btnSearch);
  Self.GroupBox1.Controls.Add(Self.btnSelect);
  Self.GroupBox1.Controls.Add(Self.btnSelectName);
  Self.GroupBox1.Controls.Add(Self.panMap);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 51);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(553, 346);
  Self.GroupBox1.TabIndex := 3;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Поиск координат (Google Maps)';
  // 
  // ListGeoCoords
  // 
  Self.ListGeoCoords.Columns.AddRange(TArrayOfSystem_Windows_Forms_ColumnHeader.Create(Self.ColumnHeader1, 
          Self.ColumnHeader2, Self.ColumnHeader3));
  Self.ListGeoCoords.FullRowSelect := True;
  Self.ListGeoCoords.Location := System.Drawing.Point.Create(16, 16);
  Self.ListGeoCoords.Name := 'ListGeoCoords';
  Self.ListGeoCoords.Size := System.Drawing.Size.Create(402, 89);
  Self.ListGeoCoords.TabIndex := 0;
  Self.ListGeoCoords.View := System.Windows.Forms.View.Details;
  Include(Self.ListGeoCoords.Click, Self.ListGeoCoords_Click);
  // 
  // ColumnHeader1
  // 
  Self.ColumnHeader1.Text := 'Название';
  Self.ColumnHeader1.Width := 200;
  // 
  // ColumnHeader2
  // 
  Self.ColumnHeader2.Text := 'Широта';
  Self.ColumnHeader2.Width := 80;
  // 
  // ColumnHeader3
  // 
  Self.ColumnHeader3.Text := 'Долгота';
  Self.ColumnHeader3.Width := 80;
  // 
  // btnSearch
  // 
  Self.btnSearch.Location := System.Drawing.Point.Create(429, 16);
  Self.btnSearch.Name := 'btnSearch';
  Self.btnSearch.Size := System.Drawing.Size.Create(105, 25);
  Self.btnSearch.TabIndex := 1;
  Self.btnSearch.Text := 'Поиск';
  Include(Self.btnSearch.Click, Self.btnSearch_Click);
  // 
  // btnSelect
  // 
  Self.btnSelect.Location := System.Drawing.Point.Create(429, 48);
  Self.btnSelect.Name := 'btnSelect';
  Self.btnSelect.Size := System.Drawing.Size.Create(105, 25);
  Self.btnSelect.TabIndex := 2;
  Self.btnSelect.Text := 'Выбрать коорд.';
  Include(Self.btnSelect.Click, Self.btnSelect_Click);
  // 
  // btnSelectName
  // 
  Self.btnSelectName.Location := System.Drawing.Point.Create(429, 80);
  Self.btnSelectName.Name := 'btnSelectName';
  Self.btnSelectName.Size := System.Drawing.Size.Create(105, 25);
  Self.btnSelectName.TabIndex := 3;
  Self.btnSelectName.Text := 'Выбрать название';
  Include(Self.btnSelectName.Click, Self.btnSelectName_Click);
  // 
  // panMap
  // 
  Self.panMap.Location := System.Drawing.Point.Create(2, 111);
  Self.panMap.Name := 'panMap';
  Self.panMap.Size := System.Drawing.Size.Create(549, 233);
  Self.panMap.TabIndex := 4;
  // 
  // btnShowOnMap
  // 
  Self.btnShowOnMap.AccessibleDescription := 'Показать на карте';
  Self.btnShowOnMap.Location := System.Drawing.Point.Create(464, 24);
  Self.btnShowOnMap.Name := 'btnShowOnMap';
  Self.btnShowOnMap.Size := System.Drawing.Size.Create(70, 21);
  Self.btnShowOnMap.TabIndex := 4;
  Self.btnShowOnMap.Text := 'Показать';
  Include(Self.btnShowOnMap.Click, Self.btnShowOnMap_Click);
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(553, 399);
  Self.SheetNotes.TabIndex := 1;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  // 
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(553, 399);
  Self.SheetMultimedia.TabIndex := 2;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // TfmLocationEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(561, 473);
  Self.Controls.Add(Self.PagesData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmLocationEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Местоположение';
  Self.PagesData.ResumeLayout(False);
  Self.SheetCommon.ResumeLayout(False);
  Self.GroupBox1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmLocationEdit.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FMapBrowser := TMapBrowser.Create();
  FMapBrowser.InitMap();
  FMapBrowser.Dock := System.Windows.Forms.DockStyle.Fill;
  panMap.Controls.Add(FMapBrowser);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FSearchPoints := TObjectList.Create(True);

  SetLang();
end;

procedure TfmLocationEdit.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FSearchPoints.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmLocationEdit.SetLang();
begin
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  SheetCommon.Text := LSList[LSID_Common];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];

  Label1.Text := LSList[LSID_Title];
  Label2.Text := LSList[LSID_Latitude];
  Label3.Text := LSList[LSID_Longitude];

  ListGeoCoords.Columns[0].Text := LSList[LSID_Title];
  ListGeoCoords.Columns[1].Text := LSList[LSID_Latitude];
  ListGeoCoords.Columns[2].Text := LSList[LSID_Longitude];

  btnShowOnMap.Text := LSList[LSID_Show];
  GroupBox1.Text := LSList[LSID_SearchCoords];
  btnSearch.Text := LSList[LSID_Search];
  btnSelect.Text := LSList[LSID_SelectCoords];
  btnSelectName.Text := LSList[LSID_SelectName];
end;

procedure TfmLocationEdit.EditName_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  Text := LSList[LSID_Location] + ' "' + EditName.Text + '"';
end;

procedure TfmLocationEdit.ControlsRefresh();
begin
  Base.RecListNotesRefresh(FLocationRecord, FNotesList.List, nil);
  Base.RecListMediaRefresh(FLocationRecord, TGKListView(FMediaList.List), nil);
end;

procedure TfmLocationEdit.SetLocationRecord(const Value: TGEDCOMLocationRecord);
begin
  FLocationRecord := Value;

  EditName.Text := FLocationRecord.Name;
  EditLatitude.Text := FLocationRecord.Map.Lati;
  EditLongitude.Text := FLocationRecord.Map.Long;

  ControlsRefresh();
end;

procedure TfmLocationEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  FLocationRecord.Name := EditName.Text;
  FLocationRecord.Map.Lati := EditLatitude.Text;
  FLocationRecord.Map.Long := EditLongitude.Text;

  Base.ChangeRecord(FLocationRecord);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmLocationEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FLocationRecord, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(Self, FLocationRecord, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmLocationEdit.EditName_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  if (e.KeyCode = Keys.Down) and (e.Control)
  then EditName.Text := EditName.Text.ToLower();
end;

procedure TfmLocationEdit.btnSearch_Click(sender: System.Object; e: System.EventArgs);
var
  k: Integer;
  pt: TMapBrowser.TGMapPoint;
  item: TExtListItem;
begin
  ListGeoCoords.BeginUpdate;
  FMapBrowser.BeginUpdate();
  try
    FSearchPoints.Clear;
    TMapBrowser.RequestGeoCoords(EditName.Text, FSearchPoints);

    ListGeoCoords.Items.Clear;
    FMapBrowser.ClearPoints();

    for k := 0 to FSearchPoints.Count - 1 do
      if (FSearchPoints[k] is TMapBrowser.TGMapPoint) then begin
        pt := TMapBrowser.TGMapPoint(FSearchPoints[k]);

        item := TExtListItem.Create;
        item.Text := pt.Hint;
        item.Data := pt;
        item.SubItems.Add(pt.Latitude.ToString('0.000000'));
        item.SubItems.Add(pt.Longitude.ToString('0.000000'));
        ListGeoCoords.Items.Add(item);

        FMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
      end;

    FMapBrowser.ZoomToBounds();
  finally
    FMapBrowser.EndUpdate();
    ListGeoCoords.EndUpdate();
  end;
end;

procedure TfmLocationEdit.ListGeoCoords_Click(sender: System.Object; e: System.EventArgs);
var
  item: TExtListItem;
  pt: TMapBrowser.TGMapPoint;
begin
  if (ListGeoCoords.SelectedItems.Count > 0) then begin
    item := TExtListItem(ListGeoCoords.SelectedItems[0]);
    
    pt := TMapBrowser.TGMapPoint(item.Data);
    if (pt <> nil)
    then FMapBrowser.SetCenter(pt.Latitude, pt.Longitude);
  end;
end;

procedure TfmLocationEdit.btnShowOnMap_Click(sender: System.Object; e: System.EventArgs);
begin
  if (EditLatitude.Text <> '') and (EditLongitude.Text <> '')
  then FMapBrowser.SetCenter(System.Double.Parse(EditLatitude.Text), System.Double.Parse(EditLongitude.Text));
end;

procedure TfmLocationEdit.btnSelect_Click(sender: System.Object; e: System.EventArgs);
var
  item: TExtListItem;
begin
  if (ListGeoCoords.SelectedItems.Count > 0) then begin
    item := TExtListItem(ListGeoCoords.SelectedItems[0]);
    EditLatitude.Text := item.SubItems[1].Text;
    EditLongitude.Text := item.SubItems[2].Text;
  end;
end;

procedure TfmLocationEdit.btnSelectName_Click(sender: System.Object; e: System.EventArgs);
begin
  if (ListGeoCoords.SelectedItems.Count > 0)
  then EditName.Text := TExtListItem(ListGeoCoords.SelectedItems[0]).Text;
end;

end.
