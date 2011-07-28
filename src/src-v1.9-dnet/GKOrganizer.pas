unit GKOrganizer; {trans:fin}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GKBase, GKCtrls, GKLists, GedCom551, GKEngine, GKLangs;

type
  TfmOrganizer = class(System.Windows.Forms.Form)
  strict private
    PageControl1: System.Windows.Forms.TabControl;
    SheetAddresses: System.Windows.Forms.TabPage;
    SheetTelephones: System.Windows.Forms.TabPage;
    SheetEMails: System.Windows.Forms.TabPage;
    SheetWebs: System.Windows.Forms.TabPage;

    FAdrList: TSheetList;
    FPhonesList: TSheetList;
    FMailsList: TSheetList;
    FWebsList: TSheetList;

    FBase: TfmBase;

    procedure CollectData();
    procedure InitializeComponent;
    procedure TfmOrganizer_Load(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
  end;

implementation

procedure TfmOrganizer.InitializeComponent;
begin
  Self.PageControl1 := System.Windows.Forms.TabControl.Create;
  Self.SheetAddresses := System.Windows.Forms.TabPage.Create;
  Self.SheetTelephones := System.Windows.Forms.TabPage.Create;
  Self.SheetEMails := System.Windows.Forms.TabPage.Create;
  Self.SheetWebs := System.Windows.Forms.TabPage.Create;
  Self.PageControl1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PageControl1
  // 
  Self.PageControl1.Controls.Add(Self.SheetAddresses);
  Self.PageControl1.Controls.Add(Self.SheetTelephones);
  Self.PageControl1.Controls.Add(Self.SheetEMails);
  Self.PageControl1.Controls.Add(Self.SheetWebs);
  Self.PageControl1.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.PageControl1.Location := System.Drawing.Point.Create(0, 0);
  Self.PageControl1.Name := 'PageControl1';
  Self.PageControl1.SelectedIndex := 0;
  Self.PageControl1.Size := System.Drawing.Size.Create(736, 476);
  Self.PageControl1.TabIndex := 0;
  // 
  // SheetAddresses
  // 
  Self.SheetAddresses.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetAddresses.Name := 'SheetAddresses';
  Self.SheetAddresses.Size := System.Drawing.Size.Create(728, 450);
  Self.SheetAddresses.TabIndex := 0;
  Self.SheetAddresses.Text := 'Адреса';
  // 
  // SheetTelephones
  // 
  Self.SheetTelephones.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTelephones.Name := 'SheetTelephones';
  Self.SheetTelephones.Size := System.Drawing.Size.Create(728, 496);
  Self.SheetTelephones.TabIndex := 1;
  Self.SheetTelephones.Text := 'Телефоны';
  // 
  // SheetEMails
  // 
  Self.SheetEMails.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetEMails.Name := 'SheetEMails';
  Self.SheetEMails.Size := System.Drawing.Size.Create(728, 496);
  Self.SheetEMails.TabIndex := 2;
  Self.SheetEMails.Text := 'Почта';
  // 
  // SheetWebs
  // 
  Self.SheetWebs.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetWebs.Name := 'SheetWebs';
  Self.SheetWebs.Size := System.Drawing.Size.Create(728, 496);
  Self.SheetWebs.TabIndex := 3;
  Self.SheetWebs.Text := 'Сайты';
  // 
  // TfmOrganizer
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(736, 476);
  Self.Controls.Add(Self.PageControl1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmOrganizer';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Органайзер';
  Include(Self.Load, Self.TfmOrganizer_Load);
  Self.PageControl1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmOrganizer.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FAdrList := TSheetList.Create(SheetAddresses);
  FAdrList.Buttons := TEnumSet.Create();
  FAdrList.List.AddListColumn(LSList[LSID_Person], 350, False);
  FAdrList.List.AddListColumn(LSList[LSID_Address], 100, False);

  FPhonesList := TSheetList.Create(SheetTelephones);
  FPhonesList.Buttons := TEnumSet.Create();
  FPhonesList.List.AddListColumn(LSList[LSID_Person], 350, False);
  FPhonesList.List.AddListColumn(LSList[LSID_Telephone], 100, False);

  FMailsList := TSheetList.Create(SheetEMails);
  FMailsList.Buttons := TEnumSet.Create();
  FMailsList.List.AddListColumn(LSList[LSID_Person], 350, False);
  FMailsList.List.AddListColumn(LSList[LSID_Mail], 100, False);

  FWebsList := TSheetList.Create(SheetWebs);
  FWebsList.Buttons := TEnumSet.Create();
  FWebsList.List.AddListColumn(LSList[LSID_Person], 350, False);
  FWebsList.List.AddListColumn(LSList[LSID_WebSite], 100, False);

  /// SetLang
  Text := LSList[LSID_MIOrganizer];
end;

procedure TfmOrganizer.TfmOrganizer_Load(sender: System.Object; e: System.EventArgs);
begin
  CollectData();
end;

procedure TfmOrganizer.CollectData();

  procedure AddItem(aList: TGKListView; aPerson, aData: string);
  var
    item: TExtListItem;
  begin
    item := aList.AddItem(aPerson, nil);
    item.SubItems.Add(aData);
  end;

  procedure PrepareEvent(iName: string; ev: TGEDCOMCustomEvent);
  var
    addr: TGEDCOMAddress;
    addr_str: string;
    m: Integer;
  begin
    addr := ev.Detail.Address;
    if (addr <> nil) then begin
      addr_str := addr.Address.Text.Trim();

      if (addr_str <> '')
      then AddItem(FAdrList.List, iName, addr_str);

      for m := 0 to addr.PhoneNumbersCount - 1 do
        AddItem(FPhonesList.List, iName, addr.PhoneNumbers[m]);

      for m := 0 to addr.EmailAddressesCount - 1 do
        AddItem(FMailsList.List, iName, addr.EmailAddresses[m]);

      for m := 0 to addr.WebPagesCount - 1 do
        AddItem(FWebsList.List, iName, addr.WebPages[m]);
    end;
  end;

var
  i, k: Integer;
  rec: TGEDCOMRecord;
  i_rec: TGEDCOMIndividualRecord;
  nm: string;
begin
  FAdrList.List.Items.Clear;
  FPhonesList.List.Items.Clear;
  FMailsList.List.Items.Clear;
  FWebsList.List.Items.Clear;

  for i := 0 to Base.Tree.RecordsCount - 1 do begin
    rec := Base.Tree.Records[i];

    if (rec is TGEDCOMIndividualRecord) then begin
      i_rec := (rec as TGEDCOMIndividualRecord);
      nm := TGenEngine.GetNameStr(i_rec);

      for k := 0 to i_rec.IndividualEventsCount - 1 do
        PrepareEvent(nm, i_rec.IndividualEvents[k]);
    end;
  end;

  FAdrList.List.ResizeColumn(0);
  FAdrList.List.ResizeColumn(1);

  FPhonesList.List.ResizeColumn(0);
  FPhonesList.List.ResizeColumn(1);

  FMailsList.List.ResizeColumn(0);
  FMailsList.List.ResizeColumn(1);

  FWebsList.List.ResizeColumn(0);
  FWebsList.List.ResizeColumn(1);
end;

end.
