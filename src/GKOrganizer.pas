unit GKOrganizer; {prepare:fin}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, ComCtrls, GKBase, GKCtrls,
  GKLists;

type
  TfmOrganizer = class(TForm)
    PageControl1: TPageControl;
    SheetAddresses: TTabSheet;
    SheetTelephones: TTabSheet;
    SheetEMails: TTabSheet;
    SheetWebs: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAdrList: TSheetList;
    FPhonesList: TSheetList;
    FMailsList: TSheetList;
    FWebsList: TSheetList;

    procedure CollectData();
    function GetBase(): TfmBase;
  public
    property Base: TfmBase read GetBase;
  end;

var
  fmOrganizer: TfmOrganizer;

implementation

uses GedCom551, GKEngine;

{$R *.dfm}

{ TfmOrganizer }

function TfmOrganizer.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmOrganizer.FormShow(Sender: TObject);
begin
  CollectData();
end;

procedure TfmOrganizer.CollectData();

  procedure AddItem(aList: TCustomListControl; aPerson, aData: string);
  var
    item: TListItem;
  begin
    item := TGKListView(aList).Items.Add();
    item.Caption := aPerson;
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
      addr_str := Trim(addr.Address.Text);

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
  FAdrList.List.Clear;
  FPhonesList.List.Clear;
  FMailsList.List.Clear;
  FWebsList.List.Clear;

  for i := 0 to Base.Tree.RecordsCount - 1 do begin
    rec := Base.Tree.Records[i];

    if (rec is TGEDCOMIndividualRecord) then begin
      i_rec := (rec as TGEDCOMIndividualRecord);
      nm := GetNameStr(i_rec);

      for k := 0 to i_rec.IndividualEventsCount - 1 do
        PrepareEvent(nm, i_rec.IndividualEvents[k]);
    end;
  end;

  ResizeColumn(FAdrList.List, 0);
  ResizeColumn(FAdrList.List, 1);

  ResizeColumn(FPhonesList.List, 0);
  ResizeColumn(FPhonesList.List, 1);

  ResizeColumn(FMailsList.List, 0);
  ResizeColumn(FMailsList.List, 1);

  ResizeColumn(FWebsList.List, 0);
  ResizeColumn(FWebsList.List, 1);
end;

procedure TfmOrganizer.FormCreate(Sender: TObject);
begin
  FAdrList := TSheetList.Create(SheetAddresses);
  FAdrList.Buttons := [];
  AddListColumn(FAdrList.List, 'Персона', 350, False);
  AddListColumn(FAdrList.List, 'Адрес', 100, False);

  FPhonesList := TSheetList.Create(SheetTelephones);
  FPhonesList.Buttons := [];
  AddListColumn(FPhonesList.List, 'Персона', 350, False);
  AddListColumn(FPhonesList.List, 'Телефонный номер', 100, False);

  FMailsList := TSheetList.Create(SheetEMails);
  FMailsList.Buttons := [];
  AddListColumn(FMailsList.List, 'Персона', 350, False);
  AddListColumn(FMailsList.List, 'Адрес эл. почты', 100, False);

  FWebsList := TSheetList.Create(SheetWebs);
  FWebsList.Buttons := [];
  AddListColumn(FWebsList.List, 'Персона', 350, False);
  AddListColumn(FWebsList.List, 'Сайт', 100, False);
end;

end.
