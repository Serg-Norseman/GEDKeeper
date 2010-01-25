unit GKAddressEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GedCom551, StdCtrls, Buttons, ComCtrls, ExtCtrls;

type
  TfmAddressEdit = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edCountry: TEdit;
    edState: TEdit;
    edCity: TEdit;
    edPostalCode: TEdit;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label5: TLabel;
    edAddress: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    btnDataAdd: TSpeedButton;
    btnDataDelete: TSpeedButton;
    btnDataEdit: TSpeedButton;
    PageAddrData: TPageControl;
    SheetPhones: TTabSheet;
    ListPhones: TListBox;
    SheetEmails: TTabSheet;
    SheetWebPages: TTabSheet;
    ListEmails: TListBox;
    ListWebPages: TListBox;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnDataAddClick(Sender: TObject);
    procedure btnDataEditClick(Sender: TObject);
    procedure btnDataDeleteClick(Sender: TObject);
  private
    FAddress: TGEDCOMAddress;
    procedure SetAddress(const Value: TGEDCOMAddress);
    procedure UpdateLists();
  public
    property Address: TGEDCOMAddress read FAddress write SetAddress;
  end;

var
  fmAddressEdit: TfmAddressEdit;

implementation

{$R *.dfm}

{ TfmAddressEdit }

procedure TfmAddressEdit.SetAddress(const Value: TGEDCOMAddress);
begin
  FAddress := Value;

  edCountry.Text := FAddress.AddressCountry;
  edState.Text := FAddress.AddressState;
  edCity.Text := FAddress.AddressCity;
  edPostalCode.Text := FAddress.AddressPostalCode;
  edAddress.Text := Trim(FAddress.Address.Text);

  UpdateLists();
end;

procedure TfmAddressEdit.btnAcceptClick(Sender: TObject);
var
  sl: TStringList;
begin
  FAddress.AddressCountry := edCountry.Text;
  FAddress.AddressState := edState.Text;
  FAddress.AddressCity := edCity.Text;
  FAddress.AddressPostalCode := edPostalCode.Text;

  sl := TStringList.Create;
  try
    sl.Text := edAddress.Text;
    FAddress.Address := sl;
  finally
    sl.Destroy;
  end;
end;

procedure TfmAddressEdit.UpdateLists();
var
  i: Integer;
begin
  ListPhones.Clear;
  ListEmails.Clear;
  ListWebPages.Clear;

  for i := 0 to FAddress.PhoneNumbersCount - 1 do
    ListPhones.Items.Add(FAddress.PhoneNumbers[i]);

  for i := 0 to FAddress.EmailAddressesCount - 1 do
    ListEmails.Items.Add(FAddress.EmailAddresses[i]);

  for i := 0 to FAddress.WebPagesCount - 1 do
    ListWebPages.Items.Add(FAddress.WebPages[i]);
end;

const
  Titles: array [0..2] of string = ('Телефон', 'Эл. почта', 'Веб-страница');

procedure TfmAddressEdit.btnDataAddClick(Sender: TObject);
var
  val: string;
begin
  if InputQuery(Titles[PageAddrData.TabIndex], 'Значение', val) then begin
    case PageAddrData.TabIndex of
      0: FAddress.PhoneNumbers[FAddress.PhoneNumbersCount] := val;
      1: FAddress.EmailAddresses[FAddress.EmailAddressesCount] := val;
      2: FAddress.WebPages[FAddress.WebPagesCount] := val;
    end;
  end;

  UpdateLists();
end;

procedure TfmAddressEdit.btnDataEditClick(Sender: TObject);
var
  idx: Integer;
  val: string;
begin
  case PageAddrData.TabIndex of
    0: begin
      idx := ListPhones.ItemIndex;
      val := FAddress.PhoneNumbers[idx];
    end;
    1: begin
      idx := ListEmails.ItemIndex;
      val := FAddress.EmailAddresses[idx];
    end;
    2: begin
      idx := ListWebPages.ItemIndex;
      val := FAddress.WebPages[idx];
    end;
  end;

  if (idx < 0) then Exit;

  if InputQuery(Titles[PageAddrData.TabIndex], 'Значение', val) then begin
    case PageAddrData.TabIndex of
      0: FAddress.PhoneNumbers[idx] := val;
      1: FAddress.EmailAddresses[idx] := val;
      2: FAddress.WebPages[idx] := val;
    end;
  end;

  UpdateLists();
end;

procedure TfmAddressEdit.btnDataDeleteClick(Sender: TObject);
var
  idx: Integer;
begin
  case PageAddrData.TabIndex of
    0: begin
      idx := ListPhones.ItemIndex;
      if (idx < 0) then Exit;
      FAddress.DeletePhoneNumber(idx);
    end;
    1: begin
      idx := ListEmails.ItemIndex;
      if (idx < 0) then Exit;
      FAddress.DeleteEmail(idx);
    end;
    2: begin
      idx := ListWebPages.ItemIndex;
      if (idx < 0) then Exit;
      FAddress.DeleteWebPage(idx);
    end;
  end;

  UpdateLists();
end;

end.
