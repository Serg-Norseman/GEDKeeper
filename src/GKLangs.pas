unit GKLangs;

interface

uses
  Windows, SysUtils;

type
  TLangID = Windows.LANGID;

  ILocalization = interface
    procedure SetLang(LangID: TLangID);
  end;

const
  // Language String IDs
  LSID_First            = 1;

  LSID_MIFile           = 1;
  LSID_MIEdit           = 2;
  LSID_MIPedigree       = 3;
  LSID_MIWindow         = 4;
  LSID_MIHelp           = 5;
  LSID_MIMRUFiles       = 6;

  LSID_Last             = 6;

function GetLangStr(ID: Cardinal): string;

implementation

const
  LSList: array [LSID_First..LSID_Last] of string = (
    'Файл',
    'Правка',
    'Родословная',
    '&Окна',
    'Справка',
    'Открыть последний'
  );

function GetLangStr(ID: Cardinal): string;
begin
  Result := LSList[ID];
end;

end.
