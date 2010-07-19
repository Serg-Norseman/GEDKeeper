unit GKGenBase;

interface

uses
  Classes, GedCom551, Contnrs;

type
  TGenBase = class(TObject)
  private
    FMainTree: TGEDCOMTree;
  public
    constructor Create(aTree: TGEDCOMTree);
    destructor Destroy; override;

    procedure TreeMerge(aFileName: string);
    procedure TreeSync(aFileName: string; aLog: TStrings);
    
    property Tree: TGEDCOMTree read FMainTree;
  end;

implementation

uses
  SysUtils, GKCommon;

{ TGenBase }

constructor TGenBase.Create(aTree: TGEDCOMTree);
begin
  inherited Create;
  FMainTree := aTree;
end;

destructor TGenBase.Destroy;
begin
  inherited Destroy;
end;

procedure TGenBase.TreeMerge(aFileName: string);
var
  repMap: TXRefReplaceMap;
  i: Integer;
  extTree: TGEDCOMTree;
  rec: TGEDCOMRecord;
  newXRef: string;
begin
  extTree := TGEDCOMTree.Create;
  repMap := TXRefReplaceMap.Create;
  try
    extTree.LoadFromFile(aFileName);
    extTree.Header.Clear;

    while (extTree.RecordsCount > 0) do begin
      rec := extTree.Extract(0);
      newXRef := FMainTree.XRefIndex_NewXRef(rec);
      repMap.AddXRef(rec, rec.XRef, newXRef);
      rec.XRef := newXRef;
      rec.ResetOwner(FMainTree);
      FMainTree.AddRecord(rec);
    end;

    for i := 0 to repMap.Count - 1 do begin
      rec := repMap.Records[i].Rec;
      rec.ReplaceXRefs(repMap);
    end;
  finally
    repMap.Free;
    extTree.Destroy;
  end;
end;

type
  TSyncRec = class(TObject)
  public
    MasterRecord, UpdateRecord: TGEDCOMRecord;
    State: (ssUndefined, ssHasMaster, ssNoMaster);

    UpdateOldXRef, UpdateNewXRef: string;
  end;

procedure TGenBase.TreeSync(aFileName: string; aLog: TStrings);
// текущая реализация - доверенная синхронизация
var
  repMap: TXRefReplaceMap;
  extTree: TGEDCOMTree;
  i: Integer;
  rec: TGEDCOMRecord;
  sync_list: TObjectList;
  sync_rec: TSyncRec;
  s, newXRef, backUID: string;
begin
  aLog.Clear;

  extTree := TGEDCOMTree.Create;
  repMap := TXRefReplaceMap.Create;
  sync_list := TObjectList.Create(True);
  try
    extTree.LoadFromFile(aFileName);
    extTree.Header.Clear;

    CheckGEDCOMFormat(extTree);

    // создание списка объектов синхронизации
    for i := 0 to extTree.RecordsCount - 1 do begin
      rec := extTree.Records[i];

      sync_rec := TSyncRec.Create;
      sync_rec.MasterRecord := nil;
      sync_rec.UpdateRecord := rec;
      sync_rec.State := ssUndefined;
      sync_rec.UpdateOldXRef := '';
      sync_rec.UpdateNewXRef := '';
      sync_list.Add(sync_rec);
    end;

    // поиск записей в мастер-базе
    for i := 0 to sync_list.Count - 1 do begin
      sync_rec := TSyncRec(sync_list[i]);
      rec := FMainTree.FindUID(sync_rec.UpdateRecord.UID);

      if (rec <> nil) then begin
        sync_rec.MasterRecord := rec;
        sync_rec.State := ssHasMaster;
      end else begin
        sync_rec.State := ssNoMaster;

        // если не найдена - просто переносим,
        // в них ничего менять не нужно
        rec := extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
        newXRef := FMainTree.XRefIndex_NewXRef(rec);
        repMap.AddXRef(rec, rec.XRef, newXRef);
        rec.XRef := newXRef;
        rec.ResetOwner(FMainTree);
        FMainTree.AddRecord(rec);
      end;
    end;

    // обновить ссылки в перенесенных
    for i := 0 to repMap.Count - 1 do begin
      rec := repMap.Records[i].Rec;
      rec.ReplaceXRefs(repMap);
    end;

    // обновить ссылки в оставшихся
    for i := 0 to extTree.RecordsCount - 1 do begin
      rec := extTree.Records[i];
      rec.ReplaceXRefs(repMap);
    end;

    // слить оставшиеся записи в их оригиналы
    for i := 0 to sync_list.Count - 1 do begin
      sync_rec := TSyncRec(sync_list[i]);

      if (sync_rec.State = ssHasMaster) then begin
        // подготовка
        rec := extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
        rec.XRef := FMainTree.XRefIndex_NewXRef(rec);
        rec.ResetOwner(FMainTree);
        FMainTree.AddRecord(rec);

        // в мастер-записи нужно сохранить UID
        backUID := sync_rec.MasterRecord.UID;

        // перенос
        sync_rec.UpdateRecord.MoveTo(sync_rec.MasterRecord, [mfClearDest]);

        // восстановить UID
        sync_rec.MasterRecord.UID := backUID;

        // зачистка
        FMainTree.DeleteRecord(rec);
      end;
    end;

    // диагностика
    {for i := 0 to sync_list.Count - 1 do begin
      sync_rec := TSyncRec(sync_list[i]);

      s := IntToStr(i) + ': [' + sync_rec.UpdateRecord.XRef + '] -> [';
      if (sync_rec.MasterRecord <> nil)
      then s := s + sync_rec.MasterRecord.XRef + '] '
      else s := s + '-] ';

      aLog.Add(s);
    end;}

    aLog.Add('Синхронизация завершена.');
  finally
    sync_list.Destroy;
    repMap.Free;
    extTree.Destroy;
  end;
end;

end.
