unit GKEngineAPI;

{$I GEDKeeper.inc}

interface

uses
  Classes, GKBase;

procedure lua_run(script: string; aBase: TfmBase; aDebugOutput: TStrings);

implementation

uses
  TypInfo, SysUtils, Dialogs,
  GedCom551, GKUtils, GKEngine, GKProgress, GKSexCheck, GKMain
  {$IFNDEF DELPHI_NET}, lua, lualib, lauxlib, SdfData, ADODB {$ENDIF};

var
  DebugOutput: TStrings;
  Base: TfmBase;

type
  TScriptResource = (srProgress, srCSV);

{$IFNDEF DELPHI_NET}

{==============================================================================}

procedure lua_Output(s: string);
begin
  DebugOutput.Add(s);
end;

function lua_Print(L: Plua_State): Integer; cdecl;
var
  i, n: Integer;
begin
  n := lua_gettop(L);
  for i := 1 to n do begin
    if (i > 1) then
      lua_Output(#9);

    if lua_isstring(L, i)
    then lua_Output(lua_tostring(L, i))
    else lua_Output(Format('%s:%p', [lua_type(L, i), lua_topointer(L, i)]));
  end;
  Result := 0;
end;

procedure lua_setvar_f(L: Plua_State; Name: string; N: Double);
begin
  lua_pushnumber(L, N); // Вводим в стек lua значение
  lua_setglobal(L, PChar(Name)); // Говорим, что глобальная переменная имеет название
end;

procedure lua_setvar_i(L: Plua_State; Name: string; I: Integer);
begin
  lua_pushinteger(L, I);
  lua_setglobal(L, PChar(Name));
end;

procedure lua_DoError(L: Plua_State; ErrorMsg: string);
begin
  lua_pushstring(L, PChar(ErrorMsg));
  lua_error(L);
end;

procedure lua_ExpectNArgs(LVM: Plua_State; ErrorMsg: string; Expected: Integer);
var
  argc: Integer;
begin
  argc := lua_gettop(LVM);
  if (argc <> Expected)
  then lua_DoError(LVM, Format(ErrorMsg + ', неверное количество аргументов: необходимо %d, задано %d', [Expected, argc]));
end;

procedure lua_ExpectFloatArg(LVM: Plua_State; idx: Integer; ErrorMsg: string; var Arg: Double);
begin
  if lua_isnumber(LVM, idx)
  then Arg := lua_tonumber(LVM, idx)
  else lua_DoError(LVM, ErrorMsg + ', expected float number for argument #'+IntToStr(idx));
end;

procedure lua_ExpectIntArg(LVM: Plua_State; idx: Integer; ErrorMsg: string; var Arg: Integer);
begin
  if lua_isnumber(LVM, idx)
  then Arg := Trunc(lua_tonumber(LVM, idx))
  else lua_DoError(LVM, ErrorMsg + ', expected integer number for argument #'+IntToStr(idx));
end;

procedure lua_ExpectStringArg(LVM: Plua_State; idx: Integer; ErrorMsg: string; var Arg: string);
begin
  if lua_isstring(LVM, idx)
  then Arg := lua_tostring(LVM, idx)
  else lua_DoError(LVM, ErrorMsg + ', expected string for argument #'+IntToStr(idx));
end;

procedure lua_ExpectBooleanArg(LVM: Plua_State; idx: Integer; ErrorMsg: string; var Arg: Boolean);
begin
  if lua_isboolean(LVM, idx)
  then Arg := (Integer(lua_toboolean(LVM, idx)) <> 0)
  else lua_DoError(LVM, ErrorMsg + ', expected boolean for argument #'+IntToStr(idx));
end;

procedure lua_ExpectPtrArg(LVM: Plua_State; idx: Integer; ErrorMsg: string; var Arg: Pointer);
begin
  if lua_islightuserdata(LVM, idx)
  then Arg := lua_touserdata(LVM, idx)
  else lua_DoError(LVM, ErrorMsg + ', expected pointer for argument #'+IntToStr(idx));
end;

procedure lua_pushptr(L: Plua_State; p: Pointer);
begin
  if Assigned(p)
  then lua_pushlightuserdata(L, p)
  else lua_pushnil(L);
end;

{==============================================================================}

function gk_print(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gk_print()';
var
  text: string;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectStringArg(LVM, 1, func_name, text);

  lua_Output(text);
  Result := 0;
end;

function gk_progress_init(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gk_progress_init()';
var
  length: Integer;
  title: string;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectIntArg(LVM, 1, func_name, length);
  lua_ExpectStringArg(LVM, 2, func_name, title);

  ProgressInit(length, title);
  Result := 0;
end;

function gk_progress_done(LVM: Plua_State): Integer; cdecl;
begin
  ProgressDone();
  Result := 0;
end;

function gk_progress_step(LVM: Plua_State): Integer; cdecl;
begin
  ProgressStep();
  Result := 0;
end;

function gk_strpos(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gk_strpos()';
var
  substr, str: string;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectStringArg(LVM, 1, func_name, substr);
  lua_ExpectStringArg(LVM, 2, func_name, str);

  lua_pushnumber(LVM, Pos(substr, str));
  Result := 1;
end;

function gk_update_view(LVM: Plua_State): Integer; cdecl;
begin
  Base.ListsRefresh();
  Result := 0;
end;

function gk_select_file(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gk_select_file()';
var
  dlg: TOpenDialog;
  fn: string;
begin
  lua_ExpectNArgs(LVM, func_name, 0);

  dlg := TOpenDialog.Create(nil);
  try
    if dlg.Execute
    then fn := dlg.FileName
    else fn := '';
  finally
    dlg.Destroy;
  end;

  lua_pushstring(LVM, PChar(fn));
  Result := 1;
end;

{==============================================================================}

function gt_get_records_count(LVM: Plua_State): Integer; cdecl;
begin
  lua_pushnumber(LVM, Base.Tree.RecordsCount);
  Result := 1;
end;

function gt_get_record(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_record()';
var
  idx: Integer;
  rec: TGEDCOMRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectIntArg(LVM, 1, func_name, idx);

  rec := Base.Tree.Records[idx];
  lua_pushptr(LVM, rec);
  Result := 1;
end;

function gt_get_record_type(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_record_type()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMRecord(rec_ptr);

  lua_pushinteger(LVM, Ord(rec.RecordType));
  Result := 1;
end;

function gt_delete_record(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_delete_record()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
  res: Boolean;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMRecord(rec_ptr);
  res := Base.DeleteRecord(rec, False);

  lua_pushboolean(LVM, res);
  Result := 1;
end;

function gt_get_record_xref(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_record_xref()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMRecord(rec_ptr);
  lua_pushstring(LVM, PChar(rec.XRef));
  Result := 1;
end;

function gt_get_record_uid(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_record_uid()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMRecord(rec_ptr);
  lua_pushstring(LVM, PChar(rec.UID));
  Result := 1;
end;

function gt_get_record_type_name(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_record_type_name()';
var
  rec_type: Integer;
  rt_name: string;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectIntArg(LVM, 1, func_name, rec_type);

  rt_name := GetEnumName(TypeInfo(TGEDCOMRecordType), Integer(rec_type));
  lua_pushstring(LVM, PChar(rt_name));
  Result := 1;
end;

function gt_record_is_filtered(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_record_is_filtered()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
  res: Boolean;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMRecord(rec_ptr);
  res := Base.RecordIsFiltered(rec);

  lua_pushboolean(LVM, res);
  Result := 1;
end;

function gt_select_record(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_select_record()';
var
  rec_type: Integer;
  rec: TGEDCOMRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectIntArg(LVM, 1, func_name, rec_type);

  rec := Base.SelectRecord(TGEDCOMRecordType(rec_type), []);
  lua_pushptr(LVM, rec);
  Result := 1;
end;

function gt_get_person_name(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_name()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  lua_pushstring(LVM, PChar(GetNameStr(rec)));
  Result := 1;
end;

function gt_get_person_associations_count(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_associations_count()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  lua_pushinteger(LVM, rec.AssociationsCount);
  Result := 1;
end;

function gt_get_person_association(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_association()';
var
  rec_ptr: Pointer;
  idx: Integer;
  rec: TGEDCOMIndividualRecord;
  asso: TGEDCOMAssociation;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectIntArg(LVM, 2, func_name, idx);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  asso := rec.Associations[idx];

  lua_pushptr(LVM, asso);
  Result := 1;
end;

function gt_add_person_association(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_add_person_association()';
var
  rec_ptr, a_ptr: Pointer;
  rel: string;
  rec, a_rec: TGEDCOMIndividualRecord;
  asso: TGEDCOMAssociation;
begin
  lua_ExpectNArgs(LVM, func_name, 3);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectStringArg(LVM, 2, func_name, rel);
  lua_ExpectPtrArg(LVM, 3, func_name, a_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  a_rec := TGEDCOMIndividualRecord(a_ptr);
  asso := Base.Engine.AddAssociation(rec, rel, a_rec);

  lua_pushptr(LVM, asso);
  Result := 1;
end;

function gt_delete_person_association(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_delete_person_association()';
var
  rec_ptr: Pointer;
  idx: Integer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectIntArg(LVM, 2, func_name, idx);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  rec.DeleteAssociation(idx);
  Result := 0;
end;

function gt_get_person_events_count(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_events_count()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  lua_pushinteger(LVM, rec.IndividualEventsCount);
  Result := 1;
end;

function gt_get_person_event(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_event()';
var
  rec_ptr: Pointer;
  idx: Integer;
  rec: TGEDCOMIndividualRecord;
  evt: TGEDCOMCustomEvent;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectIntArg(LVM, 2, func_name, idx);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  evt := rec.IndividualEvents[idx];

  lua_pushptr(LVM, evt);
  Result := 1;
end;

function gt_delete_person_event(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_delete_person_event()';
var
  rec_ptr: Pointer;
  idx: Integer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectIntArg(LVM, 2, func_name, idx);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  rec.DeleteIndividualEvent(idx);

  Result := 0;
end;

function gt_get_event_date(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_event_date()';
var
  ev_ptr: Pointer;
  evt: TGEDCOMIndividualEvent;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, ev_ptr);

  evt := TGEDCOMIndividualEvent(ev_ptr);
  lua_pushstring(LVM, PChar(GEDCOMEventToDateStr(evt, dfDD_MM_YYYY, False)));
  Result := 1;
end;

function gt_set_event_date(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_set_event_date()';
var
  ev_ptr: Pointer;
  evt: TGEDCOMIndividualEvent;
  date: string;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, ev_ptr);
  lua_ExpectStringArg(LVM, 2, func_name, date);

  {fixme!!!}
  try
    if (date <> '') then begin
      evt := TGEDCOMIndividualEvent(ev_ptr);
      evt.Detail.Date.ParseString(date);
    end;
  except
    lua_DoError(LVM, 'Некорректный формат даты: ' + date);
  end;

  Result := 0;
end;

function gt_get_event_value(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_event_value()';
var
  ev_ptr: Pointer;
  evt: TGEDCOMIndividualEvent;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, ev_ptr);

  evt := TGEDCOMIndividualEvent(ev_ptr);
  lua_pushstring(LVM, PChar(evt.StringValue));
  Result := 1;
end;

function gt_get_event_place(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_event_place()';
var
  ev_ptr: Pointer;
  evt: TGEDCOMIndividualEvent;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, ev_ptr);

  evt := TGEDCOMIndividualEvent(ev_ptr);
  lua_pushstring(LVM, PChar(evt.Detail.Place.StringValue));
  Result := 1;
end;

function gt_set_event_place(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_set_event_place()';
var
  ev_ptr: Pointer;
  evt: TGEDCOMIndividualEvent;
  place: string;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, ev_ptr);
  lua_ExpectStringArg(LVM, 2, func_name, place);

  evt := TGEDCOMIndividualEvent(ev_ptr);
  evt.Detail.Place.StringValue := place;

  Result := 0;
end;

function gt_get_event_name(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_event_name()';
var
  ev_ptr: Pointer;
  evt: TGEDCOMIndividualEvent;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, ev_ptr);

  evt := TGEDCOMIndividualEvent(ev_ptr);
  lua_pushstring(LVM, PChar(evt.Name));
  Result := 1;
end;

function gt_get_person_sex(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_sex()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  lua_pushstring(LVM, PChar(SexData[rec.Sex].LatSign));
  Result := 1;
end;

function gt_set_person_sex(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_set_person_sex()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
  s_sex: string;
  sex: TGEDCOMSex;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectStringArg(LVM, 2, func_name, s_sex);

  rec := TGEDCOMIndividualRecord(rec_ptr);

  if (Length(s_sex) = 1)
  then sex := GetSexBySign(s_sex[1])
  else sex := svNone;

  rec.Sex := sex;

  Result := 0;
end;

function gt_create_person(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_create_person()';
var
  name, patronymic, family, s_sex: string;
  sex: TGEDCOMSex;
  i_rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 4);
  lua_ExpectStringArg(LVM, 1, func_name, name);
  lua_ExpectStringArg(LVM, 2, func_name, patronymic);
  lua_ExpectStringArg(LVM, 3, func_name, family);
  lua_ExpectStringArg(LVM, 4, func_name, s_sex);

  if (Length(s_sex) = 1)
  then sex := GetSexBySign(s_sex[1])
  else sex := svNone;

  i_rec := CreatePersonEx(Base.Tree, name, patronymic, family, sex, False);
  lua_pushptr(LVM, i_rec);
  Result := 1;
end;

function gt_create_family(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_create_family()';
var
  f_rec: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 0);

  f_rec := CreateFamilyEx(Base.Tree);
  lua_pushptr(LVM, f_rec);
  Result := 1;
end;

function gt_create_note(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_create_note()';
var
  n_rec: TGEDCOMNoteRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 0);

  n_rec := CreateNote(Base.Tree);
  lua_pushptr(LVM, n_rec);
  Result := 1;
end;

function gt_create_source(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_create_source()';
var
  src_rec: TGEDCOMSourceRecord;
  name: string;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectStringArg(LVM, 1, func_name, name);

  src_rec := CreateSource(Base.Tree);
  src_rec.FiledByEntry := name;

  lua_pushptr(LVM, src_rec);
  Result := 1;
end;

function gt_add_note_text(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_add_note_text()';
var
  note_ptr: Pointer;
  n_rec: TGEDCOMNoteRecord;
  txt: string;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, note_ptr);
  lua_ExpectStringArg(LVM, 2, func_name, txt);

  n_rec := TGEDCOMNoteRecord(note_ptr);
  AddNoteText(n_rec, txt);

  Result := 0;
end;

function gt_bind_record_note(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_bind_record_note()';
var
  rec_ptr, note_ptr: Pointer;
  rec: TGEDCOMRecord;
  note_rec: TGEDCOMNoteRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectPtrArg(LVM, 2, func_name, note_ptr);

  rec := TGEDCOMRecord(rec_ptr);
  note_rec := TGEDCOMNoteRecord(note_ptr);

  BindRecordNote(Base.Tree, rec, note_rec);

  Result := 0;
end;

function gt_bind_record_source(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_bind_record_source()';
var
  rec_ptr, src_ptr: Pointer;
  quality: Integer;
  rec: TGEDCOMRecord;
  src_rec: TGEDCOMSourceRecord;
  page: string;
begin
  lua_ExpectNArgs(LVM, func_name, 4);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectPtrArg(LVM, 2, func_name, src_ptr);
  lua_ExpectStringArg(LVM, 3, func_name, page);
  lua_ExpectIntArg(LVM, 4, func_name, quality);

  rec := TGEDCOMRecord(rec_ptr);
  src_rec := TGEDCOMSourceRecord(src_ptr);

  BindRecordSource(Base.Tree, rec, src_rec, page, quality);

  Result := 0;
end;

function gt_bind_family_spouse(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_bind_family_spouse()';
var
  f_ptr, sp_ptr: Pointer;
  f_rec: TGEDCOMFamilyRecord;
  sp_rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, f_ptr);
  lua_ExpectPtrArg(LVM, 2, func_name, sp_ptr);

  f_rec := TGEDCOMFamilyRecord(f_ptr);
  sp_rec := TGEDCOMIndividualRecord(sp_ptr);

  Base.Engine.AddFamilySpouse(f_rec, sp_rec);
  Result := 0;
end;

function gt_bind_family_child(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_bind_family_child()';
var
  f_ptr, ch_ptr: Pointer;
  f_rec: TGEDCOMFamilyRecord;
  ch_rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, f_ptr);
  lua_ExpectPtrArg(LVM, 2, func_name, ch_ptr);

  f_rec := TGEDCOMFamilyRecord(f_ptr);
  ch_rec := TGEDCOMIndividualRecord(ch_ptr);

  Base.Engine.AddFamilyChild(f_rec, ch_rec);

  Result := 0;
end;

function gt_define_sex(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_define_sex()';
var
  name, patr: string;
  sx: TGEDCOMSex;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectStringArg(LVM, 1, func_name, name);
  lua_ExpectStringArg(LVM, 2, func_name, patr);

  sx := DefineSex(name, patr, fmGEDKeeper.NamesTable);

  lua_pushstring(LVM, PChar(SexData[sx].LatSign));
  Result := 1;
end;

function gt_find_source(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_find_source()';
var
  src_rec: TGEDCOMSourceRecord;
  name: string;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectStringArg(LVM, 1, func_name, name);

  src_rec := Base.Engine.FindSource(name);
  lua_pushptr(LVM, src_rec);
  Result := 1;
end;

function gt_create_event(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_create_event()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
  sign: string;
  evt: TGEDCOMCustomEvent;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectStringArg(LVM, 2, func_name, sign);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  evt := CreateEventEx(Base.Tree, rec, sign, '', '');

  lua_pushptr(LVM, evt);
  Result := 1;
end;

function gt_define_patronymic(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_define_patronymic()';
var
  father_name, child_sex, child_patronymic: string;
  sex: TGEDCOMSex;
  confirm: Boolean;
begin
  lua_ExpectNArgs(LVM, func_name, 3);
  lua_ExpectStringArg(LVM, 1, func_name, father_name);
  lua_ExpectStringArg(LVM, 2, func_name, child_sex);
  lua_ExpectBooleanArg(LVM, 3, func_name, confirm);

  if (Length(child_sex) = 1)
  then sex := GetSexBySign(child_sex[1])
  else sex := svNone;

  child_patronymic := Base.DefinePatronymic(father_name, sex, confirm);

  lua_pushstring(LVM, PChar(child_patronymic));
  Result := 1;
end;

function gt_get_person_parents_family(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_parents_family()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);

  if (rec.ChildToFamilyLinksCount < 1)
  then fam := nil
  else fam := rec.ChildToFamilyLinks[0].Family;

  lua_pushptr(LVM, fam);
  Result := 1;
end;

function gt_get_person_spouses_count(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_spouses_count()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  lua_pushinteger(LVM, rec.SpouseToFamilyLinksCount);
  Result := 1;
end;

function gt_get_person_spouse_family(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_person_spouse_family()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMIndividualRecord;
  sp_idx: Integer;
  fam: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectIntArg(LVM, 2, func_name, sp_idx);

  rec := TGEDCOMIndividualRecord(rec_ptr);
  fam := rec.SpouseToFamilyLinks[sp_idx].Family;

  lua_pushptr(LVM, fam);
  Result := 1;
end;

function gt_get_family_husband(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_family_husband()';
var
  rec_ptr: Pointer;
  fam: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  fam := TGEDCOMFamilyRecord(rec_ptr);
  if (fam = nil)
  then rec_ptr := nil
  else rec_ptr := fam.Husband.Value;

  lua_pushptr(LVM, rec_ptr);
  Result := 1;
end;

function gt_get_family_wife(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_family_wife()';
var
  rec_ptr: Pointer;
  fam: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  fam := TGEDCOMFamilyRecord(rec_ptr);
  if (fam = nil)
  then rec_ptr := nil
  else rec_ptr := fam.Wife.Value;

  lua_pushptr(LVM, rec_ptr);
  Result := 1;
end;

function gt_get_family_childs_count(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_family_childs_count()';
var
  rec_ptr: Pointer;
  fam: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  fam := TGEDCOMFamilyRecord(rec_ptr);
  lua_pushinteger(LVM, fam.ChildrenCount);
  Result := 1;
end;

function gt_get_family_child(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_family_child()';
var
  rec_ptr: Pointer;
  ch_idx: Integer;
  fam: TGEDCOMFamilyRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);
  lua_ExpectIntArg(LVM, 2, func_name, ch_idx);

  fam := TGEDCOMFamilyRecord(rec_ptr);
  lua_pushptr(LVM, fam.Children[ch_idx].Value);
  Result := 1;
end;

function gt_get_location_usages(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_location_usages()';
var
  rec_ptr: Pointer;
  loc: TGEDCOMLocationRecord;
  link_list: TStringList;
  usages: Integer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  loc := TGEDCOMLocationRecord(rec_ptr);
  usages := 0;

  link_list := TStringList.Create;
  try
    GetLocationLinks(Base.Tree, loc, link_list);
    usages := link_list.Count;
  finally
    link_list.Destroy;
  end;

  lua_pushinteger(LVM, usages);
  Result := 1;
end;

function gt_get_record_notes_count(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'gt_get_record_notes_count()';
var
  rec_ptr: Pointer;
  rec: TGEDCOMRecord;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, rec_ptr);

  rec := TGEDCOMRecord(rec_ptr);

  lua_pushinteger(LVM, rec.NotesCount);
  Result := 1;
end;

{==============================================================================}

var
  csv_data: TSdfDataSet = nil;

function csv_load(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'csv_load()';
var
  file_name: string;
  first_line_is_schema: Boolean;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectStringArg(LVM, 1, func_name, file_name);
  lua_ExpectBooleanArg(LVM, 2, func_name, first_line_is_schema);

  try
    csv_data := TSdfDataSet.Create(nil);
    csv_data.FileName := file_name;
    csv_data.FirstLineAsSchema := first_line_is_schema;
    csv_data.Delimiter := ';';
    csv_data.Open;

    lua_pushboolean(LVM, True);
    Result := 1;
  except
    lua_pushboolean(LVM, False);
    Result := 1;
  end;
end;

function csv_close(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'csv_close()';
begin
  lua_ExpectNArgs(LVM, func_name, 0);

  try
    csv_data.Free;
    Result := 0;
  except
    Result := 0;
  end;
end;

function csv_get_cols(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'csv_get_cols()';
begin
  lua_ExpectNArgs(LVM, func_name, 0);

  lua_pushinteger(LVM, csv_data.Fields.Count);
  Result := 1;
end;

function csv_get_rows(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'csv_get_rows()';
var
  r_count: Integer;
begin
  lua_ExpectNArgs(LVM, func_name, 0);

  r_count := csv_data.RecordCount;
  {hack!!!}
  if (csv_data.FirstLineAsSchema) then Dec(r_count);

  lua_pushinteger(LVM, r_count);
  Result := 1;
end;

function csv_get_cell(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'csv_get_cell()';
var
  col, row: Integer;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectIntArg(LVM, 1, func_name, col);
  lua_ExpectIntArg(LVM, 2, func_name, row);

  {hack!!!}
  csv_data.First;
  csv_data.MoveBy(row);

  lua_pushstring(LVM, PChar(csv_data.Fields[col].AsString));
  Result := 1;
end;

{==============================================================================}

function ado_open(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_open()';
var
  constr: string;
  con: TADOConnection;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectStringArg(LVM, 1, func_name, constr);

  try
    con := TADOConnection.Create(nil);
    con.ConnectionString := constr;
    con.Mode := cmRead;
    con.LoginPrompt := False;
    con.Connected := True;

    lua_pushptr(LVM, con);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_close(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_close()';
var
  conptr: Pointer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, conptr);

  try
    TADOConnection(conptr).Free;
    lua_pushboolean(LVM, True);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_query_open(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_query_open()';
var
  conptr: Pointer;
  query: string;
  q_obj: TADOQuery;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, conptr);
  lua_ExpectStringArg(LVM, 2, func_name, query);

  try
    q_obj := TADOQuery.Create(nil);
    q_obj.Connection := TADOConnection(conptr);
    q_obj.SQL.Text := query;
    q_obj.Open;

    lua_pushptr(LVM, q_obj);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_query_close(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_query_close()';
var
  qptr: Pointer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, qptr);

  try
    TADOQuery(qptr).Free;
    lua_pushboolean(LVM, True);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_query_first(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_query_first()';
var
  qptr: Pointer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, qptr);

  try
    TADOQuery(qptr).First;
    lua_pushboolean(LVM, True);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_query_prev(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_query_prev()';
var
  qptr: Pointer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, qptr);

  try
    TADOQuery(qptr).Prior;
    lua_pushboolean(LVM, True);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_query_next(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_query_next()';
var
  qptr: Pointer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, qptr);

  try
    TADOQuery(qptr).Next;
    lua_pushboolean(LVM, True);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_query_last(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_query_last()';
var
  qptr: Pointer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, qptr);

  try
    TADOQuery(qptr).Last;
    lua_pushboolean(LVM, True);
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_get_query_field(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_get_query_field()';
var
  qptr: Pointer;
  fname, fval: string;
begin
  lua_ExpectNArgs(LVM, func_name, 2);
  lua_ExpectPtrArg(LVM, 1, func_name, qptr);
  lua_ExpectStringArg(LVM, 2, func_name, fname);

  try
    fval := TADOQuery(qptr).FieldByName(fname).AsString;
    lua_pushstring(LVM, PChar(fval));
    Result := 1;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

function ado_dump(LVM: Plua_State): Integer; cdecl;
const
  func_name = 'ado_dump()';
var
  conptr: Pointer;
  query: string;
  con: TADOConnection;
  tables, fields: TStringList;
  i, k: Integer;
begin
  lua_ExpectNArgs(LVM, func_name, 1);
  lua_ExpectPtrArg(LVM, 1, func_name, conptr);

  try
    con := TADOConnection(conptr);

    tables := TStringList.Create;
    fields := TStringList.Create;
    try
      con.GetTableNames(tables, False);

      lua_Output('Tables:');
      for i := 0 to tables.Count - 1 do begin
        lua_Output('  [ ' + tables[i] + ' ]');

        con.GetFieldNames(tables[i], fields);
        for k := 0 to fields.Count - 1 do begin
          lua_Output('    - ' + fields[k]);
        end;
      end;
    finally
      fields.Free;
      tables.Free;
    end;

    Result := 0;
  except
    on E: Exception do lua_DoError(LVM, 'ADOFail: ' + E.Message);
  end;
end;

{==============================================================================}

procedure lua_gk_init(LVM: Plua_State);
begin
  lua_register(LVM, PChar('gk_print'), gk_print);
  lua_register(LVM, PChar('gk_progress_init'), gk_progress_init);
  lua_register(LVM, PChar('gk_progress_done'), gk_progress_done);
  lua_register(LVM, PChar('gk_progress_step'), gk_progress_step);
  lua_register(LVM, PChar('gk_strpos'), gk_strpos);
  lua_register(LVM, PChar('gk_update_view'), gk_update_view);
  lua_register(LVM, PChar('gk_select_file'), gk_select_file);

  lua_setvar_i(LVM, 'rtNone', Ord(rtNone));
  lua_setvar_i(LVM, 'rtIndividual', Ord(rtIndividual));
  lua_setvar_i(LVM, 'rtFamily', Ord(rtFamily));
  lua_setvar_i(LVM, 'rtNote', Ord(rtNote));
  lua_setvar_i(LVM, 'rtMultimedia', Ord(rtMultimedia));
  lua_setvar_i(LVM, 'rtSource', Ord(rtSource));
  lua_setvar_i(LVM, 'rtRepository', Ord(rtRepository));
  lua_setvar_i(LVM, 'rtGroup', Ord(rtGroup));
  lua_setvar_i(LVM, 'rtResearch', Ord(rtResearch));
  lua_setvar_i(LVM, 'rtTask', Ord(rtTask));
  lua_setvar_i(LVM, 'rtCommunication', Ord(rtCommunication));
  lua_setvar_i(LVM, 'rtLocation', Ord(rtLocation));
  lua_setvar_i(LVM, 'rtSubmission', Ord(rtSubmission));
  lua_setvar_i(LVM, 'rtSubmitter', Ord(rtSubmitter));

  lua_register(LVM, PChar('gt_get_records_count'), gt_get_records_count);
  lua_register(LVM, PChar('gt_get_record'), gt_get_record);
  lua_register(LVM, PChar('gt_get_record_type'), gt_get_record_type);
  lua_register(LVM, PChar('gt_get_record_type_name'), gt_get_record_type_name);
  lua_register(LVM, PChar('gt_get_record_xref'), gt_get_record_xref);
  lua_register(LVM, PChar('gt_get_record_uid'), gt_get_record_uid);

  lua_register(LVM, PChar('gt_delete_record'), gt_delete_record);
  lua_register(LVM, PChar('gt_record_is_filtered'), gt_record_is_filtered);
  lua_register(LVM, PChar('gt_select_record'), gt_select_record);

  lua_register(LVM, PChar('gt_create_person'), gt_create_person);
  lua_register(LVM, PChar('gt_create_family'), gt_create_family);
  lua_register(LVM, PChar('gt_create_note'), gt_create_note);

  lua_register(LVM, PChar('gt_get_person_name'), gt_get_person_name);
  lua_register(LVM, PChar('gt_define_sex'), gt_define_sex);

  lua_register(LVM, PChar('gt_get_person_associations_count'), gt_get_person_associations_count);
  lua_register(LVM, PChar('gt_get_person_association'), gt_get_person_association);
  lua_register(LVM, PChar('gt_delete_person_association'), gt_delete_person_association);

  lua_register(LVM, PChar('gt_get_person_events_count'), gt_get_person_events_count);
  lua_register(LVM, PChar('gt_get_person_event'), gt_get_person_event);
  lua_register(LVM, PChar('gt_delete_person_event'), gt_delete_person_event);

  lua_register(LVM, PChar('gt_bind_record_note'), gt_bind_record_note);
  lua_register(LVM, PChar('gt_bind_record_source'), gt_bind_record_source);

  lua_register(LVM, PChar('gt_bind_family_spouse'), gt_bind_family_spouse);
  lua_register(LVM, PChar('gt_bind_family_child'), gt_bind_family_child);

  lua_register(LVM, PChar('gt_add_note_text'), gt_add_note_text);

  lua_register(LVM, PChar('gt_create_event'), gt_create_event);

  lua_register(LVM, PChar('gt_get_event_value'), gt_get_event_value);
  lua_register(LVM, PChar('gt_get_event_place'), gt_get_event_place);
  lua_register(LVM, PChar('gt_get_event_date'), gt_get_event_date);
  lua_register(LVM, PChar('gt_get_event_name'), gt_get_event_name);

  lua_register(LVM, PChar('gt_set_event_place'), gt_set_event_place);
  lua_register(LVM, PChar('gt_set_event_date'), gt_set_event_date);

  lua_register(LVM, PChar('gt_create_source'), gt_create_source);
  lua_register(LVM, PChar('gt_find_source'), gt_find_source);

  lua_register(LVM, PChar('csv_load'), csv_load);
  lua_register(LVM, PChar('csv_close'), csv_close);
  lua_register(LVM, PChar('csv_get_cols'), csv_get_cols);
  lua_register(LVM, PChar('csv_get_rows'), csv_get_rows);
  lua_register(LVM, PChar('csv_get_cell'), csv_get_cell);

  ///

  lua_register(LVM, PChar('gt_add_person_association'), gt_add_person_association);
  lua_register(LVM, PChar('gt_define_patronymic'), gt_define_patronymic);

  ///

  lua_register(LVM, PChar('gt_get_person_parents_family'), gt_get_person_parents_family);
  lua_register(LVM, PChar('gt_get_person_spouses_count'), gt_get_person_spouses_count);
  lua_register(LVM, PChar('gt_get_person_spouse_family'), gt_get_person_spouse_family);
  lua_register(LVM, PChar('gt_get_family_husband'), gt_get_family_husband);
  lua_register(LVM, PChar('gt_get_family_wife'), gt_get_family_wife);
  lua_register(LVM, PChar('gt_get_family_childs_count'), gt_get_family_childs_count);
  lua_register(LVM, PChar('gt_get_family_child'), gt_get_family_child);

  ///

  lua_register(LVM, PChar('gt_get_location_usages'), gt_get_location_usages);
  lua_register(LVM, PChar('gt_get_record_notes_count'), gt_get_record_notes_count);
  lua_register(LVM, PChar('gt_get_person_sex'), gt_get_person_sex);
  lua_register(LVM, PChar('gt_set_person_sex'), gt_set_person_sex);

  lua_register(LVM, PChar('ado_open'), ado_open);
  lua_register(LVM, PChar('ado_close'), ado_close);
  lua_register(LVM, PChar('ado_query_open'), ado_query_open);
  lua_register(LVM, PChar('ado_query_close'), ado_query_close);
  lua_register(LVM, PChar('ado_query_first'), ado_query_first);
  lua_register(LVM, PChar('ado_query_prev'), ado_query_prev);
  lua_register(LVM, PChar('ado_query_next'), ado_query_next);
  lua_register(LVM, PChar('ado_query_last'), ado_query_last);
  lua_register(LVM, PChar('ado_get_query_field'), ado_get_query_field);
  lua_register(LVM, PChar('ado_dump'), ado_dump);
end;

{$ENDIF}

procedure lua_run(script: string; aBase: TfmBase; aDebugOutput: TStrings);
{$IFNDEF DELPHI_NET}
var
  L: Plua_State;
  res: Integer;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  DebugOutput := aDebugOutput;
  Base := aBase;

  L := lua_open();
  if (L = nil) then begin
    lua_Output('Ошибка запуска Lua!');
    Exit;
  end;

  try
    try
      luaL_openlibs(L);
      lua_gk_init(L);

      res := luaL_loadstring(L, PChar(script));

      if (res = 0)
      then res := lua_pcall(L, 0, LUA_MULTRET, 0);

      if (res <> 0)
      then lua_Print(L);
    except
      on E: Exception do lua_Output('> Ошибка: ' + E.Message);
    end;

    {
    lua_getglobal(L, 'width');
    Width := lua_tonumber(L, -2);

    lua_getglobal(L, 'height');
    Height := lua_tonumber(L, -1);
    }
  finally
    lua_close(L);
  end;
  {$ENDIF}
end;

end.
