(*
** $Id: lua.h,v 1.175 2003/03/18 12:31:39 roberto Exp $
** Lua - An Extensible Extension Language
** TeCGraf: Computer Graphics Technology Group, PUC-Rio, Brazil
** http://www.lua.org   mailto:info@lua.org
** See Copyright Notice at the end of this file
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
**    - lua_upvalueindex constant was transformed to function
**    - Some compatibility function was isolated because with it you must have
**      lualib.
**    - LUA_VERSION was suffixed by '_' for avoiding name collision.
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)
(* $Updated by foofighter69@gmail.com -> 5.1.1 2008-03-26 $ *)
unit lua;

{ $DEFINE LUA_REFCOMP}

interface

const
{$IFDEF LINUX}
  LUA_NAME = 'liblua.so';
  LUA_LIB_NAME = 'liblualib.so';
{$ELSE}
  LUA_NAME = 'lua51.dll';
  LUA_LIB_NAME = 'lua51.dll';
{$ENDIF}

type
  size_t = Cardinal;
  Psize_t = ^size_t;

const
  LUA_VERSION_ = 'Lua 5.1';
  LUA_RELEASE = 'Lua 5.1.1';
  LUA_VERSION_NUM = 501;
  LUA_COPYRIGHT = 'Copyright (C) 1994-2006 TeCGraf, PUC-Rio';
  LUA_AUTHORS = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

(* mark for precompiled code (`<esc>Lua') *)
  LUA_SIGNATURE = #33+'Lua';

(* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;

(*
** pseudo-indices
*)
  LUA_REGISTRYINDEX = -10000;
  LUA_ENVIRONINDEX = -10001;
  LUA_GLOBALSINDEX = -10002;

function lua_upvalueindex(I: Integer): Integer;

(* thread status; 0 is OK *)
const
(* error codes for `lua_load' and `lua_pcall' *)
  LUA_ERRYIELD = 1; // error identifier redeclared: 'lua_yield'
  LUA_ERRRUN    = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRERR    = 5;

type
  Plua_State = Pointer;

  lua_CFunction = function(L: Plua_State): Integer; cdecl;

(*
** functions that read/write blocks when loading/dumping Lua chunks
*)
type
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: PInteger): PChar; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: Integer; ud: Pointer): Integer; cdecl;
  lua_Chunkreader = lua_Reader;
  lua_Chunkwriter = lua_Writer;

(*
** prototype for memory-allocation functions
*)
(* typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize); *)
  lua_Alloc = function(ud: Pointer; osize: size_t; nsize: size_t):Pointer;cdecl;




(*
** basic types
*)
const
  LUA_TNONE          = -1;

  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA      = 7;
  LUA_TTHREAD        = 8;

(* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

type
(* Type of Numbers in Lua *)
  lua_Number = Double;
  lua_Integer = Integer;
(*
** state manipulation
*)

function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_state; cdecl;
procedure lua_close(L: Plua_State); cdecl;
function lua_newthread(L: Plua_State): Plua_State; cdecl;

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

(*
** basic stack manipulation
*)
function lua_gettop(L: Plua_State): Integer; cdecl;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl;
procedure lua_pushvalue(L: Plua_State; Idx: Integer); cdecl;
procedure lua_remove(L: Plua_State; idx: Integer); cdecl;
procedure lua_insert(L: Plua_State; idx: Integer); cdecl;
procedure lua_replace(L: Plua_State; idx: Integer); cdecl;
function lua_checkstack(L: Plua_State; sz: Integer): LongBool; cdecl;

procedure lua_xmove(from, to_: Plua_State; n: Integer); cdecl;

(*
** access functions (stack -> C)
*)
function lua_isnumber(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl;
function lua_typename(L: Plua_State; tp: Integer): PChar; cdecl;

function lua_equal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_lessthan(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl;

	(* change: tostring, strlen vanished *)
function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; cdecl;
function lua_toboolean(L: Plua_State; idx: Integer): LongBool; cdecl;
function lua_tolstring(L: Plua_State; idx: Integer; len: Integer): PChar; cdecl;
function lua_objlen(L: Plua_State; idx: Integer): size_t; cdecl;
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl;

(*
** push functions (C -> stack)
*)
procedure lua_pushnil(L: Plua_State); cdecl;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl;

procedure lua_pushlstring(L: Plua_State; const s: PChar; l_: size_t); cdecl; 
procedure lua_pushstring(L: Plua_State; const s: PChar); cdecl;
function lua_pushvfstring(L: Plua_State; const fmt: PChar; argp: Pointer): PChar; cdecl;

function lua_pushfstring(L: Plua_State; const fmt: PChar): PChar; cdecl; varargs; (* !varargs *)
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl;
procedure lua_pushboolean(L: Plua_State; b: LongBool); cdecl; (* memo *)
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl;
function lua_pushthread(L: Plua_State): Integer; cdecl;

(*
** get functions (Lua -> stack)
*)
procedure lua_gettable(L: Plua_State; idx: Integer); cdecl;
procedure lua_getfield(L: Plua_State; idx: Integer; k: Pchar); cdecl;
procedure lua_rawget(L: Plua_State; idx: Integer); cdecl;
procedure lua_rawgeti(L: Plua_State; idx, n: Integer); cdecl;
procedure lua_createtable(L: Plua_State; narr: Integer; nrec: Integer); cdecl; (* old newtable *)
function lua_newuserdata(L: Plua_State; sz: Integer): Pointer; cdecl;
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
procedure lua_getfenv(L: Plua_State; idx: Integer); cdecl;

(*
** set functions (stack -> Lua)
*)
procedure lua_settable(L: Plua_State; idx: Integer); cdecl;
procedure lua_setfield(L: Plua_State; idx: Integer; const k: Pchar); cdecl; (* new *)
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl;
procedure lua_rawseti(L: Plua_State; idx, n: Integer); cdecl;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl;
function lua_setfenv(L: Plua_State; idx: Integer): Integer; cdecl;

(*
** `load' and `call' functions (load and run Lua code)
*)
procedure lua_call(L: Plua_State; nargs, nresults: Integer); cdecl;
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
function lua_cpcall(L: Plua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl;
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PChar): Integer; cdecl;

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer): Integer; cdecl;

(*
** coroutine functions
*)
function lua_yield(L: Plua_State; nresults: Integer): Integer; cdecl;
function lua_resume(L: Plua_State; narg: Integer): Integer; cdecl;
function lua_status(L: Plua_State): Integer; cdecl;

(*
** Garbage-collection functions
*)
(* removed:
function lua_getgcthreshold(L: Plua_State): Integer; cdecl;
function lua_getgccount(L: Plua_State): Integer; cdecl; 
procedure lua_setgcthreshold(L: Plua_State; newthreshold: Integer); cdecl; *)
const 
  LUA_GCSTOP = 0;
  LUA_GCRESTART = 1;
  LUA_GCCOLLECT = 2;
  LUA_GCCOUNT = 3;
  LUA_GCCOUNTB = 4;
  LUA_GCSTEP = 5;
  LUA_GCSETPAUSE = 6;
  LUA_GCSETSTEPMUL = 7;

function lua_gc(L: Plua_State; what: Integer; data: Integer):Integer;cdecl;
(*
** miscellaneous functions
*)
(* function lua_version: PChar; cdecl; *) (* removed *)

function lua_error(L: Plua_State): Integer; cdecl;

function lua_next(L: Plua_State; idx: Integer): Integer; cdecl;

procedure lua_concat(L: Plua_State; n: Integer); cdecl;

(*
LUA_API lua_Alloc (lua_getallocf) (lua_State *L, void **ud);
LUA_API void lua_setallocf (lua_State *L, lua_Alloc f, void *ud);
*)
function lua_getallocf(L: Plua_state; ud: PPointer): lua_Alloc;cdecl;
procedure lua_setallocf(L: Plua_state; f: lua_Alloc; ud: Pointer);cdecl;

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

//procedure lua_boxpointer(L: Plua_State; u: Pointer); (-)

//function lua_unboxpointer(L: Plua_State; i: Integer): Pointer; (-)

procedure lua_pop(L: Plua_State; n: Integer);
(* #define lua_newtable(L)		lua_createtable(L, 0, 0) *)
procedure lua_newtable(L: Plua_State);

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);

(* #define lua_strlen(L,i)		lua_objlen(L, (i)) *)
function lua_strlen(L: Plua_State; i: Integer): Integer;


function lua_isfunction(L: Plua_State; n: Integer): Boolean;
function lua_istable(L: Plua_State; n: Integer): Boolean;
function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
function lua_isnil(L: Plua_State; n: Integer): Boolean;
function lua_isboolean(L: Plua_State; n: Integer): Boolean;
(* #define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD) *)
function lua_isthread(L: Plua_State; n: Integer): Boolean;
function lua_isnone(L: Plua_State; n: Integer): Boolean;
function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;

procedure lua_pushliteral(L: Plua_State; s: PChar);

procedure lua_setglobal(L: Plua_State; const s: PChar);
procedure lua_getglobal(L: Plua_State; const s: PChar);

(* #define lua_tostring(L,i)	lua_tolstring(L, (i), NULL) *)
function lua_tostring(L: Plua_State; i: Integer):Pchar;
(*
** compatibility macros and functions
*)
(* function lua_pushupvalues(L: Plua_State): Integer; cdecl; *)

(* #define lua_open()	luaL_newstate() *)
function lua_open: Plua_State;
procedure lua_getregistry(L: Plua_State);



(* compatibility with ref system*)

(* pre-defined references *)
const
  LUA_NOREF = -2;
  LUA_REFNIL = -1;

function lua_ref(L: Plua_State; lock: Integer): Integer;
procedure lua_unref(L: Plua_State; ref: Integer);
procedure lua_getref(L: Plua_State; ref: Integer);

(*
** {======================================================================
** Debug API
** =======================================================================
*)

const
  LUA_HOOKCALL    = 0;
  LUA_HOOKRET     = 1;
  LUA_HOOKLINE    = 2;
  LUA_HOOKCOUNT   = 3;
  LUA_HOOKTAILRET = 4;
(*
** Event masks
*)
const
  LUA_MASKCALL  = 1 shl Ord(LUA_HOOKCALL);
  LUA_MASKRET   = 1 shl Ord(LUA_HOOKRET);
  LUA_MASKLINE  = 1 shl Ord(LUA_HOOKLINE);
  LUA_MASKCOUNT = 1 shl Ord(LUA_HOOKCOUNT);

const
  LUA_IDSIZE = 60;

type
  lua_Debug = record           (* activation record *)
    event: Integer;
    name: PChar;               (* (n) *)
    namewhat: PChar;           (* (n) `global', `local', `field', `method' *)
    what: PChar;               (* (S) `Lua', `C', `main', `tail'*)
    source: PChar;             (* (S) *)
    currentline: Integer;      (* (l) *)
    nups: Integer;             (* (u) number of upvalues *)
    linedefined: Integer;      (* (S) *)
    short_src: array[0..LUA_IDSIZE - 1] of Char; (* (S) *)
    (* private part *)
    i_ci: Integer;              (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl;
function lua_getinfo(L: Plua_State; const what: PChar; ar: Plua_Debug): Integer; cdecl;
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl;
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl;
function lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl;
function lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl;

function lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer): Integer; cdecl;
function lua_gethook(L: Plua_State): lua_hook; cdecl;
function lua_gethookmask(L: Plua_State): Integer; cdecl;
function lua_gethookcount(L: Plua_State): Integer; cdecl;

implementation

//{$IFDEF LUA_REFCOMP}
uses
  lauxlib;
//{$ENDIF}

function lua_upvalueindex(I: Integer): Integer;
begin
  Result := LUA_GLOBALSINDEX - i;
end;

function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_state; cdecl; external LUA_NAME;
procedure lua_close(L: Plua_State); cdecl; external LUA_NAME;
function lua_newthread(L: Plua_State): Plua_State; cdecl; external LUA_NAME;

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external LUA_NAME;

function lua_gettop(L: Plua_State): Integer; cdecl; external LUA_NAME;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_pushvalue(L: Plua_State; Idx: Integer); cdecl; external LUA_NAME;
procedure lua_remove(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_insert(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_replace(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
function lua_checkstack(L: Plua_State; sz: Integer): LongBool; cdecl; external LUA_NAME;
procedure lua_xmove(from, to_: Plua_State; n: Integer); cdecl; external LUA_NAME;

function lua_isnumber(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_isstring(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_iscfunction(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_isuserdata(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_NAME;
function lua_typename(L: Plua_State; tp: Integer): PChar; cdecl; external LUA_NAME;

function lua_equal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_NAME;
function lua_rawequal(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_NAME;
function lua_lessthan(L: Plua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_NAME;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; cdecl; external LUA_NAME;
function lua_toboolean(L: Plua_State; idx: Integer): LongBool; cdecl; external LUA_NAME;
(* function lua_tostring(L: Plua_State; idx: Integer): PChar; cdecl; external LUA_NAME; *)
function lua_tolstring(L: Plua_State; idx: Integer; len: Integer): PChar; cdecl; external LUA_NAME;

(* function lua_strlen(L: Plua_State; idx: Integer): size_t; cdecl; external LUA_NAME; *)
 function lua_objlen(L: Plua_State; idx: Integer): size_t; cdecl; external LUA_NAME; 

function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl; external LUA_NAME;
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_NAME;
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl; external LUA_NAME;
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_NAME;

procedure lua_pushnil(L: Plua_State); cdecl; external LUA_NAME;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl; external LUA_NAME;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl; external LUA_NAME; (* new *)

procedure lua_pushlstring(L: Plua_State; const s: PChar; l_: size_t); cdecl; external LUA_NAME;
procedure lua_pushstring(L: Plua_State; const s: PChar); cdecl; external LUA_NAME;
function lua_pushvfstring(L: Plua_State; const fmt: PChar; argp: Pointer): PChar; cdecl; external LUA_NAME;
function lua_pushfstring(L: Plua_State; const fmt: PChar): PChar; cdecl; varargs; external LUA_NAME;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl; external LUA_NAME;
procedure lua_pushboolean(L: Plua_State; b: LongBool); cdecl; external LUA_NAME;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl; external LUA_NAME;
(* new *)
function lua_pushthread(L: Plua_State): Integer; cdecl; external LUA_NAME;

procedure lua_gettable(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_getfield(L: Plua_State; idx: Integer; k: Pchar); cdecl; external LUA_NAME; (* new *)

procedure lua_rawget(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_rawgeti(L: Plua_State; idx, n: Integer); cdecl; external LUA_NAME;
(* procedure lua_newtable(L: Plua_State); cdecl; external LUA_NAME; *)
procedure lua_createtable(L: Plua_State; narr: Integer; nrec: Integer); cdecl; external LUA_NAME;
function lua_newuserdata(L: Plua_State; sz: Integer): Pointer; cdecl; external LUA_NAME;
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_NAME;
procedure lua_getfenv(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;

procedure lua_settable(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_setfield(L: Plua_State; idx: Integer; const k: Pchar); cdecl; external LUA_NAME; (* new *)
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external LUA_NAME;
procedure lua_rawseti(L: Plua_State; idx, n: Integer); cdecl; external LUA_NAME;
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_NAME;
function lua_setfenv(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_NAME;

procedure lua_call(L: Plua_State; nargs, nresults: Integer); cdecl; external LUA_NAME;
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl; external LUA_NAME;
function lua_cpcall(L: Plua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl; external LUA_NAME;
function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname: PChar): Integer; cdecl; external LUA_NAME;

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer): Integer; cdecl; external LUA_NAME;

function lua_yield(L: Plua_State; nresults: Integer): Integer; cdecl; external LUA_NAME;
function lua_resume(L: Plua_State; narg: Integer): Integer; cdecl; external LUA_NAME;
function lua_status(L: Plua_State): Integer; cdecl; external LUA_NAME;(* new *)
 
(* old:
function lua_getgcthreshold(L: Plua_State): Integer; cdecl; external LUA_NAME;
function lua_getgccount(L: Plua_State): Integer; cdecl; external LUA_NAME;
procedure lua_setgcthreshold(L: Plua_State; newthreshold: Integer); cdecl; external LUA_NAME;
*)
function lua_gc(L: Plua_State; what: Integer; data: Integer):Integer;cdecl; external LUA_NAME;(* new *)

(* old:
function lua_version: PChar; cdecl; external LUA_NAME;
*)
function lua_error(L: Plua_State): Integer; cdecl; external LUA_NAME;
function lua_next(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_NAME;
procedure lua_concat(L: Plua_State; n: Integer); cdecl; external LUA_NAME;

function lua_getallocf(L: Plua_state; ud: PPointer): lua_Alloc;cdecl; external LUA_NAME;(* new *)
procedure lua_setallocf(L: Plua_state; f: lua_Alloc; ud: Pointer);cdecl; external LUA_NAME;(* new *)


(* some usefule macros *)
(*
procedure lua_boxpointer(L: Plua_State; u: Pointer);
type
  PPointer = ^Pointer;
begin
  PPointer(lua_newuserdata(L, SizeOf(Pointer)))^ := u;
end;

function lua_unboxpointer(L: Plua_State; i: Integer): Pointer;
type
  PPointer = ^Pointer;
begin
  Result := PPointer(lua_touserdata(L, i))^;
end;
*)

procedure lua_pop(L: Plua_State; n: Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L: Plua_State); (* new *)
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
begin
  lua_pushstring(L, n);
  lua_pushcfunction(L, f);
  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_strlen(L: Plua_State; i: Integer): Integer;(* new *)
begin
  Result := lua_objlen(L,i);
end;

function lua_isfunction(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L: Plua_State; s: PChar);
begin
  lua_pushlstring(L, s, (SizeOf(s) div SizeOf(Char)) - 1);
end;

(* function lua_pushupvalues(L: Plua_State): Integer; cdecl; external LUA_NAME; *) (* old *)

procedure lua_getregistry(L: Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

procedure lua_setglobal(L: Plua_State; const s: PChar);
begin
  lua_pushstring(L, s);
  lua_insert(L, -2);
  lua_settable(L, LUA_GLOBALSINDEX);
end;

procedure lua_getglobal(L: Plua_State; const s: PChar);
begin
  lua_pushstring(L, s);
  lua_gettable(L, LUA_GLOBALSINDEX);
end;

(* function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_state; cdecl; *)
function lua_open: Plua_state;
begin
  Result := luaL_newstate();
end;
 (* #define lua_tostring(L,i)	lua_tolstring(L, (i), NULL) *)
function lua_tostring(L: Plua_State; i: Integer):Pchar;
begin
  Result := lua_tolstring(L,i,0);
end;
(* compatibility with ref system*)

function lua_ref(L: Plua_State; lock: Integer): Integer;
begin
{$IFDEF LUA_REFCOMP}
  Result := 0;
  if lock <> 0 then
  begin
    Result := luaL_ref(L, LUA_REGISTRYINDEX);
  end
  else
  begin
    lua_pushstring(L, PChar('unlocked references are obsolete'));
    lua_error(L);
  end;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure lua_unref(L: Plua_State; ref: Integer);
begin
{$IFDEF LUA_REFCOMP}
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
{$ENDIF}
end;

procedure lua_getref(L: Plua_State; ref: Integer);
begin
{$IFDEF LUA_REFCOMP}
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
{$ENDIF}
end;

(*
** {======================================================================
** Debug API
** =======================================================================
*)

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl; external LUA_NAME;
function lua_getinfo(L: Plua_State; const what: PChar; ar: Plua_Debug): Integer; cdecl; external LUA_NAME;
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PChar; cdecl; external LUA_NAME;
function lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer): Integer; cdecl; external LUA_NAME;
function lua_gethook(L: Plua_State): lua_hook; cdecl; external LUA_NAME;
function lua_gethookmask(L: Plua_State): Integer; cdecl; external LUA_NAME;
function lua_gethookcount(L: Plua_State): Integer; cdecl; external LUA_NAME;
(******************************************************************************
* Copyright (C) 1994-2006 Tecgraf, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
end.
