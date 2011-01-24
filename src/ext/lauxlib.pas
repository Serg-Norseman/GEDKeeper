(*
** $Id: lauxlib.h,v 1.59 2003/03/18 12:25:32 roberto Exp $
** Auxiliary functions for building Lua libraries
** See Copyright Notice in lua.h
*)
(*
** Translated to pascal by Lavergne Thomas
** Notes :
**    - Pointers type was prefixed with 'P'
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)
(* $Updated by foofighter69@gmail.com -> 5.1.1 2008-03-26 $ *)
unit lauxlib;

interface

uses
  Lua;

const
  LUA_ERRFILE = LUA_ERRERR+1; (* new *)
type
  luaL_reg = record
    name: PChar;
    func: lua_CFunction;
  end;
  PluaL_reg = ^luaL_reg;


procedure luaI_openlib(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer); cdecl;
(* LUALIB_API void (luaL_register) (lua_State *L, const char *libname,
                                const luaL_Reg *l) *)
procedure luaL_register(L: Plua_State; const libname: Pchar; const l_: luaL_Reg);cdecl;
function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl;
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl;
function luaL_typerror(L: Plua_State; narg: Integer; const tname: PChar): Integer; cdecl;
function luaL_argerror(L: Plua_State; numarg: Integer; const extramsg: PChar): Integer; cdecl;
function luaL_checklstring(L: Plua_State; numArg: Integer; l_: Psize_t): PChar; cdecl;
function luaL_optlstring(L: Plua_State; numArg: Integer; const def: PChar; l_: Psize_t): PChar; cdecl;
function luaL_checknumber(L: Plua_State; numArg: Integer): lua_Number; cdecl;
function luaL_optnumber(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl;
(* new *)
function luaL_checkinteger(L: Plua_State; numArg: Integer): lua_Integer; cdecl;
function luaL_optinteger(L: Plua_State; nArg: Integer; def: lua_Integer): lua_Integer; cdecl;

procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PChar); cdecl;
procedure luaL_checktype(L: Plua_State; narg, t: Integer); cdecl;
procedure luaL_checkany(L: Plua_State; narg: Integer); cdecl;

function luaL_newmetatable(L: Plua_State; const tname: PChar): Integer; cdecl;
(* procedure luaL_getmetatable(L: Plua_State; const tname: PChar); cdecl; *) (* old *)
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PChar): Pointer; cdecl;

procedure luaL_where(L: Plua_State; lvl: Integer); cdecl;
function luaL_error(L: Plua_State; const fmt: PChar; argsup: array of pointer): Integer; cdecl;

(* function luaL_findstring(st: PChar; const lst: PPChar): Integer; cdecl; *) (* old *)

function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl;

function luaL_getn(L: Plua_State; n: Integer): Integer; cdecl;
procedure luaL_setn(L: Plua_State; t, n: Integer); cdecl;

function luaL_loadfile(L: Plua_State; const filename: PChar): Integer; cdecl;
function luaL_loadbuffer(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl;
(* 
LUALIB_API int (luaL_loadstring) (lua_State *L, const char *s);

LUALIB_API lua_State *(luaL_newstate) (void);

LUALIB_API const char *(luaL_gsub) (lua_State *L, const char *s, const char *p,
                                                  const char *r);

LUALIB_API const char *(luaL_findtable) (lua_State *L, int idx,
                                         const char *fname, int szhint);

*)

function luaL_loadstring(L: Plua_State; const s: PChar):Integer;cdecl;
function luaL_newstate: Plua_State; cdecl;
function luaL_gsub(L: Plua_State; const s: PChar; const p: PChar; const r: PChar):PChar; cdecl;
function luaL_findtable(L: Plua_State; idx: Integer; const fname: PChar; szhint: Integer):PChar;cdecl;



(*
** ===============================================================
** some useful macros
** ===============================================================
*)

procedure luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
function luaL_checkstring(L: Plua_State; n: Integer): PChar;
function luaL_optstring(L: Plua_State; n: Integer; d: PChar): PChar;
function luaL_checkint(L: Plua_State; n: Integer): Integer;
function luaL_checklong(L: Plua_State; n: Integer): LongInt;
function luaL_optint(L: Plua_State; n: Integer; d: Double): Integer;
function luaL_optlong(L: Plua_State; n: Integer; d: Double): LongInt;

(*
#define luaL_typename(L,i)	lua_typename(L, lua_type(L,(i)))

#define luaL_dofile(L, fn) \
	(luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0))

#define luaL_dostring(L, s) \
	(luaL_loadstring(L, s) || lua_pcall(L, 0, LUA_MULTRET, 0))

#define luaL_getmetatable(L,n)	(lua_getfield(L, LUA_REGISTRYINDEX, (n)))

#define luaL_opt(L,f,n,d)	(lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))
*)

function luaL_typename(L: Plua_State; i: Integer):PChar;
function luaL_dofile(L: Plua_State; const fn: PChar):Integer;
function lua_dofile(L: Plua_State; const fn: PChar):Integer;
function luaL_dostring(L: Plua_State; const fn: PChar):Integer;
procedure lua_getmetatable(L:Plua_state; n: PChar);

//function luaL_opt(L: Plua_state;f: Pointer;n: Integer;d: Double):Double; (* ? *)
//function luaL_opt(L: Plua_state;f: Pointer;n: Integer;d: Double):Double;
(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

const
  LUAL_BUFFERSIZE = 4096;

type
  luaL_Buffer = record
    p: PChar;       (* current position in buffer *)
    lvl: Integer;   (* number of strings in the stack (level) *)
    L: Plua_State;
    buffer: array [0..LUAL_BUFFERSIZE - 1] of Char;
  end;
  PluaL_Buffer = ^luaL_Buffer;

procedure luaL_putchar(B: PluaL_Buffer; c: Char);

procedure luaL_addchar(B: PluaL_Buffer; c: Char);(* macro *)

procedure luaL_addsize(B: PluaL_Buffer; n: Integer);

procedure luaL_buffinit(L: Plua_State ; B: PluaL_Buffer); cdecl;
function luaL_prepbuffer(B: PluaL_Buffer): PChar; cdecl;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PChar; l: size_t); cdecl;
procedure luaL_addstring(B: PluaL_Buffer; const s: PChar); cdecl;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl;

(*
** Compatibility macros and functions
*)
(*
function luaL_dofile(L: Plua_State; const filename: PChar): Integer; cdecl;
function luaL_dostring(L: Plua_State; const str: PChar): Integer; cdecl;
function luaL_dobuffer(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl;
*)
function luaL_check_lstr(L: Plua_State; numArg: Integer; len: Psize_t): PChar;
function luaL_opt_lstr(L: Plua_State; numArg: Integer; const def: PChar; len: Psize_t): PChar;
function luaL_check_number(L: Plua_State; numArg: Integer): lua_Number;
function luaL_opt_number(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number;
procedure luaL_arg_check(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
function luaL_check_string(L: Plua_State; n: Integer): PChar;
function luaL_opt_string(L: Plua_State; n: Integer; d: PChar): PChar;
function luaL_check_int(L: Plua_State; n: Integer): Integer;
function luaL_check_long(L: Plua_State; n: Integer): LongInt;
function luaL_opt_int(L: Plua_State; n: Integer; d: Double): Integer;
function luaL_opt_long(L: Plua_State; n: Integer; d: Double): LongInt;

procedure luaL_openlib(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer);(* new *)

implementation



procedure luaI_openlib(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer); cdecl; external LUA_LIB_NAME;
function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_register(L: Plua_State; const libname: Pchar; const l_: luaL_Reg);cdecl;  external LUA_LIB_NAME; (* new *)
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_typerror(L: Plua_State; narg: Integer; const tname: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_argerror(L: Plua_State; numarg: Integer; const extramsg: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_checklstring(L: Plua_State; numArg: Integer; l_: Psize_t): PChar; cdecl; external LUA_LIB_NAME;
function luaL_optlstring(L: Plua_State; numArg: Integer; const def: PChar; l_: Psize_t): PChar; cdecl; external LUA_LIB_NAME;
function luaL_checknumber(L: Plua_State; numArg: Integer): lua_Number; cdecl; external LUA_LIB_NAME;
function luaL_optnumber(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl; external LUA_LIB_NAME;

(* new *)
function luaL_checkinteger(L: Plua_State; numArg: Integer): lua_Integer; cdecl; external LUA_LIB_NAME;
function luaL_optinteger(L: Plua_State; nArg: Integer; def: lua_Integer): lua_Integer; cdecl; external LUA_LIB_NAME;



procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PChar); cdecl; external LUA_LIB_NAME;
procedure luaL_checktype(L: Plua_State; narg, t: Integer); cdecl; external LUA_LIB_NAME;
procedure luaL_checkany(L: Plua_State; narg: Integer); cdecl; external LUA_LIB_NAME;

function luaL_newmetatable(L: Plua_State; const tname: PChar): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_getmetatable(L: Plua_State; const tname: PChar); cdecl; external LUA_LIB_NAME;
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PChar): Pointer; cdecl; external LUA_LIB_NAME;

procedure luaL_where(L: Plua_State; lvl: Integer); cdecl; external LUA_LIB_NAME;
function luaL_error(L: Plua_State; const fmt: PChar; argsup: array of pointer): Integer; cdecl; external LUA_LIB_NAME;

function luaL_findstring(st: PChar; const lst: PPChar): Integer; cdecl; external LUA_LIB_NAME;

function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl; external LUA_LIB_NAME;

function luaL_getn(L: Plua_State; n: Integer): Integer; cdecl; external LUA_LIB_NAME;
procedure luaL_setn(L: Plua_State; t, n: Integer); cdecl; external LUA_LIB_NAME;

function luaL_loadfile(L: Plua_State; const filename: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_loadbuffer(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl; external LUA_LIB_NAME;

function luaL_loadstring(L: Plua_State; const s: PChar):Integer; cdecl; external LUA_LIB_NAME;(* new *)
function luaL_newstate: Plua_state; cdecl; external LUA_LIB_NAME;(* new *)
function luaL_gsub(L: Plua_State; const s: PChar; const p: PChar; const r: PChar): PChar; cdecl; external LUA_LIB_NAME;(* new *)
function luaL_findtable(L: Plua_State; idx: Integer; const fname: PChar; szhint: Integer):PChar; cdecl; external LUA_LIB_NAME;(* new *)











procedure luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
begin
  if not cond then
    luaL_argerror(L, numarg, extramsg)
end;

function luaL_checkstring(L: Plua_State; n: Integer): PChar;
begin
  Result := luaL_checklstring(L, n, nil)
end;

function luaL_optstring(L: Plua_State; n: Integer; d: PChar): PChar;
begin
  Result := luaL_optlstring(L, n, d, nil)
end;

function luaL_checkint(L: Plua_State; n: Integer): Integer;
begin
  Result := Integer(Trunc(luaL_checknumber(L, n)))
end;

function luaL_checklong(L: Plua_State; n: Integer): LongInt;
begin
  Result := LongInt(Trunc(luaL_checknumber(L, n)))
end;

function luaL_optint(L: Plua_State; n: Integer; d: Double): Integer;
begin
  Result := Integer(Trunc(luaL_optnumber(L, n, d)))
end;

function luaL_optlong(L: Plua_State; n: Integer; d: Double): LongInt;
begin
  Result := LongInt(Trunc(luaL_optnumber(L, n, d)))
end;

(* #define luaL_putchar(B,c) \
  ((void)((B)->p < ((B)->buffer+LUAL_BUFFERSIZE) || luaL_prepbuffer(B)), \
   (*(B)->p++ = (char)(c))) *)
procedure luaL_addchar(B: PluaL_Buffer; c: Char);
begin
  if Cardinal(@(B^.p)) < (Cardinal(@(B^.buffer[0])) + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B^.p[1] := c;
  B^.p := B^.p + 1;
end;
procedure luaL_putchar(B: PluaL_Buffer; c: Char);
begin
 luaL_addchar(B, c);
end;
  
  
procedure luaL_addsize(B: PluaL_Buffer; n: Integer);
begin
  B^.p := B^.p + n;
end;

procedure luaL_buffinit(L: Plua_State ; B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
function luaL_prepbuffer(B: PluaL_Buffer): PChar; cdecl; external LUA_LIB_NAME;
procedure luaL_addlstring(B: PluaL_Buffer; const s: PChar; l: size_t); cdecl; external LUA_LIB_NAME;
procedure luaL_addstring(B: PluaL_Buffer; const s: PChar); cdecl; external LUA_LIB_NAME;
procedure luaL_addvalue(B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
procedure luaL_pushresult(B: PluaL_Buffer); cdecl; external LUA_LIB_NAME;
(*
function luaL_dofile(L: Plua_State; const filename: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_dostring(L: Plua_State; const str: PChar): Integer; cdecl; external LUA_LIB_NAME;
function luaL_dobuffer(L: Plua_State; const buff: PChar; size: Integer; const name: PChar): Integer; cdecl; external LUA_LIB_NAME;
*)
(* #define luaL_dofile(L, fn) \
	(luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0)) *)

function luaL_dofile(L: Plua_State; const fn: PChar):Integer;
var
	i: integer;
begin
	i := luaL_loadfile(L, fn);
	if i = 0 then 
		i := lua_pcall(L, 0, LUA_MULTRET, 0);
	Result:=i;
end;
function lua_dofile(L: Plua_State; const fn: PChar):Integer;
begin
       Result:=luaL_dofile(L,fn);
end;

function luaL_dostring(L: Plua_State; const fn: PChar):Integer;
begin
	Result:=luaL_dofile(L, fn);
end;

function luaL_typename(L: Plua_State; i: Integer):PChar;
begin
	Result := lua_typename(L, i);
end;	
	
procedure lua_getmetatable(L: Plua_State; n: PChar);
begin
	lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

(* #define luaL_opt(L,f,n,d)	(lua_isnoneornil(L,(n)) ? (d) : f(L,(n))) *)
(* wrong solution:
function luaL_opt(L: Plua_state;f: PChar;n: Integer;d: Double):Double;
begin
	if lua_isnoneornil(L, n) then
		Result := d
	else
	Result := f(L, n); // I can't solve this situation
end;
*)
(* Executing a function only by its name *)

function luaL_check_lstr(L: Plua_State; numArg: Integer; len: Psize_t): PChar;
begin
  Result := luaL_checklstring(L, numArg, len);
end;

function luaL_opt_lstr(L: Plua_State; numArg: Integer; const def: PChar; len: Psize_t): PChar;
begin
  Result := luaL_optlstring(L, numArg, def, len);
end;

function luaL_check_number(L: Plua_State; numArg: Integer): lua_Number;
begin
  Result := luaL_checknumber(L, numArg);
end;

function luaL_opt_number(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number;
begin
  Result := luaL_optnumber(L, nArg, def);
end;

procedure luaL_arg_check(L: Plua_State; cond: Boolean; numarg: Integer; extramsg: PChar);
begin
  luaL_argcheck(L, cond, numarg, extramsg);
end;

function luaL_check_string(L: Plua_State; n: Integer): PChar;
begin
  Result := luaL_checkstring(L, n);
end;

function luaL_opt_string(L: Plua_State; n: Integer; d: PChar): PChar;
begin
  Result := luaL_optstring(L, n, d);
end;

function luaL_check_int(L: Plua_State; n: Integer): Integer;
begin
  Result := luaL_checkint(L, n);
end;

function luaL_check_long(L: Plua_State; n: Integer): LongInt;
begin
  Result := luaL_checklong(L, n);
end;

function luaL_opt_int(L: Plua_State; n: Integer; d: Double): Integer;
begin
  Result := luaL_optint(L, n, d);
end;

function luaL_opt_long(L: Plua_State; n: Integer; d: Double): LongInt;
begin
  Result := luaL_optlong(L, n, d);
end;



procedure luaL_openlib(L: Plua_State; const libname: PChar; const lr: PluaL_reg; nup: Integer);(* new *)
begin
  luaI_openlib(L, libname, lr, nup);
end;

end.