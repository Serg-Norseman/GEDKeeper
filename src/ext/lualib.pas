(*
** $Id: lualib.h,v 1.28 2003/03/18 12:24:26 roberto Exp $
** Lua standard libraries
** See Copyright Notice in lua.h
*)
(*
** Translated to pascal by Lavergne Thomas
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)
(* $Updated by foofighter69@gmail.com -> 5.1.1 2008-03-26 $ *)
unit lualib;

interface

uses
  Lua;

const
  LUA_COLIBNAME = 'coroutine';

function luaopen_base(L: Plua_State): LongBool; cdecl;

const
  LUA_TABLIBNAME = 'table';

function luaopen_table(L: Plua_State): LongBool; cdecl;

const
  LUA_IOLIBNAME = 'io';

function luaopen_io(L: Plua_State): LongBool; cdecl;

const
  LUA_OSLIBNAME = 'os';

function luaopen_os(L: Plua_State): LongBool; cdecl; (* new *)

const
  LUA_STRLIBNAME = 'string';

function luaopen_string(L: Plua_State): LongBool; cdecl;

const
  LUA_MATHLIBNAME = 'math';

function luaopen_math(L: Plua_State): LongBool; cdecl;

const
  LUA_DBLIBNAME = 'debug';

function luaopen_debug(L: Plua_State): LongBool; cdecl;

const
  LUA_PACKAGENAME = 'package';

function luaopen_package(L: Plua_State): LongBool; cdecl;

(* function luaopen_loadlib(L: Plua_State): LongWord; cdecl; *)
procedure luaL_openlibs(L: Plua_State);cdecl;

(*
LUALIB_API void luaL_openlibs (lua_State *L) {
  const luaL_Reg *lib = lualibs;
  for (; lib->func; lib++) {
    lua_pushcfunction(L, lib->func);
    lua_pushstring(L, lib->name);
    lua_call(L, 1, 0);
  }
}
*)


(* compatibility code *)

function lua_baselibopen(L: Plua_State): LongBool;
function lua_tablibopen(L: Plua_State): LongBool;
function lua_iolibopen(L: Plua_State): LongBool;
function lua_strlibopen(L: Plua_State): LongBool;
function lua_mathlibopen(L: Plua_State): LongBool;
function lua_dblibopen(L: Plua_State): LongBool;

implementation

function luaopen_base(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_table(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_io(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_os(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_string(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_math(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_debug(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_package(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME; (* new *)

(* function luaopen_loadlib(L: Plua_State): LongWord; cdecl; external LUA_LIB_NAME;*)
procedure luaL_openlibs(L: Plua_State);cdecl; external  LUA_LIB_NAME;(* new *)

function lua_baselibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_base(L);
end;

function lua_tablibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_table(L);
end;

function lua_iolibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_io(L);
end;

function lua_strlibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_string(L);
end;

function lua_mathlibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_math(L);
end;

function lua_dblibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_debug(L);
end;


end.