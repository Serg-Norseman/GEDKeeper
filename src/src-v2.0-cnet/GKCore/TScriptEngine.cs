using GKSys;
using GKUI;
using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKCore
{
	public class TScriptEngine
	{
		public void lua_run(string script, TfmBase aBase, TextBox aDebugOutput)
		{
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
