using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Threading;
using System.Windows.Forms;

using GKCore;
using GKSys;
using GKUI;

[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009-2012 Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyTitle("GEDKeeper2")]
[assembly: AssemblyVersion("2.0.467.1")]

[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]

[assembly: ComVisible(false)]
//[assembly: Guid("")]
//[assembly: TypeLibVersion(1, 0)]

/// <summary>
/// Localization: clean
/// </summary>

namespace GEDKeeper2
{
	internal sealed class Program
	{
		[STAThread]
		[SecurityPermission(SecurityAction.Demand, Flags=SecurityPermissionFlag.ControlAppDomain)]
		private static void Main(string[] args)
		{
			bool isFirstInstance;
			using (Mutex mtx = new Mutex(true, TGenEngine.AppTitle, out isFirstInstance)) {
				if (isFirstInstance) {
					Application.ThreadException += new System.Threading.ThreadExceptionEventHandler(ExExceptionHandler);
					Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException, true);
					AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledExceptionsHandler);

					Application.EnableVisualStyles();
					Application.SetCompatibleTextRenderingDefault(false);
					Application.Run(new TfmGEDKeeper(args));
				} else {
					// The application is already running
					// TODO: Display message box or change focus to existing application instance
				}
			} // releases the Mutex
		}

		static void ExExceptionHandler(object sender, ThreadExceptionEventArgs args)
	    {
      		SysUtils.LogWrite("GK.ExExceptionHandler(): " + args.Exception.Message);
      		SysUtils.LogWrite("GK.ExExceptionHandler(): " + args.Exception.StackTrace.ToString());
	    }

		static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs args) {
			//fixme!!! - saving restore copies
			/*for (int i = mdiChildren.Length - 1; i >= 0; i--)
			{
				if (mdiChildren[i] is TfmBase)
				{
					this.FOptions.AddLastBase((mdiChildren[i] as TfmBase).FileName);
				}
			}*/

			Exception e = (Exception) args.ExceptionObject;
      		SysUtils.LogWrite("GK.UnhandledExceptionsHandler(): " + e.Message);
      		SysUtils.LogWrite("GK.ExExceptionHandler(): " + e.StackTrace.ToString());
		}

	}
}
