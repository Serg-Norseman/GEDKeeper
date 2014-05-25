using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Threading;
using System.Windows.Forms;

using ExtUtils;
using GKCore;
using GKUI;

[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009-2014 Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyTitle("GEDKeeper2")]
[assembly: AssemblyVersion("2.1.500.0")]

[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]

[assembly: ComVisible(false)]

/// <summary>
/// Localization: clean
/// </summary>

namespace GK2
{
	internal sealed class Program
	{
		[STAThread]
		[SecurityPermission(SecurityAction.Demand, Flags=SecurityPermissionFlag.ControlAppDomain)]
		private static void Main(string[] args)
		{
			bool isFirstInstance;
			using (Mutex mtx = new Mutex(true, GKData.AppTitle, out isFirstInstance)) {
				if (isFirstInstance) {
					Application.ThreadException += ExExceptionHandler;
					Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException, true);
					AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;

					Application.EnableVisualStyles();
					Application.SetCompatibleTextRenderingDefault(false);
					Application.Run(new TfmGEDKeeper(args));
				} else {
					// The application is already running
					// TODO: Display message box or change focus to existing application instance
				}
			}
		}

		static void ExExceptionHandler(object sender, ThreadExceptionEventArgs args)
	    {
      		SysUtils.LogWrite("GK.ExExceptionHandler(): " + args.Exception.Message);
      		SysUtils.LogWrite("GK.ExExceptionHandler(): " + args.Exception.StackTrace.ToString());
	    }

		static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs args) {
			// saving restore copies
			TfmGEDKeeper.Instance.CriticalSave();

			Exception e = (Exception) args.ExceptionObject;
      		SysUtils.LogWrite("GK.UnhandledExceptionsHandler(): " + e.Message);
      		SysUtils.LogWrite("GK.ExExceptionHandler(): " + e.StackTrace.ToString());
		}

	}
}
