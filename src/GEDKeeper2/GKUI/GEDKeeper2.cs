using System;
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Threading;
using System.Windows.Forms;

using Externals.SingleInstancing;
using GKCommon;
using GKCore;

[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009-2016 Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyTitle("GEDKeeper2")]
[assembly: AssemblyVersion("2.5.0.0")]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: NeutralResourcesLanguage("en")]

namespace GKUI
{
	/// <summary>
	/// 
	/// </summary>
	internal sealed class Program
	{
		[STAThread]
		[SecurityPermission(SecurityAction.Demand, Flags=SecurityPermissionFlag.ControlAppDomain)]
		private static void Main(string[] args)
		{
			Application.ThreadException += ExExceptionHandler;
			Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException, true);
			AppDomain.CurrentDomain.UnhandledException += UnhandledExceptionsHandler;
			Application.EnableVisualStyles();
			Application.SetCompatibleTextRenderingDefault(false);

			#if GK_LINUX
			Application.Run(new TfmGEDKeeper());
			#else
			SingleInstanceTracker tracker = null;
			try
			{
				tracker = new SingleInstanceTracker(GKData.APP_TITLE, GetSingleInstanceEnforcer);

				if (tracker.IsFirstInstance) {
					TfmGEDKeeper fmMain = (TfmGEDKeeper)tracker.Enforcer;
					fmMain.SetArgs(args);
					Application.Run(fmMain);
				} else {
					tracker.SendMessageToFirstInstance(args);
				}
			}
			finally
			{
				if (tracker != null) tracker.Dispose();
			}
			#endif
		}

		private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
		{
			return new TfmGEDKeeper();
		}

		static void ExExceptionHandler(object sender, ThreadExceptionEventArgs args)
		{
			Logger.LogWrite("GK.ExExceptionHandler(): " + args.Exception.Message);
			Logger.LogWrite("GK.ExExceptionHandler(): " + args.Exception.StackTrace);
		}

		static void UnhandledExceptionsHandler(object sender, UnhandledExceptionEventArgs args) {
			// saving restore copies
			TfmGEDKeeper.Instance.CriticalSave();

			Exception e = (Exception) args.ExceptionObject;
			Logger.LogWrite("GK.UnhandledExceptionsHandler(): " + e.Message);
			Logger.LogWrite("GK.ExExceptionHandler(): " + e.StackTrace);
		}
	}
}
