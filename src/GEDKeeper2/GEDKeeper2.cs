using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using System.Threading;
using System.Windows.Forms;

using ExtUtils.SingleInstancing;
using GKCommon;
using GKCore;
using GKUI;

[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2009-2015 Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: AssemblyTitle("GEDKeeper2")]
[assembly: AssemblyVersion("2.4.0.0")]
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile("")]
[assembly: AssemblyKeyName("")]
[assembly: ComVisible(false)]

namespace GK2
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

			SingleInstanceTracker tracker = null;
			try
			{
				tracker = new SingleInstanceTracker(GKData.AppTitle, new SingleInstanceEnforcerRetriever(GetSingleInstanceEnforcer));

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
		}

		private static ISingleInstanceEnforcer GetSingleInstanceEnforcer()
		{
			return new TfmGEDKeeper();
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
