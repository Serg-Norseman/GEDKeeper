using System;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using System.Threading;
using System.Windows.Forms;

using Ext.MapiMail;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
{
	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct TScrollInfo
	{
		public uint cbSize;
		public uint fMask;
		public int nMin;
		public int nMax;
		public uint nPage;
		public int nPos;
		public int nTrackPos;
	}

	public sealed class SysUtils
	{
		public static void Free(object Self)
		{
			if (Self != null && Self is IDisposable)
			{
				((IDisposable)Self).Dispose();
			}
		}

		public static long Trunc([In] double AValue)
		{
			return (long)Math.Truncate(AValue);
		}

		public static int ParseInt([In] string S, [In] int Default)
		{
			int res;
			if (!int.TryParse(S, out res)) res = Default;
			return res;
		}

		public static double ParseFloat([In] string S, [In] double Default)
		{
			if (string.IsNullOrEmpty(S)) return Default;

			NumberFormatInfo LFormat = Thread.CurrentThread.CurrentCulture.NumberFormat.Clone() as NumberFormatInfo;
			LFormat.NumberDecimalSeparator = ".";
			LFormat.NumberGroupSeparator = " ";

			double Value;
			double Result;
			if (double.TryParse(S, NumberStyles.Float, LFormat, out Value)) {
				Result = Value;
			} else {
				Result = Default;
			}
			return Result;
		}

		public static string TrimLeft([In] string S)
		{
			int L = (S != null) ? S.Length : 0;
			int I = 1;
			while (I <= L && S[I - 1] <= ' ') I++;

			string Result;
			if (I > L) {
				Result = "";
			} else {
				Result = ((I != 1) ? S.Substring(I - 1) : S);
			}
			return Result;
		}

		public static string TrimRight([In] string S)
		{
			int L = (S != null) ? S.Length : 0;
			int I = L;
			while (I > 0 && S[I - 1] <= ' ') I--;

			string Result = ((I != L) ? S.Substring(0, I) : S);
			return Result;
		}

		private static string LogFilename;

		public static void LogInit([In] string aFileName)
		{
			LogFilename = aFileName;
		}

		public static void LogWrite([In] string aMsg)
		{
			using (StreamWriter Log = new StreamWriter(LogFilename, true, Encoding.GetEncoding(1251)))
			{
				Log.WriteLine("[" + DateTime.Now.ToString() + "] -> " + aMsg);
				Log.Flush();
				Log.Close();
			}
		}

		public static void LogSend()
		{
			if (File.Exists(LogFilename)) {
				MapiMailMessage message = new MapiMailMessage("GEDKeeper: error notification", "This automatic notification of error.");
				message.Recipients.Add("serg.zhdanovskih@gmail.com");
				message.Files.Add(LogFilename);
				message.ShowDialog();
			}
		}

		public static void LogView()
		{
			if (File.Exists(LogFilename)) {
				SysUtils.ShellExecute(0, "open", LogFilename, "", "", 5);
			}
		}



		private static readonly ScrollEventType[] m_ScrollEvents;

		static SysUtils()
		{
			m_ScrollEvents = new ScrollEventType[]
			{
				ScrollEventType.SmallDecrement, 
				ScrollEventType.SmallIncrement, 
				ScrollEventType.LargeDecrement, 
				ScrollEventType.LargeIncrement, 
				ScrollEventType.ThumbPosition, 
				ScrollEventType.ThumbTrack, 
				ScrollEventType.First, 
				ScrollEventType.Last, 
				ScrollEventType.EndScroll
			};
		}

		public static ScrollEventType GetScrollEventType(uint wParam)
		{
			ScrollEventType Result = ((wParam <= 8u) ? m_ScrollEvents[(int)wParam] : ScrollEventType.EndScroll);
			return Result;
		}



		[SuppressUnmanagedCodeSecurity]
		[DllImport("shell32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		public static extern int ShellExecute(uint hWnd, string Operation, string FileName, string Parameters, string Directory, int ShowCmd);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern uint GetKeyboardLayout(uint dwLayout);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern uint ActivateKeyboardLayout(uint hkl, uint Flags);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		public static extern bool PostMessage(uint hWnd, uint Msg, int wParam, int lParam);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		public static extern bool EnableWindow(uint hWnd, bool bEnable);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", SetLastError = true)]
		public static extern IntPtr FindWindow(string lpClassName, string lpWindowName);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("hhctrl.ocx", CharSet = CharSet.Unicode, EntryPoint = "HtmlHelpW", SetLastError = true)]
		public static extern uint HtmlHelp(IntPtr hwndCaller, string pszFile, uint uCommand, uint dwData);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		public static extern bool GetScrollInfo(uint hWnd, int BarFlag, ref TScrollInfo ScrollInfo);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern int ScrollWindowEx(uint hWnd, int dx, int dy, [In] ref TRect prcScroll, [In] ref TRect prcClip, uint hrgnUpdate, out TRect prcUpdate, uint flags);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern bool SetScrollRange(uint hWnd, int nBar, int nMinPos, int nMaxPos, bool bRedraw);

		[SuppressUnmanagedCodeSecurity]
		[DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
		public static extern int SetScrollPos(uint hWnd, int nBar, int nPos, bool bRedraw);

	}
}
