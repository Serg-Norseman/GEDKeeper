using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Security;

namespace GKCore
{
    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    public static class SysUtils
	{
		public static void LoadExtFile(string fileName)
		{
			if (File.Exists(fileName)) {
				ShellExecute(IntPtr.Zero, "open", fileName, "", "", ShowCommands.SW_SHOW);
			}
		}

        public static ushort GetKeyLayout()
        {
            return unchecked((ushort)GetKeyboardLayout(0u));
        }

        public static void SetKeyLayout(ushort layout)
        {
            ActivateKeyboardLayout(layout, 0u);
        }

        public static bool IsConnectedToInternet()  
        {  
            int iDesc;
            return InternetGetConnectedState(out iDesc, 0);
        }

		public static int DaysBetween(DateTime now, DateTime then)
		{
			TimeSpan span = ((now < then) ? then - now : now - then);
			return span.Days;
		}

		private static readonly ushort[][] MONTH_DAYS = new ushort[][]
		{
			new ushort[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }, 
			new ushort[] { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
		};

		public static ushort DaysInAMonth(ushort year, ushort month)
		{
			return MONTH_DAYS[(month == 2 && DateTime.IsLeapYear(year)) ? 1 : 0][month - 1];
		}

        public static bool IsSetBit(uint val, int pos)
        {
        	return (val & (1 << pos)) != 0;
        }

        #region NativeMethods

		public const uint WM_USER 			= 0x0400;
		public const uint WM_KEEPMODELESS 	= WM_USER + 111;

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);

        [DllImport("hhctrl.ocx", CharSet = CharSet.Unicode, EntryPoint = "HtmlHelpW", SetLastError = true)]
        public static extern uint HtmlHelp(IntPtr hwndCaller, string pszFile, uint uCommand, uint dwData);


        public enum ShowCommands
        {
            SW_HIDE = 0,
            SW_SHOWNORMAL = 1,
            SW_NORMAL = 1,
            SW_SHOWMINIMIZED = 2,
            SW_SHOWMAXIMIZED = 3,
            SW_MAXIMIZE = 3,
            SW_SHOWNOACTIVATE = 4,
            SW_SHOW = 5,
            SW_MINIMIZE = 6,
            SW_SHOWMINNOACTIVE = 7,
            SW_SHOWNA = 8,
            SW_RESTORE = 9,
            SW_SHOWDEFAULT = 10,
            SW_FORCEMINIMIZE = 11,
            SW_MAX = 11
        }

        [DllImport("shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern IntPtr ShellExecute(IntPtr hWnd, string lpOperation, string lpFile, string lpParameters, string lpDirectory, ShowCommands nShowCmd);

        [DllImport("user32.dll", SetLastError = true)]
        private static extern IntPtr GetKeyboardLayout(uint dwLayout);

        [DllImport("user32.dll", SetLastError = true)]
        private static extern uint ActivateKeyboardLayout(uint hkl, uint flags);

        [DllImport("wininet.dll", EntryPoint = "InternetGetConnectedState")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool InternetGetConnectedState(out int conState, int reder);

        #endregion
    }
}
