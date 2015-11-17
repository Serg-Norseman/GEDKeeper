using System;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using ExtUtils;

namespace GKCommon
{
    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    public static class NativeMethods
    {
    	public const uint WM_ERASEBKGND		= 0x14;

    	public const uint WM_SIZE			= 0x0005;
    	public const uint WM_GETDLGCODE		= 0x0087;
    	public const uint WM_HSCROLL     	= 0x0114;
    	public const uint WM_VSCROLL     	= 0x0115;
    	
		public const uint WM_USER 			= 0x0400;
		public const uint WM_KEEPMODELESS 	= WM_USER + 111;
		
		public const uint DLGC_WANTARROWS 	= 0x0001;
		public const uint DLGC_WANTTAB 		= 0x0002;
		public const uint DLGC_WANTCHARS 	= 0x0080;
		public const uint DLGC_WANTALLKEYS 	= 0x0004;

		public const int WS_VSCROLL 		= 0x00200000;
		public const int WS_HSCROLL 		= 0x00100000;

		public const int SB_LINELEFT = 0; 
		public const int SB_LINERIGHT = 1; 
		public const int SB_PAGELEFT = 2;
		public const int SB_PAGERIGHT = 3;

		public const int SB_HORZ = 0;
        public const int SB_VERT = 1;
        public const int SB_CTL = 2;
        public const int SB_BOTH = 3;
        
        public const int ESB_ENABLE_BOTH = 0;
		
		[DllImport("user32.dll")]
		public static extern IntPtr SendMessage(IntPtr hWnd, uint wMsg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

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
        public static extern IntPtr ShellExecute(IntPtr hWnd, string lpOperation, string lpFile, string lpParameters, string lpDirectory, ShowCommands nShowCmd);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern IntPtr GetKeyboardLayout(uint dwLayout);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern uint ActivateKeyboardLayout(uint hkl, uint flags);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);

        [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern IntPtr FindWindow(string lpClassName, string lpWindowName);

        [DllImport("hhctrl.ocx", CharSet = CharSet.Unicode, EntryPoint = "HtmlHelpW", SetLastError = true)]
        public static extern uint HtmlHelp(IntPtr hwndCaller, string pszFile, uint uCommand, uint dwData);


        [StructLayout(LayoutKind.Sequential, Pack = 1)]
        public struct ScrollInfo
        {
        	public uint cbSize;
        	public uint fMask;
        	public int nMin;
        	public int nMax;
        	public uint nPage;
        	public int nPos;
        	public int nTrackPos;
        }


        [DllImport("user32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
		public static extern bool EnableScrollBar(IntPtr hWnd, uint wSBflags, uint wArrows);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetScrollInfo(IntPtr hWnd, int barFlag, ref ScrollInfo scrollInfo);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern int ScrollWindowEx(IntPtr hWnd, int dx, int dy, [In] ref ExtRect prcScroll, [In] ref ExtRect prcClip, IntPtr hrgnUpdate, out ExtRect prcUpdate, uint flags);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool SetScrollRange(IntPtr hWnd, int nBar, int nMinPos, int nMaxPos, [MarshalAs(UnmanagedType.Bool)]bool bRedraw);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern int SetScrollPos(IntPtr hWnd, int nBar, int nPos, [MarshalAs(UnmanagedType.Bool)]bool bRedraw);


        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern uint GetPrivateProfileString(string lpAppName, string lpKeyName, string lpDefault, StringBuilder lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern uint GetPrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        public static extern uint GetPrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(string lpAppName, string lpKeyName, string lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(string lpAppName, string lpKeyName, IntPtr lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);


        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public class MapiFileDescriptor
        {
            public int reserved = 0;
            public int flags = 0;
            public int position = 0;
            public string path = null;
            public string name = null;
            public IntPtr type = IntPtr.Zero;
        }

        public const int MAPI_LOGON_UI = 0x1;

        [DllImport("MAPI32.DLL", CharSet = CharSet.Unicode)]
        public static extern int MAPILogon(IntPtr hwnd, string prf, string pw, int flg, int rsv, ref IntPtr sess);

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public class MapiMessage
        {
            public int Reserved = 0;
            public string Subject = null;
            public string NoteText = null;
            public string MessageType = null;
            public string DateReceived = null;
            public string ConversationID = null;
            public int Flags = 0;
            public IntPtr Originator = IntPtr.Zero;
            public int RecipientCount = 0;
            public IntPtr Recipients = IntPtr.Zero;
            public int FileCount = 0;
            public IntPtr Files = IntPtr.Zero;
        }

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        public class MapiRecipDesc
        {
            public int Reserved = 0;
            public int RecipientClass = 0;
            public string Name = null;
            public string Address = null;
            public int eIDSize = 0;
            public IntPtr EntryID = IntPtr.Zero;
        }

        [DllImport("MAPI32.DLL", CharSet = CharSet.Unicode)]
        public static extern uint MAPISendMail(IntPtr lhSession, IntPtr ulUIParam, MapiMessage lpMessage, uint flFlags, uint ulReserved);

        [DllImport("wininet.dll", EntryPoint = "InternetGetConnectedState")]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool InternetGetConnectedState(out int conState, int reder);


        [DllImport("kernel32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool QueryPerformanceFrequency(ref ulong frequencyCount);

        [DllImport("kernel32.dll", SetLastError = true), SuppressUnmanagedCodeSecurity]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool QueryPerformanceCounter(ref ulong performanceCount);

        [DllImport("winmm.dll")]
        public static extern /*ulong*/uint timeGetTime();
    }
}
