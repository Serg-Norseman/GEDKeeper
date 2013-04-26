using System;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;


namespace Ext.Utils
{
    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    internal static class Win32Native
    {
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
		
        [DllImport("shell32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern int ShellExecute(uint hWnd, string Operation, string FileName, string Parameters, string Directory, int ShowCmd);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern uint GetKeyboardLayout(uint dwLayout);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern uint ActivateKeyboardLayout(uint hkl, uint Flags);

        [DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(uint hWnd, uint Msg, int wParam, int lParam);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(uint hWnd, bool bEnable);

        [DllImport("user32.dll", SetLastError = true)]
        public static extern IntPtr FindWindow(string lpClassName, string lpWindowName);

        [DllImport("hhctrl.ocx", CharSet = CharSet.Unicode, EntryPoint = "HtmlHelpW", SetLastError = true)]
        public static extern uint HtmlHelp(IntPtr hwndCaller, string pszFile, uint uCommand, uint dwData);


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


        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool GetScrollInfo(uint hWnd, int BarFlag, ref TScrollInfo ScrollInfo);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern int ScrollWindowEx(uint hWnd, int dx, int dy, [In] ref TRect prcScroll, [In] ref TRect prcClip, uint hrgnUpdate, out TRect prcUpdate, uint flags);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern bool SetScrollRange(uint hWnd, int nBar, int nMinPos, int nMaxPos, bool bRedraw);

        [DllImport("user32.dll", CharSet = CharSet.Ansi, SetLastError = true)]
        public static extern int SetScrollPos(uint hWnd, int nBar, int nPos, bool bRedraw);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern uint GetPrivateProfileString(string lpAppName, string lpKeyName, string lpDefault, StringBuilder lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern uint GetPrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        public static extern uint GetPrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpDefault, [Out] byte[] lpReturnedString, uint nSize, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(string lpAppName, string lpKeyName, string lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(string lpAppName, string lpKeyName, IntPtr lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(string lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool WritePrivateProfileString(IntPtr lpAppName, IntPtr lpKeyName, IntPtr lpString, string lpFileName);


        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
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

        [DllImport("MAPI32.DLL", CharSet = CharSet.Ansi)]
        public static extern int MAPILogon(IntPtr hwnd, string prf, string pw, int flg, int rsv, ref IntPtr sess);

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
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

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
        public class MapiRecipDesc
        {
            public int Reserved = 0;
            public int RecipientClass = 0;
            public string Name = null;
            public string Address = null;
            public int eIDSize = 0;
            public IntPtr EntryID = IntPtr.Zero;
        }

        [DllImport("MAPI32.DLL")]
        public static extern int MAPISendMail(IntPtr session, IntPtr hwnd, MapiMessage message, int flg, int rsv);

    }
}
