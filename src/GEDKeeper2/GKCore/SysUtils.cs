using System;
using System.Runtime.InteropServices;
using System.Security;

namespace GKCore
{
    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    public static class SysUtils
	{
		public const uint WM_USER 			= 0x0400;
		public const uint WM_KEEPMODELESS 	= WM_USER + 111;

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);
    }
}
