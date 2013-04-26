using System;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
{
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
                Win32Native.ShellExecute(0, "open", LogFilename, "", "", 5);
			}
		}


        public static string TrimChars(string s, params char[] trimChars)
        {
            // FIXME: развернуть TrimStart для повышения производительности
            // FIXME: досконально протестировать этот метод
            if (s.Length > 0)
            {
                return s.TrimStart(trimChars);
            }
            else
            {
                return string.Empty;
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
			ScrollEventType result;
			result = ((wParam <= 8u) ? m_ScrollEvents[(int)wParam] : ScrollEventType.EndScroll);
			return result;
		}

        public static ushort GetKeyLayout()
        {
            return unchecked((ushort)Win32Native.GetKeyboardLayout(0u));
        }

        public static void SetKeyLayout(ushort aLayout)
        {
            Win32Native.ActivateKeyboardLayout((uint)aLayout, 0u);
        }

        public static void LoadExtFile([In] string aFileName)
        {
            Win32Native.ShellExecute(0, "open", aFileName, "", "", 5);
        }

		public static int DoScroll(uint handle, uint wParam, int nBar, int aOldPos, int aMin, int aMax, int sm_piece, int big_piece)
		{
			ScrollEventType scrType = SysUtils.GetScrollEventType(wParam & 65535u);
			
			int NewPos = aOldPos;

			switch (scrType) {
				case ScrollEventType.SmallDecrement:
				{
					NewPos -= sm_piece;
					break;
				}

				case ScrollEventType.SmallIncrement:
				{
					NewPos += sm_piece;
					break;
				}

				case ScrollEventType.LargeDecrement:
				{
					NewPos -= big_piece;
					break;
				}

				case ScrollEventType.LargeIncrement:
				{
					NewPos += big_piece;
					break;
				}

				case ScrollEventType.ThumbPosition:
				case ScrollEventType.ThumbTrack:
				{
					Win32Native.TScrollInfo ScrollInfo = new Win32Native.TScrollInfo();
					ScrollInfo.cbSize = (uint)Marshal.SizeOf( ScrollInfo );
					ScrollInfo.fMask = 23u;
                    Win32Native.GetScrollInfo(handle, nBar, ref ScrollInfo);
					NewPos = ScrollInfo.nTrackPos;
					break;
				}

				case ScrollEventType.First:
				{
					NewPos = 0;
					break;
				}

				case ScrollEventType.Last:
				{
					NewPos = aMax;
					break;
				}
			}

			if (NewPos < aMin)
			{
				NewPos = aMin;
			}
			if (NewPos > aMax)
			{
				NewPos = aMax;
			}

			return NewPos;
		}

	}
}
