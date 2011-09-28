using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using GKCore.Sys;

namespace GKCore
{
	[Guid("F7295BF7-EE01-4DD7-8C44-3BDFEAA9E318")]
	public interface ILocalization
	{
		void SetLang();
	}

	public class TLangMan
	{
		public string this[LSID Index]
		{
			get { return TLangMan.GetLStr(Index); }
		}

		private static string GetLStr(LSID Index)
		{
			return GKL.LSList[(int)Index - 1];
		}

		public static bool LoadFromFile([In] string aFileName)
		{
			bool Result = false;
			if (File.Exists(aFileName))
			{
				StreamReader lng_file = new StreamReader(aFileName, Encoding.UTF8);
				try
				{
					lng_file.ReadLine();
					int i = 0;
					while (lng_file.Peek() != -1)
					{
						string st = lng_file.ReadLine();
						i++;
						GKL.LSList[i - 1] = st.Trim();
					}
					Result = true;
				}
				finally
				{
					lng_file.Close();
				}
			}
			return Result;
		}


		public static void DefInit()
		{
			for (LSID id = LSID.LSID_First; id <= LSID.LSID_Last; id++)
			{
				GKL.LSList[(int)id - 1] = GKL.LSDefList[(int)id - 1];
			}
		}


		public static void SaveDefaultLanguage()
		{
			StreamWriter lf = new StreamWriter(SysUtils.GetAppPath() + "langs\\russian.sample", false, Encoding.UTF8);
			try
			{
				lf.WriteLine(";" + 1049.ToString() + "," + "Русский");
				LSID i = LSID.LSID_First;
				do
				{
					lf.WriteLine(GKL.LSDefList[(int)i - 1]);
					i++;
				}
				while (i != (LSID)593);
			}
			finally
			{
				lf.Close();
			}
		}
	}
}
