using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace GKCore
{
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
			bool Result;
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
						GKL.LSList[(int)((ushort)i) - 1] = st.Trim();
					}
					Result = true;
				}
				finally
				{
					lng_file.Close();
				}
			}
			else
			{
				Result = false;
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
			StreamWriter lf = new StreamWriter(TGKSys.GetAppPath() + "langs\\russian.sample", false, Encoding.UTF8);
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

		public void Free()
		{
			TObjectHelper.Free(this);
		}

	}
}
