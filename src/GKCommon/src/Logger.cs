using System;
using System.IO;
using System.Text;

namespace GKCommon
{
	public static class Logger
	{
		private static string LogFilename;

		public static void LogInit(string fileName)
		{
			LogFilename = fileName;
		}

		public static void LogWrite(string msg)
		{
			using (StreamWriter Log = new StreamWriter(LogFilename, true, Encoding.GetEncoding(1251)))
			{
				Log.WriteLine("[" + DateTime.Now.ToString() + "] -> " + msg);
				Log.Flush();
				Log.Close();
			}
		}
	}
}
