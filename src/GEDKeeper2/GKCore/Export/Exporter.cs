using System.Diagnostics;
using System.IO;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class Exporter : BaseObject
	{
		protected GlobalOptions FOptions;
		protected string FPath;
		protected readonly IBase fBase;
		protected GEDCOMTree FTree;

		public GlobalOptions Options
		{
			get { return this.FOptions; }
			set { this.FOptions = value; }
		}

	    protected Exporter(IBase aBase)
		{
	    	this.fBase = aBase;
			this.FTree = aBase.Tree;

			//if (!Directory.Exists(this.FPath)) Directory.CreateDirectory(this.FPath);
		}

		public abstract void Generate(bool show);

		protected void WriteHTMLHeader(StreamWriter stream, string title)
		{
			stream.WriteLine("<html>");
			stream.WriteLine("<head>");
			stream.WriteLine("<meta HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=windows-1251\">");
			stream.WriteLine("<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>");
			stream.WriteLine("<title>" + title + "</title>");
			stream.WriteLine("</head>");
			stream.WriteLine("<body>");
		}

		protected void WriteHTMLFooter(StreamWriter stream)
		{
			stream.WriteLine("</body>");
			stream.WriteLine("</html>");
		}

		protected bool IsRequireFilename(string filter)
		{
			bool result = false;

			using (SaveFileDialog dlg = new SaveFileDialog())
			{
				dlg.Filter = filter;
				result = (dlg.ShowDialog() == DialogResult.OK);

				if (result) this.FPath = dlg.FileName;
			}

			return result;
		}

		protected void ShowResult()
		{
			if (File.Exists(this.FPath)) {
				Process.Start(this.FPath);
			}
		}

		protected static void PrepareSpecIndex(StringList index, string val, GEDCOMIndividualRecord iRec)
		{
			StringList persons;

			int idx = index.IndexOf(val);
			if (idx < 0) {
				persons = new StringList();
				index.AddObject(val, persons);
			} else {
				persons = index.GetObject(idx) as StringList;
			}

			if (persons.IndexOfObject(iRec) < 0) {
				persons.AddObject(iRec.aux_GetNameStr(true, false), iRec);
			}
		}

		protected static void PrepareEventYear(StringList index, GEDCOMCustomEvent evt, GEDCOMIndividualRecord iRec)
		{
			int year = -1;
			if (evt == null)
			{
				year = -1;
			}
			else
			{
				ushort m, d;
				evt.Detail.Date.aux_GetIndependentDate(out year, out m, out d);
				if (year == 0) year = -1;
			}

			string yst = ((year < 0) ? "?" : year.ToString());
			PrepareSpecIndex(index, yst, iRec);
		}

	}
}
