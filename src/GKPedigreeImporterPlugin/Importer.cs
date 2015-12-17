using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Interfaces;
using Microsoft.Office.Interop.Excel;
using MSOExcel = Microsoft.Office.Interop.Excel;
using MSOWord = Microsoft.Office.Interop.Word;

namespace GKPedigreeImporterPlugin
{
    [Serializable]
    public class ImporterException : Exception
    {
        public ImporterException()
        {
        }

        public ImporterException(string message) : base(message)
        {
        }
    }

    public enum SourceType
    {
    	stText,
    	stTable
    }

    public enum PersonNumbersType
    {
    	pnUndefined,
    	pnDAboville,
    	pnKonovalov
    }

    public enum CellType
    {
    	ct
    }

    public enum NameFormat
    {
    	nfIOF,
    	nfFIO
    }

    public enum GenerationFormat
    {
    	gfRome,
    	gfGenWord
    }

    public enum RawLineType
    {
    	rltComment,
    	rltPerson,
    	rltRomeGeneration,
    	rltEOF
    }

    public class RawLine
    {
    	public int SourceNum;
    	public RawLineType Type;
    	public PersonNumbersType NumbersType;
    	
    	public RawLine(int sourceNum)
    	{
    		this.SourceNum = sourceNum;
    	}
    }

    /// <summary>
    /// Localization: dirty
    /// </summary>
    public class Importer : BaseObject
	{
		private const bool DEBUG_EXCEL = true;
		private const bool DEBUG_WORD = true;

    	private readonly IBaseWindow fBase;
		private readonly GEDCOMTree fTree;
		private readonly System.Windows.Forms.ListBox.ObjectCollection fLog;
		private Dictionary<string, GEDCOMIndividualRecord> fPersonsList;
		private ILangMan fLangMan;
		private string fFileName;

		private StringList fRawContents;

		// settings
		public PersonNumbersType NumbersType;
		public PersonNumbersType CanNumbersType;
		public char PersonLineSeparator;
		public SourceType SourceType;
		public NameFormat NameFormat;
		public GenerationFormat GenerationFormat;
		public bool SurnamesNormalize;

		public StringList RawContents
		{
			get { return this.fRawContents; }
		}

        public Importer(IBaseWindow aBase, ILangMan langMan, System.Windows.Forms.ListBox.ObjectCollection aLog)
        {
        	this.fBase = aBase;
            this.fTree = aBase.Tree;
            this.fLog = aLog;
            this.fLangMan = langMan;

            this.NumbersType = PersonNumbersType.pnKonovalov;
            this.CanNumbersType = PersonNumbersType.pnUndefined;
            this.PersonLineSeparator = (char)0;
            this.SurnamesNormalize = false;
            
            this.fPersonsList = new Dictionary<string, GEDCOMIndividualRecord>();
            this.fRawContents = new StringList();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            	if (this.fRawContents != null) this.fRawContents.Dispose();
                if (this.fPersonsList != null) this.fPersonsList = null;
            }
            base.Dispose(disposing);
        }

		private GEDCOMFamilyRecord AddFamily(GEDCOMIndividualRecord parent)
		{
			GEDCOMFamilyRecord result = this.fTree.CreateFamily();
			result.AddSpouse(parent);
			return result;
		}

        private void AddChild(GEDCOMIndividualRecord parent, int marNum, GEDCOMIndividualRecord child)
		{
        	if (marNum <= 0)
        	{
        		marNum = 1;
        	}

			GEDCOMSex sex = parent.Sex;
			if (sex == GEDCOMSex.svNone || sex == GEDCOMSex.svUndetermined)
			{
				parent.Sex = GEDCOMSex.svMale;
			}

			while (parent.SpouseToFamilyLinks.Count < marNum)
			{
				this.AddFamily(parent);
			}

			GEDCOMFamilyRecord family = parent.SpouseToFamilyLinks[marNum - 1].Family;
			family.AddChild(child);
		}

		private static string RemoveDot(string str)
		{
			if (!string.IsNullOrEmpty(str))
			{
				if (str[str.Length - 1] == '.')
				{
					str = str.Substring(0, str.Length - 1);
				}
			}
			return str.Trim();
		}

		private bool DefinePersonName(string str, out string f_name, out string f_pat, out string f_fam, out string bd, out string dd)
		{
			f_name = "";
			f_pat = "";
			f_fam = "";
			bd = "";
			dd = "";
			string tmp = str;

			int b_pos = tmp.IndexOf("*");
			int d_pos = tmp.IndexOf("+");

			if (d_pos >= 0 && d_pos > b_pos) {
				dd = tmp.Substring(d_pos + 1, tmp.Length - d_pos - 1);
				int num2 = ((dd != null) ? dd.Length : 0) + 1;
				tmp = tmp.Remove(d_pos, num2);
				tmp = tmp.Trim();
			}

			if (b_pos >= 0) {
				bd = tmp.Substring(b_pos + 1, tmp.Length - b_pos - 1);
				int num3 = ((bd != null) ? bd.Length : 0) + 1;
				tmp = tmp.Remove(b_pos, num3);
				tmp = tmp.Trim();
			}

			string[] tokens = tmp.Trim().Split(new char[] { ' ' }, 3);

			switch (this.NameFormat) {
				case NameFormat.nfIOF:
					if (tokens.Length > 0) f_name = RemoveDot(tokens[0]);
					if (tokens.Length > 1) f_pat = RemoveDot(tokens[1]);
					if (tokens.Length > 2) f_fam = RemoveDot(tokens[2]);
					break;

				case NameFormat.nfFIO:
					if (tokens.Length > 0) f_fam = RemoveDot(tokens[0]);
					if (tokens.Length > 1) f_name = RemoveDot(tokens[1]);
					if (tokens.Length > 2) f_pat = RemoveDot(tokens[2]);
					break;
			}

			if (this.SurnamesNormalize) {
				f_fam = GEDCOMUtils.NormalizeName(f_fam);
			}

			bd = RemoveDot(bd);
			dd = RemoveDot(dd);

			return true;
		}

		private bool IsPersonLine(string str, ref string p_id)
		{
			switch (this.NumbersType) {
				case PersonNumbersType.pnDAboville:
					return ImpUtils.IsPersonLine_DAboville(str, ref p_id);

				case PersonNumbersType.pnKonovalov:
					return ImpUtils.IsPersonLine_Konovalov(str, ref p_id);

				default:
					return false;
			}
		}

		private bool ParsePersonLine(string str, out string persId, out string parentId, out string marNum, 
		                             out string extData, out int pos)
		{
			switch (this.NumbersType) {
				case PersonNumbersType.pnDAboville:
					return ImpUtils.ParsePersonLine_DAboville(str, out persId, out parentId, out marNum, out extData, out pos);

				case PersonNumbersType.pnKonovalov:
					return ImpUtils.ParsePersonLine_Konovalov(str, out persId, out parentId, out marNum, out extData, out pos);

				default:
					persId = "";
					parentId = "";
					marNum = "";
					extData = "";
					pos = 0;
					return false;
			}
		}

		private void SetEvent(GEDCOMRecordWithEvents record, string evName, string date)
		{
			int[] val = new int[3];
			GEDCOMCustomEvent evt = this.fBase.Context.CreateEventEx(record, evName, "", "");
			try
			{
				string prefix = "";
				if (date.IndexOf("п.") == 0)
				{
					prefix = "AFT ";
					date = date.Remove(0, 2).Trim();
				}
				else
				{
					if (date.IndexOf("до") == 0)
					{
						prefix = "BEF ";
						date = date.Remove(0, 3).Trim();
					}
				}

				string tmp = "";
				string[] toks = date.Split('.');
				if (toks.Length > 3)
				{
                    throw new ImporterException("date failed");
				}
				string ym = "";

				for (int i = 0; i < toks.Length; i++)
				{
					tmp = toks[i];

					int x = tmp.IndexOf("/");
					if (x >= 0)
					{
						ym = tmp.Substring(x + 1, tmp.Length - x - 1);
						tmp = tmp.Remove(x, ym.Length + 1);
					}

					val[i] = int.Parse(tmp);
				}

				if (toks.Length != 1)
				{
					if (toks.Length != 2)
					{
						if (toks.Length == 3)
						{
                            tmp = val[0].ToString() + " " + GEDCOMCustomDate.GEDCOMMonthArray[val[1] - 1] + " " + val[2].ToString();
						}
					}
					else
					{
						tmp = GEDCOMCustomDate.GEDCOMMonthArray[val[0] - 1] + " " + val[1].ToString();
					}
				}
				else
				{
					tmp = val[0].ToString();
				}

				tmp = prefix + tmp;
				if (ym != "")
				{
					tmp = tmp + "/" + ym;
				}

				evt.Detail.Date.ParseString(tmp);
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_DateInvalid) + " \"" + date + "\"");
			}
		}

		private GEDCOMIndividualRecord DefinePerson(string str, GEDCOMSex proposeSex)
		{
			GEDCOMIndividualRecord result;

			string iName, iPatr, iSurname, bd, dd;
			this.DefinePersonName(str, out iName, out iPatr, out iSurname, out bd, out dd);

			result = this.fBase.Context.CreatePersonEx(iName, iPatr, iSurname, proposeSex, false);

			if (proposeSex == GEDCOMSex.svNone || proposeSex == GEDCOMSex.svUndetermined) {
				this.fBase.CheckPersonSex(result);
			}

			if (bd != "") this.SetEvent(result, "BIRT", bd);
			if (dd != "") this.SetEvent(result, "DEAT", dd);

			return result;
		}

		private GEDCOMIndividualRecord ParsePerson(StringList buf, string str, string p_id, ref int self_id)
		{
			try
			{
				self_id = -1;
				int mar_id = -1;
				int pid_end = 0;

				string persId, parentId, marNum, extData;
				bool res = this.ParsePersonLine(str, out persId, out parentId, out marNum, out extData, out pid_end);
				// extData - (в/б)

				if (this.NumbersType == PersonNumbersType.pnKonovalov) {
					self_id = int.Parse(persId);
					int.TryParse(marNum, out mar_id);
				}

				str = str.Substring(pid_end).Trim();

				GEDCOMSex proposeSex = this.GetProposeSex(buf);

				GEDCOMIndividualRecord result = this.DefinePerson(str, proposeSex);

				this.fPersonsList.Add(persId, result);

				if (buf != null) {
					buf.Add(str);
				}

				if (!string.IsNullOrEmpty(parentId))
				{
					GEDCOMIndividualRecord parent;
					if (this.fPersonsList.TryGetValue(parentId, out parent)) {
						this.AddChild(parent, mar_id, result);
					} else {
						this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_AncNotFound) + " \"" + parentId + "\".");
					}
				}

				return result;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("Importer.ParsePerson(): " + ex.Message);
				throw ex;
			}
		}

		private GEDCOMSex GetProposeSex(StringList buffer)
		{
			GEDCOMSex result = GEDCOMSex.svNone;

			try
			{
				int num = buffer.Count;
				for (int i = 0; i < num; i++)
				{
					string line = buffer[i];

					if (line.Length > 2) {
						char c1 = line[0];
						char c2 = line[1];
						if ((c1 == 'М' || c1 == 'Ж') && ((c2 == ' ') || (c2 >= '1' && c2 <= '9'))) {
							// define sex (if spouse is male, then result = female, else result = male)
							GEDCOMSex res = (c1 == 'М') ? GEDCOMSex.svFemale : GEDCOMSex.svMale;

							if (result == GEDCOMSex.svNone) {
								result = res;
							} else {
								if (result != res) {
									this.fLog.Add(">>>> Противоречивая информация о супругах");
									return GEDCOMSex.svNone;
								} else {
									// matched, checked
								}
							}
						}
					}
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("Importer.GetProposeSex(): " + ex.Message);
			}

			return result;
		}

		private void CheckSpouses(StringList buffer, GEDCOMIndividualRecord curPerson)
		{
			int num2 = buffer.Count;
			for (int i = 0; i < num2; i++)
			{
				string line = buffer[i];
				if (string.IsNullOrEmpty(line)) continue;

				try
				{
					string spouse, marNum, extData;
					int pos;
					if (ImpUtils.ParseSpouseLine(line, out spouse, out marNum, out extData, out pos))
					{
						// define sex
						GEDCOMSex sx = (spouse[0] == 'М') ? GEDCOMSex.svMale : GEDCOMSex.svFemale;

						// number of spouse, not used
						int num = int.Parse(marNum);

						// extract name
						line = line.Substring(pos).Trim();

						if (!string.IsNullOrEmpty(line))
						{
							GEDCOMFamilyRecord fam = this.AddFamily(curPerson);
							GEDCOMIndividualRecord sp = this.DefinePerson(line, sx);
							fam.AddSpouse(sp);

							// extract marriage date
							if (!string.IsNullOrEmpty(extData)) {
								string mar_date = extData.Substring(1, extData.Length - 2).Trim();

								if (mar_date != "") this.SetEvent(fam, "MARR", mar_date);
							}
						}
					}
				}
				catch (Exception ex)
				{
					this.fBase.Host.LogWrite("Importer.CheckSpouses(): " + ex.Message);
				}
			}
		}

		private void CheckBuffer(StringList buffer, GEDCOMIndividualRecord curPerson)
		{
			if (!buffer.IsEmpty())
			{
				if (curPerson != null)
				{
					this.CheckSpouses(buffer, curPerson);
				}

				this.fTree.CreateNoteEx(curPerson, buffer);

				buffer.Clear();
			}
		}

		private void ParseBuffer(StringList buffer, ref int prev_id)
		{
			try
			{
				if (buffer.IsEmpty()) {
					return;
				}

				string p_id = "";
				string s = buffer[0];
				if (this.IsPersonLine(s, ref p_id))
				{
					this.fLog.Add("> " + fLangMan.LS(ILS.LSID_PersonParsed) + " \"" + p_id + "\"");

					int self_id = 0;
					GEDCOMIndividualRecord curPerson = this.ParsePerson(null, s, p_id, ref self_id);

					if (this.NumbersType == PersonNumbersType.pnKonovalov && self_id - prev_id > 1)
					{
						this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_LineSeq));
					}

					prev_id = self_id;

					this.CheckBuffer(buffer, curPerson);
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("Importer.ParseBuffer(): " + ex.Message);
				throw ex;
			}
		}

		private bool IsGenerationLine(string str)
		{
			switch (this.GenerationFormat)
			{
				case GenerationFormat.gfRome:
					return ImpUtils.IsRomeLine(str);

				case GenerationFormat.gfGenWord:
					return str.StartsWith("Поколение ", StringComparison.InvariantCultureIgnoreCase);

				default:
					return false;
			}
		}

		private string PrepareLine(string line)
		{
			string result;
			result = line.Replace('–', '-').Trim();
			return result;
		}

		#region Integral loading

		private void AnalyseRaw()
		{
			if (this.SourceType == SourceType.stTable) {
				return;
			}

			try
			{
				int[] numberStats = new int[3];

				int num = this.fRawContents.Count;
				this.fBase.ProgressInit("Анализ", num);

				for (int i = 0; i < num; i++) {
					string txt = this.fRawContents[i].Trim();
					RawLine rawLine = this.fRawContents.GetObject(i) as RawLine;

					if (!string.IsNullOrEmpty(txt)) {
						if (this.IsGenerationLine(txt)) {
							rawLine.Type = RawLineType.rltRomeGeneration;
						} else {
							PersonNumbersType numbType = PersonNumbersType.pnUndefined;
							string dummy = "";
							
							if (ImpUtils.IsPersonLine_DAboville(txt, ref dummy)) {
								rawLine.Type = RawLineType.rltPerson;
								numbType = PersonNumbersType.pnDAboville;
								numberStats[1]++;
							} else if (ImpUtils.IsPersonLine_Konovalov(txt, ref dummy)) {
								rawLine.Type = RawLineType.rltPerson;
								numbType = PersonNumbersType.pnKonovalov;
								numberStats[2]++;
							}

							rawLine.NumbersType = numbType;
						}
					} else {
						rawLine.Type = RawLineType.rltEOF;
					}

					this.fBase.ProgressStep(i + 1);
				}

				if (numberStats[1] > numberStats[2]) {
					this.CanNumbersType = PersonNumbersType.pnDAboville;
				} else {
					this.CanNumbersType = PersonNumbersType.pnKonovalov;
				}
			}
			finally
			{
				this.fBase.ProgressDone();
			}
		}

		public bool ImportContent()
		{
			this.AnalyseRaw();

			switch (this.SourceType)
			{
				case SourceType.stText:
					return this.ImportTextContent();

				case SourceType.stTable:
					return this.ImportTableContent();
			
				default:
					return false;
			}
		}

		private bool ImportTextContent()
		{
			try
			{
				this.fLog.Clear();

				StringList buffer = new StringList();
				try
				{
					int prev_id = 0;

					int num = this.fRawContents.Count;
					for (int i = 0; i < num; i++)
					{
						string line = this.PrepareLine(this.fRawContents[i]);
						RawLine rawLine = this.fRawContents.GetObject(i) as RawLine;

						switch (rawLine.Type) {
							case RawLineType.rltComment:
								buffer.Add(line);
								break;

							case RawLineType.rltPerson:
							case RawLineType.rltRomeGeneration:
							case RawLineType.rltEOF:
								{
									this.ParseBuffer(buffer, ref prev_id);
									buffer.Clear();

									switch (rawLine.Type) {
										case RawLineType.rltPerson:
											buffer.Add(line);
											break;
										case RawLineType.rltRomeGeneration:
											this.fLog.Add("> " + fLangMan.LS(ILS.LSID_Generation) + " \"" + line + "\"");
											break;
										case RawLineType.rltEOF:
											this.fLog.Add("> EOF.");
											break;
									}
								}
								break;
						}
					}

					return true;
				}
				finally
				{
					buffer.Dispose();
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("Importer.ImportTextContent(): " + ex.Message);
				throw ex;
			}
		}

		private static string GetCell(object[,] values, int row, int col)
		{
			object obj = values[row, col];
			if (obj == null) {
				return "";
			}/* else if (obj.GetType() is string) {
				return (string)obj;
			}*/ else {
				return obj.ToString();
			}
		}

		private bool ImportTableContent()
		{
			try
			{
				fLog.Clear();

				MSOExcel.Application excel;
				try
				{
					excel = new MSOExcel.Application();
				}
				catch (Exception ex)
				{
					return false;
				}

				excel.Visible = DEBUG_EXCEL;
				excel.DisplayAlerts = false;
				excel.WindowState = MSOExcel.XlWindowState.xlMaximized;
				excel.Workbooks.Open(this.fFileName);
				MSOExcel.Worksheet sheet = excel.Worksheets[1] as MSOExcel.Worksheet;
				//sheet.Activate();

				StringList buf = new StringList();
				try
				{
					// получаем используемое количество строк и столбцов
					int rows_count = sheet.UsedRange.Rows.Count;
					int cols_count = sheet.UsedRange.Columns.Count;

					this.fBase.ProgressInit("Загрузка", rows_count);

					MSOExcel.Range excelRange = sheet.UsedRange;
					object[,] valueArray = (object[,])excelRange.get_Value(XlRangeValueDataType.xlRangeValueDefault);
					
					int prev_id = 0;
					GEDCOMIndividualRecord i_rec = null;

					for (int row = 1; row <= rows_count; row++)
					{
						string c1 = GetCell(valueArray, row, 1).Trim(); // номер позиции
						string c2 = GetCell(valueArray, row, 2).Trim(); // номер предка
						string c3 = GetCell(valueArray, row, 3).Trim(); // имя, может начинаться с номера брака
						string c4 = GetCell(valueArray, row, 4).Trim(); // дата рождения
						string c5 = GetCell(valueArray, row, 5).Trim(); // дата смерти
						string c6 = GetCell(valueArray, row, 6).Trim(); // место рождения или проживания

						string s123 = c1 + c2;
						if (s123 != "" && !string.IsNullOrEmpty(c3) && c3[0] != '/') {
							s123 += ". " + c3;
						} else {
							s123 += c3;
						}

						string s, p_id = "";
						int self_id = 0;

						if (s123 == "") {
							CheckBuffer(buf, i_rec);
							i_rec = null;
						} else {
							if (this.IsGenerationLine(s123)) {
								CheckBuffer(buf, i_rec);
								i_rec = null;

								fLog.Add("> " + fLangMan.LS(ILS.LSID_Generation) + " " + s123);
							} else {
								s = s123 + " " + c4 + " " + c5;
								if (c6 != "") {
									s = s + ". " + c6 + ".";
								}

								s = s.Trim();

								if (!IsPersonLine(s, ref p_id)) {
									buf.Add(s);
								} else {
									CheckBuffer(buf, i_rec);
									i_rec = ParsePerson(buf, s, p_id, ref self_id);

									fLog.Add("> "+fLangMan.LS(ILS.LSID_PersonParsed) + " " + p_id + ".");

									if (self_id - prev_id > 1) {
										fLog.Add(">>>> "+fLangMan.LS(ILS.LSID_ParseError_LineSeq));
									}

									prev_id = self_id;
								}
							}
						}

						this.fBase.ProgressStep(row);
					}

					return true;
				}
				finally
				{
					this.fBase.ProgressDone();

					buf.Dispose();

					excel.Quit();
					excel = null;
				}
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
				this.fBase.Host.LogWrite("Importer.ImportExcel(): " + ex.Message);
				return false;
			}
		}

		private bool LoadRawExcel(string fileName)
		{
			this.SourceType = SourceType.stTable;

			this.AnalyseRaw();

			return true;
		}

		private bool LoadRawText(string fileName)
		{
			this.SourceType = SourceType.stText;

			try
			{
				StreamReader strd = new StreamReader(fileName, Encoding.GetEncoding(1251));
				try
				{
					this.fBase.ProgressInit("Загрузка", (int)strd.BaseStream.Length);

					int lineNum = 0;
					while (strd.Peek() != -1) {
						string txt = strd.ReadLine().Trim();

						if (!string.IsNullOrEmpty(txt)) {
							this.fRawContents.AddObject(txt, new RawLine(lineNum));
						}

						this.fBase.ProgressStep((int)strd.BaseStream.Position);
						lineNum++;
					}
					this.fRawContents.AddObject("", new RawLine(lineNum));

					this.AnalyseRaw();

					return true;
				}
				finally
				{
					this.fBase.ProgressDone();
					strd.Close();
				}
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
				this.fBase.Host.LogWrite("Importer.ImportPlainText(): " + ex.Message);
				return false;
			}
		}

		private bool LoadRawWord(string fileName)
		{
			this.SourceType = SourceType.stText;

			try
			{
				MSOWord.Application wordApp;
				try
				{
					wordApp = new MSOWord.Application();
				}
				catch
				{
					return false;
				}

				try
				{
					wordApp.Visible = DEBUG_WORD;
					wordApp.WindowState = MSOWord.WdWindowState.wdWindowStateMaximize;

					MSOWord.Document doc = wordApp.Documents.Open(fileName);

					this.fBase.ProgressInit("Загрузка", doc.Paragraphs.Count);

					int lineNum = 0;
					for (int i = 0; i < doc.Paragraphs.Count; i++)
					{
						string txt = doc.Paragraphs[i+1].Range.Text;
						txt = txt.Trim();

						if (!string.IsNullOrEmpty(txt)) {
							this.fRawContents.AddObject(txt, new RawLine(lineNum));
						}

						this.fBase.ProgressStep(i + 1);
						lineNum++;
					}					
					this.fRawContents.AddObject("", new RawLine(lineNum));

					this.AnalyseRaw();

					return true;
				}
				finally
				{
					this.fBase.ProgressDone();
					wordApp.Quit();
					wordApp = null;
				}
			}
			catch (Exception ex)
			{
				this.fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
				this.fBase.Host.LogWrite("Importer.ImportWord(): " + ex.Message);
				return false;
			}
		}

		public bool LoadRawData(string fileName)
		{
			this.fRawContents.Clear();

			this.fFileName = fileName;
			string ext = Path.GetExtension(fileName).ToLower();

			if (ext == ".txt")
			{
				return this.LoadRawText(fileName);
			}
			else if (ext == ".doc")
			{
				return this.LoadRawWord(fileName);
			}
			else if (ext == ".xls")
			{
				return this.LoadRawExcel(fileName);
			}
			else
			{
				throw new ImporterException(fLangMan.LS(ILS.LSID_FormatUnsupported));
			}
		}

		#endregion
	}
}
