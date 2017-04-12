/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#if __MonoCS__
#define NO_DEPEND
#endif

#if CI_MODE
#define NO_DEPEND
#endif

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKPedigreeImporterPlugin
{
    #if !NO_DEPEND
    using MSOExcel = Microsoft.Office.Interop.Excel;
    using MSOWord = Microsoft.Office.Interop.Word;
    #endif

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

    public sealed class RawLine
    {
        public int SourceNum;
        public RawLineType Type;
        public PersonNumbersType NumbersType;

        public RawLine(int sourceNum)
        {
            SourceNum = sourceNum;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class Importer : BaseObject
    {
        private const bool DEBUG_EXCEL = false;
        private const bool DEBUG_WORD = false;

        private readonly IBaseWindow fBase;
        private readonly ILangMan fLangMan;
        private readonly System.Windows.Forms.ListBox.ObjectCollection fLog;
        private readonly StringList fRawContents;
        private readonly GEDCOMTree fTree;

        private Dictionary<string, GEDCOMIndividualRecord> fPersonsList;
        private string fFileName;

        // settings
        public PersonNumbersType NumbersType;
        public PersonNumbersType CanNumbersType;
        public char PersonLineSeparator;
        public SourceType SourceType;
        public NameFormat NameFormat;
        public GenerationFormat GenerationFormat;
        public bool SurnamesNormalize;
        public DateFormat DateFormat;
        public char DateSeparator;

        public bool SpecialFormat_1;

        public StringList RawContents
        {
            get { return fRawContents; }
        }

        public Importer(IBaseWindow baseWin, ILangMan langMan, System.Windows.Forms.ListBox.ObjectCollection aLog)
        {
            fBase = baseWin;
            fTree = baseWin.Tree;
            fLog = aLog;
            fLangMan = langMan;

            NumbersType = PersonNumbersType.pnKonovalov;
            CanNumbersType = PersonNumbersType.pnUndefined;
            PersonLineSeparator = (char)0;
            SurnamesNormalize = false;

            fPersonsList = new Dictionary<string, GEDCOMIndividualRecord>();
            fRawContents = new StringList();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fRawContents != null) fRawContents.Dispose();
                fPersonsList = null;
            }
            base.Dispose(disposing);
        }

        private GEDCOMFamilyRecord GetFamilyByNum(GEDCOMIndividualRecord parent, int marrNum)
        {
            // it's source of ERRORS! but without this - bad! (AddSpouse() not linking parent to family)
            GEDCOMSex sex = parent.Sex;
            if (sex == GEDCOMSex.svNone || sex == GEDCOMSex.svUndetermined)
            {
                parent.Sex = GEDCOMSex.svMale;
            }

            while (parent.SpouseToFamilyLinks.Count < marrNum)
            {
                GEDCOMFamilyRecord fam = fTree.CreateFamily();
                fam.AddSpouse(parent);
            }

            GEDCOMFamilyRecord family = parent.SpouseToFamilyLinks[marrNum - 1].Family;
            return family;
        }

        private void AddChild(GEDCOMIndividualRecord parent, int marrNum, GEDCOMIndividualRecord child)
        {
            if (marrNum <= 0) {
                marrNum = 1;
            }

            GEDCOMFamilyRecord family = GetFamilyByNum(parent, marrNum);
            if (family != null) {
                family.AddChild(child);
            } else {
                // ???
            }
        }

        private static string RemoveDot(string str)
        {
            if (!string.IsNullOrEmpty(str))
            {
                if (str[str.Length - 1] == '.')
                {
                    str = str.Substring(0, str.Length - 1);
                }
                str = str.Trim();
            }
            return str;
        }

        private static string RemoveCommaDot(string str)
        {
            if (!string.IsNullOrEmpty(str))
            {
                char last = str[str.Length - 1];
                if (last == ',' || last == '.')
                {
                    str = str.Substring(0, str.Length - 1);
                }
                str = str.Trim();
            }
            return str;
        }

        private static void ParseDatesLine(string tmp, out string bd, out string dd)
        {
            bd = "";
            dd = "";

            int b_pos = tmp.IndexOf(ImportUtils.STD_BIRTH_SIGN);
            int d_pos = tmp.IndexOf(ImportUtils.STD_DEATH_SIGN);

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

            bd = RemoveDot(bd);
            dd = RemoveDot(dd);
        }
        
        private void DefinePersonName(string str, out string f_name, out string f_pat, out string f_fam, out string bd, out string dd)
        {
            f_name = "";
            f_pat = "";
            f_fam = "";

            string tmp = str;

            string dates = "";
            if (SpecialFormat_1) {
                int ob_pos = tmp.IndexOf("(*");
                if (ob_pos >= 0) {
                    int cb_pos = tmp.IndexOf(")", ob_pos);
                    if (ob_pos >= 0 && cb_pos > ob_pos) {
                        dates = tmp.Substring(ob_pos + 1, cb_pos - ob_pos - 1).Trim();
                        tmp = tmp.Remove(ob_pos, dates.Length + 2);
                    }
                }
            }

            // if not Special or SpecialNotFound, then classic
            if (string.IsNullOrEmpty(dates))
            {
                int bd_pos = tmp.IndexOf(ImportUtils.STD_BIRTH_SIGN);
                int dd_pos = tmp.IndexOf(ImportUtils.STD_DEATH_SIGN);
                
                int datesPos = -1;
                if (bd_pos >= 0 && (dd_pos < 0 || dd_pos > bd_pos)) {
                    datesPos = bd_pos;
                } else {
                    datesPos = dd_pos;
                }
                
                if (datesPos >= 0) {
                    dates = tmp.Substring(datesPos, tmp.Length - datesPos);
                    tmp = tmp.Remove(datesPos, dates.Length).Trim(); // can be blanks at end
                }
            }

            ParseDatesLine(dates, out bd, out dd);

            tmp = RemoveCommaDot(tmp); // &Trim()

            string[] tokens = tmp.Split(new char[] { ' ' }, 3);

            switch (NameFormat) {
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

            if (SurnamesNormalize) {
                f_fam = SysUtils.NormalizeName(f_fam);
            }

            //return true;
        }

        private bool IsPersonLine(string str, ref string p_id)
        {
            switch (NumbersType) {
                case PersonNumbersType.pnDAboville:
                    return ImportUtils.IsPersonLine_DAboville(str, out p_id);

                case PersonNumbersType.pnKonovalov:
                    return ImportUtils.IsPersonLine_Konovalov(str, out p_id);

                default:
                    return false;
            }
        }

        private bool ParsePersonLine(string str, out string persId, out string parentId, out string marNum,
                                     out string extData, out int pos)
        {
            switch (NumbersType) {
                case PersonNumbersType.pnDAboville:
                    return ImportUtils.ParsePersonLine_DAboville(str, out persId, out parentId, out marNum, out extData, out pos);

                case PersonNumbersType.pnKonovalov:
                    return ImportUtils.ParsePersonLine_Konovalov(str, out persId, out parentId, out marNum, out extData, out pos);

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
            GEDCOMCustomEvent evt = fBase.Context.CreateEventEx(record, evName, "", "");
            try
            {
                string prefix = "";
                if (date.IndexOf("п.") == 0)
                {
                    prefix = "AFT ";
                    date = date.Remove(0, 2);
                }
                else if (date.IndexOf("после") == 0)
                {
                    prefix = "AFT ";
                    date = date.Remove(0, 5);
                }
                else if (date.IndexOf("до") == 0)
                {
                    prefix = "BEF ";
                    date = date.Remove(0, 2);
                }
                else if (date.IndexOf("ок.") == 0)
                {
                    prefix = "ABT ";
                    date = date.Remove(0, 3);
                }
                else if (date.IndexOf("около") == 0)
                {
                    prefix = "ABT ";
                    date = date.Remove(0, 5);
                }

                date = date.Trim();
                
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

                evt.Date.ParseString(tmp);
            }
            catch (Exception)
            {
                fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_DateInvalid) + " \"" + date + "\"");
            }
        }

        private GEDCOMIndividualRecord DefinePerson(string str, GEDCOMSex proposeSex)
        {
            string iName, iPatr, iSurname, bd, dd;
            DefinePersonName(str, out iName, out iPatr, out iSurname, out bd, out dd);

            GEDCOMIndividualRecord result = fBase.Context.CreatePersonEx(iName, iPatr, iSurname, proposeSex, false);

            if (proposeSex == GEDCOMSex.svNone || proposeSex == GEDCOMSex.svUndetermined) {
                AppHub.BaseController.CheckPersonSex(fBase.Context, result);
            }

            if (bd != "") SetEvent(result, "BIRT", bd);
            if (dd != "") SetEvent(result, "DEAT", dd);

            return result;
        }

        private GEDCOMIndividualRecord ParsePerson(StringList buffer, string str, ref int selfId)
        {
            try
            {
                selfId = -1;
                int marrNum = -1;
                int pid_end = 0;

                string persId, parentId, marNum, extData;
                bool res = ParsePersonLine(str, out persId, out parentId, out marNum, out extData, out pid_end);
                // extData - (в/б)

                if (!res) {
                    return null;
                }

                if (fPersonsList.ContainsKey(persId)) {
                    fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_NumDuplicate) + " \"" + persId + "\".");
                    return null;
                }

                if (NumbersType == PersonNumbersType.pnKonovalov) {
                    selfId = int.Parse(persId);
                    int.TryParse(marNum, out marrNum);
                }

                str = str.Substring(pid_end).Trim();

                GEDCOMSex proposeSex = GetProposeSex(buffer);

                GEDCOMIndividualRecord result = DefinePerson(str, proposeSex);

                fPersonsList.Add(persId, result);

                if (!string.IsNullOrEmpty(parentId))
                {
                    GEDCOMIndividualRecord parent;
                    if (fPersonsList.TryGetValue(parentId, out parent)) {
                        AddChild(parent, marrNum, result);
                    } else {
                        fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_AncNotFound) + " \"" + parentId + "\".");
                    }
                }

                return result;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("Importer.ParsePerson(): " + ex.Message);
                throw;
            }
        }

        private GEDCOMSex GetProposeSex(StringList buffer)
        {
            GEDCOMSex result = GEDCOMSex.svNone;
            if (buffer == null) return result;

            try
            {
                int num = buffer.Count;
                for (int i = 0; i < num; i++)
                {
                    string line = buffer[i];
                    if (line.Length <= 2) continue;

                    char c1 = line[0];
                    char c2 = line[1];
                    if ((c1 == 'М' || c1 == 'Ж') && ((c2 == ' ') || (c2 >= '1' && c2 <= '9'))) {
                        // define sex (if spouse is male, then result = female, else result = male)
                        GEDCOMSex res = (c1 == 'М') ? GEDCOMSex.svFemale : GEDCOMSex.svMale;

                        if (result == GEDCOMSex.svNone) {
                            result = res;
                        } else {
                            if (result != res) {
                                fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_SpousesInfoConflict));
                                return GEDCOMSex.svNone;
                            } else {
                                // matched, checked
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("Importer.GetProposeSex(): " + ex.Message);
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
                    string spSex, extData;
                    int marrNum;
                    int pos;
                    if (ImportUtils.ParseSpouseLine(line, out spSex, out marrNum, out extData, out pos))
                    {
                        // define sex
                        GEDCOMSex sx = (spSex[0] == 'М') ? GEDCOMSex.svMale : GEDCOMSex.svFemale;

                        // extract name
                        line = line.Substring(pos).Trim();

                        if (!string.IsNullOrEmpty(line))
                        {
                            GEDCOMIndividualRecord spouse = DefinePerson(line, sx);

                            GEDCOMFamilyRecord family = GetFamilyByNum(curPerson, marrNum);
                            family.AddSpouse(spouse);

                            // extract marriage date
                            if (!string.IsNullOrEmpty(extData)) {
                                string marrDate = extData.Substring(1, extData.Length - 2).Trim();

                                if (marrDate != "") SetEvent(family, "MARR", marrDate);
                            }
                        }
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogWrite("Importer.CheckSpouses(): " + ex.Message);
                }
            }
        }

        private void CheckBuffer(StringList buffer, GEDCOMIndividualRecord curPerson)
        {
            if (buffer.IsEmpty()) return;

            if (curPerson != null)
            {
                CheckSpouses(buffer, curPerson);
            }

            GEDCOMNoteRecord noteRec = fTree.CreateNote();
            noteRec.Note = buffer;
            if (curPerson != null) curPerson.AddNote(noteRec);

            buffer.Clear();
        }

        private void ParseBuffer(StringList buffer, ref int prevId)
        {
            try
            {
                if (buffer.IsEmpty()) {
                    return;
                }

                string personId = "";
                string s = buffer[0];
                if (IsPersonLine(s, ref personId))
                {
                    fLog.Add("> " + fLangMan.LS(ILS.LSID_PersonParsed) + " \"" + personId + "\"");

                    int selfId = 0;
                    GEDCOMIndividualRecord curPerson = ParsePerson(buffer, s, ref selfId);

                    if (NumbersType == PersonNumbersType.pnKonovalov && selfId - prevId > 1)
                    {
                        fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_ParseError_LineSeq));
                    }

                    prevId = selfId;

                    CheckBuffer(buffer, curPerson);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("Importer.ParseBuffer(): " + ex.Message);
                throw;
            }
        }

        private bool IsGenerationLine(string str)
        {
            switch (GenerationFormat)
            {
                case GenerationFormat.gfRome:
                    return ImportUtils.IsRomeLine(str);

                case GenerationFormat.gfGenWord:
                    return str.StartsWith("Поколение ", StringComparison.InvariantCultureIgnoreCase);

                default:
                    return false;
            }
        }

        private static string PrepareLine(string line)
        {
            string result = line.Replace('–', '-');
            result = result.Replace('', '+'); // some formats of the death date prefix

            return result.Trim();
        }

        #region Integral loading

        private bool AnalyseRaw()
        {
            if (SourceType == SourceType.stTable) {
                return false;
            }

            try
            {
                IProgressController progress = AppHub.Progress;

                try
                {
                    int[] numberStats = new int[3];

                    int num = fRawContents.Count;
                    progress.ProgressInit(fLangMan.LS(ILS.LSID_Analysis), num);

                    for (int i = 0; i < num; i++) {
                        string txt = fRawContents[i].Trim();
                        RawLine rawLine = (RawLine)fRawContents.GetObject(i);

                        if (!string.IsNullOrEmpty(txt)) {
                            if (IsGenerationLine(txt)) {
                                rawLine.Type = RawLineType.rltRomeGeneration;
                            } else {
                                PersonNumbersType numbType = PersonNumbersType.pnUndefined;
                                string dummy;

                                if (ImportUtils.IsPersonLine_DAboville(txt, out dummy))
                                {
                                    rawLine.Type = RawLineType.rltPerson;
                                    numbType = PersonNumbersType.pnDAboville;
                                    numberStats[1]++;
                                }
                                else if (ImportUtils.IsPersonLine_Konovalov(txt, out dummy))
                                {
                                    rawLine.Type = RawLineType.rltPerson;
                                    numbType = PersonNumbersType.pnKonovalov;
                                    numberStats[2]++;
                                }

                                rawLine.NumbersType = numbType;
                            }
                        } else {
                            rawLine.Type = RawLineType.rltEOF;
                        }

                        progress.ProgressStep(i + 1);
                    }

                    if (numberStats[1] > numberStats[2]) {
                        CanNumbersType = PersonNumbersType.pnDAboville;
                    } else {
                        CanNumbersType = PersonNumbersType.pnKonovalov;
                    }

                    return true;
                }
                finally
                {
                    progress.ProgressDone();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("Importer.AnalyseRaw(): " + ex.Message);
                return false;
            }
        }

        public bool ImportContent()
        {
            AnalyseRaw();

            switch (SourceType)
            {
                case SourceType.stText:
                    return ImportTextContent();

                case SourceType.stTable:
                    #if !NO_DEPEND
                    return ImportTableContent();
                    #else
                    return false;
                    #endif

                default:
                    return false;
            }
        }

        private bool ImportTextContent()
        {
            try
            {
                fLog.Clear();

                StringList buffer = new StringList();
                try
                {
                    int prev_id = 0;

                    int num = fRawContents.Count;
                    for (int i = 0; i < num; i++)
                    {
                        string line = PrepareLine(fRawContents[i]);
                        RawLine rawLine = (RawLine)fRawContents.GetObject(i);

                        switch (rawLine.Type) {
                            case RawLineType.rltComment:
                                buffer.Add(line);
                                break;

                            case RawLineType.rltPerson:
                            case RawLineType.rltRomeGeneration:
                            case RawLineType.rltEOF:
                                {
                                    ParseBuffer(buffer, ref prev_id);
                                    buffer.Clear();

                                    switch (rawLine.Type) {
                                        case RawLineType.rltPerson:
                                            buffer.Add(line);
                                            break;
                                        case RawLineType.rltRomeGeneration:
                                            fLog.Add("> " + fLangMan.LS(ILS.LSID_Generation) + " \"" + line + "\"");
                                            break;
                                        case RawLineType.rltEOF:
                                            fLog.Add("> EOF.");
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
                Logger.LogWrite("Importer.ImportTextContent(): " + ex.Message);
                throw;
            }
        }

        private static string GetCell(object[,] values, int row, int col)
        {
            object obj = values[row, col];
            return (obj == null) ? "" : obj.ToString();
        }

        #if !NO_DEPEND
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
                catch (Exception)
                {
                    return false;
                }

                excel.Visible = DEBUG_EXCEL;
                excel.DisplayAlerts = false;
                excel.WindowState = MSOExcel.XlWindowState.xlMaximized;
                excel.Workbooks.Open(fFileName);
                MSOExcel.Worksheet sheet = excel.Worksheets[1] as MSOExcel.Worksheet;
                //sheet.Activate();

                IProgressController progress = AppHub.Progress;
                StringList buffer = new StringList();
                try
                {
                    int rowsCount = sheet.UsedRange.Rows.Count;
                    //int colsCount = sheet.UsedRange.Columns.Count;

                    progress.ProgressInit(fLangMan.LS(ILS.LSID_Loading), rowsCount);

                    MSOExcel.Range excelRange = sheet.UsedRange;
                    object[,] valueArray = (object[,])excelRange.get_Value(MSOExcel.XlRangeValueDataType.xlRangeValueDefault);

                    int prevId = 0;

                    for (int row = 1; row <= rowsCount; row++)
                    {
                        string c1 = GetCell(valueArray, row, 1).Trim(); // position number
                        string c2 = GetCell(valueArray, row, 2).Trim(); // ancestor number
                        string c3 = GetCell(valueArray, row, 3).Trim(); // name, maybe start with the number of marriage
                        string c4 = GetCell(valueArray, row, 4).Trim(); // birth date
                        string c5 = GetCell(valueArray, row, 5).Trim(); // death date
                        string c6 = GetCell(valueArray, row, 6).Trim(); // birth or residence place

                        string s123 = c1 + c2;
                        if (s123 != "" && !string.IsNullOrEmpty(c3) && c3[0] != '/') {
                            s123 += ". " + c3;
                        } else {
                            s123 += c3;
                        }

                        if (s123 == "") {
                            continue;
                        }

                        string line, p_id = "";
                        RawLineType lineType = RawLineType.rltComment;

                        if (IsGenerationLine(s123)) {
                            line = s123;
                            lineType = RawLineType.rltRomeGeneration;
                        } else {
                            line = s123 + " " + c4 + " " + c5;
                            if (c6 != "") {
                                line = line + ". " + c6 + ".";
                            }

                            line = line.Trim();

                            if (IsPersonLine(line, ref p_id)) {
                                lineType = RawLineType.rltPerson;
                            }
                        }

                        switch (lineType)
                        {
                            case RawLineType.rltComment:
                                buffer.Add(line);
                                break;

                            case RawLineType.rltPerson:
                            case RawLineType.rltRomeGeneration:
                            case RawLineType.rltEOF:
                                {
                                    ParseBuffer(buffer, ref prevId);
                                    buffer.Clear();

                                    switch (lineType) {
                                        case RawLineType.rltPerson:
                                            buffer.Add(line);
                                            break;
                                        case RawLineType.rltRomeGeneration:
                                            fLog.Add("> " + fLangMan.LS(ILS.LSID_Generation) + " \"" + line + "\"");
                                            break;
                                        case RawLineType.rltEOF:
                                            fLog.Add("> EOF.");
                                            break;
                                    }
                                }
                                break;
                        }

                        progress.ProgressStep(row);
                    }

                    // hack: processing last items before end
                    ParseBuffer(buffer, ref prevId);

                    return true;
                }
                finally
                {
                    progress.ProgressDone();

                    buffer.Dispose();

                    excel.Quit();
                    excel = null;
                }
            }
            catch (Exception ex)
            {
                fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
                Logger.LogWrite("Importer.ImportTableContent(): " + ex.Message);
                return false;
            }
        }
        #endif

        private bool LoadRawText()
        {
            SourceType = SourceType.stText;

            try
            {
                using (StreamReader strd = new StreamReader(fFileName, Encoding.GetEncoding(1251)))
                {
                    IProgressController progress = AppHub.Progress;
                    try
                    {
                        progress.ProgressInit(fLangMan.LS(ILS.LSID_Loading), (int)strd.BaseStream.Length);

                        int lineNum = 0;
                        while (strd.Peek() != -1) {
                            string txt = strd.ReadLine().Trim();

                            if (!string.IsNullOrEmpty(txt)) {
                                fRawContents.AddObject(txt, new RawLine(lineNum));
                            }

                            progress.ProgressStep((int)strd.BaseStream.Position);
                            lineNum++;
                        }
                        fRawContents.AddObject("", new RawLine(lineNum));
                    }
                    finally
                    {
                        progress.ProgressDone();
                    }
                }

                return AnalyseRaw();
            }
            catch (Exception ex)
            {
                fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
                Logger.LogWrite("Importer.LoadRawText(): " + ex.Message);
                return false;
            }
        }

        #if !NO_DEPEND
        private bool LoadRawWord()
        {
            SourceType = SourceType.stText;

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

                IProgressController progress = AppHub.Progress;
                try
                {
                    wordApp.Visible = DEBUG_WORD;
                    wordApp.WindowState = MSOWord.WdWindowState.wdWindowStateMaximize;

                    MSOWord.Document doc = wordApp.Documents.Open(fFileName);

                    progress.ProgressInit(fLangMan.LS(ILS.LSID_Loading), doc.Paragraphs.Count);

                    int lineNum = 0;
                    for (int i = 0; i < doc.Paragraphs.Count; i++)
                    {
                        string txt = doc.Paragraphs[i+1].Range.Text;
                        txt = txt.Trim();

                        if (!string.IsNullOrEmpty(txt)) {
                            fRawContents.AddObject(txt, new RawLine(lineNum));
                        }

                        progress.ProgressStep(i + 1);
                        lineNum++;
                    }
                    fRawContents.AddObject("", new RawLine(lineNum));

                    return AnalyseRaw();
                }
                finally
                {
                    progress.ProgressDone();

                    object saveOptionsObject = MSOWord.WdSaveOptions.wdDoNotSaveChanges;
                    wordApp.Quit(ref saveOptionsObject);
                    wordApp = null;
                }
            }
            catch (Exception ex)
            {
                fLog.Add(">>>> " + fLangMan.LS(ILS.LSID_DataLoadError));
                Logger.LogWrite("Importer.LoadRawWord(): " + ex.Message);
                return false;
            }
        }
        #endif

        private bool LoadRawExcel()
        {
            SourceType = SourceType.stTable;

            return AnalyseRaw();
        }

        public bool LoadRawData(string fileName)
        {
            fRawContents.Clear();

            fFileName = fileName;
            string ext = SysUtils.GetFileExtension(fileName);

            if (ext == ".txt")
            {
                return LoadRawText();
            }
            else if (ext == ".doc")
            {
                #if !NO_DEPEND
                return LoadRawWord();
                #else
                return false;
                #endif
            }
            else if (ext == ".xls")
            {
                #if !NO_DEPEND
                return LoadRawExcel();
                #else
                return false;
                #endif
            }
            else
            {
                throw new ImporterException(fLangMan.LS(ILS.LSID_FormatUnsupported));
            }
        }

        #endregion
    }
}
