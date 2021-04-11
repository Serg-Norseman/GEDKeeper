/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using BSLib.Design.Handlers;
using GDModel;
using GDModel.Providers.GEDCOM;
using GEDmill.Model;
using GKCore;
using GKCore.Logging;
using GKCore.Tools;
using GKCore.Types;

namespace GEDmill
{
    /// <summary>
    /// Utility functions.
    /// </summary>
    public static class GMHelper
    {
        public const string GfxFilter = "JPEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|"
                                        + "Portable Network Graphics (*.png)|*.png|"
                                        + "Graphics Interchange Format (*.gif)|*.gif|"
                                        + "Windows Bitmap (*.bmp)|*.bmp|"
                                        + "All supported picture files|*.jpg;*.jpeg;*.gif;*.bmp;*.png";


        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(GMHelper).Name);


        private static readonly Dictionary<string, CISRecordChanges> recX = new Dictionary<string, CISRecordChanges>();

        public static void SetVisibility(GDMRecord record, bool value)
        {
            if (record == null) return;

            CISRecordChanges changes;
            if (!recX.TryGetValue(record.XRef, out changes)) {
                changes = new CISRecordChanges(true);
                recX.Add(record.XRef, changes);
            }

            changes.Visibility = value;
        }

        public static bool GetVisibility(GDMRecord record)
        {
            if (record == null) return true;

            CISRecordChanges changes;
            if (!recX.TryGetValue(record.XRef, out changes)) {
                return true;
            }

            return changes.Visibility;
        }

        // Modifies rectNew to fit within the limits given, keeping its aspect ratio
        public static void ScaleAreaToFit(ref Rectangle rectNew, int uMaxWidth, int uMaxHeight)
        {
            if (rectNew.Height > uMaxHeight) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Width = (rectNew.Width * (int)uMaxHeight) / rectNew.Height;
                rectNew.Height = (int)uMaxHeight;
            }

            if (rectNew.Width > uMaxWidth) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Height = (rectNew.Height * (int)uMaxWidth) / rectNew.Width;
                rectNew.Width = (int)uMaxWidth;
            }
        }

        // Presents a file selection dialog and returns the selecetd file name and path
        public static bool SelectFile(ref string fileDir, ref string fileName, string title, string defaultName,
                                      bool loadNotSave, string filterName, List<string> filterExtensions)
        {
            bool fileSelected = false;

            FileDialog fileDialog;
            if (loadNotSave) {
                fileDialog = new OpenFileDialog();
            } else {
                fileDialog = new SaveFileDialog();
            }

            if (fileDir.Length > 0) {
                fileDialog.InitialDirectory = fileDir;
            } else {
                fileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
            }

            if (fileName.Length > 0) {
                fileDialog.FileName = fileName;
            } else {
                fileDialog.FileName = defaultName;
            }

            string sFilterString = "";
            int nFilterAllIndex = 1;
            if (filterExtensions.Count > 0) {
                nFilterAllIndex++;
                string sFilterCode = "";
                bool bFirst = true;
                //"Picture files (*.jpg; *.gif)|*.jpg;*.gif|All files (*.*)|*.*";
                foreach (string sFilterExtn in filterExtensions) {
                    if (!bFirst) {
                        sFilterCode += ";";
                    } else {
                        bFirst = false;
                    }
                    sFilterCode += "*" + sFilterExtn;
                }
                sFilterString = filterName + " (" + sFilterCode + ")|" + sFilterCode + "|";
            }
            sFilterString += "All files (*.*)|*.*";
            fileDialog.Filter = sFilterString;
            fileDialog.FilterIndex = 1;
            string sExtn = Path.GetExtension(fileDialog.FileName);

            // Check whether selected file matches given filter
            bool bValidExtn = true;
            if (fileDialog.FileName.Length > 0) {
                bValidExtn = false;
                string sExtnFromDlg = Path.GetExtension(fileDialog.FileName).ToUpper();
                foreach (string sFilterExtn in filterExtensions) {
                    if (sExtnFromDlg == sFilterExtn.ToUpper()) {
                        bValidExtn = true;
                        break;
                    }
                }
            }

            if (!bValidExtn) {
                // Use *.* filter if default file isn't a .txt file.
                fileDialog.FilterIndex = nFilterAllIndex;
            }
            fileDialog.RestoreDirectory = true;
            fileDialog.Title = title;

            if (fileDialog.ShowDialog() == DialogResult.OK) {
                fileSelected = true;
                fileDir = Path.GetDirectoryName(fileDialog.FileName);
                fileName = Path.GetFileName(fileDialog.FileName);
            }
            fLogger.WriteInfo("Selected file : " + fileDir + "\\" + fileName);

            return (fileSelected);
        }

        // Used to display the finished website. Uses whatever app the user has assigned to open HTML files.
        public static void OpenURL(string sURL)
        {
            try {
                System.Diagnostics.Process.Start(sURL);
            } catch (Exception e2) {
                fLogger.WriteError("Caught exception while opening finished webpages : {0}", e2);
            }
        }

        public static string GetAppPath()
        {
            return Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);
        }

        // Returns true if the given sFilename would exist on the Windows Desktop.
        // This is a hack ( because I don't know the official way to find the path of the user's Desktop ).
        public static bool IsDesktop(string filename)
        {
            string path_part = Path.GetDirectoryName(filename);
            // Strip trailing slashes
            while (path_part.Length > 0 && path_part.Substring(path_part.Length - 1, 1) == "\\") {
                path_part = path_part.Substring(0, path_part.Length - 1);
            }
            int folder_index = path_part.LastIndexOf('\\');
            if (folder_index > 0 && (folder_index + 1) < path_part.Length) {
                string folder_name = path_part.Substring(folder_index + 1);
                if (folder_name == "Desktop") {
                    return (true);
                }
            }
            return (false);
        }

        // Modifies the provided string to have its first letter capitalised and the rest unchanged.
        public static void Capitalise(ref string s)
        {
            if (!string.IsNullOrEmpty(s)) {
                s = Char.ToUpper(s[0]) + s.Substring(1);
            } else {
                s = "";
            }
        }

        // Capitalises an individual's name according to config setting
        public static string CapitaliseName(string name, ref string firstName, ref string surname)
        {
            if (name == null) {
                if (surname != null) {
                    surname = CConfig.Instance.UnknownName;
                }
                return CConfig.Instance.UnknownName;
            }

            string newName = "";
            switch (CConfig.Instance.NameCapitalisation) {
                case 1:
                case 0:
                    // capitalise surname (the bit in //s)
                    bool bSeenSlash = false;
                    bool bFirstName = true;
                    char oldc = '\0';
                    foreach (char c in name) {
                        if (c == '/') {
                            bSeenSlash = !bSeenSlash;
                            if (bFirstName && oldc != ' ' && newName.Length > 0) {
                                // Ensure there is a space between first and last names (e.g. from "Fred/Bloggs/")
                                newName += ' ';
                                oldc = ' '; // To make oldc set to space too.
                            } else {
                                oldc = c;
                            }
                            bFirstName = false;
                        } else if (bSeenSlash) {
                            char cc = c;
                            if (CConfig.Instance.NameCapitalisation == 1) {
                                cc = char.ToUpper(cc);
                            }
                            newName += cc;
                            if (surname != null) {
                                surname += cc;
                            }
                            oldc = c;
                        } else {
                            newName += c;

                            // Collapse multiple spaces into one
                            if (oldc != ' ' || c != ' ') {
                                if (bFirstName && firstName != null) {
                                    firstName += c;
                                } else if (!bFirstName && surname != null) {
                                    surname += c;
                                }
                            }
                            oldc = c;
                        }
                    }
                    break;
                default:
                    newName = name;
                    break;
            }

            return newName;
        }

        // Returns the name of the alternative picture file to display for non-diaplayable files of the given format
        public static string NonPicFilename(string format, bool small, bool clickToDownload)
        {
            string filename;
            switch (format.ToLower()) {
                case "wav":
                case "mp3":
                case "mid":
                case "midi":
                case "rmi":
                case "au":
                case "wma":
                    filename = small ? "gmaudio_sm.png" : clickToDownload ? "gmaudio.png" : "gmaudion.png";
                    break;
                case "avi":
                case "mpeg":
                case "mpg":
                case "wmv":
                    filename = small ? "gmvideo_sm.png" : clickToDownload ? "gmvideo.png" : "gmvideon.png";
                    break;
                default:
                    filename = small ? "gmdoc_sm.png" : clickToDownload ? "gmdoc.png" : "gmdocn.png";
                    break;
            }
            return filename;
        }

        public static int GetChronologicalYear(GDMDateValue dateVal)
        {
            return (dateVal == null) ? 0 : dateVal.GetChronologicalYear();
        }

        public static int GetEventsYearsDiff(GDMDateValue ev1, GDMDateValue ev2, bool currentEnd = true)
        {
            int result = -1;

            try {
                int dt1 = GetChronologicalYear(ev1);
                int dt2 = GetChronologicalYear(ev2);

                if (currentEnd && dt2 == 0) {
                    dt2 = DateTime.Now.Year;
                }

                if (dt1 != 0 && dt2 != 0) {
                    result = Math.Abs(dt2 - dt1);
                }
            } catch (Exception ex) {
                fLogger.WriteError("GKUtils.GetEventsYearsDiff()", ex);
            }

            return result;
        }

        public static string GetName(GDMIndividualRecord iRec, int i)
        {
            return (i >= 0 && i < iRec.PersonalNames.Count) ? iRec.PersonalNames[i].StringValue : "";
        }

        // Returns the n'th name and associated sources
        public static NameAndSource GetNameAndSource(GDMIndividualRecord record, int n)
        {
            if (record.PersonalNames.Count <= n || n < 0) {
                return null;
            }
            GDMPersonalName pns = record.PersonalNames[n];
            NameAndSource nas = new NameAndSource(pns.StringValue);
            nas.Sources.AddRange(pns.SourceCitations);
            return nas;
        }

        public static List<GDMFamilyRecord> GetFamilyList(GDMTree tree, GDMIndividualRecord record)
        {
            var result = new List<GDMFamilyRecord>();

            foreach (var link in record.SpouseToFamilyLinks) {
                var family = tree.GetPtrValue(link);
                if (family != null) {
                    result.Add(family);
                }
            }

            return result;
        }

        public static string GetLifeDatesStr(GDMIndividualRecord record)
        {
            var lifeDates = record.GetLifeDates();
            //TODO
            //TreeChartOptions options = GlobalOptions.Instance.ChartOptions;
            //DateFormat dateFormat = (options.OnlyYears) ? DateFormat.dfYYYY : DateFormat.dfDD_MM_YYYY;
            DateFormat dateFormat = DateFormat.dfYYYY;

            string birthDate = GKUtils.GEDCOMEventToDateStr(lifeDates.BirthEvent, dateFormat, false);
            string deathDate = GKUtils.GEDCOMEventToDateStr(lifeDates.DeathEvent, dateFormat, false);
            return birthDate + " - " + deathDate;
        }

        public static bool IsPictureFormat(GDMFileReferenceWithTitle fileRef)
        {
            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
            return (mmKind == MultimediaKind.mkImage);
        }

        public static string MakeLinkNumber(GDMSourceCitation sourCit, int sourceCount, bool bComma)
        {
            string sComma = bComma ? "," : "";
            return string.Concat("<span class=\"reference\">", sComma, sourceCount.ToString(), "</span>");
        }

        // Returns a string to use in the list of references at the bottom of the page
        public static string MakeLinkText(GDMTree tree, GDMSourceCitation sourCit, int sourceCount)
        {
            var sourRec = tree.GetPtrValue<GDMSourceRecord>(sourCit);
            return string.Concat(sourceCount.ToString(), ". ", /*m_sSourceDescription*/sourRec.ShortTitle);
        }

        public static void RestrictAssociatedSources(GDMTree tree, GDMIndividualRecord iRec)
        {
            // Restrict sources connected with individual directly
            foreach (GDMSourceCitation sc in iRec.SourceCitations) {
                RestrictSource(tree, sc, true);
            }

            // Restrict sources connected with name
            foreach (GDMPersonalName pns in iRec.PersonalNames) {
                foreach (GDMSourceCitation sc in pns.SourceCitations) {
                    RestrictSource(tree, sc, true);
                }
            }

            // Restrict sources connected with events
            foreach (GDMCustomEvent ies in iRec.Events) {
                foreach (GDMSourceCitation sc in ies.SourceCitations) {
                    RestrictSource(tree, sc, true);
                }
            }

            // Restrict sources connected with m_associationStructures
            foreach (GDMAssociation ass in iRec.Associations) {
                foreach (GDMSourceCitation sc in ass.SourceCitations) {
                    RestrictSource(tree, sc, true);
                }
            }
        }

        // Marks the given source citation as (un)restricted
        public static void RestrictSource(GDMTree tree, GDMSourceCitation sc, bool visible)
        {
            if (tree != null && sc != null) {
                var sourceRec = tree.GetPtrValue<GDMSourceRecord>(sc);
                if (sourceRec != null) {
                    GMHelper.SetVisibility(sourceRec, visible);
                }
            }
        }

        private static bool RestrictProc(GDMIndividualRecord iRec, TreeTools.TreeWalkMode mode, object extData)
        {
            bool visible = (bool)extData;
            GMHelper.SetVisibility(iRec, visible);
            return true;
        }

        public static void RestrictAncestors(GDMTree tree, GDMIndividualRecord iRec, bool visible)
        {
            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmAncestors, RestrictProc, ((object)visible));
        }

        public static void RestrictDescendants(GDMTree tree, GDMIndividualRecord iRec, bool visible)
        {
            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmDescendants, RestrictProc, ((object)visible));
        }

        private static bool MarkProc(GDMIndividualRecord iRec, TreeTools.TreeWalkMode mode, object extData)
        {
            var marks = (List<GDMRecord>)extData;
            marks.Add(iRec);
            return true;
        }

        public static void MarkConnected(GDMTree tree, GDMIndividualRecord iRec, List<GDMRecord> marks)
        {
            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmAll, MarkProc, marks);
        }

        public static void RestrictUnmarked(GDMTree tree, List<GDMRecord> marks, out int changed)
        {
            changed = 0;
            var treeEnum = tree.GetEnumerator(GDMRecordType.rtIndividual);
            GDMRecord record;
            while (treeEnum.MoveNext(out record)) {
                if (!marks.Contains(record)) {
                    GMHelper.SetVisibility(record, false);
                    changed++;
                }
            }
        }

        public static int SetAllMFRsVisible(GDMTree tree, GDMRecord record, bool visible)
        {
            int nChanged = 0;
            foreach (GDMMultimediaLink mfr in record.MultimediaLinks) {
                var mmRec = tree.GetPtrValue<GDMMultimediaRecord>(mfr);
                if (mmRec != null && GMHelper.GetVisibility(mmRec) != visible) {
                    GMHelper.SetVisibility(mmRec, visible);
                    nChanged++;
                }
            }
            return nChanged;
        }

        private static void GetBackReferences(GDMRecord inRecord, GDMRecord subject, List<BackReference> list)
        {
            try {
                if (subject is GDMSourceRecord) {
                    int num = inRecord.SourceCitations.Count;
                    for (int i = 0; i < num; i++) {
                        if (inRecord.SourceCitations[i].XRef == subject.XRef) {
                            list.Add(new BackReference(inRecord.RecordType, inRecord.XRef, ""));
                        }
                    }

                    var recordWithEvents = inRecord as GDMRecordWithEvents;
                    if (recordWithEvents != null) {
                        GDMRecordWithEvents evsRec = recordWithEvents;

                        num = evsRec.Events.Count;
                        for (int i = 0; i < num; i++) {
                            var evt = evsRec.Events[i];

                            int num2 = evt.SourceCitations.Count;
                            for (int k = 0; k < num2; k++) {
                                if (evt.SourceCitations[k].XRef == subject.XRef) {
                                    list.Add(new BackReference(inRecord.RecordType, inRecord.XRef, evt.GetTagName()));
                                }
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("GMHelper.GetBackReferences()", ex);
            }
        }

        public static List<BackReference> GetBackReferences(GDMTree tree, GDMSourceRecord sourceRec)
        {
            var result = new List<BackReference>();

            int num2 = tree.RecordsCount;
            for (int j = 0; j < num2; j++) {
                GetBackReferences(tree[j], sourceRec, result);
            }

            return result;
        }

        public static Dictionary<object, GDMIndividualRecord> MakeBackReferences(GDMRecord record)
        {
            var result = new Dictionary<object, GDMIndividualRecord>();
            return result;
        }

        public static void CountMFRs(GDMRecord record, out int total, out int visible)
        {
            total = 0;
            visible = 0;
        }

        public static Color SelectColor(Color color)
        {
            var colorHandle = AppHost.StdDialogs.SelectColor(new ColorHandler(color));
            return ((ColorHandler)colorHandle).Handle;
        }

        public static string GetInitialDirectory(string fileName)
        {
            string sPath = fileName;
            if (!Directory.Exists(sPath)) {
                int iLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (iLastFolder >= 0) {
                    sPath = sPath.Substring(0, iLastFolder);
                }
                if (!Directory.Exists(sPath)) {
                    sPath = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
                }
            }
            return sPath;
        }

        public static bool IsSupportedFile(string fileName)
        {
            if (!string.IsNullOrEmpty(fileName)) {
                string exten = Path.GetExtension(fileName).ToLower();
                if (exten != ".jpg" && exten != ".jpeg" && exten != ".png" && exten != ".gif" && exten != ".bmp") {
                    MessageBox.Show("The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                    return false;
                }
            }
            return true;
        }

        public static string GetIndiName(string surname, string firstName)
        {
            string name = "";

            if (firstName != "" && surname != "") {
                name = string.Concat(surname, ", ", firstName);
            } else if (surname != "") {
                name = surname;
            } else {
                name = firstName;
            }

            if (name == "") {
                name = CConfig.Instance.UnknownName;
            }

            return name;
        }
    }
}
