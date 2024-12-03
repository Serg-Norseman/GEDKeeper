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
using System.Globalization;
using System.IO;
using System.Reflection;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GEDmill.Model;
using GKCore;
using GKCore.Tools;
using GKCore.Types;
using GKL = GKCore.Logging;

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


        private static readonly GKL.ILogger fLogger = GKL.LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(GMHelper).Name);


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
            if (record == null) return false;

            CISRecordChanges changes;
            if (!recX.TryGetValue(record.XRef, out changes)) {
                return true;
            }

            return changes.Visibility;
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
        public static string CapitaliseName(GDMPersonalName name, out string firstName, out string surname)
        {
            if (name == null) {
                firstName = string.Empty;
                surname = GMConfig.Instance.UnknownName;
                return GMConfig.Instance.UnknownName;
            }

            string tmpFirstName = name.Given;
            string tmpSurname = name.Surname;

            if (GMConfig.Instance.NameCapitalisation == 1) {
                tmpSurname = tmpSurname.ToUpper();
            }

            firstName = tmpFirstName;
            surname = tmpSurname;
            return string.Concat(tmpFirstName, " ", tmpSurname);
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
            if (iRec.HasEvents) {
                foreach (GDMCustomEvent ies in iRec.Events) {
                    foreach (GDMSourceCitation sc in ies.SourceCitations) {
                        RestrictSource(tree, sc, true);
                    }
                }
            }

            // Restrict sources connected with associations
            if (iRec.HasAssociations) {
                foreach (GDMAssociation ass in iRec.Associations) {
                    foreach (GDMSourceCitation sc in ass.SourceCitations) {
                        RestrictSource(tree, sc, true);
                    }
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
            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmAncestors, RestrictProc, visible);
        }

        public static void RestrictDescendants(GDMTree tree, GDMIndividualRecord iRec, bool visible)
        {
            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmDescendants, RestrictProc, visible);
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

                    var evsRec = inRecord as GDMRecordWithEvents;
                    if (evsRec != null && evsRec.HasEvents) {
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

        public static string GetNowDateStr()
        {
            string result = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss", DateTimeFormatInfo.InvariantInfo);
            return result;
        }

        // Modifies rectNew to fit within the limits given, keeping its aspect ratio
        public static void ScaleAreaToFit(ref ExtRect rectangle, int maxWidth, int maxHeight)
        {
            if (rectangle.Height > maxHeight) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectangle = new ExtRect(rectangle.Left, rectangle.Top, (rectangle.Width * maxHeight) / rectangle.Height, maxHeight);
            }

            if (rectangle.Width > maxWidth) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectangle = new ExtRect(rectangle.Left, rectangle.Top, (rectangle.Height * maxWidth) / rectangle.Width, maxWidth);
            }
        }

        public static string ConstructName(string surname, string firstName)
        {
            string name;
            if (firstName != "" && surname != "") {
                name = string.Concat(surname, ", ", firstName);
            } else if (surname != "") {
                name = surname;
            } else {
                name = firstName;
            }

            if (name == "") {
                name = GMConfig.Instance.UnknownName;
            }

            return name;
        }

        public static string GetLifeDatesStr(GDMIndividualRecord record)
        {
            var lifeDates = record.GetLifeEvents();
            string birthDate = GKUtils.GEDCOMEventToDateStr(lifeDates.BirthEvent, DateFormat.dfYYYY, false);
            string deathDate = GKUtils.GEDCOMEventToDateStr(lifeDates.DeathEvent, DateFormat.dfYYYY, false);
            return birthDate + " - " + deathDate;
        }

        public static Stream LoadResourceStream(string resName)
        {
            Assembly assembly = typeof(GMHelper).Assembly;
            Stream resStream = assembly.GetManifestResourceStream(resName);
            return resStream;
        }
    }
}
