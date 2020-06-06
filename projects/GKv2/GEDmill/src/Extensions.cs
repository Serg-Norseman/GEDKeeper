/* 
 * Copyright 2019-2020 Serg V. Zhdanovskih
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
using GDModel;
using GEDmill.Model;
using GKCore;
using GKCore.Tools;
using GKCore.Types;

namespace GEDmill
{
    public static class Extensions
    {
        private static readonly Dictionary<string, CISRecordChanges> recX = new Dictionary<string, CISRecordChanges>();

        public static void SetVisibility(this GDMRecord record, bool value)
        {
            if (record == null) return;

            CISRecordChanges changes;
            if (!recX.TryGetValue(record.XRef, out changes)) {
                changes = new CISRecordChanges(true);
                recX.Add(record.XRef, changes);
            }

            changes.Visibility = value;
        }

        public static bool GetVisibility(this GDMRecord record)
        {
            if (record == null) return true;

            CISRecordChanges changes;
            if (!recX.TryGetValue(record.XRef, out changes)) {
                return true;
            }

            return changes.Visibility;
        }

        public static Dictionary<object, GDMIndividualRecord> MakeBackReferences(this GDMRecord record)
        {
            var result = new Dictionary<object, GDMIndividualRecord>();
            return result;
        }

        // Returns the n'th name and associated sources
        public static NameAndSource GetNameAndSource(this GDMIndividualRecord record, int n)
        {
            if (record.PersonalNames.Count <= n || n < 0) {
                return null;
            }
            GDMPersonalName pns = record.PersonalNames[n];
            NameAndSource nas = new NameAndSource(pns.StringValue);
            nas.Sources.AddRange(pns.Pieces.SourceCitations);
            return nas;
        }

        public static List<GDMFamilyRecord> GetFamilyList(this GDMIndividualRecord record)
        {
            var result = new List<GDMFamilyRecord>();

            foreach (var link in record.SpouseToFamilyLinks) {
                var family = link.Value as GDMFamilyRecord;
                if (family != null) {
                    result.Add(family);
                }
            }

            return result;
        }

        public static List<BackReference> GetBackReferences(this GDMSourceRecord record)
        {
            var result = new List<BackReference>();
            return result;
        }

        public static string GetLifeDatesStr(this GDMIndividualRecord record)
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

        public static bool IsPictureFormat(this GDMFileReferenceWithTitle fileRef)
        {
            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
            return (mmKind == MultimediaKind.mkImage);
        }

        private static bool PruneProc(GDMIndividualRecord iRec, TreeTools.TreeWalkMode mode, object extData)
        {
            bool visible = (bool)extData;
            iRec.SetVisibility(visible);
            return true;
        }

        public static void PruneAncestors(this GDMTree tree, GDMIndividualRecord iRec, bool visible)
        {
            TreeTools.WalkTree(iRec, TreeTools.TreeWalkMode.twmAncestors, PruneProc, ((object)visible));
        }

        public static void PruneDescendants(this GDMTree tree, GDMIndividualRecord iRec, bool visible)
        {
            TreeTools.WalkTree(iRec, TreeTools.TreeWalkMode.twmDescendants, PruneProc, ((object)visible));
        }

        private static bool PruneMarkProc(GDMIndividualRecord iRec, TreeTools.TreeWalkMode mode, object extData)
        {
            var marks = (List<GDMRecord>)extData;
            marks.Add(iRec);
            return true;
        }

        public static void PruneMarkConnected(this GDMTree tree, GDMIndividualRecord iRec, List<GDMRecord> marks)
        {
            TreeTools.WalkTree(iRec, TreeTools.TreeWalkMode.twmAll, PruneMarkProc, marks);
        }

        public static void PruneUnmarked(this GDMTree tree, List<GDMRecord> marks)
        {
            var treeEnum = tree.GetEnumerator(GDMRecordType.rtIndividual);
            GDMRecord record;
            while (treeEnum.MoveNext(out record)) {
                if (marks.IndexOf(record) < 0) {
                    record.SetVisibility(false);
                }
            }
        }

        public static string MakeLinkNumber(this GDMSourceCitation sourCit, uint uSourceCount, bool bComma)
        {
            string sComma = bComma ? "," : "";
            return string.Concat("<span class=\"reference\">", sComma, uSourceCount.ToString(), "</span>");
        }

        // Returns a string to use in the list of references at the bottom of the page
        public static string MakeLinkText(this GDMSourceCitation sourCit, uint uSourceCount)
        {
            var sourRec = sourCit.Value as GDMSourceRecord;
            return string.Concat(uSourceCount.ToString(), ". ", /*m_sSourceDescription*/sourRec.ShortTitle);
        }

        public static void RestrictAssociatedSources(this GDMTree tree, GDMIndividualRecord iRec)
        {
            // Restrict sources connected with individual directly
            foreach (GDMSourceCitation sc in iRec.SourceCitations) {
                RestrictSource(sc, true);
            }

            // Restrict sources connected with name
            foreach (GDMPersonalName pns in iRec.PersonalNames) {
                foreach (GDMSourceCitation sc in pns.Pieces.SourceCitations) {
                    RestrictSource(sc, true);
                }
            }

            // Restrict sources connected with events
            foreach (GDMCustomEvent ies in iRec.Events) {
                foreach (GDMSourceCitation sc in ies.SourceCitations) {
                    RestrictSource(sc, true);
                }
            }

            // Restrict sources connected with m_associationStructures
            foreach (GDMAssociation ass in iRec.Associations) {
                foreach (GDMSourceCitation sc in ass.SourceCitations) {
                    RestrictSource(sc, true);
                }
            }
        }

        // Marks the given source citation as (un)restricted
        public static void RestrictSource(GDMSourceCitation sc, bool visible)
        {
            if (sc != null) {
                GDMSourceRecord sr = sc.Value as GDMSourceRecord;
                if (sr != null) {
                    sr.SetVisibility(visible);
                }
            }
        }

        public static int SetAllMFRsVisible(this GDMRecord record, bool visible)
        {
            int nChanged = 0;
            foreach (GDMMultimediaLink mfr in record.MultimediaLinks) {
                var mmRec = mfr.Value as GDMMultimediaRecord;
                if (mmRec != null && mmRec.GetVisibility() != visible) {
                    mmRec.SetVisibility(visible);
                    nChanged++;
                }
            }
            return nChanged;
        }

        public static int CountVisibleMFRs(this GDMSourceRecord sourRec)
        {
            return 0;
        }

        public static int CountAllMFRs(this GDMSourceRecord sourRec)
        {
            return 0;
        }

        public static int CountVisibleMFRs(this GDMIndividualRecord iRec)
        {
            return 0;
        }

        public static int CountAllMFRs(this GDMIndividualRecord iRec)
        {
            return 0;
        }

        public static string GetName(this GDMIndividualRecord iRec, int i)
        {
            return (i >= 0 && i < iRec.PersonalNames.Count) ? iRec.PersonalNames[i].StringValue : "";
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
                Logger.WriteError("GKUtils.GetEventsYearsDiff(): ", ex);
            }

            return result;
        }
    }
}
