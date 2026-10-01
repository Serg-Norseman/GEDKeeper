/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Utilities;

namespace GKCore.Sync
{
    /// <summary>
    ///
    /// </summary>
    public class SyncTool
    {
        private GDMTree fMainTree;
        private GDMTree fOtherTree;

        public List<RecordDiff> Results;

        public GDMTree MainTree { get { return fMainTree; } }
        public GDMTree OtherTree { get { return fOtherTree; } }

        public static IColor GetDiffColor(DiffStatus diffStatus)
        {
            int backColor;
            switch (diffStatus) {
                case DiffStatus.Equal:
                default:
                    backColor = GKColors.White;
                    break;
                case DiffStatus.Deleted:
                    backColor = GKColors.Coral;
                    break;
                case DiffStatus.Inserted:
                    backColor = GKColors.LightBlue;
                    break;
                case DiffStatus.Modified:
                    backColor = GKColors.Yellow;
                    break;
                case DiffStatus.DeepModified:
                    backColor = GKColors.Orange;
                    break;
            }
            return ChartRenderer.GetColor(backColor);
        }

        public void LoadOtherFile(GDMTree mainTree, string fileName)
        {
            if (mainTree == null)
                throw new ArgumentNullException(nameof(mainTree));

            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException(nameof(fileName));

            fMainTree = mainTree;

            fOtherTree = new GDMTree();
            var gedcomProvider = new GEDCOMProvider(fOtherTree);
            gedcomProvider.LoadFromFile(fileName);
        }

        public void CompareTrees(GDMRecordType recordType)
        {
            RecordDiff.ResetNum();

            Results = new List<RecordDiff>();

            var records1 = fMainTree.GetRecords(recordType);
            var records2 = fOtherTree.GetRecords(recordType);

            var diffResults = DiffUtil.Diff(records1, records2, new RecordComparer());
            foreach (var diff in diffResults) {
                var diffRec = new RecordDiff(diff.Obj1, diff.Obj2, diff.Status);
                Results.Add(diffRec);

                /// The primary method for determining the difference between trees based on records is by UID
                /// (<see cref="RecordComparer.Equals" />).
                if (diffRec.Status == DiffStatus.Equal) {
                    var rec1 = diffRec.Obj1;
                    var rec2 = diffRec.Obj2;

                    if (rec1.XRef != rec2.XRef) {
                        diffRec.Status = DiffStatus.Modified;
                    }

                    if (rec1.GetHashCode() != rec2.GetHashCode()) {
                        diffRec.Status = DiffStatus.DeepModified;
                    }
                }
            }
        }

        public List<IDiffResult> CompareRecords(RecordDiff diffRecord)
        {
            TagDiff.ResetNum();

            List<IDiffResult> differences;

            switch (diffRecord.Obj1.RecordType) {
                case GDMRecordType.rtIndividual:
                    differences = CompareIndividualRecords(diffRecord.Obj1 as GDMIndividualRecord, diffRecord.Obj2 as GDMIndividualRecord);
                    break;
                case GDMRecordType.rtFamily:
                    differences = CompareFamilyRecords(diffRecord.Obj1 as GDMFamilyRecord, diffRecord.Obj2 as GDMFamilyRecord);
                    break;
                case GDMRecordType.rtNote:
                    differences = CompareNoteRecords(diffRecord.Obj1 as GDMNoteRecord, diffRecord.Obj2 as GDMNoteRecord);
                    break;
                case GDMRecordType.rtMultimedia:
                    differences = CompareMultimediaRecords(diffRecord.Obj1 as GDMMultimediaRecord, diffRecord.Obj2 as GDMMultimediaRecord);
                    break;
                case GDMRecordType.rtSource:
                    differences = CompareSourceRecords(diffRecord.Obj1 as GDMSourceRecord, diffRecord.Obj2 as GDMSourceRecord);
                    break;
                case GDMRecordType.rtRepository:
                    differences = CompareRepositoryRecords(diffRecord.Obj1 as GDMRepositoryRecord, diffRecord.Obj2 as GDMRepositoryRecord);
                    break;
                case GDMRecordType.rtGroup:
                    differences = CompareGroupRecords(diffRecord.Obj1 as GDMGroupRecord, diffRecord.Obj2 as GDMGroupRecord);
                    break;
                case GDMRecordType.rtResearch:
                    differences = CompareResearchRecords(diffRecord.Obj1 as GDMResearchRecord, diffRecord.Obj2 as GDMResearchRecord);
                    break;
                case GDMRecordType.rtTask:
                    differences = CompareTaskRecords(diffRecord.Obj1 as GDMTaskRecord, diffRecord.Obj2 as GDMTaskRecord);
                    break;
                case GDMRecordType.rtCommunication:
                    differences = CompareCommunicationRecords(diffRecord.Obj1 as GDMCommunicationRecord, diffRecord.Obj2 as GDMCommunicationRecord);
                    break;
                case GDMRecordType.rtLocation:
                    differences = CompareLocationRecords(diffRecord.Obj1 as GDMLocationRecord, diffRecord.Obj2 as GDMLocationRecord);
                    break;
                default:
                    differences = null;
                    break;
            }

            return differences;
        }

        private static void CompareEvents(GDMRecordWithEvents evRec1, GDMRecordWithEvents evRec2, List<IDiffResult> differences)
        {
            CompareLists<GDMCustomEvent>(evRec1.Events, evRec2.Events, new EventComparer<GDMCustomEvent>(), true, differences);
        }

        private static void CompareSourceCitations(IGDMStructWithSourceCitations struct1, IGDMStructWithSourceCitations struct2, List<IDiffResult> differences)
        {
            CompareLists<GDMSourceCitation>(struct1.SourceCitations, struct2.SourceCitations, new PointerComparer<GDMSourceCitation>(), true, differences);
        }

        private static void CompareMultimediaLinks(IGDMStructWithMultimediaLinks struct1, IGDMStructWithMultimediaLinks struct2, List<IDiffResult> differences)
        {
            CompareLists<GDMMultimediaLink>(struct1.MultimediaLinks, struct2.MultimediaLinks, new PointerComparer<GDMMultimediaLink>(), true, differences);
        }

        private static void CompareNotes(IGDMStructWithNotes struct1, IGDMStructWithNotes struct2, List<IDiffResult> differences)
        {
            CompareLists<GDMNotes>(struct1.Notes, struct2.Notes, new PointerComparer<GDMNotes>(), false, differences);
        }

        private static void CompareUserReferences(IGDMStructWithUserReferences struct1, IGDMStructWithUserReferences struct2, List<IDiffResult> differences)
        {
            CompareLists<GDMUserReference>(struct1.UserReferences, struct2.UserReferences, new ValueTagComparer<GDMUserReference>(), true, differences);
        }

        private static void CompareTagLists<T>(GDMList<T> struct1, GDMList<T> struct2, List<IDiffResult> differences) where T : GDMTag
        {
            /// The primary method for determining the difference between pointers is by GetHashCode
            /// (<see cref="TagComparer.Equals" />).
            CompareLists<T>(struct1, struct2, new TagComparer<T>(), false, differences);
        }

        private static void CompareLists<T>(GDMList<T> struct1, GDMList<T> struct2, IEqualityComparer<T> equalityComparer, bool checkChanges, List<IDiffResult> differences) where T : GDMTag
        {
            var diffResults = DiffUtil.Diff(struct1, struct2, equalityComparer);
            foreach (var diff in diffResults) {
                var obj1 = diff.Obj1;
                var obj2 = diff.Obj2;

                var diffRec = new TagDiff(obj1, obj2, diff.Status);
                differences.Add(diffRec);

                if (checkChanges && diffRec.Status == DiffStatus.Equal) {
                    if (obj1.GetHashCode() != obj2.GetHashCode()) {
                        diffRec.Status = DiffStatus.DeepModified;
                    }
                }
            }
        }

        private static void ComparePtrLists<T>(GDMList<T> struct1, GDMList<T> struct2, List<IDiffResult> differences) where T : GDMPointer
        {
            /// The primary method for determining the difference between pointers is by XRef
            /// (<see cref="PointerComparer.Equals" />).
            CompareLists<T>(struct1, struct2, new PointerComparer<T>(), true, differences);
        }

        /// <summary>
        /// GDMValueTag + DiffStatus.Modified -> Assign!
        /// </summary>
        private static void CompareTags(GEDCOMTagType tagType, string val1, string val2, List<IDiffResult> differences)
        {
            if (string.IsNullOrEmpty(val1) && string.IsNullOrEmpty(val2))
                return;

            var diffStatus = (val1 == val2) ? DiffStatus.Equal : DiffStatus.Modified;
            differences.Add(new TagDiff(new GDMValueTag((int)tagType, val1), new GDMValueTag((int)tagType, val2), diffStatus));
        }

        /// <summary>
        /// Value + DiffStatus.Modified -> Assign!
        /// </summary>
        private static void CompareValues(GEDCOMTagType tagType, object val1, object val2, string displayItem1, string displayItem2, List<IDiffResult> differences)
        {
            if (val1 == null && val2 == null) return;
            if (string.IsNullOrEmpty(displayItem1) && string.IsNullOrEmpty(displayItem2)) return;

            var diffStatus = object.Equals(val1, val2) ? DiffStatus.Equal : DiffStatus.Modified;
            differences.Add(new ValDiff((int)tagType, val1, displayItem1, val2, displayItem2, diffStatus));
        }

        private static void CompareValues(GEDCOMTagType tagType, object val1, object val2, List<IDiffResult> differences)
        {
            string displayItem1 = Convert.ToString(val1);
            string displayItem2 = Convert.ToString(val2);
            CompareValues(tagType, val1, val2, displayItem1, displayItem2, differences);
        }

        /// <summary>
        /// T(GDMTag) + DiffStatus.Modified -> Assign!
        /// </summary>
        private static void CompareStruct<T>(T struct1, T struct2, List<IDiffResult> differences) where T : GDMTag
        {
            var eq1 = (IGDEquatable<T>)struct1;
            if (!eq1.DataEquals(struct2))
                differences.Add(new TagDiff(struct1, struct2, DiffStatus.Modified));
        }

        private static void CompareRecords(GDMRecord rec1, GDMRecord rec2, List<IDiffResult> differences)
        {
            CompareSourceCitations(rec1, rec2, differences);
            CompareMultimediaLinks(rec1, rec2, differences);
            CompareNotes(rec1, rec2, differences);
            CompareUserReferences(rec1, rec2, differences);

            CompareValues(GEDCOMTagType.RIN, rec1.AutomatedRecordID, rec2.AutomatedRecordID, differences);
        }

        private static void CompareEventRecords(GDMRecordWithEvents rec1, GDMRecordWithEvents rec2, List<IDiffResult> differences)
        {
            CompareRecords(rec1, rec2, differences);

            CompareEvents(rec1, rec2, differences);
            CompareValues(GEDCOMTagType.RESN, rec1.Restriction, rec2.Restriction, LangMan.LS(GKData.Restrictions[(int)rec1.Restriction]), LangMan.LS(GKData.Restrictions[(int)rec2.Restriction]), differences);
        }

        private static List<IDiffResult> CompareIndividualRecords(GDMIndividualRecord indiRec1, GDMIndividualRecord indiRec2)
        {
            var differences = new List<IDiffResult>();

            CompareEventRecords(indiRec1, indiRec2, differences);

            CompareValues(GEDCOMTagType.SEX, indiRec1.Sex, indiRec2.Sex, GKUtils.SexStr(indiRec1.Sex), GKUtils.SexStr(indiRec2.Sex), differences);

            CompareTagLists<GDMPersonalName>(indiRec1.PersonalNames, indiRec2.PersonalNames, differences);
            CompareTagLists<GDMAssociation>(indiRec1.Associations, indiRec2.Associations, differences);
            CompareTagLists<GDMDNATest>(indiRec1.DNATests, indiRec2.DNATests, differences);
            ComparePtrLists<GDMGroupLink>(indiRec1.Groups, indiRec2.Groups, differences);
            ComparePtrLists<GDMChildToFamilyLink>(indiRec1.ChildToFamilyLinks, indiRec2.ChildToFamilyLinks, differences);
            ComparePtrLists<GDMSpouseToFamilyLink>(indiRec1.SpouseToFamilyLinks, indiRec2.SpouseToFamilyLinks, differences);

            CompareValues(GEDCOMTagType._BOOKMARK, indiRec1.Bookmark, indiRec2.Bookmark, differences);
            CompareValues(GEDCOMTagType._PATRIARCH, indiRec1.Patriarch, indiRec2.Patriarch, differences);

            return differences;
        }

        private static List<IDiffResult> CompareFamilyRecords(GDMFamilyRecord famRec1, GDMFamilyRecord famRec2)
        {
            var differences = new List<IDiffResult>();

            CompareEventRecords(famRec1, famRec2, differences);

            CompareValues(GEDCOMTagType._STAT, famRec1.Status, famRec2.Status, LangMan.LS(GKData.MarriageStatus[(int)famRec1.Status].Name), LangMan.LS(GKData.MarriageStatus[(int)famRec2.Status].Name), differences);

            string husb1XRef = famRec1.Husband?.XRef ?? "";
            string husb2XRef = famRec2.Husband?.XRef ?? "";
            CompareValues(GEDCOMTagType.HUSB, husb1XRef, husb2XRef, differences);
            string wife1XRef = famRec1.Wife?.XRef ?? "";
            string wife2XRef = famRec2.Wife?.XRef ?? "";
            CompareValues(GEDCOMTagType.WIFE, wife1XRef, wife2XRef, differences);
            ComparePtrLists(famRec1.Children, famRec2.Children, differences); // simple pointers

            return differences;
        }

        private static List<IDiffResult> CompareNoteRecords(GDMNoteRecord noteRec1, GDMNoteRecord noteRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(noteRec1, noteRec2, differences);

            CompareValues(GEDCOMTagType.NOTE, noteRec1.Lines.Text, noteRec2.Lines.Text, differences);

            return differences;
        }

        private static List<IDiffResult> CompareMultimediaRecords(GDMMultimediaRecord mediaRec1, GDMMultimediaRecord mediaRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(mediaRec1, mediaRec2, differences);

            CompareTagLists<GDMFileReferenceWithTitle>(mediaRec1.FileReferences, mediaRec2.FileReferences, differences);

            return differences;
        }

        private static List<IDiffResult> CompareSourceRecords(GDMSourceRecord sourRec1, GDMSourceRecord sourRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(sourRec1, sourRec2, differences);

            CompareValues(GEDCOMTagType.ABBR, sourRec1.ShortTitle, sourRec2.ShortTitle, differences);
            CompareValues(GEDCOMTagType.TITL, sourRec1.Title.StringValue, sourRec2.Title.StringValue, differences);
            CompareValues(GEDCOMTagType.AUTH, sourRec1.Originator.StringValue, sourRec2.Originator.StringValue, differences);
            CompareValues(GEDCOMTagType.PUBL, sourRec1.Publication.StringValue, sourRec2.Publication.StringValue, differences);
            CompareValues(GEDCOMTagType.TEXT, sourRec1.Text.StringValue, sourRec2.Text.StringValue, differences);

            CompareTagLists<GDMRepositoryCitation>(sourRec1.RepositoryCitations, sourRec2.RepositoryCitations, differences);
            CompareStruct(sourRec1.Data, sourRec2.Data, differences);
            CompareValues(GEDCOMTagType.DATE, sourRec1.Date, sourRec2.Date, GKUtils.GetDateDisplayString(sourRec1.Date), GKUtils.GetDateDisplayString(sourRec2.Date), differences);

            return differences;
        }

        private static List<IDiffResult> CompareRepositoryRecords(GDMRepositoryRecord repRec1, GDMRepositoryRecord repRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(repRec1, repRec2, differences);

            CompareValues(GEDCOMTagType.NAME, repRec1.RepositoryName, repRec2.RepositoryName, differences);
            CompareStruct(repRec1.Address, repRec2.Address, differences);

            return differences;
        }

        private static List<IDiffResult> CompareGroupRecords(GDMGroupRecord groupRec1, GDMGroupRecord groupRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(groupRec1, groupRec2, differences);

            CompareValues(GEDCOMTagType.NAME, groupRec1.GroupName, groupRec2.GroupName, differences);
            ComparePtrLists<GDMMemberLink>(groupRec1.Members, groupRec2.Members, differences);

            return differences;
        }

        private static List<IDiffResult> CompareResearchRecords(GDMResearchRecord resRec1, GDMResearchRecord resRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(resRec1, resRec2, differences);

            CompareValues(GEDCOMTagType.NAME, resRec1.ResearchName, resRec2.ResearchName, differences);
            CompareValues(GEDCOMTagType._PRIORITY, resRec1.Priority, resRec2.Priority, GKInfoPanel.GetPriorityStr(resRec1.Priority), GKInfoPanel.GetPriorityStr(resRec2.Priority), differences);
            CompareValues(GEDCOMTagType._STATUS, resRec1.Status, resRec2.Status, LangMan.LS(GKData.StatusNames[(int)resRec1.Status]), LangMan.LS(GKData.StatusNames[(int)resRec2.Status]), differences);
            CompareValues(GEDCOMTagType._PERCENT, resRec1.Percent, resRec2.Percent, resRec1.Percent.ToString(), resRec2.Percent.ToString(), differences);
            CompareValues(GEDCOMTagType._STARTDATE, resRec1.StartDate, resRec2.StartDate, GKUtils.GetDateDisplayString(resRec1.StartDate), GKUtils.GetDateDisplayString(resRec2.StartDate), differences);
            CompareValues(GEDCOMTagType._STOPDATE, resRec1.StopDate, resRec2.StopDate, GKUtils.GetDateDisplayString(resRec1.StopDate), GKUtils.GetDateDisplayString(resRec2.StopDate), differences);

            // simple pointers, not require details
            ComparePtrLists<GDMPointer>(resRec1.Tasks, resRec2.Tasks, differences);
            ComparePtrLists<GDMPointer>(resRec1.Communications, resRec2.Communications, differences);
            ComparePtrLists<GDMPointer>(resRec1.Groups, resRec2.Groups, differences);

            return differences;
        }

        private List<IDiffResult> CompareTaskRecords(GDMTaskRecord taskRec1, GDMTaskRecord taskRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(taskRec1, taskRec2, differences);

            CompareValues(GEDCOMTagType._GOAL, taskRec1.Goal, taskRec2.Goal, GKUtils.GetTaskGoalStr(fMainTree, taskRec1), GKUtils.GetTaskGoalStr(fOtherTree, taskRec2), differences);
            CompareValues(GEDCOMTagType._PRIORITY, taskRec1.Priority, taskRec2.Priority, GKInfoPanel.GetPriorityStr(taskRec1.Priority), GKInfoPanel.GetPriorityStr(taskRec2.Priority), differences);
            CompareValues(GEDCOMTagType._STARTDATE, taskRec1.StartDate, taskRec2.StartDate, GKUtils.GetDateDisplayString(taskRec1.StartDate), GKUtils.GetDateDisplayString(taskRec2.StartDate), differences);
            CompareValues(GEDCOMTagType._STOPDATE, taskRec1.StopDate, taskRec2.StopDate, GKUtils.GetDateDisplayString(taskRec1.StopDate), GKUtils.GetDateDisplayString(taskRec2.StopDate), differences);

            return differences;
        }

        private static List<IDiffResult> CompareCommunicationRecords(GDMCommunicationRecord commRec1, GDMCommunicationRecord commRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(commRec1, commRec2, differences);

            CompareValues(GEDCOMTagType.NAME, commRec1.CommName, commRec2.CommName, differences);
            CompareValues(GEDCOMTagType.TYPE, commRec1.CommunicationType, commRec2.CommunicationType, LangMan.LS(GKData.CommunicationNames[(int)commRec1.CommunicationType]), LangMan.LS(GKData.CommunicationNames[(int)commRec2.CommunicationType]), differences);
            CompareValues(GEDCOMTagType._DIR, commRec1.CommDirection, commRec2.CommDirection, LangMan.LS(GKData.CommunicationDirs[(int)commRec1.CommDirection]), LangMan.LS(GKData.CommunicationDirs[(int)commRec2.CommDirection]), differences);
            CompareValues(GEDCOMTagType.DATE, commRec1.Date, commRec2.Date, GKUtils.GetDateDisplayString(commRec1.Date), GKUtils.GetDateDisplayString(commRec2.Date), differences);

            //CompareValues(GEDCOMTagType._CORR, commRec1.Corresponder.XRef, commRec2.Corresponder.XRef, differences);
            CompareStruct(commRec1.Corresponder, commRec2.Corresponder, differences);

            return differences;
        }

        private static List<IDiffResult> CompareLocationRecords(GDMLocationRecord locRec1, GDMLocationRecord locRec2)
        {
            var differences = new List<IDiffResult>();

            CompareRecords(locRec1, locRec2, differences);

            CompareStruct(locRec1.Map, locRec2.Map, differences);
            CompareLists<GDMLocationName>(locRec1.Names, locRec2.Names, new ValueTagComparer<GDMLocationName>(), true, differences);
            ComparePtrLists<GDMLocationLink>(locRec1.TopLevels, locRec2.TopLevels, differences);

            return differences;
        }

        /// <summary>
        /// Accept all checked changes in the tree - by records.
        /// </summary>
        public bool AcceptChange(IEnumerable<RecordDiff> recordsDiff)
        {
            bool result = true;
            foreach (var diff in recordsDiff) {
                switch (diff.Status) {
                    case DiffStatus.Equal:
                        // TODO: disable checkbox for equal status
                        break;

                    case DiffStatus.Deleted:
                        fMainTree.DeleteRecord(diff.Obj1);
                        break;

                    case DiffStatus.Inserted:
                        if (CheckLinks(diff.Obj2)) {
                            // TODO
                            //fMainTree.AddRecord(diff.Obj2.Clone());
                        }
                        break;

                    case DiffStatus.Modified:
                    case DiffStatus.DeepModified:
                        // Merging changes into the record is only possible via the detailed comparison dialog.
                        // TSDetailForm -> Merge()
                        break;
                }
            }
            return result;
        }

        /// <summary>
        /// Merge changes of one record across different databases.
        /// </summary>
        public bool Merge(GDMRecord target, GDMRecord source, IEnumerable<IDiffResult> tagsDiff)
        {
            bool result = false;

            foreach (var diff in tagsDiff) {
                switch (diff.Status) {
                    case DiffStatus.Equal:
                        // TODO: disable checkbox for equal status
                        break;

                    case DiffStatus.Deleted:
                        var tagDiff_d = diff as TagDiff;
                        result = RemoveStruct(target, tagDiff_d.Obj1);
                        break;

                    case DiffStatus.Inserted:
                        var tagDiff_i = diff as TagDiff;
                        if (CheckLinks(tagDiff_i.Obj2)) {
                            result = AddStruct(target, tagDiff_i.Obj2);
                        }
                        break;

                    case DiffStatus.Modified:
                    case DiffStatus.DeepModified:
                        result = AssignChange(target, diff);
                        break;
                }
            }

            return result;
        }

        private bool CheckLinks(GDMTag tag)
        {
            // TODO: Create a cross-index from the XRef in the second file to the position in the diff
            // to determine whether it is local to the second file or existed in the first.
            var refs = ReferenceVerifier.VerifyTagReferences(fMainTree, tag);
            if (refs.Count > 0) {
                var strList = string.Join(", ", refs);
                var vote = AppHost.StdDialogs.ShowQuestion(string.Format("The main tree is missing records:\n{0}. Add them?", strList));

                // TODO: Compare links based on differences between trees for cases
                // where records with a specific XRef were added independently.
                return false;
            }
            return true;
        }

        /// <summary>
        /// Accept all checked changes in the record - by sub-structures and tags.
        /// </summary>
        private bool AssignChange(GDMRecord target, IDiffResult diff)
        {
            if (diff is ValDiff valDiff) {
                string strVal2 = Convert.ToString(valDiff.Obj2);

                switch ((GEDCOMTagType)valDiff.ValType) {
                    case GEDCOMTagType.RESN:
                        ((GDMRecordWithEvents)target).Restriction = (GDMRestriction)valDiff.Obj2;
                        break;
                    case GEDCOMTagType.SEX:
                        ((GDMIndividualRecord)target).Sex = (GDMSex)valDiff.Obj2;
                        break;
                    case GEDCOMTagType._STAT:
                        ((GDMFamilyRecord)target).Status = (GDMMarriageStatus)valDiff.Obj2;
                        break;
                    case GEDCOMTagType.DATE:
                        switch (target.RecordType) {
                            case GDMRecordType.rtSource:
                                ((GDMSourceRecord)target).Date.Assign(valDiff.Obj2 as GDMCustomDate);
                                break;
                            case GDMRecordType.rtCommunication:
                                ((GDMCommunicationRecord)target).Date.Assign(valDiff.Obj2 as GDMCustomDate);
                                break;
                        }
                        break;
                    case GEDCOMTagType._PRIORITY:
                        switch (target.RecordType) {
                            case GDMRecordType.rtResearch:
                                ((GDMResearchRecord)target).Priority = (GDMResearchPriority)valDiff.Obj2;
                                break;
                            case GDMRecordType.rtTask:
                                ((GDMTaskRecord)target).Priority = (GDMResearchPriority)valDiff.Obj2;
                                break;
                        }
                        break;
                    case GEDCOMTagType._STATUS:
                        ((GDMResearchRecord)target).Status = (GDMResearchStatus)valDiff.Obj2;
                        break;
                    case GEDCOMTagType._PERCENT:
                        ((GDMResearchRecord)target).Percent = (int)valDiff.Obj2;
                        break;
                    case GEDCOMTagType._STARTDATE:
                        switch (target.RecordType) {
                            case GDMRecordType.rtResearch:
                                ((GDMResearchRecord)target).StartDate.Assign(valDiff.Obj2 as GDMCustomDate);
                                break;
                            case GDMRecordType.rtTask:
                                ((GDMTaskRecord)target).StartDate.Assign(valDiff.Obj2 as GDMCustomDate);
                                break;
                        }
                        break;
                    case GEDCOMTagType._STOPDATE:
                        switch (target.RecordType) {
                            case GDMRecordType.rtResearch:
                                ((GDMResearchRecord)target).StopDate.Assign(valDiff.Obj2 as GDMCustomDate);
                                break;
                            case GDMRecordType.rtTask:
                                ((GDMTaskRecord)target).StopDate.Assign(valDiff.Obj2 as GDMCustomDate);
                                break;
                        }
                        break;
                    case GEDCOMTagType._GOAL:
                        ((GDMTaskRecord)target).Goal = strVal2;
                        break;
                    case GEDCOMTagType.TYPE:
                        ((GDMCommunicationRecord)target).CommunicationType = (GDMCommunicationType)valDiff.Obj2;
                        break;
                    case GEDCOMTagType._DIR:
                        ((GDMCommunicationRecord)target).CommDirection = (GDMCommunicationDir)valDiff.Obj2;
                        break;

                    /*case GEDCOMTagType._CORR:
                        // processed as ptr
                        ((GDMCommunicationRecord)target).Corresponder.XRef = strVal2;
                        break;*/

                    case GEDCOMTagType._BOOKMARK:
                        ((GDMIndividualRecord)target).Bookmark = (bool)valDiff.Obj2;
                        break;
                    case GEDCOMTagType._PATRIARCH:
                        ((GDMIndividualRecord)target).Patriarch = (bool)valDiff.Obj2;
                        break;

                    case GEDCOMTagType.RIN:
                        target.AutomatedRecordID = strVal2;
                        break;

                    case GEDCOMTagType.HUSB:
                        ((GDMFamilyRecord)target).Husband.XRef = strVal2;
                        break;

                    case GEDCOMTagType.WIFE:
                        ((GDMFamilyRecord)target).Wife.XRef = strVal2;
                        break;

                    case GEDCOMTagType.NOTE:
                        ((GDMNoteRecord)target).Lines.Text = strVal2;
                        break;

                    case GEDCOMTagType.ABBR:
                        ((GDMSourceRecord)target).ShortTitle = strVal2;
                        break;

                    case GEDCOMTagType.TITL:
                        ((GDMSourceRecord)target).Title.Lines.Text = strVal2;
                        break;

                    case GEDCOMTagType.AUTH:
                        ((GDMSourceRecord)target).Originator.Lines.Text = strVal2;
                        break;

                    case GEDCOMTagType.PUBL:
                        ((GDMSourceRecord)target).Publication.Lines.Text = strVal2;
                        break;

                    case GEDCOMTagType.TEXT:
                        ((GDMSourceRecord)target).Text.Lines.Text = strVal2;
                        break;

                    case GEDCOMTagType.NAME:
                        switch (target.RecordType) {
                            case GDMRecordType.rtRepository:
                                ((GDMRepositoryRecord)target).RepositoryName = strVal2;
                                break;
                            case GDMRecordType.rtGroup:
                                ((GDMGroupRecord)target).GroupName = strVal2;
                                break;
                            case GDMRecordType.rtResearch:
                                ((GDMResearchRecord)target).ResearchName = strVal2;
                                break;
                            case GDMRecordType.rtCommunication:
                                ((GDMCommunicationRecord)target).CommName = strVal2;
                                break;
                        }
                        break;
                }

                return false;
            } else if (diff is TagDiff tagDiff) {
                if (CheckLinks(tagDiff.Obj2)) {
                    tagDiff.Obj1.Assign(tagDiff.Obj2);
                    return true;
                }
            }

            return false;
        }

        private bool RemoveStruct<T>(GDMRecord target, T xStruct) where T : GDMTag
        {
            if (xStruct is GDMPersonalName persName) {
                ((GDMIndividualRecord)target).PersonalNames.Remove(persName);
            } else if (xStruct is GDMDNATest dnaTest) {
                ((GDMIndividualRecord)target).DNATests.Remove(dnaTest);
            } else if (xStruct is GDMGroupLink groupLink) {
                ((GDMIndividualRecord)target).Groups.Remove(groupLink);
            } else if (xStruct is GDMChildToFamilyLink ctfLink) {
                ((GDMIndividualRecord)target).ChildToFamilyLinks.Remove(ctfLink);
            } else if (xStruct is GDMSpouseToFamilyLink spfLink) {
                ((GDMIndividualRecord)target).SpouseToFamilyLinks.Remove(spfLink);
            } else if (xStruct is GDMChildLink child) {
                ((GDMFamilyRecord)target).Children.Remove(child);
            } else if (xStruct is GDMIndividualEvent indiEvent) {
                ((GDMIndividualRecord)target).Events.Remove(indiEvent);
            } else if (xStruct is GDMIndividualAttribute indiAttr) {
                ((GDMIndividualRecord)target).Events.Remove(indiAttr);
            } else if (xStruct is GDMFamilyEvent famEvent) {
                ((GDMFamilyRecord)target).Events.Remove(famEvent);
            } else if (xStruct is GDMAssociation asso) {
                ((GDMIndividualRecord)target).Associations.Remove(asso);
            } else if (xStruct is GDMSourceCitation sourCit) {
                target.SourceCitations.Remove(sourCit);
            } else if (xStruct is GDMMultimediaLink mediaLink) {
                target.MultimediaLinks.Remove(mediaLink);
            } else if (xStruct is GDMNotes noteLink) {
                target.Notes.Remove(noteLink);
            } else if (xStruct is GDMUserReference userRef) {
                target.UserReferences.Remove(userRef);
            } else if (xStruct is GDMRepositoryCitation repoCit) {
                ((GDMSourceRecord)target).RepositoryCitations.Remove(repoCit);
            } else if (xStruct is GDMFileReferenceWithTitle fileRef) {
                ((GDMMultimediaRecord)target).FileReferences.Remove(fileRef);
            } else if (xStruct is GDMLocationName locName) {
                ((GDMLocationRecord)target).Names.Remove(locName);
            } else if (xStruct is GDMLocationLink locLink) {
                ((GDMLocationRecord)target).TopLevels.Remove(locLink);
            } else if (xStruct is GDMMemberLink member) {
                ((GDMGroupRecord)target).Members.Remove(member);
            } else if (xStruct is GDMPointer ptr) {
                switch ((GEDCOMTagType)ptr.Id) {
                    case GEDCOMTagType._TASK:
                        ((GDMResearchRecord)target).Tasks.Remove(ptr);
                        break;
                    case GEDCOMTagType._COMM:
                        ((GDMResearchRecord)target).Communications.Remove(ptr);
                        break;
                    case GEDCOMTagType._GROUP:
                        ((GDMResearchRecord)target).Groups.Remove(ptr);
                        break;
                }
            }

            return true;
        }

        private bool AddStruct<T>(GDMRecord target, T xStruct) where T : GDMTag
        {
            if (xStruct is GDMPersonalName persName) {
                ((GDMIndividualRecord)target).PersonalNames.Add(persName.Clone());
            } else if (xStruct is GDMDNATest dnaTest) {
                ((GDMIndividualRecord)target).DNATests.Add(dnaTest.Clone());
            } else if (xStruct is GDMGroupLink groupLink) {
                ((GDMIndividualRecord)target).Groups.Add(groupLink.Clone());
            } else if (xStruct is GDMChildToFamilyLink ctfLink) {
                ((GDMIndividualRecord)target).ChildToFamilyLinks.Add(ctfLink.Clone());
            } else if (xStruct is GDMSpouseToFamilyLink spfLink) {
                ((GDMIndividualRecord)target).SpouseToFamilyLinks.Add(spfLink.Clone());
            } else if (xStruct is GDMChildLink child) {
                ((GDMFamilyRecord)target).Children.Add(child.Clone());
            } else if (xStruct is GDMIndividualEvent indiEvent) {
                ((GDMIndividualRecord)target).Events.Add(indiEvent.Clone());
            } else if (xStruct is GDMIndividualAttribute indiAttr) {
                ((GDMIndividualRecord)target).Events.Add(indiAttr.Clone());
            } else if (xStruct is GDMFamilyEvent famEvent) {
                ((GDMFamilyRecord)target).Events.Add(famEvent.Clone());
            } else if (xStruct is GDMAssociation asso) {
                ((GDMIndividualRecord)target).Associations.Add(asso.Clone());
            } else if (xStruct is GDMSourceCitation sourCit) {
                target.SourceCitations.Add(sourCit.Clone());
            } else if (xStruct is GDMMultimediaLink mediaLink) {
                target.MultimediaLinks.Add(mediaLink.Clone());
            } else if (xStruct is GDMNotes noteLink) {
                target.Notes.Add(noteLink.Clone());
            } else if (xStruct is GDMUserReference userRef) {
                target.UserReferences.Add(userRef.Clone());
            } else if (xStruct is GDMRepositoryCitation repoCit) {
                ((GDMSourceRecord)target).RepositoryCitations.Add(repoCit.Clone());
            } else if (xStruct is GDMFileReferenceWithTitle fileRef) {
                ((GDMMultimediaRecord)target).FileReferences.Add(fileRef.Clone());
            } else if (xStruct is GDMLocationName locName) {
                ((GDMLocationRecord)target).Names.Add(locName.Clone());
            } else if (xStruct is GDMLocationLink locLink) {
                ((GDMLocationRecord)target).TopLevels.Add(locLink.Clone());
            } else if (xStruct is GDMMemberLink member) {
                ((GDMGroupRecord)target).Members.Add(member.Clone());
            } else if (xStruct is GDMPointer ptr) {
                switch ((GEDCOMTagType)ptr.Id) {
                    case GEDCOMTagType._TASK:
                        ((GDMResearchRecord)target).Tasks.Add(ptr.Clone());
                        break;
                    case GEDCOMTagType._COMM:
                        ((GDMResearchRecord)target).Communications.Add(ptr.Clone());
                        break;
                    case GEDCOMTagType._GROUP:
                        ((GDMResearchRecord)target).Groups.Add(ptr.Clone());
                        break;
                }
            }

            return true;
        }
    }
}
