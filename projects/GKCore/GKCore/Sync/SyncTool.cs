/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
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

        public List<DiffRecord> Results;

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
            DiffRecord.ResetNum();

            Results = new List<DiffRecord>();

            var records1 = fMainTree.GetRecords(recordType);
            var records2 = fOtherTree.GetRecords(recordType);

            var diffResults = DiffUtil.Diff(records1, records2, new RecordComparer());
            foreach (var diff in diffResults) {
                var diffRec = new DiffRecord(diff.Obj1, diff.Obj2, diff.Status);
                Results.Add(diffRec);

                /// The primary method for determining the difference between trees based on records is by UID
                /// (<see cref="RecordComparer.Equals" />).
                if (diffRec.Status == DiffStatus.Equal) {
                    var rec1 = diffRec.Obj1;
                    var rec2 = diffRec.Obj2;

                    // ChangeDateTime eliminated, only contents!
                    if (rec1.XRef != rec2.XRef /*|| rec1.ChangeDate.ChangeDateTime != rec2.ChangeDate.ChangeDateTime*/) {
                        diffRec.Status = DiffStatus.Modified;
                    }

                    if (rec1.GetHashCode() != rec2.GetHashCode()) {
                        diffRec.Status = DiffStatus.DeepModified;
                    }
                }
            }
        }

        public List<DiffTag> CompareRecords(DiffRecord diffRecord)
        {
            DiffTag.ResetNum();

            List<DiffTag> differences;

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

        private static void CompareEvents(IGDMRecordWithEvents evRec1, IGDMRecordWithEvents evRec2, List<DiffTag> differences)
        {
            CompareLists<GDMCustomEvent>(evRec1.Events, evRec2.Events, new EventComparer<GDMCustomEvent>(), true, differences);
        }

        private static void CompareSourceCitations(IGDMStructWithSourceCitations struct1, IGDMStructWithSourceCitations struct2, List<DiffTag> differences)
        {
            CompareLists<GDMSourceCitation>(struct1.SourceCitations, struct2.SourceCitations, new PointerComparer<GDMSourceCitation>(), true, differences);
        }

        private static void CompareMultimediaLinks(IGDMStructWithMultimediaLinks struct1, IGDMStructWithMultimediaLinks struct2, List<DiffTag> differences)
        {
            CompareLists<GDMMultimediaLink>(struct1.MultimediaLinks, struct2.MultimediaLinks, new PointerComparer<GDMMultimediaLink>(), true, differences);
        }

        private static void CompareNotes(IGDMStructWithNotes struct1, IGDMStructWithNotes struct2, List<DiffTag> differences)
        {
            CompareLists<GDMNotes>(struct1.Notes, struct2.Notes, new PointerComparer<GDMNotes>(), false, differences);
        }

        private static void CompareUserReferences(IGDMStructWithUserReferences struct1, IGDMStructWithUserReferences struct2, List<DiffTag> differences)
        {
            CompareLists<GDMUserReference>(struct1.UserReferences, struct2.UserReferences, new ValueTagComparer<GDMUserReference>(), true, differences);
        }

        private static void CompareTagLists<T>(GDMList<T> struct1, GDMList<T> struct2, List<DiffTag> differences) where T : GDMTag
        {
            /// The primary method for determining the difference between pointers is by GetHashCode
            /// (<see cref="TagComparer.Equals" />).
            CompareLists<T>(struct1, struct2, new TagComparer<T>(), false, differences);
        }

        private static void CompareLists<T>(GDMList<T> struct1, GDMList<T> struct2, IEqualityComparer<T> equalityComparer, bool checkChanges, List<DiffTag> differences) where T : GDMTag
        {
            var diffResults = DiffUtil.Diff(struct1, struct2, equalityComparer);
            foreach (var diff in diffResults) {
                var obj1 = diff.Obj1;
                var obj2 = diff.Obj2;

                var diffRec = new DiffTag(obj1, obj2, diff.Status);
                differences.Add(diffRec);

                if (checkChanges && diffRec.Status == DiffStatus.Equal) {
                    if (obj1.GetHashCode() != obj2.GetHashCode()) {
                        diffRec.Status = DiffStatus.DeepModified;
                    }
                }
            }
        }

        private static void ComparePtrLists<T>(GDMList<T> struct1, GDMList<T> struct2, List<DiffTag> differences) where T : GDMPointer
        {
            /// The primary method for determining the difference between pointers is by XRef
            /// (<see cref="PointerComparer.Equals" />).
            CompareLists<T>(struct1, struct2, new PointerComparer<T>(), true, differences);
        }

        private static void CompareSimpleTag(GEDCOMTagType tagType, string val1, string val2, List<DiffTag> differences)
        {
            if (string.IsNullOrEmpty(val1) && string.IsNullOrEmpty(val2))
                return;

            var diffStatus = (val1 == val2) ? DiffStatus.Equal : DiffStatus.Modified;
            differences.Add(new DiffTag(new GDMValueTag((int)tagType, val1), new GDMValueTag((int)tagType, val2), diffStatus));
        }

        private static void CompareStruct<T>(T struct1, T struct2, List<DiffTag> differences) where T : GDMTag
        {
            var eq1 = (IGDEquatable<T>)struct1;
            if (!eq1.DataEquals(struct2))
                differences.Add(new DiffTag(struct1, struct2, DiffStatus.Modified));
        }

        private static void CompareRecords(GDMRecord rec1, GDMRecord rec2, List<DiffTag> differences)
        {
            CompareSourceCitations(rec1, rec2, differences);
            CompareMultimediaLinks(rec1, rec2, differences);
            CompareNotes(rec1, rec2, differences);
            CompareUserReferences(rec1, rec2, differences);

            CompareSimpleTag(GEDCOMTagType.RIN, rec1.AutomatedRecordID, rec2.AutomatedRecordID, differences);
        }

        private static void CompareEventRecords(GDMRecordWithEvents rec1, GDMRecordWithEvents rec2, List<DiffTag> differences)
        {
            CompareRecords(rec1, rec2, differences);

            CompareEvents(rec1, rec2, differences);
            CompareSimpleTag(GEDCOMTagType.RESN, LangMan.LS(GKData.Restrictions[(int)rec1.Restriction]), LangMan.LS(GKData.Restrictions[(int)rec2.Restriction]), differences);
        }

        private static List<DiffTag> CompareIndividualRecords(GDMIndividualRecord indiRec1, GDMIndividualRecord indiRec2)
        {
            var differences = new List<DiffTag>();

            CompareEventRecords(indiRec1, indiRec2, differences);

            CompareSimpleTag(GEDCOMTagType.SEX, GKUtils.SexStr(indiRec1.Sex), GKUtils.SexStr(indiRec2.Sex), differences);

            CompareTagLists<GDMPersonalName>(indiRec1.PersonalNames, indiRec2.PersonalNames, differences);
            CompareTagLists<GDMAssociation>(indiRec1.Associations, indiRec2.Associations, differences);
            CompareTagLists<GDMDNATest>(indiRec1.DNATests, indiRec2.DNATests, differences);
            ComparePtrLists<GDMGroupLink>(indiRec1.Groups, indiRec2.Groups, differences);
            ComparePtrLists<GDMChildToFamilyLink>(indiRec1.ChildToFamilyLinks, indiRec2.ChildToFamilyLinks, differences);
            ComparePtrLists<GDMSpouseToFamilyLink>(indiRec1.SpouseToFamilyLinks, indiRec2.SpouseToFamilyLinks, differences);

            return differences;
        }

        private static List<DiffTag> CompareFamilyRecords(GDMFamilyRecord famRec1, GDMFamilyRecord famRec2)
        {
            var differences = new List<DiffTag>();

            CompareEventRecords(famRec1, famRec2, differences);

            CompareSimpleTag(GEDCOMTagType._STAT, LangMan.LS(GKData.MarriageStatus[(int)famRec1.Status].Name), LangMan.LS(GKData.MarriageStatus[(int)famRec2.Status].Name), differences);

            string husb1XRef = famRec1.Husband?.XRef ?? "";
            string husb2XRef = famRec2.Husband?.XRef ?? "";
            CompareSimpleTag(GEDCOMTagType.HUSB, husb1XRef, husb2XRef, differences);
            string wife1XRef = famRec1.Wife?.XRef ?? "";
            string wife2XRef = famRec2.Wife?.XRef ?? "";
            CompareSimpleTag(GEDCOMTagType.WIFE, wife1XRef, wife2XRef, differences);
            ComparePtrLists(famRec1.Children, famRec2.Children, differences); // simple pointers

            return differences;
        }

        private static List<DiffTag> CompareNoteRecords(GDMNoteRecord noteRec1, GDMNoteRecord noteRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(noteRec1, noteRec2, differences);

            CompareSimpleTag(GEDCOMTagType.NOTE, noteRec1.Lines.Text, noteRec2.Lines.Text, differences);

            return differences;
        }

        private static List<DiffTag> CompareMultimediaRecords(GDMMultimediaRecord mediaRec1, GDMMultimediaRecord mediaRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(mediaRec1, mediaRec2, differences);

            CompareTagLists<GDMFileReferenceWithTitle>(mediaRec1.FileReferences, mediaRec2.FileReferences, differences);

            return differences;
        }

        private static List<DiffTag> CompareSourceRecords(GDMSourceRecord sourRec1, GDMSourceRecord sourRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(sourRec1, sourRec2, differences);

            CompareSimpleTag(GEDCOMTagType.ABBR, sourRec1.ShortTitle, sourRec1.ShortTitle, differences);
            CompareSimpleTag(GEDCOMTagType.TITL, sourRec1.Title.StringValue, sourRec1.Title.StringValue, differences);
            CompareSimpleTag(GEDCOMTagType.AUTH, sourRec1.Originator.StringValue, sourRec1.Originator.StringValue, differences);
            CompareSimpleTag(GEDCOMTagType.PUBL, sourRec1.Publication.StringValue, sourRec1.Publication.StringValue, differences);
            CompareSimpleTag(GEDCOMTagType.TEXT, sourRec1.Text.StringValue, sourRec1.Text.StringValue, differences);

            CompareTagLists<GDMRepositoryCitation>(sourRec1.RepositoryCitations, sourRec2.RepositoryCitations, differences);
            CompareStruct(sourRec1.Data, sourRec2.Data, differences);
            CompareSimpleTag(GEDCOMTagType.DATE, GetDateStr(sourRec1.Date), GetDateStr(sourRec1.Date), differences);

            return differences;
        }

        private static List<DiffTag> CompareRepositoryRecords(GDMRepositoryRecord repRec1, GDMRepositoryRecord repRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(repRec1, repRec2, differences);

            CompareSimpleTag(GEDCOMTagType.NAME, repRec1.RepositoryName, repRec2.RepositoryName, differences);
            CompareStruct(repRec1.Address, repRec2.Address, differences);

            return differences;
        }

        private static List<DiffTag> CompareGroupRecords(GDMGroupRecord groupRec1, GDMGroupRecord groupRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(groupRec1, groupRec2, differences);

            CompareSimpleTag(GEDCOMTagType.NAME, groupRec1.GroupName, groupRec2.GroupName, differences);
            ComparePtrLists<GDMMemberLink>(groupRec1.Members, groupRec2.Members, differences);

            return differences;
        }

        private static List<DiffTag> CompareResearchRecords(GDMResearchRecord resRec1, GDMResearchRecord resRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(resRec1, resRec2, differences);

            CompareSimpleTag(GEDCOMTagType.NAME, resRec1.ResearchName, resRec2.ResearchName, differences);
            CompareSimpleTag(GEDCOMTagType._PRIORITY, GetPriorityStr(resRec1.Priority), GetPriorityStr(resRec2.Priority), differences);
            CompareSimpleTag(GEDCOMTagType._STATUS, LangMan.LS(GKData.StatusNames[(int)resRec1.Status]), LangMan.LS(GKData.StatusNames[(int)resRec2.Status]), differences);
            CompareSimpleTag(GEDCOMTagType._PERCENT, resRec1.Percent.ToString(), resRec2.Percent.ToString(), differences);
            CompareSimpleTag(GEDCOMTagType._STARTDATE, GetDateStr(resRec1.StartDate), GetDateStr(resRec2.StartDate), differences);
            CompareSimpleTag(GEDCOMTagType._STOPDATE, GetDateStr(resRec1.StopDate), GetDateStr(resRec2.StopDate), differences);

            // simple pointers, not require details
            ComparePtrLists<GDMPointer>(resRec1.Tasks, resRec2.Tasks, differences);
            ComparePtrLists<GDMPointer>(resRec1.Communications, resRec2.Communications, differences);
            ComparePtrLists<GDMPointer>(resRec1.Groups, resRec2.Groups, differences);

            return differences;
        }

        private List<DiffTag> CompareTaskRecords(GDMTaskRecord taskRec1, GDMTaskRecord taskRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(taskRec1, taskRec2, differences);

            CompareSimpleTag(GEDCOMTagType._GOAL, GKUtils.GetTaskGoalStr(fMainTree, taskRec1), GKUtils.GetTaskGoalStr(fOtherTree, taskRec2), differences);
            CompareSimpleTag(GEDCOMTagType._PRIORITY, GetPriorityStr(taskRec1.Priority), GetPriorityStr(taskRec2.Priority), differences);
            CompareSimpleTag(GEDCOMTagType._STARTDATE, GetDateStr(taskRec1.StartDate), GetDateStr(taskRec2.StartDate), differences);
            CompareSimpleTag(GEDCOMTagType._STOPDATE, GetDateStr(taskRec1.StopDate), GetDateStr(taskRec2.StopDate), differences);

            return differences;
        }

        private static List<DiffTag> CompareCommunicationRecords(GDMCommunicationRecord commRec1, GDMCommunicationRecord commRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(commRec1, commRec2, differences);

            CompareSimpleTag(GEDCOMTagType.NAME, commRec1.CommName, commRec2.CommName, differences);
            CompareSimpleTag(GEDCOMTagType.TYPE, LangMan.LS(GKData.CommunicationNames[(int)commRec1.CommunicationType]), LangMan.LS(GKData.CommunicationNames[(int)commRec2.CommunicationType]), differences);
            //CompareSimpleTag(GEDCOMTagType., commRec1.CommDirection.ToString(), commRec2.CommDirection.ToString(), differences); // TODO
            CompareSimpleTag(GEDCOMTagType.DATE, GetDateStr(commRec1.Date), GetDateStr(commRec2.Date), differences);

            return differences;
        }

        private static List<DiffTag> CompareLocationRecords(GDMLocationRecord locRec1, GDMLocationRecord locRec2)
        {
            var differences = new List<DiffTag>();

            CompareRecords(locRec1, locRec2, differences);

            CompareStruct(locRec1.Map, locRec2.Map, differences);
            CompareLists<GDMLocationName>(locRec1.Names, locRec2.Names, new ValueTagComparer<GDMLocationName>(), true, differences);
            ComparePtrLists<GDMLocationLink>(locRec1.TopLevels, locRec2.TopLevels, differences);

            return differences;
        }

        private static string GetDateStr(GDMCustomDate date)
        {
            return GKUtils.GetDateDisplayString(date);
        }

        private static string GetPriorityStr(GDMResearchPriority priority)
        {
            return LangMan.LS(GKData.PriorityNames[(int)priority]);
        }

        public bool Merge(GDMRecord target, GDMRecord source, IEnumerable<DiffTag> tagsDiff)
        {
            bool result = false;

#if RELEASE
            AppHost.StdDialogs.ShowWarning("Merge functionality is not yet implemented.");
            return result;
#endif

            // TODO: Implement merging logic
            foreach (var diff in tagsDiff) {
                switch (diff.Status) {
                    case DiffStatus.Equal:
                        // TODO: disable checkbox for equal status
                        break;

                    case DiffStatus.Deleted:
                        DeleteStruct(target, diff.Obj1);
                        result = true;
                        break;

                    case DiffStatus.Inserted:
                        if (CheckLinks(diff)) {
                            InsertStruct(target, diff.Obj2);
                            result = true;
                        }
                        break;

                    case DiffStatus.Modified:
                    case DiffStatus.DeepModified:
                        if (CheckLinks(diff)) {
                            diff.Obj1.Assign(diff.Obj2);
                            result = true;
                        }
                        break;
                }
            }

            return result;
        }

        // TODO: CROSS-RECORD LINKS! - for merge and insert!
        private bool CheckLinks(DiffTag tagDiff)
        {
            // TODO: Create a cross-index from the XRef in the second file to the position in the diff
            // to determine whether it is local to the second file or existed in the first.
            return false;
        }

        private void DeleteStruct<T>(GDMRecord target, T xStruct) where T : GDMTag
        {
            // TODO: variations of record types and first-level structure types!
        }

        private void InsertStruct<T>(GDMRecord target, T xStruct) where T : GDMTag
        {
            // TODO: cross-record links
            // TODO: variations of record types and first-level structure types!
        }
    }
}
