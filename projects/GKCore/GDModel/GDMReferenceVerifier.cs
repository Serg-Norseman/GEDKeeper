/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    /// <summary>
    /// Class for verifying references and dependencies between GDM records in different trees.
    /// </summary>
    public class GDMReferenceVerifier
    {
        /// <summary>
        /// Represents the status of a reference in the destination tree.
        /// </summary>
        public enum RefStatus
        {
            Present,
            Missing
        }

        /// <summary>
        /// Represents a reference with its status.
        /// </summary>
        public class RefInfo
        {
            public string XRef { get; }
            public RefStatus Status { get; set; }

            public RefInfo(string xref, RefStatus status)
            {
                XRef = xref;
                Status = status;
            }
        }

        /// <summary>
        /// Verifies all references for a given record.
        /// </summary>
        public static List<RefInfo> VerifyReferences(GDMTag obj, GDMTree sourceTree, GDMTree targetTree, bool onlyMissing = false)
        {
            var refInfos = new List<RefInfo>();
            var processedRefs = new HashSet<string>();
            var stack = new Stack<string>();
            var visitor = new GDMTagReferenceVerifier();

            if (obj is GDMRecord record) {
                stack.Push(record.XRef);
            } else {
                obj.Accept(visitor);
                foreach (var link in visitor.Result) {
                    if (!processedRefs.Contains(link))
                        stack.Push(link);
                }
            }

            while (stack.Count > 0) {
                var currentXRef = stack.Pop();
                processedRefs.Add(currentXRef);

                var status = targetTree.FindXRef<GDMRecord>(currentXRef) != null ? RefStatus.Present : RefStatus.Missing;
                if (!onlyMissing || status == RefStatus.Missing)
                    refInfos.Add(new RefInfo(currentXRef, status));

                var sourceRecord = sourceTree.FindXRef<GDMRecord>(currentXRef);

                visitor.Result.Clear();
                sourceRecord.Accept(visitor);

                foreach (var link in visitor.Result) {
                    if (!processedRefs.Contains(link))
                        stack.Push(link);
                }
            }

            return refInfos;
        }

        /// <summary>
        /// Collects all XRefs from a record and its substructures.
        /// </summary>
        public static HashSet<string> CollectRefs(GDMRecord record, GDMTree tree)
        {
            var refs = new HashSet<string>();

            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    CollectRefsFromIndividual((GDMIndividualRecord)record, refs);
                    break;
                case GDMRecordType.rtFamily:
                    CollectRefsFromFamily((GDMFamilyRecord)record, refs);
                    break;
                case GDMRecordType.rtNote:
                    CollectRefsFromNote((GDMNoteRecord)record, refs);
                    break;
                case GDMRecordType.rtMultimedia:
                    CollectRefsFromMultimedia((GDMMultimediaRecord)record, refs);
                    break;
                case GDMRecordType.rtSource:
                    CollectRefsFromSource((GDMSourceRecord)record, refs);
                    break;
                case GDMRecordType.rtRepository:
                    CollectRefsFromRepository((GDMRepositoryRecord)record, refs);
                    break;
                case GDMRecordType.rtGroup:
                    CollectRefsFromGroup((GDMGroupRecord)record, refs);
                    break;
                case GDMRecordType.rtResearch:
                    CollectRefsFromResearch((GDMResearchRecord)record, refs);
                    break;
                case GDMRecordType.rtTask:
                    CollectRefsFromTask((GDMTaskRecord)record, refs);
                    break;
                case GDMRecordType.rtCommunication:
                    CollectRefsFromCommunication((GDMCommunicationRecord)record, refs);
                    break;
                case GDMRecordType.rtLocation:
                    CollectRefsFromLocation((GDMLocationRecord)record, refs);
                    break;
                case GDMRecordType.rtSubmission:
                    CollectRefsFromSubmission((GDMSubmissionRecord)record, refs);
                    break;
                case GDMRecordType.rtSubmitter:
                    CollectRefsFromSubmitter((GDMSubmitterRecord)record, refs);
                    break;
            }

            return refs;
        }

        private static void CollectRefsFromBaseRecord(GDMRecord record, HashSet<string> refs)
        {
            if (record.HasNotes) {
                for (int i = 0; i < record.Notes.Count; i++) {
                    CollectRef(record.Notes[i].XRef, refs);
                }
            }

            if (record.HasMultimediaLinks) {
                for (int i = 0; i < record.MultimediaLinks.Count; i++) {
                    CollectRef(record.MultimediaLinks[i].XRef, refs);
                }
            }

            if (record.HasSourceCitations) {
                for (int i = 0; i < record.SourceCitations.Count; i++) {
                    CollectRef(record.SourceCitations[i].XRef, refs);
                }
            }

            if (record.HasUserReferences) {
                // Do nothing: user references don't have XRefs
            }
        }

        internal static void CollectRefsFromEvent(GDMCustomEvent evt, HashSet<string> refs)
        {
            CollectStructWithPlace(evt, refs);

            CollectStructWithNotes(evt, refs);

            if (evt.HasMultimediaLinks) {
                for (int i = 0; i < evt.MultimediaLinks.Count; i++) {
                    CollectRef(evt.MultimediaLinks[i].XRef, refs);
                }
            }

            CollectStructWithSourceCitations(evt, refs);
        }

        internal static void CollectRefsFromIndividual(GDMIndividualRecord individual, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(individual, refs);

            if (individual.HasEvents) {
                for (int i = 0; i < individual.Events.Count; i++) {
                    CollectRefsFromEvent(individual.Events[i], refs);
                }
            }

            for (int i = 0; i < individual.ChildToFamilyLinks.Count; i++) {
                CollectRefsFromChildToFamilyLink(individual.ChildToFamilyLinks[i], refs);
            }

            for (int i = 0; i < individual.SpouseToFamilyLinks.Count; i++) {
                CollectRefsFromSpouseToFamilyLink(individual.SpouseToFamilyLinks[i], refs);
            }

            if (individual.HasAssociations) {
                for (int i = 0; i < individual.Associations.Count; i++) {
                    CollectRefsFromAssociation(individual.Associations[i], refs);
                }
            }

            if (individual.HasGroups) {
                for (int i = 0; i < individual.Groups.Count; i++) {
                    CollectRef(individual.Groups[i].XRef, refs);
                }
            }

            for (int i = 0; i < individual.PersonalNames.Count; i++) {
                CollectRefsFromPersonalName(individual.PersonalNames[i], refs);
            }
        }

        internal static void CollectRefsFromSpouseToFamilyLink(GDMSpouseToFamilyLink link, HashSet<string> refs)
        {
            CollectRef(link.XRef, refs);
            CollectStructWithNotes(link, refs);
        }

        internal static void CollectRefsFromAssociation(GDMAssociation assoc, HashSet<string> refs)
        {
            CollectRef(assoc.XRef, refs);
            CollectStructWithNotes(assoc, refs);
            CollectStructWithSourceCitations(assoc, refs);
        }

        internal static void CollectRefsFromPersonalName(GDMPersonalName name, HashSet<string> refs)
        {
            CollectStructWithNotes(name, refs);
            CollectStructWithSourceCitations(name, refs);
        }

        internal static void CollectRefsFromChildToFamilyLink(GDMChildToFamilyLink link, HashSet<string> refs)
        {
            CollectRef(link.XRef, refs);
            CollectStructWithNotes(link, refs);
        }

        internal static void CollectRefsFromFamily(GDMFamilyRecord family, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(family, refs);

            if (family.HasEvents) {
                for (int i = 0; i < family.Events.Count; i++) {
                    CollectRefsFromEvent(family.Events[i], refs);
                }
            }

            CollectRef(family.Husband.XRef, refs);
            CollectRef(family.Wife.XRef, refs);

            for (int i = 0; i < family.Children.Count; i++) {
                CollectRef(family.Children[i].XRef, refs);
            }
        }

        internal static void CollectRefsFromNote(GDMNoteRecord note, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(note, refs);
        }

        internal static void CollectRefsFromMultimedia(GDMMultimediaRecord media, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(media, refs);
        }

        internal static void CollectRefsFromSource(GDMSourceRecord source, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(source, refs);

            CollectStructWithNotes(source.Data, refs);
            for (int i = 0; i < source.Data.Events.Count; i++) {
                CollectStructWithPlace(source.Data.Events[i], refs);
            }

            for (int i = 0; i < source.RepositoryCitations.Count; i++) {
                CollectRefsFromRepositoryCitation(source.RepositoryCitations[i], refs);
            }
        }

        internal static void CollectRefsFromRepositoryCitation(GDMRepositoryCitation repCit, HashSet<string> refs)
        {
            CollectRef(repCit.XRef, refs);
            CollectStructWithNotes(repCit, refs);
        }

        internal static void CollectRefsFromRepository(GDMRepositoryRecord repository, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(repository, refs);
        }

        internal static void CollectRefsFromGroup(GDMGroupRecord group, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(group, refs);

            for (int i = 0; i < group.Members.Count; i++) {
                CollectRef(group.Members[i].XRef, refs);
            }
        }

        internal static void CollectRefsFromResearch(GDMResearchRecord research, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(research, refs);

            for (int i = 0; i < research.Tasks.Count; i++) {
                CollectRef(research.Tasks[i].XRef, refs);
            }

            for (int i = 0; i < research.Communications.Count; i++) {
                CollectRef(research.Communications[i].XRef, refs);
            }

            for (int i = 0; i < research.Groups.Count; i++) {
                CollectRef(research.Groups[i].XRef, refs);
            }
        }

        internal static void CollectRefsFromTask(GDMTaskRecord task, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(task, refs);

            if (GEDCOMUtils.IsXRef(task.Goal))
                CollectRef(task.Goal, refs);
        }

        internal static void CollectRefsFromCommunication(GDMCommunicationRecord communication, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(communication, refs);

            CollectRef(communication.Corresponder.XRef, refs);
        }

        internal static void CollectRefsFromLocation(GDMLocationRecord location, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(location, refs);

            for (int i = 0; i < location.TopLevels.Count; i++) {
                CollectRef(location.TopLevels[i].XRef, refs);
            }
        }

        internal static void CollectRefsFromSubmission(GDMSubmissionRecord submission, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(submission, refs);

            CollectRef(submission.Submitter.XRef, refs);
        }

        internal static void CollectRefsFromSubmitter(GDMSubmitterRecord submitter, HashSet<string> refs)
        {
            CollectRefsFromBaseRecord(submitter, refs);
        }

        internal static void CollectRef(string xRef, HashSet<string> refs)
        {
            if (!string.IsNullOrEmpty(xRef))
                refs.Add(xRef);
        }

        private static void CollectStructWithSourceCitations(IGDMStructWithSourceCitations swsc, HashSet<string> refs)
        {
            if (swsc.HasSourceCitations) {
                for (int i = 0; i < swsc.SourceCitations.Count; i++) {
                    CollectRef(swsc.SourceCitations[i].XRef, refs);
                }
            }
        }

        private static void CollectStructWithPlace(IGDMStructWithPlace swp, HashSet<string> refs)
        {
            if (swp.HasPlace)
                CollectRef(swp.Place.Location.XRef, refs);
        }

        private static void CollectStructWithNotes(IGDMStructWithNotes swn, HashSet<string> refs)
        {
            if (swn.HasNotes) {
                for (int i = 0; i < swn.Notes.Count; i++) {
                    CollectRef(swn.Notes[i].XRef, refs);
                }
            }
        }
    }


    internal class GDMTagReferenceVerifier : IGDMObjectVisitor
    {
        private readonly HashSet<string> fResult = new HashSet<string>();

        public HashSet<string> Result { get { return fResult; } }

        public GDMTagReferenceVerifier()
        {
        }

        public void Visit(GDMPersonalName obj)
        {
            GDMReferenceVerifier.CollectRefsFromPersonalName(obj, fResult);
        }

        public void Visit(GDMChildToFamilyLink obj)
        {
            GDMReferenceVerifier.CollectRefsFromChildToFamilyLink(obj, fResult);
        }

        public void Visit(GDMTag obj)
        {
            // empty
        }

        public void Visit(GDMCustomEvent obj)
        {
            GDMReferenceVerifier.CollectRefsFromEvent(obj, fResult);
        }

        public void Visit(GDMIndividualRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromIndividual(obj, fResult);
        }

        public void Visit(GDMSpouseToFamilyLink obj)
        {
            GDMReferenceVerifier.CollectRefsFromSpouseToFamilyLink(obj, fResult);
        }

        public void Visit(GDMAssociation obj)
        {
            GDMReferenceVerifier.CollectRefsFromAssociation(obj, fResult);
        }

        public void Visit(GDMFamilyRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromFamily(obj, fResult);
        }

        public void Visit(GDMNoteRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromNote(obj, fResult);
        }

        public void Visit(GDMMultimediaRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromMultimedia(obj, fResult);
        }

        public void Visit(GDMSourceRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromSource(obj, fResult);
        }

        public void Visit(GDMRepositoryRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromRepository(obj, fResult);
        }

        public void Visit(GDMGroupRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromGroup(obj, fResult);
        }

        public void Visit(GDMResearchRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromResearch(obj, fResult);
        }

        public void Visit(GDMTaskRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromTask(obj, fResult);
        }

        public void Visit(GDMCommunicationRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromCommunication(obj, fResult);
        }

        public void Visit(GDMLocationRecord obj)
        {
            GDMReferenceVerifier.CollectRefsFromLocation(obj, fResult);
        }

        public void Visit(GDMPointer obj)
        {
            GDMReferenceVerifier.CollectRef(obj.XRef, fResult);
        }

        public void Visit(GDMRepositoryCitation obj)
        {
            GDMReferenceVerifier.CollectRefsFromRepositoryCitation(obj, fResult);
        }
    }
}
