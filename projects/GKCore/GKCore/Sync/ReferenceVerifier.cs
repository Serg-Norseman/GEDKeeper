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

namespace GKCore.Sync
{
    /// <summary>
    /// Class for verifying references and dependencies between GDM records in different trees.
    /// </summary>
    public class ReferenceVerifier
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
        /// Verifies references for a given GDM record recursively.
        /// </summary>
        public List<RefInfo> VerifyReferences(GDMRecord record, GDMTree sourceTree, GDMTree destinationTree)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));
            if (sourceTree == null)
                throw new ArgumentNullException(nameof(sourceTree));
            if (destinationTree == null)
                throw new ArgumentNullException(nameof(destinationTree));

            var refInfos = new List<RefInfo>();
            var processedRefs = new HashSet<string>();

            VerifyReferencesRecursive(record, sourceTree, destinationTree, refInfos, processedRefs);

            return refInfos;
        }

        /// <summary>
        /// Recursively verifies references for a given GDM record and all its dependencies.
        /// </summary>
        /// <param name="record">The GDM record to verify.</param>
        /// <param name="sourceTree">The tree to which the record belongs.</param>
        /// <param name="destinationTree">The tree to check for ref presence.</param>
        /// <param name="refInfos">The list to populate with RefInfo objects.</param>
        /// <param name="processedRefs">Set of already processed refs to avoid infinite recursion.</param>
        private void VerifyReferencesRecursive(GDMRecord record, GDMTree sourceTree, GDMTree destinationTree, List<RefInfo> refInfos, HashSet<string> processedRefs)
        {
            if (record == null || processedRefs.Contains(record.XRef))
                return;

            // Mark this record as processed
            processedRefs.Add(record.XRef);

            // Check if this record exists in the destination tree
            var status = destinationTree.FindXRef<GDMRecord>(record.XRef) != null ? RefStatus.Present : RefStatus.Missing;
            refInfos.Add(new RefInfo(record.XRef, status));

            // If the record is missing in the destination tree, we still need to check its dependencies
            // Get all links from the record and its substructures
            var refs = CollectRefs(record, sourceTree);

            // Check each link against the destination tree and recursively verify dependencies
            foreach (var link in refs) {
                if (!processedRefs.Contains(link)) {
                    var linkedRecord = sourceTree.FindXRef<GDMRecord>(link);
                    if (linkedRecord != null) {
                        VerifyReferencesRecursive(linkedRecord, sourceTree, destinationTree, refInfos, processedRefs);
                    } else {
                        // If we can't find the record in the source tree, just add it to the list with missing status
                        refInfos.Add(new RefInfo(link, RefStatus.Missing));
                    }
                }
            }
        }

        public static HashSet<string> VerifyTagReferences(GDMTree targetTree, GDMTag obj)
        {
            var result = new HashSet<string>();
            if (obj != null) {
                var visitor = new GDMTagReferenceVerifier();
                obj.Accept(visitor);
                foreach (var xRef in visitor.Result) {
                    var refRecord = targetTree.FindXRef<GDMRecord>(xRef);
                    if (refRecord == null)
                        result.Add(xRef);
                }
            }
            return result;
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
            ReferenceVerifier.CollectRefsFromPersonalName(obj, fResult);
        }

        public void Visit(GDMChildToFamilyLink obj)
        {
            ReferenceVerifier.CollectRefsFromChildToFamilyLink(obj, fResult);
        }

        public void Visit(GDMTag obj)
        {
            // empty
        }

        public void Visit(GDMCustomEvent obj)
        {
            ReferenceVerifier.CollectRefsFromEvent(obj, fResult);
        }

        public void Visit(GDMIndividualRecord obj)
        {
            ReferenceVerifier.CollectRefsFromIndividual(obj, fResult);
        }

        public void Visit(GDMSpouseToFamilyLink obj)
        {
            ReferenceVerifier.CollectRefsFromSpouseToFamilyLink(obj, fResult);
        }

        public void Visit(GDMAssociation obj)
        {
            ReferenceVerifier.CollectRefsFromAssociation(obj, fResult);
        }

        public void Visit(GDMFamilyRecord obj)
        {
            ReferenceVerifier.CollectRefsFromFamily(obj, fResult);
        }

        public void Visit(GDMNoteRecord obj)
        {
            ReferenceVerifier.CollectRefsFromNote(obj, fResult);
        }

        public void Visit(GDMMultimediaRecord obj)
        {
            ReferenceVerifier.CollectRefsFromMultimedia(obj, fResult);
        }

        public void Visit(GDMSourceRecord obj)
        {
            ReferenceVerifier.CollectRefsFromSource(obj, fResult);
        }

        public void Visit(GDMRepositoryRecord obj)
        {
            ReferenceVerifier.CollectRefsFromRepository(obj, fResult);
        }

        public void Visit(GDMGroupRecord obj)
        {
            ReferenceVerifier.CollectRefsFromGroup(obj, fResult);
        }

        public void Visit(GDMResearchRecord obj)
        {
            ReferenceVerifier.CollectRefsFromResearch(obj, fResult);
        }

        public void Visit(GDMTaskRecord obj)
        {
            ReferenceVerifier.CollectRefsFromTask(obj, fResult);
        }

        public void Visit(GDMCommunicationRecord obj)
        {
            ReferenceVerifier.CollectRefsFromCommunication(obj, fResult);
        }

        public void Visit(GDMLocationRecord obj)
        {
            ReferenceVerifier.CollectRefsFromLocation(obj, fResult);
        }

        public void Visit(GDMPointer obj)
        {
            ReferenceVerifier.CollectRef(obj.XRef, fResult);
        }

        public void Visit(GDMRepositoryCitation obj)
        {
            ReferenceVerifier.CollectRefsFromRepositoryCitation(obj, fResult);
        }
    }
}
