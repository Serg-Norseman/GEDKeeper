/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using GDModel;
using GKCore.Design;
using GKCore.Lists;
using GKCore.Utilities;

namespace GKCore.Search
{
    public struct FARParameters
    {
        public string Pattern;
        public string Replacement;
        public bool MatchCase;
        public bool MatchWildcards;
        public bool WholeWord;
        public GDMRecordType RecordType;
        public FARPropertyType PropertyType;
    }


    public delegate void FARReplacer(IGDMObject prop);


    public class FARSearchResult : SearchResult
    {
        public readonly IGDMObject Property;
        public readonly FARReplacer Replacer;

        public FARSearchResult(GDMRecord record, IGDMObject property, FARReplacer replacer) : base(record)
        {
            Property = property;
            Replacer = replacer;
        }
    }


    public class FARStrategy : SearchStrategy
    {
        private readonly IBaseWindow fBaseWindow;
        private FARParameters fParameters;

        // runtime
        private Regex fPatternRegex;
        private StringComparison fStrComparison;


        public FARStrategy(IBaseWindow baseWindow, FARParameters parameters)
        {
            if (string.IsNullOrEmpty(parameters.Pattern))
                throw new ArgumentNullException("searchPattern");

            fBaseWindow = baseWindow;
            fParameters = parameters;
            fCurrentResults = FindAll();
        }

        private bool FindPattern(string str)
        {
            if (!string.IsNullOrEmpty(str)) {
                if (fPatternRegex != null) {
                    return fPatternRegex.IsMatch(str, 0);
                } else {
                    return str.IndexOf(fParameters.Pattern, fStrComparison) >= 0;
                }
            }
            return false;
        }

        private string ReplacePattern(string str)
        {
            if (!string.IsNullOrEmpty(str)) {
                if (fPatternRegex != null) {
                    return fPatternRegex.Replace(str, fParameters.Replacement);
                } else {
                    return str.Replace(fParameters.Pattern, fParameters.Replacement, fStrComparison);
                }
            }
            return string.Empty;
        }

        public override IList<ISearchResult> FindAll()
        {
            List<ISearchResult> result = new List<ISearchResult>();

            fPatternRegex = (!fParameters.MatchWildcards) ? null : GKUtils.InitMaskRegex(fParameters.Pattern, !fParameters.MatchCase);
            fStrComparison = (fParameters.MatchCase) ? StringComparison.CurrentCulture : StringComparison.CurrentCultureIgnoreCase;

            var tree = fBaseWindow.Context.Tree;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];
                if (rec.RecordType != fParameters.RecordType) continue;

                switch (rec.RecordType) {
                    case GDMRecordType.rtIndividual:
                        var indiRec = rec as GDMIndividualRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindPersNamePattern(result, indiRec);
                                break;

                            case FARPropertyType.ptPlace:
                                FindPlacePattern(result, indiRec);
                                break;

                            case FARPropertyType.ptFact:
                                FindFactPattern(result, indiRec);
                                break;

                            case FARPropertyType.ptAssociation:
                                FindAssociationPattern(result, indiRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtFamily:
                        var famRec = rec as GDMFamilyRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptPlace:
                                FindPlacePattern(result, famRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtNote:
                        var noteRec = rec as GDMNoteRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptText:
                                FindNoteTextPattern(result, noteRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtMultimedia:
                        var mediaRec = rec as GDMMultimediaRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindMediaNamePattern(result, mediaRec);
                                break;

                            case FARPropertyType.ptFilePath:
                                FindMediaFilePathPattern(result, mediaRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtSource:
                        var sourRec = rec as GDMSourceRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindSourceNamePattern(result, sourRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtRepository:
                        var repoRec = rec as GDMRepositoryRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindRepoNamePattern(result, repoRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtGroup:
                        var groupRec = rec as GDMGroupRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindGroupNamePattern(result, groupRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtResearch:
                        var resRec = rec as GDMResearchRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindResearchNamePattern(result, resRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtTask:
                        var taskRec = rec as GDMTaskRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptGoal:
                                FindTaskGoalPattern(result, taskRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtCommunication:
                        var commRec = rec as GDMCommunicationRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptTheme:
                                FindCommThemePattern(result, commRec);
                                break;
                        }
                        break;

                    case GDMRecordType.rtLocation:
                        var locRec = rec as GDMLocationRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindLocationNamePattern(result, locRec);
                                break;
                        }
                        break;

                    default:
                        break;
                }
            }

            return result;
        }

        #region Handlers

        private void FindPersNamePattern(List<ISearchResult> result, GDMIndividualRecord indiRec)
        {
            for (int k = 0; k < indiRec.PersonalNames.Count; k++) {
                var persName = indiRec.PersonalNames[k];
                if (FindPattern(persName.Given)) {
                    result.Add(new FARSearchResult(indiRec, persName, ReplaceGivenName));
                } else if (FindPattern(persName.Surname)) {
                    result.Add(new FARSearchResult(indiRec, persName, ReplaceSurname));
                }
            }
        }

        private void ReplaceGivenName(IGDMObject prop)
        {
            var persName = (GDMPersonalName)prop;
            persName.Given = ReplacePattern(persName.Given);
        }

        private void ReplaceSurname(IGDMObject prop)
        {
            var persName = (GDMPersonalName)prop;
            persName.Surname = ReplacePattern(persName.Surname);
        }

        private void FindAssociationPattern(List<ISearchResult> result, GDMIndividualRecord indiRec)
        {
            if (!indiRec.HasAssociations) return;

            for (int k = 0; k < indiRec.Associations.Count; k++) {
                var ast = indiRec.Associations[k];
                if (FindPattern(ast.Relation)) {
                    result.Add(new FARSearchResult(indiRec, ast, ReplaceAssociationRelation));
                }
            }
        }

        private void ReplaceAssociationRelation(IGDMObject prop)
        {
            var ast = (GDMAssociation)prop;
            ast.Relation = ReplacePattern(ast.Relation);
        }

        private void FindFactPattern(List<ISearchResult> result, GDMIndividualRecord indiRec)
        {
            if (!indiRec.HasEvents) return;

            for (int k = 0; k < indiRec.Events.Count; k++) {
                var evt = indiRec.Events[k];
                if (FindPattern(evt.StringValue)) {
                    result.Add(new FARSearchResult(indiRec, evt, ReplaceFact));
                }
            }
        }

        private void ReplaceFact(IGDMObject prop)
        {
            var evt = (GDMCustomEvent)prop;
            evt.StringValue = ReplacePattern(evt.StringValue);
        }

        private void FindPlacePattern(List<ISearchResult> result, GDMRecordWithEvents rwe)
        {
            if (!rwe.HasEvents) return;

            for (int k = 0; k < rwe.Events.Count; k++) {
                var evt = rwe.Events[k];
                if (FindPattern(evt.Place.StringValue)) {
                    result.Add(new FARSearchResult(rwe, evt, ReplacePlace));
                }
            }
        }

        private void ReplacePlace(IGDMObject prop)
        {
            var evt = (GDMCustomEvent)prop;
            evt.Place.StringValue = ReplacePattern(evt.Place.StringValue);
        }

        private void FindMediaNamePattern(List<ISearchResult> result, GDMMultimediaRecord mediaRec)
        {
            for (int k = 0; k < mediaRec.FileReferences.Count; k++) {
                var fileRef = mediaRec.FileReferences[k];
                if (FindPattern(fileRef.Title)) {
                    result.Add(new FARSearchResult(mediaRec, fileRef, ReplaceMediaName));
                }
            }
        }
        private void ReplaceMediaName(IGDMObject prop)
        {
            var fileRef = (GDMFileReferenceWithTitle)prop;
            fileRef.Title = ReplacePattern(fileRef.Title);
        }

        private void FindMediaFilePathPattern(List<ISearchResult> result, GDMMultimediaRecord mediaRec)
        {
            for (int k = 0; k < mediaRec.FileReferences.Count; k++) {
                var fileRef = mediaRec.FileReferences[k];
                if (FindPattern(fileRef.StringValue)) {
                    result.Add(new FARSearchResult(mediaRec, fileRef, ReplaceMediaFilePath));
                }
            }
        }
        private void ReplaceMediaFilePath(IGDMObject prop)
        {
            var fileRef = (GDMFileReferenceWithTitle)prop;
            fileRef.StringValue = ReplacePattern(fileRef.StringValue);
        }


        private void FindGroupNamePattern(List<ISearchResult> result, GDMGroupRecord groupRec)
        {
            if (FindPattern(groupRec.GroupName)) {
                result.Add(new FARSearchResult(groupRec, groupRec, ReplaceGroupName));
            }
        }

        private void ReplaceGroupName(IGDMObject prop)
        {
            var groupRec = (GDMGroupRecord)prop;
            groupRec.GroupName = ReplacePattern(groupRec.GroupName);
        }

        private void FindRepoNamePattern(List<ISearchResult> result, GDMRepositoryRecord repoRec)
        {
            if (FindPattern(repoRec.RepositoryName)) {
                result.Add(new FARSearchResult(repoRec, repoRec, ReplaceRepoName));
            }
        }

        private void ReplaceRepoName(IGDMObject prop)
        {
            var repoRec = (GDMRepositoryRecord)prop;
            repoRec.RepositoryName = ReplacePattern(repoRec.RepositoryName);
        }

        private void FindSourceNamePattern(List<ISearchResult> result, GDMSourceRecord sourRec)
        {
            if (FindPattern(sourRec.ShortTitle)) {
                result.Add(new FARSearchResult(sourRec, sourRec, ReplaceSourceName));
            }
        }

        private void ReplaceSourceName(IGDMObject prop)
        {
            var sourRec = (GDMSourceRecord)prop;
            sourRec.ShortTitle = ReplacePattern(sourRec.ShortTitle);
        }

        private void FindResearchNamePattern(List<ISearchResult> result, GDMResearchRecord resRec)
        {
            if (FindPattern(resRec.ResearchName)) {
                result.Add(new FARSearchResult(resRec, resRec, ReplaceResearchName));
            }
        }

        private void ReplaceResearchName(IGDMObject prop)
        {
            var resRec = (GDMResearchRecord)prop;
            resRec.ResearchName = ReplacePattern(resRec.ResearchName);
        }

        private void FindLocationNamePattern(List<ISearchResult> result, GDMLocationRecord locRec)
        {
            for (int k = 0; k < locRec.Names.Count; k++) {
                var locName = locRec.Names[k];
                if (FindPattern(locName.StringValue)) {
                    result.Add(new FARSearchResult(locRec, locName, ReplaceLocationName));
                }
            }
        }

        private void ReplaceLocationName(IGDMObject prop)
        {
            var locName = (GDMLocationName)prop;
            locName.StringValue = ReplacePattern(locName.StringValue);
        }

        private void FindNoteTextPattern(List<ISearchResult> result, GDMNoteRecord noteRec)
        {
            if (FindPattern(noteRec.Lines.Text)) {
                result.Add(new FARSearchResult(noteRec, noteRec, ReplaceNoteText));
            }
        }

        private void ReplaceNoteText(IGDMObject prop)
        {
            var noteRec = (GDMNoteRecord)prop;
            noteRec.Lines.Text = ReplacePattern(noteRec.Lines.Text);
        }

        private void FindTaskGoalPattern(List<ISearchResult> result, GDMTaskRecord taskRec)
        {
            if (FindPattern(taskRec.Goal)) {
                result.Add(new FARSearchResult(taskRec, taskRec, ReplaceTaskGoal));
            }
        }

        private void ReplaceTaskGoal(IGDMObject prop)
        {
            var taskRec = (GDMTaskRecord)prop;
            taskRec.Goal = ReplacePattern(taskRec.Goal);
        }

        private void FindCommThemePattern(List<ISearchResult> result, GDMCommunicationRecord commRec)
        {
            if (FindPattern(commRec.CommName)) {
                result.Add(new FARSearchResult(commRec, commRec, ReplaceCommTheme));
            }
        }

        private void ReplaceCommTheme(IGDMObject prop)
        {
            var commRec = (GDMCommunicationRecord)prop;
            commRec.CommName = ReplacePattern(commRec.CommName);
        }

        #endregion

        private void Replace(ISearchResult res)
        {
            if (res is FARSearchResult farResult) {
                farResult.Replacer(farResult.Property);
                fBaseWindow.NotifyRecord(farResult.Record, RecordAction.raEdit);
                fBaseWindow.UpdateChangedRecords(farResult.Record);
                // TODO: remove item from result's list
            }
        }

        public void ReplaceCurrent()
        {
            if (CurResult != null) {
                Replace(CurResult);
            }
        }

        public void ReplaceAll()
        {
            if (!HasResults()) return;

            while (FindNext() != null) {
                Replace(CurResult);
            }
        }
    }
}
