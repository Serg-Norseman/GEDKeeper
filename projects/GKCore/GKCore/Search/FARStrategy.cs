/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using GDModel;
using GKCore.Interfaces;

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


    public class FARStrategy : BaseSearchStrategy
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

                // TODO: only individual names yet!

                switch (rec.RecordType) {
                    case GDMRecordType.rtIndividual:
                        var indiRec = rec as GDMIndividualRecord;
                        switch (fParameters.PropertyType) {
                            case FARPropertyType.ptName:
                                FindNamePattern(result, indiRec);
                                break;

                            case FARPropertyType.ptFact:
                                FindFactPattern(result, indiRec);
                                break;

                            case FARPropertyType.ptAssociation:
                                FindAssociationPattern(result, indiRec);
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

        private void FindNamePattern(List<ISearchResult> result, GDMIndividualRecord indiRec)
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

        #endregion

        private void Replace(ISearchResult res)
        {
            if (res != null) {
                var farResult = (FARSearchResult)res;
                farResult.Replacer(farResult.Property);
                fBaseWindow.NotifyRecord(farResult.Record, Types.RecordAction.raEdit);
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
