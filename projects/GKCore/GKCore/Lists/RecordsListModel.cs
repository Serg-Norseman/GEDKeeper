﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using System.Text.RegularExpressions;
using BSLib;
using BSLib.Design.Graphics;
using GDModel;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Search;
using BSDColors = BSLib.Design.BSDConsts.Colors;

namespace GKCore.Lists
{
    /// <summary>
    ///
    /// </summary>
    public abstract class RecordsListModel<T> : ListSource<T>, IRecordsListModel<T>
        where T : GDMRecord
    {
        private readonly GDMRecordType fRecordType;


        public GDMRecordType RecordType
        {
            get { return fRecordType; }
        }


        protected RecordsListModel(IBaseContext baseContext, ListColumns<T> defaultListColumns, GDMRecordType recordType) :
            base(baseContext, defaultListColumns)
        {
            fRecordType = recordType;
        }

        public override IColor GetBackgroundColor(int itemIndex, object rowData)
        {
            if (GlobalOptions.Instance.ReadabilityHighlightRows && MathHelper.IsOdd(itemIndex)) {
                return ChartRenderer.GetColor(BSDColors.LightGray);
            }

            return null;
        }

        public override void UpdateContents()
        {
            int contentSize = fBaseContext.Tree.RecordsCount;
            InitContent(contentSize);

            for (int i = 0; i < contentSize; i++) {
                GDMRecord rec = fBaseContext.Tree[i];
                if (rec.RecordType == fRecordType) {
                    var tRec = (T)rec;
                    AddFilteredContent(tRec);
                }
            }

            DoneContent();
        }

        public List<GDMRecord> GetRecordsList()
        {
            int size = ContentList.Count;
            var result = new List<GDMRecord>(size);

            for (int i = 0; i < size; i++) {
                result.Add((GDMRecord)ContentList[i].Record);
            }

            return result;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            List<ISearchResult> result = new List<ISearchResult>();

            Regex regex = GKUtils.InitMaskRegex(searchPattern);

            int num = ContentList.Count;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = (GDMRecord)ContentList[i].Record;

                string recName = GKUtils.GetRecordName(fBaseContext.Tree, rec, false);
                if (GKUtils.MatchesRegex(recName, regex)) {
                    result.Add(new SearchResult(rec));
                }
            }

            return result;
        }

        public static IRecordsListModel Create(IBaseContext baseContext, GDMRecordType recType)
        {
            IRecordsListModel result = null;

            switch (recType) {
                case GDMRecordType.rtIndividual:
                    result = new IndividualListModel(baseContext);
                    break;

                case GDMRecordType.rtFamily:
                    result = new FamilyListModel(baseContext);
                    break;

                case GDMRecordType.rtNote:
                    result = new NoteListModel(baseContext);
                    break;

                case GDMRecordType.rtMultimedia:
                    result = new MultimediaListModel(baseContext);
                    break;

                case GDMRecordType.rtSource:
                    result = new SourceListModel(baseContext);
                    break;

                case GDMRecordType.rtRepository:
                    result = new RepositoryListModel(baseContext);
                    break;

                case GDMRecordType.rtGroup:
                    result = new GroupListModel(baseContext);
                    break;

                case GDMRecordType.rtResearch:
                    result = new ResearchListModel(baseContext);
                    break;

                case GDMRecordType.rtTask:
                    result = new TaskListModel(baseContext);
                    break;

                case GDMRecordType.rtCommunication:
                    result = new CommunicationListModel(baseContext);
                    break;

                case GDMRecordType.rtLocation:
                    result = new LocationListModel(baseContext);
                    break;

                case GDMRecordType.rtSubmission:
                    result = null;
                    break;

                case GDMRecordType.rtSubmitter:
                    result = null;
                    break;
            }

            return result;
        }
    }
}
