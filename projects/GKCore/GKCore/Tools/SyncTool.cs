/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Utilities;

namespace GKCore.Tools
{
    public sealed class DiffRecord : DiffResult<GDMRecord>
    {
        public bool Checked { get; set; }

        public DiffRecord(GDMRecord obj1, GDMRecord obj2, DiffStatus status) : base(obj1, obj2, status)
        {
        }
    }


    /// <summary>
    ///
    /// </summary>
    public class SyncTool
    {
        private GDMTree fMainTree;
        private GDMTree fOtherTree;

        public List<DiffRecord> Results;

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

        public void CompareRecords(GDMRecordType recordType)
        {
            var records1 = fMainTree.GetRecords(recordType);
            var records2 = fOtherTree.GetRecords(recordType);

            var option = new DiffOption<GDMRecord>();
            option.EqualityComparer = new Stage1Comparer();

            Results = new List<DiffRecord>();

            var diffResults = DiffUtil.Diff(records1, records2, option);
            foreach (var diff in diffResults) {
                var diffRec = new DiffRecord(diff.Obj1, diff.Obj2, diff.Status);
                Results.Add(diffRec);
            }

            CheckModified();
            CheckContents();
        }

        private void CheckModified()
        {
            foreach (var diffRes in Results) {
                if (diffRes.Status != DiffStatus.Equal) continue;

                var rec1 = diffRes.Obj1;
                var rec2 = diffRes.Obj2;

                if ((rec1.XRef != rec2.XRef) || (rec1.ChangeDate.ChangeDateTime != rec2.ChangeDate.ChangeDateTime)) {
                    diffRes.Status = DiffStatus.Modified;
                }

                if (rec1.GetHashCode() != rec2.GetHashCode()) {
                    diffRes.Status = DiffStatus.DeepModified;
                }
            }
        }

        private void CheckContents()
        {
            // IEquatable<T> and GetHashCode() - on all records, structures and tags
        }

        internal class Stage1Comparer : IEqualityComparer<GDMRecord>
        {
            public bool Equals(GDMRecord x, GDMRecord y)
            {
                return x.UID == y.UID;
            }

            public int GetHashCode(GDMRecord obj)
            {
                return obj.GetHashCode();
            }
        }
    }
}
