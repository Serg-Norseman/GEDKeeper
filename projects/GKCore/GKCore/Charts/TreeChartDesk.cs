/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;

namespace GKCore.Charts
{
    internal class TreeChartDesk
    {
        private const int SIZE = 255; // -127..+127, 0..254, 255, 1 unused

        private LinkedList<TreeChartPerson>[] fMatrix;

        public TreeChartDesk()
        {
            Reset();
        }

        public void Clear()
        {
            if (fMatrix != null) {
                for (int i = 0; i < SIZE; i++) {
                    fMatrix[i] = null;
                }
                fMatrix = null;
            }
        }

        public void Reset()
        {
            Clear();

            fMatrix = new LinkedList<TreeChartPerson>[SIZE];
            for (int i = 0; i < SIZE; i++) {
                fMatrix[i] = new LinkedList<TreeChartPerson>();
            }
        }

        public LinkedList<TreeChartPerson> GetRow(TreeChartPerson person)
        {
            int generation = person.Generation;
            if (generation < -127 || generation > 127)
                throw new ArgumentOutOfRangeException(nameof(generation));

            int index = generation + 127;
            return fMatrix[index];
        }

        public TreeChartPerson GetBefore(TreeChartPerson person)
        {
            var row = GetRow(person);
            var node = row.Find(person);
            return (node == null || node.Previous == null) ? null : node.Previous.Value;
        }

        public TreeChartPerson GetAfter(TreeChartPerson person)
        {
            var row = GetRow(person);
            var node = row.Find(person);
            return (node == null || node.Next == null) ? null : node.Next.Value;
        }

        public bool Exists(TreeChartPerson person)
        {
            var row = GetRow(person);
            var node = row.Find(person);
            return (node != null);
        }

        public void Add(TreeChartPerson person)
        {
            var row = GetRow(person);
            row.AddLast(person);
        }

        public void AddBefore(TreeChartPerson subject, TreeChartPerson person)
        {
            var row = GetRow(person);
            row.AddBefore(row.Find(subject), person);
        }

        public void AddAfter(TreeChartPerson subject, TreeChartPerson person)
        {
            var row = GetRow(person);
            row.AddAfter(row.Find(subject), person);
        }
    }
}
