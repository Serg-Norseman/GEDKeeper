/*
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
