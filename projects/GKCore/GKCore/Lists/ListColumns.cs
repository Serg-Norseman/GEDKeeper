/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Globalization;

using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ListColumn
    {
        public readonly byte Id;
        public readonly LSID ColName;
        public readonly DataType DataType;
        public readonly int DefWidth;
        public readonly bool DefActive;
        public readonly string Format;
        public readonly NumberFormatInfo NumFmt;
        public readonly bool Autosize;

        public int CurWidth;
        public bool CurActive;
        public int Order;

        public ListColumn(byte id, LSID colName, DataType dataType,
                          int defWidth, bool defActive, bool autosize = false,
                          string format = null, NumberFormatInfo numFmt = null)
        {
            Id = id;
            ColName = colName;
            DataType = dataType;
            DefWidth = defWidth;
            DefActive = defActive;
            Format = format;
            NumFmt = numFmt;
            Autosize = autosize;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class ListColumns : IListColumns
    {
        private readonly List<ListColumn> fColumns;
        private readonly List<ListColumn> fOrderedColumns;
        private sbyte fLastId;


        public int Count
        {
            get { return fColumns.Count; }
        }

        public ListColumn this[int index]
        {
            get { return fColumns[index]; }
        }

        public IList<ListColumn> OrderedColumns
        {
            get { return fOrderedColumns; }
        }


        public ListColumns()
        {
            fLastId = -1;
            fColumns = new List<ListColumn>();
            fOrderedColumns = new List<ListColumn>();

            InitColumnStatics();
            ResetDefaults();
        }

        protected virtual void InitColumnStatics()
        {
            // dummy
        }

        public void AddColumn(LSID colName, DataType dataType,
                              int defWidth, bool defActive,
                              bool autosize = false)
        {
            AddColumn(colName, dataType, defWidth, defActive, autosize, null, null);
        }

        public void AddColumn(LSID colName, DataType dataType,
                              int defWidth, bool defActive, bool autosize,
                              string format, NumberFormatInfo nfi)
        {
            fLastId += 1;
            ListColumn cs = new ListColumn((byte)fLastId, colName, dataType,
                                           defWidth, defActive, autosize, format, nfi);
            fColumns.Add(cs);
        }

        public void ResetDefaults()
        {
            int num = fColumns.Count;
            for (int i = 0; i < num; i++)
            {
                var cs = fColumns[i];

                cs.Order = i;
                cs.CurActive = cs.DefActive;
                cs.CurWidth = cs.DefWidth;
            }

            UpdateOrders();
        }

        public void CopyTo(IListColumns target)
        {
            ListColumns targetColumns = target as ListColumns;
            if (targetColumns == null)
                throw new ArgumentNullException("target");

            int num = fColumns.Count;
            for (int i = 0; i < num; i++)
            {
                var srcCol = fColumns[i];
                var tgtCol = targetColumns.fColumns[i];

                tgtCol.Order = srcCol.Order;
                tgtCol.CurActive = srcCol.CurActive;
                tgtCol.CurWidth = srcCol.CurWidth;
            }

            targetColumns.UpdateOrders();
        }

        public void LoadFromFile(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            int num = fColumns.Count;
            for (int i = 0; i < num; i++)
            {
                int colId = iniFile.ReadInteger(section, "ColType_" + i, i);

                ListColumn col = fColumns[colId];

                bool colActive = iniFile.ReadBool(section, "ColActive_" + i, col.DefActive);
                int colWidth = iniFile.ReadInteger(section, "ColWidth_" + i, col.DefWidth);

                // protection zero/hidden columns
                if (colWidth <= 10) {
                    colWidth = col.DefWidth;
                }

                col.Order = i;
                col.CurActive = colActive;
                col.CurWidth = colWidth;
            }

            UpdateOrders();
        }

        public void SaveToFile(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            int idx = -1;
            foreach (var col in fOrderedColumns)
            {
                idx += 1;
                iniFile.WriteInteger(section, "ColType_" + idx, col.Id);
                iniFile.WriteBool(section, "ColActive_" + idx, col.CurActive);
                iniFile.WriteInteger(section, "ColWidth_" + idx, col.CurWidth);
            }
        }

        private static int CompareItems(ListColumn item1, ListColumn item2)
        {
            return item1.Order.CompareTo(item2.Order);
        }

        public void UpdateOrders()
        {
            fOrderedColumns.Clear();
            if (fColumns.Count > 0) {
                foreach (var column in fColumns) fOrderedColumns.Add(column);
                SysUtils.MergeSort(fOrderedColumns, CompareItems);
            }
        }

        public bool MoveColumn(int idx, bool up)
        {
            ListColumn col1, col2;
            int tempOrder;

            if (up) {
                if (idx > 0) {
                    col1 = fOrderedColumns[idx - 1];
                    col2 = fOrderedColumns[idx];

                    tempOrder = col1.Order;
                    col1.Order = col2.Order;
                    col2.Order = tempOrder;

                    UpdateOrders();
                    return true;
                }
            } else {
                if (idx >= 0 && idx < fColumns.Count - 1) {
                    col1 = fOrderedColumns[idx];
                    col2 = fOrderedColumns[idx + 1];

                    tempOrder = col1.Order;
                    col1.Order = col2.Order;
                    col2.Order = tempOrder;

                    UpdateOrders();
                    return true;
                }
            }

            return false;
        }
    }
}
