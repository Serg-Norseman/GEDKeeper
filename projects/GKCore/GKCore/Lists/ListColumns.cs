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
using System.Globalization;
using BSLib;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Lists
{
    public enum DataType
    {
        dtString,
        dtInteger,
        dtFloat,
        dtDateTime,
        dtGEDCOMDate,
        dtBool
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ListColumn
    {
        public readonly byte Id;
        public readonly string ColName;
        public readonly DataType DataType;
        public readonly int DefWidth;
        public readonly bool DefActive;
        public readonly string Format;
        public readonly NumberFormatInfo NumFmt;

        public bool Autosize;

        public int CurWidth;
        public bool CurActive;
        public int Order;

        public ListColumn(byte id, string colName, DataType dataType,
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

        public override string ToString()
        {
            return string.Join(" / ", Id, ColName, DataType, DefWidth, DefActive, Order);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class ListColumns
    {
        private readonly List<ListColumn> fColumns;
        private readonly List<ListColumn> fOrderedColumns;
        private sbyte fLastId;
        private readonly GKListType fListType;


        public GKListType ListType
        {
            get { return fListType; }
        }

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


        public ListColumns(GKListType listType)
        {
            fLastId = -1;
            fColumns = new List<ListColumn>();
            fOrderedColumns = new List<ListColumn>();
            fListType = listType;
        }

        public void Clear()
        {
            fColumns.Clear();
            fOrderedColumns.Clear();
        }

        public void ResetDefaults()
        {
            for (int i = 0, num = fColumns.Count; i < num; i++) {
                var cs = fColumns[i];

                cs.Order = i;
                cs.CurActive = cs.DefActive;
                cs.CurWidth = cs.DefWidth;
            }

            UpdateOrders();
        }

        private static int CompareItems(ListColumn item1, ListColumn item2)
        {
            return item1.Order.CompareTo(item2.Order);
        }

        private void UpdateOrders()
        {
            fOrderedColumns.Clear();
            if (fColumns.Count > 0) {
                foreach (var column in fColumns) fOrderedColumns.Add(column);
                SortHelper.MergeSort(fOrderedColumns, CompareItems);
            }
        }

        public void AddColumn(LSID colName, int defWidth, bool autosize = false)
        {
            AddColumn(LangMan.LS(colName), DataType.dtString, defWidth, autosize, null, null);
        }

        public void AddColumn(string colName, int defWidth, bool autosize = false)
        {
            AddColumn(colName, DataType.dtString, defWidth, autosize, null, null);
        }

        public void AddColumn(LSID colName, DataType dataType, int defWidth, bool autosize = false)
        {
            AddColumn(LangMan.LS(colName), dataType, defWidth, autosize, null, null);
        }

        public void AddColumn(string colName, DataType dataType, int defWidth, bool autosize = false)
        {
            AddColumn(colName, dataType, defWidth, autosize, null, null);
        }

        public void AddColumn(string colName, DataType dataType, int defWidth, bool autosize, string format, NumberFormatInfo nfi)
        {
            fLastId += 1;
            fColumns.Add(new ListColumn((byte)fLastId, colName, dataType, defWidth, true, autosize, format, nfi));
        }

        public void CopyTo(ListColumns target)
        {
            if (target == null)
                throw new ArgumentNullException("target");

            int num = fColumns.Count;
            for (int i = 0; i < num; i++) {
                var srcCol = fColumns[i];
                var tgtCol = target.fColumns[i];

                tgtCol.Order = srcCol.Order;
                tgtCol.CurActive = srcCol.CurActive;
                tgtCol.CurWidth = srcCol.CurWidth;
            }

            target.UpdateOrders();
        }

        public void LoadFromFile(IniFile iniFile, string section, int optsVersion)
        {
            if (iniFile == null) return;

            try {
                int num = fColumns.Count;
                for (int i = 0; i < num; i++) {
                    int colId = iniFile.ReadInteger(section, "ColType_" + i, i);

                    if (optsVersion == 1 && optsVersion < GlobalOptions.OPTS_VERSION) {
                        // In version 2.21 (OptsVersion=2) there was a transition in column handling
                        colId += 1;
                    }

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
            } catch (Exception ex) {
                Logger.WriteError("ListColumns.LoadFromFile()", ex);
            }
        }

        public void SaveToFile(IniFile iniFile, string section, int optsVersion)
        {
            if (iniFile == null) return;

            try {
                int idx = -1;
                foreach (var col in fOrderedColumns) {
                    idx += 1;
                    iniFile.WriteInteger(section, "ColType_" + idx, col.Id);
                    iniFile.WriteBool(section, "ColActive_" + idx, col.CurActive);
                    iniFile.WriteInteger(section, "ColWidth_" + idx, col.CurWidth);
                }
            } catch (Exception ex) {
                Logger.WriteError("ListColumns.SaveToFile()", ex);
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
