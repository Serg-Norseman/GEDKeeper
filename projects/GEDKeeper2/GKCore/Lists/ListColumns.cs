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
    public sealed class ColumnProps
    {
        public byte ColType;
        public bool ColActive;
        public int ColWidth;

        public ColumnProps()
        {
        }

        public ColumnProps(byte colType, bool colActive, int colWidth)
        {
            ColType = colType;
            ColActive = colActive;
            ColWidth = colWidth;
        }

        public void Assign(ColumnProps source)
        {
            if (source == null) return;

            ColType = source.ColType;
            ColActive = source.ColActive;
            ColWidth = source.ColWidth;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class ColumnStatic
    {
        //public byte colType;
        public LSID ColName;
        public DataType DataType;
        public NumberFormatInfo NumFmt;
        public string Format;
        public int Width;
        public bool Active;
    }

    /// <summary>
    /// 
    /// </summary>
    public abstract class ListColumns : IListColumns
    {
        private List<ColumnProps> fColumns;
        private Type fColumnsEnum;

        public List<ColumnStatic> ColumnStatics;

        public int Count
        {
            get { return fColumns.Count; }
        }

        public ColumnProps this[int index]
        {
            get { return fColumns[index]; }
        }

        protected abstract void InitColumnStatics();

        protected ListColumns()
        {
            ColumnStatics = new List<ColumnStatic>();
        }

        protected void InitData(Type colEnum)
        {
            fColumnsEnum = colEnum;

            InitColumnStatics();

            fColumns = new List<ColumnProps>();
            foreach (Enum e in Enum.GetValues(fColumnsEnum))
            {
                fColumns.Add(new ColumnProps());
            }
        }

        public void ResetDefaults()
        {
            foreach (Enum e in Enum.GetValues(fColumnsEnum))
            {
                byte i = (e as IConvertible).ToByte(null);

                ColumnStatic cs = ColumnStatics[i];

                fColumns[i] = new ColumnProps(i, cs.Active, cs.Width);
            }
        }

        public Type GetColumnsEnum()
        {
            return fColumnsEnum;
        }

        public void AddStatic(/*Enum colType*/ LSID colName, DataType dataType, int defWidth, bool defActive)
        {
            ColumnStatic cs = new ColumnStatic();

            //cs.colType = ((IConvertible)colType).ToByte(null);
            cs.ColName = colName;
            cs.DataType = dataType;
            cs.Width = defWidth;
            cs.NumFmt = null;
            cs.Format = null;
            cs.Active = defActive;

            ColumnStatics.Add(cs);
        }

        public void AddStatic(/*Enum colType*/ LSID colName, DataType dataType, int defWidth, bool defActive, string format, NumberFormatInfo nfi)
        {
            ColumnStatic cs = new ColumnStatic();

            //cs.colType = ((IConvertible)colType).ToByte(null);
            cs.ColName = colName;
            cs.DataType = dataType;
            cs.Width = defWidth;
            cs.NumFmt = nfi;
            cs.Format = format;
            cs.Active = defActive;

            ColumnStatics.Add(cs);
        }

        public void CopyTo(IListColumns columns)
        {
            ListColumns cols = columns as ListColumns;
            if (cols == null)
                throw new ArgumentNullException("columns");

            foreach (Enum e in Enum.GetValues(fColumnsEnum))
            {
                byte i = (e as IConvertible).ToByte(null);

                ColumnProps col = fColumns[i];
                cols[i].Assign(col);
            }
        }

        public void LoadFromFile(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            foreach (Enum e in Enum.GetValues(fColumnsEnum))
            {
                byte i = (e as IConvertible).ToByte(null);

                ColumnStatic defCol = ColumnStatics[i];

                ColumnProps col = fColumns[i];
                col.ColType = (byte)iniFile.ReadInteger(section, "ColType_" + i, i);
                col.ColActive = iniFile.ReadBool(section, "ColActive_" + i, defCol.Active);
                col.ColWidth = iniFile.ReadInteger(section, "ColWidth_" + i, defCol.Width);
                
                // protection zero/hidden columns
                if (col.ColWidth <= 10) {
                    col.ColWidth = defCol.Width;
                }
                
                fColumns[i] = col;
            }
        }

        public void SaveToFile(IniFile iniFile, string section)
        {
            if (iniFile == null) return;

            foreach (Enum e in Enum.GetValues(fColumnsEnum))
            {
                byte i = (e as IConvertible).ToByte(null);

                iniFile.WriteInteger(section, "ColType_" + i, fColumns[i].ColType);
                iniFile.WriteBool(section, "ColActive_" + i, fColumns[i].ColActive);
                iniFile.WriteInteger(section, "ColWidth_" + i, fColumns[i].ColWidth);
            }
        }

        public bool MoveColumn(int idx, bool up)
        {
            if (up) {
                if (idx > 0) {
                    ColumnProps temp = fColumns[idx - 1];
                    fColumns[idx - 1] = fColumns[idx];
                    fColumns[idx] = temp;

                    return true;
                }
            } else {
                if (idx >= 0 && idx < fColumns.Count - 1) {
                    ColumnProps temp = fColumns[idx + 1];
                    fColumns[idx + 1] = fColumns[idx];
                    fColumns[idx] = temp;

                    return true;
                }
            }

            return false;
        }
    }
}
